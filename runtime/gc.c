#include<stdint.h>
#include "platform.h"
#include "wrapper.h"

#define NULL 0


/*
 the linklist of roots.
 format: caller - pointer to callers rootlist
         numroots - number of roots. roots has 2*numroots entries
         roots - even num is pointer to stack root, odd (+1) is metadata pointer
         describing the root. if this is directly a heap root, this is null and we
         track the heap root through the allocation itself.
 
 **/
struct StackEntry {
  struct StackEntry *caller;
  int32_t numroots;
  void* roots[0];
};

struct StackEntry* gc_root_chain;


// an object containing further heap objects as members
struct HeapObj{
    // offset (in bytes) into the object
    int64_t offset;
    // pointer to heaptracking struct for this member object
    struct HeapTracking* ptr;
    // array_size - if nonzero, offset pointes to the i64_t object "length", and the length number of objects 
    // the size itself is not length of array, but the size of an individual element. this means that 
    // array[0] is 1st element array + (array_size number of bytes is the second). basically, each element takes up
    // array size amount of bytes. so a i8 -> 1, i64 -> 8, i32 -> 4 etc.
    int64_t array_size; 
};

/*
 heaptracking - structure of location of heap pointers of an object
 */
struct HeapTracking {
    int32_t num_objs;
    struct HeapObj objs[0];
};


struct HeapEntry {
    // pointer to the heap allocation
    void* ptr;
    // metadata associated with the allocation.
    struct HeapTracking* meta;
    // mark. this object is marked when it is still assecible to the mutator.
    int8_t mark;
    // generation
    int32_t generation;
    // size (in bytes)
    int64_t size;
    // next heapentry; hashtable chaining with linked lists.
    struct HeapEntry* chain_next;
};
#ifdef DEBUG
#define HEAP_SIZE 10
#endif
#ifndef DEBUG
#define HEAP_SIZE 5000
#endif

struct Heap {
    struct HeapEntry* values[HEAP_SIZE];
};

struct Heap heap;

// 1mb - 1024b = 1kb * 1024 = 1024kb = 1mb
#define INIT_GC_THRESHOLD 1024*1024 

int64_t gc_threshold = INIT_GC_THRESHOLD;

// memory usage by the mutator, in bytes.
// this DOES NOT include memory allocated by the runtime,
// or collector itself needed to track usage.
int64_t usage = 0;


/**
 system_reserve_mem - the raw system allocation function.
 */

int8_t* system_reserve_mem(int64_t numbytes){
    return (int8_t*) _syscall6(SYS_mmap, 0, (size_t) numbytes, 0x3, 0x22, -1, 0);
}


// system free mem - the raw system deallocation function
void system_free_mem(int8_t* ptr, int64_t numbytes){
    _syscall6(SYS_munmap, (int64_t) ptr, numbytes, 0, 0, 0, 0);
}

// allocate - essentially "malloc". wrapper around system_reserve_mem for now, but this is different in order
// to implement custom behavoir for performance possibly.
int8_t* allocate(int64_t numbytes){
    return system_reserve_mem(numbytes);
}

// free - counterpart to allocate.
void free(int8_t* ptr, int64_t numbytes){
    system_free_mem(ptr, numbytes);
}


/*
 FNV-1a hash function. system allocaters might align allocated blocks
 to certain boundaries which gives it poor chariteriscis for hashtable storage.
 so this is used to hash the resulting pointer.
 */

#define FNV_OFFSET_BASIS 14695981039346656037U
#define FNV_PRIME 1099511628211 

uint64_t fnv1a(int8_t* data, int len){
    uint64_t hash = FNV_OFFSET_BASIS;
    for(int i = 0; i < len; i++){
        hash = hash ^ data[i];
        hash = hash * FNV_PRIME;
    }
    return hash;
}

struct HeapEntry* find(int8_t* key, int8_t create){
    // n.b. this doesnt if bytes are reversed, since its just for hash.
    int8_t bytes[8];
    int64_t v = (int64_t) key;
    bytes[0] = v & 0xFF;
    bytes[1] = (v >> 8) & 0xFF;
    bytes[2] = (v >> 16) & 0xFF;
    bytes[3] = (v >> 24) & 0xFF;
    bytes[4] = (v >> 32) & 0xFF;
    bytes[5] = (v >> 40) & 0xFF;
    bytes[6] = (v >> 48) & 0xFF;
    bytes[7] = (v >> 56) & 0xFF;
    uint64_t hash = fnv1a(bytes, 8);
    uint64_t idx = hash % HEAP_SIZE;
    struct HeapEntry* h = heap.values[idx];
    if (h == NULL){
        if(!create){
            return NULL;
        }
        struct HeapEntry* p = (struct HeapEntry*) allocate(sizeof (struct HeapEntry));
        p->chain_next = NULL;
        p->generation = 0;
        p->ptr = (int8_t*) key;
        heap.values[idx] = p;
        return p;
    }
    while(h->chain_next != NULL){
        if(h->ptr == key){
            return h;
        }
        h = h->chain_next;
    }
    if(h->ptr == key){
        return h;
    }
    if(!create){
        return NULL;
    }
    h->chain_next = (struct HeapEntry*) allocate(sizeof (struct HeapEntry));
    h->chain_next->generation = 0;
    h->chain_next->ptr = (int8_t*) key;
    h->chain_next->chain_next = NULL;
    return h->chain_next;
}

#ifdef DEBUG

void debug_print_chainentry(struct HeapEntry* p){
    debug_printint((int64_t)p->ptr);
}

void debug_print_table(){
    for(int i = 0; i < HEAP_SIZE; i++){
        struct HeapEntry* p = heap.values[i];
        debug_printint(i);
        debug_print("|");
        while(p != NULL){
            debug_print_chainentry(p);
            debug_print("|");
            p = p->chain_next;
        }
        debug_print("\n");
    }
}
#endif

/*
 * visit - visits a pointer
 * documentation. ptr is a pointer to the stack allocation.
 * so ptr = alloca ty 
 *  ptr = ty*. 
 * if data == null, pointer is a single heap object.
 * the heap object is reffered to by **ptr. data descripter still needed, but get it from the hashtbl.
 * use *ptr in hashtable.
 * if data != null, data is a pointer to the data descripter (see struct heaptracking).
 * this means ptr is a stack object, so **ptr is not valid. 
 * just use *ptr, and the data descripter to visit individual fields recursively.
 */
void visit_subfields(int8_t*, struct HeapTracking*);
void visit(void* ptr, struct HeapTracking* data){
    #ifdef DEBUG
    debug_print("visit: ");
    debug_printint((int64_t) ptr);
    debug_print("\n");
    #endif
    if(data == NULL){
        // null - directly a heap object.
        void** ptr2 = (void**) ptr;
        void* heap_obj = *ptr2;
        struct HeapEntry* result = find((int8_t*) heap_obj, 0);
        if(result == NULL){
            #ifdef DEBUG 
            debug_print("ERROR: HEAP VARIABLE EARLY FREE?");
            #endif
            exit(5);
        }
        else {
            result->mark = 1;
            // meta null is ok. just means no subfields.
            if(result->meta != NULL){
                visit_subfields((int8_t*) heap_obj, result->meta);
            }
        }
    }
    else {
        // not null - stack object.
        visit_subfields(ptr, data);
    }
    
}

void visit_subfields(int8_t* ptr, struct HeapTracking* meta){
    int32_t objs = meta->num_objs;
    for(int i = 0; i < objs; i++){
        struct HeapObj member_data = meta->objs[i];
        int64_t off = member_data.offset;
        int8_t* ptr_off = &ptr[off];
        if(member_data.array_size != 0){
            int64_t* ptr_off_i64 = (int64_t*) ptr_off;
            int64_t length = ptr_off_i64[0];
            // ok, now advance ptr_off to the array element
            ptr_off_i64 = &ptr_off_i64[1];
            // dereference, since arrays are {i64, ty*}
            ptr_off = (int8_t*) *ptr_off_i64;
            // visit the "main" array pointer
            visit(ptr_off, NULL);
            // now visit subfields.
            for(int j = 0; j < length; j++){
                visit(&ptr_off[j*member_data.array_size], member_data.ptr);
            }
        }
        else {
            visit(ptr_off, member_data.ptr);
        }
    }
}

void traverse_gc_roots(){
    struct StackEntry* chain = gc_root_chain;
    while(chain != NULL){
        for(int i = 0; i < chain->numroots*2; i+=2){
            void* ptr = (void*) chain->roots[i];
            void* data = chain->roots[i+1];
            // null here means the pointer is not in scope yet
            if(ptr != NULL){
                visit(ptr, data);
            }
        }
        chain = chain->caller;
    }
}

void gc(){
    #ifdef DEBUG
    debug_print("---START GC---\n");
    debug_print_table();
    #endif
    for(int i = 0; i < HEAP_SIZE; i++){
        struct HeapEntry* p = heap.values[i];
        while(p != NULL){
            p->mark = 0;
            p = p->chain_next;
        }
    }
    traverse_gc_roots();
    for(int i = 0; i < HEAP_SIZE; i++){
        struct HeapEntry* p = heap.values[i];
        struct HeapEntry* prev = NULL;
        while(p != NULL){
            if(!p->mark){
                #ifdef DEBUG
                debug_print("FREEPTR: ");
                debug_printint((int64_t)p->ptr);
                debug_print("\n");
                #endif
                if(prev == NULL){
                    heap.values[i] = p->chain_next;
                }
                else{
                    prev->chain_next = p->chain_next;
                }
                free(p->ptr, p->size);
                
                struct HeapEntry* tmp = p->chain_next;
                free((int8_t*) p, sizeof (struct HeapEntry));
                p = tmp;
            }
            else{
                #ifdef DEBUG
                debug_print("PRESERVE: ");
                debug_printint((int64_t)p->ptr);
                debug_print("\n");
                #endif
                p->generation += 1;
                prev = p;
                p = p->chain_next;
            }
        }
    }
    #ifdef DEBUG
    debug_print_table();
    debug_print("---END GC---\n");
    #endif
}

void gc_init(){
    for(int i = 0; i < HEAP_SIZE; i++){
        heap.values[i] = NULL;
    }
}

int8_t* gc_alloc(int64_t numbytes, struct HeapTracking* meta){
    
    #ifdef DEBUG
    gc();
    #endif
    #ifndef DEBUG
    // do this check before allocating memory.
    if(usage > gc_threshold){
        gc();
        // note gc also should decrease variable "usage"
        gc_threshold = gc_threshold * 2;
    }
    #endif
    int8_t* ptr = allocate(numbytes);
    // n.b. this is ok to zero-extend on platforms where pointers are <64 bits.
    struct HeapEntry* entry = find(ptr, 1);
    entry->size = numbytes;
    entry->meta = meta;
    usage += numbytes;
    return ptr;
}
