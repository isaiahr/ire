#include<stdint.h>
#include "platform.h"

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


struct HeapEntry {
    // pointer to the heap allocation
    void* ptr;
    // metadata associated with the allocation.
    void* meta;
    // mark. this object is marked when it is still assecible to the mutator.
    int8_t mark;
    // generation
    int32_t generation;
    // next heapentry; hashtable chaining with linked lists.
    struct HeapEntry* chain_next;
};

#define HEAP_SIZE 5000

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
void system_free_mem(int8_t* ptr){
    //todo
}

// allocate - essentially "malloc". wrapper around system_reserve_mem for now, but this is different in order
// to implement custom behavoir for performance possibly.
int8_t* allocate(int64_t numbytes){
    return system_reserve_mem(numbytes);
}

// free - counterpart to allocate.
void free(int8_t* ptr){
    system_free_mem(ptr);
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


void visit(void* ptr, void* data){
    struct HeapEntry* result = find((int8_t*)ptr, 0);
    if(result == NULL){
        // not in heap.
        // lives on stack.
    }
    else {
        result->mark = 1;
    }
    
}

void traverse_gc_roots(){
    struct StackEntry* chain = gc_root_chain;
    while(chain != NULL){
        for(int i = 0; i < chain->numroots*2; i+=2){
            void* ptr = chain->roots[i];
            void* data = chain->roots[i+1];
            visit(ptr, data);
        }
        chain = chain->caller;
    }
}

void gc(){
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
                if(prev == NULL){
                    heap.values[i] = p->chain_next;
                }
                else{
                    prev->chain_next = p->chain_next;
                }
                free(p->ptr);
                free((int8_t*) p);
            }
            else{
                p->generation += 1;
            }
            prev = p;
            p = p->chain_next;
        }
    }
}

void gc_init(){
    for(int i = 0; i < HEAP_SIZE; i++){
        heap.values[i] = NULL;
    }
}

int8_t* gc_alloc(int64_t numbytes){
    // do this check before allocating memory.
    if(usage > gc_threshold){
        gc();
    }
    int8_t* ptr = allocate(numbytes);
    // n.b. this is ok to zero-extend on platforms where pointers are <64 bits.
    struct HeapEntry* entry = find(ptr, 1);
    entry->meta = NULL;
    usage += numbytes;
    return ptr;
}
