#ifndef __GC_H__
#define __GC_H__

#include<stdint.h>

void gc_init();
int8_t* gc_alloc(int64_t numbytes, void* metadata);

// these definitions are duplicated for usage elsewhere
struct HeapObj{
    int64_t offset;
    struct HeapTracking* ptr;
    int64_t array_size; 
};

struct HeapTracking {
    int32_t num_objs;
    struct HeapObj objs[0];
};

#endif
