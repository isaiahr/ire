#ifndef __GC_H__
#define __GC_H__

#include<stdint.h>

void gc_init();
int8_t* gc_alloc(int64_t numbytes);

#endif
