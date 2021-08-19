#ifndef __WRAPPER_H__
#define __WRAPPER_H__
#include<stdint.h>
#include "platform.h"

int64_t writefd(int32_t fd, int8_t* buffer, size_t count);
void exit(int64_t status);

#ifdef DEBUG
void debug_print(char* string);
void debug_printint(int64_t lnt);
#endif

#endif
