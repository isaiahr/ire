/**
 wrapper.c - wrapper around system calls and platform dependant things.
 ire rt functions should use functions in here and NOT the system calls directly.
 */

#include<stdint.h>
#include "platform.h"

int64_t writefd(int32_t fd, int8_t* buffer, size_t count){
    // buffer should be zero-extended
    return _syscall6(SYS_write, fd, (size_t) buffer, count, 0, 0, 0);
}

void exit(int64_t status){
    _syscall1(SYS_exit, status);
}

// would prefer llvm creating own memcpy, but it seems to expect one. so here it is
void* memcpy (void* restrict dest, const void* restrict src, int64_t n){
    int8_t* db = (int8_t*) dest;
    int8_t* sb = (int8_t*) src;
    for(int64_t i=0; i<n; i++){
        db[i] = sb[i];
    }
    return dest;
}

#ifdef DEBUG
void debug_print(char* string){
    int64_t len = 0;
    while(string[len] != 0){
        len += 1;
    }
    writefd(2, (int8_t*) string, len);
}

void debug_printint(int64_t value){
    int8_t buffer[20];
    if(value == 0){
        buffer[0] = '0';
        buffer[1] = 0;
        debug_print((char*) buffer);
        return;
    }
    int8_t i = 0;
    int8_t negative = 0;
    int64_t cur = value;
    if (value < 0){
        negative = 1;
        cur = -value;
    }
    while(cur != 0){
        int64_t div = cur / 10;
        int64_t mod = cur % 10;
        int8_t digit = ((int8_t) mod) + '0';
        buffer[i] = digit;
        i++;
        cur = div;
    }
    if(negative){
        buffer[i] = '-';
        i++;
    }
    int8_t j = 0;
    for(; j < i/2; j++){
        int8_t tmp = buffer[i-j-1];
        buffer[i-j-1] = buffer[j];
        buffer[j] = tmp;
    }
    buffer[i] = 0;
    debug_print((char*) buffer);
}
#endif
