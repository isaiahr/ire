#include <stdint.h>
#include "iretypes.h"

#ifdef LINUX_AARCH64
#include "linux_aarch64.h"
#endif

#ifdef LINUX_AMD64
#include"linux_amd64.h"
#endif

#define STDOUT 1
#define STDERR 2
#define STDIN 0


extern void main();

struct i_string_t {
    int64_t bytes;
    int8_t* ptr;
};


void __irert__exit__(ire_int_t);
int8_t* __irert__gc_alloc__(ire_int_t);
void __irert__print__(ire_string_t);

void _start() {
    main();
    __irert__exit__(0);
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


int64_t writefd(int32_t fd, int8_t* buffer, size_t count){
    // buffer should be zero-extended
    return _syscall6(SYS_write, fd, (size_t) buffer, count, 0, 0, 0);
}

int64_t __irert__writefd__(int64_t fd, ire_string_t str){
    return writefd(fd, str.ptr, str.bytes);
}

int8_t* __irert__gc_alloc__(int64_t numbytes){
    return (int8_t*) _syscall6(SYS_mmap, 0, (size_t) numbytes, 0x3, 0x22, -1, 0);
}

void __irert__print__(ire_string_t str){
    __irert__writefd__(STDOUT, str);
}

void __irert__exit__(ire_int_t status) {
    _syscall1(SYS_exit, status);
}

void __irert__panic__(){
    writefd(STDERR, (int8_t*) "ire: program panic (unrecoverable error)\nCall Stack:", 52);
    writefd(STDERR, (int8_t*) "not yet implemented", 19);
    __irert__exit__(-1);
}


ire_string_t __irert__inttostring__(ire_int_t value){
    // note: max a 64bit int can hold (converted to str) is 20 chars
    // (loosely: log10(2^63) +1 <= 20)
    int8_t buffer[20];
    if(value == 0){
        // special case; doesn't play well with everything else
        // since the base case would output empty string for 0.
        // handle this manually.
        ire_string_t t;
        t.bytes = 1;
        t.ptr = __irert__gc_alloc__(1);
        t.ptr[0] = '0';
        return t;
    }
    int8_t i = 0;
    int8_t negative = 0;
    ire_int_t cur = value;
    if (value < 0){
        negative = 1;
        cur = -value;
    }
    while(cur != 0){
        ire_int_t div = cur / 10;
        ire_int_t mod = cur % 10;
        int8_t digit = ((int8_t) mod) + '0';
        buffer[i] = digit;
        i++;
        cur = div;
    }
    // copy (in reverse) to an irestring.
    ire_string_t t;
    if(negative){
        t.bytes = i+1;
        t.ptr = __irert__gc_alloc__(t.bytes);
        t.ptr[0] = '-';
    }
    else{
        t.bytes = i;
        t.ptr = __irert__gc_alloc__(t.bytes);
    }
    for(int8_t j = 0; j < i; j++){
        t.ptr[j+negative] = buffer[i-j-1];
    }
    return t;
}
