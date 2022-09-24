#include <stdint.h>

#include "iretypes.h"
#include "platform.h"
#include "wrapper.h"
#include "gc.h"
#include "ryu/ryu.h"

#define NULL 0
#define STDOUT 1
#define STDERR 2
#define STDIN 0

extern void main();

void __irert__exit__(ire_int_t);
int8_t* __irert__gc_alloc__(ire_int_t, void* metadata);
void __irert__print__(ire_string_t);

// same abi as heaptracker with a = 1
struct ht_1 {
    int32_t a;
    int64_t b;
    void* c;
    int64_t d;
};

struct ht_0 {
    int32_t a;
};

struct ht_1 ire_string_trck = {
    1,
    8,
    NULL,
    0
};

struct ht_0 ire_anonymous_block = {
    0
};

#define ire_string_tracker ((struct HeapTracking*) (&ire_string_trck))
#define ire_anon_tracker ((struct HeapTracking*) (&ire_anonymous_block))

void _start() {
    gc_init();
    main();
    __irert__exit__(0);
}

int64_t __irert__writefd__(int64_t fd, ire_string_t str){
    return writefd(fd, str.ptr, str.bytes);
}

int8_t* __irert__gc_alloc__(int64_t numbytes, void* metadata){
    return gc_alloc(numbytes, metadata);
}

void __irert__print__(ire_string_t str){
    __irert__writefd__(STDOUT, str);
}

void __irert__exit__(ire_int_t status) {
    exit(status);
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
        t.ptr = __irert__gc_alloc__(1, ire_anon_tracker);
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
        t.ptr = __irert__gc_alloc__(t.bytes, ire_anon_tracker);
        t.ptr[0] = '-';
    }
    else{
        t.bytes = i;
        t.ptr = __irert__gc_alloc__(t.bytes, ire_anon_tracker);
    }
    for(int8_t j = 0; j < i; j++){
        t.ptr[j+negative] = buffer[i-j-1];
    }
    return t;
}

ire_string_t __irert__floattostring__(ire_float_t value){
    char buffer[32];
    int32_t len = d2s(value, buffer);
    ire_string_t t;
    t.ptr = __irert__gc_alloc__(len, ire_anon_tracker);
    t.bytes = len;
    while(--len >= 0){
        t.ptr[len] = buffer[len];
    }
    return t;
}
