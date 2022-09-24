#ifndef __IRETYPES_H__
#define __IRETYPES_H__

/**
 * iretypes.h - typedefs for types the ire language uses, for cross-boundary calls
 */

#include <stdint.h>

typedef int64_t ire_int_t;
typedef double ire_float_t;
typedef struct {
    int64_t bytes;
    // Note: pointer here is always heap allocated.
    int8_t* ptr;
} ire_string_t;

typedef int8_t ire_bool_t;

typedef struct {} ire_void_t;

#endif
