#ifndef __LINUX_AARCH64_H__
#define __LINUX_AARCH64_H__
#include <stdint.h>

#define SYS_exit 93
#define SYS_write 64
#define SYS_mmap 222
#define SYS_munmap 215

#define linux_reg_size_t int64_t 
#define size_t int64_t
static inline linux_reg_size_t _syscall1 (linux_reg_size_t syscall, linux_reg_size_t param0){
    register linux_reg_size_t x8 __asm__ ("x8") = syscall;
    register linux_reg_size_t x0 __asm__ ("x0") = param0;
    __asm__ volatile (
        "svc 0;"
        :"+r" (x0)
        :"r" (x8)
        :"cc", "memory"
        // cc = FLAGS (maybe not modified?) 
    );
    return x0;
}

static inline linux_reg_size_t _syscall6 (linux_reg_size_t syscall, linux_reg_size_t param0, linux_reg_size_t param1, linux_reg_size_t param2, linux_reg_size_t param3, linux_reg_size_t param4, linux_reg_size_t param5){
    register linux_reg_size_t x8 __asm__ ("x8") = syscall;
    register linux_reg_size_t x0 __asm__ ("x0") = param0;
    register linux_reg_size_t x1 __asm__ ("x1") = param1;
    register linux_reg_size_t x2 __asm__ ("x2") = param2;
    register linux_reg_size_t x3 __asm__ ("x3") = param3;
    register linux_reg_size_t x4 __asm__ ("x4") = param4;
    register linux_reg_size_t x5 __asm__ ("x5") = param5;
    __asm__ volatile (
        "svc 0;"
        :"+r" (x0)
        :"r" (x8), "r" (x1), "r" (x2), "r" (x3), "r" (x4), "r" (x5)
        :"cc", "memory"
    );
    // note: different than x86, return val is x0
    return x0;
}

#endif
