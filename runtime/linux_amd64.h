#ifndef __LINUX_AMD64_H__
#define __LINUX_AMD64_H__
#include <stdint.h>

#define SYS_exit 60
#define SYS_write 1
#define SYS_mmap 9

#define linux_reg_size_t int64_t 
#define size_t int64_t
static inline linux_reg_size_t _syscall1 (linux_reg_size_t syscall, linux_reg_size_t param0){
    register linux_reg_size_t rax __asm__ ("rax") = syscall;
    register linux_reg_size_t rdi __asm__ ("rdi") = param0;
    __asm__ volatile (
        "syscall"
        :"+r" (rax)
        :"r" (rdi)
        :"cc", "memory"
        // cc = FLAGS (maybe not modified?) 
    );
    return rax;
}

static inline linux_reg_size_t _syscall6 (linux_reg_size_t syscall, linux_reg_size_t param0, linux_reg_size_t param1, linux_reg_size_t param2, linux_reg_size_t param3, linux_reg_size_t param4, linux_reg_size_t param5){
    register linux_reg_size_t rax __asm__ ("rax") = syscall;
    register linux_reg_size_t rdi __asm__ ("rdi") = param0;
    register linux_reg_size_t rsi __asm__ ("rsi") = param1;
    register linux_reg_size_t rdx __asm__ ("rdx") = param2;
    register linux_reg_size_t r10 __asm__ ("r10") = param3;
    register linux_reg_size_t r8 __asm__ ("r8") = param4;
    register linux_reg_size_t r9 __asm__ ("r9") = param5;
    __asm__ volatile (
        "syscall"
        :"+r" (rax)
        :"r" (rdi), "r" (rsi), "r" (rdx), "r" (r10), "r" (r8), "r" (r9)
        :"cc", "memory"
    );
    return rax;
}

#endif
