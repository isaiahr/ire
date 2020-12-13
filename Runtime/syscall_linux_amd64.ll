
define i64 @_syscall6(i64, i64, i64, i64, i64, i64, i64){
%8 = call i64 asm sideeffect "
    movq $1, %rax
    movq $2, %rdi
    movq $3, %rsi
    movq $4, %rdx
    movq $5, %r10
    movq $6, %r8
    movq $7, %r9
    syscall
    movq %rax, $0
", "=r,r,r,r,r,r,r,r,~{memory},~{rax},~{rdi},~{rsi},~{rdx},~{r10},~{r8},~{r9}" (i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6)
ret i64 %8
}

define i64 @_syscall1(i64, i64){
%3 = call i64 asm sideeffect "
    movq $1, %rax
    movq $2, %rdi
    syscall
    movq %rax, $0
", "=r,r,r,~{memory},~{rax},~{rdi}" (i64 %0, i64 %1)
ret i64 %3
}
