#include <sys/syscall.h>
#include <sys/types.h>

extern size_t _syscall1(size_t, size_t);
extern size_t _syscall6(size_t, size_t, size_t, size_t, size_t, size_t, size_t);

extern void main();

void __irert__exit__(int64_t);

void _start() {
    main();
    __irert__exit__(0);
}

void __irert__exit__(int64_t status) {
    _syscall1(SYS_exit, status);
}

int64_t writefd(int32_t fd, char* buffer, size_t count){
    // buffer should be zero-extended
    return _syscall6(SYS_write, fd, (size_t) buffer, count, 0, 0, 0);
}


