compile:
	mkdir -p ../build/
	clang -nostdlib -fno-builtin -ffreestanding entry_linux.c -S -emit-llvm -o ../build/temp0.ll
	llvm-link -S ../build/temp0.ll syscall_linux_amd64.ll > ../build/irert.ll
	llc --filetype=obj ../build/irert.ll -o ../build/irert_linux_amd64.o
