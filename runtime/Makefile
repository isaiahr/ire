compile:
	mkdir -p ../build/
	clang -DLINUX_AMD64=1 -target x86_64-unknown-none-elf -nostdlib -fno-builtin -ffreestanding rt.c -c -o ../build/irert_linux_amd64.o
	clang -DLINUX_AARCH64=1 -target aarch64-unknown-none-elf -nostdlib -fno-builtin -ffreestanding rt.c -c -o ../build/irert_linux_aarch64.o
