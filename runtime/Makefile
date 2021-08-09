CC = clang
LD = ld.lld
LDFLAGS = -r
CFLAGS = -nostdlib -fno-builtin -ffreestanding -Wall -O3
OUTDIR = ../build
SRCS = gc.c rt.c

all: prepare $(OUTDIR)/irert_linux_amd64.o $(OUTDIR)/irert_linux_aarch64.o

prepare:
	mkdir -p ../build/

$(OUTDIR)/irert_linux_amd64.o: $(SRCS:%=$(OUTDIR)/linux_amd64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_amd64_%.o: %
	$(CC) -DLINUX_AMD64=1 -target x86_64-unknown-none-elf $(CFLAGS) $< -c -o $@
	

$(OUTDIR)/irert_linux_aarch64.o: $(SRCS:%=$(OUTDIR)/linux_aarch64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_aarch64_%.o: %
	$(CC) -DLINUX_AARCH64=1 -target aarch64-unknown-none-elf $(CFLAGS) $< -c -o $@
	


clean: 
	rm $(OUTDIR)/*.o

.PHONY: all prepare clean

