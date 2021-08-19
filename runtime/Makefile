CC = clang
LD = ld.lld
LDFLAGS = -r
CCFLAGS = -nostdlib -fno-builtin -ffreestanding -Wall -O0
OUTDIR = ../build
SRCS = gc.c rt.c wrapper.c

# clean before to avoid linking debug and release builds together.
all: clean prepare $(OUTDIR)/irert_linux_amd64.o $(OUTDIR)/irert_linux_aarch64.o

debug: CCFLAGS += -g -DDEBUG=1
debug: all

prepare:
	mkdir -p ../build/

$(OUTDIR)/irert_linux_amd64.o: $(SRCS:%=$(OUTDIR)/linux_amd64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_amd64_%.o: %
	$(CC) -DLINUX_AMD64=1 -target x86_64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	

$(OUTDIR)/irert_linux_aarch64.o: $(SRCS:%=$(OUTDIR)/linux_aarch64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_aarch64_%.o: %
	$(CC) -DLINUX_AARCH64=1 -target aarch64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	


clean: 
	rm -f $(wildcard $(OUTDIR)/*.o)

.PHONY: all debug prepare clean

