CC = clang
LD = ld.lld
LDFLAGS = -r
CCFLAGS = -nostdlib -fno-builtin -ffreestanding -Wall -O0
OUTDIR = ../build
BINDIR = ../bin
SRCS = gc.c rt.c wrapper.c

# note: MUST clean when changing options (debug or no) before to avoid linking debug and release builds together.
all: prepare $(BINDIR)/irert_linux_amd64.o $(BINDIR)/irert_linux_aarch64.o

debug: CCFLAGS += -g -DDEBUG=1
debug: all

prepare:
	@mkdir -p $(OUTDIR)
	@mkdir -p $(BINDIR)

$(BINDIR)/irert_linux_amd64.o: $(SRCS:%=$(OUTDIR)/linux_amd64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_amd64_%.o: %
	$(CC) -DLINUX_AMD64=1 -target x86_64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	

$(BINDIR)/irert_linux_aarch64.o: $(SRCS:%=$(OUTDIR)/linux_aarch64_%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_aarch64_%.o: %
	$(CC) -DLINUX_AARCH64=1 -target aarch64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	


clean: 
	rm -f $(wildcard $(OUTDIR)/*.o)

.PHONY: all debug prepare clean

