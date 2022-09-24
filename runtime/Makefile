CC = clang
LD = ld.lld
LDFLAGS = -r
CCFLAGS = -nostdlib -fno-builtin -ffreestanding -Wall -O0 -I ./
OUTDIR = ../build
BINDIR = ../bin
SRCS = gc.c rt.c wrapper.c ryu/d2s.c

# note: MUST clean when changing options (debug or no) before to avoid linking debug and release builds together.
all: prepare $(BINDIR)/irert_linux_amd64.o $(BINDIR)/irert_linux_aarch64.o

debug: CCFLAGS += -g -DDEBUG=1
debug: all

prepare:
	

$(BINDIR)/irert_linux_amd64.o: $(SRCS:%=$(OUTDIR)/linux_amd64/%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_amd64/%.o: %
	@mkdir -p $(dir $@)
	$(CC) -DLINUX_AMD64=1 -target x86_64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	

$(BINDIR)/irert_linux_aarch64.o: $(SRCS:%=$(OUTDIR)/linux_aarch64/%.o)
	$(LD) $(LDFLAGS) $^ -o $@

$(OUTDIR)/linux_aarch64/%.o: %
	@mkdir -p $(dir $@)
	$(CC) -DLINUX_AARCH64=1 -target aarch64-unknown-none-elf $(CCFLAGS) $< -c -o $@
	


clean: 
	rm -rf $(OUTDIR)/linux_amd64/
	rm -rf $(OUTDIR)/linux_aarch64/

.PHONY: all debug prepare clean

