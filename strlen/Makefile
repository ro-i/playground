.POSIX:

INCS = -I. -I/usr/include

CFLAGS = -std=c99 -pedantic -Wall -Wextra -O2 -march=native $(INCS) $(CPPFLAGS)
CFLAGS_DEBUG = -ggdb -std=c99 -pedantic -Wall -Wextra -Og $(INCS) $(CPPFLAGS)
CFLAGS_DEBUG_NO_OPT = -ggdb -std=c99 -pedantic -Wall -Wextra -O0 $(INCS) $(CPPFLAGS)
LDFLAGS = 

ASFLAGS = -i /usr/share/sasm/include/ -f elf64 -Wall
ASFLAGS_DEBUG = -g -i /usr/share/sasm/include/ -f elf64 -Wall
ASFLAGS_DEBUG_NO_OPT = -O0 -g -i /usr/share/sasm/include/ -f elf64 -Wall
AS_LDFLAGS = -no-pie

CC = gcc
AS = nasm

bin = strlen
bin_asm = strlen_asm
src = strlen.c
src_asm = strlen_asm.asm
obj = $(bin).o
obj_asm = $(bin_asm).o

all: $(bin) $(bin_asm)

debug: CFLAGS = $(CFLAGS_DEBUG)
debug: $(bin)
debug_asm: ASFLAGS = $(ASFLAGS_DEBUG)
debug_asm: $(bin_asm)
debug_no_opt: CFLAGS = $(CFLAGS_DEBUG_NO_OPT)
debug_no_opt: $(bin)
debug_asm_no_opt: ASFLAGS = $(ASFLAGS_DEBUG_NO_OPT)
debug_asm_no_opt: $(bin_asm)

$(bin): $(obj)
	$(CC) $(LDFLAGS) -o $@ $^

$(bin_asm): $(obj_asm)
	$(CC) $(AS_LDFLAGS) -o $@ $^

macro.o:
	gcc -c -Wno-implicit-function-declaration -o $@ \
		/usr/share/sasm/NASM/macro.c

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

clean:
	rm -f $(bin) $(bin_asm) $(obj) $(obj_asm)

.PHONY: all clean debug debug_asm debug_no_opt debug_asm_no_opt
