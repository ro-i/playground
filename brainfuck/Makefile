.POSIX:

CPPFLAGS = -D_POSIX_C_SOURCE=200809L

CFLAGS = -std=c99 -pedantic -Wall -Wextra -O2 -march=native
CFLAGS_DEBUG = -ggdb -std=c99 -pedantic -Wall -Wextra
CFLAGS_DEBUG_NO_OPT = -ggdb -std=c99 -pedantic -Wall -Wextra -O0

CC = gcc

bin = brainfuck
src = brainfuck.c brainfuck.S

all: $(bin)

debug: CFLAGS = $(CFLAGS_DEBUG)
debug: $(bin)
debug_no_opt: CFLAGS = $(CFLAGS_DEBUG_NO_OPT)
debug_no_opt: $(bin)

$(bin): $(src)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $^

clean:
	rm -f $(bin)

.PHONY: all clean debug debug_no_opt
