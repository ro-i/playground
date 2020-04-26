/* for the time-measuring functions */
#define _POSIX_C_SOURCE 199309L

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


static void *
_realloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}
	return p;
}

/*
 * Make sure that the char pointer is 4-Byte aligned before casting it to an
 * int pointer, thus preventing a possible page fault.
 * Thanks to my professor Martin Schulz (@TUM) for this nice idea!
 *
 * Place the offset in "offset" and return true if the string needs further
 * procession, or return false if a null-byte has already been found in the
 * offset part of the string (in this case, "offset" is equivalent to the
 * length of the string).
 */
static bool
alignment_check(const char *s, int *offset)
{
	int i;

	/* TODO: portable ? */
	*offset = 4 - ((long long unsigned) s) % 4;

	if (*offset == 4)
		return true;

	for (i = 0; i < *offset; i++) {
		if (!s[i])
			return false;
	}

	return true;
}

static char *
read_input(void)
{
	char *str = NULL, *tmp;
	size_t n, size = 0;

	/* read string from stdin */
	do {
		/* make "str" one chunk larger */
		size += BUFSIZ;
		str = _realloc(str, size);
		/* make "tmp" point to this new chunk */
		tmp = str+(size-BUFSIZ);
	} while ((n = fread(tmp, 1, BUFSIZ, stdin)) == BUFSIZ);
	tmp[n] = '\0';

	return str;
}

/* portable */
static size_t
strlen0(const char *s)
{
	const char *base_ptr = s;

	while(*s++);
	return --s-base_ptr;
}

/* not portable */
static size_t
strlen1(const char *s)
{
	const int *ptr;
	int offset;

	if (!alignment_check(s, &offset))
		return offset;

	ptr = (int *) (s+offset);

	for (;;) {
		if (!(*ptr & 0x000000ff))
			return (char *)ptr-s;
		else if (!(*ptr & 0x0000ff00))
			return (char *)ptr-s+1;
		else if (!(*ptr & 0x00ff0000))
			return (char *)ptr-s+2;
		else if (!(*ptr++ & 0xff000000)) // increment
			return (char *)--ptr-s+3;
	}
}

/* portable */
static size_t
strlen2(const char *s)
{
	const int *ptr;
	char *is_little_endian;
	int offset, n, shift, test = 1;

	if (!alignment_check(s, &offset))
		return offset;

	ptr = (int *) (s+offset);

	/* little or big endian? */
	is_little_endian = (char *)&test;
	/* number of bits in one int */
	n = sizeof(n)*CHAR_BIT;

	for (;; ptr++) {
		for (shift = 0; shift < n; shift += CHAR_BIT) {
			if ((*ptr >> shift) & 0xff)
				continue;
			return *is_little_endian ?
				(char *)ptr-s+shift/CHAR_BIT :
				(char *)ptr-s+((n-shift)/CHAR_BIT-1);
		}
	}
}

/* portable */
static size_t
strlen3(const char * restrict s)
{
	const char * restrict base_ptr = s;

	while(*s++);
	return --s-base_ptr;
}

/* portable */
static size_t
strlen4(const char *s)
{
	size_t i = 0;

	while(s[i++]);
	return --i;
}

/* portable */
static size_t
strlen5(const char * restrict s)
{
	size_t i = 0;

	while(s[i++]);
	return --i;
}

static size_t
(*parse_strlen_cmd(const char *arg))(const char *s)
{
	if (!strcmp(arg, "strlen"))
		return strlen;
	else if (!strcmp(arg, "strlen0"))
		return strlen0;
	else if (!strcmp(arg, "strlen1"))
		return strlen1;
	else if (!strcmp(arg, "strlen2"))
		return strlen2;
	else if (!strcmp(arg, "strlen3"))
		return strlen3;
	else if (!strcmp(arg, "strlen4"))
		return strlen4;
	else if (!strcmp(arg, "strlen5"))
		return strlen5;
	else
		return NULL;
}

static void
usage(const char *name)
{
	printf("usage: %s [STRLEN_CMD]\n\n"
			"STRLEN_CMD may be:\n"
			"  strlen  (default library version)\n"
			"  strlen[0-5]\n",
			name);
}

int
main(int argc, char **argv)
{
	size_t (*strlen_cmd)(const char *s);
	char *str;
	size_t len;
	struct timespec start, end;

	if (argc != 2 || !(strlen_cmd = parse_strlen_cmd(argv[1]))) {
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	str = read_input();

	if (clock_gettime(CLOCK_MONOTONIC, &start)) {
		perror("clock_gettime");
		return EXIT_FAILURE;
	}
	
	len = strlen_cmd(str);

	if (clock_gettime(CLOCK_MONOTONIC, &end)) {
		perror("clock_gettime");
		return EXIT_FAILURE;
	}

	printf("%s: %zu\nduration: %lu seconds, %lu milliseconds\n",
			argv[1], len,
			end.tv_sec-start.tv_sec,
			end.tv_nsec-start.tv_nsec
	      );

	free(str);
	return EXIT_SUCCESS;
}
