#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


/* size of buffer provided for commands in bytes */
#define COMMAND_BUF_SIZE 1000000
/* amount of memory for the interpreter in bytes */
#define MEMORY_BUF_SIZE 1000000


/* error messages */
const char *err_before_memory = "moved before available memory";
const char *err_oom = "moved after available memory";
const char *err_read = "error while trying to read from stdin";
const char *err_write = "error while trying to write to stdout";
const char *err_jump_forward = "no matching ']' found";
const char *err_jump_backward = "no matching '[' found";
const char *stdin_eof = "stdin: EOF";



/*
 * variables:
 *
 * command_buf  commands to be executed
 * memory_buf   memory buffer for interpreter
 * name         command line name of the interpreter (argv[0])
 */
static char        *command_buf;
static char        *memory_buf;
static const char  *name;


static void   *_calloc(size_t n, size_t s);
static void   *_malloc(size_t n);
static void   *_realloc(void *p, size_t s);
extern char   *brainfuck(const char *commands, char *memory_buf, size_t memory_buf_size);
static void    cleanup(void);
static void    die(const char *format, ...);
static char   *read_commands(const char *file);
static char   *read_commands_string(const char *cmd_src);
static void    usage(void);


void *
_calloc(size_t n, size_t s)
{
	void *p;

	p = calloc(n, s);
	if (!p)
		die("calloc: %s", strerror(errno));

	return p;
}

void *
_malloc(size_t n)
{
	void *p;

	p = malloc(n);
	if (!p)
		die("malloc: %s", strerror(errno));

	return p;
}

void *
_realloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p)
		die("realloc: %s", strerror(errno));

	return p;
}

void
cleanup(void)
{
	/* we might deal with sensitive data */
	if (command_buf)
		memset(command_buf, 0, strlen(command_buf));
	free(command_buf);
	if (memory_buf)
		memset(memory_buf, 0, MEMORY_BUF_SIZE);
	free(memory_buf);
}

void
die(const char *format, ...)
{
	va_list args;

	cleanup();
	if (format) {
		fprintf(stderr, "%s - ", name);
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		fputc('\n', stderr);
	}
	exit(EXIT_FAILURE);
}

/* Read commands from "file" to "cmd_str", and return the length ot the latter. */
char *
read_commands(const char *file)
{
	char *str = NULL, *tmp;
	FILE *f;
	size_t n, size = 0;

	if (!(f = fopen(file, "r")))
		die("fopen: %s", strerror(errno));

	do {
		/* make "str" one chunk larger */
		size += BUFSIZ;
		if (size > COMMAND_BUF_SIZE)
			die("error: too much commands.");
		str = _realloc(str, size);
		/* make "tmp" point to this new chunk */
		tmp = str+(size-BUFSIZ);
	} while ((n = fread(tmp, 1, BUFSIZ, f)) == BUFSIZ);

	fclose(f);
	tmp[n] = '\0';

	return str;
}

/* Read commands from "cmd_source" to "cmd_str", and return the length of the latter. */
char *
read_commands_string(const char *cmd_src)
{
	char *cmd_dest;
	size_t len = strlen(cmd_src)+1;

	if (len > COMMAND_BUF_SIZE)
		die("error: too much commands.");

	cmd_dest = _malloc(len);
	memcpy(cmd_dest, cmd_src, len);

	return cmd_dest;
}

void
usage(void)
{
	printf("usage: %s FILE\n"
			"   or: %s -c STRING\n"
			"A simple brainfuck interpreter that reads commands "
			"from STRING or FILE.\n",
			name, name);
}

int
main(int argc, char **argv)
{
	/* pointer to possible error messages - disregarded if NULL */
	char *error_msg;
	int opt;

	name = argv[0];

	while ((opt = getopt(argc, argv, "c:h")) != -1) {
		switch (opt) {
		case 'c':
			command_buf = read_commands_string(optarg);
			break;
		case 'h':
			usage();
			return EXIT_SUCCESS;
		default:
			usage();
			return EXIT_FAILURE;
		}
	}

	/* read commands from file */
	if (!command_buf && optind < argc) {
		command_buf = read_commands(argv[optind]);
	} else if (!command_buf) {
		usage();
		return EXIT_FAILURE;
	}

	memory_buf = _calloc(MEMORY_BUF_SIZE, 1);

	error_msg = brainfuck(command_buf, memory_buf, MEMORY_BUF_SIZE);
	if (error_msg)
		die("%s", error_msg);

	cleanup();
	return EXIT_SUCCESS;
}
