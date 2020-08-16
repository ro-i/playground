#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


/* error messages */
const char *err_before_memory = "moved before available memory";
const char *err_oom = "cannot allocate more memory";
const char *err_read = "failed to read from stdin";
const char *err_write = "failed to write to stdout";
const char *err_jump_forward = "no matching ']' found";
const char *err_jump_backward = "no matching '[' found";


/*
 * variables:
 *
 * global:
 * asm_eof           make value of EOF accessible to Assembler function
 * asm_bufsiz        make value of BUFSIZ accessible to Assembler function
 * eof_behavior      behavior code for end-of-file handling in the interpreter
 * memory_buf_limit  limit size of memory buffer
 * file scope:
 * commands          commands to be executed
 * name              command line name of the interpreter (argv[0])
 */
const int           asm_eof = EOF;
const size_t        asm_bufsiz = BUFSIZ;
int                 eof_behavior = 0;
size_t              memory_buf_limit = -1;
static char        *commands;
static const char  *name;


void          *s_calloc(size_t n, size_t s);
void          *s_malloc(size_t n);
void          *s_realloc(void *p, size_t s);
extern char   *brainfuck(const char *commands, size_t memory_buf_limit);
static void    cleanup(void);
static void    die(const char *format, ...);
static char   *read_file(const char *file);
static void    usage(void);


void *
s_calloc(size_t n, size_t s)
{
	void *p;

	p = calloc(n, s);
	if (!p)
		die("calloc: %s", strerror(errno));

	return p;
}

void *
s_malloc(size_t n)
{
	void *p;

	p = malloc(n);
	if (!p)
		die("malloc: %s", strerror(errno));

	return p;
}

void *
s_realloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p)
		die("realloc: %s", strerror(errno));

	return p;
}

/*
 * This function may be called multiple times, so reset the pointer after free.
 */
void
cleanup(void)
{
	free(commands);
	commands = NULL;
}

void
die(const char *format, ...)
{
	va_list args;

	cleanup();

	if (format) {
		fprintf(stderr, "%s - error - ", name);
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		fputc('\n', stderr);
	}

	exit(EXIT_FAILURE);
}

/*
 * Keep only the command characters and remove all the other stuff (comments,
 * whitespace).
 */
char *
parse_commands(const char *str)
{
	char *cmd = s_malloc(strlen(str) + 1);
	size_t i = 0;

	for (; *str; str++) {
		switch (*str) {
		case '>': /* fall through */
		case '<': /* fall through */
		case '+': /* fall through */
		case '-': /* fall through */
		case '.': /* fall through */
		case ',': /* fall through */
		case '[': /* fall through */
		case ']':
			cmd[i++] = *str;
			break;
		}
	}

	/* terminating null byte */
	cmd[i++] = '\0';

	return s_realloc(cmd, i);
}

/*
 * Read "file" and return its content.
 */
char *
read_file(const char *file)
{
	char *str = NULL, *tmp;
	FILE *f;
	size_t n, size = 0;

	if (!(f = fopen(file, "r")))
		die("fopen: %s", strerror(errno));

	do {
		/* make "str" one chunk larger */
		size += BUFSIZ;
		str = s_realloc(str, size);
		/* make "tmp" point to this new chunk */
		tmp = str + size - BUFSIZ;
	} while ((n = fread(tmp, 1, BUFSIZ, f)) == BUFSIZ);

	fclose(f);
	tmp[n] = '\0';

	return str;
}

void
usage(void)
{
	printf("usage: %s [OPTION]... FILE...\n"
			"\nA simple brainfuck interpreter that reads commands"
			" from STRING or FILE(s).\n"
			"If multiple files are given, they are executed"
			" consecutively.\nIf the -c option is used in conjunction"
			" with one or multiple files, the commands provided by the"
			" string are executed first.\n"
			"\nOptions:\n"
			"  -c STRING\texecute commands provided by STRING\n"
			"  -e ARG\tspecify end-of-file behavior\n"
			"\t\t  possible values for ARG:\n"
			"\t\t    0\tset cell to zero (default)\n"
			"\t\t    1\tset cell to -1\n"
			"\t\t    2\tleave cell value unchanged\n"
			"  -h\t\tshow this help\n"
			"  -m SIZE\tlimit the memory available to the executed"
			" programs to SIZE (default: unlimited)\n",
			name);
}

int
main(int argc, char **argv)
{
	/* pointer to possible error messages - disregarded if NULL */
	char *error_msg;
	char *command_buf = NULL, *ptr;
	int opt;

	name = argv[0];

	while ((opt = getopt(argc, argv, "c:e:hm:")) != -1) {
		switch (opt) {
		case 'c':
			command_buf = optarg;
			break;
		case 'e':
			eof_behavior = strtol(optarg, &ptr, 10);
			if (!*optarg || *ptr || eof_behavior < 0 || eof_behavior > 2) {
				usage();
				return EXIT_FAILURE;
			}
			break;
		case 'h':
			usage();
			return EXIT_SUCCESS;
		case 'm':
			memory_buf_limit = strtoll(optarg, &ptr, 10);
			if (!*optarg || *ptr || memory_buf_limit < 1) {
				usage();
				return EXIT_FAILURE;
			}
			break;
		default:
			usage();
			return EXIT_FAILURE;
		}
	}

	if (!command_buf && optind >= argc) {
		usage();
		return EXIT_FAILURE;
	}

	do {
		/* read commands from file */
		if (!command_buf)
			command_buf = read_file(argv[optind++]);
		commands = parse_commands(command_buf);
		free(command_buf);

		error_msg = brainfuck(commands, memory_buf_limit);
		if (error_msg)
			die("%s", error_msg);

		cleanup();
	} while (optind < argc);

	return EXIT_SUCCESS;
}
