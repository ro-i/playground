#!/bin/sh

usage () {
	printf '%s\n%s\n' "usage: $0 [OPTION...] STRLEN_ASM_EXECUTABLE TEST DIR" \
		"   or: $0 [-h] [--help]"
	printf 'options available:
    -h, --help       show this help
    -r               read stdin instead of using files in DIR
    --time           measure time
    --valgrind       execute test using valgrind
  tests available:
    strlen (normal strlen implementation)
    strlen_opt4 (operates on DWORD string)
    strlen_opt8 (operates on QWORD string)

Note: All null-bytes in the files will be discarded.\n'
}

test_dir () {
	for file in "$dir"/*; do
		if [ -d "$file" ] || [ ! -r "$file" ]; then
			continue;
		fi
		content=$(tr -d '\0' < "$file")
		test_test "$file" "$content"
	done
}

test_stdin () {
	content=$(tr -d '\0')
	test_test "stdin" "$content"
}

test_test () {
	file="$1"
	content="$2"

	was=$(printf '%s' "$content" | $time_cmd $valgrind_cmd "$bin" "$test")
	should=$(printf '%s' "$content" | wc -c)
	if [ "$was" -ne "$should" ]; then
		printf '%s\n' "Test $file failed; should be $should, but was $was."
	else
		printf '%s\n' "Test $file passed."
	fi
}

read_stdin=false
time_cmd=""
valgrind_cmd=""

if ! type time > /dev/null 2>&1; then
	printf "Error: could not find 'time' command"; exit 1
fi

while true; do
	case "$1" in
		"-h"|"--help")
			usage; exit 0;;
		"-r")
			read_stdin=true;;
		"--time")
			time_cmd="time -p";;
		"--valgrind")
			valgrind_cmd="valgrind -q --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all";;
		"-"*)
			printf '%s\n' "Unknown option: $1"
			usage; exit 1;;
		*)
			break;
			;;
	esac
	shift
done

bin="$1"
if [ -z "$bin" ]; then
	usage; exit 1
elif [ ! -x "$bin" ]; then
	printf '%s\n' "Error: $bin is not an executable."
	exit 1
fi

case "$2" in
	"strlen"|"strlen_opt4"|"strlen_opt8")
		test="$2";;
	*)
		usage; exit 1;;
esac

if "$read_stdin"; then
	test_stdin
else
	dir="$3"
	if [ ! -d "$dir" ]; then
		printf '%s\n' "Error: $dir is not a directory."; exit 1
	fi
	test_dir
fi

