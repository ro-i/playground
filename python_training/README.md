Description
-----------
This is temporary training stuff... :-)

I wrote this little thing in order to refresh my Python skills, especially
Python's OO-features, data types, command line argument parsing, regex, and
file I/O.
Therefore some of the stuff here is constructed a little complicated... :-)

What have I done?

- "abstract" super class `Alg`:
  * input may be set using the constructor or `set_input()`
  * algorithm may be executed using `run()` ...
  * ... or using `test()` which additionally verifies the result

- "abstract" classes `SearchAlg` and `SortAlg`:
  They inherit from `Alg` and specify some useful functionality for search
  and sorting algorithms. Test data in `test()` is generated automatically.

- `BinarySearch`:
  Actual implementation of a search algorithm, inherits from `SearchAlg`

- `MergeSort`:
  Actual implementation of a sorting algorithm, inherits from `SearchAlg`

- `FindFilesContainingRegex`:
  Another concrete implementation of an algorithm, inherits from `Alg`.
  Searches recursively in all files of a directory for a regex and counts
  all matching lines per file. `test()` asks user interactively for
  directory path and regex. Result is verified using `grep`.
  Note: Although `grep`'s extended regular expressions are used, the results
  for non-standard regexes might differ!


usage
-----
```
usage: alg.py [-h] [-d] [-o]

Run some algorithms and let them test themselves automatically with random input.

optional arguments:
  -h, --help    show this help message and exit
  -d, --debug   genrate additional output for debugging purposes (implies -o)
  -o, --output  inform on wrong results
```

The output of a sample execution:
```
$ time ./alg.py
Tested BinarySearch on 100 sequences with up to 1000 elements in the range 0 - 10000.
BinarySearch - test passed
Tested MergeSort on 100 sequences with up to 1000 elements in the range 0 - 10000.
MergeSort - test passed
FindFilesContainingRegex - Please provide a directory for the test: /usr/share/doc
FindFilesContainingRegex - Please provide a regex to search for: system.*[0-9]{1,2}
Tested FindFilesContainingRegex on 6869 files with a total of 1984 matches.
FindFilesContainingRegex - test passed
All tests passed successfully! :-)
```
