#!/usr/bin/python3

import argparse
import copy
import os
import random
import re
import subprocess
import sys


# Note: "Internally, a list is represented as an array [...]"
# So getting and setting an item is in O(1).
# cf. https://wiki.python.org/moin/TimeComplexity

# sequence types documentation:
# https://docs.python.org/3/library/stdtypes.html#typesseq


class InvalidStateException(Exception):
    message = "algorithm input not initialized correctly"

    def __str__(self):
        return InvalidStateException.message


class Alg:
    # number of iterations in test() if any
    iterations = 100
    threshold_seq_length = 1000
    # threshold_item_value must be >= threshold_seq_length
    threshold_item_value = 10000
    # name of specific Algorithm
    name = None

    def __init__(self, *args):
        # are there valid values as input?
        self.input_initialized = False
        # initialize result
        self.result = None

    def get_result(self):
        return copy.copy(self.result)

    def _print_result(self, ok, expected = None, was = None):
        if ok:
            if debug and expected is not None and was is not None:
                print("{} - correct result - expected and was: {}"
                      .format(self.name, self.result))
            elif debug:
                print(self.name + " - correct result")
            return
        elif expected is not None and was is not None:
            print("{} - wrong result - expected: {}, but was: {}"
                  .format(self.name, expected, was))
        else:
            print(self.name + " - wrong result")

    def set_input(self, *args):
        raise NotImplementedError()

    # common run wrapper
    def run(self):
        if not self.input_initialized:
            raise InvalidStateException()
        self._run()

    # do the actual work
    def _run(self):
        raise NotImplementedError()

    def test(self, output = False, *args):
        raise NotImplementedError()

    def verify_result(self, output = False, *args):
        raise NotImplementedError()


class SearchAlg(Alg):
    def __init__(self, seq = None, element = None, *args):
        super().__init__(args)
        self.set_input(seq, element, args)

    # static method
    def search(seq, element):
        raise NotImplementedError()

    def _run(self):
        # call appropriate static method - hack ?
        self.result = type(self).search(self.seq, self.element)

    def set_input(self, seq, element, *args):
        if seq is None or element is None:
            self.seq = None
            self.element = None
            return
        self.seq = copy.copy(seq)
        self.element = element
        self.input_initialized = True

    def test(self, output = False, *args):
        for i in range(Alg.iterations):
            # random sequence length
            seq_length = random.randrange(1, Alg.threshold_seq_length)
            # generate random sequence
            seq = random.sample(range(Alg.threshold_item_value), seq_length)

            # cf. https://stackoverflow.com/a/34371844
            element = random.choice(list(set(range(Alg.threshold_item_value))-set(seq)))
            self.set_input(seq, element)

            self.run()

            print("Tested {} on {} sequences with up to {} elements in the range 0 - {}."
                  .format(self.name, Alg.iterations, Alg.threshold_seq_length,
                          Alg.threshold_item_value))
            return self.verify_result(output = output)

    def verify_result(self, output = False, *args):
        if self.element in self.seq:
            ret = self.seq[self.result] == self.element
            if output:
                self._print_result(ret)
        else:
            ret = self.result == -1
            if output:
                self._print_result(ret, -1, self.result)
        return ret


class SortAlg(Alg):
    def __init__(self, seq = None, *args):
        super().__init__()
        self.set_input(seq, args)

    def set_input(self, seq, *args):
        if seq is None:
            self.seq = None
            return
        # convert (and copy) to list
        self.seq = list(seq)
        self.input_initialized = True

    def _run(self):
        self.sort(self.seq)

    def sort(self, seq):
        raise NotImplementedError()

    def test(self, output = False, *args):
        for i in range(Alg.iterations):
            # random sequence length
            seq_length = random.randrange(1, Alg.threshold_seq_length)
            # generate random sequence
            seq = random.sample(range(Alg.threshold_item_value), seq_length)

            self.set_input(seq)

            self.run()

            print("Tested {} on {} sequences with up to {} elements in the range 0 - {}."
                  .format(self.name, Alg.iterations, Alg.threshold_seq_length,
                          Alg.threshold_item_value))
            return self.verify_result(expected = sorted(seq), output = output)

    def verify_result(self, expected = None, output = False, *args):
        if expected is None:
            return

        ret = expected == self.result
        if output:
            self._print_result(ret, expected, self.result)

        return ret


class BinarySearch(SearchAlg):
    name = "BinarySearch"

    def set_input(self, seq, element, *args):
        super().set_input(seq, element)
        if not self.input_initialized:
            return
        # binary search requires the sequence to be sorted
        self.seq = sorted(self.seq)

    # static method
    def search(seq, element):
        return BinarySearch.__search(seq, element, 0, len(seq) - 1)

    # static method
    def __search(seq, element, low, high):
        if low > high:
            return -1
        mid = int((low + high) / 2)
        if seq[mid] > element:
            return BinarySearch.__search(seq, element, low, mid - 1)
        elif seq[mid] < element:
            return BinarySearch.__search(seq, element, mid + 1, high)
        else:
            return mid


class MergeSort(SortAlg):
    name = "MergeSort"

    def set_input(self, seq, *args):
        super().set_input(seq)
        if not self.input_initialized:
            return
        # initialize helper sequence
        self.result = [None] * len(seq)

    def sort(self, seq):
        self.__sort(seq, 0, len(seq) - 1)

    def __sort(self, seq, low, high):
        if low > high:
            return
        elif low == high:
            self.result[low] = self.seq[low]
            return
        mid = int((low + high) / 2)

        self.__sort(seq, low, mid)
        self.__sort(seq, mid + 1, high)

        i = low
        j = mid + 1

        for k in range(low, high + 1):
            if i <= mid and (j > high or self.seq[i] <= self.seq[j]):
                self.result[k] = self.seq[i]
                i += 1
            elif j <= high:
                self.result[k] = self.seq[j]
                j += 1

        # reflect changes in original sequence
        for i in range(low, high + 1):
            self.seq[i] = self.result[i]


# Search recursively for files in "dir" which contain regex "reg" and display
# the number of matching lines per file.
# (The matching is line based, without multiline support.)
class FindFilesContainingRegex(Alg):
    name = "FindFilesContainingRegex"

    def __init__(self, dir = None, reg = None, *args):
        super().__init__(args)
        self.set_input(dir, reg, args)
        # for verify_result() as grep can tolerate more file types
        self.processed_files = []
        self.matches = 0

    def __process_file(self, file):
        # force utf-8 encoding
        with open(file, "r", encoding = "utf-8") as f:
            # initialize result entry
            self.result[file] = 0
            for line in f:
                if self.pat.search(line):
                    self.result[file] += 1
            self.processed_files.append(file)
            self.matches += self.result[file]

    def _run(self):
        self.result = {}

        if not os.path.exists(self.dir):
            return
        elif not os.path.isdir(self.dir):
            self.__process_file(self.dir)
            return

        for (dirpath, dirnames, filenames) in os.walk(self.dir):
            for files in filenames:
                # get path to that file
                file = os.path.join(dirpath, files)
                try:
                    self.__process_file(file)
                except:
                    # file was not processed correctly
                    del self.result[file]

    def set_input(self, dir, reg, *args):
        if dir is None or reg is None or reg == "":
            self.dir = None
            self.reg = None
            return
        self.dir = os.path.expanduser(dir)
        # compile to Pattern
        self.pat = re.compile(reg)
        self.input_initialized = True

    def test(self, output = False, *args):
        if self.dir is None:
            try:
                dir = input(self.name + " - Please provide a directory for the test: ")
            except Exception as e:
                sys.exit(e)
        if self.reg is None:
            try:
                regex = input(self.name + " - Please provide a regex to search for: ")
            except Exception as e:
                sys.exit(e)

        self.set_input(dir, regex)
        self.run()

        print("Tested {} on {} files with a total of {} matches."
              .format(self.name, len(self.processed_files), self.matches))
        return self.verify_result(output = output)

    def verify_result(self, output = False, *args):
        if not self.processed_files:
            if output:
                self._print_result(ok = True, expected = dict(), was = self.result)
            return True

        try:
            # grep: print for every file the number of matching lines in the format
            # "filename:COUNT". Use extended regex.
            proc = subprocess.run(["grep", "-acEH", self.pat.pattern]
                                  + self.processed_files,
                                  stdout = subprocess.PIPE, encoding = "utf-8")
        except Exception as e:
            print(self.name + " - could not verify result: " + str(e))
            return

        # initialize
        tmp = self.result.copy()

        for line in proc.stdout.splitlines():
            # consider the case that the filename contains ":"!
            (file, count) = line.rsplit(":", 1)
            tmp[file] = int(count)

        ret = tmp == self.result

        if output:
            self._print_result(ok = ret, expected = tmp, was = self.result)

        return ret



# global variables
debug = False
description = "Run some algorithms and let them test themselves automatically "\
        + "with random input."


# command line argument parsing
argument_parser = argparse.ArgumentParser(description = description)
argument_parser.add_argument(
    "-d", "--debug",
    help = "genrate additional output for debugging purposes (implies -o)",
    action = "store_true")
argument_parser.add_argument(
    "-o", "--output",
    help = "inform on wrong results",
    action = "store_true")
args = argument_parser.parse_args()
debug = args.debug


# execution
algorithms = [ BinarySearch(), MergeSort(), FindFilesContainingRegex() ]
result = True

for alg in algorithms:
    ret = alg.test(output = args.output or args.debug)
    if ret:
        print("\033[32m{} - test passed\033[00m".format(alg.name))
    else:
        print("\033[31m{} - test failed\033[00m".format(alg.name))
        result = False

if result:
    print("All tests passed successfully! :-)")
else:
    print("Some tests failed! :-(")
