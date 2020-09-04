#!/usr/bin/python3

import argparse
import copy
import decimal
import math
import numpy
import os
import random
import re
import subprocess
import sys

from decimal import Decimal
from numbers import Number
from typing import Any, List, Sequence, TypeVar


# Note: "Internally, a list is represented as an array [...]"
# So getting and setting an item is in O(1).
# cf. https://wiki.python.org/moin/TimeComplexity

# sequence types documentation:
# https://docs.python.org/3/library/stdtypes.html#typesseq


T = TypeVar("T")


# helper class
class InvalidStateException(Exception):
    message = "algorithm input not initialized correctly"

    def __str__(self):
        return InvalidStateException.message


# helper class
class DecimalRand:
    @staticmethod
    def sample(max_item_value: int, seq_length: int) -> List[Decimal]:
        return [
            # no need to use str -> Decimal to convert floats correctly
            # because random.sample() returns ints
            Decimal(i)
            for i in random.sample(range(max_item_value), seq_length)
        ]


class Alg:
    # number of iterations in test() if any
    iterations = 100
    max_seq_length = 1000
    # max_item_value must be >= max_seq_length
    max_item_value = 10000

    def __init__(self, *args) -> None:
        # are there valid values as input?
        self.input_initialized = False
        # initialize result
        self.result = None

    def get_result(self) -> Any:
        return copy.copy(self.result)

    def _print_result(self, ok: bool, expected: Any = None,
                      was: Any = None) -> None:
        if ok:
            if debug and expected is not None and was is not None:
                print("{} - correct result - expected and was: {}"
                      .format(type(self).__name__, self.result))
            elif debug:
                print(type(self).__name__ + " - correct result")
            return
        elif expected is not None and was is not None:
            print("{} - wrong result - expected: {}, but was: {}"
                  .format(type(self).__name__, expected, was))
        else:
            print(type(self).__name__ + " - wrong result")

    def set_input(self, *args) -> None:
        raise NotImplementedError()

    # common run wrapper
    def run(self) -> None:
        if not self.input_initialized:
            raise InvalidStateException()
        self._run()

    # do the actual work
    def _run(self) -> None:
        raise NotImplementedError()

    def test(self, output: bool = False, *args) -> bool:
        raise NotImplementedError()

    def verify_result(self, output: bool = False, *args) -> bool:
        raise NotImplementedError()


class SearchAlg(Alg):
    def __init__(self, seq: Sequence[T] = None, element: T = None,
                 *args) -> None:
        super().__init__(args)
        self.set_input(seq, element, args)

    @staticmethod
    def search(seq: Sequence[T], element) -> int:
        raise NotImplementedError()

    def _run(self) -> None:
        # call appropriate static method - hack ?
        self.result = type(self).search(self.seq, self.element)

    def set_input(self, seq: Sequence[T], element: T, *args) -> None:
        if seq is None or element is None:
            self.seq = None
            self.element = None
            return
        self.seq = copy.copy(seq)
        self.element = element
        self.input_initialized = True

    def test(self, output: bool = False, *args) -> bool:
        ret = True

        for i in range(type(self).iterations):
            # random sequence length
            seq_length = random.randrange(1, type(self).max_seq_length)
            # generate random sequence
            seq = random.sample(range(type(self).max_item_value), seq_length)

            # cf. https://stackoverflow.com/a/34371844
            element = random.choice(list(
                set(range(type(self).max_item_value))-set(seq)
            ))
            self.set_input(seq, element)

            self.run()

            if not self.verify_result(output = output):
                ret = False

        print("Tested {} on {} sequences with up to {} elements in the range "
               "0 - {}.".format(type(self).__name__, type(self).iterations,
                                 type(self).max_seq_length,
                                 type(self).max_item_value))
        return ret

    def verify_result(self, output: bool = False, *args) -> bool:
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
    def __init__(self, seq: Sequence = None, *args) -> None:
        super().__init__(args)
        self.set_input(seq, args)

    def set_input(self, seq: Sequence, *args) -> None:
        if seq is None:
            self.seq = None
            return
        # convert (and copy) to list
        self.seq = list(seq)
        self.input_initialized = True

    def _run(self) -> None:
        self.sort(self.seq)

    def sort(self, seq: Sequence) -> None:
        raise NotImplementedError()

    def test(self, output: bool = False, *args) -> bool:
        ret = True

        for i in range(type(self).iterations):
            # random sequence length
            seq_length = random.randrange(1, type(self).max_seq_length)
            # generate random sequence
            seq = random.sample(range(type(self).max_item_value), seq_length)

            self.set_input(seq)

            self.run()

            if not self.verify_result(expected = sorted(seq), output = output):
                ret = False

        print("Tested {} on {} sequences with up to {} elements in the range "
               "0 - {}.".format(type(self).__name__, type(self).iterations,
                                type(self).max_seq_length,
                                type(self).max_item_value))
        return ret

    def verify_result(self, expected: Any = None, output: bool = False,
                      *args) -> bool:
        if expected is None:
            return

        ret = expected == self.result
        if output:
            self._print_result(ret, expected, self.result)

        return ret


class BinarySearch(SearchAlg):
    def set_input(self, seq: Sequence[T], element: T, *args) -> None:
        super().set_input(seq, element)
        if not self.input_initialized:
            return
        # binary search requires the sequence to be sorted
        self.seq = sorted(self.seq)

    @staticmethod
    def search(seq: Sequence[T], element: T) -> int:
        return BinarySearch.__search(seq, element, 0, len(seq) - 1)

    @staticmethod
    def __search(seq: Sequence[T], element: T, low: int, high: int) -> int:
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
    def set_input(self, seq: Sequence[T], *args) -> None:
        super().set_input(seq)
        if not self.input_initialized:
            return
        # initialize helper sequence
        self.result = [None] * len(seq)

    def sort(self, seq: Sequence[T]) -> None:
        self.__sort(seq, 0, len(seq) - 1)

    def __sort(self, seq: Sequence[T], low: int, high: int) -> None:
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
    def __init__(self, dir: os.PathLike = None, reg: str = None, *args) -> None:
        super().__init__(args)
        self.set_input(dir, reg, args)
        # for verify_result() as grep can tolerate more file types
        self.processed_files = []
        self.matches = 0

    def __process_file(self, file: os.PathLike) -> None:
        try:
            # initialize result entry
            self.result[file] = 0
            # force utf-8 encoding
            with open(file, "r", encoding = "utf-8") as f:
                for line in f:
                    if self.pat.search(line):
                        self.result[file] += 1
                self.processed_files.append(file)
                self.matches += self.result[file]
        except:
            del self.result[file]

    def _run(self) -> None:
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
                self.__process_file(file)

    def set_input(self, dir: os.PathLike, reg: str, *args) -> None:
        if dir is None or reg is None or reg == "":
            self.dir = None
            self.reg = None
            return
        self.dir = os.path.expanduser(dir)
        # compile to Pattern
        self.pat = re.compile(reg)
        self.input_initialized = True

    def test(self, output: bool = False, *args) -> bool:
        if self.dir is None:
            try:
                dir = input(type(self).__name__
                            + " - Please provide a directory for the test: ")
            except Exception as e:
                sys.exit(e)
        if self.reg is None:
            try:
                regex = input(type(self).__name__
                              + " - Please provide a regex to search for: ")
            except Exception as e:
                sys.exit(e)

        self.set_input(dir, regex)
        self.run()

        print("Tested {} on {} files with a total of {} matches."
              .format(type(self).__name__, len(self.processed_files), self.matches))
        return self.verify_result(output = output)

    def verify_result(self, output: bool = False, *args) -> bool:
        if not self.processed_files:
            if output:
                self._print_result(ok = True, expected = dict(),
                                   was = self.result)
            return True

        try:
            # grep: print for every file the number of matching lines in the format
            # "filename:COUNT". Use extended regex.
            proc = subprocess.run(["grep", "-acEH", self.pat.pattern]
                                  + self.processed_files,
                                  stdout = subprocess.PIPE, encoding = "utf-8")
        except Exception as e:
            print(type(self).__name__ + " - could not verify result: " + str(e))
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


# Does this algorithm has a fixed number of vectors to operate on?
# A value of vec_num = 0 means no, any other value represents this number.
# (cf. child classes)
# "max_vec_num" does not have any affect in classes where vec_num != 0.
class MultiVectorAlg(Alg):
    vec_num = 0
    max_vec_num = 100

    def __init__(self, vectors: List[Sequence[Decimal]] = None, *args) -> None:
        super().__init__(args)
        self.set_input(vectors, *args)

    def set_input(self, vectors: List[Sequence[Decimal]], *args) -> None:
        if vectors is None:
            return
        elif (len(vectors) == 0 or
              (type(self).vec_num != 0 and len(vectors) != type(self).vec_num)):
            print(len(vectors))
            print(type(self).vec_num)
            raise InvalidStateException()
        # verify if every vector has the same length
        length = len(vectors[0])
        for vec in vectors:
            if len(vec) != length:
                raise InvalidStateException()
        self.vectors = vectors
        self.vectors_np = [ numpy.array(v) for v in vectors ]
        self.input_initialized = True

    def test(self, output: bool = False, *args) -> bool:
        ret = True

        for i in range(type(self).iterations):
            # random vector length
            vec_length = random.randrange(1, type(self).max_seq_length)
            # random number of vectors if type(self).vec_num == 0
            if type(self).vec_num == 0:
                vec_num = random.randrange(1, type(self).max_vec_num)
            else:
                vec_num = type(self).vec_num
            # generate random vectors
            vectors = [
                DecimalRand.sample(type(self).max_item_value, vec_length)
                for i in range(vec_num)
            ]

            self.set_input(vectors)
            self.run()

            if not self.verify_result(output = output):
                ret = False

        print("Tested {} in {} iterations using {} vectors with each up "
              "to {} elements in the range 0 - {}."
              .format(type(self).__name__, type(self).iterations,
                      "up to {}".format(type(self).max_vec_num)
                      if type(self).vec_num == 0 else type(self).vec_num,
                      type(self).max_seq_length, type(self).max_item_value))
        return ret


class OneVectorAlg(MultiVectorAlg):
    vec_num = 1

    def set_input(self, vectors: List[Sequence[Decimal]], *args) -> None:
        super().set_input(vectors)
        if not self.input_initialized:
            return
        # alias for our vector
        self.a = self.vectors[0]
        self.a_np = self.vectors_np[0]


class TwoVectorAlg(MultiVectorAlg):
    vec_num = 2

    def set_input(self, vectors: List[Sequence[Decimal]], *args) -> None:
        super().set_input(vectors)
        if not self.input_initialized:
            return
        # aliases for the two vectors
        self.a = self.vectors[0]
        self.b = self.vectors[1]
        self.a_np = self.vectors_np[0]
        self.b_np = self.vectors_np[1]


class DotProduct(TwoVectorAlg):
    def _run(self) -> None:
        self.result = numpy.dot(self.a_np, self.b_np)

    def verify_result(self, output: bool = False, *args) -> bool:
        tmp = Decimal(0)
        for i in range(len(self.a)):
            tmp += self.a[i] * self.b[i]

        ret = tmp == self.result

        if output:
            self._print_result(ret, expected = tmp, was = self.result)

        return ret


# norm?
class VectorLength(OneVectorAlg):
    def _run(self) -> None:
        self.result = Decimal.sqrt(self.a_np.dot(self.a_np))


class VectorAddition(MultiVectorAlg):
    def _run(self) -> None:
        self.result = numpy.zeros(self.vectors_np[0].size, dtype = Decimal)
        for vec in self.vectors_np:
            self.result += vec

    def verify_result(self, output: bool = False, *args) -> bool:
        tmp = [Decimal(0)] * len(self.vectors[0])
        for vec in self.vectors:
            tmp = [ i + j for i, j in zip(tmp, vec) ]

        ret = (numpy.array(tmp) == self.result).all()
        if output:
            self._print_result(ret, expected = tmp, was = self.result)

        return ret


class VectorMultiplication(MultiVectorAlg):
    def _run(self) -> None:
        self.result = numpy.ones(self.vectors_np[0].size, dtype = Decimal)
        for vec in self.vectors_np:
            self.result *= vec

    def verify_result(self, output: bool = False, *args) -> bool:
        tmp = [Decimal(1)] * len(self.vectors[0])
        for vec in self.vectors:
            tmp = [ i * j for i, j in zip(tmp, vec) ]

        ret = (numpy.array(tmp) == self.result).all()
        if output:
            self._print_result(ret, expected = tmp, was = self.result)

        return ret

# result: Orthonormalbasis
class GramSchmidt(MultiVectorAlg):
    max_vec_num = 25

    def _run(self) -> None:
        self.result = []
        dp = DotProduct()
        vl = VectorLength()
        m = 0
        for vec in self.vectors_np:
            tmp = numpy.array(vec)
            for o_vec in self.result:
                dp.set_input([o_vec, vec])
                dp.run()
                tmp = tmp - dp.get_result() * o_vec
            vl.set_input([tmp])
            vl.run()
            n = vl.get_result()
            if math.isclose(n, 0, abs_tol = 1e-10):
                continue
            self.result.append(tmp / n)
            m += 1

    # Note: This does not really verify the result :-) (TODO)
    def verify_result(self, output: bool = False, *args) -> bool:
        ret = True
        vl = VectorLength([self.result[0]])

        for i in range(1, len(self.result)):
            vl.set_input([self.result[i]])
            vl.run()
            if not math.isclose(vl.get_result(), 1):
                print("huhu: {}".format(vl.get_result()))
                ret = False
                break
            if i == 0:
                continue
            if not math.isclose(numpy.dot(self.result[i-1], self.result[i]), 0,
                               abs_tol = 1e-10):
                print(numpy.dot(self.result[i-1], self.result[i]))
                ret = False
                break
        
        if output:
            self._print_result(ret)

        return ret


# settings
#decimal.getcontext().prec = 50 # default = 28

# global variables
debug: bool = False
description: str = (
    "Run some algorithms and let them test themselves automatically with "
    "random input."
)


# command line argument parsing
argument_parser = argparse.ArgumentParser(description = description)
argument_parser.add_argument(
    "-d", "--debug",
    help = "genrate additional output for debugging purposes (implies -o)",
    action = "store_true")
argument_parser.add_argument(
    "-n", "--numpy",
    help = "play with numpy algorithms",
    action = "store_true")
argument_parser.add_argument(
    "-o", "--output",
    help = "inform on wrong results",
    action = "store_true")
args = argument_parser.parse_args()
debug = args.debug


# execution
if args.numpy:
    algorithms = [ DotProduct(), VectorAddition(), VectorMultiplication(),
                  GramSchmidt() ]
else:
    algorithms = [ BinarySearch(), MergeSort(), FindFilesContainingRegex() ]
result = True

for alg in algorithms:
    ret = alg.test(output = args.output or args.debug)
    if ret:
        print("\033[32m{} - test passed\033[00m".format(type(alg).__name__))
    else:
        print("\033[31m{} - test failed\033[00m".format(type(alg).__name__))
        result = False

if result:
    print("All tests passed successfully! :-)")
else:
    print("Some tests failed! :-(")
