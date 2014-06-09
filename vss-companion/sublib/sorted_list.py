#!/usr/bin/env python3

import copy
import sys

from bisect import bisect_left, bisect_right, insort_right
from collections import MutableSet, Sequence
from functools import total_ordering


@total_ordering
class SortedList(Sequence, MutableSet):
    def __init__(self, iterable=None):
        if iterable is None:
            iterable = []
        self.data = sorted(iterable)

    def __repr__(self):
        return "{}({!r})".format(self.__class__.__name__, self.data)

    def __eq__(self, other):
        return self.data == other

    def __lt__(self, other):
        return self.data < other

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        if isinstance(index, slice):
            sliced = copy.copy(self)
            sliced.data = self.data[index]
            return sliced
        return self.data[index]

    def __delitem__(self, index):
        del self.data[index]

    def __contains__(self, value):
        return self._get_index(value)[1]

    def index(self, value, start=0, end=None):
        index, is_present = self._get_index(value, start, end)
        if is_present:
            return index
        raise ValueError("{} is not in sequence".format(value))

    def add(self, value):
        insort_right(self.data, value)

    def remove(self, value):
        index, is_present = self._get_index(value)
        if not is_present:
            raise KeyError(value)
        del self.data[index]

    def discard(self, value):
        index, is_present = self._get_index(value)
        if is_present:
            del self.data[index]

    def pop(self, index=-1):
        value = self.data[index]
        del self.data[index]
        return value

    def clear(self):
        self.data[:] = []

    def copy(self):
        return self[:]

    def update(self, iterable):
        self.data += list(iterable)
        self.data.sort()

    def sort(self):
        self.data.sort()

    def bisect_left(self, value, lo=0, hi=None):
        if hi is None:
            hi = len(self)
        return bisect_left(self.data, value, lo, hi)

    def bisect_right(self, value, lo=0, hi=None):
        if hi is None:
            hi = len(self)
        return bisect_right(self.data, value, lo, hi)

    def _get_index(self, value, start=0, end=None):
        if start < 0:
            start += len(self.data)
        if end is None:
            end = len(self.data)
        elif end < 0:
            end += len(self.data)
        index = bisect_left(self.data, value, start, end)
        return index, index < end and self.data[index] == value


class SortedSet(SortedList):
    def __init__(self, iterable=None):
        if iterable is None:
            iterable = []
        super().__init__(set(iterable))

    def add(self, value):
        index, is_present = self._get_index(value)
        if not is_present:
            self.data.insert(index, value)

    def update(self, iterable):
        self.data = sorted(set(self.data + list(iterable)))


def main():
    import random
    sl = SortedList()
    for n in range(100):
        sl.add(random.randint(0, 99))
    print(sl)


if __name__ == "__main__":
    sys.exit(main())
