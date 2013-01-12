import re

from collections import namedtuple
from functools import total_ordering, partial


HALF_A_SECOND = 500
ONE_SECOND = 1000
ONE_MINUTE = ONE_SECOND * 60
ONE_HOUR = ONE_MINUTE * 60


@total_ordering
class Time(int):
    FORMAT = "{h:02d}:{min:02d}:{s:06.3f}"
    #PATTERN = r"(\d+)\D+(\d+)\D+(\d+)\D+(\d+)"

    _decimal_mark_sub = partial(re.compile(r"[,:]").sub, ".", count=1)

    @classmethod
    def from_int(cls, n):
        return cls(n)

    @classmethod
    def from_h_min_s(cls, h, min, s): #@ReservedAssignment
        return cls(h * ONE_HOUR + min * ONE_MINUTE + round(s * ONE_SECOND))

    @classmethod
    def from_str(cls, text):
        units = text.split(":", 2)
        if len(units) < 3:
            units = [0] * (3 - units_len) + units
        return cls.from_h_min_s(int(units[0]), int(units[1]),
                                float(cls._decimal_mark_sub(units[2])))

    def __repr__(self):
        return "{}({!r})".format(self.__class__.__name__, int(self))

    def __str__(self):
        return self.FORMAT.format_map(self.to_h_min_s().__dict__)

    def to_h_min_s(self):
        #if self < 0:
            #raise ValueError("negative time value: {!r}".format(int(self)))
        h, remainder = divmod(self, ONE_HOUR)
        min, ms = divmod(remainder, ONE_MINUTE) #@ReservedAssignment
        return HoursMinutesSeconds(h, min, ms / ONE_SECOND)

    def __add__(self, other):
        return Time(int(self) + other)

    def __sub__(self, other):
        return self + -other


HoursMinutesSeconds = namedtuple("HoursMinutesSeconds", ("h", "min", "s"))
