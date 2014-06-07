import bisect
#import collections
import io
import os
import re

import codecs
from collections import Sequence, MutableSet
from fractions import Fraction
from functools import total_ordering, partial

try:
    from guess_language import guess_language
except ImportError:
    guess_language = None
try:
    import translit
except ImportError:
    translit = None
try:
    from util.guess_text import guess_text
except ImportError:
    guess_text = None
from util.update_text import update_text

from .subtitle import Subtitle
from .style import Style
from .sorted_list import SortedList

from .errors import *


class SubtitleFile(SortedList):
    DEFAULT_ENCODING = "utf-8"
    DEFAULT_NEWLINE = "\r\n"
    Subtitle = Subtitle
    NEWLINE_SUB = partial(re.compile("\r\n?").sub, "\n")

    def __init__(self, text="", file=None, encoding=None,
                 language=None, fps=None):
        self.Subtitle = self.Subtitle.factory(self)
        self.indexes = {}
        # self._transcript = None
        self.style_list = [Style()]
        self.file = file
        self.encoding = encoding or self.DEFAULT_ENCODING
        self.newline = self.DEFAULT_NEWLINE
        self.language = language
        self._set_fps(fps)
        self.is_sorted = True
        super().__init__(self.parse_text(text))

    @classmethod
    def load(cls, file, encoding=None, language=None, fps=None):
        basename, ext = os.path.splitext(file)
        try:
            sub_cls = cls.formats[ext]
        except KeyError:
            raise UnsupportedFormat

        with open(file, "rb") as f:
            buf = f.read()

        text, encoding, language = sub_cls._decode_buf(buf, encoding, language)
        return sub_cls(text, file, encoding, language, fps)

    def save(self, file=None, encoding=None, errors=None):
        if not file:
            file = self.file
        if not encoding:
            encoding = self.encoding
        with open(file, "w", encoding=encoding,
                  errors=errors, newline=self.newline) as f:
            f.write(str(self))

    @classmethod
    def _decode_buf(cls, buf, encoding, language):
        if encoding == "auto":
            if not guess_text:
                raise Error("'guess_text' module not available")
            return guess_text(buf, encoding, language)
        if not encoding:
            encoding = cls._detect_encoding(buf)
        text = buf.decode(encoding, "replace")
        return text, encoding, language

    @classmethod
    def _detect_encoding(cls, buf):
        if buf.startswith(codecs.BOM_UTF8):
            encoding = "utf-8-sig"
        elif (buf.startswith(codecs.BOM_UTF32_LE) or
              buf.startswith(codecs.BOM_UTF32_BE)):
            encoding = "utf-32"
        elif (buf.startswith(codecs.BOM_UTF16_LE) or
              buf.startswith(codecs.BOM_UTF16_LE)):
            encoding = "utf-16"
        else:
            encoding = cls.DEFAULT_ENCODING
        return encoding

    def __repr__(self):
        return "{}({!r}, {!r}, {!r}, {!r}, {!r})".format(
            self.__class__.__name__,
            str(self), self.file, self.encoding, self.language, self.fps)

    def __str__(self):
        return "\n".join(str(subtitle) for subtitle in self) + "\n"

    def __getitem__(self, index):
        if isinstance(index, slice):
            o = self.__new__(self.__class__)
            o.data = self.data[index]
            o.file = self.file
            o.encoding = self.encoding
            o.language = self.language
            o.fps = self.fps
            return o
        return self.data[index]

    def __delitem__(self, key):
        super().__delitem__(key)
        self.indexes.clear()

    def add(self, value):
        super().add(value)
        self.indexes.clear()

    def remove(self, value):
        super().remove(value)
        self.indexes.clear()

    def discard(self, value):
        super().discard(value)
        self.indexes.clear()

    def pop(self, index=-1):
        value = super().pop(index)
        self.indexes.clear()
        return value

    def clear(self):
        super().clear()
        self.indexes.clear()

    def update(self, iterable):
        super().update(iterable)
        self.indexes.clear()

    def sort(self):
        super().sort()
        self.indexes.clear()

    def parse_text(self, text):
        matches = self.Subtitle.PARSER.finditer(self.NEWLINE_SUB(text))
        return (self.Subtitle.from_match(match) for match in matches)

    def get_index(self, subtitle):
        if not self.indexes:
            for n, sub in enumerate(self):
                self.indexes[id(sub)] = n
        try:
            return self.indexes[id(subtitle)]
        except KeyError:
            return None

    # def _clear_caches(self):
        # self.indexes.clear()
        # self._transcript = None

    @property
    def fps(self):
        return self._fps

    @fps.setter
    def fps(self, value):
        times_list = [subtitle.times for subtitle in self]
        self._set_fps(value)
        for subtitle, times in zip(self, times_list):
            subtitle.times = times
        self.is_sorted = True

    def _set_fps(self, value):
        self._frame_duration = (1 if not value else
                                1000 * value.denominator / value.numerator)
        self._fps = value

    def assume_fps(self, fps):
        if not self._fps:
            raise ValueError("original FPS was not set")
        self._frame_duration = 1000 * fps.denominator / fps.numerator
        self._fps = fps

    @property
    def frame_duration(self):
        return self._frame_duration

    @frame_duration.setter
    def frame_duration(self, value):
        self.fps = 1000 / (value if isinstance(value, Fraction) else
                           Fraction(value).limit_denominator())

    def shift(self, time):
        for subtitle in self:
            subtitle.times = [t + time for t in subtitle.times]

    def shift_frames(self, frames):
        for subtitle in self:
            subtitle.frames = [f + frames for f in subtitle.frames]

    if translit:
        def upgrade(self):
            for subtitle in self:
                t = translit.upgrade(subtitle.plain_text, self.language)
                subtitle.plain_text = t

    if guess_language:
        def guess_language(self):
            self.language = guess_language(self.transcript)
            return self.language

    @property
    def transcript(self):
        return "\n".join(subtitle.plain_text_no_dialog for subtitle in self)

    @transcript.setter
    def transcript(self, value):
        value = value.split("\n")
        old_len = len(self.transcript.split("\n"))
        if old_len != len(value):
            raise ValueError("number of lines has changed: {}, {}"
                             .format(old_len, len(value)))
        n = 0
        for subtitle in self:
            num_lines = len(subtitle.lines)
            old_text = subtitle.plain_text_no_dialog
            new_text = "\n".join(value[n:n+num_lines])
            if (old_text != new_text):
                subtitle.plain_text_no_dialog = new_text
            n += num_lines
