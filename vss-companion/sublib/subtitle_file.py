import os
import re

from fractions import Fraction
from functools import partial

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
from util.update_text import undo_space_changes
from util.detect_encoding import detect_encoding

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
            encoding = detect_encoding(buf, cls.DEFAULT_ENCODING)
        text = buf.decode(encoding, "replace")
        return text, encoding, language

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

    # TODO: use ffms
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
        self.update_transcript(value, False)

    @property
    def reformatted_transcript(self):
        # TODO
        transcript = self.transcript
        return transcript

        # transcript = self.transcript
        # transcript = re.sub(r"([^\.?!])\n", r"\1 ", transcript)
        # transcript = re.sub(r"(…|\.{3})\n([a-zß-öø-ÿœ])", r"\1 \2", transcript)

        def no_quotes(text, chars="'\"‘’“”„‹›«»\xa0\u202f "):
            return text.strip(chars)

        def is_sentence_end(text, pattern=re.compile(r'[\.?!]$')):
            return bool(pattern.search(no_quotes(text)))

        # def is_sentence_start(text):
            # return no_quotes(text)[0:1].isupper()

        def is_stand_alone(text,
                pattern=re.compile(r"(https?://)?(www\.)?\w+\.\w+")):
            return bool(pattern.match(text)) or text.isupper()

        def is_far_subtitle(subtitle, max_interval=2000):
            return (subtitle.time_to_previous is not None and
                    subtitle.time_to_previous > max_interval)

        # def is_in_italics(text,
                # pattern=re.compile(r"^(\{\.*?})?<i>.*</i>({\pub})?$", re.I):
            # return pattern.match(text)

        lines = []
        current_line = []

        def flush_lines():
            lines.append(" ".join(current_line))
            current_line.clear()

        for subtitle in self:
            for n, line in enumerate(subtitle.plain_lines_no_dialog):
                if not line:
                    continue
                if n == 0 and is_far_subtitle(subtitle) and current_line:
                    flush_lines()
                if is_stand_alone(line):
                    if current_line:
                        flush_lines()
                    lines.append(line)
                else:
                    current_line.append(line)
                    if is_sentence_end(line):
                        flush_lines()
        if current_line:
            flush_lines()

        transcript = "\n".join(lines) + "\n"
        transcript = re.sub(r"(…|\.{3})\n([a-zß-öø-ÿœ])", r"\1 \2", transcript)
        return transcript

    @reformatted_transcript.setter
    def reformatted_transcript(self, value):
        self.update_transcript(value, True)

    def update_transcript(self, transcript, reformat=True):
        old_transcript = self.transcript
        if reformat:
            transcript = undo_space_changes(transcript, old_transcript)
        changes = 0
        if old_transcript != transcript:
            old_transcript_lines = old_transcript.split("\n")
            transcript_lines = transcript.split("\n")
            if len(old_transcript_lines) != len(transcript_lines):
                raise ValueError("number of lines mismatch: {}, {}"
                                 .format(len(old_transcript_lines),
                                         len(transcript_lines)))
            n = 0
            for subtitle in self:
                num_lines = len(subtitle.lines)
                old_text = subtitle.plain_text_no_dialog
                new_text = "\n".join(transcript_lines[n:n+num_lines])
                if old_text.strip() and not new_text.strip():
                    raise ValueError(
                        "change aborted: {}: {!r} -> {!r}".format(
                            subtitle.identifier, old_text, new_text
                        )
                    )
                if old_text != new_text:
                    subtitle.plain_text_no_dialog = new_text
                    changes += 1
                n += num_lines
        return changes
