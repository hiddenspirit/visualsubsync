import functools
import re

from collections import namedtuple

from util.get_text_width import get_text_width_pixels, get_text_width_chars
from util.update_text import update_text

from .time import Time, HALF_A_SECOND, ONE_SECOND


PARSER = (r"^(?P<start_time>{time_pattern})\D+(?P<end_time>{time_pattern}).*$"
          r"\n(?P<text>[\s\S]*?)(?:\n\n|\n*\Z)")


@functools.total_ordering
class Subtitle:
    Time = Time
    FORMAT_BASE = "{start_time} --> {end_time}\n{text}\n"
    FORMAT = "{identifier}\n" + FORMAT_BASE
    PARSER = re.compile(PARSER.format(time_pattern=Time.PATTERN), re.MULTILINE)

    TAG_RE = re.compile(r"<.*?>|\{.*?\}")
    STRIP_DIALOG_SUB = (re.compile(r"^[\-–—]\s", re.MULTILINE), "")
    ADD_DIALOG_SUB = (re.compile(r"^(?![\-–—]\s)", re.MULTILINE), "– ")

    @classmethod
    def factory(cls, subtitle_file):
        class Subtitle(cls):
            parent = subtitle_file
        return Subtitle

    def __init__(self, start_frame=0, end_frame=0, text="", style_index=0):
        if start_frame > end_frame:
            raise ValueError("start can’t come after end")
        self._start_frame = start_frame
        self._end_frame = end_frame
        self.text = text
        self.style_index = style_index

    @classmethod
    def from_times(cls, start_time=0, end_time=0, text="", style_index=0):
        start_frame = cls.time_to_frame(start_time)
        end_frame = cls.time_to_frame(end_time)
        return cls(start_frame, end_frame, text, style_index)

    @classmethod
    def from_frames(cls, start_frame=0, end_frame=0, text="", style_index=0):
        return cls(start_frame, end_frame, text, style_index)

    @classmethod
    def from_str(cls, text):
        match = cls.PARSER.search(text)
        if not match:
            raise ValueError(
                "can’t parse text into a subtitle: {!r}".format(text))
        return cls.from_match(match)

    @classmethod
    def from_match(cls, match):
        d = match.groupdict()
        return cls.from_times(cls.Time.from_str(d["start_time"]),
                              cls.Time.from_str(d["end_time"]),
                              d["text"])

    def __repr__(self):
        return "{}({!r}, {!r}, {!r}, {!r})".format(
            self.__class__.__name__, self.start_frame, self.end_frame,
            self.text, self.style_index)

    def __str__(self):
        index = self._index
        start_time = str(self.Time(self.start_time))
        end_time = str(self.Time(self.end_time))
        text = self.text
        if index is None:
            return self.FORMAT_BASE.format_map(locals())
        identifier = index + 1
        return self.FORMAT.format_map(locals())

    def __eq__(self, other):
        return ((self.start_frame, self.end_frame, self.text) ==
                (other.start_frame, other.end_frame, other.text))

    def __lt__(self, other):
        return self.start_frame < other.start_frame

    @classmethod
    def time_to_frame(cls, time):
        # TODO: use ffms
        return round(time / cls.parent.frame_duration)

    @classmethod
    def frame_to_time(cls, frame):
        # TODO: use ffms
        return int(frame * cls.parent.frame_duration)

    @property
    def _index(self):
        return self.parent.get_index(self)

    @property
    def style(self):
        return self.parent.style_list[self.style_index]

    @property
    def start_time(self):
        return self.frame_to_time(self._start_frame)

    @start_time.setter
    def start_time(self, value):
        self.start_frame = self.time_to_frame(value)

    @property
    def end_time(self):
        return self.frame_to_time(self._end_frame)

    @end_time.setter
    def end_time(self, value):
        self.end_frame = self.time_to_frame(value)

    @property
    def times(self):
        return self.start_time, self.end_time

    @times.setter
    def times(self, value):
        self.frames = [self.time_to_frame(e) for e in value]

    @property
    def duration(self):
        return self.frame_to_time(self._end_frame - self._start_frame)

    @duration.setter
    def duration(self, value):
        if value < 0:
            raise ValueError("negative duration")
        self.end_time = self.start_time + value

    @property
    def ideal_duration(self):
        return HALF_A_SECOND + self.char_amount * 50

    @property
    def start_frame(self):
        return self._start_frame

    @start_frame.setter
    def start_frame(self, value):
        if value > self._end_frame:
            raise ValueError("start can’t come after end")
        self._start_frame = value
        previous_subtitle = self.previous_subtitle
        if previous_subtitle is not None and self < previous_subtitle:
            self.parent.is_sorted = False
        else:
            next_subtitle = self.next_subtitle
            if next_subtitle is not None and self > next_subtitle:
                self.parent.is_sorted = False

    @property
    def end_frame(self):
        return self._end_frame

    @end_frame.setter
    def end_frame(self, value):
        if value < self._start_frame:
            raise ValueError("end can’t come before start")
        self._end_frame = value

    @property
    def frames(self):
        return self._start_frame, self._end_frame

    @frames.setter
    def frames(self, value):
        start_frame, end_frame = value
        if start_frame > end_frame:
            raise ValueError("start can’t come after end")
        self._start_frame, self._end_frame = start_frame, end_frame
        previous_subtitle = self.previous_subtitle
        if previous_subtitle is not None and self < previous_subtitle:
            self.parent.is_sorted = False
        else:
            next_subtitle = self.next_subtitle
            if next_subtitle is not None and self > next_subtitle:
                self.parent.is_sorted = False

    def shift(self, time):
        self.times = [t + time for t in self.times]

    def shift_frames(self, frames):
        self.frames = [f + frames for f in self.frames]

    @property
    def char_amount(self):
        if self._char_amount is None:
            plain_lines = self.plain_lines
            num_lines = len(plain_lines)
            self._char_amount = (
                sum([len(plain_line) for plain_line in plain_lines]) +
                (num_lines - 1) * 2
            )
        return self._char_amount

    @property
    def width_info(self):
        plain_text = self.plain_text
        style = self.style
        return {
            "chars": get_text_width_chars(plain_text),
            "pixels": get_text_width_pixels(
                plain_text, style.font_name, style.font_size, style.font_bold),
            # TODO: Deprecate pixel width as calculated from VSS plugins?
            "pixels_arial18bold": get_text_width_pixels(
                plain_text, "Arial", 18, True),
            }

    @property
    def text(self):
        return self._text

    @text.setter
    def text(self, value):
        self._text = value
        self._plain_text = None
        self._char_amount = None

    @property
    def plain_text(self):
        if self._plain_text is None:
            self._plain_text = self.TAG_RE.sub("", self._text)
        return self._plain_text

    @plain_text.setter
    def plain_text(self, value):
        self._text = self._update_text(self._text, self.plain_text, value)
        self._plain_text = value
        self._char_amount = None

    @property
    def plain_text_no_dialog(self):
        expr, repl = self.STRIP_DIALOG_SUB
        return expr.sub(repl, self.plain_text)

    @plain_text_no_dialog.setter
    def plain_text_no_dialog(self, value):
        self.text = self._update_text(
            self.text, self.plain_text_no_dialog, value)

    @property
    def lines(self):
        return tuple(self._text.split("\n"))

    @lines.setter
    def lines(self, value):
        self.text = "\n".join(value)

    def set_line(self, index, line):
        lines = self.lines
        self.lines = lines[:index] + (line,) + lines[index + 1:]

    @property
    def plain_lines(self):
        return tuple(self.plain_text.split("\n"))

    @plain_lines.setter
    def plain_lines(self, value):
        self.plain_text = "\n".join(value)

    def set_plain_line(self, index, line):
        lines = self.plain_lines
        self.plain_lines = lines[:index] + (line,) + lines[index + 1:]

    @property
    def plain_lines_no_dialog(self):
        return tuple(self.plain_text_no_dialog.split("\n"))

    @plain_lines_no_dialog.setter
    def plain_lines_no_dialog(self, value):
        self.plain_text_no_dialog = "\n".join(value)

    def set_plain_line_no_dialog(self, index, line):
        lines = self.plain_lines_no_dialog
        self.plain_lines_no_dialog = (lines[:index] + (line,) +
                                      lines[index + 1:])

    @property
    def text_as_one_line(self):
        return self._text.replace("\n", " ")

    @text_as_one_line.setter
    def text_as_one_line(self, value):
        self.text = self._update_text(self._text, self.text_as_one_line, value)

    @property
    def plain_text_as_one_line(self):
        return self.plain_text.replace("\n", " ")

    @plain_text_as_one_line.setter
    def plain_text_as_one_line(self, value):
        self.plain_text = self._update_text(
            self.plain_text, self.plain_text_as_one_line, value)

    @property
    def cps(self):
        try:
            return self.char_amount * ONE_SECOND / self.duration
        except ZeroDivisionError:
            return float("inf")

    @cps.setter
    def cps(self, value):
        self.duration = self.char_amount * ONE_SECOND / value

    def limit_cps(self, max_cps):
        self.cps = max_cps
        while self.cps > max_cps:
            self.end_frame += 1

    @property
    def rs(self):
        duration = self.duration
        if duration <= HALF_A_SECOND:
            return float("inf")
        return self.char_amount * ONE_SECOND / (duration - HALF_A_SECOND)

    @rs.setter
    def rs(self, value):
        self.duration = self.char_amount * ONE_SECOND / value + HALF_A_SECOND

    def limit_rs(self, max_rs):
        self.rs = max_rs
        while self.rs >= max_rs:
            self.end_frame += 1

    @property
    def previous_subtitle(self):
        index = self._index
        if index is None or index == 0:
            return None
        return self.parent[index - 1]

    @property
    def next_subtitle(self): #@ReservedAssignment
        index = self._index
        if index is None or index == len(self.parent) - 1:
            return None
        return self.parent[index + 1]

    @property
    def time_to_previous(self):
        previous_subtitle = self.previous_subtitle
        if previous_subtitle is None:
            return None
        return self.start_time - previous_subtitle.end_time

    @time_to_previous.setter
    def time_to_previous(self, value):
        previous_subtitle = self.previous_subtitle
        if previous_subtitle is None:
            return
        new_start = previous_subtitle.end_time + value
        if new_start <= self.end_time:
            self.start_time = new_start

    @property
    def time_to_next(self):
        next_ = self.next_subtitle
        if next_ is None:
            return None
        return next_.start_time - self.end_time

    @time_to_next.setter
    def time_to_next(self, value):
        next_ = self.next_subtitle
        if next_ is None:
            return
        new_end = next_.start_time - value
        if new_end >= self.start_time:
            self.end_time = new_end

    @property
    def is_dialog(self):
        plain_lines = self.plain_lines
        return (len(plain_lines) > 1 and
                self.STRIP_DIALOG_SUB[0].match(plain_lines[-1]) is not None)

    @is_dialog.setter
    def is_dialog(self, value):
        if value:
            expr, repl = self.ADD_DIALOG_SUB
        else:
            expr, repl = self.STRIP_DIALOG_SUB
        self.plain_text = expr.sub(repl, self.plain_text)

    @staticmethod
    def _update_text(text, plain_text, new_plain_text):
        new_text = update_text(text, plain_text, new_plain_text)
        if text.startswith("{"):
            new_text = re.sub(r"^(.+?)(\{.*?\})", r"\2\1", new_text)
        return new_text
