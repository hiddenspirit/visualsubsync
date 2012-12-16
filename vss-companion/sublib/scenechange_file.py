import bisect
import codecs
import functools
import os
import re

from contextlib import contextmanager

import numpy

import ffms
from .time import Time
from .sorted_list import SortedSet


@functools.total_ordering
class SceneChangeFile(SortedSet):
    FORMAT_VERSION = 1
    EXT = ".scenechange"
    ENCODING = "ascii"
    NEWLINE = "\n"
    VERSION_RE = re.compile(r"SceneChangeFormatVersion\s*=\s*(\w+)", re.I)
    OUTPUT_FORMAT = [0], 64, 64, ffms.FFMS_RESIZER_FAST_BILINEAR

    def __init__(self, data=None, file=None):
        if data is None:
            data = []
        super().__init__(data)
        self.file = file

    @classmethod
    def from_source(cls, vsource, file=None):
        if not file and vsource.index.source_file:
            base_name = os.path.splitext(vsource.index.source_file)[0]
            file = base_name + cls.EXT
        return cls(get_scene_changes(vsource), file)

    @classmethod
    def load(cls, file):
        with open(file, "rb") as f:
            buf = f.read()
        if buf.startswith(codecs.BOM_UTF8):
            encoding = "utf-8-sig"
        elif (buf.startswith(codecs.BOM_UTF32_LE) or
              buf.startswith(codecs.BOM_UTF32_BE)):
            encoding = "utf-32"
        elif (buf.startswith(codecs.BOM_UTF16_LE) or
              buf.startswith(codecs.BOM_UTF16_LE)):
            encoding = "utf-16"
        else:
            encoding = cls.ENCODING
        text = buf.decode(encoding, "ignore")
        lines = text.splitlines()
        match = cls.VERSION_RE.match(lines[0])
        if match:
            s = match.group(1)
            if not s.isdigit() or int(s) > cls.FORMAT_VERSION:
                raise ValueError(
                    "unknown scenechange file format version: {!r}"
                    .format(s))
            lines = lines[1:]
        data = []
        for line in lines:
            try:
                t = Time.from_str(line)
            except ValueError:
                continue
            data.append(int(t))
        return cls(data, file)

    def __repr__(self):
        return "{}({!r}, {!r})".format(self.__class__.__name__,
                                       list(self), self.file)

    def save(self, file=None):
        if not file:
            file = self.file
        with open(file, "w", encoding=self.ENCODING, newline=self.NEWLINE)\
             as f:
            f.write("SceneChangeFormatVersion={}\n"
                    .format(self.FORMAT_VERSION))
            f.write("\n".join(str(Time.from_int(t)) for t in self) + "\n")

    def revert(self, vsource):
        self.clear()
        self.update(get_scene_changes(vsource))

    def get_previous(self, time):
        pos = self.bisect_left(time)
        return self[pos - 1] if pos else None

    def get_next(self, time):
        pos = self.bisect_right(time)
        return self[pos] if pos < len(self) else None

    def get_nearest(self, time):
        # TODO: optimize
        prev = self.get_previous(time)
        next_ = self.get_next(time)
        return (prev
                if next_ is None or
                prev is not None and abs(time - prev) < abs(time - next_)
                else next_)

    def contains(self, start_time, end_time):
        pos = self.bisect_right(start_time)
        return pos < len(self) and self[pos] <= end_time

    @classmethod
    def find(cls, vsource, start_time, end_time):
        with vsource.output_format(*cls.OUTPUT_FORMAT):
            frame_duration = get_frame_duration(vsource.properties)
            start = frame_time_to_position(start_time, frame_duration)
            end = frame_time_to_position(end_time, frame_duration)
            if frame_position_to_time(start, frame_duration) < start_time:
                start += 1
            if frame_position_to_time(end, frame_duration) > end_time:
                end -= 1
            selected = start
            selected_diff = 0
            if start:
                frame = vsource.get_frame(start - 1)
                plane = frame.planes[0]
                prev = plane.astype(numpy.int_)
            else:
                frame = vsource.get_frame(0)
                plane = frame.planes[0]
                prev = numpy.zeros(len(plane), numpy.int_)
            for pos in range(start, end + 1):
                frame = vsource.get_frame(pos)
                diff = numpy.absolute(plane - prev).sum()
                if diff > selected_diff:
                    selected = pos
                    selected_diff = diff
                prev = plane.astype(numpy.int_)
        
        if vsource.index.source_file:
            ext = os.path.splitext(vsource.index.source_file)[1]
            if ext.lower() == ".mp4":
                selected += 2

        return frame_position_to_time(selected, frame_duration)

    def scan_bad(self, vsource, threshold=0.05):
        with vsource.output_format(*self.OUTPUT_FORMAT):
            frame_duration = get_frame_duration(vsource.properties)
            sc_positions = [frame_time_to_position(t, frame_duration)
                                  for t in self]
            plane = vsource.planes[0]   # XXX
            max_diff = len(plane) * 255
            data_set = set(self)
            for pos in sc_positions:
                frame = vsource.get_frame(pos - 1)
                prev = plane.astype(numpy.int_)
                frame = vsource.get_frame(pos)
                pct = numpy.true_divide(
                    numpy.absolute(plane - prev).sum(), max_diff)
                if pct < threshold:
                    t = frame_position_to_time(pos, frame_duration)
                    yield t if t in data_set else self.get_nearest(t)


def get_scene_changes(vsource):
    sc_list = [int(tc) for tc in vsource.track.keyframes_as_timecodes]
    if sc_list and not sc_list[0]:
        sc_list = sc_list[1:]
    return sc_list


def get_frame_duration(props):
    return 1000 * props.FPSDenominator / props.FPSNumerator


def frame_time_to_position(time, frame_duration):
    return round(time / frame_duration)


def frame_position_to_time(frame, frame_duration):
    return int(frame * frame_duration)
