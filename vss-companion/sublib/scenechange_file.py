import bisect
import codecs
import functools
import os
import re

from contextlib import contextmanager
from fractions import Fraction

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
    MISSING_THRESHOLD = 6.0
    BAD_THRESHOLD = 0.03
    DEFAULT_FILTER_OFFSET = 500
    FILTER_OFFSETS = {
        Fraction(24000, 1001): DEFAULT_FILTER_OFFSET,
        Fraction(25000, 1000): 480,
    }

    VERSION_RE = re.compile(r"SceneChangeFormatVersion\s*=\s*(\w+)", re.I)
    OUTPUT_FORMAT = ([ffms.get_pix_fmt("yuv420p")], 64, 64,
                     ffms.FFMS_RESIZER_FAST_BILINEAR)

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
        with open(
            file, "w", encoding=self.ENCODING, newline=self.NEWLINE
        ) as f:
            f.write("SceneChangeFormatVersion={}\n"
                    .format(self.FORMAT_VERSION))
            f.write("\n".join(str(Time.from_int(t)) for t in self) + "\n")

    def revert(self, vsource):
        self.clear()
        self.update(get_scene_changes(vsource))

    def get_previous(self, time):
        pos = self.bisect_right(time)
        return self[pos - 1] if pos else None

    def get_next(self, time):
        pos = self.bisect_left(time)
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
        start = frame_time_to_position(vsource, start_time)
        end = frame_time_to_position(vsource, end_time)
        if frame_position_to_time(vsource, start) < start_time:
            start += 1
        #if frame_position_to_time(vsource, end) > end_time:
            #end -= 1
        pos = cls.find_frame(vsource, start, end)
        return frame_position_to_time(vsource, pos)

    @classmethod
    def find_frame(cls, vsource, start, end):
        selected = start
        selected_diff = 0
        with vsource.output_format(*cls.OUTPUT_FORMAT):
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
                plane = frame.planes[0]
                diff = numpy.absolute(plane - prev).sum()
                if diff > selected_diff:
                    selected = pos
                    selected_diff = diff
                prev = plane.astype(numpy.int_)
        return selected

    def scan_missing(self, vsource, timings,
                     missing_threshold=MISSING_THRESHOLD,
                     bad_threshold=BAD_THRESHOLD,
                     filter_offset=None):
        if filter_offset is None:
            filter_offset = self.FILTER_OFFSETS.get(
                get_fps(vsource), self.DEFAULT_FILTER_OFFSET
            )
        filter_offset += 1

        with vsource.output_format(*self.OUTPUT_FORMAT):
            max_diff = len(vsource.get_frame(0).planes[0]) * 255
            for start_time, end_time in timings:
                borders = [
                    (start_time, min(start_time + filter_offset, end_time)),
                    (max(end_time - filter_offset, start_time), end_time),
                ]
                for start_time, end_time in borders:
                    if self.contains(start_time, end_time):
                        continue
                    start = frame_time_to_position(vsource, start_time)
                    end = frame_time_to_position(vsource, end_time)
                    if frame_position_to_time(vsource, end) < end_time:
                        end += 1
                    scan_start = start + 1
                    diffs = numpy.empty(end - scan_start, numpy.int_)

                    plane = vsource.get_frame(start).planes[0]
                    for n, pos in enumerate(range(scan_start, end)):
                        prev = plane.astype(numpy.int_)
                        plane = vsource.get_frame(pos).planes[0]
                        diffs[n] = numpy.absolute(plane - prev).sum()

                    if (
                        diffs.max() / max_diff > bad_threshold and
                        diffs.max() / numpy.median(diffs) >= missing_threshold
                    ):
                        pos = diffs.argmax() + scan_start
                        yield frame_position_to_time(vsource, pos)

    def scan_bad(self, vsource, timings, bad_threshold=BAD_THRESHOLD,
                 filter_offset=None):
        if filter_offset is None:
            filter_offset = self.FILTER_OFFSETS.get(
                get_fps(vsource), self.DEFAULT_FILTER_OFFSET
            )
        offset = filter_offset // 2
        sc_set = set()
        for start_time, end_time in timings:
            sc = self.get_previous(start_time)
            if (sc is not None and start_time - sc <= offset):
                sc_set.add(sc)
            sc = self.get_next(start_time + 1)
            if (sc is not None and sc - start_time <= filter_offset):
                sc_set.add(sc)
            sc = self.get_previous(end_time)
            if (sc is not None and end_time - sc <= filter_offset):
                sc_set.add(sc)
            sc = self.get_next(end_time + 1)
            if (sc is not None and sc - end_time <= offset):
                sc_set.add(sc)

        with vsource.output_format(*self.OUTPUT_FORMAT):
            sc_positions = [frame_time_to_position(vsource, t)
                            for t in sorted(sc_set)]
            if sc_positions and not sc_positions[0]:
                del sc_positions[0]
            max_diff = len(vsource.get_frame(0).planes[0]) * 255
            data_set = set(self)
            for pos in sc_positions:
                prev = vsource.get_frame(pos - 1).planes[0].astype(numpy.int_)
                plane = vsource.get_frame(pos).planes[0]
                pct = numpy.absolute(plane - prev).sum() / max_diff
                if pct <= bad_threshold:
                    t = frame_position_to_time(vsource, pos)
                    yield t if t in data_set else self.get_nearest(t)

    def scan_bad_all(self, vsource, bad_threshold=BAD_THRESHOLD):
        with vsource.output_format(*self.OUTPUT_FORMAT):
            sc_positions = [frame_time_to_position(vsource, t) for t in self]
            if sc_positions and not sc_positions[0]:
                del sc_positions[0]
            max_diff = len(vsource.get_frame(0).planes[0]) * 255
            data_set = set(self)
            for pos in sc_positions:
                prev = vsource.get_frame(pos - 1).planes[0].astype(numpy.int_)
                plane = vsource.get_frame(pos).planes[0]
                pct = numpy.absolute(plane - prev).sum() / max_diff
                if pct <= bad_threshold:
                    t = frame_position_to_time(vsource, pos)
                    yield t if t in data_set else self.get_nearest(t)


def get_scene_changes(vsource):
    sc_list = [int(tc) for tc in vsource.track.keyframes_as_timecodes]
    if sc_list and not sc_list[0]:
        sc_list = sc_list[1:]
    return sc_list


def frame_time_to_position(vsource, time):
    return bisect.bisect(vsource.track.timecodes, time) - 1


def frame_position_to_time(vsource, frame):
    return vsource.track.timecodes[frame]


def get_fps(vsource):
    return Fraction(vsource.properties.FPSNumerator,
                    vsource.properties.FPSDenominator)

# def get_frame_duration(props):
    # return 1000 * props.FPSDenominator / props.FPSNumerator


# def frame_time_to_position(time, frame_duration):
    # return round(time / frame_duration)


# def frame_position_to_time(frame, frame_duration):
    # return int(frame * frame_duration)
