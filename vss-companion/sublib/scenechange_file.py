import bisect
import codecs
import functools
import os
import re
import threading

#from contextlib import contextmanager
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
    DIFF_PCT_THRESHOLD = 0.15
    RATIO_THRESHOLD = 3.0
    DEFAULT_FILTER_OFFSET = 500
    FILTER_OFFSETS = {
        Fraction(24000, 1001): DEFAULT_FILTER_OFFSET,
        Fraction(25000, 1000): 480,
    }
    _cancel_event = threading.Event()

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
        pos = self.bisect_left(start_time)
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
                     diff_pct_threshold=DIFF_PCT_THRESHOLD,
                     ratio_threshold=RATIO_THRESHOLD,
                     filter_offset=None, cancel_event=_cancel_event):
        if not timings:
            raise ValueError("empty timings")
        first_time = int(vsource.properties.FirstTime * 1000)
        last_time = int(vsource.properties.LastTime * 1000)
        get_bounded_timing = lambda t: min(max(t, first_time), last_time)
        get_bounded_timings = \
            lambda t: (get_bounded_timing(t[0]), get_bounded_timing(t[1]))
        timings = [get_bounded_timings(t) for t in timings]

        if filter_offset is None:
            filter_offset = self.FILTER_OFFSETS.get(
                get_fps(vsource), self.DEFAULT_FILTER_OFFSET
            )
        offset = filter_offset // 2
        filter_offset += 1
        den = numpy.log1p(255)
        sc_time = first_time

        with vsource.output_format(*self.OUTPUT_FORMAT):
            for start_time, end_time in timings:
                if cancel_event.is_set():
                    return
                borders = [
                    (start_time - offset, start_time + filter_offset),
                    (end_time - filter_offset, end_time + offset),
                ]
                for start_time, end_time in borders:
                    if start_time < sc_time:
                        start_time = sc_time
                    if end_time > last_time:
                        end_time = last_time
                    if self.contains(start_time, end_time):
                        continue
                    start = frame_time_to_position(vsource, start_time)
                    end = frame_time_to_position(vsource, end_time)
                    if frame_position_to_time(vsource, end) < end_time:
                        end += 1
                    scan_start = start + 1
                    diffs = numpy.empty(end - scan_start, numpy.float16)

                    plane = vsource.get_frame(start).planes[0]
                    for n, pos in enumerate(range(scan_start, end)):
                        prev = plane.astype(numpy.int_)
                        plane = vsource.get_frame(pos).planes[0]
                        diffs[n] = numpy.log1p(
                            numpy.absolute(plane - prev)).mean()

                    diff_pct = diffs.max() / den
                    if diff_pct >= diff_pct_threshold:
                        diffs /= den
                        ratio = diffs.max() / numpy.median(diffs)
                        if ratio >= ratio_threshold:
                            pos = diffs.argmax() + scan_start
                            sc_time = frame_position_to_time(vsource, pos)
                            yield sc_time, diff_pct, ratio

    def scan_bogus(self, vsource, timings,
                   diff_pct_threshold=DIFF_PCT_THRESHOLD, filter_offset=None,
                   cancel_event=_cancel_event):
        if filter_offset is None:
            filter_offset = self.FILTER_OFFSETS.get(
                get_fps(vsource), self.DEFAULT_FILTER_OFFSET
            )
        offset = filter_offset // 2
        sc_set = set()
        data_set = set(self)
        den = numpy.log1p(255)

        for start_time, end_time in timings:
            sc_time = self.get_previous(start_time)
            if (sc_time is not None and start_time - sc_time <= offset):
                sc_set.add(sc_time)
            sc_time = self.get_next(start_time + 1)
            if (sc_time is not None and sc_time - start_time <= filter_offset):
                sc_set.add(sc_time)
            sc_time = self.get_previous(end_time)
            if (sc_time is not None and end_time - sc_time <= filter_offset):
                sc_set.add(sc_time)
            sc_time = self.get_next(end_time + 1)
            if (sc_time is not None and sc_time - end_time <= offset):
                sc_set.add(sc_time)

        sc_positions = [frame_time_to_nearest_position(vsource, t)
                        for t in sorted(sc_set)]
        if sc_positions and not sc_positions[0]:
            del sc_positions[0]

        with vsource.output_format(*self.OUTPUT_FORMAT):
            for pos in sc_positions:
                if cancel_event.is_set():
                    return
                prev = vsource.get_frame(pos - 1).planes[0].astype(numpy.int_)
                plane = vsource.get_frame(pos).planes[0]
                diff_pct = numpy.log1p(
                    numpy.absolute(plane - prev)).mean() / den
                if diff_pct < diff_pct_threshold:
                    t = frame_position_to_time(vsource, pos)
                    yield t if t in data_set else self.get_nearest(t), diff_pct


def get_scene_changes(vsource):
    sc_list = [int(tc) for tc in vsource.track.keyframes_as_timecodes]
    #if sc_list and not sc_list[0]:
        #sc_list = sc_list[1:]
    return sc_list


def frame_time_to_position(vsource, time):
    pos = bisect.bisect(vsource.track.timecodes, time)
    return pos - 1 if pos > 0 else pos


def frame_time_to_nearest_position(vsource, time):
    pos = bisect.bisect(vsource.track.timecodes, time)
    if (pos == 0 or abs(vsource.track.timecodes[pos] - time) <
            abs(vsource.track.timecodes[pos-1] - time)):
        return pos
    return pos - 1


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
