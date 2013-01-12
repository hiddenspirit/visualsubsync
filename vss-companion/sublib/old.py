# -*- coding: utf-8 -*-
"""Subtitle Library (Python 3)
"""
import re
import codecs
import itertools

__author__ = "Spirit <hiddenspirit (at) gmail.com>",
__version__ = 0, 1, 0

DEBUG = False
NEWLINE = "\r\n"
ENCODING = "Windows-1252"
INFINITY = 1e999


class Subtitle(object):
    tags_pattern = re.compile(r"<[^>]*>|{[^}]*}")

    def __init__(self, start=0, stop=0, text=""):
        self.start = start
        self.stop = stop
        self.text = text

    def __repr__(self):
        return "Subtitle(%r, %r, %r)" % (self.start, self.stop, self.text)

    def __bytes__(self):
        return str(self).encode(ENCODING, "replace")

    def __str__(self):
        return "%s:%s:%s" % (self.start, self.stop, self.text)

    def __lt__(self, other):
        if self.start < other.start:
            return True
        elif self.start > other.start:
            return False
        elif self.stop < other.stop:
            return True
        elif self.stop > other.stop:
            return False
        elif self.index < other.index:
            return True
        elif self.index > other.index:
            return False
        else:
            return False

    def __eq__(self, other):
        return (
            self.start == other.start and self.stop == other.stop and
            self.text == other.text
        )

    def __len__(self):
        stripped_lines = self.stripped_lines()
        return (
            sum([len(stripped_line) for stripped_line in stripped_lines]) +
            (len(stripped_lines) - 1) * 2
        )

    def lines(self):
        return self.text.split(NEWLINE)

    def reading_speed(self):
        if self.duration() <= 500:
            return INFINITY
        return len(self) * 1000 / (self.duration() - 500)

    def characters_per_second(self):
        if self.duration() <= 0:
            return INFINITY
        return len(self) * 1000 / self.duration()

    def duration(self):
        return self.stop - self.start


class SubRipSubtitle(Subtitle):
    time_str = r"(-?\d+):(-?\d+):(-?\d+)[,.](-?\d+)"
    time_pair_pattern = re.compile(time_str + r"\D+" + time_str)
    time_format = "%02d:%02d:%02d,%03d"
    time_pair_format = time_format + " --> " + time_format

    def __init__(self, index=0, start=0, stop=0, text=""):
        Subtitle.__init__(self, start, stop, text)
        self.index = index

    def __repr__(self):
        return "SubRipSubtitle(%r, %r, %r, %r)" % (
            self.index, self.start,
            self.stop, self.text
        )

    def __str__(self):
        return "%d%s%s%s%s%s" % (
            self.index, NEWLINE, self.time_pair(), NEWLINE,
            self.text, NEWLINE
        )

    def __lt__(self, other):
        if self.start < other.start:
            return True
        elif self.start > other.start:
            return False
        elif self.stop < other.stop:
            return True
        elif self.stop > other.stop:
            return False
        elif self.index < other.index:
            return True
        elif self.index > other.index:
            return False
        else:
            return False

    @classmethod
    def time_pair_to_start_stop(cls, time_pair):
        match = cls.time_pair_pattern.match(time_pair)
        if not match:
            raise RuntimeError("Can't interpret time pair: " + time_pair)
        bh, bm, bs, bms, eh, em, es, ems = [int(e) for e in match.groups()]
        start = bh * 3600000 + bm * 60000 + bs * 1000 + bms
        stop = eh * 3600000 + em * 60000 + es * 1000 + ems
        return start, stop

    @staticmethod
    def time_to_hmsms(time):
        h = time // 3600000
        ms = time % 1000
        time %= 3600000
        m = time // 60000
        time %= 60000
        s = time // 1000
        return h, m, s, ms

    def time_pair(self):
        bh, bm, bs, bms = self.time_to_hmsms(self.start)
        eh, em, es, ems = self.time_to_hmsms(self.stop)
        return self.time_pair_format % (bh, bm, bs, bms, eh, em, es, ems)

    def stripped_text(self):
        return self.tags_pattern.sub("", self.text)

    def stripped_lines(self):
        return self.stripped_text().split(NEWLINE)


class AssSubtitle(Subtitle):
    pass


class SubtitleFile(object):
    def detect_encoding(self):
        with open(self.file_path, "rb") as f:
            data = f.read()
        return "utf-8-sig" if data.startswith(codecs.BOM_UTF8) else ENCODING


class SubRipFile(SubtitleFile):
    def __init__(self, file_path, encoding=None, reindex=True):
        self.file_path = file_path
        self.encoding = encoding if encoding else self.detect_encoding()
        self.read(file_path)
        if reindex:
            self.reindex()

    def __repr__(self):
        return "SubRipFile(%r, %r)" % (file_path, self.encoding)

    def __bytes(self):
        return str(self).encode(ENCODING, "replace")

    def __str__(self):
        out = ""
        for subtitle in self.sub_list:
            out += str(subtitle)
            out += NEWLINE
        return out

    def read(self, file_path):
        self.sub_list = []
        with codecs.open(file_path, "r", self.encoding) as in_file:
            entry = 0
            index = 0
            start, stop = 0, 0
            lines = []
            first_line = True
            for line in in_file:
                if first_line:
                    if line.startswith("\xef\xbb\xbf"):
                        line = line[3]
                    first_line = False
                line = line.strip()
                if entry == 0:
                    if line.isdigit():
                        index = int(line)
                        entry += 1
                elif entry == 1:
                    if line:
                        try:
                            start, stop = (
                                SubRipSubtitle.
                                time_pair_to_start_stop(line)
                            )
                        except RuntimeError as e:
                            print(index, e)
                            entry = 0
                        else:
                            entry += 1
                elif line == "":
                    srt = SubRipSubtitle(
                        index, start, stop,
                        NEWLINE.join(lines)
                    )
                    self.sub_list.append(srt)
                    entry = 0
                    index = 0
                    start, stop = 0, 0
                    lines = []
                else:
                    lines.append(line)
            if entry:
                self.sub_list.append(
                    SubRipSubtitle(index, start, stop, NEWLINE.join(lines))
                )

    def write(self, file_path=None, encoding=None):
        if file_path is None:
            file_path = self.file_path
        if encoding is None:
            encoding = self.encoding
        with open(file_path, "wb") as out_file:
            out_file.write(str(self).encode(encoding))

    def reindex(self):
        self.sub_list.sort()
        for index, subtitle in zip(itertools.count(1), self.sub_list):
            subtitle.index = index


class AssFile(SubtitleFile):
    pass


class Tags(object):
    pass


class HtmlTags(Tags):
    ITALIC = "<i>%s</i>"
    BOLD = "<b>%s</b>"
    UNDERLINE = "<u>%s</u>"
    COLOR = '<font color="#%X%X%X">%s</font>'
    SIZE = '<font size="%d">%s</font>'


class AssTags(Tags):
    ITALIC = r"{\i1}%s{\i0}"
    BOLD = r"{\b1}%s{\b0}"
    UNDERLINE = r"{\u1}%s{\u0}"
    COLOR = r"{\1c&H%X%X%X&}%s{\1c}"
    SIZE = r"{\fs%d}%s{\fs}"
    POS = r"{\pos(%d,%d)}"
    A = r"{\a%d}"
    FAD = r"{\fad(%d,%d)}"
