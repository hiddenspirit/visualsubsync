import os
from collections import Sequence

import numpy


class PeakFile(Sequence):
    DEFAULT_RATE = 100
    TYPE = numpy.float32
    STORAGE_TYPE = numpy.int16

    EXT = ".peak.npz"

    def __init__(self, data, rate=DEFAULT_RATE, file=None):
        self.data = data
        self.rate = rate
        self.file = file

    @classmethod
    def from_source(cls, asource, rate=DEFAULT_RATE, file=None):
        if not file and asource.source_file:
            base_name = os.path.splitext(asource.source_file)[0]
            file = base_name + cls.EXT
        return cls(get_peak_data(asource, rate, cls.TYPE), rate, file)

    @classmethod
    def load(cls, file):
        npz_file = numpy.load(file)
        data = npz_file["data"]
        rate = npz_file["rate"] if "rate" in npz_file else cls.DEFAULT_RATE
        if data.dtype.kind != "f":
            m = 2 ** (8 * data.itemsize - 1) - 1
            data = data.astype(cls.TYPE) / m
        return cls(data, rate, file)

    def __repr__(self):
        return "{}(data={!r}, rate={!r}, file={!r})".format(
            self.__class__.__name__, self.data, self.rate, self.file)

    def __eq__(self, other):
        return (self.rate == other.rate and
            len(self.data) == len(other.data) and
            all(self.data.round(2) == other.data.round(2)))

    def __getitem__(self, key):
        return self.data[key]

    def __len__(self):
        return len(self.data)

    def save(self, file=None):
        if not file:
            file = self.file
        if numpy.dtype(self.STORAGE_TYPE).kind != "f":
            m = 2 ** (8 * numpy.dtype(self.STORAGE_TYPE).itemsize - 1) - 1
            data = (self.data * m).round()
        else:
            data = self.data
        if self.rate == self.DEFAULT_RATE:
            numpy.savez_compressed(file, data=data.astype(self.STORAGE_TYPE))
        else:
            numpy.savez_compressed(file, data=data.astype(self.STORAGE_TYPE),
                                   rate=numpy.uint32(self.rate))


# Don’t call more than once on the same audio source,
# because of a strange ReadPacket error with FFMS_GetAudio() under Linux.
def get_peak_data(asource, rate=100, type_=numpy.float32):
    linear_access = asource.linear_access(rate=rate)
    num_frames = len(linear_access)
    peak_data = numpy.zeros((num_frames, 2), asource.sample_type)
    peak_data_min, peak_data_max = float("inf"), float("-inf")

    for n, audio in enumerate(linear_access):
        peak_min = audio.min()
        peak_max = audio.max()
        if peak_min < peak_data_min:
            peak_data_min = peak_min
        if peak_max > peak_data_max:
            peak_data_max = peak_max
        peak_data[n] = peak_min, peak_max

    kind = peak_data.dtype.kind

    if kind == "f":
        peak_data.clip(-1, 1, peak_data)
        peak_data_min = peak_data_min.clip(-1, 1)
        peak_data_max = peak_data_max.clip(-1, 1)
    elif kind == "u":
        size = peak_data.itemsize
        mid_point = 2 ** (8 * size - 1)
        new_type = (numpy.int_ if size < numpy.dtype(numpy.int_).itemsize
                    else numpy.float_)
        peak_data = peak_data.astype(new_type) - mid_point
        peak_data_min -= mid_point
        peak_data_max -= mid_point

    abs_max = max(abs(peak_data_min), abs(peak_data_max))
    return peak_data.astype(type_) / abs_max
