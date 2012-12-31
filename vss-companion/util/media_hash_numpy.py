"""Hash for media files

http://trac.opensubtitles.org/projects/opensubtitles/wiki/HashSourceCodes
"""
import os
import numpy


class MediaHash:
    BLOCK_SIZE = 0x10000
    DIGEST_TYPE = numpy.uint64
    NUM_ITEMS = BLOCK_SIZE // numpy.dtype(DIGEST_TYPE).itemsize

    def __init__(self, path):
        file_size = os.path.getsize(path)
        h = self.DIGEST_TYPE(file_size)
        with open(path, "rb") as f:
            for pos in [0, file_size - self.BLOCK_SIZE]:
                if pos < 0:
                    break
                f.seek(pos)
                v = numpy.fromfile(f, self.DIGEST_TYPE, self.NUM_ITEMS).sum()
                h = h.__add__(v)  # Prevent overflow RuntimeWarning.
        self.digest = int(h)

    @property
    def hex_digest(self):
        return "{:016x}".format(self.digest)

