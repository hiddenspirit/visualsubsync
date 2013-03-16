"""Hash for media files

http://trac.opensubtitles.org/projects/opensubtitles/wiki/HashSourceCodes
"""
import os
import sys
import numpy


# Workaround for an OSError after seeking from offset 0x7ffff000/0x7fffffff
# Error occurs with numpy 1.7.0 on Python 3.3.0 (32 bit).
if sys.maxsize == 0x7fffffff:
    def array_from_file(file, dtype=numpy.float, count=-1, sep=""):
        """Construct an array from data in a text or binary file.
        """
        item_size = numpy.dtype(dtype).itemsize
        try:
            if count < 0:
                buf = file.read()
                buf = buf[:len(buf) // item_size * item_size]
            else:
                buf = file.read(count * item_size)
        except AttributeError:
            return numpy.fromfile(file, dtype, count, sep)
        return numpy.frombuffer(buf, dtype)
else:
    array_from_file = numpy.fromfile


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
                v = array_from_file(f, self.DIGEST_TYPE, self.NUM_ITEMS).sum()
                h = h.__add__(v)  # Prevent overflow RuntimeWarning.
        self.digest = int(h)

    @property
    def hex_digest(self):
        return "{:016x}".format(self.digest)
