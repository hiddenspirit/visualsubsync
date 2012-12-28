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


import struct, os

def media_hash(name):
    longlongformat = 'q'  # long long
    bytesize = struct.calcsize(longlongformat)

    with open(name, "rb") as f:
        filesize = os.path.getsize(name)
        hash = filesize

        # if filesize < 65536 * 2:
               # return "SizeError"

        for x in range(65536 // bytesize):
                buffer = f.read(bytesize)
                (l_value,) = struct.unpack(longlongformat, buffer)
                hash += l_value
                hash = hash & 0xFFFFFFFFFFFFFFFF #to remain as 64bit number

        f.seek(max(0, filesize - 65536), 0)
        for x in range(65536 // bytesize):
                buffer = f.read(bytesize)
                (l_value,) = struct.unpack(longlongformat, buffer)
                hash += l_value
                hash = hash & 0xFFFFFFFFFFFFFFFF

    returnedhash =  "%016x" % hash
    return returnedhash
