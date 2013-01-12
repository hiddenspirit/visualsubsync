"""Hash for media files

http://trac.opensubtitles.org/projects/opensubtitles/wiki/HashSourceCodes
"""
import os
import struct


class MediaHash:
    def __init__(self, path):
        # long long
        longlongformat = 'q'
        bytesize = struct.calcsize(longlongformat)

        with open(path, "rb") as f:
            filesize = os.path.getsize(path)
            hash = filesize

            if filesize < 65536 * 2:
                raise NotImplementedError("File too small")

            for x in range(65536 // bytesize):
                    buffer = f.read(bytesize)
                    (l_value,) = struct.unpack(longlongformat, buffer)
                    hash += l_value
                    # to remain as 64bit number
                    hash = hash & 0xFFFFFFFFFFFFFFFF

            f.seek(max(0, filesize - 65536), 0)
            for x in range(65536 // bytesize):
                    buffer = f.read(bytesize)
                    (l_value,) = struct.unpack(longlongformat, buffer)
                    hash += l_value
                    hash = hash & 0xFFFFFFFFFFFFFFFF

        self.digest = hash

    @property
    def hex_digest(self):
        return "{:016x}".format(self.digest)
