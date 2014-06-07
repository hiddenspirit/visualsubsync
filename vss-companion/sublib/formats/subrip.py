import re

from ..subtitle import Subtitle, PARSER
from ..subtitle_file import SubtitleFile
from ..time import Time, ONE_SECOND


class SubRipTime(Time):
    FORMAT = "{h:02d}:{min:02d}:{s:02d},{ms:03d}"

    def __str__(self):
        h, min, s = self.to_h_min_s() #@ReservedAssignment
        s, ms = divmod(round(s * ONE_SECOND), ONE_SECOND)
        return self.FORMAT.format_map(locals())


class SubRipSubtitle(Subtitle):
    Time = SubRipTime
    PARSER = re.compile(PARSER.format(time_pattern=SubRipTime.PATTERN),
                        re.MULTILINE)


class SubRipFile(SubtitleFile):
    DEFAULT_ENCODING = "cp1252"
    EXT = ".srt"
    Subtitle = SubRipSubtitle
