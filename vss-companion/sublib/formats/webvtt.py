from ..subtitle import Subtitle, PARSER
from ..subtitle_file import SubtitleFile
from ..time import Time


class WebVTTSubtitle(Subtitle):
    pass


class WebVTTFile(SubtitleFile):
    EXT = ".vtt"
    Subtitle = WebVTTSubtitle

    def __str__(self):
        return ("WEBVTT\n\n" + "\n".join(str(subtitle) for subtitle in self) +
                "\n")
