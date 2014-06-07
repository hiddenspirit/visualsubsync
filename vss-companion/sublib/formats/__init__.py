from enum import Enum

from .webvtt import WebVTTFile
from .subrip import SubRipFile

__all__ = ["FORMATS", "Formats"]

class CaseInsensitiveDict(dict):
    def __setitem__(self, key, value):
        super().__setitem__(key.lower(), value)

    def __getitem__(self, key):
        return super().__getitem__(key.lower())


FORMATS = CaseInsensitiveDict([
    (WebVTTFile.EXT, WebVTTFile),
    (SubRipFile.EXT, SubRipFile),
])


class Formats(Enum):
    WebVTT = WebVTTFile
    SubRip = SubRipFile
