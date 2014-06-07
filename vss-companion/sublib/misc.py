from bisect import bisect_right
from collections import namedtuple
from fractions import Fraction

__all__ = ["get_rs_index", "FPS_NAMES"]

ReadingSpeedDef = namedtuple("ReadingSpeedDef", ("value", "color", "text"))

READING_SPEED_DEF = [
    ReadingSpeedDef(5.0, 0x9999ff, "TOO SLOW"),
    ReadingSpeedDef(10.0, 0x99ccff, "Slow, acceptable"),
    ReadingSpeedDef(13.0, 0x99ffff, "A bit slow"),
    ReadingSpeedDef(15.0, 0x99ffcc, "Good"),
    ReadingSpeedDef(23.0, 0x99ff99, "Perfect"),
    ReadingSpeedDef(27.0, 0xccff99, "Good"),
    ReadingSpeedDef(31.0, 0xffff99, "A bit fast"),
    ReadingSpeedDef(35.0, 0xffcc99, "Fast, acceptable"),
    ReadingSpeedDef(float("inf"), 0xff9999, "TOO FAST"),
    ]

READING_SPEED_LIST = [e.value for e in READING_SPEED_DEF]


def get_rs_index(rs):
    return bisect_right(READING_SPEED_LIST, rs)


# http://avisynth.org.ru/docs/english/corefilters/fps.htm
FPS_NAMES = {
    "ntsc_film": Fraction(24000, 1001),
    "ntsc_video":  Fraction(30000, 1001),
    "ntsc_double": Fraction(60000, 1001),
    "ntsc_quad": Fraction(120000, 1001),
    "ntsc_round_film": Fraction(2997, 125),
    "ntsc_round_video": Fraction(2997, 100),
    "ntsc_round_double": Fraction(2997, 50),
    "ntsc_round_quad": Fraction(2997, 25),
    "film": Fraction(24, 1),
    "pal_film": Fraction(25, 1),
    "pal_video": Fraction(25, 1),
    "pal_double": Fraction(50, 1),
    "pal_quad": Fraction(100, 1),
    }
