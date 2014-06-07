"""sublib
"""
# import itertools
# import sys


# TODO: port useful common.js stuff
# TODO: split line


from .peak_file import PeakFile
from .scenechange_file import SceneChangeFile
from .time import Time
from .misc import *


from .errors import *
from .formats import *
from .subtitle_file import SubtitleFile
SubtitleFile.formats = FORMATS
del FORMATS

#class Settings:
    #max_num_lines = 2
    #max_line_len = 40
    #min_duration = 792
    #max_duration = 5005
    #min_blank = 250
    #max_rs = 31
    #max_width = 545
#
    #min_rs = 5
    #sc_start_offset = 125
    #sc_end_offset = 125
