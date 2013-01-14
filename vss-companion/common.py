import glob
import operator
import os
import sys

from fractions import Fraction

import ffms

from util.get_app_data_dir import get_app_data_dir
from util.media_hash import MediaHash


FFINDEX_MAX_FILES = 24
BUILD_DATE = "2013-01-01 18:50:13"

APP_NAME = "VisualSubSync-Companion"
APP_DATA_DIR = os.path.join(get_app_data_dir(), APP_NAME)


def purge_old_ffindexes():
    paths = glob.glob(os.path.join(APP_DATA_DIR, "*" + ffms.FFINDEX_EXT))
    data = [(path, os.path.getatime(path)) for path in paths]
    data = sorted(data, key=operator.itemgetter(1))
    for path, adate in data[:len(data)-FFINDEX_MAX_FILES]:
        os.remove(path)


def get_video_source(file_path, num_threads=None):
    hex_digest = MediaHash(file_path).hex_digest
    ffindex_filepath = os.path.join(APP_DATA_DIR,
                                    hex_digest + ffms.FFINDEX_EXT)
    if os.path.isfile(ffindex_filepath):
        index = ffms.Index.read(ffindex_filepath, file_path)
        vsource = ffms.VideoSource(file_path, index=index,
                                   num_threads=num_threads)
    else:
        vsource = ffms.VideoSource(file_path, num_threads=num_threads)
        if not os.path.isdir(APP_DATA_DIR):
            os.makedirs(APP_DATA_DIR)
        vsource.index.write(ffindex_filepath)
    purge_old_ffindexes()
    return vsource


def print_error(msg):
    print("build date: {}".format(BUILD_DATE), file=sys.stderr)
    print(msg, file=sys.stderr)


def get_fps(vsource):
    return Fraction(vsource.properties.FPSNumerator,
                    vsource.properties.FPSDenominator)


def round_timing(timing, fps):
    return (round(timing * fps.numerator / fps.denominator / 1000) *
            1000 * fps.denominator / fps.numerator)
