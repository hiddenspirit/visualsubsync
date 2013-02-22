import glob
import operator
import os
import sys
import threading

from fractions import Fraction

import ffms

from util.get_app_data_dir import get_app_data_dir
from util.media_hash import MediaHash


FFINDEX_MAX_FILES = 24
BUILD_DATE = "2013-02-22 17:54:17"

APP_NAME = "VisualSubSync-Companion"
APP_DATA_DIR = os.path.join(get_app_data_dir(), APP_NAME)


def purge_old_ffindexes():
    paths = glob.glob(os.path.join(APP_DATA_DIR, "*" + ffms.FFINDEX_EXT))
    data = [(path, os.path.getatime(path)) for path in paths]
    data = sorted(data, key=operator.itemgetter(1))
    for path, atime in data[:len(data)-FFINDEX_MAX_FILES]:
        os.remove(path)


def get_format_name(file_path):
    return ffms.Indexer(file_path).format_name


def get_video_source(file_path, num_threads=0):
    ffindex_filepath = get_index_path(file_path)
    if os.path.isfile(ffindex_filepath):
        try:
            index = ffms.Index.read(ffindex_filepath, file_path)
        except ffms.Error as ffms_error:
            try:
                os.remove(ffindex_filepath)
                index = ffms.Index.make(file_path)
                index.write(ffindex_filepath)
                print("Index remade: {}".format(ffms_error), file=sys.stderr)
            except Exception as e:
                raise ffms_error
    else:
        index = ffms.Index.make(file_path)
        async_index_write(index, ffindex_filepath)
    async_purge_old_ffindexes()
    return ffms.VideoSource(file_path, index=index, num_threads=num_threads)


def get_index_path(file_path):
    hex_digest = MediaHash(file_path).hex_digest
    return os.path.join(APP_DATA_DIR, hex_digest + ffms.FFINDEX_EXT)


def print_error(msg):
    print("build date: {}".format(BUILD_DATE), file=sys.stderr)
    print(msg, file=sys.stderr)


def get_fps(vsource):
    return Fraction(vsource.properties.FPSNumerator,
                    vsource.properties.FPSDenominator)


def round_timing(timing, fps):
    return (round(timing * fps.numerator / fps.denominator / 1000) *
            1000 * fps.denominator / fps.numerator)


def async_index_write(index, path):
    def write_index():
        if not os.path.isdir(APP_DATA_DIR):
            os.makedirs(APP_DATA_DIR)
        index.write(path)

    threading.Thread(target=write_index).start()


def async_purge_old_ffindexes():
    threading.Thread(target=purge_old_ffindexes).start()
