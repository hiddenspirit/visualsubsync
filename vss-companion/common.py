import glob
import operator
import os
import sys
import threading

from fractions import Fraction

import ffms

from util.get_app_data_dir import get_app_data_dir
from util.media_hash import MediaHash


VERBOSE = False
FFINDEX_MAX_FILES = 32
BUILD_DATE = "2014-10-01 01:21:56"

APP_NAME = "VisualSubSync-Companion"
APP_DATA_DIR = os.path.join(get_app_data_dir(), APP_NAME)

VIDEO_EXTS = [
    ".avi",
    ".flv",
    ".mkv",
    ".mp4",
    ".m4v",
    ".mpg",
    ".ogg",
    ".ogm",
    ".ogv",
    ".ts",
    ".webm",
    ".wmv",
]

SUB_EXTS = [
    ".srt",
]


def purge_old_ffindexes():
    paths = glob.glob(os.path.join(APP_DATA_DIR, "*" + ffms.FFINDEX_EXT))
    data = [(path, os.path.getatime(path)) for path in paths]
    data = sorted(data, key=operator.itemgetter(1))
    for path, atime in data[:len(data)-FFINDEX_MAX_FILES]:
        os.remove(path)


def get_video_source(file_path, num_threads=0):
    ffindex_filepath = get_index_path(file_path)
    if os.path.isfile(ffindex_filepath):
        try:
            #FIXME: this can hang with truncated index files.
            index = ffms.Index.read(ffindex_filepath, file_path)
        except ffms.Error as ffms_error:
            try:
                os.remove(ffindex_filepath)
                index = ffms.Index.make(file_path)
                index.write(ffindex_filepath)
                print("Index remade: {}".format(ffms_error), file=sys.stderr)
            except Exception:
                raise ffms_error
    else:
        index = ffms.Index.make(file_path)
        async_index_write(index, ffindex_filepath)
    async_purge_old_ffindexes()
    return VideoSource(file_path, index=index, num_threads=num_threads)


def get_index_path(file_path):
    hex_digest = MediaHash(file_path).hex_digest
    return os.path.join(APP_DATA_DIR, hex_digest + ffms.FFINDEX_EXT)


def print_error(msg):
    print("build date: {}".format(BUILD_DATE), file=sys.stderr)
    print(msg, file=sys.stderr)


def get_fps(vsource):
    return Fraction(vsource.properties.FPSNumerator,
                    vsource.properties.FPSDenominator)


def async_index_write(index, path):
    def write_index():
        if not os.path.isdir(APP_DATA_DIR):
            os.makedirs(APP_DATA_DIR)
        index.write(path)

    threading.Thread(target=write_index).start()


def async_purge_old_ffindexes():
    threading.Thread(target=purge_old_ffindexes).start()


class VideoSource(ffms.VideoSource):
        @property
        def track(self):
            """Track from video source
            """
            if self._track is None:
                format_name = ffms.Indexer(self.index.source_file).format_name
                format_names = set(format_name.split(","))
                if "matroska" in format_names:
                    self._track = MKVVideoTrack(
                        ffms.FFMS_GetTrackFromVideo(self._source),
                        self.track_number, self.index, self.properties.fps)
                elif "mp4" in format_names:
                    self._track = MP4VideoTrack(
                        ffms.FFMS_GetTrackFromVideo(self._source),
                        self.track_number, self.index)
                else:
                    self._track = super().track
            return self._track


class MKVVideoTrack(ffms.VideoTrack):
    def __init__(self, track, number, index, fps):
        super().__init__(track, number, index)
        self._fps = fps

    @property
    def timecodes(self):
        """List of timecodes
        """
        if self._timecodes is None:
            original = super().timecodes
            if self._fps.denominator == 1001:
                self._timecodes = [self._round_timing(t) for t in original]
                if VERBOSE:
                    print("fixed timecodes for {!r}:\n{!r}\n{!r}".format(
                          self.index.source_file,
                          original[:5], self._timecodes[:5],
                          file=sys.stderr))
            else:
                self._timecodes = original
        return self._timecodes

    def _round_timing(self, timing):
        num, den = self._fps.numerator, self._fps.denominator
        return round(timing * num / den / 1000) * 1000 * den / num


# Workaround for https://code.google.com/p/ffmpegsource/issues/detail?id=109

class MP4VideoTrack(ffms.VideoTrack):
    @property
    def timecodes(self):
        """List of timecodes
        """
        if self._timecodes is None:
            original = super().timecodes
            first_pts = self.frame_info_list[0].PTS
            frame_duration = ((self.frame_info_list[1].PTS - first_pts) *
                              self.time_base)
            first_time = first_pts * self.time_base
            if (first_time > 2 * frame_duration and
                    (first_time / frame_duration).denominator != 1):
                delta = int(first_pts - 2 * frame_duration / self.time_base)
                num, den = self.time_base.numerator, self.time_base.denominator
                self._timecodes = [(frame_info.PTS - delta) * num / den
                                   for frame_info in self.frame_info_list]
                if VERBOSE:
                    print("fixed timecodes for {!r}:\n{!r}\n{!r}".format(
                          self.index.source_file,
                          original[:5], self._timecodes[:5],
                          file=sys.stderr))
            else:
                self._timecodes = original
        return self._timecodes
