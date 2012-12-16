#!/usr/bin/env python3
"""Find scene change
"""

import argparse
import glob
import operator
import os
import sys

import ffms
import sublib
from util.get_app_data_dir import get_app_data_dir
from util.media_hash import MediaHash


FFINDEX_MAX_FILES = 24
MAX_RANGE = 30000

APP_NAME = "VisualSubSync-Companion"
APP_DATA_DIR = os.path.join(get_app_data_dir(), APP_NAME)


def parse_args():
    parser = argparse.ArgumentParser("Find scene change")
    parser.add_argument("video_file",
                        help="video media filename")
    parser.add_argument("--start-time", type=int, metavar="ms",
                        help="start time")
    parser.add_argument("--end-time", type=int, metavar="ms",
                        help="end time")
    return parser.parse_args()


def purge_old_ffindexes():
    paths = glob.glob(os.path.join(APP_DATA_DIR, "*" + ffms.FFINDEX_EXT))
    data = [(path, os.path.getatime(path)) for path in paths]
    data = sorted(data, key=operator.itemgetter(1))
    for path, adate in data[:len(data)-FFINDEX_MAX_FILES]:
        os.remove(path)


def main():
    args = parse_args()

    if not os.path.isfile(args.video_file):
        print("{!r} file does not exist.".format(args.video_file),
              file=sys.stderr)
        return 1

    if args.start_time is None or args.end_time is None:
        print("Both start and end time are required.", file=sys.stderr)
        return 1

    if args.start_time > args.end_time:
        print("Start time must come before end time.", file=sys.stderr)
        return 1
        
    if args.end_time - args.start_time > MAX_RANGE:
        print("Range must not exceed {} ms.".format(MAX_RANGE),
              file=sys.stderr)
        return 1

    hex_digest = MediaHash(args.video_file).hex_digest
    ffindex_filepath = os.path.join(APP_DATA_DIR,
                                    hex_digest + ffms.FFINDEX_EXT)

    if os.path.isfile(ffindex_filepath):
        index = ffms.Index.read(ffindex_filepath, args.video_file)
        vsource = ffms.VideoSource(args.video_file, index=index)
    else:
        vsource = ffms.VideoSource(args.video_file)
        if not os.path.isdir(APP_DATA_DIR):
            os.makedirs(APP_DATA_DIR)
        vsource.index.write(ffindex_filepath)

    purge_old_ffindexes()

    sc_time = sublib.SceneChangeFile.find(vsource,
                                          args.start_time, args.end_time)

    print(sc_time)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:
        print(e, file=sys.stderr)
        input()
