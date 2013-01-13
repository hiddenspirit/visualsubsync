#!/usr/bin/env python3
"""Scan for missing scene changes
"""
import argparse
import os
import sys
import time

import sublib
import sublib.old
import common


def parse_args():
    parser = argparse.ArgumentParser(
        "Find scene change",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("video_file",
                        help="video filename")
    parser.add_argument("sub_file",
                        help="subtitle filename")
    parser.add_argument("--sc-file",
                        help="scene change filename")
    parser.add_argument("--filter-offset", metavar="ms", type=int, default=500,
                        help="filter offset")
    parser.add_argument("--threshold", type=float, default=6.0,
                        help="threshold")
    parser.add_argument("--time-output", action="store_true",
                        help="time output")
    parser.add_argument("--apply", action="store_true",
                        help="apply suggestions")
    return parser.parse_args()


def main():
    args = parse_args()

    if not os.path.isfile(args.video_file):
        common.print_error("{!r} video file does not exist."
                           .format(args.video_file))
        return 11

    if not os.path.isfile(args.sub_file):
        common.print_error("{!r} subtitle file does not exist."
                           .format(args.sub_file))
        return 12

    if args.sc_file and os.path.isfile(args.sc_file):
        sc_file = sublib.SceneChangeFile.load(args.sc_file)
    else:
        path = os.path.splitext(args.video_file)[0] + ".scenechange"
        if os.path.isfile(path):
            sc_file = sublib.SceneChangeFile.load(path)
        else:
            sc_file = sublib.SceneChangeFile.from_source(vsource)

    vsource = common.get_video_source(args.video_file)
    sub_file = sublib.old.SubRipFile(args.sub_file)
    filter_offset = args.filter_offset + 1
    threshold = args.threshold

    if args.time_output:
        time_output = lambda t: sublib.Time.from_int(int(t))
    else:
        time_output = int

    fps = common.get_fps(vsource)
    timings = []
    missing_list = []

    for sub in sub_file.sub_list:
        start_stops = [
            (sub.start, min(sub.start + filter_offset, sub.stop)),
            (max(sub.stop - filter_offset, sub.start), sub.stop),
        ]
        for start, stop in start_stops:
            if not sc_file.contains(start, stop):
                timings.append((start, stop))

    for sc_time in sublib.SceneChangeFile.scan_missing(
        vsource, timings, threshold
    ):
        sc_time = common.round_timing(sc_time, fps)
        print(time_output(sc_time))
        missing_list.append(sc_time)

    if args.apply:
        count = 0
        for sc_time in missing_list:
            if sc_time not in sc_file:
                sc_file.add(sc_time)
                count += 1
        if count:
            sc_file.save()
        print("Added {} scene changes.".format(len(missing_list)))


if __name__ == "__main__":
    sys.exit(main())
