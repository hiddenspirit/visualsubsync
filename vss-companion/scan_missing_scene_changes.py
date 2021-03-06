#!/usr/bin/env python3
"""Scan for missing scene changes
"""
import argparse
import os
import sys

import sublib
import common


def parse_args():
    parser = argparse.ArgumentParser(
        "Scan for missing scene changes",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("video_file",
                        help="video filename")
    parser.add_argument("sub_file",
                        help="subtitle filename")
    parser.add_argument("--sc-file",
                        help="scene change filename")
    parser.add_argument("--diff-pct-threshold", type=float,
                        default=sublib.SceneChangeFile.DIFF_PCT_THRESHOLD,
                        help="diff-pct-threshold")
    parser.add_argument("--ratio-threshold", type=float,
                        default=sublib.SceneChangeFile.RATIO_THRESHOLD,
                        help="ratio-threshold")
    parser.add_argument("--filter-offset", metavar="ms", type=int,
                        default=None,
                        help="filter offset")
    parser.add_argument("--milliseconds", action="store_true",
                        help="display timestamp in milliseconds")
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

    vsource = common.get_video_source(args.video_file)

    if args.sc_file and os.path.isfile(args.sc_file):
        sc_file = sublib.SceneChangeFile.load(args.sc_file)
    else:
        path = os.path.splitext(args.video_file)[0] + ".scenechange"
        if os.path.isfile(path):
            sc_file = sublib.SceneChangeFile.load(path)
        else:
            sc_file = sublib.SceneChangeFile.from_source(vsource)

    sub_file = sublib.SubtitleFile.load(args.sub_file)
    missing_list = []
    timings = [(sub.start_time, sub.end_time) for sub in sub_file]
    time_output = (int if args.milliseconds
                   else lambda t: sublib.Time.from_int(int(t)))

    for sc_time, diff_pct, ratio in sc_file.scan_missing(
            vsource, timings, args.diff_pct_threshold, args.ratio_threshold,
            args.filter_offset):
        print("{}\t{:.1%}\t{:.2f}".format(time_output(sc_time),
                                          diff_pct, ratio))
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
