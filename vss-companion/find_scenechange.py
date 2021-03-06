#!/usr/bin/env python3
"""Find scene change
"""
import argparse
import os
import sys

import sublib
import common


MAX_RANGE = 30000


def parse_args():
    parser = argparse.ArgumentParser("Find scene change")
    parser.add_argument("video_file",
                        help="video media filename")
    parser.add_argument("--start-time", type=int, metavar="ms",
                        help="start time")
    parser.add_argument("--end-time", type=int, metavar="ms",
                        help="end time")
    return parser.parse_args()


def main():
    args = parse_args()

    if not os.path.isfile(args.video_file):
        common.print_error("{!r} file does not exist.".format(args.video_file))
        return 11

    if args.start_time is None or args.end_time is None:
        common.print_error("Both start and end time are required.")
        return 12

    if args.start_time > args.end_time:
        common.print_error("Start time must come before end time.")
        return 13

    if args.end_time - args.start_time > MAX_RANGE:
        common.print_error("Range must not exceed {} ms.".format(MAX_RANGE))
        return 14

    vsource = common.get_video_source(args.video_file)

    sc_time = sublib.SceneChangeFile.find(vsource,
                                          args.start_time, args.end_time)

    print(int(sc_time))


if __name__ == "__main__":
    sys.exit(main())
