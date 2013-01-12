#!/usr/bin/env python3
"""Scan for missing scene changes
"""
import argparse
import os
import sys
import time

import multiprocessing
import concurrent.futures

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
    parser.add_argument("--threshold", type=int, default=6,
                        help="threshold")
    parser.add_argument("--time-output", action="store_true",
                        help="time output")
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
    filter_offset = args.filter_offset
    threshold = args.threshold

    if args.time_output:
        time_output = lambda t: sublib.Time.from_int(int(t))
    else:
        time_output = int

    fps = common.get_fps(vsource)
    timings = []
    for sub in sub_file.sub_list:
        start_stops = [
            (sub.start, min(sub.start + filter_offset, sub.stop)),
            (max(sub.stop - filter_offset, sub.start), sub.stop),
        ]
        for start, stop in start_stops:
            if sc_file.contains(start, stop):
                continue
            timings.append((sub.index, (start, stop)))
    for index, sc_time in sublib.SceneChangeFile.scan_timings(
            vsource, timings, threshold):
        sc_time = common.round_timing(sc_time, fps)
        print("{}\t{}".format(index, time_output(sc_time)))

    #with vsource.output_format(*sublib.SceneChangeFile.OUTPUT_FORMAT):
        #for sub in sub_file.sub_list:
            #start_stops = [
                #(sub.start, min(sub.start + filter_offset, sub.stop)),
                #(max(sub.stop - filter_offset, sub.start), sub.stop),
            #]
            #for start, stop in start_stops:
                #if sc_file.contains(start, stop):
                    #continue
                #sc_time = sublib.SceneChangeFile.scan(vsource, start, stop,
                                                      #threshold)
                #if sc_time is None:
                    #continue
                #print("{}\t{}".format(sub.index, time_output(sc_time)))


def f(video_file, timings, threshold, fps):
    vsource = common.get_video_source(video_file)
    for index, sc_time in sublib.SceneChangeFile.scan_timings(
            vsource, timings, threshold):
        pass


if __name__ == "__main__":
    sys.exit(main())
