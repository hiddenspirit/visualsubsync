#!/usr/bin/env python3
"""Make scene change file
"""
import argparse
import os
import sys

import sublib
import common


def parse_args():
    parser = argparse.ArgumentParser("Make scene change file")
    parser.add_argument("video_file",
                        help="video media filename")
    return parser.parse_args()

    
def main():
    args = parse_args()

    if not os.path.isfile(args.video_file):
        print("{!r} file does not exist.".format(args.video_file),
              file=sys.stderr)
        return 1

    vsource = common.get_video_source(args.video_file)
    sc_file = sublib.SceneChangeFile.from_source(vsource)
    sc_file.save()
    print(sc_file.file)


if __name__ == "__main__":
    sys.exit(main())
