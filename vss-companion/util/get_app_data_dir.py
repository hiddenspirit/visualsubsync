#!/usr/bin/env python3

import os
import sys


if os.name == "nt":
    def get_app_data_dir():
        return os.environ["APPDATA"]

elif os.name == "mac":
    def get_app_data_dir():
        return os.path.expanduser("~/Library/Application Support")

else:
    def get_app_data_dir():
        return os.path.expanduser("~/.config")


def main():
    print(get_app_data_dir())


if __name__ == "__main__":
    sys.exit(main())
