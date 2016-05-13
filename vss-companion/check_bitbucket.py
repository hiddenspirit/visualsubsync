#!/usr/bin/env python3
"""Check Bitbucket
"""

import os
import sys
import re
import webbrowser
import urllib.request
import urllib.error

from collections import namedtuple

import win32api
import win32ui
import win32con


APP_NAME = "VisualSubSync"
APP_EXE = APP_NAME + ".exe"
DOWNLOAD_PAGE = "https://bitbucket.org/spirit/visualsubsync/downloads"


class Date(namedtuple("Date", ("year", "month", "day"))):
    @classmethod
    def from_version_info(cls, version_info):
        year = version_info.release
        month, day = divmod(version_info.build, 100)
        return cls(year, month, day)

    def __str__(self):
        return "{:02d}-{:02d}-{:02d}".format(self.year, self.month, self.day)


def get_file_version_info(path):
    """Retrieve version information for the specified file.
    """
    info = win32api.GetFileVersionInfo(path, "\\")
    ms = info["FileVersionMS"]
    ls = info["FileVersionLS"]
    major, minor = win32api.HIWORD(ms), win32api.LOWORD(ms)
    release, build = win32api.HIWORD(ls), win32api.LOWORD(ls)
    VersionInfo = namedtuple("VersionInfo",
                             ("major", "minor", "release", "build"))
    return VersionInfo(major, minor, release, build)


def main():
    try:
        script_dir = os.path.dirname(sys.argv[0])
        vss_path = os.path.join(script_dir, os.path.pardir, APP_EXE)
        vss_version_info = get_file_version_info(vss_path)
        vss_date = Date.from_version_info(vss_version_info)
        with urllib.request.urlopen(DOWNLOAD_PAGE) as f:
            data = f.read()
        pattern = (os.path.splitext(APP_EXE)[0].encode() +
                   rb"-(\d+)-(\d+)-(\d+)-Setup\.exe")
        match = re.search(pattern, data)
        if not match:
            raise ValueError("can't find setup on download page")
        setup_date = Date(*(int(s) for s in match.groups()))
        if setup_date <= vss_date:
            msg = ("Current version: {}\n\nNo updates available."
                   .format(vss_date))
            win32ui.MessageBox(msg, APP_NAME,
                               win32con.MB_ICONINFORMATION)
            return
        msg = ("Current version: {}\nNew version: {}\n\nGo to download page?"
               .format(vss_date, setup_date))
        r = win32ui.MessageBox(msg, APP_NAME,
                               win32con.MB_ICONINFORMATION | win32con.MB_YESNO)
        if r == win32con.IDYES:
            webbrowser.open(DOWNLOAD_PAGE)
    except urllib.error.URLError as e:
        win32ui.MessageBox("Unable to query download page.", APP_NAME,
                           win32con.MB_ICONEXCLAMATION)
    except Exception as e:
        win32ui.MessageBox(str(e), APP_NAME, win32con.MB_ICONERROR)


if __name__ == "__main__":
    sys.exit(main())
