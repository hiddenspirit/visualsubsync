#!/usr/bin/env python3
"""Check grammar
"""
import argparse
import os
import subprocess
import sys
import time
import winreg

import win32api
import win32gui
import pywintypes

import guess_language
guess_language.use_enchant(False)

import sublib
import common


def get_name(path):
    return os.path.splitext(os.path.basename(path))[0]


def poll_window(path, timeout=60):
    class Checker:
        def __init__(self, filename):
            self.filename = filename
            self.hwnd = None

    def enum_window_proc(hwnd, checker):
        if checker.filename in win32gui.GetWindowText(hwnd):
            checker.hwnd = hwnd
            return False
        return True

    checker = Checker(os.path.basename(path))
    start_time = time.time()

    while True:
        try:
            win32gui.EnumWindows(enum_window_proc, checker)
        except pywintypes.error as e:
            if e.winerror:
                raise
        if checker.hwnd:
            break
        if time.time() - start_time > timeout:
            raise TimeoutError(
                "couldn't find a window for {!r}".format(checker.filename))
        time.sleep(0.1)

    while win32gui.IsWindow(checker.hwnd):
        time.sleep(0.5)


def has_antidote_agent():
    class AgentSearch:
        def __init__(self):
            self.found = False

    agent_search = AgentSearch()

    def enum_window_proc(hwnd, agent_search):
        if win32gui.GetWindowText(hwnd) == "AgentAntidote":
            agent_search.found = True
            return False
        return True

    try:
        win32gui.EnumWindows(enum_window_proc, agent_search)
    except pywintypes.error as e:
        if e.winerror:
            raise

    return agent_search.found


def get_grammar_checker(language):
    GRAMMAR_CHECKERS = {
        "Antidote": (
            r"SOFTWARE\Druide informatique inc.\Antidote",
            "DossierAntidote",
            "Antidote.exe",
        ),
        "LibreOffice": (
            r"SOFTWARE\LibreOffice\UNO\InstallPath",
            None,
            "swriter.exe",
        ),
    }

    def get_path(checker_name):
        sub_key, value, name = GRAMMAR_CHECKERS[checker_name]
        with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, sub_key) as k:
            grammar_checker_dir = winreg.QueryValueEx(k, value)[0]
        grammar_checker = os.path.join(grammar_checker_dir, name)
        if not os.path.isfile(grammar_checker):
            raise FileNotFoundError("can't find {}".format(checker_name))
        return grammar_checker

    preferred_checkers = ["LibreOffice"]

    if language == "fr":
        preferred_checkers.insert(0, "Antidote")

    for checker_name in preferred_checkers:
        try:
            name, grammar_checker = checker_name, get_path(checker_name)
        except FileNotFoundError:
            continue
        break
    else:
        raise FileNotFoundError("can't find {}".format(preferred_checkers[0]))

    return name, grammar_checker


def parse_args():
    parser = argparse.ArgumentParser("Grammar checker")
    parser.add_argument("subtitle_file",
                        help="subtitle filename")
    parser.add_argument("--grammar-checker",
                        help="external grammar checker")
    return parser.parse_args()


def main():
    args = parse_args()
    subtitle_file = os.path.abspath(
        win32api.GetLongPathNameW(args.subtitle_file))
    ts_path = os.path.splitext(subtitle_file)[0] + ".txt"
    sub_file = sublib.SubtitleFile.load(subtitle_file)

    msg = None
    with open(ts_path, "w", encoding="utf-8-sig", newline="\n") as f:
        f.write(sub_file.transcript)

    try:
        try:
            if args.grammar_checker:
                grammar_checker = args.grammar_checker
                name = get_name(grammar_checker)
                if not os.path.isfile(grammar_checker):
                    raise FileNotFoundError(
                        "can't find {!r}".format(grammar_checker))
            else:
                language = sub_file.guess_language()
                name, grammar_checker = get_grammar_checker(language)
        except FileNotFoundError as e:
            print(e)
            return 2

        if name.lower() == "antidote":
            has_agent = has_antidote_agent()
            subprocess.call([grammar_checker, ts_path])
            if has_agent:
                poll_window(ts_path)
        else:
            subprocess.call([grammar_checker, ts_path])

        with open(ts_path, "r", encoding="utf-8-sig") as f:
            transcript = f.read()

        if transcript != sub_file.transcript:
            changes = sub_file.update_transcript(transcript)
            sub_file.save()
            msg = "{} updated cues".format(changes)
            return 0
        else:
            msg = "no changes"
            return 1
    finally:
        os.remove(ts_path)
        if msg:
            print("{}: {}".format(os.path.basename(sub_file.file), msg))


if __name__ == "__main__":
    sys.exit(main())
