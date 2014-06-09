#!/usr/bin/env python3
"""Check grammar
"""
import argparse
import glob
import itertools
import os
import subprocess
import re
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
        if win32gui.GetWindowText(hwnd).lower() == "agentantidote":
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
    def get_antidote():
        antidote_key = r"Software\Druide informatique inc.\Antidote"
        value = "DossierAntidote"
        names = ["Antidote.exe", "antido32.exe"]

        for name in names:
            latest_sub_key = None
            try:
                with winreg.OpenKeyEx(
                        winreg.HKEY_CURRENT_USER, antidote_key) as k:
                    try:
                        for n in itertools.count():
                            sub_key = winreg.EnumKey(k, n)
                            if sub_key[0].isdigit():
                                latest_sub_key = sub_key
                    except OSError:
                        pass
                if latest_sub_key:
                    sub_key = r"{}\{}\Installation".format(antidote_key,
                                                           latest_sub_key)
                    with winreg.OpenKeyEx(
                            winreg.HKEY_CURRENT_USER, sub_key) as k:
                        directory = winreg.QueryValueEx(k, value)[0]
                    path = os.path.join(directory, name)
                    if os.path.isfile(path):
                        return path
            except FileNotFoundError:
                pass

            try:
                return get_path_from_registry(
                    winreg.HKEY_LOCAL_MACHINE, antidote_key, value, name)
            except FileNotFoundError:
                pass

            for env in ["PROGRAMFILES", "PROGRAMW6432"]:
                try:
                    program_files = os.environ[env]
                except KeyError:
                    continue
                for antidote_dir in [
                        r"Druide\Antidote*\Programmes*", r"Druide\Antidote*"]:
                    path = os.path.join(program_files, antidote_dir, name)
                    paths = glob.glob(path)
                    if paths:
                        return sorted(paths)[-1]

        raise FileNotFoundError("can't find Antidote")

    def get_libre_office():
        key = winreg.HKEY_LOCAL_MACHINE
        sub_key = r"SOFTWARE\LibreOffice\UNO\InstallPath"
        value = None
        name = "swriter.exe"
        return get_path_from_registry(key, sub_key, value, name)

    def get_path_from_registry(key, sub_key, value, name):
        with winreg.OpenKey(key, sub_key) as k:
            directory = winreg.QueryValueEx(k, value)[0]
        path = os.path.join(directory, name)
        if not os.path.isfile(path):
            raise FileNotFoundError(
                "can't find in registry: {}, {}, {}, {}"
                .format(key, sub_key, value, name))
        return path

    GRAMMAR_CHECKERS = {
        "Antidote": get_antidote,
        "LibreOffice": get_libre_office,
    }

    preferred_checkers = ["LibreOffice"]

    if language == "fr":
        preferred_checkers.insert(0, "Antidote")

    for checker_name in preferred_checkers:
        try:
            name, grammar_checker = \
                checker_name, GRAMMAR_CHECKERS[checker_name]()
        except FileNotFoundError:
            continue
        break
    else:
        raise FileNotFoundError("can't find {}".format(preferred_checkers[0]))

    return name, grammar_checker


def update_transcript_old_antidote(sub_file, value):
    old_len = len(sub_file.transcript.split("\n"))
    value = value.split("\n")
    if old_len != len(value):
        raise ValueError("number of lines mismatch: {}, {}"
                         .format(old_len, len(value)))
    n = 0
    changes = 0
    for subtitle in sub_file:
        num_lines = len(subtitle.lines)
        old_text = subtitle.plain_text_no_dialog
        new_text = "\n".join(value[n:n+num_lines])
        if old_text != new_text and old_text.replace("œ", "?") != new_text:
            subtitle.plain_text_no_dialog = new_text
            changes += 1
        n += num_lines

    return changes


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
    language = sub_file.guess_language()

    try:
        if args.grammar_checker:
            grammar_checker = args.grammar_checker
            name = get_name(grammar_checker)
            if not os.path.isfile(grammar_checker):
                raise FileNotFoundError(
                    "can't find {!r}".format(grammar_checker))
        else:
            name, grammar_checker = get_grammar_checker(language)
    except FileNotFoundError as e:
        print("{} ({}): {}"
              .format(os.path.basename(sub_file.file), language, e))
        return 127

    with open(ts_path, "w", encoding="utf-8-sig", newline="\n") as f:
        f.write(sub_file.transcript)
    msg = None

    try:
        if name.lower() == "antidote":
            has_agent = has_antidote_agent()
            subprocess.call([grammar_checker, ts_path])
            if has_agent:
                poll_window(ts_path)
        else:
            subprocess.call([grammar_checker, ts_path])

        with open(ts_path, "rb") as f:
            buf = f.read()
        encoding = sub_file._detect_encoding(buf)
        transcript = re.sub(r"\r\n|\r", "\n", buf.decode(encoding))

        if transcript != sub_file.transcript:
            try:
                if encoding.startswith("utf"):
                    changes = sub_file.update_transcript(transcript)
                else:
                    changes = update_transcript_old_antidote(
                        sub_file, transcript)
            except ValueError as e:
                msg = e.args[0]
                return 2
            sub_file.save()
            msg = "{} updated cue{}".format(
                changes, "s" if changes != 1 else "")
            return 0
        else:
            msg = "no changes"
            return 1
    finally:
        os.remove(ts_path)
        print("{} ({}): {}"
              .format(os.path.basename(sub_file.file), language, msg))


if __name__ == "__main__":
    sys.exit(main())
