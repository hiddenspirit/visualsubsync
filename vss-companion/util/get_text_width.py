#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import sys
from collections import namedtuple
from functools import lru_cache

from PyQt5 import QtGui, QtWidgets, QtPrintSupport


__all__ = ["get_text_width_pixels", "get_text_width_chars"]


FontDef = namedtuple("FontDef", ("font_name", "font_size", "font_bold"))
WidthInfo = namedtuple("WidthInfo", ("width", "index"))


class _TextWidth:
    DEFAULT_FONT_DEF = FontDef("Verdana", 18, True)
    RESOLUTION = 96
    CACHE_SIZE = 2000
    WIDTH_FUNC_CACHE_SIZE = 10

    @lru_cache(CACHE_SIZE)
    def get_pixels(
            self, text,
            font_name=DEFAULT_FONT_DEF.font_name,
            font_size=DEFAULT_FONT_DEF.font_size,
            font_bold=DEFAULT_FONT_DEF.font_bold):
        return self.get_width_info(text.replace("\u202f", "\u2009"),
                                   self.func(font_name, font_size, font_bold))

    def get_chars(self, text):
        return self.get_width_info(text, len)

    def get_width_info(self, text, func):
        width = line_index = 0
        for n, line in enumerate(text.splitlines()):
            w = func(line)
            if w > width:
                width, line_index = w, n
        return WidthInfo(width, line_index)

    def func(self, font_name, font_size, font_bold):
        if not QtWidgets.QApplication.instance():
            self.app = QtWidgets.QApplication(sys.argv)
        self.pd = QtPrintSupport.QPrinter()
        self.pd.setResolution(self.RESOLUTION)
        self.func = self.func2
        return self.func2(font_name, font_size, font_bold)

    @lru_cache(WIDTH_FUNC_CACHE_SIZE)
    def func2(self, font_name, font_size, font_bold):
        weight = QtGui.QFont.Bold if font_bold else QtGui.QFont.Normal
        font = QtGui.QFont(font_name, font_size, weight)
        return QtGui.QFontMetrics(font, self.pd).width


_text_width = _TextWidth()
get_text_width_pixels = _text_width.get_pixels
get_text_width_chars = _text_width.get_chars


def main():
    pass


if __name__ == "__main__":
    sys.exit(main())
