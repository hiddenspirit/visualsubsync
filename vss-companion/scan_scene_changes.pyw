#!/usr/bin/env python3
#-*- coding: utf-8 -*-
"""Scan for scene changes
"""

import argparse
import os
import sys
from concurrent import futures
import threading

import win32gui
from win32con import WM_APP

from PyQt4 import QtCore, QtGui
#from PyQt4 import uic
from PyQt4.QtCore import Qt

from scan_scene_changes_window import Ui_MainWindow

import sublib.old
import common


getOpenFileName = QtGui.QFileDialog.getOpenFileNameAndFilter
Signal = QtCore.pyqtSignal
Slot = QtCore.pyqtSlot

VIDEO_EXTS = [
    ".avi",
    ".mkv",
    ".mp4",
    ".mpg",
    ".ts",
    ".wmv",
]

SUB_EXTS = [
    ".srt",
]

WM_APP_COMPANION_CLOSED = WM_APP + 0x00
WM_APP_COMPANION_OPENED = WM_APP + 0x01

WM_APP_SAVE = WM_APP + 0x10
WM_APP_SET_POSITION = WM_APP + 0x11
WM_APP_INSERT_SC = WM_APP + 0x12
WM_APP_DELETE_SC = WM_APP + 0x13

WM_APP_EXIT = WM_APP + 0x100
WM_APP_SAVE_DONE = WM_APP + 0x101
WM_APP_VIDEO_CHANGED = WM_APP + 0x102
WM_APP_SUB_CHANGED = WM_APP + 0x103



class ScanSceneChangeApplication(QtGui.QApplication):
    def winEventFilter(self, msg):
        if msg.message == WM_APP_EXIT:
            self.exit(0)
            return True, id(msg)
        elif msg.message == WM_APP_SAVE_DONE:
            form.saved_event()
            return True, id(msg)
        elif msg.message == WM_APP_VIDEO_CHANGED:
            form.closeEvent()
            return True, id(msg)
        elif msg.message == WM_APP_SUB_CHANGED:
            form.closeEvent()
            return True, id(msg)
        return False, id(msg)


class ScanSceneChangeForm(QtGui.QMainWindow):
    index_created = Signal()
    bogus_scene_change_found = Signal(int, float)
    missing_scene_change_found = Signal(int, float, float)
    scan_done = Signal()
    modal = True
    job = None
    executor = futures.ThreadPoolExecutor(1)

    def __init__(self, args, parent=None):
        super().__init__(parent)
        # self.ui = uic.loadUi("scan_scene_changes_window.ui")
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)

        if args.video_file:
            self.video_file = args.video_file
        else:
            self.model = False
            self.video_file = self.get_open_filename(self.tr("Video file"),
                                                     self.video_dialog_filter)
        if args.sub_file:
            self.sub_file = args.sub_file
        else:
            self.model = False
            self.sub_file = self.get_open_filename(self.tr("Subtitle file"),
                                                   self.sub_dialog_filter)

        if self.modal:
            if args.vss:
                self.vss = args.vss
            else:
                self.vss = win32gui.FindWindow(None, "VisualSubSync")
            if self.vss:
                self.setWindowFlags(Qt.Tool | Qt.WindowStaysOnTopHint)
                win32gui.PostMessage(self.vss, WM_APP_COMPANION_OPENED, self.winId(), 0)
        else:
            self.vss = None

        self.ui.doubleSpinBoxRatio.valueChanged.connect(self.on_spinbox_ratio_changed)
        self.ui.horizontalSliderRatio.valueChanged.connect(self.on_slider_ratio_changed)
        self.ui.toolButtonReset.clicked.connect(self.on_reset_clicked)
        self.ui.toolButtonScan.clicked.connect(self.on_scan_clicked)
        self.ui.toolButtonCancel.clicked.connect(self.on_cancel_clicked)
        self.bogus_scene_change_found.connect(self.on_bogus_scene_change_found)
        self.missing_scene_change_found.connect(self.on_missing_scene_change_found)
        self.scan_done.connect(self.on_scan_done)
        self.reset_parameters()

        # self.ui.show()
        self.show()

        index_path = common.get_index_path(self.video_file)
        if os.path.isfile(index_path):
            self.enable_scan()
        else:
            app.setOverrideCursor(QtGui.QCursor(Qt.BusyCursor))
            self.ui.statusbar.showMessage(self.tr("Indexing…"))
            self.index_created.connect(self.on_index_created)
            self.executor.submit(self.create_index)

    def closeEvent(self, event=None):
        if self.job is not None:
            self.stop_job_event.set()
            self.job.result()
        if self.vss:
            win32gui.PostMessage(self.vss, WM_APP_COMPANION_CLOSED, self.winId(), 0)
        app.exit(0)

    def create_index(self):
        common.get_video_source(self.video_file)
        self.index_created.emit()

    @property
    def video_dialog_filter(self):
        return "{} ({})".format(self.tr("Video files"),
            " ".join("*" + ext for ext in VIDEO_EXTS))

    @property
    def sub_dialog_filter(self):
        return "{} ({})".format(self.tr("Subtitle files"),
            " ".join("*" + ext for ext in SUB_EXTS))

    def get_open_filename(self, title=None, dialog_filter=None):
        star_filter = "{} (*.*)".format(self.tr("All files"))
        if dialog_filter:
            dialog_filter += ";;" + star_filter
        else:
            dialog_filter = star_filter
        filename = getOpenFileName(self, title, ".", dialog_filter)[0]
        if os.path.isfile(filename):
            return filename
        raise ValueError("invalid file: {!r}".format(filename))

    def on_index_created(self):
        app.restoreOverrideCursor()
        self.enable_scan()

    def enable_scan(self):
        self.ui.statusbar.showMessage(self.tr("Ready"))
        self.ui.toolButtonScan.setEnabled(True)
        
    def on_spinbox_ratio_changed(self, value):
        self.ui.horizontalSliderRatio.setValue(int(value * 10))
    
    def on_slider_ratio_changed(self, value):
        self.ui.doubleSpinBoxRatio.setValue(value / 10.0)

    @property
    def diff_pct(self):
        return self.ui.spinBoxDifference.value() / 100.0
    
    @diff_pct.setter
    def diff_pct(self, value):
        self.ui.spinBoxDifference.setValue(round(value * 100))

    @property
    def ratio(self):
        return self.ui.doubleSpinBoxRatio.value()
    
    @ratio.setter
    def ratio(self, value):
        self.ui.doubleSpinBoxRatio.setValue(value)

    def on_reset_clicked(self):
        self.reset_parameters()

    def reset_parameters(self):
        self.diff_pct = sublib.SceneChangeFile.DIFF_PCT_THRESHOLD
        self.ratio = sublib.SceneChangeFile.RATIO_THRESHOLD

    def set_enabled_parameters(self, enabled=True):
        self.ui.checkBoxBogus.setEnabled(enabled)
        self.ui.checkBoxMissing.setEnabled(enabled)
        self.ui.spinBoxDifference.setEnabled(enabled)
        self.ui.horizontalSliderDifference.setEnabled(enabled)
        self.ui.doubleSpinBoxRatio.setEnabled(enabled)
        self.ui.horizontalSliderRatio.setEnabled(enabled)
        self.ui.toolButtonReset.setEnabled(enabled)

    @property
    def scan_bogus(self):
        return self.ui.checkBoxBogus.isChecked()

    @property
    def scan_missing(self):
        return self.ui.checkBoxMissing.isChecked()

    def on_scan_clicked(self):
        app.setOverrideCursor(QtGui.QCursor(Qt.BusyCursor))
        self.ui.toolButtonScan.setEnabled(False)
        self.ui.toolButtonCancel.setEnabled(True)
        self.set_enabled_parameters(False)
        self.ui.statusbar.showMessage("Scanning…")
        self.job = None
        if self.vss:
            win32gui.PostMessage(self.vss, WM_APP_SAVE, 0, 0)
        else:
            self.saved_event()

    def on_cancel_clicked(self):
        self.ui.toolButtonCancel.setEnabled(False)

        if self.job is None:
            self.finalize_cancel()
        else:
            #self.ui.statusbar.showMessage(self.tr("Canceling…"))
            self.stop_job_event.set()

    def finalize_cancel(self):
        self.ui.toolButtonScan.setEnabled(True)
        self.set_enabled_parameters(True)
        self.ui.statusbar.showMessage(self.tr("Canceled"))
        app.restoreOverrideCursor()

    def saved_event(self):
        self.reset_tree()
        self.stop_job_event = threading.Event()
        self.job = self.executor.submit(self.scan_thread, self.stop_job_event)

    def scan_thread(self, cancel_job_event):        
        vsource = common.get_video_source(self.video_file)
        sc_file = self.load_sc_file(vsource)
        sub_file = sublib.old.SubRipFile(self.sub_file)
        timings = [(sub.start, sub.stop) for sub in sub_file.sub_list]

        if self.scan_bogus:
            for sc_time, diff_pct in sc_file.scan_bogus(vsource, timings, self.diff_pct, cancel_event=cancel_job_event):
                self.bogus_scene_change_found.emit(sc_time, diff_pct)
                self.count += 1

        if self.scan_missing:
            fps = common.get_fps(vsource)
            for sc_time, diff_pct, ratio in sc_file.scan_missing(vsource, timings, self.diff_pct, self.ratio, cancel_event=cancel_job_event):
                sc_time = common.round_timing(sc_time, fps)
                self.missing_scene_change_found.emit(sc_time, diff_pct, ratio)
                self.count += 1

        self.scan_done.emit()
    
    def on_scan_done(self):
        if self.stop_job_event.is_set():
            self.finalize_cancel()
        else:
            self.ui.toolButtonCancel.setEnabled(False)
            self.ui.toolButtonScan.setEnabled(True)
            self.set_enabled_parameters(True)
            self.ui.statusbar.showMessage(self.tr("%n results(s)", "", self.count))
            app.restoreOverrideCursor()

    def on_bogus_scene_change_found(self, sc_time, diff_pct):
        self.add_child(self.bogus_root, sc_time, diff_pct)

    def on_missing_scene_change_found(self, sc_time, diff_pct, ratio):
        self.add_child(self.missing_root, sc_time, diff_pct, ratio)

    @classmethod
    def time_output(cls, t):
        return str(sublib.Time.from_int(int(t)))

    @classmethod
    def load_sc_file(cls, vsource):
        path = os.path.splitext(vsource.index.source_file)[0] + ".scenechange"
        if os.path.isfile(path):
            return sublib.SceneChangeFile.load(path)
        else:
            return sublib.SceneChangeFile.from_source(vsource)

    def reset_tree(self):
        self.count = 0
        self.errors = {}
        self.ui.treeWidget.clear()
        if self.scan_bogus:
            self.bogus_root = self.create_root("Bogus")
            self.bogus_root.setExpanded(True)
        if self.scan_missing:
            self.missing_root = self.create_root("Missing")
            self.missing_root.setExpanded(True)
    
    def create_root(self, name):
        item = QtGui.QTreeWidgetItem(self.ui.treeWidget)
        item.setText(0, name)
        return item

    def add_child(self, parent, sc_time, diff=None, ratio=None):
        item = QtGui.QTreeWidgetItem()
        item.setText(0, self.time_output(sc_time))
        if diff is not None:
            item.setText(1, "{:.1%}".format(diff))
            item.setTextAlignment(1, Qt.AlignRight)
        if ratio is not None:
            item.setText(2, "{:.2f}".format(ratio))
            item.setTextAlignment(2, Qt.AlignRight)
        parent.addChild(item)
        self.errors[id(item)] = sc_time


def parse_args():
    parser = argparse.ArgumentParser("Scan for scene changes")
    parser.add_argument("--video_file",
                        help="video filename")
    parser.add_argument("--sub_file",
                        help="subtitle filename")
    parser.add_argument("--vss", type=int,
                        help="VisualSubSync hWnd")
    return parser.parse_args()


if __name__ == "__main__":
    QtCore.QTextCodec.setCodecForTr(QtCore.QTextCodec.codecForName("utf-8"))
    app = ScanSceneChangeApplication(sys.argv)
    form = ScanSceneChangeForm(parse_args())
    sys.exit(app.exec())
