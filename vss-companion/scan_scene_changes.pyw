#!/usr/bin/env python3
#-*- coding: utf-8 -*-
"""Scan for scene changes
"""

import argparse
import os
import sys
import threading
from collections import namedtuple
from concurrent import futures

import pywintypes
import win32gui
import win32api
from win32con import WM_APP
from ctypes import wintypes

try:
    from PyQt5 import QtCore, QtGui, QtWidgets
    from PyQt5.QtCore import Qt
except ImportError:
    raise
    # from PyQt4 import QtCore, QtGui
    # from PyQt4.QtCore import Qt
    # QtWidgets = QtMultimedia = QtGui
    # QtGui.QFileDialog.getOpenFileName = QtGui.QFileDialog.getOpenFileNameAndFilter
    # QtCore.QStandardPaths = QtGui.QDesktopServices
    # QtCore.QStandardPaths.writableLocation = QtGui.QDesktopServices.storageLocation

from scan_scene_changes_window import Ui_MainWindow

import ffms
import sublib
import common


Signal = QtCore.pyqtSignal
Slot = QtCore.pyqtSlot

DEBUG = True
common.VERBOSE = True

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

ScanResult = namedtuple("ScanResult", ("item", "category", "timecode"))


def print_error():
    _exc_type, exc_obj, exc_tb = sys.exc_info()
    fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
    msg = "{},{}: {!r}".format(fname, exc_tb.tb_lineno, exc_obj)
    print(msg, file=sys.stderr)


class EmittingStream(QtCore.QObject):
    text_written = Signal(str)

    def write(self, text):
        self.text_written.emit(str(text))


class ScanSceneChangeApplication(QtWidgets.QApplication):
    def __init__(self, argv):
        super().__init__(argv)
        self._memory = QtCore.QSharedMemory(self)
        self._memory.setKey(self.name)
        if self._memory.attach():
            self.running = True
        else:
            if self._memory.create(1):
                self.running = False
            elif self._memory.attach():
                self.running = True
            else:
                raise RuntimeError(self._memory.errorString())

    @property
    def name(self):
        return self.__class__.__name__


class NativeEventFilter(QtCore.QAbstractNativeEventFilter):
    def __init__(self, form):
        self.form = form
        self.form_hWnd = form.winId().__int__()
        return super().__init__()

    def nativeEventFilter(self, event_type, message):
        filter = False
        if event_type == b"windows_generic_MSG":
            msg = wintypes.MSG.from_address(message.__int__())
            if self.form_hWnd == msg.hWnd and msg.message >= WM_APP_EXIT:
                if msg.message == WM_APP_EXIT:
                    self.form.closeEvent()
                    filter = True
                elif msg.message == WM_APP_SAVE_DONE:
                    self.form.saved_event()
                    filter = True
                elif msg.message == WM_APP_VIDEO_CHANGED:
                    self.form.closeEvent()
                    filter = True
                elif msg.message == WM_APP_SUB_CHANGED:
                    self.form.closeEvent()
                    filter = True
        return filter, id(message)


class ScanSceneChangeForm(QtWidgets.QMainWindow):
    index_created = Signal()
    bogus_scene_change_found = Signal(int, float)
    missing_scene_change_found = Signal(int, float, float)
    bogus_scan_done = Signal()
    missing_scan_done = Signal()
    scan_done = Signal()
    job = None
    cancel_bogus_event = None
    cancel_missing_event = None
    state = None
    executor = futures.ThreadPoolExecutor(1)

    def __init__(self, args, parent=None):
        super().__init__(parent)
        # self.ui = uic.loadUi("scan_scene_changes_window.ui")
        # self.ui.show()
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)

        if args.debug or DEBUG:
            self.debug = True
            self.ui.tab_Log = QtWidgets.QWidget()
            self.ui.tab_Log.setObjectName("tab_Log")
            self.ui.gridLayoutDebug = QtWidgets.QGridLayout(self.ui.tab_Log)
            self.ui.gridLayoutDebug.setObjectName("gridLayoutDebug")
            self.ui.textEdit_Log = QtWidgets.QTextEdit(self.ui.tab_Log)
            self.ui.textEdit_Log.setObjectName("textEdit_Log")
            self.ui.gridLayoutDebug.addWidget(self.ui.textEdit_Log, 0, 0, 1, 1)
            self.ui.tabWidget.addTab(self.ui.tab_Log, self.tr("Log"))
            self.ui.textEdit_Log.setReadOnly(True)
            sys.stdout = sys.stderr = EmittingStream(
                text_written=self.on_log_written)
        else:
            self.debug = False

        print("build date:", common.BUILD_DATE)
        print("PyQt v{}, Qt v{}".format(QtCore.PYQT_VERSION_STR,
                                        QtCore.QT_VERSION_STR))
        print("FFMS v{}".format(ffms.get_version()))
        #print(ffms.libffms2.lib._name)

        if args.vss:
            self.vss = args.vss
            # self.setWindowFlags(Qt.Tool | Qt.WindowStaysOnTopHint)
            self.setWindowFlags(Qt.WindowStaysOnTopHint)
            win32gui.PostMessage(self.vss, WM_APP_COMPANION_OPENED,
                                 self.winId(), 0)
        else:
            self.vss = None

        self.show()

        if args.video_file:
            self.video_file = win32api.GetLongPathName(args.video_file)
        else:
            self.model = False
            self.video_file = self.get_open_filename(self.tr("Video file"),
                                                     self.video_dialog_filter)

        print("video: {}".format(os.path.basename(self.video_file)))

        index_path = common.get_index_path(self.video_file)
        if os.path.isfile(index_path):
            self.enable_scan()
        else:
            app.setOverrideCursor(QtGui.QCursor(Qt.BusyCursor))
            self.ui.statusbar.showMessage(self.tr("Indexing..."))
            self.index_created.connect(self.on_index_created)
            self.executor.submit(self.create_index)

        if args.sub_file:
            self.sub_file = args.sub_file
        else:
            self.model = False
            self.sub_file = self.get_open_filename(self.tr("Subtitle file"),
                                                   self.sub_dialog_filter)

        self.bogus_category = self.tr("Bogus")
        self.missing_category = self.tr("Missing")
        self.scan_results = {}
        self.undoables = {}

        self.ui.spinBoxDifference.valueChanged.connect(
            self.on_spinbox_difference_changed)
        self.ui.doubleSpinBoxRatio.valueChanged.connect(
            self.on_spinbox_ratio_changed)
        self.ui.horizontalSliderRatio.valueChanged.connect(
            self.on_slider_ratio_changed)
        self.ui.toolButtonDefaults.clicked.connect(self.on_defaults_clicked)
        self.ui.toolButtonScan.clicked.connect(self.on_scan_clicked)
        self.ui.toolButtonCancel.clicked.connect(self.on_cancel_clicked)
        self.ui.checkBoxBogus.stateChanged.connect(self.on_check_bogus_changed)
        self.ui.checkBoxMissing.stateChanged.connect(
            self.on_check_missing_changed)
        self.bogus_scene_change_found.connect(self.on_bogus_scene_change_found)
        self.missing_scene_change_found.connect(
            self.on_missing_scene_change_found)
        self.bogus_scan_done.connect(self.on_bogus_scan_done)
        self.missing_scan_done.connect(self.on_missing_scan_done)
        self.scan_done.connect(self.on_scan_done)
        self.reset_parameters()

        self.ui.treeWidget.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.ui.treeWidget.customContextMenuRequested.connect(self.on_context)
        self.ui.treeWidget.keyPressEvent = self.tree_key_press_event

        self.right_click_widget = QtWidgets.QWidget()
        self.ui.actionApply_suggestion.setParent(self.right_click_widget)
        self.ui.actionApply_suggestions_category.setParent(
            self.right_click_widget)
        self.ui.actionApply_all_suggestions.setParent(self.right_click_widget)
        self.ui.actionUndo_suggestion.setParent(self.right_click_widget)
        self.ui.actionUndo_suggestions_category.setParent(
            self.right_click_widget)
        self.ui.actionUndo_all_suggestions.setParent(self.right_click_widget)

        self.ui.treeWidget.itemSelectionChanged.connect(
            self.on_selection_changed)
        self.ui.treeWidget.itemDoubleClicked.connect(self.on_double_click)
        self.ui.actionApply_suggestion.triggered.connect(
            self.on_apply_suggestion)
        self.ui.actionApply_suggestions_category.triggered.connect(
            self.on_apply_suggestions_category)
        self.ui.actionApply_all_suggestions.triggered.connect(
            self.on_apply_all_suggestions)
        self.ui.actionUndo_suggestion.triggered.connect(
            self.on_undo_suggestion)
        self.ui.actionUndo_suggestions_category.triggered.connect(
            self.on_undo_suggestions_category)
        self.ui.actionUndo_all_suggestions.triggered.connect(
            self.on_undo_all_suggestions)

        menu = QtWidgets.QMenu("Right-click menu", self)
        menu.addAction(self.ui.actionApply_suggestion)
        menu.addAction(self.ui.actionApply_suggestions_category)
        menu.addAction(self.ui.actionApply_all_suggestions)
        menu.addSeparator()
        menu.addAction(self.ui.actionUndo_suggestion)
        menu.addAction(self.ui.actionUndo_suggestions_category)
        menu.addAction(self.ui.actionUndo_all_suggestions)
        self.right_click_menu = menu

    def on_log_written(self, text):
        cursor = self.ui.textEdit_Log.textCursor()
        cursor.movePosition(QtGui.QTextCursor.End)
        cursor.insertText(text)
        self.ui.textEdit_Log.setTextCursor(cursor)
        self.ui.textEdit_Log.ensureCursorVisible()

    def closeEvent(self, event=None):
        if self.job is not None:
            self.cancel_bogus_event.set()
            self.cancel_missing_event.set()
            self.job.result()
        if self.vss:
            win32gui.PostMessage(self.vss, WM_APP_COMPANION_CLOSED,
                                 self.winId(), 0)
        app.exit(0)

    def create_index(self):
        try:
            common.get_video_source(self.video_file)
        except Exception:
            print_error()
            if self.debug:
                self.ui.tabWidget.setCurrentIndex(2)
        finally:
            self.index_created.emit()

    @property
    def video_dialog_filter(self):
        return "{} ({})".format(
            self.tr("Video files"),
            " ".join("*" + ext for ext in common.VIDEO_EXTS)
        )

    @property
    def sub_dialog_filter(self):
        return "{} ({})".format(self.tr("Subtitle files"),
                                " ".join("*" + ext for ext in common.SUB_EXTS))

    def get_open_filename(self, title=None, dialog_filter=None):
        star_filter = "{} (*.*)".format(self.tr("All files"))
        if dialog_filter:
            dialog_filter += ";;" + star_filter
        else:
            dialog_filter = star_filter
        filename = QtWidgets.QFileDialog.getOpenFileName(self, title, ".", dialog_filter)[0]
        if os.path.isfile(filename):
            return filename
        raise ValueError("invalid file: {!r}".format(filename))

    def on_index_created(self):
        app.restoreOverrideCursor()
        self.enable_scan()

    def enable_scan(self):
        self.ui.statusbar.showMessage(self.tr("Ready"))
        self.ui.toolButtonScan.setEnabled(True)
        self.state = "ready"

    def on_spinbox_difference_changed(self, value):
        self.check_defaults()

    def on_spinbox_ratio_changed(self, value):
        self.ui.horizontalSliderRatio.setValue(int(value * 10))
        self.check_defaults()

    def on_slider_ratio_changed(self, value):
        self.ui.doubleSpinBoxRatio.setValue(value / 10.0)

    def check_defaults(self):
        non_defaults = (
            self.diff_pct != sublib.SceneChangeFile.DIFF_PCT_THRESHOLD or
            self.ratio != sublib.SceneChangeFile.RATIO_THRESHOLD
        )
        self.ui.toolButtonDefaults.setEnabled(non_defaults)

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

    def on_defaults_clicked(self):
        self.reset_parameters()

    def reset_parameters(self):
        self.diff_pct = sublib.SceneChangeFile.DIFF_PCT_THRESHOLD
        self.ratio = sublib.SceneChangeFile.RATIO_THRESHOLD

    def set_enabled_parameters(self, enabled=True):
        if enabled:
            self.ui.checkBoxBogus.setEnabled(enabled)
            self.ui.checkBoxMissing.setEnabled(enabled)
            self.check_defaults()
        else:
            self.ui.toolButtonDefaults.setEnabled(enabled)
        self.ui.spinBoxDifference.setEnabled(enabled)
        self.ui.horizontalSliderDifference.setEnabled(enabled)
        self.ui.doubleSpinBoxRatio.setEnabled(enabled)
        self.ui.horizontalSliderRatio.setEnabled(enabled)

    @property
    def scan_bogus(self):
        return self.ui.checkBoxBogus.isChecked()

    @property
    def scan_missing(self):
        return self.ui.checkBoxMissing.isChecked()

    def on_scan_clicked(self):
        self.state = "busy"
        app.setOverrideCursor(QtGui.QCursor(Qt.BusyCursor))
        self.ui.toolButtonScan.setEnabled(False)
        self.ui.toolButtonCancel.setEnabled(True)
        self.set_enabled_parameters(False)
        self.ui.statusbar.showMessage(self.tr("Scanning..."))
        self.job = None
        if self.vss:
            win32gui.PostMessage(self.vss, WM_APP_SAVE, self.winId(), 0)
        else:
            self.saved_event()

    def on_cancel_clicked(self):
        self.ui.toolButtonCancel.setEnabled(False)

        if self.job is None:
            self.finalize_cancel()
        else:
            self.cancel_bogus_event.set()
            self.cancel_missing_event.set()

    def finalize_cancel(self):
        scannable = (self.ui.checkBoxBogus.isChecked() or
                     self.ui.checkBoxMissing.isChecked())
        self.ui.toolButtonScan.setEnabled(scannable)
        self.set_enabled_parameters(True)
        self.ui.statusbar.showMessage(self.tr("Canceled"))
        app.restoreOverrideCursor()
        self.state = "ready"

    def saved_event(self):
        self.reset_tree()
        self.cancel_bogus_event = threading.Event()
        self.cancel_missing_event = threading.Event()
        self.job = self.executor.submit(
            self.scan_thread,
            self.cancel_bogus_event, self.cancel_missing_event
        )

    def on_check_bogus_changed(self, state):
        if self.state == "ready":
            scannable = (self.ui.checkBoxBogus.isChecked() or
                         self.ui.checkBoxMissing.isChecked())
            self.ui.toolButtonScan.setEnabled(scannable)
        elif self.state == "busy":
            if not state and self.cancel_bogus_event:
                self.cancel_bogus_event.set()
            if self.job:
                if not self.bogus_root:
                    self.bogus_root = self.create_root(self.bogus_category)
                    self.bogus_root.setExpanded(True)

    def on_check_missing_changed(self, state):
        if self.state == "ready":
            scannable = (self.ui.checkBoxBogus.isChecked() or
                         self.ui.checkBoxMissing.isChecked())
            self.ui.toolButtonScan.setEnabled(scannable)
        elif self.state == "busy":
            if not state and self.cancel_missing_event:
                self.cancel_missing_event.set()
            if self.job:
                if not self.missing_root:
                    self.missing_root = self.create_root(self.missing_category)
                    self.missing_root.setExpanded(True)

    def scan_thread(self, cancel_bogus_event, cancel_missing_event):
        try:
            vsource = common.get_video_source(self.video_file)
            sc_file = self.load_sc_file(vsource)
            sub_file = sublib.SubtitleFile.load(self.sub_file)
            timings = [(sub.start_time, sub.end_time) for sub in sub_file]

            if self.scan_bogus:
                for sc_time, diff_pct in sc_file.scan_bogus(
                        vsource, timings, self.diff_pct,
                        cancel_event=cancel_bogus_event):
                    self.bogus_scene_change_found.emit(sc_time, diff_pct)
                    self.count += 1

            self.bogus_scan_done.emit()

            if self.scan_missing:
                for sc_time, diff_pct, ratio in sc_file.scan_missing(
                        vsource, timings, self.diff_pct, self.ratio,
                        cancel_event=cancel_missing_event):
                    self.missing_scene_change_found.emit(sc_time, diff_pct,
                                                         ratio)
                    self.count += 1

            self.job = None
            #self.missing_scan_done.emit()
        except Exception:
            print_error()
            if self.debug:
                self.ui.tabWidget.setCurrentIndex(2)
        finally:
            self.scan_done.emit()

    def on_bogus_scan_done(self):
        self.ui.checkBoxBogus.setEnabled(False)

    def on_missing_scan_done(self):
        pass

    def on_scan_done(self):
        if (not self.count and self.cancel_bogus_event.is_set() and
                self.cancel_missing_event.is_set()):
            self.finalize_cancel()
        else:
            self.ui.toolButtonCancel.setEnabled(False)
            scannable = (self.ui.checkBoxBogus.isChecked() or
                         self.ui.checkBoxMissing.isChecked())
            self.ui.toolButtonScan.setEnabled(scannable)
            self.set_enabled_parameters(True)
            self.ui.statusbar.showMessage(
                self.tr("%n result(s)", "", self.count))
            app.restoreOverrideCursor()
            self.state = "ready"

    def on_bogus_scene_change_found(self, sc_time, diff_pct):
        self.add_item(self.bogus_root, self.bogus_category, sc_time, diff_pct)

    def on_missing_scene_change_found(self, sc_time, diff_pct, ratio):
        self.add_item(self.missing_root, self.missing_category,
                      sc_time, diff_pct, ratio)

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
        self.scan_results.clear()
        self.undoables.clear()
        self.ui.treeWidget.clear()
        if self.scan_bogus:
            self.bogus_root = self.create_root(self.bogus_category)
            self.bogus_root.setExpanded(True)
        else:
            self.bogus_root = None
        if self.scan_missing:
            self.missing_root = self.create_root(self.missing_category)
            self.missing_root.setExpanded(True)
        else:
            self.missing_root = None

    def create_root(self, name):
        item = QtWidgets.QTreeWidgetItem(self.ui.treeWidget)
        item.setText(0, name)
        return item

    def add_item(self, parent, category, sc_time, diff=None, ratio=None):
        item = QtWidgets.QTreeWidgetItem()
        item.setText(0, self.time_output(sc_time))
        if diff is not None:
            item.setText(1, "{:.1%}".format(diff))
            item.setTextAlignment(1, Qt.AlignRight)
        if ratio is not None:
            item.setText(2, "{:.2f}".format(ratio))
            item.setTextAlignment(2, Qt.AlignRight)
        parent.addChild(item)
        self.scan_results[id(item)] = ScanResult(item, category, sc_time)

    def remove_item(self, item):
        self.undoables[id(item)] = self.scan_results[id(item)]
        del self.scan_results[id(item)]
        #item.parent().removeChild(item)
        font = QtGui.QFont()
        font.setStrikeOut(True)
        item.setFont(0, font)

    def undo_remove_item(self, item):
        self.scan_results[id(item)] = self.undoables[id(item)]
        del self.undoables[id(item)]
        font = QtGui.QFont()
        font.setStrikeOut(False)
        item.setFont(0, font)

    def on_context(self, point):
        if self.vss:
            if self.selection:
                key = id(self.selection)
                if key in self.scan_results:
                    self.ui.actionApply_suggestion.setEnabled(True)
                    self.ui.actionUndo_suggestion.setEnabled(False)
                    category = self.scan_results[key].category
                elif key in self.undoables:
                    self.ui.actionApply_suggestion.setEnabled(False)
                    self.ui.actionUndo_suggestion.setEnabled(True)
                    category = self.undoables[key].category
                else:
                    self.ui.actionApply_suggestion.setEnabled(False)
                    self.ui.actionUndo_suggestion.setEnabled(False)
                    category = self.selection.text(0)
                non_empty = any(r.category == category
                                for r in self.scan_results.values())
                self.ui.actionApply_suggestions_category.setEnabled(non_empty)
                non_empty = any(r.category == category
                                for r in self.undoables.values())
                self.ui.actionUndo_suggestions_category.setEnabled(non_empty)
            else:
                self.ui.actionApply_suggestion.setEnabled(False)
                self.ui.actionUndo_suggestion.setEnabled(False)
                self.ui.actionApply_suggestions_category.setEnabled(False)
                self.ui.actionUndo_suggestions_category.setEnabled(False)
            self.ui.actionApply_all_suggestions.setEnabled(
                len(self.scan_results))
            self.ui.actionUndo_all_suggestions.setEnabled(len(self.undoables))
        else:
            self.ui.actionApply_suggestion.setEnabled(False)
            self.ui.actionApply_suggestions_category.setEnabled(False)
            self.ui.actionApply_all_suggestions.setEnabled(False)
            self.ui.actionUndo_suggestion.setEnabled(False)
            self.ui.actionUndo_suggestions_category.setEnabled(False)
            self.ui.actionUndo_all_suggestions.setEnabled(False)

        global_point = self.ui.treeWidget.mapToGlobal(point)
        if not self.ui.treeWidget.isHeaderHidden():
            global_point.setY(global_point.y() +
                              self.ui.treeWidget.header().height())

        self.right_click_menu.exec(global_point)

    @property
    def selection(self):
        if not self.ui.treeWidget.selectedItems():
            return None
        return self.ui.treeWidget.selectedItems()[0]

    def on_selection_changed(self):
        if id(self.selection) in self.scan_results:
            result = self.scan_results[id(self.selection)]
        elif id(self.selection) in self.undoables:
            result = self.undoables[id(self.selection)]
        else:
            return
        win32gui.PostMessage(self.vss, WM_APP_SET_POSITION, result.timecode, 0)

    def on_double_click(self, item, column):
        self.toggle_item(item)

    def tree_key_press_event(self, event=None):
        super(QtWidgets.QTreeWidget, self.ui.treeWidget).keyPressEvent(event)
        if event.key() in [QtCore.Qt.Key_Return, QtCore.Qt.Key_Enter]:
            if self.selection is self.bogus_root:
                self.bogus_root.setExpanded(
                    not self.bogus_root.isExpanded())
            elif self.selection is self.missing_root:
                self.missing_root.setExpanded(
                    not self.missing_root.isExpanded())
            else:
                self.toggle_item(self.selection)

    def toggle_item(self, item):
        item_id = id(item)
        if item_id in self.scan_results:
            results = [self.scan_results[item_id]]
            self.apply_suggestions(results)
        elif item_id in self.undoables:
            results = [self.undoables[item_id]]
            self.undo_suggestions(results)

    def apply_suggestions(self, results):
        for result in results:
            if result.category == self.bogus_category:
                win32gui.PostMessage(self.vss, WM_APP_DELETE_SC,
                                     result.timecode, 0)
                self.remove_item(result.item)
            elif result.category == self.missing_category:
                win32gui.PostMessage(self.vss, WM_APP_INSERT_SC,
                                     result.timecode, 0)
                self.remove_item(result.item)

    def undo_suggestions(self, results):
        for result in results:
            if result.category == self.bogus_category:
                win32gui.PostMessage(self.vss, WM_APP_INSERT_SC,
                                     result.timecode, 0)
                self.undo_remove_item(result.item)
            elif result.category == self.missing_category:
                win32gui.PostMessage(self.vss, WM_APP_DELETE_SC,
                                     result.timecode, 0)
                self.undo_remove_item(result.item)

    def on_apply_suggestion(self, checked=False):
        if id(self.selection) not in self.scan_results:
            return
        results = [self.scan_results[id(self.selection)]]
        self.apply_suggestions(results)

    def on_apply_suggestions_category(self, checked=False):
        if id(self.selection) in self.scan_results:
            category = self.scan_results[id(self.selection)].category
        elif id(self.selection) in self.undoables:
            category = self.undoables[id(self.selection)].category
        else:
            category = self.selection.text(0)
        results = [r for r in self.scan_results.values()
                   if r.category == category]
        self.apply_suggestions(results)

    def on_apply_all_suggestions(self, checked=False):
        results = list(self.scan_results.values())
        self.apply_suggestions(results)

    def on_undo_suggestion(self, checked=False):
        if id(self.selection) not in self.undoables:
            return
        results = [self.undoables[id(self.selection)]]
        self.undo_suggestions(results)

    def on_undo_suggestions_category(self, checked=False):
        if id(self.selection) in self.scan_results:
            category = self.scan_results[id(self.selection)].category
        elif id(self.selection) in self.undoables:
            category = self.undoables[id(self.selection)].category
        else:
            category = self.selection.text(0)
        results = [r for r in self.undoables.values()
                   if r.category == category]
        self.undo_suggestions(results)

    def on_undo_all_suggestions(self, checked=False):
        results = list(self.undoables.values())
        self.undo_suggestions(results)


def parse_args():
    parser = argparse.ArgumentParser("Scan for scene changes")
    parser.add_argument("--video-file",
                        help="video filename")
    parser.add_argument("--sub-file",
                        help="subtitle filename")
    parser.add_argument("--vss", type=int,
                        help="VisualSubSync hWnd")
    parser.add_argument("--debug", action="store_true",
                        help="Debug mode")
    return parser.parse_args()


if __name__ == "__main__":
    app = ScanSceneChangeApplication(sys.argv)
    if app.running:
        print("{} is already running.".format(app.name), file=sys.stderr)
        sys.exit(1)
    form = ScanSceneChangeForm(parse_args())
    native_event_filter = NativeEventFilter(form)
    app.installNativeEventFilter(native_event_filter)

    sys.exit(app.exec())
