# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'scan_scene_changes_window.ui'
#
# Created: Thu Jan 31 06:13:12 2013
#      by: PyQt4 UI code generator 4.9.6
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName(_fromUtf8("MainWindow"))
        MainWindow.resize(391, 434)
        MainWindow.setDocumentMode(False)
        MainWindow.setTabShape(QtGui.QTabWidget.Rounded)
        self.centralwidget = QtGui.QWidget(MainWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.centralwidget)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.tabWidget = QtGui.QTabWidget(self.centralwidget)
        self.tabWidget.setTabShape(QtGui.QTabWidget.Rounded)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setObjectName(_fromUtf8("tabWidget"))
        self.tabScan = QtGui.QWidget()
        self.tabScan.setObjectName(_fromUtf8("tabScan"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.tabScan)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.treeWidget = QtGui.QTreeWidget(self.tabScan)
        self.treeWidget.setObjectName(_fromUtf8("treeWidget"))
        self.treeWidget.headerItem().setTextAlignment(1, QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter)
        self.treeWidget.headerItem().setTextAlignment(2, QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter)
        self.treeWidget.header().setDefaultSectionSize(110)
        self.treeWidget.header().setStretchLastSection(False)
        self.verticalLayout_2.addWidget(self.treeWidget)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.checkBoxBogus = QtGui.QCheckBox(self.tabScan)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.checkBoxBogus.sizePolicy().hasHeightForWidth())
        self.checkBoxBogus.setSizePolicy(sizePolicy)
        self.checkBoxBogus.setChecked(True)
        self.checkBoxBogus.setObjectName(_fromUtf8("checkBoxBogus"))
        self.horizontalLayout_2.addWidget(self.checkBoxBogus)
        self.checkBoxMissing = QtGui.QCheckBox(self.tabScan)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.checkBoxMissing.sizePolicy().hasHeightForWidth())
        self.checkBoxMissing.setSizePolicy(sizePolicy)
        self.checkBoxMissing.setChecked(True)
        self.checkBoxMissing.setObjectName(_fromUtf8("checkBoxMissing"))
        self.horizontalLayout_2.addWidget(self.checkBoxMissing)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem)
        self.toolButtonCancel = QtGui.QToolButton(self.tabScan)
        self.toolButtonCancel.setEnabled(False)
        self.toolButtonCancel.setMinimumSize(QtCore.QSize(64, 24))
        self.toolButtonCancel.setObjectName(_fromUtf8("toolButtonCancel"))
        self.horizontalLayout_2.addWidget(self.toolButtonCancel)
        self.toolButtonScan = QtGui.QToolButton(self.tabScan)
        self.toolButtonScan.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.toolButtonScan.sizePolicy().hasHeightForWidth())
        self.toolButtonScan.setSizePolicy(sizePolicy)
        self.toolButtonScan.setMinimumSize(QtCore.QSize(64, 24))
        self.toolButtonScan.setObjectName(_fromUtf8("toolButtonScan"))
        self.horizontalLayout_2.addWidget(self.toolButtonScan)
        self.verticalLayout_2.addLayout(self.horizontalLayout_2)
        self.tabWidget.addTab(self.tabScan, _fromUtf8(""))
        self.tabParameters = QtGui.QWidget()
        self.tabParameters.setObjectName(_fromUtf8("tabParameters"))
        self.verticalLayout = QtGui.QVBoxLayout(self.tabParameters)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        spacerItem1 = QtGui.QSpacerItem(20, 8, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.verticalLayout.addItem(spacerItem1)
        self.gridLayoutParameters = QtGui.QGridLayout()
        self.gridLayoutParameters.setObjectName(_fromUtf8("gridLayoutParameters"))
        self.labelRatio = QtGui.QLabel(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.labelRatio.sizePolicy().hasHeightForWidth())
        self.labelRatio.setSizePolicy(sizePolicy)
        self.labelRatio.setObjectName(_fromUtf8("labelRatio"))
        self.gridLayoutParameters.addWidget(self.labelRatio, 2, 0, 1, 1)
        self.labelDifference = QtGui.QLabel(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.labelDifference.sizePolicy().hasHeightForWidth())
        self.labelDifference.setSizePolicy(sizePolicy)
        self.labelDifference.setToolTip(_fromUtf8(""))
        self.labelDifference.setObjectName(_fromUtf8("labelDifference"))
        self.gridLayoutParameters.addWidget(self.labelDifference, 1, 0, 1, 1)
        self.horizontalSliderDifference = QtGui.QSlider(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.MinimumExpanding, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.horizontalSliderDifference.sizePolicy().hasHeightForWidth())
        self.horizontalSliderDifference.setSizePolicy(sizePolicy)
        self.horizontalSliderDifference.setMaximum(100)
        self.horizontalSliderDifference.setProperty("value", 15)
        self.horizontalSliderDifference.setOrientation(QtCore.Qt.Horizontal)
        self.horizontalSliderDifference.setObjectName(_fromUtf8("horizontalSliderDifference"))
        self.gridLayoutParameters.addWidget(self.horizontalSliderDifference, 1, 1, 1, 1)
        self.horizontalSliderRatio = QtGui.QSlider(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.MinimumExpanding, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.horizontalSliderRatio.sizePolicy().hasHeightForWidth())
        self.horizontalSliderRatio.setSizePolicy(sizePolicy)
        self.horizontalSliderRatio.setMinimum(1)
        self.horizontalSliderRatio.setMaximum(200)
        self.horizontalSliderRatio.setProperty("value", 30)
        self.horizontalSliderRatio.setSliderPosition(30)
        self.horizontalSliderRatio.setOrientation(QtCore.Qt.Horizontal)
        self.horizontalSliderRatio.setObjectName(_fromUtf8("horizontalSliderRatio"))
        self.gridLayoutParameters.addWidget(self.horizontalSliderRatio, 2, 1, 1, 1)
        self.doubleSpinBoxRatio = QtGui.QDoubleSpinBox(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.doubleSpinBoxRatio.sizePolicy().hasHeightForWidth())
        self.doubleSpinBoxRatio.setSizePolicy(sizePolicy)
        self.doubleSpinBoxRatio.setMinimumSize(QtCore.QSize(42, 0))
        self.doubleSpinBoxRatio.setMaximumSize(QtCore.QSize(42, 16777215))
        self.doubleSpinBoxRatio.setDecimals(1)
        self.doubleSpinBoxRatio.setMinimum(1.0)
        self.doubleSpinBoxRatio.setMaximum(20.0)
        self.doubleSpinBoxRatio.setSingleStep(0.1)
        self.doubleSpinBoxRatio.setProperty("value", 1.0)
        self.doubleSpinBoxRatio.setObjectName(_fromUtf8("doubleSpinBoxRatio"))
        self.gridLayoutParameters.addWidget(self.doubleSpinBoxRatio, 2, 2, 1, 1)
        self.spinBoxDifference = QtGui.QSpinBox(self.tabParameters)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.spinBoxDifference.sizePolicy().hasHeightForWidth())
        self.spinBoxDifference.setSizePolicy(sizePolicy)
        self.spinBoxDifference.setMinimumSize(QtCore.QSize(42, 0))
        self.spinBoxDifference.setMaximumSize(QtCore.QSize(42, 16777215))
        self.spinBoxDifference.setMaximum(100)
        self.spinBoxDifference.setProperty("value", 15)
        self.spinBoxDifference.setObjectName(_fromUtf8("spinBoxDifference"))
        self.gridLayoutParameters.addWidget(self.spinBoxDifference, 1, 2, 1, 1)
        self.textEdit = QtGui.QTextEdit(self.tabParameters)
        self.textEdit.setEnabled(True)
        self.textEdit.setReadOnly(True)
        self.textEdit.setObjectName(_fromUtf8("textEdit"))
        self.gridLayoutParameters.addWidget(self.textEdit, 3, 0, 1, 3)
        self.verticalLayout.addLayout(self.gridLayoutParameters)
        self.horizontalLayoutParametersButtons = QtGui.QHBoxLayout()
        self.horizontalLayoutParametersButtons.setObjectName(_fromUtf8("horizontalLayoutParametersButtons"))
        spacerItem2 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayoutParametersButtons.addItem(spacerItem2)
        self.toolButtonDefaults = QtGui.QToolButton(self.tabParameters)
        self.toolButtonDefaults.setMinimumSize(QtCore.QSize(64, 24))
        self.toolButtonDefaults.setObjectName(_fromUtf8("toolButtonDefaults"))
        self.horizontalLayoutParametersButtons.addWidget(self.toolButtonDefaults)
        self.verticalLayout.addLayout(self.horizontalLayoutParametersButtons)
        self.tabWidget.addTab(self.tabParameters, _fromUtf8(""))
        self.horizontalLayout.addWidget(self.tabWidget)
        MainWindow.setCentralWidget(self.centralwidget)
        self.statusbar = QtGui.QStatusBar(MainWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        MainWindow.setStatusBar(self.statusbar)
        self.actionApply_suggestion = QtGui.QAction(MainWindow)
        self.actionApply_suggestion.setObjectName(_fromUtf8("actionApply_suggestion"))
        self.actionApply_suggestions_category = QtGui.QAction(MainWindow)
        self.actionApply_suggestions_category.setObjectName(_fromUtf8("actionApply_suggestions_category"))
        self.actionApply_all_suggestions = QtGui.QAction(MainWindow)
        self.actionApply_all_suggestions.setObjectName(_fromUtf8("actionApply_all_suggestions"))
        self.actionUndo_suggestion = QtGui.QAction(MainWindow)
        self.actionUndo_suggestion.setObjectName(_fromUtf8("actionUndo_suggestion"))
        self.actionUndo_suggestions_category = QtGui.QAction(MainWindow)
        self.actionUndo_suggestions_category.setObjectName(_fromUtf8("actionUndo_suggestions_category"))
        self.actionUndo_all_suggestions = QtGui.QAction(MainWindow)
        self.actionUndo_all_suggestions.setObjectName(_fromUtf8("actionUndo_all_suggestions"))

        self.retranslateUi(MainWindow)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QObject.connect(self.horizontalSliderDifference, QtCore.SIGNAL(_fromUtf8("valueChanged(int)")), self.spinBoxDifference.setValue)
        QtCore.QObject.connect(self.spinBoxDifference, QtCore.SIGNAL(_fromUtf8("valueChanged(int)")), self.horizontalSliderDifference.setValue)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)
        MainWindow.setTabOrder(self.tabWidget, self.treeWidget)
        MainWindow.setTabOrder(self.treeWidget, self.checkBoxBogus)
        MainWindow.setTabOrder(self.checkBoxBogus, self.checkBoxMissing)
        MainWindow.setTabOrder(self.checkBoxMissing, self.toolButtonCancel)
        MainWindow.setTabOrder(self.toolButtonCancel, self.toolButtonScan)
        MainWindow.setTabOrder(self.toolButtonScan, self.horizontalSliderDifference)
        MainWindow.setTabOrder(self.horizontalSliderDifference, self.spinBoxDifference)
        MainWindow.setTabOrder(self.spinBoxDifference, self.horizontalSliderRatio)
        MainWindow.setTabOrder(self.horizontalSliderRatio, self.doubleSpinBoxRatio)
        MainWindow.setTabOrder(self.doubleSpinBoxRatio, self.textEdit)
        MainWindow.setTabOrder(self.textEdit, self.toolButtonDefaults)

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(_translate("MainWindow", "Scene Change Scanner", None))
        self.treeWidget.headerItem().setText(0, _translate("MainWindow", "Timecode", None))
        self.treeWidget.headerItem().setText(1, _translate("MainWindow", "Difference", None))
        self.treeWidget.headerItem().setText(2, _translate("MainWindow", "Ratio", None))
        self.checkBoxBogus.setText(_translate("MainWindow", "Bogus", None))
        self.checkBoxMissing.setText(_translate("MainWindow", "Missing", None))
        self.toolButtonCancel.setText(_translate("MainWindow", "Cancel", None))
        self.toolButtonScan.setText(_translate("MainWindow", "Scan", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tabScan), _translate("MainWindow", "Scan", None))
        self.labelRatio.setText(_translate("MainWindow", "Ratio", None))
        self.labelDifference.setText(_translate("MainWindow", "Difference", None))
        self.textEdit.setHtml(_translate("MainWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'MS Shell Dlg 2\'; font-size:8.25pt; font-weight:400; font-style:normal;\" bgcolor=\"#eeeeee\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:9pt; font-weight:600;\">Difference (0%–100%):<br /></span><span style=\" font-size:9pt;\">Scene changes that have a difference percentage (compared to previous frames) below this threshold will be reported as bogus.</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:9pt; font-weight:600;\">Ratio (×1.0–×20.0):<br /></span><span style=\" font-size:9pt;\">Each subtitle starting and ending zone will be scanned for a missing scene change. A missing scene change will be reported if </span><span style=\" font-size:9pt; font-style:italic;\">highest </span><span style=\" font-size:8pt;\"> ÷ </span><span style=\" font-size:9pt; font-style:italic;\">median</span><span style=\" font-size:9pt;\"> is greater or equal to this threshold, where </span><span style=\" font-size:9pt; font-style:italic;\">highest</span><span style=\" font-size:9pt;\"> is the frame that has the highest difference in the zone, and </span><span style=\" font-size:9pt; font-style:italic;\">median</span><span style=\" font-size:9pt;\"> is the median of the differences in the zone.</span></p></body></html>", None))
        self.toolButtonDefaults.setText(_translate("MainWindow", "Defaults", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tabParameters), _translate("MainWindow", "Parameters", None))
        self.actionApply_suggestion.setText(_translate("MainWindow", "Apply suggestion", None))
        self.actionApply_suggestions_category.setText(_translate("MainWindow", "Apply suggestions in this category", None))
        self.actionApply_all_suggestions.setText(_translate("MainWindow", "Apply all suggestions", None))
        self.actionUndo_suggestion.setText(_translate("MainWindow", "Undo suggestion", None))
        self.actionUndo_suggestions_category.setText(_translate("MainWindow", "Undo suggestions in this category", None))
        self.actionUndo_all_suggestions.setText(_translate("MainWindow", "Undo all suggestions", None))

