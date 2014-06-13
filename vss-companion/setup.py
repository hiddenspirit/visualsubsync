#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
from cx_Freeze import setup, Executable

#############################################################################
# préparation des options

# chemins de recherche des modules
path = sys.path + ["generate_scenechange_file"]

# options d'inclusion/exclusion des modules
includes = []
excludes = [
    "tkinter", "bz2", "lzma", "pyexpat",
    "curses", "distutils", "html", "lib2to3",
    "xml", "xmlrpc", "zipfile", "pdb", "ftplib",
    "pydoc_data", "inspect", "win32pdh",
]
packages = []

# copier les fichiers et/ou répertoires et leur contenu
include_files = ["ffms2.dll"]
include_files_generate_scenechange_file = [
    "avs2yuv.exe", "mvtools2.dll", "AviSynth.dll", "DevIL.dll",
    "generate_scenechange_file_presets.ini",
    "generate_scenechange_file_fr.qm",
]

bin_excludes = [
    "mfc100u.dll",
]

try:
    from PyQt5.QtCore import PYQT_VERSION, QT_VERSION
    includes += [
        "PyQt5.QtCore",
        "PyQt5.QtGui",
        "PyQt5.QtWidgets",
        "PyQt5.QtNetwork",
        "guess_language.data.models.en",
        "guess_language.data.models.fr",
    ]
except ImportError:
    raise
    # from PyQt4.QtCore import PYQT_VERSION, QT_VERSION


for f in include_files_generate_scenechange_file:
    include_files.append((os.path.join("generate_scenechange_file", f), ""))

# construction du dictionnaire des options
options = {
    "path": path,
    "includes": includes,
    "excludes": excludes,
    "packages": packages,
    "include_files": include_files,
    "bin_excludes": bin_excludes,
    "bin_path_includes": [],
    "optimize": 2,
    "replace_paths": [("*", "")],
}

#############################################################################
# préparation des cibles
exe_list = [
    Executable(
        script="find_scenechange.py",
        base=None,
        compress=True,
    ),
    Executable(
        script="check_bitbucket.py",
        base="Win32GUI",
        compress=True,
        icon="../Resources/VSS.ico",
    ),
    # Executable(
        # script="make_scenechange_file.py",
        # base=None,
        # compress=True,
    # ),
    # Executable(
        # script="scan_missing_scene_changes.py",
        # base=None,
        # compress=True,
    # ),
    # Executable(
        # script="scan_bogus_scene_changes.py",
        # base=None,
        # compress=True,
    # ),
    Executable(
        script="scan_scene_changes.pyw",
        base="Win32GUI",
        compress=True,
    ),
    Executable(
        script="generate_scenechange_file/generate_scenechange_file.pyw",
        base="Win32GUI",
        compress=True,
        icon="generate_scenechange_file/sc.ico",
    ),
    Executable(
        script="check_grammar.py",
        base=None,
        compress=True,
    ),
]

#############################################################################

# création du setup
setup(
    name="vss-companion",
    version="1",
    description="VisualSubSync Companion Tools",
    author="spirit",
    options={"build_exe": options},
    executables=exe_list
)
