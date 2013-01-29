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
excludes = ["tkinter"]
packages = []

# copier les fichiers et/ou répertoires et leur contenu
includefiles = ["ffms2.dll"]
includefiles_generate_scenechange_file = [
    "avs2yuv.exe", "mvtools2.dll", "avisynth.dll", "devil.dll",
    "generate_scenechange_file_presets.ini",
    "generate_scenechange_file_fr.qm",
    "generate_scenechange_file_debug.cmd",
]

for f in includefiles_generate_scenechange_file:
    includefiles.append((os.path.join("generate_scenechange_file", f), ""))

# construction du dictionnaire des options
options = {
    "path": path,
    "includes": includes,
    "excludes": excludes,
    "packages": packages,
    "include_files": includefiles,
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
        script="make_scenechange_file.py",
        base=None,
        compress=True,
    ),
    Executable(
        script="scan_missing_scene_changes.py",
        base=None,
        compress=True,
    ),
    Executable(
        script="scan_bogus_scene_changes.py",
        base=None,
        compress=True,
    ),
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
