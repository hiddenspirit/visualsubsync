#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
#import os
from cx_Freeze import setup, Executable

#############################################################################
# pr�paration des options

# chemins de recherche des modules
path = sys.path + []

# options d'inclusion/exclusion des modules
includes = []
excludes = ["tkinter"]
packages = []

# copier les fichiers et/ou r�pertoires et leur contenu
includefiles = ["ffms2.dll"]

# inclusion �ventuelle de biblioth�ques suppl�mentaires
binpathincludes = []
if sys.platform.startswith("linux"):
    # pour que les biblioth�ques de /usr/lib soient copi�es aussi
    binpathincludes += ["/usr/lib"]

# construction du dictionnaire des options
options = {
    "path": path,
    "includes": includes,
    "excludes": excludes,
    "packages": packages,
    "include_files": includefiles,
    "bin_path_includes": binpathincludes,
    "optimize": 2,
    "replace_paths": [("*", "")],
}

#############################################################################
# pr�paration des cibles
base = None
# if sys.platform == "win32":
    # base = "Win32GUI"

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
]

#############################################################################
# cr�ation du setup
setup(
    name="vss-companion",
    version="1",
    description="VisualSubSync Companion Tools",
    author="spirit",
    options={"build_exe": options},
    executables=exe_list
)
