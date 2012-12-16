#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys, os
from cx_Freeze import setup, Executable
import datetime, re

#############################################################################
# pr�paration des options

# chemins de recherche des modules
path = sys.path + []

# options d'inclusion/exclusion des modules
includes = ["sip"]
excludes = []
packages = []

# copier les fichiers et/ou r�pertoires et leur contenu
includefiles = ["ffms2.dll"]

# inclusion �ventuelle de biblioth�ques suppl�mentaires
binpathincludes = []
if sys.platform.startswith("linux"):
    # pour que les biblioth�ques de /usr/lib soient copi�es aussi
    binpathincludes += ["/usr/lib"]

# construction du dictionnaire des options
options = {"path": path,
           "includes": includes,
           "excludes": excludes,
           "packages": packages,
           "include_files": includefiles,
           "bin_path_includes": binpathincludes,
           "optimize": 2,
           }

#############################################################################
# pr�paration des cibles
base = None
# if sys.platform == "win32":
    # base = "Win32GUI"

exe = Executable(
    script = "find_scenechange.py",
    base = base,
    compress = True,
    )


#############################################################################
# cr�ation du setup
setup(
    name = "generate_scenechange_file",
    version = "1",
    description = "Generate SceneChange File",
    author = "spirit",
    options = {"build_exe": options},
    executables = [exe]
    )
