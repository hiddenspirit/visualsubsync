#!/usr/bin/env python3

import datetime
import re

py_file = "common.py"

build_date = datetime.datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S")

with open(py_file) as f:
    content = f.read()

content = re.sub(r'(BUILD_DATE = )(.*)', '\\1"{}"'.format(build_date), content)

with open(py_file, "w", newline="\n") as f:
    f.write(content)
