rmdir /s /q build

update_build_date.py
setup.py build

set BUILD_DIR=build\exe.win32-3.4
copy C:\Python34\Lib\site-packages\PyQt5\libEGL.dll %BUILD_DIR%
rd /s /q %BUILD_DIR%\imageformats
del %BUILD_DIR%\platforms\qminimal.dll
del %BUILD_DIR%\platforms\qoffscreen.dll
del %BUILD_DIR%\_decimal.pyd
del %BUILD_DIR%\_hashlib.pyd
