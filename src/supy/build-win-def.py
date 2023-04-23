# this file is used to generate the def file for the python library
# which is needed to build the supy-driver on Windows
# in virtual environments required by the cibuildwheel package
# see: https://cibuildwheel.readthedocs.io/en/stable/faq/#building-wheels-for-python-extensions
from numpy.distutils.mingw32ccompiler import Mingw32CCompiler as mgwin32C

from numpy.distutils.mingw32ccompiler import find_python_dll, generate_def
import sys, os
from pathlib import Path
from time import sleep
import platform

sysname = platform.system()

if sysname != "Windows":
    print("This script is for Windows only")
else:
    print("Fixing def file for Windows")
    print("Python version:", sys.version)

    dll_file = find_python_dll()
    p_dll = Path(dll_file)
    print("Here is the pythonlib.dll:", dll_file)

    # generate symbol list from this library
    def_name = "python%d%d.def" % tuple(sys.version_info[:2])
    def_file = os.path.join(sys.prefix, "libs", def_name)

    # fix path for def file
    Path(def_file).parent.mkdir(parents=True, exist_ok=True)
    print("OK to create? ", Path(def_file).parent.exists())

    # t_sleep_sec=1
    # print(f'sleeping for {t_sleep_sec} seconds')
    # sleep(t_sleep_sec)

    generate_def(dll_file, def_file)
