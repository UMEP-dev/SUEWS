import os
from signal import raise_signal
import sys
from time import sleep
from setuptools import setup
from pathlib import Path

# write version info using git commit
# import subprocess
# import warnings
# import re

from setuptools import Distribution, find_packages

from numpy.distutils.core import Extension, setup
import platform
import glob
import os
from pathlib import Path
import subprocess
import shutil

#########################################
# customised f2py
#########################################
"""
TS 19 Feb 2019 :
adopted this script below to resolve the Fortran `stop` issue:
Credit to: https://github.com/joezuntz/pycamb/blob/master/nonstopf2py.py
This is an adapted version with added modification for windows,
where several GNU C libs are missing that require windows alternatives.

original header:

   An non-stop f2py. When the fortran program calls stop, it is trapped with a long jmp,
   and the error is converted to a python error.

   A lot of inspiration came from patch_f2py.py in James Kermode's quippy
     (http://www.jrkermode.co.uk/quippy)

   currently supported fortran runtimes are:
    ifcore (intel)
    gfortran (gnu)

   For new runtimes, compile a small f90 that uses stop

   boo.f90:
      program boo
          stop "boo the dog stops here"
      end program boo

   and

     objdump -t a.o

   to find the function to override.

  -- Yu Feng <yfeng1@cmu.edu> @ McWilliam Center, Carnegie Mellon 2012

"""

import platform

from numpy import f2py

print("Customised f2py loaded!")


# trap the fortran STOP methods.
# luckily they are not builtin/inlined.
# we do not need to free 'message'. it appears to be staticly allocated
# by the compiler.

f2py.rules.module_rules["modulebody"] = f2py.rules.module_rules["modulebody"].replace(
    "#includes0#\n",
    r"""#includes0#
#include <setjmp.h>
static char * _error;
static jmp_buf _env;
void for_stop_core(char * message, int len) {
  _error = strndup(message, len);
  longjmp(_env, 1);
}
void _gfortran_stop_string(char * message, int len) {
  _error = strndup(message, len);
  longjmp(_env, 1);
}
     """,
)

# here we fight the leak as f2py will no longer always return.
# the easiest way is to first construct the return tuple,
# then free them all
f2py.rules.routine_rules["body"] = f2py.rules.routine_rules["body"].replace(
    """\t\tif (f2py_success) {
#pyobjfrom#
/*end of pyobjfrom*/
\t\tCFUNCSMESS(\"Building return value.\\n\");
\t\tcapi_buildvalue = Py_BuildValue(\"#returnformat#\"#return#);
/*closepyobjfrom*/
#closepyobjfrom#
\t\t} /*if (f2py_success) after callfortranroutine*/""",
    """\t\t{
#pyobjfrom#
/*end of pyobjfrom*/
\t\tCFUNCSMESS(\"Building return value.\\n\");
\t\tcapi_buildvalue = Py_BuildValue(\"#returnformat#\"#return#);
/*closepyobjfrom*/
#closepyobjfrom#
\t\tif(!f2py_success) {
\t\t\tPy_XDECREF(capi_buildvalue);
\t\t\tcapi_buildvalue = NULL;
\t\t}
\t\t}
/*if (f2py_success) after callfortranroutine*/
""",
)

# the actual function call. free _error as PyErr_SetString will copy it.
f2py.rules.routine_rules["body"] = f2py.rules.routine_rules["body"].replace(
    "#callfortranroutine#\n",
    r"""
       if(setjmp(_env)) {
         PyErr_SetString(PyExc_RuntimeError, _error);
         free(_error);
       } else {
         #callfortranroutine#
       }
   """,
)

# distinguish platform to handle lib missing issues on windows
sysname = platform.system()

# change `setjmp` and `longjmp` to windows compliant versions
# http://www.agardner.me/golang/windows/cgo/64-bit/setjmp/longjmp/2016/02/29/go-windows-setjmp-x86.html
if sysname == "Windows":
    f2py.rules.routine_rules["body"] = f2py.rules.routine_rules["body"].replace(
        r"setjmp(_env)", r"__builtin_setjmp(_env)"
    )
    f2py.rules.module_rules["modulebody"] = f2py.rules.module_rules[
        "modulebody"
    ].replace(r"longjmp(_env)", r"__builtin_longjmp(_env)")

    # add an implementation of `strndup`
    # https://github.com/noahp/cflow-mingw/blob/4e3f48c6636f4e54e2f30671eaeb3c8bc96fd4a4/cflow-1.4/gnu/strndup.c
    f2py.rules.module_rules["modulebody"] = f2py.rules.module_rules[
        "modulebody"
    ].replace(
        r"#include <setjmp.h>",
        r"""
#include <setjmp.h>
#include <stdlib.h>
char *
strndup (char const *s, size_t n)
{
  size_t len = strnlen (s, n);
  char *new = malloc (len + 1);

  if (new == NULL)
    return NULL;

  new[len] = '\0';
  return memcpy (new, s, len);
}
""",
    )
#########################################
# end: customised f2py
#########################################


########################################
# monkey patching ms visual c runtime detector
########################################
def get_msvcr_patch():
    """Include the appropriate MSVC runtime library if Python was built
    with MSVC 7.0 or later.
    """
    import sys

    msc_pos = sys.version.find("MSC v.")
    if msc_pos != -1:
        msc_ver = sys.version[msc_pos + 6 : msc_pos + 10]
        if msc_ver == "1300":
            # MSVC 7.0
            return ["msvcr70"]
        elif msc_ver == "1310":
            # MSVC 7.1
            return ["msvcr71"]
        elif msc_ver == "1400":
            # VS2005 / MSVC 8.0
            return ["msvcr80"]
        elif msc_ver == "1500":
            # VS2008 / MSVC 9.0
            return ["msvcr90"]
        elif msc_ver == "1600":
            # VS2010 / MSVC 10.0
            return ["msvcr100"]
        elif int(msc_ver) >= 1900:
            # VS2015 / MSVC 14.0
            return ["msvcr140"]
        else:
            raise ValueError("Unknown MS Compiler version %s " % msc_ver)


import distutils.cygwinccompiler

distutils.cygwinccompiler.get_msvcr = get_msvcr_patch

########################################
# end: monkey patching ms visual c runtime detector
########################################

#########################################
# wrap OS-specific `SUEWS_driver` libs
sysname = platform.system()
lib_basename = "supy_driver"
if sysname == "Windows":
    from numpy.distutils.mingw32ccompiler import find_python_dll, generate_def

    lib_suffix = ".pyd"
    Path("setup.cfg").write_text(
        "[build_ext]\ncompiler=mingw32\n[build]\ncompiler=mingw32\n"
    )
    print("setup.cfg created")
    # create missing def file on windows virtual env
    print("Fixing def file for Windows")

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
    print(list(Path.cwd().glob("*")))
elif sysname == "Darwin":
    lib_suffix = ".so"
elif sysname == "Linux":
    lib_suffix = ".so"
lib_name = lib_basename + lib_suffix
# change compiler settings
if sysname == "Windows":
    pfn = Path.cwd() / "setup.cfg"
    try:
        shutil.copyfile("win-setup.cfg", pfn)
    except:
        pass

# load SUEWS Fortran source files
dir_f95 = "../suews/src"
path_src = Path(dir_f95)
path_mod = (path_src.parent / "mod").resolve()
path_lib = (path_src.parent / "lib").resolve()
path_target_f95 = [
    (path_src / f)
    for f in [
        "suews_ctrl_const.f95",
        "suews_ctrl_error.f95",
        "suews_util_meteo.f95",
        "suews_phys_waterdist.f95",
        "suews_phys_narp.f95",
        "suews_phys_atmmoiststab.f95",
        "suews_phys_resist.f95",
        "suews_phys_evap.f95",
        "suews_phys_snow.f95",
        "suews_phys_dailystate.f95",
        "suews_phys_lumps.f95",
        "suews_phys_anemsn.f95",
        "suews_phys_rslprof.f95",
        "suews_phys_biogenco2.f95",
        "suews_phys_ohm.f95",
        "suews_phys_estm.f95",
        "suews_ctrl_driver.f95",
    ]
]
# all_f95 = glob.glob(os.path.join(dir_f95, "*.f95"))
path_all_f95 = [f for f in path_src.glob("*.f95")]
path_exclude_f95 = [
    (path_src / f)
    for f in [
        "suews_c_wrapper.f95",
        "suews_ctrl_sumin.f95",
        "suews_program.f95",
        "suews_ctrl_init.f95",
        "suews_ctrl_calculations.f95",
        "suews_ctrl_translate.f95",
    ]
]
path_other_f95 = list(set(path_all_f95) - set(path_target_f95) - set(path_exclude_f95))
fn_other_obj = [str(f).replace(".f95", ".o") for f in path_other_f95]
if sysname == "Windows":
    fn_other_obj.append(os.path.join(dir_f95, "strptime.o"))

src_f95 = path_target_f95 + path_other_f95


def readme():
    try:
        with open("../README.md", encoding="utf-8") as f:
            return f.read()
    except:
        return f"SuPy package"


class BinaryDistribution(Distribution):
    def has_ext_modules(self):
        return True

    def is_pure(self):
        return False


########################################
# below is the f2py based extension
# from setuptools import setup, Extension
# ext_modules = [
#     Extension(
#         "supy.supy_driver.suews_driver",
#         [str(p) for p in path_target_f95],
#         extra_compile_args=[
#             "-D_POSIX_C_SOURCE=200809L",
#             "-fbracket-depth=1024"
#             if sysname == "Darwin"
#             else "-Wall",  # for clang on MacOS
#         ],
#         extra_f90_compile_args=["-cpp", f"-I{str(path_mod)}"],
#         f2py_options=[
#             # '--quiet',
#             # "--verbose",
#             # "--debug-capi",  # this is for debugging data types
#             # '--f2cmap="f2py_f2cmap"',
#             # ('-DF2PY_REPORT_ATEXIT' if sysname == 'Linux' else ''),
#         ],
#         extra_objects=fn_other_obj,
#         # "-v" under Linux is necessary because it can avoid the blank variable issue
#         # ref: https://github.com/metomi/fcm/issues/220
#         extra_link_args=["-v" if sysname == "Linux" else "-static"]
#         + [f"-L{str(path_lib)}", "-lspartacus"],
#     )
# ]
########################################


ext_module_f90wrap = [
    Extension(
        "supy_driver",
        sources=[],  # just a placeholder
    ),
]

from setuptools.command.build_ext import build_ext
import subprocess
import os
from distutils.dir_util import mkpath
from pathlib import Path


# class CustomBuildExtCommand(build_ext):
#     def run(self):
#         if platform.system() == "Darwin":
#             # Call the external Makefile here.
#             self.run_external_make()

#             # Now let the original build_ext command do its work.
#             # super().run()

#     def run_external_make(self):
#         # Assuming your Makefile is located at "../external/Makefile"
#         makefile_dir = os.path.abspath(os.path.dirname(__file__))
#         make_file_path = os.path.join(makefile_dir, "Makefile")

#         print("Current working directory:", os.getcwd())
#         sleep(10)

#         if not os.path.exists(make_file_path):
#             raise FileNotFoundError(f"Cannot find Makefile at {make_file_path}")
#         subprocess.run(["pwd"])
#         subprocess.run(["make", "-f", make_file_path, "driver"], check=True)

#         p_dir_ext = Path.cwd() / "supy"
#         print(f"p_dir_ext: {p_dir_ext}")
#         fn_lib = list(p_dir_ext.glob("_supy_driver*.*"))[0]
#         fn_wrapper = p_dir_ext / "supy_driver.py"
#         ext_files = [
#             fn_lib,
#             fn_wrapper,
#         ]
#         print(f"ext_files: {ext_files}")
#         print(f"build_lib: {self.build_lib}")

#         for fn_src in ext_files:
#             fn_dst = Path(self.build_lib) / "supy" / fn_src.name
#             shutil.copy(fn_src, fn_dst)

# if platform.system() == "Darwin":
#     cmdclass = {"build_ext": CustomBuildExtCommand}


setup(
    name="supy",
    # version=__version__,
    description="the SUEWS model that speaks python",
    long_description=readme(),
    long_description_content_type="text/markdown",
    url="https://github.com/UMEP-Dev/SuPy",
    author=", ".join(
        [
            "Dr Ting Sun",
            "Dr Hamidreza Omidvar",
            "Prof Sue Grimmond",
        ]
    ),
    author_email=", ".join(
        [
            "ting.sun@ucl.ac.uk",
            "h.omidvar@reading.ac.uk",
            "c.s.grimmond@reading.ac.uk",
        ]
    ),
    license="GPL-V3.0",
    packages=["supy"],
    include_package_data=True,
    package_data={
        "supy": [
            "sample_run/*",
            "sample_run/Input/*",
            "*.json",
            "util/*",
            "cmd/*",
            f"_supy_driver*{lib_suffix}",
        ]
    },
    distclass=BinaryDistribution,
    ext_modules=ext_module_f90wrap,
    # cmdclass=cmdclass,
    # ext_modules=ext_modules,
    install_requires=[
        "pandas< 1.5; python_version <= '3.9'",  # to fix scipy dependency issue in UMEP under QGIS3 wtih python 3.9
        "pandas; python_version > '3.9'",
        "importlib_resources; python_version < '3.9'", # to fix importlib issue in UMEP under QGIS3
        "matplotlib",
        "chardet",
        "f90wrap",
        "scipy",
        "dask",  # needs dask for parallel tasks
        "f90nml",  # utility for namelist files
        "seaborn",  # stat plotting
        "atmosp",  # my own `atmosp` module forked from `atmos-python`
        "cdsapi",  # ERA5 data
        "xarray",  # utility for high-dimensional datasets
        "multiprocess",  # a better multiprocessing library
        "click",  # cmd tool
        "lmfit",  # optimiser
        "numdifftools",  # required by `lmfit` for uncertainty estimation
        "pvlib",  # TMY-related solar radiation calculations
        "platypus-opt==1.0.4",  # a multi-objective optimiser
        # DRIVER_REQ,  # a separate f2py-based driver
    ],
    extras_require={
        # "hdf": [
        #     "tables",  # for dumping in hdf5
        # ]
    },
    entry_points={
        #   command line tools
        "console_scripts": [
            "suews-run=supy.cmd.SUEWS:SUEWS",
            "suews-convert=supy.cmd.table_converter:convert_table_cmd",
        ]
    },
    python_requires="~=3.8",
    classifiers=[
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Intended Audience :: Education",
        "Intended Audience :: Science/Research",
        "Operating System :: MacOS :: MacOS X",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX :: Linux",
    ],
    zip_safe=False,
)




