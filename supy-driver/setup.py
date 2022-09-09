# %%
from setuptools import Distribution
from numpy.distutils.core import Extension, setup
import platform
import glob
import os
from pathlib import Path
import subprocess
import shutil
from nonstopf2py import f2py


# from gen_suewsdrv import merge_source

# wrap OS-specific `SUEWS_driver` libs
sysname = platform.system()
lib_basename = "supy_driver"
if sysname == "Windows":
    lib_name = lib_basename + ".pyd"
elif sysname == "Darwin":
    lib_name = lib_basename + ".so"
elif sysname == "Linux":
    lib_name = lib_basename + ".so"

# change compiler settings
if sysname == "Windows":
    pfn = Path.cwd() / "setup.cfg"
    try:
        shutil.copyfile("win-setup.cfg", pfn)
    except:
        pass

# load SUEWS Fortran source files
dir_f95 = "../SUEWS-SourceCode/src"
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
    ]
]
path_other_f95 = list(set(path_all_f95) - set(path_target_f95) - set(path_exclude_f95))
fn_other_obj = [str(f).replace(".f95", ".o") for f in path_other_f95]
if sysname == "Windows":
    fn_other_obj.append(os.path.join(dir_f95, "strptime.o"))

src_f95 = path_target_f95 + path_other_f95


def readme():
    f = """
    `supy_driver` is `F2PY`-based python binary package for `supy` with `SUEWS` as the computation core.
    """
    return f


def get_version(ver_minor=0, dir_source=dir_f95, file="suews_ctrl_const.f95"):
    import subprocess

    pipe = None
    for cmd in ["git", "git.cmd"]:
        # try to find git in system path
        try:
            pipe = subprocess.Popen(
                [cmd, "describe", "--always", "--match", "2[0-9]*", "--dirty"],
                stdout=subprocess.PIPE,
            )
            (sout, serr) = pipe.communicate()

            # use git describe to get the version
            ver_git = sout.decode().strip()
            print('git describe: ', ver_git)
            list_ver_git = ver_git.split("-")
            ver_tag = list_ver_git[0]
            flag_dirty = False
            count_post = 0
            if 'dirty' in list_ver_git:
                flag_dirty = True
            else:
                if len(list_ver_git) > 1:
                    count_post = int(list_ver_git[1])
            if count_post > 0:
                ver = f'{ver_tag}.post{count_post}'+('+dirty' if flag_dirty else '')
            else:
                ver = ver_tag

            if pipe.returncode == 0:
                break
        except:
            pass
    if pipe is None or pipe.returncode != 0:
        # no git, use version from source
        try:
            path_source = Path(dir_source)
            path_makefile = path_source.parent / "Makefile"
            print(path_makefile, path_makefile.exists())

            # get version from `file`
            path_constfile = path_source / file
            print(path_constfile, path_constfile.exists())
            print(path_constfile)
            with open(str(path_constfile)) as fm:
                for line in fm:
                    if "progname" in line:
                        ver = line.split("SUEWS_V")[-1].replace("'", "").strip()
                        ver += str(ver_minor)
        except IOError:
            raise IOError(f"{path_constfile} not existing!")


    # cast `ver` to the driver package
    path_pkg_init = Path(".") / lib_basename / "version.py"
    try:
        with open(str(path_pkg_init), "w") as fm:
            fm.write("__version__='{ver}'".format(ver=ver))
    except:
        pass

    print('running version: ', ver)
    return ver


# %%
class BinaryDistribution(Distribution):
    def has_ext_modules(self):
        return True

    def is_pure(self):
        return False


# # print('will build', lib_name)
# for x in other_obj:
#     print(x)

ext_modules = [
    Extension(
        "supy_driver.suews_driver",
        [str(p) for p in path_target_f95],
        extra_compile_args=[
            "-D_POSIX_C_SOURCE=200809L",
            "-fbracket-depth=1024"
            if sysname == "Darwin"
            else "-Wall",  # for clang on MacOS
        ],
        extra_f90_compile_args=["-cpp", f"-I{str(path_mod)}"],
        f2py_options=[
            # '--quiet',
            # "--verbose",
            # "--debug-capi",  # this is for debugging data types
            # '--f2cmap="f2py_f2cmap"',
            # ('-DF2PY_REPORT_ATEXIT' if sysname == 'Linux' else ''),
        ],
        extra_objects=fn_other_obj,
        # "-v" under Linux is necessary because it can avoid the blank variable issue
        # ref: https://github.com/metomi/fcm/issues/220
        extra_link_args=["-v" if sysname == "Linux" else "-static"]
        + [f"-L{str(path_lib)}", "-lspartacus"],
    )
]

setup(
    name="supy_driver",
    # update version info here!
    version=get_version(ver_minor=9),
    description="the SUEWS driver driven by f2py",
    long_description=readme(),
    url="https://github.com/UMEP-dev/SuPy",
    author="Ting Sun",
    author_email="ting.sun@ucl.ac.uk",
    # license='GPL-V3.0',
    packages=["supy_driver"],
    package_data={
        "supy_driver": [
            # lib_name,
            # '*.json'
        ]
    },
    distclass=BinaryDistribution,
    ext_modules=ext_modules,
    python_requires=">=3.7",
    install_requires=["numpy"],
    include_package_data=True,
    zip_safe=False,
)


# check latest build
path_dir_driver = Path(__file__).resolve().parent
list_wheels = [str(x) for x in path_dir_driver.glob("dist/*whl")]

# use auditwheel to repair file name for Linux
if sysname == "Linux":
    if len(list_wheels) > 0:
        fn_wheel = sorted(list_wheels, key=os.path.getmtime)[-1]
        # print(list_wheels, fn_wheel)
        # path_dir_driver = Path(__file__).resolve().parent
        # list_wheels = [str(x) for x in path_dir_driver.glob('dist/*whl')]
        # fn_wheel = sorted(list_wheels, key=os.path.getmtime)[-1]
        # print(list_wheels, fn_wheel)
        subprocess.call(["auditwheel", "repair", fn_wheel])
        subprocess.call(["ls", "-lrt"])


#  change compiler settings
if sysname == "Windows":
    try:
        os.remove("setup.cfg")
    except FileNotFoundError:
        pass
