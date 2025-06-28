# this is a patched version of f2py-f90wrap to make it work with cibuildwheel on windows


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


import sys

if sys.version_info < (3, 12):
    import distutils.cygwinccompiler

    distutils.cygwinccompiler.get_msvcr = get_msvcr_patch
else:
    pass


# -*- coding: utf-8 -*-
import re
import sys
from f90wrap.scripts.f2py_f90wrap import main

if __name__ == "__main__":
    sys.argv[0] = re.sub(r"(-script\.pyw|\.exe)?$", "", sys.argv[0])
    sys.exit(main())
