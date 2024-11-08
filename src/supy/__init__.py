###########################################################################
# SuPy: SUEWS that speaks Python
# Authors:
# Ting Sun, ting.sun@reading.ac.uk
# History:
# 20 Jan 2018: first alpha release
# 01 Feb 2018: performance improvement
# 03 Feb 2018: improvement in output processing
# 08 Mar 2018: pypi packaging
# 01 Jan 2019: public release
# 22 May 2019: restructure of module layout
# 02 Oct 2019: logger restructured
###########################################################################


# core functions
from ._supy_module import (
    init_supy,
    load_SampleData,
    load_forcing_grid,
    run_supy,
    save_supy,
    check_forcing,
    check_state,
    load_stebbs_bldg_nml
)


# utilities
from . import util


# version info
from ._version import show_version, __version__

from .cmd import SUEWS

# module docs
__doc__ = """
supy - SUEWS that speaks Python
===============================

**SuPy** is a Python-enhanced urban climate model with SUEWS as its computation core.

"""
