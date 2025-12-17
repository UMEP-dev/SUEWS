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

# Lazy import implementation for fast CLI startup
# Heavy imports are deferred until actually accessed

# Version can be imported quickly (doesn't trigger heavy package discovery)
from ._version_scm import __version__

# module docs
__doc__ = """
supy - SUEWS that speaks Python
===============================

**SuPy** is a Python-enhanced urban climate model with SUEWS as its computation core.

"""

# List of public symbols (for `from supy import *`)
__all__ = [
    # Core functions
    "init_supy",
    "load_SampleData",
    "load_sample_data",
    "load_forcing_grid",
    "load_config_from_df",
    "run_supy",
    "save_supy",
    "check_forcing",
    "check_state",
    "init_config",
    "run_supy_sample",
    "resample_output",
    # Debug utilities
    "pack_dts_state_selective",
    "inspect_dts_structure",
    "dict_structure",
    # Modules
    "util",
    "data_model",
    # Validation
    "validate_suews_config_conditional",
    "ValidationController",
    "ValidationResult",
    # Modern interface
    "SUEWSSimulation",
    "SUEWSForcing",
    "SUEWSOutput",
    # Exceptions
    "SUEWSKernelError",
    # Version
    "show_version",
    "__version__",
    # CLI
    "SUEWS",
]

# Cache for lazy-loaded modules and attributes
_lazy_cache = {}


def __getattr__(name):
    """Lazy attribute loader - defers heavy imports until actually needed.

    This enables fast CLI startup by not loading heavy modules at package import time.
    """
    if name in _lazy_cache:
        return _lazy_cache[name]

    # Core functions from _supy_module
    if name in {
        "init_supy",
        "load_SampleData",
        "load_sample_data",
        "load_forcing_grid",
        "load_config_from_df",
        "run_supy",
        "save_supy",
        "check_forcing",
        "check_state",
        "init_config",
        "run_supy_sample",
    }:
        from . import _supy_module

        _lazy_cache[name] = getattr(_supy_module, name)
        return _lazy_cache[name]

    # resample_output can come from either module
    if name == "resample_output":
        from ._supy_module import resample_output

        _lazy_cache[name] = resample_output
        return _lazy_cache[name]

    # Debug utilities from _post
    if name in {"pack_dts_state_selective", "inspect_dts_structure", "dict_structure"}:
        from . import _post

        _lazy_cache[name] = getattr(_post, name)
        return _lazy_cache[name]

    # Submodules
    if name == "util":
        from . import util

        _lazy_cache[name] = util
        return _lazy_cache[name]

    if name == "data_model":
        from . import data_model

        _lazy_cache[name] = data_model
        return _lazy_cache[name]

    # Validation functionality
    if name in {
        "validate_suews_config_conditional",
        "ValidationController",
        "ValidationResult",
    }:
        try:
            from .data_model.validation import (
                validate_suews_config_conditional,
                ValidationController,
                ValidationResult,
            )

            _lazy_cache["validate_suews_config_conditional"] = (
                validate_suews_config_conditional
            )
            _lazy_cache["ValidationController"] = ValidationController
            _lazy_cache["ValidationResult"] = ValidationResult
            return _lazy_cache[name]
        except ImportError:
            _lazy_cache[name] = None
            return None

    # Modern simulation interface
    if name == "SUEWSSimulation":
        try:
            from .suews_sim import SUEWSSimulation

            _lazy_cache[name] = SUEWSSimulation
            return _lazy_cache[name]
        except ImportError:
            return None

    if name == "SUEWSForcing":
        try:
            from .suews_forcing import SUEWSForcing

            _lazy_cache[name] = SUEWSForcing
            return _lazy_cache[name]
        except ImportError:
            return None

    if name == "SUEWSOutput":
        try:
            from .suews_output import SUEWSOutput

            _lazy_cache[name] = SUEWSOutput
            return _lazy_cache[name]
        except ImportError:
            return None

    # Version info
    if name == "show_version":
        from ._version import show_version

        _lazy_cache[name] = show_version
        return _lazy_cache[name]

    # CLI command
    if name == "SUEWS":
        from .cmd import SUEWS

        _lazy_cache[name] = SUEWS
        return _lazy_cache[name]

    # Exception for Fortran kernel errors
    if name == "SUEWSKernelError":
        from ._run import SUEWSKernelError

        _lazy_cache[name] = SUEWSKernelError
        return _lazy_cache[name]

    raise AttributeError(f"module 'supy' has no attribute {name!r}")
