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
    "resample_output",  # Deprecated - use SUEWSOutput.resample() instead
    # Modules
    "util",
    "data_model",
    # Validation
    "validate_suews_config_conditional",
    "ValidationController",
    "ValidationResult",
    # Modern interface
    "SUEWSSimulation",
    "SUEWSCheckpoint",
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

# Procedural-API names that must emit a one-shot FutureWarning on first
# attribute access (gh#1370 phase 2 — visibility). Kept in sync with
# `supy._supy_module._FUNCTIONAL_DEPRECATIONS`; a regression test asserts
# the two are equal so a future addition cannot drift silently. Hard-coded
# here so the lazy-import router does not need to load `_supy_module` on
# every attribute miss — that would defeat fast CLI startup.
_DEPRECATED_FUNCTIONAL_NAMES = frozenset({
    "init_supy",
    "load_forcing_grid",
    "load_sample_data",
    "load_SampleData",
    "load_config_from_df",
    "run_supy",
    "run_supy_sample",
    "save_supy",
    "init_config",
    "resample_output",
})


def __getattr__(name):
    """Lazy attribute loader - defers heavy imports until actually needed.

    This enables fast CLI startup by not loading heavy modules at package import time.
    """
    if name in _lazy_cache:
        return _lazy_cache[name]

    # Procedural API: emit a one-shot FutureWarning on first attribute access
    # so users importing the symbol see the migration nudge immediately rather
    # than only when the function is called (gh#1370 phase 2). Subsequent
    # accesses hit `_lazy_cache` and stay silent. The in-body
    # `_warn_functional_deprecation` calls inside each function definition
    # remain as a safety net for code that bypasses `__getattr__`.
    if name in _DEPRECATED_FUNCTIONAL_NAMES:
        from . import _supy_module

        _supy_module._warn_functional_deprecation(name)
        _lazy_cache[name] = getattr(_supy_module, name)
        return _lazy_cache[name]

    # Non-deprecated `_supy_module` exports (utilities — keep silent)
    if name in {"check_forcing", "check_state"}:
        from . import _supy_module

        _lazy_cache[name] = getattr(_supy_module, name)
        return _lazy_cache[name]

    # Submodules
    if name == "util":
        import importlib

        util = importlib.import_module(".util", __name__)
        _lazy_cache[name] = util
        return _lazy_cache[name]

    if name == "data_model":
        import importlib

        data_model = importlib.import_module(".data_model", __name__)
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

    if name == "SUEWSCheckpoint":
        try:
            from .suews_checkpoint import SUEWSCheckpoint

            _lazy_cache[name] = SUEWSCheckpoint
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

    # Exception for Fortran kernel errors (kept for backward compatibility)
    if name == "SUEWSKernelError":

        class SUEWSKernelError(RuntimeError):
            """Error raised when the SUEWS Fortran kernel encounters a fatal condition."""

            def __init__(self, code=None, message=None):
                self.code = code
                self.message = message or "SUEWS kernel error"
                super().__init__(f"SUEWS kernel error (code={code}): {self.message}")

        _lazy_cache[name] = SUEWSKernelError
        return _lazy_cache[name]

    raise AttributeError(f"module 'supy' has no attribute {name!r}")
