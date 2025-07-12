"""
SuPy: SUEWS that speaks Python

This package provides Python bindings for the Surface Urban Energy and Water balance Scheme (SUEWS).
"""

# Handle Windows DLL loading before importing the extension
import os
import sys
import platform

def _setup_windows_dll_path():
    """Ensure Windows can find the MinGW runtime DLLs."""
    if platform.system() != 'Windows':
        return
    
    # Add the directory containing the extension module to DLL search path
    try:
        # For Python 3.8+, use os.add_dll_directory
        if hasattr(os, 'add_dll_directory'):
            import importlib.util
            import pathlib
            
            # Find the directory containing our extension
            spec = importlib.util.find_spec('supy._supy_driver')
            if spec and spec.origin:
                ext_dir = pathlib.Path(spec.origin).parent
                if ext_dir.exists():
                    os.add_dll_directory(str(ext_dir))
                    
                    # Also add any DLL subdirectories
                    dll_dir = ext_dir / '.libs'
                    if dll_dir.exists():
                        os.add_dll_directory(str(dll_dir))
        
        # For older Python versions, modify PATH
        else:
            import pathlib
            import importlib
            
            # Try to find the module directory
            try:
                import supy
                supy_dir = pathlib.Path(supy.__file__).parent
                if supy_dir.exists():
                    os.environ['PATH'] = str(supy_dir) + os.pathsep + os.environ.get('PATH', '')
            except:
                pass
                
    except Exception as e:
        # Don't fail import if DLL setup fails
        import warnings
        warnings.warn(f"Could not set up Windows DLL search path: {e}")

# Set up DLL paths before any imports
_setup_windows_dll_path()

# Now do the actual imports
from ._run import *
from ._load import *
from ._post import *

from .util._era5 import *
from .util._wrf import *
from .util._gap_fill import *
from .util._roughness import *
from .util._plot import *
from .util import _datalayer
from .util._sim import *

# load sample data
from ._sample_data import load_SampleData

# other utilities
from ._env import *

# version info
from ._version import show_version, __version__

# Set up default backend for parallel processing
# this is put here to set the backend AFTER all serial imports are done
# NB: don't delete the above imports as they guarantee serial imports
from ._env import set_default_backend

# set_default_backend()

__all__ = [
    # Version info
    "__version__",
    "show_version",
    # Main functions
    "run_supy",
    "run_supy_ser",
    "init_supy",
    "load_forcing",
    "load_SampleData",
    # Utilities
    "make_grid",
    "collect_numpy_output",
    "SuPy_forcing_GMT",
    "cal_dailystate",
    "cal_dailystate_DTS",
    "gap_fill_forcing",
    "load_SUEWS_nml",
    "load_InitialCond_grid_df",
    "save_supy",
]