import os
import sys
import importlib.util
import sysconfig

# Get the proper file extension for the shared object file
so_ext = sysconfig.get_config_var("EXT_SUFFIX")

# Get the absolute path of the shared object file
so_file = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), f"_supy_driver{so_ext}"
)

# Load the shared object file as a module
spec = importlib.util.spec_from_file_location("_supy_driver", so_file)
_supy_driver = importlib.util.module_from_spec(spec)
sys.modules["_supy_driver"] = _supy_driver
spec.loader.exec_module(_supy_driver)

# Import the supy_driver package
from .supy_driver import *
