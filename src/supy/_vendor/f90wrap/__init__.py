# Vendored f90wrap runtime - eliminates pip dependency on f90wrap
# Source: https://github.com/UMEP-dev/f90wrap (submodule at ext_lib/f90wrap)
#
# Only the runtime components are vendored (runtime.py, fortrantype.py,
# and compiled extensions). The code generation tools remain a build-time
# dependency via pyproject.toml.

from .runtime import *
from .fortrantype import (
    FortranDerivedType,
    FortranDerivedTypeArray,
    FortranModule,
)
