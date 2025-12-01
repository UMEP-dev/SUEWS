# Vendored f90wrap runtime - eliminates pip dependency on f90wrap
# Source: https://github.com/UMEP-dev/f90wrap (submodule at _vendor/f90wrap_src)
#
# Only the runtime components are vendored (runtime.py, fortrantype.py,
# and compiled extensions). The code generation tools remain a build-time
# dependency via pyproject.toml.
#
# Note: runtime.py is patched during build to use relative imports.

from .fortrantype import (
    FortranDerivedType,
    FortranDerivedTypeArray,
    FortranModule,
)
from .runtime import *
