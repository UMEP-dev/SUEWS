#!/usr/bin/env python
"""Patch f90wrap runtime.py to use relative imports for vendoring.

This script is called during the meson build to copy and patch runtime.py
from the submodule to the install directory.
"""
import sys
from pathlib import Path


def patch_runtime(src: Path, dst: Path) -> None:
    """Copy runtime.py and convert absolute imports to relative."""
    content = src.read_text()

    # Convert absolute imports to relative
    content = content.replace('from f90wrap.fortrantype', 'from .fortrantype')
    content = content.replace('from f90wrap.arraydata', 'from .arraydata')
    content = content.replace('from f90wrap.sizeof_fortran_t', 'from .sizeof_fortran_t')
    content = content.replace('import f90wrap.fortrantype', 'from . import fortrantype')

    dst.write_text(content)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <src_runtime.py> <dst_runtime.py>")
        sys.exit(1)

    patch_runtime(Path(sys.argv[1]), Path(sys.argv[2]))
