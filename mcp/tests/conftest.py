"""Shared fixtures for the suews-mcp test suite.

Adds the package's ``src/`` directory to ``sys.path`` so tests can be run
directly from the repo without installing the package, mirroring how the
parent ``supy`` development install works.
"""

from __future__ import annotations

import sys
from pathlib import Path

_HERE = Path(__file__).resolve().parent
_SRC = _HERE.parent / "src"
if str(_SRC) not in sys.path:
    sys.path.insert(0, str(_SRC))
