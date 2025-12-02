"""Pytest configuration for UMEP/QGIS compatibility tests.

Shared markers and constants for all test modules in this directory.
Target environment: Windows + Python 3.12 (QGIS 3.40 LTR bundled Python).

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import sys

import pytest

# Target environment: Windows + Python 3.12 (QGIS 3.40 LTR)
_IS_WINDOWS = sys.platform == "win32"
_IS_PY312 = sys.version_info[:2] == (3, 12)
_IS_QGIS_TARGET = _IS_WINDOWS and _IS_PY312

# Check if safe_stdout_stderr is available (added in PR #903)
try:
    from supy.util._era5 import safe_stdout_stderr  # noqa: F401

    _HAS_SAFE_STDOUT_STDERR = True
except ImportError:
    _HAS_SAFE_STDOUT_STDERR = False


def pytest_collection_modifyitems(items):
    """Apply qgis marker and skip condition to all tests in this directory."""
    for item in items:
        # Only apply to tests in test/umep/
        if "test/umep" in str(item.fspath) or "test\\umep" in str(item.fspath):
            item.add_marker(pytest.mark.qgis)
            if not _IS_QGIS_TARGET:
                item.add_marker(
                    pytest.mark.skip(
                        reason="QGIS compatibility tests only run on Windows + Python 3.12 (QGIS 3.40 LTR)"
                    )
                )
