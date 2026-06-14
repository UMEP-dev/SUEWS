"""Pytest configuration for UMEP/QGIS compatibility tests.

Shared markers and constants for all test modules in this directory.
Target environment: Windows + Python 3.12. Current Windows QGIS 3 LTR and
QGIS 4 runtimes both use this CPython line, while differing mainly in Qt/PyQt.

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import sys

import pytest

# Target environment: Windows + Python 3.12 (current QGIS 3 LTR / QGIS 4)
_IS_WINDOWS = sys.platform == "win32"
_IS_PY312 = sys.version_info[:2] == (3, 12)
_IS_QGIS_TARGET = _IS_WINDOWS and _IS_PY312

# Check if safe_stdout_stderr is available (added in PR #903)
try:
    from supy.util._era5 import safe_stdout_stderr  # noqa: F401, PLC2701

    _HAS_SAFE_STDOUT_STDERR = True
except ImportError:
    _HAS_SAFE_STDOUT_STDERR = False


def pytest_collection_modifyitems(items):
    """Auto-apply qgis + api markers and skip condition to all tests in this directory.

    UMEP tests are Python wrapper tests (pydantic/pandas/QGIS plugin glue), so they
    carry the `api` nature marker (gh#1300). They still matter with the Rust backend:
    physics coverage lives elsewhere, while this lane guards plugin-facing API,
    output-path, import, and QGIS stdout/stderr contracts. The `qgis` tier gates them
    to Windows + Python 3.12; the platform skip below enforces that at runtime.
    """
    for item in items:
        # Only apply to tests in test/umep/
        if "test/umep" in str(item.fspath) or "test\\umep" in str(item.fspath):
            item.add_marker(pytest.mark.qgis)
            item.add_marker(pytest.mark.api)
            if not _IS_QGIS_TARGET:
                item.add_marker(
                    pytest.mark.skip(
                        reason=(
                            "QGIS compatibility tests only run on Windows + Python 3.12 "
                            "(current QGIS 3 LTR / QGIS 4 runtime)"
                        )
                    )
                )
