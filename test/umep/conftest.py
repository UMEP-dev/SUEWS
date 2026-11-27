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
    """Auto-apply qgis + native + os_compat markers and skip condition to test/umep/.

    UMEP tests exercise the QGIS plugin-facing wrapper surface including
    ``SUEWSSimulation.run()`` in ``test_processor.py``, so they carry the
    ``native`` primary dependency axis alongside ``qgis`` and ``os_compat``
    (gh#2349 W1-4). Root ``test/conftest.py`` attaches the temporary Schema-1
    ``api`` compatibility shim automatically until W1-7. The ``qgis`` tier
    gates them to Windows + Python 3.12; the platform skip below enforces
    that at runtime.
    """
    for item in items:
        # Only apply to tests in test/umep/
        if "test/umep" in str(item.fspath) or "test\\umep" in str(item.fspath):
            existing = {m.name for m in item.iter_markers()}
            if "qgis" not in existing:
                item.add_marker(pytest.mark.qgis)
            if "native" not in existing:
                item.add_marker(pytest.mark.native)
            if "os_compat" not in existing:
                item.add_marker(pytest.mark.os_compat)
            if not _IS_QGIS_TARGET:
                item.add_marker(
                    pytest.mark.skip(
                        reason=(
                            "QGIS compatibility tests only run on Windows + Python 3.12 "
                            "(current QGIS 3 LTR / QGIS 4 runtime)"
                        )
                    )
                )
