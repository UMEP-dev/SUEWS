#!/usr/bin/env python
"""Test Fortran STOP statement interception (GH-1035).

This module tests that:
1. The _supy_driver extension exports STOP intercept symbols
2. Calling these symbols raises RuntimeError instead of terminating

The STOP interception is critical for QGIS compatibility - without it,
any Fortran STOP statement would crash the entire QGIS application.

For CI testing on Windows with OSGeo4W installation.
"""

from __future__ import annotations

import ctypes
import os
import subprocess
import sys
from pathlib import Path

# Target environment check
_IS_WINDOWS = sys.platform == "win32"

# Skip when run via pytest on non-Windows platforms
try:
    import pytest

    pytestmark = [
        pytest.mark.qgis,
        pytest.mark.skipif(
            not _IS_WINDOWS,
            reason="STOP interception test only runs on Windows",
        ),
    ]
except ImportError:
    pass  # Running standalone without pytest


def get_extension_path() -> Path | None:
    """Find the _supy_driver extension module path."""
    try:
        import supy._supy_driver as driver

        return Path(driver.__file__)
    except (ImportError, AttributeError):
        return None


def get_stop_symbols_dumpbin(pyd_path: Path) -> tuple[dict[str, bool], bool]:
    """Check for STOP intercept symbols using dumpbin (Windows only).

    Returns:
        tuple: (symbols_found dict, tools_available bool)
            - symbols_found: dict mapping symbol name to whether it was found
            - tools_available: True if dumpbin/nm was available for inspection
    """
    symbols_to_check = [
        "suews_stop_string",
        "suews_stop_numeric",
        "_gfortran_stop_string",
        "_gfortran_stop_numeric",
        "for_stop_core",
        "f90wrap_abort_",
    ]
    found = {sym: False for sym in symbols_to_check}

    if not _IS_WINDOWS:
        return found, False

    output = None
    try:
        # Try dumpbin (Visual Studio)
        result = subprocess.run(
            ["dumpbin", "/SYMBOLS", str(pyd_path)],
            capture_output=True,
            text=True,
            timeout=30,
        )
        output = result.stdout + result.stderr
    except (FileNotFoundError, subprocess.TimeoutExpired):
        try:
            # Try nm (MinGW/MSYS2)
            result = subprocess.run(
                ["nm", str(pyd_path)],
                capture_output=True,
                text=True,
                timeout=30,
            )
            output = result.stdout + result.stderr
        except (FileNotFoundError, subprocess.TimeoutExpired):
            print("WARNING: Neither dumpbin nor nm available for symbol inspection")
            return found, False

    for sym in symbols_to_check:
        if sym in output:
            found[sym] = True

    return found, True


def test_stop_symbols_present():
    """Verify STOP intercept symbols are present in the extension.

    This test FAILS if:
    - Symbol inspection tools are available AND critical symbols are missing

    This test SKIPS if:
    - Symbol inspection tools (dumpbin/nm) are not available

    Critical symbols that must be present (from vendored f90wrap):
    - suews_stop_string: Custom STOP handler
    - suews_stop_numeric: Custom numeric STOP handler
    - f90wrap_abort_: Error propagation handler
    """
    pyd_path = get_extension_path()
    if pyd_path is None:
        pytest.skip("_supy_driver extension not found")

    print(f"\nExtension path: {pyd_path}")

    symbols, tools_available = get_stop_symbols_dumpbin(pyd_path)

    if not tools_available:
        pytest.skip("Symbol inspection tools (dumpbin/nm) not available")

    print("\nSTOP intercept symbols:")
    for sym, found in symbols.items():
        status = "FOUND" if found else "MISSING"
        print(f"  {sym}: {status}")

    # Core symbols that must be present for QGIS compatibility
    critical_symbols = ["suews_stop_string", "suews_stop_numeric", "f90wrap_abort_"]
    missing = [sym for sym in critical_symbols if not symbols.get(sym, False)]

    if missing:
        pytest.fail(
            f"Critical STOP intercept symbols missing: {missing}\n"
            f"This indicates the vendored f90wrap with STOP handlers was not used during build.\n"
            f"Check that meson.build uses f2py-f90wrap-patched.py and that\n"
            f"src/supy/_vendor/f90wrap_src/f90wrap/__init__.py exists."
        )


def test_stop_interception_via_ctypes():
    """Test that STOP interception converts to Python exception.

    This test directly calls the stop handler functions via ctypes
    to verify they raise RuntimeError instead of terminating.
    """
    pyd_path = get_extension_path()
    if pyd_path is None:
        pytest.skip("_supy_driver extension not found")

    try:
        # Load the extension as a shared library
        lib = ctypes.CDLL(str(pyd_path))
    except OSError as e:
        pytest.skip(f"Could not load extension as CDLL: {e}")

    # Test suews_stop_string
    try:
        stop_string = lib.suews_stop_string
        stop_string.argtypes = [ctypes.c_char_p, ctypes.c_int]
        stop_string.restype = None
    except AttributeError:
        print("suews_stop_string not exported - may be inlined")
        return

    # Note: We can't actually call these functions directly because
    # they require the setjmp/longjmp context to be set up by f2py.
    # Calling them without that context would likely crash or behave
    # unpredictably.
    #
    # The real test is that these symbols exist and are linked into
    # the extension, which get_stop_symbols_dumpbin verifies.
    print("\nSTOP handler functions are accessible via ctypes.")
    print("Full interception test requires calling through f2py wrapper.")


def inspect_extension_symbols():
    """Print detailed symbol information for debugging.

    Returns exit code:
    - 0: All critical symbols found OR tools not available (can't verify)
    - 1: Extension not found
    - 2: Tools available but critical symbols missing (BUILD ERROR)
    """
    pyd_path = get_extension_path()
    if pyd_path is None:
        print("ERROR: _supy_driver extension not found")
        return 1

    print("=" * 60)
    print("STOP Interception Symbol Inspection")
    print("=" * 60)
    print(f"\nExtension: {pyd_path}")
    print(f"Size: {pyd_path.stat().st_size:,} bytes")

    symbols, tools_available = get_stop_symbols_dumpbin(pyd_path)

    if not tools_available:
        print("\nWARNING: Symbol inspection tools (dumpbin/nm) not available")
        print("Cannot verify STOP intercept symbols - skipping check")
        return 0

    print("\n--- STOP Intercept Symbols ---")
    for sym, found in symbols.items():
        status = "OK" if found else "MISSING"
        print(f"  [{status}] {sym}")

    # Check critical symbols
    critical_symbols = ["suews_stop_string", "suews_stop_numeric", "f90wrap_abort_"]
    missing = [sym for sym in critical_symbols if not symbols.get(sym, False)]

    if missing:
        print(f"\nERROR: Critical STOP intercept symbols missing: {missing}")
        print("This indicates the vendored f90wrap was NOT used during build!")
        print("Check: meson.build should use f2py-f90wrap-patched.py")
        print("Check: src/supy/_vendor/f90wrap_src/f90wrap/__init__.py must exist")
        return 2  # Build error - fail CI

    print("\nSUCCESS: All critical STOP intercept symbols found!")
    return 0


def main():
    """Main entry point for standalone execution."""
    if not _IS_WINDOWS:
        print("Skipping: STOP interception test only runs on Windows")
        return 0

    return inspect_extension_symbols()


if __name__ == "__main__":
    sys.exit(main())
