#!/usr/bin/env python
"""Verify STOP intercept symbols in built extension (GH-1035).

This test validates that the vendored f90wrap with STOP handlers was used
during build. Without these symbols, Fortran STOP statements crash the
Python process instead of raising exceptions.

Runs on all platforms as part of cibuildwheel test step.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

# Mark as core test - runs with smoke and core tiers
pytestmark = pytest.mark.core


def get_extension_path() -> Path | None:
    """Find the _supy_driver extension module path."""
    try:
        import supy._supy_driver as driver

        return Path(driver.__file__)
    except (ImportError, AttributeError):
        return None


def get_stop_symbols(ext_path: Path) -> tuple[dict[str, bool], bool]:
    """Check for STOP intercept symbols using nm (cross-platform).

    Returns:
        tuple: (symbols_found dict, tools_available bool)
            - symbols_found: dict mapping symbol name to whether it was found
            - tools_available: True if nm/dumpbin was available for inspection
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

    # Try nm first (available on all platforms)
    # On Windows in cibw, nm is available from MSYS2
    output = None
    for cmd in [["nm", str(ext_path)], ["dumpbin", "/SYMBOLS", str(ext_path)]]:
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30,
            )
            if result.returncode == 0 and result.stdout:
                output = result.stdout
                break
        except (FileNotFoundError, subprocess.TimeoutExpired):
            continue

    if output is None:
        print("WARNING: Neither nm nor dumpbin available for symbol inspection")
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
    - Symbol inspection tools (nm/dumpbin) are not available
    - Extension module not found

    Critical symbols that must be present (from vendored f90wrap):
    - suews_stop_string: Custom STOP handler
    - suews_stop_numeric: Custom numeric STOP handler
    - f90wrap_abort_: Error propagation handler
    """
    ext_path = get_extension_path()
    if ext_path is None:
        pytest.skip("_supy_driver extension not found")

    print(f"\nExtension path: {ext_path}")

    symbols, tools_available = get_stop_symbols(ext_path)

    if not tools_available:
        pytest.skip("Symbol inspection tools (nm/dumpbin) not available")

    print("\nSTOP intercept symbols:")
    for sym, present in symbols.items():
        status = "FOUND" if present else "MISSING"
        print(f"  {sym}: {status}")

    # Core symbols that must be present
    critical_symbols = ["suews_stop_string", "suews_stop_numeric", "f90wrap_abort_"]
    missing = [sym for sym in critical_symbols if not symbols.get(sym, False)]

    if missing:
        pytest.fail(
            f"Critical STOP intercept symbols missing: {missing}\n"
            f"This indicates the vendored f90wrap with STOP handlers was not used during build.\n"
            f"Check that meson.build uses f2py-f90wrap-patched.py and that\n"
            f"src/supy/_vendor/f90wrap_src/f90wrap/__init__.py exists."
        )


def main():
    """Standalone entry point for debugging."""
    ext_path = get_extension_path()
    if ext_path is None:
        print("ERROR: _supy_driver extension not found")
        return 1

    print("=" * 60)
    print("STOP Intercept Symbol Validation")
    print("=" * 60)
    print(f"\nPlatform: {sys.platform}")
    print(f"Extension: {ext_path}")
    print(f"Size: {ext_path.stat().st_size:,} bytes")

    symbols, tools_available = get_stop_symbols(ext_path)

    if not tools_available:
        print("\nWARNING: Symbol inspection tools not available")
        print("Cannot verify STOP intercept symbols")
        return 0

    print("\n--- STOP Intercept Symbols ---")
    for sym, present in symbols.items():
        status = "OK" if present else "MISSING"
        print(f"  [{status}] {sym}")

    critical_symbols = ["suews_stop_string", "suews_stop_numeric", "f90wrap_abort_"]
    missing = [sym for sym in critical_symbols if not symbols.get(sym, False)]

    if missing:
        print(f"\nERROR: Critical symbols missing: {missing}")
        print("Build used wrong f90wrap - vendored version not applied!")
        return 2

    print("\nSUCCESS: All critical STOP intercept symbols found!")
    return 0


if __name__ == "__main__":
    sys.exit(main())
