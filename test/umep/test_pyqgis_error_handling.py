#!/usr/bin/env python
"""Test SUEWS error handling inside PyQGIS context.

This test initialises the full QGIS application (headless) and runs
supy error handling tests within that context. Run as standalone script
in CI: python test/umep/test_pyqgis_error_handling.py

For CI testing on Windows with OSGeo4W installation (GH-1035).
"""

import importlib.util
import os
import sys

# QGIS LTR Python version - update when QGIS LTR changes
# QGIS 3.40 LTR (2024-2025) uses Python 3.12
# Check: https://qgis.org/en/site/forusers/visualchangelog340/
QGIS_LTR_PYTHON_VERSION = (3, 12)

# Target environment check
_IS_QGIS_PLATFORM = (
    sys.platform == "win32"
    and sys.version_info[:2] == QGIS_LTR_PYTHON_VERSION
)
_HAS_QGIS = importlib.util.find_spec("qgis") is not None
_IS_QGIS_TARGET = _IS_QGIS_PLATFORM and _HAS_QGIS

# Skip when run via pytest on non-QGIS platforms (pytest may not be installed)
try:
    import pytest
    pytestmark = [
        pytest.mark.qgis,
        pytest.mark.skipif(
            not _IS_QGIS_TARGET,
            reason=f"PyQGIS test only runs on Windows + Python {QGIS_LTR_PYTHON_VERSION[0]}.{QGIS_LTR_PYTHON_VERSION[1]} (QGIS LTR)",
        ),
    ]
except ImportError:
    pass  # Running standalone without pytest

# Must set QT_QPA_PLATFORM before importing Qt
os.environ["QT_QPA_PLATFORM"] = "offscreen"


def setup_qgis():
    """Initialise QGIS application in headless mode."""
    # Import QGIS modules
    from qgis.core import QgsApplication, Qgis

    # Supply path to QGIS install location
    # On OSGeo4W, this is typically handled by o4w_env.bat
    qgs = QgsApplication([], False)

    # Load providers
    qgs.initQgis()

    print(f"QGIS version: {Qgis.version()}")
    print(f"QGIS prefix path: {qgs.prefixPath()}")

    return qgs


def test_suews_error_in_qgis():
    """Test that SUEWS errors are caught properly in QGIS context."""
    import supy as sp
    import pandas as pd
    import copy

    print(f"\nsupy version: {sp.__version__}")
    print("Testing SUEWS error handling in PyQGIS context...")

    # Load benchmark data
    df_state_init, df_forcing = sp.load_SampleData()
    df_state_test = copy.deepcopy(df_state_init)

    # Modify state to trigger z < zdm error (invalid configuration)
    # z=5.0 with zdm=15.0 causes (z-zdm) < 0, triggering kernel error
    df_state_test.loc[:, ("z", "0")] = 5.0
    df_state_test.loc[:, ("zdm_in", "0")] = 15.0

    print("Running SUEWS with invalid z/zdm configuration...")
    print("  z = 5.0 m, zdm = 15.0 m (z - zdm < 0, should trigger error)")

    error_caught = False
    try:
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:288],  # First day only
            df_state_test,
            logging_level=50,  # CRITICAL only
        )
    except sp.SUEWSKernelError as e:
        error_caught = True
        # Accept errors that indicate z < zdm issue caught correctly
        # - Code 14: "Inappropriate value calculated" (roughness parameters)
        # - Code 32: "Windspeed Ht too low" (stability calculation)
        # Use error code and simple patterns to avoid character encoding issues
        error_str = str(e)
        valid_codes = {14, 32}
        valid_patterns = [
            "Windspeed Ht too low",
            "z-zd",  # Simpler pattern without < character
            "Inappropriate value",
            "RoughnessParameters",
        ]
        code_valid = e.code in valid_codes
        pattern_valid = any(msg in error_str for msg in valid_patterns)
        if not (code_valid or pattern_valid):
            raise AssertionError(
                f"Unexpected SUEWS error for z/zdm test (code={e.code}): {e}"
            ) from e
        print("\n*** SUEWSKernelError caught in PyQGIS context: ***")
        print(f"  {str(e)[:100]}...")
        print("*** QGIS did NOT crash! ***")

    if __name__ != "__main__" and "pytest" in sys.modules:
        assert error_caught, "Expected SUEWSKernelError in PyQGIS context"
    return error_caught


def main():
    """Main test runner."""
    if not _IS_QGIS_PLATFORM:
        print(
            "Skipping PyQGIS error handling test: QGIS not available or "
            "not running on Windows + QGIS LTR Python."
        )
        return 0
    if not _HAS_QGIS:
        print(
            "PyQGIS not available on sys.path. Ensure OSGeo4W/QGIS is installed "
            "and the environment is configured (o4w_env.bat)."
        )
        return 1
    print("=" * 60)
    print("PyQGIS Error Handling Test")
    print("=" * 60)

    # Initialise QGIS
    qgs = setup_qgis()

    try:
        # Run the test
        success = test_suews_error_in_qgis()

        if success:
            print("\n" + "=" * 60)
            print("SUCCESS: Error handling works in PyQGIS context!")
            print("QGIS did NOT crash when SUEWS error occurred!")
            print("=" * 60)
            return 0
        else:
            print("\n" + "=" * 60)
            print("UNEXPECTED: No error was raised (check test configuration)")
            print("=" * 60)
            return 1

    finally:
        # Clean up QGIS
        qgs.exitQgis()


if __name__ == "__main__":
    sys.exit(main())
