#!/usr/bin/env python
"""Test SUEWS error handling inside PyQGIS context.

This test initialises the full QGIS application (headless) and runs
supy error handling tests within that context. This is the closest
we can get to testing the actual QGIS crash scenario without a GUI.

For CI testing on Windows with OSGeo4W installation.
"""

import sys
import os

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

    # Modify state to trigger "Windspeed Ht too low" error
    # z=5.0 with zdm=15.0 causes (z-zdm) < 0
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
    except RuntimeError as e:
        error_caught = True
        print(f"\n*** RuntimeError caught in PyQGIS context: ***")
        print(f"  {str(e)[:100]}...")
        print("*** QGIS did NOT crash! ***")

    return error_caught


def main():
    """Main test runner."""
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
