#!/usr/bin/env python3
"""
Headless Qt test for SUEWS error handling.

Tests whether f90wrap's signal handler conflicts with Qt.
This simulates the QGIS environment without the full GUI.

Test scenarios:
1. Normal run (no error) - Tests if SIGINT handler conflicts with Qt
2. Error run (trigger error) - Tests if exception handling works in Qt context
"""
import sys
import os

# Initialize Qt event loop (simulates QGIS environment)
from PyQt5.QtCore import QCoreApplication

app = QCoreApplication(sys.argv)
print("Qt initialized successfully")
print(f"Qt version: {QCoreApplication.applicationVersion()}")

# Test 1: Simple import and normal operation
print("\n=== Test 1: Normal supy import and version check ===")
try:
    import supy as sp
    print(f"supy version: {sp.__version__}")
    print("SUCCESS: supy import works in Qt context")
except Exception as e:
    print(f"FAILED: {type(e).__name__}: {e}")
    sys.exit(1)

# Test 2: Load a config (tests basic functionality without running simulation)
print("\n=== Test 2: Load config without running simulation ===")
try:
    from supy.data_model import init_config_from_yaml

    config_path = os.path.join(
        os.path.dirname(__file__),
        '..', 'fixtures', 'benchmark1', 'benchmark1_short.yml'
    )

    if os.path.exists(config_path):
        config = init_config_from_yaml(config_path)
        df_state = config.to_df_state()
        print(f"Config loaded, grids: {len(df_state)}")
        print("SUCCESS: Config loading works in Qt context")
    else:
        print(f"SKIPPED: Config not found at {config_path}")

except Exception as e:
    print(f"FAILED: {type(e).__name__}: {e}")

# Test 3: Run a very short simulation (tests Fortran call with setjmp)
print("\n=== Test 3: Run simulation (tests Fortran call + setjmp) ===")
try:
    from supy.data_model import init_config_from_yaml

    config_path = os.path.join(
        os.path.dirname(__file__),
        '..', 'fixtures', 'benchmark1', 'benchmark1_short.yml'
    )

    if os.path.exists(config_path):
        config = init_config_from_yaml(config_path)
        df_state = config.to_df_state()
        grid = df_state.index[0]

        # Load forcing for just 1 day to speed up test
        df_forcing = sp.load_forcing_grid(config_path, grid, df_state_init=df_state)
        df_forcing_short = df_forcing.head(288)  # 1 day at 5-min intervals

        print(f"Running simulation for grid {grid}...")
        df_output, df_state_final = sp.run_supy(df_forcing_short, df_state, chunk_day=1)

        print(f"Output shape: {df_output.shape}")
        print("SUCCESS: Simulation runs in Qt context!")
    else:
        print(f"SKIPPED: Config not found")

except Exception as e:
    print(f"Exception during simulation: {type(e).__name__}: {e}")
    # This is expected if we trigger an error condition

print("\n=== All tests completed ===")
print("If you see this message, Qt did NOT crash!")

# Clean exit
app.quit()
sys.exit(0)
