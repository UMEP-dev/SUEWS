#!/usr/bin/env python3
"""
Full Qt GUI test for SUEWS ERROR handling.

Uses QApplication instead of QCoreApplication to simulate
a full GUI environment like QGIS.

Tests if Qt's full event loop handles SUEWS errors correctly.
"""
import sys
import os

# Must set display for headless GUI
os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')

# Initialize Qt GUI application (simulates QGIS environment)
from PyQt5.QtWidgets import QApplication

app = QApplication(sys.argv)
print("QApplication initialized successfully (offscreen mode)")

print("\n=== Test: Trigger SUEWS error in full Qt GUI context ===")
print("This will trigger 'Windspeed Ht too low' error")

try:
    import supy as sp
    from supy._run import SUEWSKernelError
    from supy.data_model import init_config_from_yaml
    import warnings
    warnings.filterwarnings('ignore', category=DeprecationWarning)

    print(f"supy version: {sp.__version__}")

    # Use benchmark config, then modify state to trigger error
    config_path = os.path.join(
        os.path.dirname(__file__),
        '..', 'fixtures', 'benchmark1', 'benchmark1_short.yml'
    )

    print(f"Config: {config_path}")

    config = init_config_from_yaml(config_path)
    df_state = config.to_df_state()
    grid = df_state.index[0]

    # Load forcing
    df_forcing = sp.load_forcing_grid(config_path, grid, df_state_init=df_state)
    df_forcing_short = df_forcing.head(288)  # 1 day

    # Directly modify z and zdm_in columns
    z_col = ('z', '0')
    zdm_col = ('zdm_in', '0')

    print(f"Original z: {df_state.loc[grid, z_col]}")
    print(f"Original zdm_in: {df_state.loc[grid, zdm_col]}")

    # Set z=5, zdm=15 to trigger error (z - zdm = -10 < 0)
    df_state.loc[grid, z_col] = 5.0
    df_state.loc[grid, zdm_col] = 15.0

    print(f"Modified z: {df_state.loc[grid, z_col]}")
    print(f"Modified zdm_in: {df_state.loc[grid, zdm_col]}")

    print("\nRunning simulation with modified state...")
    print("z=5.0 should be less than zdm~15, triggering error")
    print("(Expecting SUEWSKernelError or RuntimeError)")

    # Run simulation - this should trigger error
    df_output, df_state_final = sp.run_supy(df_forcing_short, df_state, chunk_day=1)

    print(f"Output shape: {df_output.shape}")
    print("WARNING: No error was triggered. State modification may need adjustment.")

except SUEWSKernelError as e:
    print(f"\n*** SUEWSKernelError caught: {e} ***")
    print("SUCCESS: Exception handling works in Qt GUI context!")
    print("Qt did NOT crash when error occurred!")

except RuntimeError as e:
    print(f"\n*** RuntimeError caught: {e} ***")
    print("SUCCESS: RuntimeError handling works in Qt GUI context!")
    print("Qt did NOT crash when error occurred!")

except Exception as e:
    print(f"\n*** Unexpected exception: {type(e).__name__}: {e} ***")
    import traceback
    traceback.print_exc()

print("\n=== Test completed ===")
print("If you see this message, Qt GUI did NOT crash!")

# Clean exit
app.quit()
sys.exit(0)
