#!/usr/bin/env python3
"""
Headless Qt test for SUEWS ERROR handling.

This test triggers an error condition to see if Qt crashes
when SUEWSKernelError is raised.

Error trigger: Low observation height relative to displacement height
"""
import sys
import os

# Initialize Qt event loop (simulates QGIS environment)
from PyQt5.QtCore import QCoreApplication

app = QCoreApplication(sys.argv)
print("Qt initialized successfully")

print("\n=== Test: Trigger SUEWS error in Qt context ===")
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

    # Modify state to have very low z (measurement height)
    # Find the z column and set it lower than zdm
    print(f"Original z: looking for column...")

    # Find columns containing 'z' in the name
    z_cols = [c for c in df_state.columns if 'z' in str(c).lower()]
    print(f"Z-related columns: {z_cols[:10]}")

    # Modify the measurement height (z) to be lower than zdm_in
    # This should trigger "Windspeed Ht too low" error
    # In SUEWS: zzd = z - zdm  -> if negative, triggers ErrorHint(32)

    # Let's try setting z very low
    # Check current values
    site_cols = [c for c in df_state.columns if c[0] == 'site']
    print(f"Site columns: {site_cols}")

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
    print("SUCCESS: Exception handling works in Qt context!")
    print("Qt did NOT crash when error occurred!")

except RuntimeError as e:
    print(f"\n*** RuntimeError caught: {e} ***")
    print("SUCCESS: RuntimeError handling works in Qt context!")
    print("Qt did NOT crash when error occurred!")

except Exception as e:
    print(f"\n*** Unexpected exception: {type(e).__name__}: {e} ***")
    import traceback
    traceback.print_exc()

print("\n=== Test completed ===")
print("If you see this message, Qt did NOT crash!")

# Clean exit
app.quit()
sys.exit(0)
