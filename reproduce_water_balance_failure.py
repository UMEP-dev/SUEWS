#!/usr/bin/env python
"""
Reproduce the water balance test failure exactly as it happens in CI
"""
import sys
import platform
import pandas as pd
import numpy as np
import supy as sp

print("=== Environment Info ===")
print(f"Python version: {sys.version}")
print(f"Platform: {platform.system()} {platform.machine()}")
print(f"Pandas version: {pd.__version__}")
print(f"Numpy version: {np.__version__}")
print(f"SuPy version: {sp.__version__}")

# Replicate the exact test
print("\n=== Running Water Balance Test ===")

# Load data exactly as in test
df_state_init, df_forcing_tstep = sp.load_SampleData()

# Configuration from test
n_days = 100
tolerance = 1e-6
grid_id = 1

# Run simulation
print(f"Running simulation for {n_days} days...")
df_forcing_part = df_forcing_tstep.iloc[:288 * n_days]
df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

# Extract data exactly as in the original test (before our fixes)
df_soilstore = df_output.loc[grid_id, "debug"].filter(regex="^ss_.*_next$")
ser_sfr_surf = df_state_init.loc[grid_id, "sfr_surf"]

print(f"\n=== Debugging dtype issue ===")
print(f"ser_sfr_surf dtype: {ser_sfr_surf.dtype}")
print(f"ser_sfr_surf values: {ser_sfr_surf.values}")

# Calculate weighted soilstore - ORIGINAL CODE (that fails in CI)
ser_soilstore_weighted = df_soilstore.dot(ser_sfr_surf.values)

print(f"\nser_soilstore_weighted has NaN: {ser_soilstore_weighted.isnull().any()}")
if ser_soilstore_weighted.isnull().any():
    print(f"NaN count: {ser_soilstore_weighted.isnull().sum()}")
    print(f"First few values: {ser_soilstore_weighted.head()}")

# Extract water balance components
water_columns = ["Rain", "Irr", "Evap", "RO", "State"]
df_suews_water = df_output.loc[grid_id, "SUEWS"][water_columns]

print(f"\n=== Testing DataFrame assignment (ORIGINAL failing approach) ===")
# This is the ORIGINAL code that causes NaN in CI
df_water = df_suews_water.assign(
    SoilStore=ser_soilstore_weighted.values,
    TotalStore=ser_soilstore_weighted.values + df_suews_water["State"].values
)

print(f"df_water shape: {df_water.shape}")
print(f"df_water has NaN: {df_water.isnull().any().any()}")
if df_water.isnull().any().any():
    print(f"Columns with NaN: {df_water.columns[df_water.isnull().any()].tolist()}")
    print(f"NaN counts by column:")
    print(df_water.isnull().sum())

# Calculate water balance closure
ser_totalstore_change = df_water["TotalStore"].diff().dropna()
ser_water_in = df_water["Rain"] + df_water["Irr"]
ser_water_out = df_water["Evap"] + df_water["RO"]
ser_water_balance = ser_water_in - ser_water_out

# Check the result
try:
    max_diff = (ser_totalstore_change - ser_water_balance).abs().max()
    test_passed = max_diff < tolerance
    
    print(f"\n=== Water Balance Test Result ===")
    print(f"Max water balance difference: {max_diff}")
    print(f"Tolerance: {tolerance}")
    print(f"Test result: {'PASS' if test_passed else 'FAIL'}")
    
    if not test_passed:
        print(f"\nTest FAILED!")
        print(f"max_diff ({max_diff}) > tolerance ({tolerance})")
        
        # Debug info
        diff_series = (ser_totalstore_change - ser_water_balance).abs()
        print(f"\nDiff series dtype: {diff_series.dtype}")
        print(f"Diff series has NaN: {diff_series.isnull().any()}")
        if diff_series.isnull().any():
            print(f"NaN count in diff: {diff_series.isnull().sum()}")
            
except Exception as e:
    print(f"\nERROR in water balance calculation: {e}")
    print(f"Error type: {type(e)}")
    import traceback
    traceback.print_exc()

print("\n=== Testing index mismatch theory ===")
print(f"df_suews_water index type: {type(df_suews_water.index)}")
print(f"df_suews_water index: {df_suews_water.index[:5]}")
print(f"ser_soilstore_weighted index type: {type(ser_soilstore_weighted.index)}")
print(f"ser_soilstore_weighted index: {ser_soilstore_weighted.index[:5]}")

# Test if indices match
if hasattr(df_suews_water.index, 'levels'):
    print(f"df_suews_water is MultiIndex with levels: {df_suews_water.index.names}")
if hasattr(ser_soilstore_weighted.index, 'levels'):
    print(f"ser_soilstore_weighted is MultiIndex with levels: {ser_soilstore_weighted.index.names}")
else:
    print(f"ser_soilstore_weighted is simple index")