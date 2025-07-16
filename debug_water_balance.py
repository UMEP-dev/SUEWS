#!/usr/bin/env python3
"""
Debug script to investigate water balance test failure on macOS ARM64.

This script reproduces the failing test and adds detailed debugging to identify
the root cause of the NaN values in the water balance calculation.
"""

import pandas as pd
import numpy as np
import sys
import os
from pathlib import Path

# Add the src directory to the path so we can import supy
sys.path.insert(0, str(Path(__file__).parent / "src"))

# Force clean environment to prevent conda/mamba pollution
os.environ.pop("CMAKE_ARGS", None)
os.environ.pop("CONDA_PYTHON_EXE", None)
os.environ.pop("_CONDA_PYTHON_SYSCONFIGDATA_NAME", None)
os.environ.pop("CONDA_TOOLCHAIN_HOST", None)
os.environ.pop("CONDA_TOOLCHAIN_BUILD", None)
os.environ.pop("AR", None)
os.environ.pop("RANLIB", None)

# Set clean compiler environment
os.environ["AR"] = "ar"
os.environ["RANLIB"] = "ranlib"

try:
    import supy as sp
    print(f"✓ Successfully imported supy version: {sp.__version__}")
except Exception as e:
    print(f"✗ Failed to import supy: {e}")
    sys.exit(1)

def debug_water_balance():
    """Debug the water balance calculation to identify NaN source."""
    
    print("=" * 60)
    print("DEBUGGING WATER BALANCE CALCULATION")
    print("=" * 60)
    
    # Load benchmark data
    p_config = Path("test/benchmark1/benchmark1.yml")
    p_forcing = Path("test/benchmark1/forcing/Kc1_2011_data_5.txt")
    
    if not p_config.exists():
        print(f"✗ Config file not found: {p_config}")
        return False
    
    if not p_forcing.exists():
        print(f"✗ Forcing file not found: {p_forcing}")
        return False
    
    print(f"✓ Loading configuration from: {p_config}")
    print(f"✓ Loading forcing data from: {p_forcing}")
    
    # Load forcing data
    df_forcing = sp.load_forcing_df(p_forcing)
    print(f"✓ Forcing data shape: {df_forcing.shape}")
    print(f"✓ Forcing data columns: {list(df_forcing.columns)}")
    print(f"✓ Forcing data dtypes:\n{df_forcing.dtypes}")
    
    # Load initial state
    df_state_init = sp.init_supy(p_config)
    print(f"✓ Initial state shape: {df_state_init.shape}")
    
    # Run a shorter simulation to reproduce the issue
    n_days = 10  # Use fewer days for debugging
    df_forcing_part = df_forcing.iloc[:288 * n_days]
    
    print(f"✓ Running SUEWS simulation for {n_days} days...")
    print(f"✓ Forcing data subset shape: {df_forcing_part.shape}")
    
    # Run the simulation
    df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)
    
    print(f"✓ Simulation completed successfully")
    print(f"✓ Output shape: {df_output.shape}")
    print(f"✓ State shape: {df_state.shape}")
    
    # Debug the water balance calculation step by step
    print("\n" + "=" * 60)
    print("STEP-BY-STEP WATER BALANCE ANALYSIS")
    print("=" * 60)
    
    # Step 1: Get soilstore
    print("\n1. Getting soilstore...")
    df_soilstore = df_output.loc[1, "debug"].filter(regex="^ss_.*_next$")
    print(f"   Soilstore columns: {list(df_soilstore.columns)}")
    print(f"   Soilstore shape: {df_soilstore.shape}")
    print(f"   Soilstore dtypes:\n{df_soilstore.dtypes}")
    
    # Check for NaN values in soilstore
    nan_count = df_soilstore.isna().sum().sum()
    print(f"   NaN values in soilstore: {nan_count}")
    
    ser_sfr_surf = df_state_init.sfr_surf.iloc[0]
    print(f"   sfr_surf shape: {ser_sfr_surf.shape}")
    print(f"   sfr_surf dtype: {ser_sfr_surf.dtype}")
    
    ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)
    print(f"   ser_soilstore shape: {ser_soilstore.shape}")
    print(f"   ser_soilstore dtype: {ser_soilstore.dtype}")
    print(f"   ser_soilstore sample values: {ser_soilstore.head()}")
    
    # Step 2: Get water balance components
    print("\n2. Getting water balance components...")
    suews_cols = ["Rain", "Irr", "Evap", "RO", "State"]
    df_suews = df_output.SUEWS[suews_cols]
    
    print(f"   SUEWS data shape: {df_suews.shape}")
    print(f"   SUEWS data dtypes:\n{df_suews.dtypes}")
    
    # Check for NaN values in SUEWS data
    for col in suews_cols:
        nan_count = df_suews[col].isna().sum()
        print(f"   NaN values in {col}: {nan_count}")
    
    # Step 3: Create water balance DataFrame
    print("\n3. Creating water balance DataFrame...")
    df_water = df_suews.assign(
        SoilStore=ser_soilstore,
        TotalStore=ser_soilstore + df_suews.State
    )
    
    print(f"   Water balance DataFrame shape: {df_water.shape}")
    print(f"   Water balance DataFrame dtypes:\n{df_water.dtypes}")
    
    # Check for NaN values in water balance components
    for col in df_water.columns:
        nan_count = df_water[col].isna().sum()
        print(f"   NaN values in {col}: {nan_count}")
    
    # Step 4: Calculate water balance differences
    print("\n4. Calculating water balance differences...")
    
    # Change in total store
    ser_totalstore_change = df_water.TotalStore.diff().dropna()
    print(f"   TotalStore change dtype: {ser_totalstore_change.dtype}")
    print(f"   TotalStore change shape: {ser_totalstore_change.shape}")
    print(f"   TotalStore change NaN count: {ser_totalstore_change.isna().sum()}")
    print(f"   TotalStore change sample: {ser_totalstore_change.head()}")
    
    # Water input
    ser_water_in = df_water.Rain + df_water.Irr
    print(f"   Water input dtype: {ser_water_in.dtype}")
    print(f"   Water input shape: {ser_water_in.shape}")
    print(f"   Water input NaN count: {ser_water_in.isna().sum()}")
    print(f"   Water input sample: {ser_water_in.head()}")
    
    # Water output
    ser_water_out = df_water.Evap + df_water.RO
    print(f"   Water output dtype: {ser_water_out.dtype}")
    print(f"   Water output shape: {ser_water_out.shape}")
    print(f"   Water output NaN count: {ser_water_out.isna().sum()}")
    print(f"   Water output sample: {ser_water_out.head()}")
    
    # Water balance
    ser_water_balance = ser_water_in - ser_water_out
    print(f"   Water balance dtype: {ser_water_balance.dtype}")
    print(f"   Water balance shape: {ser_water_balance.shape}")
    print(f"   Water balance NaN count: {ser_water_balance.isna().sum()}")
    print(f"   Water balance sample: {ser_water_balance.head()}")
    
    # Step 5: Final calculation
    print("\n5. Final balance calculation...")
    
    # Align the series for subtraction
    print(f"   Aligning series for subtraction...")
    print(f"   ser_totalstore_change index: {ser_totalstore_change.index[:5]}")
    print(f"   ser_water_balance index: {ser_water_balance.index[:5]}")
    
    # Slice ser_water_balance to match ser_totalstore_change
    ser_water_balance_aligned = ser_water_balance.iloc[1:].reset_index(drop=True)
    ser_totalstore_change_aligned = ser_totalstore_change.reset_index(drop=True)
    
    print(f"   Aligned totalstore_change shape: {ser_totalstore_change_aligned.shape}")
    print(f"   Aligned water_balance shape: {ser_water_balance_aligned.shape}")
    
    # Calculate difference
    diff_series = ser_totalstore_change_aligned - ser_water_balance_aligned
    print(f"   Difference series dtype: {diff_series.dtype}")
    print(f"   Difference series NaN count: {diff_series.isna().sum()}")
    print(f"   Difference series sample: {diff_series.head()}")
    
    # Check if difference is all NaN
    if diff_series.isna().all():
        print("   ✗ All difference values are NaN!")
        return False
    
    # Calculate max difference
    max_diff = diff_series.abs().max()
    print(f"   Max difference: {max_diff}")
    
    # Test result
    test_result = max_diff < 1e-6
    print(f"   Test result: {test_result}")
    
    return test_result

if __name__ == "__main__":
    success = debug_water_balance()
    
    if success:
        print("\n✓ Water balance test passed!")
        sys.exit(0)
    else:
        print("\n✗ Water balance test failed!")
        sys.exit(1)