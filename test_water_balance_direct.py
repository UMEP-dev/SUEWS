#!/usr/bin/env python3
"""
Direct test of water balance calculation without relying on the editable install.
This script reproduces the water balance test failure by running the calculation directly.
"""

import sys
import os
from pathlib import Path
import numpy as np
import pandas as pd

# Add the source directory to the Python path
sys.path.insert(0, str(Path(__file__).parent / "src"))

# Set clean environment
os.environ.pop("CMAKE_ARGS", None)
os.environ.pop("CONDA_PYTHON_EXE", None)
os.environ.pop("_CONDA_PYTHON_SYSCONFIGDATA_NAME", None)
os.environ.pop("CONDA_TOOLCHAIN_HOST", None)
os.environ.pop("CONDA_TOOLCHAIN_BUILD", None)
os.environ.pop("AR", None)
os.environ.pop("RANLIB", None)
os.environ["AR"] = "ar"
os.environ["RANLIB"] = "ranlib"

def test_water_balance_direct():
    """Test water balance calculation directly using benchmark data."""
    
    print("="*60)
    print("DIRECT WATER BALANCE TEST")
    print("="*60)
    
    # Check if we can import supy
    try:
        import supy as sp
        print(f"✓ Successfully imported supy version: {sp.__version__}")
    except Exception as e:
        print(f"✗ Failed to import supy: {e}")
        print("This is expected - we'll use the compiled Fortran libraries directly")
        return False
    
    # Test benchmark paths
    p_config = Path("test/benchmark1/benchmark1.yml")
    p_forcing = Path("test/benchmark1/forcing/Kc1_2011_data_5.txt")
    
    if not p_config.exists():
        print(f"✗ Config file not found: {p_config}")
        return False
    
    if not p_forcing.exists():
        print(f"✗ Forcing file not found: {p_forcing}")
        return False
    
    print(f"✓ Config file found: {p_config}")
    print(f"✓ Forcing file found: {p_forcing}")
    
    # Load data
    print("\nLoading data...")
    try:
        df_forcing = sp.load_forcing_df(p_forcing)
        print(f"✓ Forcing data loaded: {df_forcing.shape}")
        
        df_state_init = sp.init_supy(p_config)
        print(f"✓ Initial state loaded: {df_state_init.shape}")
        
        # Run simulation for fewer days to speed up testing
        n_days = 10
        df_forcing_part = df_forcing.iloc[:288 * n_days]
        print(f"✓ Using {n_days} days of data: {df_forcing_part.shape}")
        
        # Run the simulation
        print("\nRunning SUEWS simulation...")
        df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)
        print(f"✓ Simulation completed: output={df_output.shape}, state={df_state.shape}")
        
        # Water balance calculation
        print("\nPerforming water balance calculation...")
        
        # Get soilstore
        df_soilstore = df_output.loc[1, "debug"].filter(regex="^ss_.*_next$")
        ser_sfr_surf = df_state_init.sfr_surf.iloc[0]
        ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)
        
        print(f"✓ Soilstore calculated: {ser_soilstore.shape}")
        print(f"  Soilstore dtype: {ser_soilstore.dtype}")
        print(f"  Sample values: {ser_soilstore.head()}")
        
        # Get water balance components
        df_water = df_output.SUEWS[["Rain", "Irr", "Evap", "RO", "State"]].assign(
            SoilStore=ser_soilstore,
            TotalStore=ser_soilstore + df_output.SUEWS.State
        )
        
        print(f"✓ Water balance DataFrame created: {df_water.shape}")
        for col in df_water.columns:
            print(f"  {col}: dtype={df_water[col].dtype}, nulls={df_water[col].isnull().sum()}")
        
        # Calculate water balance
        print("\nCalculating water balance...")
        
        # Change in total store
        ser_totalstore_change = df_water.TotalStore.diff().dropna()
        print(f"✓ TotalStore change: shape={ser_totalstore_change.shape}, dtype={ser_totalstore_change.dtype}")
        
        # Water input and output
        ser_water_in = df_water.Rain + df_water.Irr
        ser_water_out = df_water.Evap + df_water.RO
        ser_water_balance = ser_water_in - ser_water_out
        
        print(f"✓ Water in: dtype={ser_water_in.dtype}, nulls={ser_water_in.isnull().sum()}")
        print(f"✓ Water out: dtype={ser_water_out.dtype}, nulls={ser_water_out.isnull().sum()}")
        print(f"✓ Water balance: dtype={ser_water_balance.dtype}, nulls={ser_water_balance.isnull().sum()}")
        
        # Final calculation
        print("\nFinal balance calculation...")
        try:
            # Align the series (totalstore_change starts from index 1)
            ser_water_balance_aligned = ser_water_balance.iloc[1:]
            
            print(f"✓ Aligned series:")
            print(f"  TotalStore change: {ser_totalstore_change.shape}, dtype={ser_totalstore_change.dtype}")
            print(f"  Water balance: {ser_water_balance_aligned.shape}, dtype={ser_water_balance_aligned.dtype}")
            
            # Calculate difference
            diff_series = ser_totalstore_change - ser_water_balance_aligned
            print(f"✓ Difference series: shape={diff_series.shape}, dtype={diff_series.dtype}")
            print(f"  Null count: {diff_series.isnull().sum()}")
            print(f"  Sample values: {diff_series.head()}")
            
            # Check for issues
            if diff_series.dtype == 'object':
                print("⚠️  WARNING: Difference series has object dtype!")
                print("  This indicates a data type mismatch in the calculation.")
                return False
            
            if diff_series.isnull().all():
                print("⚠️  WARNING: All difference values are null!")
                return False
            
            # Calculate max difference
            max_diff = diff_series.abs().max()
            print(f"✓ Max difference: {max_diff}")
            
            # Test result
            test_result = max_diff < 1e-6
            print(f"✓ Test result: {test_result} (max_diff={max_diff:.2e} vs threshold=1e-6)")
            
            if not test_result:
                print("⚠️  Water balance test failed!")
                print("  Largest differences:")
                print(diff_series.abs().nlargest(10))
            
            return test_result
            
        except Exception as e:
            print(f"✗ Error in final calculation: {e}")
            print(f"  Error type: {type(e)}")
            return False
    
    except Exception as e:
        print(f"✗ Error in water balance test: {e}")
        print(f"  Error type: {type(e)}")
        return False

if __name__ == "__main__":
    success = test_water_balance_direct()
    if success:
        print("\n✅ Water balance test PASSED!")
        sys.exit(0)
    else:
        print("\n❌ Water balance test FAILED!")
        sys.exit(1)