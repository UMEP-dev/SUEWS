#!/usr/bin/env python
"""
Test if MultiIndex extraction behaves differently
"""
import pandas as pd
import numpy as np
import supy as sp

# Load sample data
df_state_init, df_forcing_tstep = sp.load_SampleData()

# Run short simulation
df_forcing_part = df_forcing_tstep.iloc[:288 * 5]  # Just 5 days
df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

grid_id = 1

print("=== Checking df_output structure ===")
print(f"df_output index type: {type(df_output.index)}")
print(f"df_output index names: {df_output.index.names}")
print(f"df_output columns type: {type(df_output.columns)}")
print(f"df_output columns names: {df_output.columns.names}")

print("\n=== Different ways to extract SUEWS data ===")

# Method 1: Using .loc with tuple
print("\nMethod 1: df_output.loc[(grid_id, slice(None)), 'SUEWS']")
try:
    df_suews1 = df_output.loc[(grid_id, slice(None)), "SUEWS"]
    print(f"  Result index type: {type(df_suews1.index)}")
    print(f"  Result shape: {df_suews1.shape}")
    print(f"  Index sample: {df_suews1.index[:3]}")
except Exception as e:
    print(f"  ERROR: {e}")

# Method 2: Using .loc[grid_id, "SUEWS"]
print("\nMethod 2: df_output.loc[grid_id, 'SUEWS']")
try:
    df_suews2 = df_output.loc[grid_id, "SUEWS"]
    print(f"  Result index type: {type(df_suews2.index)}")
    print(f"  Result shape: {df_suews2.shape}")
    print(f"  Index sample: {df_suews2.index[:3]}")
except Exception as e:
    print(f"  ERROR: {e}")

# Method 3: Using xs
print("\nMethod 3: df_output.xs(grid_id, level='grid')['SUEWS']")
try:
    df_suews3 = df_output.xs(grid_id, level='grid')['SUEWS']
    print(f"  Result index type: {type(df_suews3.index)}")
    print(f"  Result shape: {df_suews3.shape}")
    print(f"  Index sample: {df_suews3.index[:3]}")
except Exception as e:
    print(f"  ERROR: {e}")

print("\n=== Testing water balance components extraction ===")
water_columns = ["Rain", "Irr", "Evap", "RO", "State"]
df_suews_water = df_output.loc[grid_id, "SUEWS"][water_columns]
print(f"df_suews_water index type: {type(df_suews_water.index)}")
print(f"df_suews_water shape: {df_suews_water.shape}")

# Now test soilstore
df_soilstore = df_output.loc[grid_id, "debug"].filter(regex="^ss_.*_next$")
print(f"\ndf_soilstore index type: {type(df_soilstore.index)}")
print(f"df_soilstore shape: {df_soilstore.shape}")

# Test if assign would cause issues
print("\n=== Testing DataFrame.assign behavior ===")
test_series = pd.Series(np.random.rand(len(df_suews_water)), index=df_suews_water.index)
print(f"test_series index matches df_suews_water: {test_series.index.equals(df_suews_water.index)}")

# Test assign
df_test = df_suews_water.assign(TestCol=test_series.values)
print(f"After assign, has NaN: {df_test['TestCol'].isnull().any()}")

# Test with different index
test_series2 = pd.Series(np.random.rand(len(df_suews_water)), index=range(len(df_suews_water)))
df_test2 = df_suews_water.assign(TestCol2=test_series2.values)
print(f"After assign with different index source, has NaN: {df_test2['TestCol2'].isnull().any()}")