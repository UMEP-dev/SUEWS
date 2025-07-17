#!/usr/bin/env python3
"""
Analyze the benchmark data to understand the water balance test failure.
This script examines the data files without running SUEWS to identify potential issues.
"""

import pandas as pd
import numpy as np
import yaml
from pathlib import Path

def analyze_benchmark_data():
    """Analyze the benchmark data files to understand the water balance issue."""
    
    print("="*60)
    print("BENCHMARK DATA ANALYSIS")
    print("="*60)
    
    # Check files exist
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
    
    # Read configuration
    print("\nAnalyzing configuration...")
    with open(p_config, 'r') as f:
        config = yaml.safe_load(f)
    
    print(f"✓ Configuration loaded")
    print(f"  Keys: {list(config.keys())}")
    
    # Read forcing data
    print("\nAnalyzing forcing data...")
    try:
        # Try to read the forcing file
        df_forcing = pd.read_csv(p_forcing, sep=r'\s+', header=None)
        print(f"✓ Forcing data loaded: {df_forcing.shape}")
        print(f"  Columns: {df_forcing.shape[1]}")
        print(f"  Data types: {df_forcing.dtypes.value_counts()}")
        
        # Check for problematic values
        print("\nChecking for problematic values...")
        for col in df_forcing.columns:
            col_data = df_forcing[col]
            print(f"  Column {col}:")
            print(f"    dtype: {col_data.dtype}")
            print(f"    nulls: {col_data.isnull().sum()}")
            print(f"    infs: {np.isinf(col_data).sum() if col_data.dtype.kind in 'biufc' else 'N/A'}")
            print(f"    range: {col_data.min():.3f} to {col_data.max():.3f}" if col_data.dtype.kind in 'biufc' else f"    unique: {col_data.nunique()}")
            
            # Check for object dtype in numeric columns
            if col_data.dtype == 'object':
                print(f"    ⚠️  WARNING: Column {col} has object dtype!")
                print(f"    Sample values: {col_data.head()}")
        
        # Look for columns that should be numeric but aren't
        print("\nChecking for type conversion issues...")
        for col in df_forcing.columns:
            if df_forcing[col].dtype == 'object':
                print(f"  Column {col} is object dtype - trying to convert to numeric...")
                try:
                    numeric_col = pd.to_numeric(df_forcing[col], errors='coerce')
                    if numeric_col.isnull().sum() > 0:
                        print(f"    ⚠️  {numeric_col.isnull().sum()} values couldn't be converted!")
                        non_numeric = df_forcing[col][numeric_col.isnull()]
                        print(f"    Non-numeric values: {non_numeric.unique()}")
                    else:
                        print(f"    ✓ All values can be converted to numeric")
                except Exception as e:
                    print(f"    ✗ Conversion failed: {e}")
        
        # Check for specific patterns that might cause issues
        print("\nChecking for specific patterns...")
        
        # Look for mixed numeric/string values
        for col in df_forcing.columns:
            if df_forcing[col].dtype == 'object':
                values = df_forcing[col].astype(str)
                numeric_pattern = values.str.match(r'^-?\d+\.?\d*$')
                if not numeric_pattern.all():
                    print(f"  Column {col} has mixed data types:")
                    non_numeric = values[~numeric_pattern]
                    print(f"    Non-numeric values: {non_numeric.unique()}")
        
        # Summary
        print("\nSUMMARY:")
        object_cols = df_forcing.select_dtypes(include=['object']).columns
        if len(object_cols) > 0:
            print(f"⚠️  {len(object_cols)} columns have object dtype: {list(object_cols)}")
            print("  This could cause arithmetic operations to return object dtype results.")
            return False
        else:
            print("✓ All columns have appropriate numeric dtypes")
            
        # Check for extreme values that might cause overflow
        numeric_cols = df_forcing.select_dtypes(include=['number']).columns
        for col in numeric_cols:
            col_data = df_forcing[col]
            if col_data.abs().max() > 1e10:
                print(f"⚠️  Column {col} has very large values: max={col_data.abs().max():.2e}")
            if col_data.abs().min() < 1e-10 and col_data.abs().min() > 0:
                print(f"⚠️  Column {col} has very small values: min={col_data.abs().min():.2e}")
        
        return True
        
    except Exception as e:
        print(f"✗ Error reading forcing data: {e}")
        return False

if __name__ == "__main__":
    success = analyze_benchmark_data()
    if success:
        print("\n✅ Benchmark data analysis completed successfully!")
    else:
        print("\n❌ Benchmark data analysis found issues!")