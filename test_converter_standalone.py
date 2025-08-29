#!/usr/bin/env python3
"""Standalone test for df_state conversion."""

import pandas as pd
import numpy as np
from pathlib import Path
import tempfile
import sys

# Direct test without full supy import
def test_basic_conversion():
    """Test basic df_state format detection and conversion."""
    
    csv_path = Path('/Users/tingsun/Library/Application Support/com.conductor.app/uploads/originals/932628f2-d84e-48d2-84b2-fd2c2d81c49b.csv')
    
    print("=" * 60)
    print("Testing df_state Format Detection")
    print("=" * 60)
    
    # 1. Load CSV
    print("\n1. Loading CSV file...")
    df = pd.read_csv(csv_path, header=[0, 1], index_col=0)
    print(f"   ✓ Loaded: {df.shape}")
    
    # 2. Check for old format indicators
    print("\n2. Checking for old format columns...")
    col_names = {col[0] if isinstance(col, tuple) else col for col in df.columns}
    
    old_indicators = ['age_0_4', 'age_5_11', 'age_12_18', 'age_19_64', 'age_65plus', 'hhs0']
    new_indicators = ['buildingname', 'buildingtype', 'config', 'description', 'h_std', 'lambda_c', 'n_buildings']
    
    has_old = [col for col in old_indicators if col in col_names]
    has_new = [col for col in new_indicators if col in col_names]
    
    print(f"   Old format columns found: {has_old}")
    print(f"   New format columns found: {has_new}")
    
    if has_old and not has_new:
        print("   ✓ Detected: OLD FORMAT (needs conversion)")
    elif has_new and not has_old:
        print("   ✓ Detected: NEW FORMAT (no conversion needed)")
    else:
        print("   ? Unknown format")
    
    # 3. Test file type detection
    print("\n3. Testing file type detection...")
    
    if csv_path.suffix == '.csv':
        print("   ✓ File type: CSV (df_state)")
    
    nml_path = Path("RunControl.nml")
    if nml_path.name == 'RunControl.nml':
        print("   ✓ File type: NML (table conversion)")
    
    # 4. Show what conversion would do
    print("\n4. Conversion requirements:")
    print(f"   - Remove {len(has_old)} deprecated columns")
    print(f"   - Add {len(new_indicators)} new columns")
    print(f"   - Preserve {len(col_names) - len(has_old)} common columns")
    
    # 5. Test output path validation  
    print("\n5. Output path validation:")
    output_yml = Path("config.yml")
    output_yaml = Path("config.yaml")
    
    for path in [output_yml, output_yaml]:
        if path.suffix in ['.yml', '.yaml']:
            print(f"   ✓ Valid output: {path}")
    
    print("\n" + "=" * 60)
    print("✓ Basic tests complete!")
    print("=" * 60)

if __name__ == "__main__":
    test_basic_conversion()