#!/usr/bin/env python3
"""Test df_state conversion with the provided CSV file."""

import sys
import tempfile
from pathlib import Path

# Add src to path
sys.path.insert(0, 'src')

# Import converter functions
from supy.util.converter import (
    detect_input_type,
    load_df_state_file,
    detect_df_state_version,
    convert_df_state_format,
    convert_to_yaml,
)
from supy.util._df_state_converter import validate_converted_df_state

def test_conversion():
    """Test the complete conversion flow."""
    
    # Input and output files
    csv_path = Path('/Users/tingsun/Library/Application Support/com.conductor.app/uploads/originals/932628f2-d84e-48d2-84b2-fd2c2d81c49b.csv')
    
    print("=" * 60)
    print("Testing df_state Conversion")
    print("=" * 60)
    
    # 1. Detect input type
    print("\n1. Detecting input type...")
    input_type = detect_input_type(csv_path)
    print(f"   ✓ Input type: {input_type}")
    assert input_type == 'df_state', "Should detect as df_state"
    
    # 2. Load df_state
    print("\n2. Loading df_state...")
    df_old = load_df_state_file(csv_path)
    print(f"   ✓ Loaded: {df_old.shape[0]} grids, {len(df_old.columns)} columns")
    
    # 3. Detect version
    print("\n3. Detecting df_state version...")
    version = detect_df_state_version(df_old)
    print(f"   ✓ Version: {version}")
    assert version == 'old', "Should detect as old format"
    
    # 4. Convert format
    print("\n4. Converting to new format...")
    df_new = convert_df_state_format(df_old)
    print(f"   ✓ Converted: {df_new.shape[0]} grids, {len(df_new.columns)} columns")
    
    # Check columns were removed/added
    old_cols = {col[0] if isinstance(col, tuple) else col for col in df_old.columns}
    new_cols = {col[0] if isinstance(col, tuple) else col for col in df_new.columns}
    
    removed = ['age_0_4', 'age_5_11', 'age_12_18', 'age_19_64', 'age_65plus', 'hhs0']
    added = ['buildingname', 'buildingtype', 'config', 'description', 'h_std', 'lambda_c', 'n_buildings']
    
    for col in removed:
        assert col in old_cols, f"Old format should have {col}"
        assert col not in new_cols, f"New format should not have {col}"
    
    for col in added:
        assert col not in old_cols, f"Old format should not have {col}"
        assert col in new_cols, f"New format should have {col}"
    
    print(f"   ✓ Removed {len(removed)} deprecated columns")
    print(f"   ✓ Added {len(added)} new columns")
    
    # 5. Validate
    print("\n5. Validating converted df_state...")
    is_valid, message = validate_converted_df_state(df_new)
    print(f"   {'✓' if is_valid else '⚠'} {message}")
    
    # 6. Test YAML conversion
    print("\n6. Testing YAML conversion...")
    with tempfile.NamedTemporaryFile(suffix='.yml', delete=False) as tmp:
        output_path = Path(tmp.name)
    
    try:
        convert_to_yaml(
            input_file=str(csv_path),
            output_file=str(output_path),
        )
        
        if output_path.exists():
            print(f"   ✓ YAML created: {output_path}")
            print(f"   ✓ Size: {output_path.stat().st_size:,} bytes")
            
            # Try to load and validate YAML
            import yaml
            with open(output_path) as f:
                config = yaml.safe_load(f)
            
            assert 'sites' in config, "YAML should have sites"
            assert len(config['sites']) == 1, "Should have 1 site"
            print(f"   ✓ YAML structure valid")
        else:
            print(f"   ✗ Failed to create YAML")
    
    except Exception as e:
        print(f"   ✗ Error during YAML conversion: {e}")
    finally:
        # Clean up
        if output_path.exists():
            output_path.unlink()
    
    print("\n" + "=" * 60)
    print("✓ All tests passed!")
    print("=" * 60)

if __name__ == "__main__":
    test_conversion()