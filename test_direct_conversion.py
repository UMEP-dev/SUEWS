#!/usr/bin/env python3
"""Direct test of df_state conversion without full supy import."""

import sys
import os
import tempfile
from pathlib import Path

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

# Test the conversion with minimal imports
def test_direct():
    """Test conversion directly."""
    
    # Only import what we need
    import pandas as pd
    import click
    
    csv_path = Path('/Users/tingsun/Library/Application Support/com.conductor.app/uploads/originals/932628f2-d84e-48d2-84b2-fd2c2d81c49b.csv')
    
    print("Testing direct df_state conversion...")
    print("=" * 60)
    
    # Load the CSV
    df = pd.read_csv(csv_path, header=[0, 1], index_col=0)
    print(f"✓ Loaded CSV: {df.shape}")
    
    # Check format
    col_names = {col[0] if isinstance(col, tuple) else col for col in df.columns}
    if 'age_0_4' in col_names:
        print("✓ Detected old format")
    
    # Create a temporary output file
    with tempfile.NamedTemporaryFile(suffix='.yml', delete=False) as tmp:
        output_path = tmp.name
    
    print(f"Output will be: {output_path}")
    
    # Now test the actual conversion using the CLI
    print("\nTesting CLI conversion...")
    import subprocess
    
    cmd = [
        sys.executable, '-m', 'supy.cmd.table_converter',
        '-i', str(csv_path),
        '-o', output_path
    ]
    
    print(f"Command: {' '.join(cmd)}")
    
    # Note: This will fail due to import issues, but shows the intended usage
    result = subprocess.run(cmd, capture_output=True, text=True, cwd='src')
    
    if result.returncode == 0:
        print("✓ Conversion successful!")
        print(f"Output: {output_path}")
    else:
        print("✗ Conversion failed")
        print(f"Error: {result.stderr}")
    
    # Clean up
    if Path(output_path).exists():
        Path(output_path).unlink()
    
    print("=" * 60)

if __name__ == "__main__":
    test_direct()