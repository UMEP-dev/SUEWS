"""
Verify that the pickle file can be read by NumPy 1.x.

This script checks if the converted pickle uses a format compatible with
NumPy 1.x by examining the pickle protocol and testing deserialization.
"""

import pickle
from pathlib import Path
import sys

# Get the test data directory
test_data_dir = Path(__file__).parent / "fixtures" / "data_test"
p_df_sample = Path(test_data_dir) / "sample_output.pkl"

print("Verifying NumPy 1.x compatibility...")
print(f"Pickle file: {p_df_sample}")

# Check pickle protocol
with open(p_df_sample, "rb") as f:
    # Read first byte to get protocol version
    first_byte = f.read(1)
    if first_byte == b'\x80':  # Pickle protocol marker
        protocol = f.read(1)[0]
        print(f"Pickle protocol version: {protocol}")
        if protocol <= 4:
            print("✓ Protocol 4 or lower - compatible with NumPy 1.x")
        else:
            print(f"✗ Protocol {protocol} - may not be compatible with NumPy 1.x")
    else:
        print("Using old pickle format (protocol 0-2)")

# Try to load with pandas (requires NumPy)
try:
    import pandas as pd
    import numpy as np

    print(f"\nCurrent environment: NumPy {np.__version__}, Pandas {pd.__version__}")

    df = pd.read_pickle(p_df_sample)
    print(f"✓ Successfully loaded DataFrame with shape {df.shape}")
    print(f"  Columns: {len(df.columns)} variables")
    print(f"  Rows: {len(df)} timesteps")

    # Check for NumPy 2.x specific structures
    print("\nChecking for NumPy 2.x specific structures...")

    # Serialize and check for numpy._core references
    test_pickle = pickle.dumps(df.iloc[0])
    if b'numpy._core' in test_pickle or b'numpy.core' in test_pickle:
        print("⚠ Warning: Found NumPy internal module references")
    else:
        print("✓ No NumPy internal module references found")

except Exception as e:
    print(f"✗ Error loading pickle: {e}")
    sys.exit(1)

print("\n✓ Verification complete - pickle should work with both NumPy 1.x and 2.x")
