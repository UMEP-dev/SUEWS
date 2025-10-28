"""
Convert NumPy 2.x pickle to NumPy 1.x compatible format.

This script loads the existing sample_output.pkl (created with NumPy 2.x)
and saves it using pickle protocol 4, which is compatible with NumPy 1.x.

The key is that NumPy 2.x can read NumPy 1.x pickles (forward compatible),
so we regenerate the pickle to ensure both versions can read it.
"""

from datetime import datetime
from pathlib import Path
import pandas as pd
import numpy as np

print(f"NumPy version: {np.__version__}")

# Get the test data directory
test_data_dir = Path(__file__).parent / "fixtures" / "data_test"
p_df_sample = Path(test_data_dir) / "sample_output.pkl"

# Backup existing file with timestamp
if p_df_sample.exists():
    dt_stamp = datetime.now()
    str_stamp = dt_stamp.strftime("%Y%m%d_%H%M%S")
    p_df_sample_backup = p_df_sample.with_suffix(f".numpy2x_backup_{str_stamp}.pkl")
    print(f"Backing up existing file to: {p_df_sample_backup.name}")
    p_df_sample.rename(p_df_sample_backup)
else:
    print("ERROR: sample_output.pkl not found!")
    exit(1)

# Load with NumPy 2.x
print(f"Loading pickle with NumPy {np.__version__}...")
df = pd.read_pickle(p_df_sample_backup)
print(f"Loaded DataFrame with shape: {df.shape}")
print(f"Columns: {list(df.columns)}")

# Save with explicit protocol 4 and convert to pure NumPy arrays
# This ensures no NumPy 2.x specific internal structures are preserved
print(f"Saving pickle with protocol 4 for NumPy 1.x compatibility...")

# Convert DataFrame to dict of arrays to avoid pandas/numpy internal structures
data_dict = {}
for col in df.columns:
    # Convert to plain numpy array and ensure it's a copy
    arr = np.array(df[col].values, copy=True)
    data_dict[col] = arr

# Recreate DataFrame from plain arrays
df_clean = pd.DataFrame(data_dict, index=df.index.copy())

# Save with protocol 4
df_clean.to_pickle(p_df_sample, protocol=4)
print(f"Saved to: {p_df_sample}")

# Verify the saved file can be loaded
print("Verifying saved file...")
df_verify = pd.read_pickle(p_df_sample)
assert df.shape == df_verify.shape, "Shape mismatch after reload"
print("Verification successful!")

print(
    "\nConversion complete. The new pickle file should work with both NumPy 1.x and 2.x"
)
