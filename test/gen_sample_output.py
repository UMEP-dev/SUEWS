from datetime import datetime
from pathlib import Path
import supy as sp

# Get the test data directory from the environment variable
test_data_dir = Path(__file__).parent / "fixtures" / "data_test"
# test_data_dir = os.environ.get('TEST_DATA_DIR', Path(__file__).parent / 'fixtures' / 'data_test')

# Construct the file path for the data file (using CSV for transparency)
p_df_sample = Path(test_data_dir) / "sample_output.csv.gz"

# if file exists, rename it with a timestamp
if p_df_sample.exists():
    # generate a timestamp as of now
    dt_stamp = datetime.now()
    str_stamp = dt_stamp.strftime("%Y%m%d_%H%M%S")
    # rename the file
    print(f"Renaming existing file: {p_df_sample.as_posix()}")
    p_df_sample_save = p_df_sample.with_suffix(f".{str_stamp}.csv.gz")
    p_df_sample.rename(p_df_sample_save)
    print(f"Renamed to: {p_df_sample_save.as_posix()}")

print("\n========================================")
print("Generating sample output for testing")
df_state_init, df_forcing_tstep = sp.load_SampleData()
df_forcing_part = df_forcing_tstep.iloc[: 288 * 365]

# single-step results
df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

# Save as CSV with gzip compression (transparent format, no NumPy version issues)
print(f"Saving sample output to: {p_df_sample.as_posix()}")
df_output_s.SUEWS.to_csv(p_df_sample, compression="gzip")
print(
    f"✓ Saved {df_output_s.SUEWS.shape[0]} rows × {df_output_s.SUEWS.shape[1]} columns"
)
