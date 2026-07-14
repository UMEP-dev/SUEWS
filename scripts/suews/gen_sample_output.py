"""Regenerate the one-year reference output used by the sample-output tests.

Run from anywhere (the Makefile target `update_sample_output` runs it from test/):

    python scripts/suews/gen_sample_output.py

The previous fixture is not copied aside: it is tracked in git, so `git diff` shows
what moved and `git checkout` restores it. Writing a timestamped 55 MB copy next to
it only risked that copy being committed by accident.
"""

from pathlib import Path
import supy as sp

# Resolve the fixture directory from the repository root, not from this file's own
# directory: the fixtures live under test/, while this script lives under scripts/suews/.
repo_root = Path(__file__).resolve().parents[2]
test_data_dir = repo_root / "test" / "fixtures" / "data_test"
if not test_data_dir.is_dir():
    raise SystemExit(f"[X] fixture directory not found: {test_data_dir}")

# Construct the file path for the data file (using CSV for transparency)
p_df_sample = test_data_dir / "sample_output.csv.gz"

print("\n========================================")
print("Generating sample output for testing")
df_state_init, df_forcing_tstep = sp.load_SampleData()
df_forcing_part = df_forcing_tstep.iloc[: 288 * 366]  # One year (2012 is a leap year)

# single-step results
df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

# Save as CSV with gzip compression (transparent format, no NumPy version issues).
# Pin the gzip mtime header: gzip stamps the current time by default, so regenerating
# an unchanged reference still rewrites all 55 MB of it and shows up as a diff. With
# mtime=0 the bytes depend only on the results, so `git diff` on this fixture means
# the model output actually moved.
print(f"Saving sample output to: {p_df_sample.as_posix()}")
df_output_s.SUEWS.to_csv(
    p_df_sample, compression={"method": "gzip", "mtime": 0}
)
print(
    f"[OK] Saved {df_output_s.SUEWS.shape[0]} rows x {df_output_s.SUEWS.shape[1]} columns"
)
