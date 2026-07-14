"""Regenerate the one-year reference output used by the sample-output tests.

Run from anywhere (the Makefile target `update_sample_output` runs it from test/):

    python scripts/suews/gen_sample_output.py

The reference is stored as twelve plain-CSV monthly shards
(`sample_output_2012-MM.csv`) rather than a single gzipped blob: plain CSV is
diff-able and delta-compressed by git across revisions, each shard stays under
the repository's pre-commit size limit, and there is no gzip timestamp header to
pin -- the fixture is byte-reproducible by construction. See
`test/fixtures/data_test/sample_output_io.py` for the split/combine convention.

The previous shards are not copied aside: they are tracked in git, so `git diff`
shows what moved and `git checkout` restores them.
"""

import sys
from pathlib import Path

import supy as sp

# Resolve the fixture directory from the repository root, not from this file's own
# directory: the fixtures live under test/, while this script lives under scripts/suews/.
repo_root = Path(__file__).resolve().parents[2]
test_data_dir = repo_root / "test" / "fixtures" / "data_test"
if not test_data_dir.is_dir():
    raise SystemExit(f"[X] fixture directory not found: {test_data_dir}")

# Share the split/combine convention with the tests (single source of truth).
sys.path.insert(0, str(test_data_dir))
from sample_output_io import load_sample_output, write_sample_output_shards  # noqa: E402

print("\n========================================")
print("Generating sample output for testing")
df_state_init, df_forcing_tstep = sp.load_SampleData()
df_forcing_part = df_forcing_tstep.iloc[: 288 * 366]  # One year (2012 is a leap year)

# single-step results
df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

df_output = df_output_s.SUEWS
print(f"Saving sample output shards to: {test_data_dir.as_posix()}")
written = write_sample_output_shards(df_output, test_data_dir)

# Self-check: the shards must reconstruct exactly what we just wrote, so the
# generator and the test loader can never silently disagree.
reloaded = load_sample_output(test_data_dir)
if not reloaded.equals(df_output):
    raise SystemExit(
        "[X] round-trip check failed: reloaded shards differ from the generated "
        "output -- the split/combine convention is inconsistent"
    )

print(
    f"[OK] Saved {df_output.shape[0]} rows x {df_output.shape[1]} columns "
    f"across {len(written)} monthly shards (round-trip verified)"
)
