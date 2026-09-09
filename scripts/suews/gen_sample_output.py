"""Regenerate the one-year reference output used by the sample-output tests.

Run from anywhere (the Makefile target `update_sample_output` runs it from test/):

    python scripts/suews/gen_sample_output.py

The reference is stored as twelve plain-CSV monthly shards
(`sample_output_2012-MM.csv`) rather than a single gzipped blob: plain CSV is
diff-able, so a physics change shows up line by line, each shard stays under
the repository's pre-commit size limit, and there is no gzip timestamp header to
pin -- the fixture is byte-reproducible by construction.

Plain CSV does not, on its own, make a refresh cheap in history: both refreshes
before this one stored twelve fresh blobs with no delta base, because a physics
change moves nearly every float in its last digits. Writing at seven
significant figures is what bounds that cost. See
`test/fixtures/data_test/sample_output_io.py` for the split/combine convention
and the precision justification.

Also writes `provenance.json` beside the shards, recording the SuPy build,
compiler, platform and per-shard content hashes of this run. The sample-output
tests assert that sidecar against the shards they load, so a reference that is
refreshed without it fails loudly.

The previous shards are not copied aside: they are tracked in git, so `git diff`
shows what moved and `git checkout` restores them.
"""

from pathlib import Path
import sys

import numpy as np

import supy as sp

# Resolve the fixture directory from the repository root, not from this file's own
# directory: the fixtures live under test/, while this script lives under scripts/suews/.
repo_root = Path(__file__).resolve().parents[2]
test_data_dir = repo_root / "test" / "fixtures" / "data_test"
if not test_data_dir.is_dir():
    raise SystemExit(f"[X] fixture directory not found: {test_data_dir}")

# Share the split/combine convention with the tests (single source of truth).
sys.path.insert(0, str(test_data_dir))
from sample_output_io import (  # noqa: E402
    REFERENCE_FLOAT_FORMAT,
    load_sample_output,
    write_reference_provenance,
    write_sample_output_shards,
)

# Worst-case relative error of a %.7g write: half a unit in the seventh
# significant digit, at a leading mantissa digit of 1. The round-trip check
# below allows exactly this and no more, so a genuine split/combine
# inconsistency still fails.
ROUND_TRIP_RTOL = 5e-7

print("\n========================================")
print("Generating sample output for testing")
simulation = sp.SUEWSSimulation.from_sample_data()
df_forcing_tstep = simulation.forcing.df
df_forcing_part = df_forcing_tstep.iloc[: 288 * 366]  # One year (2012 is a leap year)

# single-step results
simulation.update_forcing(df_forcing_part)
df_output_s = simulation.run().df

df_output = df_output_s.SUEWS
print(f"Saving sample output shards to: {test_data_dir.as_posix()}")
written = write_sample_output_shards(df_output, test_data_dir)

# Self-check: the shards must reconstruct what we just wrote to within the write
# precision, so the generator and the test loader can never silently disagree.
# This cannot be an equality check any more: the shards are written at
# REFERENCE_FLOAT_FORMAT, so a reloaded float differs from the in-memory double
# in the digits beyond the seventh by construction.
reloaded = load_sample_output(test_data_dir)
if not reloaded.index.equals(df_output.index):
    raise SystemExit(
        "[X] round-trip check failed: reloaded shards have a different index "
        "from the generated output -- the split/combine convention is inconsistent"
    )
if list(reloaded.columns) != list(df_output.columns):
    raise SystemExit(
        "[X] round-trip check failed: reloaded shards have different columns "
        "from the generated output -- the split/combine convention is inconsistent"
    )
if not np.allclose(
    reloaded.to_numpy(dtype=float),
    df_output.to_numpy(dtype=float),
    rtol=ROUND_TRIP_RTOL,
    atol=0.0,
    equal_nan=True,
):
    raise SystemExit(
        "[X] round-trip check failed: reloaded shards differ from the generated "
        f"output by more than the write precision ({REFERENCE_FLOAT_FORMAT}, "
        f"rtol={ROUND_TRIP_RTOL})"
    )

path_provenance = write_reference_provenance(df_output, test_data_dir, repo_root)
print(f"Wrote provenance sidecar: {path_provenance.name}")

print(
    f"[OK] Saved {df_output.shape[0]} rows x {df_output.shape[1]} columns "
    f"across {len(written)} monthly shards at {REFERENCE_FLOAT_FORMAT} "
    f"(round-trip verified)"
)
