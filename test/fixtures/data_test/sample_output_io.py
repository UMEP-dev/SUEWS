"""Shared read/write for the sharded sample-output reference fixture.

The one-year reference output (105,408 rows x 113 columns) used to live in a
single ``sample_output.csv.gz`` blob. Two problems with that shape:

- gzip is opaque to git, so every genuine physics change stored a fresh ~55 MB
  blob with no delta against the previous one, and ``git diff`` only ever said
  "binary files differ".
- a full-year *plain* CSV is ~156 MB, over the repository's pre-commit size
  limit.

The fixture is therefore stored as twelve plain-CSV monthly shards
(``sample_output_2012-MM.csv``). Each shard is ~13 MB -- under the size limit,
diff-able, and delta-compressed by git across revisions. Plain CSV also has no
timestamp header, so the fixture is byte-reproducible by construction (no gzip
``mtime`` to pin).

Both the generator (``scripts/suews/gen_sample_output.py``) and the tests import
this module so the split/combine convention has a single source of truth.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

# The reference is one leap-year at a 5-minute timestep. The final timestep is
# stamped at the start of the next interval (2013-01-01 00:00), so a naive
# month grouping would spill a single row into a 13th shard; that row is folded
# into the December shard to keep exactly twelve files.
SHARD_PREFIX = "sample_output_2012-"
SHARD_GLOB = "sample_output_2012-*.csv"
_LAST_MONTH = pd.Period("2012-12", "M")
EXPECTED_ROWS = 105408


def write_sample_output_shards(df: pd.DataFrame, data_dir: Path) -> list[Path]:
    """Write ``df`` as twelve monthly plain-CSV shards under ``data_dir``.

    ``df`` must carry the ``(grid, datetime)`` MultiIndex produced by
    ``SUEWSSimulation.run``. Row order within each month is preserved.
    """
    data_dir = Path(data_dir)
    datetimes = df.index.get_level_values("datetime")
    months = datetimes.to_period("M")
    # Fold the boundary timestep (2013-01) into the last real month.
    months = months.where(months <= _LAST_MONTH, _LAST_MONTH)

    written: list[Path] = []
    for period, shard in df.groupby(months, sort=True):
        path = data_dir / f"sample_output_{period}.csv"
        shard.to_csv(path)
        written.append(path)
    return written


def load_sample_output(data_dir: Path) -> pd.DataFrame:
    """Reconstruct the full reference frame from the monthly shards.

    Globs the shards, sorts by name (``YYYY-MM`` sorts chronologically),
    concatenates, and checks the result is the expected length with a unique
    index -- so a dropped or overlapping shard fails loudly rather than
    silently shortening the reference.
    """
    data_dir = Path(data_dir)
    shard_paths = sorted(data_dir.glob(SHARD_GLOB))
    if not shard_paths:
        raise FileNotFoundError(
            f"[X] no sample-output shards found under {data_dir} "
            f"(expected {SHARD_GLOB})"
        )

    frame = pd.concat(
        pd.read_csv(
            p,
            index_col=[0, 1],
            parse_dates=[1],
            float_precision="round_trip",
        )
        for p in shard_paths
    )

    if not frame.index.is_unique:
        raise ValueError(
            f"[X] reconstructed reference has duplicate index entries across "
            f"{len(shard_paths)} shards -- shards overlap"
        )
    if len(frame) != EXPECTED_ROWS:
        raise ValueError(
            f"[X] reconstructed reference has {len(frame)} rows, "
            f"expected {EXPECTED_ROWS} -- a shard is missing or truncated"
        )
    return frame
