"""The sample run must reproduce the stored reference digit for digit.

``test_sample_output.py`` compares the full-year run with the reference within
tolerances sized for cross-platform noise (0.8% on the fluxes). A physics
change can move every output by much less than that and still be a real
change: #1848 (273.16 -> 273.15 K) moved QS by up to 0.08 W m-2 and was merged
without the reference being refreshed, so it went unseen until the next
refresh had to account for it. The tolerances cannot be tightened to catch
that on every platform, because macOS arm64 and Linux x86_64 builds of the
same commit differ in the last stored digit of about half the values.

This test closes the gap where it can be closed exactly: on the build that
generated the reference (same system, machine, C library, Fortran compiler
and build profile, as recorded in ``provenance.json``), the run is
deterministic, so its shards written at the stored precision (``%.7g``) must
equal the committed ones byte for byte. Any moved value fails, with the
columns that moved and how to regenerate.

Where it runs:

- In the CI lane that produces the reference (``SUEWS_REFERENCE_LANE=1``, set
  for the cp312 manylinux x86_64 release wheel), it always runs, and a
  reference generated anywhere else is itself a failure: otherwise a refresh
  made on another machine would switch the check off without anyone noticing.
- Anywhere else it runs when the build happens to match the reference and is
  skipped, with the reason, when it does not.

On a mismatch in CI the regenerated shards and ``provenance.json`` are written
under ``$SUEWS_CI_METRICS_DIR/reference-regenerated`` and uploaded as the
``reference-regenerated-*`` artefact, so a change that is meant to move the
output is refreshed by downloading that artefact rather than by reproducing
the CI build elsewhere. Refreshing the reference requires the
``0-physics:change`` label and its evidence section
(``.claude/rules/physics-change-evidence.md``); a CI job checks the label.
"""

import os
from pathlib import Path
import shutil
import sys
import tempfile

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "fixtures" / "data_test"))
from sample_output_io import (  # noqa: E402
    SHARD_GLOB,
    load_sample_output,
    read_reference_provenance,
    reference_environment_mismatch,
    run_sample_reference,
    write_reference_provenance,
    write_sample_output_shards,
)

pytestmark = pytest.mark.physics

REPO_ROOT = Path(__file__).resolve().parents[2]
LANE_ENV = "SUEWS_REFERENCE_LANE"
ARTEFACT_DIRNAME = "reference-regenerated"
MAX_COLUMNS_REPORTED = 20

REFRESH_HINT = (
    "If the change is meant to move the output, refresh the reference in the "
    "same PR: download this CI run's 'reference-regenerated-cp312-manylinux-"
    "x86_64' artefact and copy its files into test/fixtures/data_test/ (or run "
    "'python scripts/suews/gen_sample_output.py' in the build that generated "
    "the reference), commit the shards with provenance.json, label the PR "
    "0-physics:change and add its Scientific evidence section "
    "(.claude/rules/physics-change-evidence.md)."
)


def _normalised_bytes(path: Path) -> bytes:
    """File bytes with CRLF folded to LF, as the provenance sidecar hashes them."""
    return path.read_bytes().replace(b"\r\n", b"\n")


def _moved_shards(regenerated_dir: Path, reference_dir: Path) -> list[str]:
    """Names of shards whose bytes differ, or that exist on one side only."""
    regenerated = {p.name: p for p in regenerated_dir.glob(SHARD_GLOB)}
    reference = {p.name: p for p in reference_dir.glob(SHARD_GLOB)}
    return sorted(
        name
        for name in regenerated.keys() | reference.keys()
        if name not in regenerated
        or name not in reference
        or _normalised_bytes(regenerated[name]) != _normalised_bytes(reference[name])
    )


def _moved_columns_report(regenerated_dir: Path, reference_dir: Path) -> str:
    """Per-column summary of the stored values that moved, largest change first."""
    df_new = load_sample_output(regenerated_dir)
    df_ref = load_sample_output(reference_dir)
    if list(df_new.columns) != list(df_ref.columns):
        return (
            "columns differ: only in the run "
            f"{sorted(set(df_new.columns) - set(df_ref.columns))}, only in the "
            f"reference {sorted(set(df_ref.columns) - set(df_new.columns))}"
        )
    datetimes = df_ref.index.get_level_values("datetime")
    rows = []
    for column in df_ref.columns:
        new = df_new[column].to_numpy(dtype=float)
        ref = df_ref[column].to_numpy(dtype=float)
        moved = ~((new == ref) | (np.isnan(new) & np.isnan(ref)))
        if not moved.any():
            continue
        change = np.abs(new - ref)[moved]
        rows.append(
            (
                column,
                int(moved.sum()),
                float(np.nanmax(change)) if np.isfinite(change).any() else float("nan"),
                datetimes[np.argmax(moved)],
            )
        )
    rows.sort(key=lambda row: -row[2] if np.isfinite(row[2]) else 0.0)
    total = df_ref.size
    n_moved = sum(row[1] for row in rows)
    lines = [
        f"{n_moved} of {total} stored values moved, in {len(rows)} of "
        f"{df_ref.shape[1]} columns (largest change first):"
    ]
    for column, count, largest, first in rows[:MAX_COLUMNS_REPORTED]:
        lines.append(
            f"  {column}: {count} values, max |change| {largest:.4g}, first at {first}"
        )
    if len(rows) > MAX_COLUMNS_REPORTED:
        lines.append(f"  ... and {len(rows) - MAX_COLUMNS_REPORTED} more columns")
    return "\n".join(lines)


def _export_regenerated(regenerated_dir: Path, df) -> Path | None:
    """Copy the regenerated shards and a fresh sidecar to the CI artefact directory."""
    metrics_dir = os.environ.get("SUEWS_CI_METRICS_DIR")
    if not metrics_dir:
        return None
    destination = Path(metrics_dir) / ARTEFACT_DIRNAME
    destination.mkdir(parents=True, exist_ok=True)
    for shard in regenerated_dir.glob(SHARD_GLOB):
        shutil.copy2(shard, destination / shard.name)
    write_reference_provenance(df, destination, REPO_ROOT)
    return destination


@pytest.mark.core
@pytest.mark.medium
def test_sample_run_reproduces_reference_exactly(sample_reference_dir):
    """The full-year sample run matches the stored shards byte for byte."""
    # ARRANGE: decide whether this build can reproduce the reference exactly
    provenance = read_reference_provenance(sample_reference_dir)
    mismatch = reference_environment_mismatch(provenance, REPO_ROOT)
    in_reference_lane = os.environ.get(LANE_ENV) == "1"
    if mismatch and not in_reference_lane:
        pytest.skip(
            "exact reference check needs the build that generated the "
            f"reference: {mismatch}"
        )

    # ACT: run the year and write it exactly as the generator would
    df_run = run_sample_reference()
    with tempfile.TemporaryDirectory() as tmp:
        regenerated_dir = Path(tmp)
        write_sample_output_shards(df_run, regenerated_dir)
        moved = _moved_shards(regenerated_dir, sample_reference_dir)
        exported = (
            _export_regenerated(regenerated_dir, df_run) if (mismatch or moved) else None
        )
        report = _moved_columns_report(regenerated_dir, sample_reference_dir) if moved else ""

    # ASSERT
    where = f" Regenerated files: {exported}." if exported else ""
    assert not mismatch, (
        "This is the CI lane that the sample-output reference must come from, "
        f"but the reference was generated elsewhere: {mismatch}. Replace "
        "test/fixtures/data_test/sample_output_2012-*.csv and provenance.json "
        "with this run's 'reference-regenerated-cp312-manylinux-x86_64' "
        f"artefact.{where}"
    )
    assert not moved, (
        f"The sample run no longer reproduces the stored reference "
        f"(shards {', '.join(moved)}).\n{report}\n{REFRESH_HINT}{where}"
    )
