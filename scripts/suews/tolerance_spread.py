#!/usr/bin/env python3
"""Measure the zero-tolerance spread of the full-year sample comparison.

The physics regression in ``test/core/test_sample_output.py`` compares the
engine's full-year run of the sample configuration against the vendored
reference and accepts each variable within a tolerance. Every tolerance there
is a bare number: nothing records the cross-platform and cross-CPython spread
those numbers are meant to absorb. This script records it.

``measure`` runs the same comparison with every tolerance set to zero and
writes a JSON artefact with, per output variable, the maximum absolute and
maximum relative deviation, the timestamp and grid where each occurs, and the
tolerance the test currently resolves on this platform. The header carries the
platform, CPython version, supy version, git SHA and Fortran build profile.
The exit code is 0 however large the spread is: this is a recorder, not a
gate. The loader, variable list, run helpers and deviation arithmetic are
imported from the test module rather than re-implemented, so the numbers are
exactly what the comparator sees.

``summarise`` reads a set of those artefacts (files, or directories as laid
out by ``gh run download``) and prints, per variable, the spread across all of
them beside the current tolerance, so tolerances can later be derived from a
week of nightly runs and cite the runs they came from.

The nightly workflow uploads one artefact per (platform, CPython bookend) named
``tolerance-spread-<platform>-<arch>-<cpXY>``; the same job can be dispatched
manually with the ``tolerance_spread`` input.
"""

from __future__ import annotations

import argparse
from datetime import UTC, datetime
import json
import os
from pathlib import Path
import platform
import subprocess
import sys
import tempfile
from typing import Any

import numpy as np

SCHEMA_VERSION = 1
PROJECT_ROOT = Path(__file__).resolve().parents[2]
TEST_DIR = PROJECT_ROOT / "test"
TEST_CORE_DIR = TEST_DIR / "core"
# The full-year test allows the engine this long; the spread run is the same run.
ENGINE_TIMEOUT_SECONDS = 1800
BUILD_PROFILE_ENV = "SUEWS_BUILD_PROFILE"


def _import_sample_test():
    """Import the sample-output test module so its helpers are reused, not copied."""
    for path in (TEST_DIR, TEST_CORE_DIR):
        if str(path) not in sys.path:
            sys.path.insert(0, str(path))
    import test_sample_output as sample  # noqa: PLC0415

    return sample


def _git_sha(explicit: str | None) -> str | None:
    if explicit:
        return explicit
    from_env = os.environ.get("GITHUB_SHA")
    if from_env:
        return from_env
    try:
        result = subprocess.run(
            ["git", "-C", str(PROJECT_ROOT), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            check=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    return result.stdout.strip() or None


def _runner_identity() -> dict[str, Any]:
    """GitHub Actions identity when present; explicit nulls otherwise."""
    keys = (
        "GITHUB_RUN_ID",
        "GITHUB_RUN_ATTEMPT",
        "GITHUB_JOB",
        "GITHUB_REF",
        "RUNNER_OS",
        "RUNNER_ARCH",
    )
    return {key.lower(): os.environ.get(key) for key in keys}


def _point(
    index_frame, position: int, actual, expected, abs_diff, rel_diff
) -> dict[str, Any]:
    """Describe one compared position: where it is and what the two frames hold."""
    grid, timestamp = index_frame[position]
    return {
        "index": int(position),
        "grid": int(grid),
        "timestamp": timestamp.isoformat(),
        "actual": float(actual[position]),
        "expected": float(expected[position]),
        "abs_deviation": float(abs_diff[position]),
        "rel_deviation": float(rel_diff[position]),
    }


def _variable_record(sample, var: str, df_actual, df_expected) -> dict[str, Any]:
    """Zero-tolerance deviation summary for one variable."""
    record: dict[str, Any] = {
        "tolerance_current": sample.get_tolerance_for_variable(var),
        "tolerance_base": dict(sample.TOLERANCE_CONFIG[var]),
    }
    if var not in df_actual.columns:
        record["status"] = "missing_in_engine_output"
        return record

    actual = np.asarray(df_actual[var].values, dtype=float)
    expected = np.asarray(df_expected[var].values, dtype=float)
    abs_diff, rel_diff, valid_mask, nan_mismatch = sample.deviation_arrays(
        actual, expected
    )

    record["status"] = "compared"
    record["n_points"] = len(actual)
    record["n_valid"] = int(valid_mask.sum())
    record["n_nan_mismatch"] = int(nan_mismatch.sum())
    record["n_exact"] = int(np.count_nonzero((abs_diff == 0) & valid_mask))

    if not valid_mask.any():
        record["max_abs_deviation"] = None
        record["max_rel_deviation"] = None
        return record

    index_frame = df_expected.index
    masked_abs = np.where(valid_mask, abs_diff, -np.inf)
    masked_rel = np.where(valid_mask, rel_diff, -np.inf)
    record["max_abs_deviation"] = _point(
        index_frame, int(np.argmax(masked_abs)), actual, expected, abs_diff, rel_diff
    )
    record["max_rel_deviation"] = _point(
        index_frame, int(np.argmax(masked_rel)), actual, expected, abs_diff, rel_diff
    )
    record["mean_abs_deviation"] = float(np.mean(abs_diff[valid_mask]))
    return record


def measure(args: argparse.Namespace) -> int:
    """Run the full-year sample comparison at zero tolerance and write the artefact."""
    sample = _import_sample_test()
    from sample_output_io import load_sample_output  # noqa: PLC0415

    import supy  # noqa: PLC0415

    engine = sample._locate_engine()
    sample_dir = sample._sample_data_dir()
    sample_config = sample_dir / "sample_config.yml"
    if not sample_config.is_file():
        raise FileNotFoundError(f"Sample config not found: {sample_config}")

    df_ref = load_sample_output(sample.test_data_dir)
    steps = len(df_ref)
    variables = list(sample.TOLERANCE_CONFIG)
    print(f"Reference: {steps} rows; variables: {', '.join(variables)}")

    with tempfile.TemporaryDirectory() as tmpdir:
        run_dir = Path(tmpdir)
        config_path, forcing_rows = sample._write_run_inputs(
            sample_dir, sample_config, run_dir, steps, truncated=False
        )
        print(f"Running {engine} on {forcing_rows} forcing rows (full year)")
        started = datetime.now(UTC)
        output_path = sample._run_engine(
            engine, config_path, run_dir, ENGINE_TIMEOUT_SECONDS
        )
        engine_seconds = (datetime.now(UTC) - started).total_seconds()
        # Read inside the temporary directory: _run_engine returns the path of
        # the Arrow file and _read_engine_output memory-maps it, so the file
        # has to outlive both calls.
        df_actual = sample._read_engine_output(output_path, variables)

    engine_rows = len(df_actual)
    rows_compared = min(engine_rows, steps)
    df_actual = df_actual.iloc[:rows_compared]
    df_expected = df_ref.iloc[:rows_compared]

    records = {
        var: _variable_record(sample, var, df_actual, df_expected) for var in variables
    }

    payload = {
        "schema_version": SCHEMA_VERSION,
        "generated_at": datetime.now(UTC).isoformat(),
        "tolerance_applied": {"rtol": 0.0, "atol": 0.0},
        "platform": {
            "system": platform.system(),
            "machine": platform.machine(),
            "release": platform.release(),
            "platform_key": sample.get_platform_key(),
        },
        "python": {
            "version": platform.python_version(),
            "implementation": platform.python_implementation(),
            "tag": f"cp{sys.version_info.major}{sys.version_info.minor}",
        },
        "supy_version": getattr(supy, "__version__", None),
        "git_sha": _git_sha(args.git_sha),
        "build_profile": args.build_profile
        or os.environ.get(BUILD_PROFILE_ENV)
        or "unknown",
        "engine": str(engine),
        "runner": _runner_identity(),
        "comparison": {
            "reference_rows": int(steps),
            "engine_rows": int(engine_rows),
            "rows_compared": int(rows_compared),
            "engine_seconds": round(engine_seconds, 3),
        },
        "variables": records,
    }

    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(payload, indent=2, sort_keys=False) + "\n", encoding="utf-8"
    )
    print(f"Wrote {output}")

    table = _measure_table(payload)
    print("\n".join(table))
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary_path:
        label = _artefact_label(payload)
        with open(summary_path, "a", encoding="utf-8") as handle:
            handle.write(f"### Zero-tolerance spread: {label}\n\n")
            handle.write("\n".join(_measure_table(payload, markdown=True)) + "\n\n")
    return 0


def _fmt(value: float | None) -> str:
    if value is None:
        return "-"
    return f"{value:.3e}"


def _measure_table(payload: dict[str, Any], markdown: bool = False) -> list[str]:
    header = [
        "variable",
        "max abs",
        "at (timestamp, grid)",
        "max rel",
        "at (timestamp, grid)",
        "exact/points",
        "rtol",
        "atol",
    ]
    rows = []
    for var, record in payload["variables"].items():
        if (
            record.get("status") != "compared"
            or record.get("max_abs_deviation") is None
        ):
            rows.append([var, "-", record.get("status", "-"), "-", "-", "-", "-", "-"])
            continue
        mabs = record["max_abs_deviation"]
        mrel = record["max_rel_deviation"]
        tol = record["tolerance_current"]
        rows.append([
            var,
            _fmt(mabs["abs_deviation"]),
            f"{mabs['timestamp']}, {mabs['grid']}",
            _fmt(mrel["rel_deviation"]),
            f"{mrel['timestamp']}, {mrel['grid']}",
            f"{record['n_exact']}/{record['n_points']}",
            f"{tol['rtol']:g}",
            f"{tol['atol']:g}",
        ])
    return _render_table(header, rows, markdown)


def _render_table(
    header: list[str], rows: list[list[str]], markdown: bool
) -> list[str]:
    if markdown:
        lines = [
            "| " + " | ".join(header) + " |",
            "|" + "|".join("---" for _ in header) + "|",
        ]
        lines.extend("| " + " | ".join(row) + " |" for row in rows)
        return lines
    widths = [
        max(len(str(cell)) for cell in column)
        for column in zip(header, *rows, strict=True)
    ]
    fmt = "  ".join(f"{{:<{width}}}" for width in widths)
    lines = [fmt.format(*header), fmt.format(*("-" * width for width in widths))]
    lines.extend(fmt.format(*row) for row in rows)
    return lines


def _artefact_label(payload: dict[str, Any]) -> str:
    return f"{payload['platform']['platform_key']}/{payload['python']['tag']}"


def _collect_artefacts(paths: list[str]) -> list[tuple[Path, dict[str, Any]]]:
    """Load every artefact JSON under the given files or directories."""
    found: list[tuple[Path, dict[str, Any]]] = []
    for raw in paths:
        path = Path(raw)
        candidates = sorted(path.rglob("*.json")) if path.is_dir() else [path]
        for candidate in candidates:
            try:
                payload = json.loads(candidate.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError) as exc:
                print(f"[skip] {candidate}: {exc}", file=sys.stderr)
                continue
            if (
                not isinstance(payload, dict)
                or payload.get("schema_version") != SCHEMA_VERSION
            ):
                print(
                    f"[skip] {candidate}: not a tolerance-spread artefact (schema {SCHEMA_VERSION})",
                    file=sys.stderr,
                )
                continue
            found.append((candidate, payload))
    return found


def summarise(args: argparse.Namespace) -> int:
    """Print the spread across a set of artefacts beside the current tolerance."""
    artefacts = _collect_artefacts(args.paths)
    if not artefacts:
        print("[X] no tolerance-spread artefacts found", file=sys.stderr)
        return 1

    lines: list[str] = []
    lines.append(f"Artefacts: {len(artefacts)}")
    shas = {payload.get("git_sha") for _, payload in artefacts}
    for path, payload in artefacts:
        lines.append(
            f"  {_artefact_label(payload)}  sha={str(payload.get('git_sha'))[:12]}  "
            f"supy={payload.get('supy_version')}  profile={payload.get('build_profile')}  "
            f"generated={payload.get('generated_at')}  ({path})"
        )
    if len(shas) > 1:
        lines.append(
            f"[warn] artefacts span {len(shas)} git SHAs; the spread mixes revisions"
        )
    lines.append("")

    variables: list[str] = []
    for _, payload in artefacts:
        for var in payload["variables"]:
            if var not in variables:
                variables.append(var)

    header = [
        "variable",
        "max abs",
        "from",
        "max rel",
        "from",
        "current rtol",
        "current atol",
        "n",
    ]
    rows = []
    for var in variables:
        best_abs: tuple[float, str, str] | None = None
        best_rel: tuple[float, str, str] | None = None
        rtols: set[float] = set()
        atols: set[float] = set()
        count = 0
        for _, payload in artefacts:
            record = payload["variables"].get(var)
            if (
                not record
                or record.get("status") != "compared"
                or record.get("max_abs_deviation") is None
            ):
                continue
            count += 1
            label = _artefact_label(payload)
            rtols.add(record["tolerance_current"]["rtol"])
            atols.add(record["tolerance_current"]["atol"])
            mabs = record["max_abs_deviation"]
            mrel = record["max_rel_deviation"]
            if best_abs is None or mabs["abs_deviation"] > best_abs[0]:
                best_abs = (
                    mabs["abs_deviation"],
                    label,
                    f"{mabs['timestamp']}, grid {mabs['grid']}",
                )
            if best_rel is None or mrel["rel_deviation"] > best_rel[0]:
                best_rel = (
                    mrel["rel_deviation"],
                    label,
                    f"{mrel['timestamp']}, grid {mrel['grid']}",
                )
        if best_abs is None or best_rel is None:
            rows.append([var, "-", "-", "-", "-", "-", "-", "0"])
            continue
        rows.append([
            var,
            _fmt(best_abs[0]),
            f"{best_abs[1]} @ {best_abs[2]}",
            _fmt(best_rel[0]),
            f"{best_rel[1]} @ {best_rel[2]}",
            _range(rtols),
            _range(atols),
            str(count),
        ])
    lines.extend(_render_table(header, rows, args.markdown))
    print("\n".join(lines))
    return 0


def _range(values: set[float]) -> str:
    if not values:
        return "-"
    low, high = min(values), max(values)
    return f"{low:g}" if low == high else f"{low:g}..{high:g}"


def main(argv: list[str] | None = None) -> int:
    """Command-line entry point."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = parser.add_subparsers(dest="command", required=True)

    run = sub.add_parser(
        "measure",
        help="run the full-year sample comparison at zero tolerance and write JSON",
    )
    run.add_argument("--output", required=True, help="JSON artefact path")
    run.add_argument(
        "--build-profile",
        default=None,
        help=f"Fortran build profile of the installed supy (else {BUILD_PROFILE_ENV}, else unknown)",
    )
    run.add_argument(
        "--git-sha",
        default=None,
        help="source SHA to record (else GITHUB_SHA, else git rev-parse HEAD)",
    )
    run.set_defaults(func=measure)

    read = sub.add_parser(
        "summarise",
        help="print the spread across a set of artefacts beside the current tolerance",
    )
    read.add_argument(
        "paths", nargs="+", help="artefact JSON files or directories containing them"
    )
    read.add_argument(
        "--markdown", action="store_true", help="emit a GitHub-flavoured Markdown table"
    )
    read.set_defaults(func=summarise)

    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
