"""``suews compare`` — compare two SUEWS runs (or run vs observations).

Loads the run output (or observation CSV) at each of two positional paths
and computes per-variable RMSE / bias / Pearson r. Time-axis overlap
(start / end / number of joint timestamps) is reported alongside so the
caller can sanity-check the alignment.
"""

from __future__ import annotations

from pathlib import Path
import sys
from typing import Any

import click
import pandas as pd

from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso


def _load_run_or_observations(path: Path) -> pd.DataFrame:
    """Best-effort load of a run output dir or an observations file.

    - If ``path`` is a directory: scans for ``df_output*.csv`` /
      ``df_output*.parquet`` and loads the first hit.
    - If ``path`` is a file: loads it as CSV (parquet detection by suffix).

    The returned DataFrame is column-flattened: tuple columns from a
    canonical SUEWS MultiIndex are reduced to their first level.
    """
    if path.is_dir():
        list_paths: list[Path] = []
        for pattern in ("df_output*.parquet", "df_output*.csv"):
            list_paths.extend(path.rglob(pattern))
        if not list_paths:
            raise FileNotFoundError(
                f"No df_output*.parquet / df_output*.csv under {path}"
            )
        # Prefer parquet for speed and dtype fidelity.
        list_paths.sort(key=lambda p: 0 if p.suffix == ".parquet" else 1)
        path_first = list_paths[0]
        if path_first.suffix == ".parquet":
            df = pd.read_parquet(path_first)
        else:
            df = pd.read_csv(path_first)
    elif path.suffix == ".parquet":
        df = pd.read_parquet(path)
    else:
        df = pd.read_csv(path)

    # Flatten MultiIndex columns to their first level so callers can ask
    # for "QH" without having to thread a tuple key through.
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [
            col[0] if isinstance(col, tuple) else col for col in df.columns
        ]
    else:
        df.columns = [
            col[0] if isinstance(col, tuple) else col for col in df.columns
        ]
    return df


def _coerce_datetime_index(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure the DataFrame has a DatetimeIndex when one is recoverable.

    The canonical SUEWS save path produces a (grid, datetime) MultiIndex.
    For metric computation we drop the grid level (taking the first grid)
    so the comparison happens on a single time series. If neither index
    is datetime-like, we leave the index as-is and rely on row-order
    alignment in ``_align_pair``.
    """
    if isinstance(df.index, pd.MultiIndex):
        # Pick the first grid present; comparing across grids is out of
        # scope for the Phase-1 compare command.
        try:
            df = df.xs(df.index.get_level_values(0).unique()[0], level=0)
        except (IndexError, KeyError):
            pass
    if not isinstance(df.index, pd.DatetimeIndex):
        # Try the first column or a 'datetime' column.
        for cand in ("datetime", "Datetime", "DateTime"):
            if cand in df.columns:
                df = df.set_index(pd.DatetimeIndex(df[cand]))
                break
    return df


def _align_pair(
    df_a: pd.DataFrame, df_b: pd.DataFrame, variable: str
) -> tuple[pd.Series, pd.Series, dict[str, Any]]:
    """Align two series on the DataFrames' shared index.

    Returns ``(series_a, series_b, overlap_meta)``. When neither input has
    a DatetimeIndex, alignment falls back to row order on the shorter
    length.
    """
    if variable not in df_a.columns or variable not in df_b.columns:
        raise KeyError(f"Variable {variable!r} missing from one or both inputs.")

    ser_a = pd.to_numeric(df_a[variable], errors="coerce")
    ser_b = pd.to_numeric(df_b[variable], errors="coerce")

    if isinstance(ser_a.index, pd.DatetimeIndex) and isinstance(
        ser_b.index, pd.DatetimeIndex
    ):
        df_pair = pd.concat({"a": ser_a, "b": ser_b}, axis=1, join="inner")
        idx = df_pair.index
        overlap = {
            "start": idx.min().isoformat() if len(idx) else None,
            "end": idx.max().isoformat() if len(idx) else None,
            "n": len(idx),
        }
        return df_pair["a"], df_pair["b"], overlap

    n = min(len(ser_a), len(ser_b))
    overlap = {"start": None, "end": None, "n": int(n)}
    return ser_a.iloc[:n].reset_index(drop=True), ser_b.iloc[:n].reset_index(drop=True), overlap


def _build_text_message(data: dict[str, Any]) -> str:
    lines = [
        "Comparison",
        "  baseline: {}".format(data["baseline"]),
        "  scenario: {}".format(data["scenario"]),
        "  overlap : start={} end={} n={}".format(
            data["time_axis_overlap"]["start"],
            data["time_axis_overlap"]["end"],
            data["time_axis_overlap"]["n"],
        ),
        "",
        "Per-variable metrics:",
    ]
    for var, metrics in (data.get("per_variable") or {}).items():
        lines.append(
            f"  {var:<6} rmse={metrics['rmse']:.4g} "
            f"bias={metrics['bias']:.4g} r={metrics['r']:.4g} "
            f"n={metrics['n']}"
        )
    return "\n".join(lines)


@click.command(
    name="compare",
    short_help="Compare two SUEWS runs or run vs observations.",
    help=(
        "Compare two SUEWS run directories (or a run and an observations CSV) "
        "by computing per-variable RMSE / bias / Pearson r. The second "
        "argument is treated as a directory if it is one and as an "
        "observations file otherwise. Time-axis overlap is reported alongside."
    ),
)
@click.argument(
    "run_a",
    type=click.Path(exists=True),
)
@click.argument(
    "run_b_or_obs",
    type=click.Path(exists=True),
)
@click.option(
    "--metrics",
    default="rmse,bias,r",
    show_default=True,
    help="Comma-separated metric names. Subset of {rmse,bias,r}.",
)
@click.option(
    "--variables",
    default="QH,QE,QN",
    show_default=True,
    help="Comma-separated variable names to compare.",
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
def compare_runs_cmd(
    run_a: str,
    run_b_or_obs: str,
    metrics: str,
    variables: str,
    output_format: str,
) -> None:
    """Implementation of ``suews compare``."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "compare", *sys.argv[1:]])

    path_a = Path(run_a)
    path_b = Path(run_b_or_obs)

    list_metrics = [m.strip() for m in metrics.split(",") if m.strip()]
    list_variables = [v.strip() for v in variables.split(",") if v.strip()]

    valid_metrics = {"rmse", "bias", "r"}
    list_unknown = [m for m in list_metrics if m not in valid_metrics]
    if list_unknown:
        message = "Unknown metric(s): {}. Valid: {}".format(
            ", ".join(list_unknown),
            ", ".join(sorted(valid_metrics)),
        )
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"baseline": str(path_a), "scenario": str(path_b)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    try:
        from ..metrics import bias as metric_bias, pearson_r, rmse as metric_rmse

        df_a = _load_run_or_observations(path_a)
        df_b = _load_run_or_observations(path_b)
        df_a = _coerce_datetime_index(df_a)
        df_b = _coerce_datetime_index(df_b)
    except (FileNotFoundError, OSError, ValueError) as exc:
        message = f"Failed to load inputs: {exc}"
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"baseline": str(path_a), "scenario": str(path_b)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    list_warnings: list[str] = []
    per_variable: dict[str, dict[str, Any]] = {}
    overlap: dict[str, Any] | None = None
    aggregate: dict[str, float] = {}

    for var in list_variables:
        try:
            ser_a, ser_b, var_overlap = _align_pair(df_a, df_b, var)
        except KeyError as exc:
            list_warnings.append(f"variable {var!r} skipped: {exc}")
            continue
        # First successful overlap is recorded as the run-level overlap.
        if overlap is None:
            overlap = var_overlap
        try:
            entry: dict[str, Any] = {"n": int(min(len(ser_a), len(ser_b)))}
            if "rmse" in list_metrics:
                entry["rmse"] = metric_rmse(ser_a, ser_b)
            if "bias" in list_metrics:
                entry["bias"] = metric_bias(ser_a, ser_b)
            if "r" in list_metrics:
                entry["r"] = pearson_r(ser_a, ser_b)
            per_variable[var] = entry
        except ValueError as exc:
            list_warnings.append(f"variable {var!r} skipped: {exc}")

    # Aggregate metrics: simple unweighted mean across variables. This is
    # a useful single-number summary for triage; per-variable detail is
    # available in ``per_variable``.
    for metric_name in list_metrics:
        list_values = [
            entry[metric_name]
            for entry in per_variable.values()
            if metric_name in entry
            and entry[metric_name] is not None
            and entry[metric_name] == entry[metric_name]  # filter NaN
        ]
        if list_values:
            aggregate[metric_name] = float(sum(list_values) / len(list_values))

    data: dict[str, Any] = {
        "baseline": str(path_a),
        "scenario": str(path_b),
        "metrics": aggregate,
        "per_variable": per_variable,
        "time_axis_overlap": overlap or {"start": None, "end": None, "n": 0},
    }

    if json_mode:
        Envelope.success(
            data=data,
            command=command,
            warnings=list_warnings or None,
            started_at=started_at,
        ).emit()
    else:
        click.echo(_build_text_message(data))
        for warn in list_warnings:
            click.echo(f"warning: {warn}", err=True)
