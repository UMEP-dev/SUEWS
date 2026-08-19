"""``suews compare`` — compare two SUEWS runs (or run vs observations).

Loads the run output (or observation CSV) at each of two positional paths
and computes per-variable RMSE / bias / Pearson r. Time-axis overlap
(start / end / number of joint timestamps) is reported alongside so the
caller can sanity-check the alignment.
"""

from __future__ import annotations

import math
from pathlib import Path
import sys
from typing import Any

import click
import pandas as pd

from .._run_output import _load_run_output_dataframe
from ..metrics import bias as metric_bias, pearson_r, rmse as metric_rmse
from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso

_VALID_METRICS = {"rmse", "bias", "r"}


def _coerce_datetime_index(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure the DataFrame has a DatetimeIndex when one is recoverable.

    The canonical SUEWS save path produces a (grid, datetime) MultiIndex.
    For metric computation we drop the grid level (taking the first grid)
    so the comparison happens on a single time series. If neither index
    is datetime-like, we leave the index as-is and rely on row-order
    alignment in ``_align_pair``.
    """
    # Pick the first grid present; comparing across grids is out of scope for
    # the Phase-1 compare command.
    if isinstance(df.index, pd.MultiIndex) and len(df.index):
        df = df.xs(df.index.get_level_values(0).unique()[0], level=0)
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
    return (
        ser_a.iloc[:n].reset_index(drop=True),
        ser_b.iloc[:n].reset_index(drop=True),
        overlap,
    )


def _fmt_metric(value: float) -> str:
    return "nan" if math.isnan(value) else f"{value:.4g}"


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
    list_metrics = data.get("requested_metrics") or ("rmse", "bias", "r")
    for var, metrics in (data.get("per_variable") or {}).items():
        parts = [f"  {var:<6}"]
        for metric_name in list_metrics:
            if metric_name in metrics:
                parts.append(f"{metric_name}={_fmt_metric(metrics[metric_name])}")
        parts.append(f"n={metrics['n']}")
        lines.append(" ".join(parts))
    return "\n".join(lines)


def _emit_user_error(
    message: str,
    *,
    json_mode: bool,
    command: str,
    data: dict[str, Any],
    started_at: str,
) -> None:
    if json_mode:
        Envelope.error(
            errors=[message],
            command=command,
            data=data,
            started_at=started_at,
        ).emit()
    else:
        click.secho(message, fg="red", err=True)
    sys.exit(EXIT_USER_ERROR)


def _parse_requested_metrics(metrics: str) -> list[str]:
    list_metrics = [metric.strip() for metric in metrics.split(",") if metric.strip()]
    list_unknown = [metric for metric in list_metrics if metric not in _VALID_METRICS]
    if list_unknown:
        valid = ", ".join(sorted(_VALID_METRICS))
        unknown = ", ".join(list_unknown)
        raise ValueError(f"Unknown metric(s): {unknown}. Valid: {valid}")
    return list_metrics


def _metric_entry(
    ser_a: pd.Series,
    ser_b: pd.Series,
    list_metrics: list[str],
) -> dict[str, Any]:
    entry: dict[str, Any] = {"n": int(min(len(ser_a), len(ser_b)))}
    if "rmse" in list_metrics:
        entry["rmse"] = metric_rmse(ser_a, ser_b)
    if "bias" in list_metrics:
        entry["bias"] = metric_bias(ser_a, ser_b)
    if "r" in list_metrics:
        entry["r"] = pearson_r(ser_a, ser_b)
    return entry


def _compare_variables(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    list_variables: list[str],
    list_metrics: list[str],
) -> tuple[dict[str, dict[str, Any]], dict[str, Any] | None, list[str]]:
    list_warnings: list[str] = []
    per_variable: dict[str, dict[str, Any]] = {}
    overlap: dict[str, Any] | None = None

    for var in list_variables:
        try:
            ser_a, ser_b, var_overlap = _align_pair(df_a, df_b, var)
            per_variable[var] = _metric_entry(ser_a, ser_b, list_metrics)
        except (KeyError, ValueError) as exc:
            list_warnings.append(f"variable {var!r} skipped: {exc}")
            continue

        if overlap is None:
            overlap = var_overlap

    return per_variable, overlap, list_warnings


def _aggregate_metrics(
    per_variable: dict[str, dict[str, Any]],
    list_metrics: list[str],
) -> dict[str, float]:
    aggregate: dict[str, float] = {}
    for metric_name in list_metrics:
        list_values: list[float] = []
        for entry in per_variable.values():
            value = entry.get(metric_name)
            if value is not None and not math.isnan(value):
                list_values.append(value)
        if list_values:
            aggregate[metric_name] = float(sum(list_values) / len(list_values))
    return aggregate


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
    """Compare two saved SUEWS outputs."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "compare", *sys.argv[1:]])

    path_a = Path(run_a)
    path_b = Path(run_b_or_obs)
    paths_data = {"baseline": str(path_a), "scenario": str(path_b)}

    try:
        list_metrics = _parse_requested_metrics(metrics)
    except ValueError as exc:
        _emit_user_error(
            str(exc),
            json_mode=json_mode,
            command=command,
            data=paths_data,
            started_at=started_at,
        )

    try:
        df_a = _load_run_output_dataframe(path_a)
        df_b = _load_run_output_dataframe(path_b)
        df_a = _coerce_datetime_index(df_a)
        df_b = _coerce_datetime_index(df_b)
    except (FileNotFoundError, OSError, ValueError) as exc:
        _emit_user_error(
            f"Failed to load inputs: {exc}",
            json_mode=json_mode,
            command=command,
            data=paths_data,
            started_at=started_at,
        )

    list_variables = [v.strip() for v in variables.split(",") if v.strip()]
    per_variable, overlap, list_warnings = _compare_variables(
        df_a,
        df_b,
        list_variables,
        list_metrics,
    )

    data: dict[str, Any] = {
        "baseline": str(path_a),
        "scenario": str(path_b),
        "metrics": _aggregate_metrics(per_variable, list_metrics),
        "per_variable": per_variable,
        "requested_metrics": list_metrics,
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
