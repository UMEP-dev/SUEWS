"""``suews compare`` — compare two SUEWS runs (or run vs observations).

Loads the run output (or observation CSV) at each of two positional paths
and computes per-variable RMSE / bias / Pearson r over the timestamps the
two inputs share. The comparison is aligned on time by default: the time
axis is taken from a ``DatetimeIndex``, from the native ``Year`` / ``DOY`` /
``Hour`` / ``Min`` clock columns of legacy text output, or from a
``datetime`` column. Row-order alignment is available only as an explicit
opt-in (``--align positional``) and is labelled as such in the output.

Multi-grid outputs must name the grid to compare with ``--grid``; the grid
identity used on each side is reported. An empty joint time axis, or a
request for which no variable yields an evaluable metric, is a user error
rather than a successful result with no numbers.
"""

from __future__ import annotations

import math
from pathlib import Path
import re
import sys
from typing import Any

import click
import numpy as np
import pandas as pd

from .._run_output import _load_run_output_partitions, _recover_datetime_index
from ..metrics import bias as metric_bias, pearson_r, rmse as metric_rmse
from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso

_VALID_METRICS = {"rmse", "bias", "r"}

# Legacy SUEWS text output marks missing values with -999.
_MISSING_SENTINEL = -999.0

_ALIGN_TIME = "time"
_ALIGN_POSITIONAL = "positional"

# Partition labels produced by ``_load_run_output_partitions``: either
# ``<file>[grid=<grid>]`` for a grid within a MultiIndex file, or the bare
# legacy text file name ``<site><grid>_<year>_SUEWS_<tstep>.txt``.
_RE_PARTITION_GRID = re.compile(r"\[grid=(?P<grid>[^\]]+)\]$")
_RE_LEGACY_TEXT_NAME = re.compile(
    r"^(?P<site>.*?)(?P<grid>\d+)_(?P<year>\d{4})_SUEWS_(?P<tstep>\d+)\.txt$"
)


def _partition_grid(label: str) -> str | None:
    """Return the grid identity encoded in a partition label, if any."""
    match = _RE_PARTITION_GRID.search(label)
    if match:
        return match.group("grid")
    match = _RE_LEGACY_TEXT_NAME.match(label)
    if match:
        return match.group("grid")
    return None


def _select_grid(
    list_partitions: list[tuple[str, pd.DataFrame]],
    grid: str | None,
    side: str,
) -> tuple[pd.DataFrame, str | None]:
    """Pick one grid's frame from loaded partitions.

    Partitions are grouped by the grid identity in their label. Legacy text
    output splits one grid across yearly files, so the frames of the chosen
    grid are concatenated. Without ``--grid`` exactly one grid must be
    present; otherwise the caller is asked to choose.

    Returns ``(frame, grid_identity)``; the identity is ``None`` for inputs
    that carry no grid label (for example an observations CSV). Such an
    input is accepted alongside ``--grid`` only when it is a single
    partition; several unlabelled partitions are ambiguous and rejected.
    """
    groups: dict[str | None, list[pd.DataFrame]] = {}
    for label, df_partition in list_partitions:
        groups.setdefault(_partition_grid(label), []).append(df_partition)

    if grid is not None:
        if grid in groups:
            chosen = grid
        elif list(groups) == [None] and len(groups[None]) == 1:
            # A single unlabelled series (typically an observations CSV) has
            # no grid to select; it is accepted as-is and reported with an
            # unknown grid identity while --grid still picks the run grid.
            chosen = None
        else:
            available = (
                ", ".join(g for g in groups if g is not None) or "none identifiable"
            )
            raise ValueError(
                f"{side}: grid {grid!r} not present (available: {available})"
            )
    elif len(groups) == 1:
        chosen = next(iter(groups))
    else:
        available = ", ".join("?" if g is None else g for g in groups)
        raise ValueError(
            f"{side}: input holds several grids ({available}); choose one with --grid"
        )

    list_frames = groups[chosen]
    df_grid = list_frames[0] if len(list_frames) == 1 else pd.concat(list_frames)
    return df_grid, chosen


def _load_side(
    path: Path,
    grid: str | None,
    side: str,
) -> tuple[pd.DataFrame, str | None]:
    """Load one side of the comparison and recover its time axis."""
    list_partitions = _load_run_output_partitions(path)
    df_side, grid_side = _select_grid(list_partitions, grid, side)
    df_side = _recover_datetime_index(df_side)
    if isinstance(df_side.index, pd.DatetimeIndex):
        df_side = df_side.sort_index()
        if df_side.index.has_duplicates:
            raise ValueError(f"{side}: time axis has duplicate timestamps")
    return df_side, grid_side


def _span(df_side: pd.DataFrame) -> str:
    """Describe the extent of one side: its time span, or its row count."""
    if not len(df_side.index):
        return "empty"
    if not isinstance(df_side.index, pd.DatetimeIndex):
        return f"{len(df_side.index)} rows"
    return f"{df_side.index.min().isoformat()} .. {df_side.index.max().isoformat()}"


def _time_overlap(df_a: pd.DataFrame, df_b: pd.DataFrame, mode: str) -> dict[str, Any]:
    """Describe the joint axis: shared timestamps, or shared row count."""
    if mode == _ALIGN_POSITIONAL:
        return {"start": None, "end": None, "n": min(len(df_a), len(df_b))}
    idx = df_a.index.intersection(df_b.index)
    return {
        "start": idx.min().isoformat() if len(idx) else None,
        "end": idx.max().isoformat() if len(idx) else None,
        "n": len(idx),
    }


def _numeric_series(df_side: pd.DataFrame, name: str) -> pd.Series:
    """Return column ``name`` as floats with sentinels and infinities as NaN."""
    column = df_side[name]
    if isinstance(column, pd.DataFrame):  # duplicate labels: keep the first
        column = column.iloc[:, 0]
    ser = pd.to_numeric(column, errors="coerce").astype(float)
    ser = ser.mask(ser == _MISSING_SENTINEL)
    return ser.replace([np.inf, -np.inf], np.nan)


def _align_pair(
    ser_a: pd.Series, ser_b: pd.Series, mode: str
) -> tuple[pd.Series, pd.Series]:
    """Align two series on shared timestamps, or by row order when asked."""
    if mode == _ALIGN_TIME:
        df_pair = pd.concat({"a": ser_a, "b": ser_b}, axis=1, join="inner")
        return df_pair["a"], df_pair["b"]

    n = min(len(ser_a), len(ser_b))
    return (
        ser_a.iloc[:n].reset_index(drop=True),
        ser_b.iloc[:n].reset_index(drop=True),
    )


def _fmt_metric(value: float) -> str:
    return "nan" if math.isnan(value) else f"{value:.4g}"


def _build_text_message(data: dict[str, Any]) -> str:
    grid = data.get("grid") or {}
    lines = [
        "Comparison",
        "  baseline : {}".format(data["baseline"]),
        "  scenario : {}".format(data["scenario"]),
        "  alignment: {}".format(data["alignment"]),
        "  grid     : baseline={} scenario={}".format(
            grid.get("baseline"), grid.get("scenario")
        ),
        "  overlap  : start={} end={} n={}".format(
            data["time_axis_overlap"]["start"],
            data["time_axis_overlap"]["end"],
            data["time_axis_overlap"]["n"],
        ),
        "",
        "Per-variable metrics (n = finite paired samples):",
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


class _CompareError(Exception):
    """A user-facing comparison failure with the partial data to report."""

    def __init__(
        self,
        message: str,
        *,
        data: dict[str, Any] | None = None,
        warnings: list[str] | None = None,
    ) -> None:
        super().__init__(message)
        self.message = message
        self.data = data or {}
        self.warnings = warnings or []


def _emit_user_error(
    exc: _CompareError,
    *,
    json_mode: bool,
    command: str,
    paths_data: dict[str, Any],
    started_at: str,
) -> None:
    if json_mode:
        Envelope.error(
            errors=[exc.message],
            command=command,
            data={**paths_data, **exc.data},
            warnings=exc.warnings or None,
            started_at=started_at,
        ).emit()
    else:
        click.secho(exc.message, fg="red", err=True)
        for warn in exc.warnings:
            click.echo(f"warning: {warn}", err=True)
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
    """Compute the requested metrics over the finite paired samples.

    ``n`` is the number of pairs in which both members are finite, which is
    exactly the sample the metric helpers use. Raises ``ValueError`` when no
    pair is finite or when the metric helpers judge the sample too small.
    """
    df_pair = pd.concat({"a": ser_a, "b": ser_b}, axis=1, join="inner")
    df_pair = df_pair.replace([np.inf, -np.inf], np.nan).dropna()
    n_finite = len(df_pair)
    if n_finite == 0:
        raise ValueError("no finite paired samples")

    entry: dict[str, Any] = {"n": n_finite}
    if "rmse" in list_metrics:
        entry["rmse"] = metric_rmse(df_pair["a"], df_pair["b"])
    if "bias" in list_metrics:
        entry["bias"] = metric_bias(df_pair["a"], df_pair["b"])
    if "r" in list_metrics:
        entry["r"] = pearson_r(df_pair["a"], df_pair["b"])
    return entry


def _compare_variables(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    list_variables: list[str],
    list_metrics: list[str],
    mode: str,
) -> tuple[dict[str, dict[str, Any]], list[str]]:
    list_warnings: list[str] = []
    per_variable: dict[str, dict[str, Any]] = {}

    for var in list_variables:
        if var not in df_a.columns or var not in df_b.columns:
            list_warnings.append(
                f"variable {var!r} skipped: missing from one or both inputs"
            )
            continue
        ser_a, ser_b = _align_pair(
            _numeric_series(df_a, var), _numeric_series(df_b, var), mode
        )
        try:
            per_variable[var] = _metric_entry(ser_a, ser_b, list_metrics)
        except ValueError as exc:
            list_warnings.append(f"variable {var!r} skipped: {exc}")

    return per_variable, list_warnings


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


def _load_sides(
    path_a: Path,
    path_b: Path,
    grid: str | None,
    mode: str,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, str | None], list[str]]:
    """Load both sides, select grids and check the requested alignment is possible."""
    try:
        df_a, grid_a = _load_side(path_a, grid, "baseline")
        df_b, grid_b = _load_side(path_b, grid, "scenario")
    except (FileNotFoundError, OSError, ValueError) as exc:
        raise _CompareError(f"Failed to load inputs: {exc}") from exc

    grid_data: dict[str, str | None] = {"baseline": grid_a, "scenario": grid_b}
    list_warnings: list[str] = []
    if grid_a is not None and grid_b is not None and grid_a != grid_b:
        list_warnings.append(
            f"grid identities differ: baseline grid {grid_a}, scenario grid {grid_b}"
        )

    if mode == _ALIGN_POSITIONAL:
        list_warnings.append(
            "positional alignment: rows are compared by order, not by timestamp"
        )
        return df_a, df_b, grid_data, list_warnings

    list_no_time = [
        side
        for side, df_side in (("baseline", df_a), ("scenario", df_b))
        if not isinstance(df_side.index, pd.DatetimeIndex)
    ]
    if list_no_time:
        raise _CompareError(
            f"No time axis recoverable for {' and '.join(list_no_time)} (supported: "
            "DatetimeIndex, Year/DOY/Hour/Min columns, or a datetime column); pass "
            "--align positional to compare rows by order.",
            data={"grid": grid_data},
        )
    return df_a, df_b, grid_data, list_warnings


def _run_comparison(
    path_a: Path,
    path_b: Path,
    *,
    metrics: str,
    variables: str,
    grid: str | None,
    mode: str,
) -> tuple[dict[str, Any], list[str]]:
    """Compute the comparison payload, raising ``_CompareError`` on user errors."""
    try:
        list_metrics = _parse_requested_metrics(metrics)
    except ValueError as exc:
        raise _CompareError(str(exc)) from exc

    df_a, df_b, grid_data, list_warnings = _load_sides(path_a, path_b, grid, mode)

    overlap = _time_overlap(df_a, df_b, mode)
    if overlap["n"] == 0:
        raise _CompareError(
            f"No overlapping timestamps: baseline covers {_span(df_a)}, scenario covers {_span(df_b)}.",
            data={"grid": grid_data, "time_axis_overlap": overlap},
            warnings=list_warnings,
        )

    list_variables = [v.strip() for v in variables.split(",") if v.strip()]
    per_variable, list_var_warnings = _compare_variables(
        df_a, df_b, list_variables, list_metrics, mode
    )
    list_warnings.extend(list_var_warnings)
    if not per_variable:
        raise _CompareError(
            "No evaluable comparison: none of the requested variables "
            f"({', '.join(list_variables)}) yielded finite paired samples.",
            data={"grid": grid_data, "time_axis_overlap": overlap},
            warnings=list_warnings,
        )

    data: dict[str, Any] = {
        "alignment": mode,
        "grid": grid_data,
        "metrics": _aggregate_metrics(per_variable, list_metrics),
        "per_variable": per_variable,
        "requested_metrics": list_metrics,
        "time_axis_overlap": overlap,
    }
    return data, list_warnings


@click.command(
    name="compare",
    short_help="Compare two SUEWS runs or run vs observations.",
    help=(
        "Compare two SUEWS run directories (or a run and an observations CSV) "
        "by computing per-variable RMSE / bias / Pearson r over the timestamps "
        "they share. The second argument is treated as a directory if it is "
        "one and as an observations file otherwise. Time-axis overlap and the "
        "grid used on each side are reported alongside; n counts finite paired "
        "samples."
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
    "--grid",
    default=None,
    help=(
        "Grid identity to compare when an input holds several grids "
        "(required in that case). Applied to both inputs."
    ),
)
@click.option(
    "--align",
    "alignment",
    type=click.Choice([_ALIGN_TIME, _ALIGN_POSITIONAL], case_sensitive=False),
    default=_ALIGN_TIME,
    show_default=True,
    help=(
        "'time' aligns on shared timestamps and rejects inputs without a "
        "recoverable time axis; 'positional' compares rows by order and is "
        "labelled as such in the output."
    ),
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
    grid: str | None,
    alignment: str,
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
        data, list_warnings = _run_comparison(
            path_a,
            path_b,
            metrics=metrics,
            variables=variables,
            grid=grid,
            mode=alignment.lower(),
        )
    except _CompareError as exc:
        _emit_user_error(
            exc,
            json_mode=json_mode,
            command=command,
            paths_data=paths_data,
            started_at=started_at,
        )

    data = {**paths_data, **data}
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
