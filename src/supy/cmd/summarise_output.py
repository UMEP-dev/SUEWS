"""``suews summarise`` — produce a per-variable summary of a SUEWS run.

For each requested variable (or every numeric column when ``--variables``
is empty) the command reports the mean / min / max / NaN-percentage
alongside the overall time period and step count.
"""

from __future__ import annotations

from pathlib import Path
import sys
from typing import Any

import click
import numpy as np
import pandas as pd

from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso


def _load_run_dataframe(path_run_dir: Path) -> pd.DataFrame:
    """Load the canonical run output from ``path_run_dir``.

    Raises ``FileNotFoundError`` when no recognised output file is present.
    """
    list_paths: list[Path] = []
    for pattern in ("df_output*.parquet", "df_output*.csv", "*_SUEWS_*.txt"):
        list_paths.extend(path_run_dir.rglob(pattern))
    if not list_paths:
        raise FileNotFoundError(
            f"No df_output*.parquet / *.csv / *_SUEWS_*.txt under {path_run_dir}"
        )
    list_paths.sort(key=lambda p: 0 if p.suffix == ".parquet" else 1)
    path_first = list_paths[0]
    if path_first.suffix == ".parquet":
        df = pd.read_parquet(path_first)
    elif path_first.suffix == ".csv":
        df = pd.read_csv(path_first)
    else:
        df = pd.read_csv(path_first, sep=r"\s+", engine="python")

    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [
            col[0] if isinstance(col, tuple) else col for col in df.columns
        ]
    return df


def _period_and_n_steps(df: pd.DataFrame) -> dict[str, Any]:
    """Extract period start/end and step count from the DataFrame."""
    n_steps = len(df)
    if isinstance(df.index, pd.MultiIndex):
        # Unique datetimes across grids.
        idx_levels = [
            level for level in df.index.names if level and "time" in level.lower()
        ]
        if idx_levels:
            try:
                values = df.index.get_level_values(idx_levels[0])
                return {
                    "period": {
                        "start": str(values.min()),
                        "end": str(values.max()),
                    },
                    "n_steps": n_steps,
                }
            except (KeyError, ValueError):
                pass
    if isinstance(df.index, pd.DatetimeIndex):
        return {
            "period": {
                "start": df.index.min().isoformat(),
                "end": df.index.max().isoformat(),
            },
            "n_steps": n_steps,
        }
    for cand in ("datetime", "Datetime", "DateTime"):
        if cand in df.columns:
            ser = pd.to_datetime(df[cand], errors="coerce").dropna()
            if len(ser):
                return {
                    "period": {
                        "start": ser.min().isoformat(),
                        "end": ser.max().isoformat(),
                    },
                    "n_steps": n_steps,
                }
    return {"period": {"start": None, "end": None}, "n_steps": n_steps}


def _summarise_variable(ser: pd.Series) -> dict[str, Any]:
    """Summary stats for a single column. NaN-aware."""
    arr = pd.to_numeric(ser, errors="coerce")
    nan_pct = float(arr.isna().mean() * 100.0)
    arr_finite = arr[np.isfinite(arr)]
    if len(arr_finite) == 0:
        return {
            "name": ser.name,
            "mean": None,
            "min": None,
            "max": None,
            "nan_pct": nan_pct,
        }
    return {
        "name": ser.name,
        "mean": float(arr_finite.mean()),
        "min": float(arr_finite.min()),
        "max": float(arr_finite.max()),
        "nan_pct": nan_pct,
    }


def _fmt_or_na(value: Any) -> str:
    return f"{value:.4g}" if value is not None else "n/a"


def _build_text_message(data: dict[str, Any]) -> str:
    period = data["period"]
    lines = [
        f"Run directory: {data['run_dir']}",
        f"Period: {period.get('start')} -> {period.get('end')} ({data['n_steps']} steps)",
        "",
        "Variable summary:",
        f"  {'name':<20} {'mean':>12} {'min':>12} {'max':>12} {'nan_pct':>8}",
    ]
    for var in data["variables"]:
        lines.append(
            f"  {var['name']:<20} "
            f"{_fmt_or_na(var['mean']):>12} "
            f"{_fmt_or_na(var['min']):>12} "
            f"{_fmt_or_na(var['max']):>12} "
            f"{var['nan_pct']:>7.1f}%"
        )
    if data.get("notes"):
        lines.append("")
        lines.append("Notes:")
        for note in data["notes"]:
            lines.append(f"  - {note}")
    return "\n".join(lines)


@click.command(
    name="summarise",
    short_help="Summarise a SUEWS run output.",
    help=(
        "Print a per-variable summary (mean / min / max / NaN-percentage) of "
        "a SUEWS run output. Pass --variables to restrict the columns; the "
        "default is every numeric column."
    ),
)
@click.argument(
    "run_dir",
    type=click.Path(exists=True, file_okay=False, dir_okay=True),
)
@click.option(
    "--variables",
    default="",
    show_default=False,
    help=(
        "Comma-separated variable names to summarise. Empty (default) "
        "means every numeric column."
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
def summarise_output_cmd(run_dir: str, variables: str, output_format: str) -> None:
    """Implementation of ``suews summarise``."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "summarise", *sys.argv[1:]])

    path_run_dir = Path(run_dir)

    try:
        df_output = _load_run_dataframe(path_run_dir)
    except (FileNotFoundError, OSError, ValueError) as exc:
        message = f"Failed to load run output: {exc}"
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"run_dir": str(path_run_dir)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    list_notes: list[str] = []

    list_variables = [v.strip() for v in variables.split(",") if v.strip()]
    if list_variables:
        list_missing = [v for v in list_variables if v not in df_output.columns]
        list_present = [v for v in list_variables if v in df_output.columns]
        for missing in list_missing:
            list_notes.append(f"variable {missing!r} not found in output")
        df_subset = df_output[list_present]
    else:
        df_subset = df_output.select_dtypes(include=[np.number])
        if df_subset.shape[1] == 0:
            list_notes.append("no numeric columns detected in run output")

    list_summaries = [
        _summarise_variable(df_subset[col]) for col in df_subset.columns
    ]

    period_data = _period_and_n_steps(df_output)

    data: dict[str, Any] = {
        "run_dir": str(path_run_dir),
        "period": period_data["period"],
        "n_steps": period_data["n_steps"],
        "variables": list_summaries,
        "notes": list_notes,
    }

    if json_mode:
        Envelope.success(
            data=data,
            command=command,
            warnings=list_notes or None,
            started_at=started_at,
        ).emit()
    else:
        click.echo(_build_text_message(data))
