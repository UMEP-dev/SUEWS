"""``suews summarise`` -- produce a per-variable summary of a SUEWS run.

For each requested variable (or every numeric column when ``--variables``
is empty) the command reports the mean / min / max / NaN-percentage
alongside the overall time period and step count. Multi-group Parquet
outputs preserve each ``(group, variable)`` series independently.
"""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
import sys
from typing import Any, NamedTuple

import click
import numpy as np
import pandas as pd

from .._run_output import (
    _COMMON_OUTPUT_VARIABLES,
    _load_run_output_dataframe,
    _pick_variable_level,
)
from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso


class _ColumnSpec(NamedTuple):
    """Positional descriptor for a single output column."""

    pos: int
    group: str | None
    name: str

    @property
    def qualified_name(self) -> str:
        """Qualified ``group.name`` label when grouped, otherwise ``name``."""
        return f"{self.group}.{self.name}" if self.group is not None else self.name


def _extract_column_specs(columns: pd.Index) -> list[_ColumnSpec]:
    """Describe every column by positional index, optional group, and variable name."""
    if isinstance(columns, pd.MultiIndex):
        var_level = _pick_variable_level(columns)
        list_group_levels = [
            lvl for lvl in range(columns.nlevels) if lvl != var_level
        ]
        list_level_names = [
            str(level_name).lower() if level_name is not None else ""
            for level_name in columns.names
        ]
        named_group_level = (
            list_level_names.index("group") if "group" in list_level_names else None
        )
        list_specs: list[_ColumnSpec] = []
        for pos, col in enumerate(columns):
            var_name = str(col[var_level])
            if not list_group_levels:
                group_name: str | None = None
            elif len(list_group_levels) == 1:
                group_name = str(col[list_group_levels[0]])
            elif named_group_level is not None and named_group_level != var_level:
                group_name = str(col[named_group_level])
            else:
                group_name = ".".join(str(col[lvl]) for lvl in list_group_levels)
            list_specs.append(_ColumnSpec(pos=pos, group=group_name, name=var_name))
        return list_specs

    if any(isinstance(col, tuple) for col in columns):
        list_specs = []
        for pos, col in enumerate(columns):
            if isinstance(col, tuple) and len(col) >= 2:
                if col[1] in _COMMON_OUTPUT_VARIABLES:
                    group_name, var_name = str(col[0]), str(col[1])
                elif col[0] in _COMMON_OUTPUT_VARIABLES:
                    var_name = str(col[0])
                    group_name = ".".join(str(part) for part in col[1:])
                else:
                    group_name = ".".join(str(part) for part in col[:-1])
                    var_name = str(col[-1])
                list_specs.append(_ColumnSpec(pos=pos, group=group_name, name=var_name))
            elif isinstance(col, tuple) and len(col) == 1:
                list_specs.append(_ColumnSpec(pos=pos, group=None, name=str(col[0])))
            else:
                list_specs.append(_ColumnSpec(pos=pos, group=None, name=str(col)))
        return list_specs

    return [_ColumnSpec(pos=pos, group=None, name=str(col)) for pos, col in enumerate(columns)]


def _select_columns(
    df_output: pd.DataFrame,
    list_variables: list[str],
) -> tuple[list[_ColumnSpec], list[str]]:
    """Resolve requested variable tokens (or all numeric columns) to column specs."""
    list_all_specs = _extract_column_specs(df_output.columns)
    list_notes: list[str] = []

    if not list_variables:
        df_positional = df_output.copy(deep=False)
        df_positional.columns = pd.RangeIndex(df_output.shape[1])
        list_numeric_pos = list(df_positional.select_dtypes(include=[np.number]).columns)
        list_selected = [list_all_specs[pos] for pos in list_numeric_pos]
        if not list_selected:
            list_notes.append("no numeric columns detected in run output")
        return list_selected, list_notes

    dict_by_flat_name: dict[str, list[_ColumnSpec]] = defaultdict(list)
    dict_by_qualified: dict[tuple[str, str], list[_ColumnSpec]] = defaultdict(list)
    dict_by_name: dict[str, list[_ColumnSpec]] = defaultdict(list)
    for spec in list_all_specs:
        dict_by_name[spec.name].append(spec)
        if spec.group is None:
            dict_by_flat_name[spec.name].append(spec)
        else:
            dict_by_qualified[spec.group, spec.name].append(spec)

    list_selected = []
    set_seen_pos: set[int] = set()
    for token in list_variables:
        is_qualified = False
        if token in dict_by_flat_name:
            list_matches = dict_by_flat_name[token]
        elif "." in token:
            req_group, req_var = token.split(".", 1)
            list_matches = dict_by_qualified.get((req_group, req_var), [])
            is_qualified = True
        else:
            list_matches = dict_by_name.get(token, [])

        if not list_matches:
            list_notes.append(f"variable {token!r} not found in output")
            continue

        if not is_qualified and len(list_matches) > 1:
            list_groups = [spec.group for spec in list_matches if spec.group is not None]
            if len(set(list_groups)) > 1:
                qualified_examples = ", ".join(
                    spec.qualified_name for spec in list_matches
                )
                list_notes.append(
                    f"variable {token!r} matches multiple groups "
                    f"({', '.join(list_groups)}); summarising all matches "
                    f"(use e.g. {qualified_examples} to select one)"
                )

        for spec in list_matches:
            if spec.pos not in set_seen_pos:
                set_seen_pos.add(spec.pos)
                list_selected.append(spec)

    return list_selected, list_notes


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


def _summarise_variable(
    ser: pd.Series,
    *,
    name: str | None = None,
    group: str | None = None,
) -> dict[str, Any]:
    """Compute NaN-aware summary statistics for a single column."""
    var_name = str(ser.name) if name is None else name
    arr = pd.to_numeric(ser, errors="coerce")
    nan_pct = 0.0 if len(arr) == 0 else float(arr.isna().mean() * 100.0)
    arr_finite = arr[np.isfinite(arr)]

    entry: dict[str, Any] = {}
    if group is not None:
        entry["group"] = group
    entry["name"] = var_name

    if len(arr_finite) == 0:
        entry.update({
            "mean": None,
            "min": None,
            "max": None,
            "nan_pct": nan_pct,
        })
        return entry

    entry.update({
        "mean": float(arr_finite.mean()),
        "min": float(arr_finite.min()),
        "max": float(arr_finite.max()),
        "nan_pct": nan_pct,
    })
    return entry


def _fmt_or_na(value: Any) -> str:
    return f"{value:.4g}" if value is not None else "n/a"


def _build_text_message(data: dict[str, Any]) -> str:
    period = data["period"]
    list_vars = data["variables"]
    has_group = any(var.get("group") is not None for var in list_vars)
    name_width = max([20, *(len(str(var["name"])) for var in list_vars)])

    lines = [
        f"Run directory: {data['run_dir']}",
        f"Period: {period.get('start')} -> {period.get('end')} ({data['n_steps']} steps)",
        "",
        "Variable summary:",
    ]
    if has_group:
        group_width = max([
            10,
            *(len(str(var.get("group") or "")) for var in list_vars),
        ])
        lines.append(
            f"  {'group':<{group_width}} {'name':<{name_width}} "
            f"{'mean':>12} {'min':>12} {'max':>12} {'nan_pct':>8}"
        )
        for var in list_vars:
            lines.append(
                f"  {(var.get('group') or ''):<{group_width}} "
                f"{var['name']:<{name_width}} "
                f"{_fmt_or_na(var['mean']):>12} "
                f"{_fmt_or_na(var['min']):>12} "
                f"{_fmt_or_na(var['max']):>12} "
                f"{var['nan_pct']:>7.1f}%"
            )
    else:
        lines.append(
            f"  {'name':<{name_width}} {'mean':>12} {'min':>12} {'max':>12} {'nan_pct':>8}"
        )
        for var in list_vars:
            lines.append(
                f"  {var['name']:<{name_width}} "
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
        "a SUEWS run directory, streamed Parquet dataset directory or "
        "standalone output file. Multi-group Parquet outputs preserve each "
        "(group, variable) series independently. Pass --variables to restrict "
        "the columns using bare names (e.g. QH,QE,QN) or group-qualified "
        "names (e.g. SUEWS.QS,ESTM.QS); the default is every numeric column.\n\n"
        "Examples:\n\n"
        "\b\n"
        "  suews summarise path/to/run_dir\n"
        "  suews summarise path/to/run_dir --variables QH,QE,QN\n"
        "  suews summarise path/to/run_dir --variables SUEWS.QS,ESTM.QS"
    ),
)
@click.argument(
    "run_dir",
    type=click.Path(exists=True),
)
@click.option(
    "--variables",
    default="",
    show_default=False,
    help=(
        "Comma-separated variable names (e.g. QH,QE,QN) or group-qualified "
        "names (e.g. SUEWS.QS,ESTM.QS). A bare name that appears in multiple "
        "groups summarises all matching groups with a note. Empty (default) "
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
    """Summarise a saved SUEWS run output."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "summarise", *sys.argv[1:]])

    path_run_dir = Path(run_dir)

    try:
        df_output = _load_run_output_dataframe(path_run_dir, flatten=False)
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

    list_variables = [v.strip() for v in variables.split(",") if v.strip()]
    list_selected, list_notes = _select_columns(df_output, list_variables)
    list_summaries = [
        _summarise_variable(
            df_output.iloc[:, spec.pos],
            name=spec.name,
            group=spec.group,
        )
        for spec in list_selected
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
