"""Helpers for loading saved SUEWS run outputs.

The command-line diagnostics operate on files written by ``suews run`` /
``SUEWSSimulation.save``. Those files may be legacy text outputs, synthetic
``df_output`` fixtures used in tests, or the current parquet artefacts.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pandas as pd

_COMMON_OUTPUT_VARIABLES = frozenset({
    "QH",
    "QE",
    "QN",
    "QS",
    "QF",
    "Tair",
    "RH",
    "Kdown",
    "UStar",
    "Lob",
})

_PARQUET_PATTERNS = ("df_output*.parquet", "*SUEWS_output.parquet")
_CSV_PATTERNS = ("df_output*.csv",)
_TEXT_PATTERNS = ("*_SUEWS_*.txt",)


def _candidate_patterns(include_text: bool = True) -> tuple[str, ...]:
    patterns = (*_PARQUET_PATTERNS, *_CSV_PATTERNS)
    if include_text:
        patterns = (*patterns, *_TEXT_PATTERNS)
    return patterns


def _list_run_output_files(
    path_run_dir: Path, *, include_text: bool = True
) -> list[Path]:
    path_run_dir = Path(path_run_dir)
    if not path_run_dir.exists() or not path_run_dir.is_dir():
        return []

    list_ranked: list[tuple[int, str, Path]] = []
    set_seen: set[Path] = set()
    for priority, pattern in enumerate(_candidate_patterns(include_text)):
        for path in path_run_dir.rglob(pattern):
            path_resolved = path.resolve()
            if not path.is_file() or path_resolved in set_seen:
                continue
            set_seen.add(path_resolved)
            list_ranked.append((priority, path.as_posix(), path))

    return [path for _, _, path in sorted(list_ranked)]


def _read_output_file(path_output: Path) -> pd.DataFrame:
    suffix = path_output.suffix.lower()
    if suffix == ".parquet":
        return pd.read_parquet(path_output)
    if suffix == ".txt":
        return pd.read_csv(path_output, sep=r"\s+", engine="python")
    return pd.read_csv(path_output)


def _pick_variable_level(columns: pd.MultiIndex) -> int:
    names = [str(name).lower() if name is not None else "" for name in columns.names]
    list_candidates: list[int] = []

    for target in ("var", "variable"):
        if target in names:
            list_candidates.append(names.index(target))

    list_candidates.extend(range(columns.nlevels))

    set_seen: set[int] = set()
    for level in list_candidates:
        if level in set_seen:
            continue
        set_seen.add(level)
        values = columns.get_level_values(level)
        if any(value in _COMMON_OUTPUT_VARIABLES for value in values):
            return level

    for target in ("var", "variable"):
        if target in names:
            return names.index(target)

    return 0


def _flatten_tuple_label(label: Any) -> Any:
    if not isinstance(label, tuple):
        return label
    for value in label:
        if value in _COMMON_OUTPUT_VARIABLES:
            return value
    return label[0] if label else label


def _flatten_output_columns(df_output: pd.DataFrame) -> pd.DataFrame:
    if isinstance(df_output.columns, pd.MultiIndex):
        level = _pick_variable_level(df_output.columns)
        df_output = df_output.copy()
        df_output.columns = df_output.columns.get_level_values(level)
        return df_output

    if any(isinstance(col, tuple) for col in df_output.columns):
        df_output = df_output.copy()
        df_output.columns = [_flatten_tuple_label(col) for col in df_output.columns]
    return df_output


def _format_tier(path_output: Path) -> int:
    """Rank an output file by format: parquet (0), CSV (1), legacy text (2)."""
    suffix = path_output.suffix.lower()
    if suffix == ".parquet":
        return 0
    if suffix == ".csv":
        return 1
    return 2


def _select_output_partitions(list_paths: list[Path]) -> list[Path]:
    """Keep only the files of the highest-priority format present.

    ``SUEWSSimulation.save`` can write the same run as parquet, CSV or
    legacy text. Reading more than one format would count the same rows
    twice, so partitions are drawn from a single format tier.
    """
    if not list_paths:
        return []
    tier_best = min(_format_tier(path) for path in list_paths)
    return [path for path in list_paths if _format_tier(path) == tier_best]


def _select_core_group(df_output: pd.DataFrame) -> pd.DataFrame:
    """Restrict MultiIndex columns to the core ``SUEWS`` output group.

    The canonical parquet output stacks every output group under one
    column MultiIndex, and some variable names recur across groups (for
    example ``QS`` in both ``SUEWS`` and ``ESTM``). Flattening such a
    frame to the variable level would leave duplicate column labels, so
    partitions keep only the core group when it is present.
    """
    if not isinstance(df_output.columns, pd.MultiIndex):
        return df_output
    for level in range(df_output.columns.nlevels):
        if "SUEWS" in df_output.columns.get_level_values(level):
            return df_output.xs("SUEWS", axis=1, level=level, drop_level=False)
    return df_output


def _load_run_output_partitions(
    path: Path,
    *,
    include_text: bool = True,
) -> list[tuple[str, pd.DataFrame]]:
    """Load every partition of a run output as ``(label, DataFrame)`` pairs.

    A partition is one output file of the highest-priority format present
    (see :func:`_select_output_partitions`), restricted to the core
    ``SUEWS`` column group when the file stacks several groups (see
    :func:`_select_core_group`), and, for files whose row index
    is a MultiIndex such as the canonical ``(grid, datetime)`` layout, one
    grid within that file. Legacy text output is already split per grid
    and per year on disk. Labels are ``"<file name>"`` or
    ``"<file name>[grid=<grid>]"``.

    Raises ``FileNotFoundError`` when ``path`` is a directory without any
    recognised output file.
    """
    path = Path(path)
    if path.is_dir():
        list_paths = _select_output_partitions(
            _list_run_output_files(path, include_text=include_text)
        )
        if not list_paths:
            patterns = " / ".join(_candidate_patterns(include_text))
            raise FileNotFoundError(
                f"No recognised SUEWS output ({patterns}) under {path}"
            )
    else:
        list_paths = [path]

    list_partitions: list[tuple[str, pd.DataFrame]] = []
    for path_file in list_paths:
        df_file = _flatten_output_columns(
            _select_core_group(_read_output_file(path_file))
        )
        if isinstance(df_file.index, pd.MultiIndex) and len(df_file.index):
            level_values = df_file.index.get_level_values(0)
            for grid in level_values.unique():
                list_partitions.append((
                    f"{path_file.name}[grid={grid}]",
                    df_file.xs(grid, level=0),
                ))
        else:
            list_partitions.append((path_file.name, df_file))
    return list_partitions


def _load_run_output_dataframe(
    path: Path,
    *,
    include_text: bool = True,
) -> pd.DataFrame:
    """Load the first recognised output file under ``path`` (or ``path`` itself).

    ``suews summarise`` / ``suews compare`` operate on a single time series
    and keep this first-file contract; diagnostics that must see every
    partition use :func:`_load_run_output_partitions` instead.
    """
    path = Path(path)
    if path.is_dir():
        list_paths = _list_run_output_files(path, include_text=include_text)
        if not list_paths:
            patterns = " / ".join(_candidate_patterns(include_text))
            raise FileNotFoundError(
                f"No recognised SUEWS output ({patterns}) under {path}"
            )
        path = list_paths[0]

    return _flatten_output_columns(_read_output_file(path))


_LEGACY_CLOCK_COLUMNS = ("Year", "DOY", "Hour", "Min")
_DATETIME_COLUMN_CANDIDATES = ("datetime", "Datetime", "DateTime")


def _has_legacy_clock(df_output: pd.DataFrame) -> bool:
    """Return True when the frame carries the native SUEWS clock columns."""
    return all(name in df_output.columns for name in _LEGACY_CLOCK_COLUMNS)


def _recover_datetime_index(df_output: pd.DataFrame) -> pd.DataFrame:
    """Return ``df_output`` indexed by time when a time axis is recoverable.

    Three layouts are recognised, in this order:

    1. an existing ``DatetimeIndex`` (canonical parquet after the grid level
       has been removed) is kept as it is;
    2. the native text layout written by ``SUEWSSimulation.save`` carries the
       clock as ``Year`` / ``DOY`` / ``Hour`` / ``Min`` columns (plus a
       derived ``Dectime``); these are combined into a ``DatetimeIndex`` and
       dropped from the columns;
    3. a ``datetime`` column (any capitalisation) is parsed and set as index.

    Any other frame is returned unchanged, so callers can decide whether a
    frame without a time axis is acceptable. ``ValueError`` is raised when
    the clock columns are present but do not parse.
    """
    if isinstance(df_output.index, pd.DatetimeIndex):
        return df_output

    if _has_legacy_clock(df_output):
        clock = df_output[list(_LEGACY_CLOCK_COLUMNS)].apply(
            pd.to_numeric, errors="coerce"
        )
        if clock.isna().any().any():
            raise ValueError(
                "Legacy clock columns Year/DOY/Hour/Min contain non-numeric values"
            )
        year_start = pd.to_datetime(clock["Year"].astype(int).astype(str), format="%Y")
        index = pd.DatetimeIndex(
            year_start
            + pd.to_timedelta(clock["DOY"].astype(int) - 1, unit="D")
            + pd.to_timedelta(clock["Hour"].astype(int), unit="h")
            + pd.to_timedelta(clock["Min"].astype(int), unit="min"),
            name="datetime",
        )
        list_drop = [*_LEGACY_CLOCK_COLUMNS, "Dectime"]
        df_output = df_output.drop(
            columns=[name for name in list_drop if name in df_output.columns]
        )
        return df_output.set_index(index)

    for cand in _DATETIME_COLUMN_CANDIDATES:
        if cand in df_output.columns:
            index = pd.DatetimeIndex(
                pd.to_datetime(df_output[cand], format="mixed"), name="datetime"
            )
            return df_output.drop(columns=[cand]).set_index(index)

    return df_output
