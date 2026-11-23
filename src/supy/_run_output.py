"""Helpers for loading saved SUEWS run outputs.

The command-line diagnostics operate on outputs written by ``suews run`` /
``SUEWSSimulation.save``. Those outputs may be legacy text files, synthetic
``df_output`` fixtures used in tests, monolithic Parquet files, or canonical
streamed Parquet dataset directories holding committed
``part-YYYY-CCC.parquet`` chunk files.
"""

from __future__ import annotations

from collections.abc import Iterator
from copy import deepcopy
from dataclasses import dataclass, field
import fnmatch
import importlib
from pathlib import Path
import re
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

_OUTPUT_TIMESTAMP_SUFFIXES: dict[str, str] = {
    "follow": "",
    "utc": "_UTC",
    "local_standard_time": "_STANDARD",
    "daylight": "_DAYLIGHT",
}
_CANONICAL_PARQUET_PATTERNS = tuple(
    f"*SUEWS_output{suffix}.parquet" for suffix in _OUTPUT_TIMESTAMP_SUFFIXES.values()
)
_PARQUET_PATTERNS = ("df_output*.parquet", *_CANONICAL_PARQUET_PATTERNS)
_CSV_PATTERNS = ("df_output*.csv",)
_TEXT_PATTERNS = ("*_SUEWS_*.txt",)

_STREAMED_PARQUET_PART_RE = re.compile(
    r"^part-(?P<year>\d{4})-(?P<chunk>\d{3})\.parquet$"
)
_INVARIANT_ARROW_FIELD_KEYS = (
    b"units",
    b"interval",
    b"cell_methods",
    b"units_metadata",
)
_COVERAGE_ATTR_KEYS = (
    "supy_output_coverage_s",
    "supy_output_daily_coverage_s",
)


def _candidate_patterns(include_text: bool = True) -> tuple[str, ...]:
    patterns = (*_PARQUET_PATTERNS, *_CSV_PATTERNS)
    if include_text:
        patterns = (*patterns, *_TEXT_PATTERNS)
    return patterns


def _is_canonical_parquet_name(name: str) -> bool:
    """Return True when ``name`` matches a canonical SUEWS Parquet output."""
    return any(
        fnmatch.fnmatchcase(name, pattern) for pattern in _CANONICAL_PARQUET_PATTERNS
    )


def _is_canonical_parquet_dataset_dir(path: Path) -> bool:
    """Return True when ``path`` is a directory named as a canonical Parquet output."""
    return path.is_dir() and _is_canonical_parquet_name(path.name)


def _list_streamed_parquet_parts(path_dataset: Path) -> list[Path]:
    """Return committed writer part files of a streamed Parquet dataset in order.

    Only regular files directly inside ``path_dataset`` matching the writer's
    committed ``part-YYYY-CCC.parquet`` naming scheme are returned, ordered by
    ``(year, chunk)``. Temporary files, sidecars, subdirectories, and retained
    unrelated notes are ignored.
    """
    if not path_dataset.is_dir():
        return []
    list_parts: list[tuple[int, int, str, Path]] = []
    for path_entry in path_dataset.iterdir():
        if not path_entry.is_file():
            continue
        match = _STREAMED_PARQUET_PART_RE.match(path_entry.name)
        if match is None:
            continue
        list_parts.append((
            int(match["year"]),
            int(match["chunk"]),
            path_entry.name,
            path_entry,
        ))
    return [path_part for _, _, _, path_part in sorted(list_parts)]


def _pattern_priority(path: Path, *, include_text: bool = True) -> int | None:
    """Return the discovery priority rank for an output file or streamed dataset."""
    is_dir = path.is_dir()
    if is_dir:
        if not _is_canonical_parquet_dataset_dir(path):
            return None
        if not _list_streamed_parquet_parts(path):
            return None
    elif not path.is_file():
        return None

    for priority, pattern in enumerate(_candidate_patterns(include_text)):
        if is_dir and pattern not in _CANONICAL_PARQUET_PATTERNS:
            continue
        if fnmatch.fnmatchcase(path.name, pattern):
            return priority
    return None


def _walk_run_dir(path_root: Path) -> Iterator[Path]:
    """Yield files and canonical Parquet dataset directories under ``path_root``.

    Traversal stops at canonical Parquet dataset directories so temporary
    files, individual chunk parts, or unrelated retained notes inside a dataset
    folder are never discovered as standalone run outputs.
    """
    list_pending = [path_root]
    set_seen_dirs: set[Path] = set()
    while list_pending:
        path_current = list_pending.pop()
        path_resolved = path_current.resolve()
        if path_resolved in set_seen_dirs:
            continue
        set_seen_dirs.add(path_resolved)
        try:
            list_entries = list(path_current.iterdir())
        except OSError:
            continue
        for path_entry in list_entries:
            if path_entry.is_dir():
                if _is_canonical_parquet_dataset_dir(path_entry):
                    yield path_entry
                elif not path_entry.is_symlink():
                    list_pending.append(path_entry)
            elif path_entry.is_file():
                yield path_entry


def _list_run_output_files(
    path_run_dir: Path, *, include_text: bool = True
) -> list[Path]:
    """List recognised SUEWS output files and streamed Parquet datasets.

    Accepts a run directory, a canonical streamed Parquet dataset directory
    directly, or a standalone recognised output file. Empty or temporary-only
    Parquet dataset directories are ignored rather than reported as outputs.
    """
    path_run_dir = Path(path_run_dir)
    if not path_run_dir.exists():
        return []

    if path_run_dir.is_file() or _is_canonical_parquet_dataset_dir(path_run_dir):
        if _pattern_priority(path_run_dir, include_text=include_text) is not None:
            return [path_run_dir]
        return []

    if not path_run_dir.is_dir():
        return []

    dict_ranked: dict[Path, tuple[int, str, Path]] = {}
    for path in _walk_run_dir(path_run_dir):
        priority = _pattern_priority(path, include_text=include_text)
        if priority is None:
            continue
        path_resolved = path.resolve()
        candidate = (priority, path.as_posix(), path)
        existing = dict_ranked.get(path_resolved)
        if existing is None or candidate[:2] < existing[:2]:
            dict_ranked[path_resolved] = candidate

    return [path for _, _, path in sorted(dict_ranked.values())]


def _format_columns(list_cols: list[Any], limit: int = 10) -> str:
    """Format column identifiers in ASCII for error messages."""
    shown = [repr(col) for col in list_cols[:limit]]
    if len(list_cols) > limit:
        shown.append(f"... (+{len(list_cols) - limit} more)")
    return ", ".join(shown)


def _column_dtype_kind(dtype: Any) -> str:
    """Classify a pandas dtype into broad compatible families."""
    if pd.api.types.is_datetime64_any_dtype(dtype):
        return "datetime"
    if pd.api.types.is_bool_dtype(dtype):
        return "bool"
    if pd.api.types.is_numeric_dtype(dtype):
        return "numeric"
    return "other"


def _normalise_interval_attr(value: Any) -> Any:
    """Normalise numeric interval attrs so int and float representations agree."""
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return float(value)
    return value


def _normalise_units_attr(value: Any) -> dict[str, dict[str, str]]:
    """Normalise ``supy_output_units`` mapping (absent and empty agree)."""
    if not isinstance(value, dict):
        return {}
    return {
        str(group): {str(var): str(unit) for var, unit in mapping.items()}
        for group, mapping in value.items()
        if isinstance(mapping, dict) and mapping
    }


def _normalise_water_rate_attr(value: Any) -> frozenset[tuple[str, ...]]:
    """Normalise ``supy_water_rate_columns`` independently of list/tuple order."""
    if not value:
        return frozenset()
    return frozenset(
        tuple(str(item) for item in entry)
        if isinstance(entry, (list, tuple))
        else (str(entry),)
        for entry in value
    )


def _schema_error(
    path_dataset: Path,
    path_ref: Path,
    path_part: Path,
    detail: str,
) -> ValueError:
    """Build an explicit ValueError naming dataset, part files, and mismatch."""
    return ValueError(
        f"Incompatible schema in streamed Parquet dataset {path_dataset.name!r}: "
        f"{path_part.name!r} does not match {path_ref.name!r} ({detail})"
    )


def _validate_and_align_part_columns(
    path_dataset: Path,
    path_ref: Path,
    cols_ref: pd.Index,
    path_part: Path,
    df_part: pd.DataFrame,
) -> pd.DataFrame:
    """Validate that ``df_part`` matches ``cols_ref`` and align harmless reordering."""
    cols_part = df_part.columns
    if (
        isinstance(cols_part, pd.MultiIndex) != isinstance(cols_ref, pd.MultiIndex)
        or cols_part.nlevels != cols_ref.nlevels
        or tuple(cols_part.names) != tuple(cols_ref.names)
    ):
        raise _schema_error(
            path_dataset,
            path_ref,
            path_part,
            f"column index structure {type(cols_part).__name__}"
            f"(names={list(cols_part.names)!r}) != "
            f"{type(cols_ref).__name__}(names={list(cols_ref.names)!r})",
        )

    if cols_part.equals(cols_ref):
        return df_part

    set_ref = set(cols_ref)
    set_part = set(cols_part)
    list_missing = [col for col in cols_ref if col not in set_part]
    list_extra = [col for col in cols_part if col not in set_ref]
    if list_missing or list_extra:
        list_details: list[str] = []
        if list_missing:
            list_details.append(f"missing columns: {_format_columns(list_missing)}")
        if list_extra:
            list_details.append(f"unexpected columns: {_format_columns(list_extra)}")
        raise _schema_error(
            path_dataset,
            path_ref,
            path_part,
            "; ".join(list_details),
        )

    if (
        len(cols_part) != len(cols_ref)
        or cols_ref.has_duplicates
        or cols_part.has_duplicates
    ):
        list_dups = list(
            dict.fromkeys(
                list(cols_part[cols_part.duplicated()])
                + list(cols_ref[cols_ref.duplicated()])
            )
        )
        raise _schema_error(
            path_dataset,
            path_ref,
            path_part,
            f"ambiguous duplicate column labels: {_format_columns(list_dups)}",
        )

    df_aligned = df_part.reindex(columns=cols_ref)
    df_aligned.attrs = dict(df_part.attrs)
    return df_aligned


def _validate_part_index(
    path_dataset: Path,
    path_ref: Path,
    idx_ref: pd.Index,
    ref_has_rows: bool,
    path_part: Path,
    df_part: pd.DataFrame,
) -> None:
    """Validate row index structure and level dtype compatibility across parts."""
    idx_part = df_part.index
    if (
        isinstance(idx_part, pd.MultiIndex) != isinstance(idx_ref, pd.MultiIndex)
        or idx_part.nlevels != idx_ref.nlevels
        or tuple(idx_part.names) != tuple(idx_ref.names)
    ):
        raise _schema_error(
            path_dataset,
            path_ref,
            path_part,
            f"row index structure {type(idx_part).__name__}"
            f"(names={list(idx_part.names)!r}) != "
            f"{type(idx_ref).__name__}(names={list(idx_ref.names)!r})",
        )

    if not ref_has_rows or len(df_part) == 0:
        return

    for level in range(idx_ref.nlevels):
        lvl_ref = (
            idx_ref.get_level_values(level)
            if isinstance(idx_ref, pd.MultiIndex)
            else idx_ref
        )
        lvl_part = (
            idx_part.get_level_values(level)
            if isinstance(idx_part, pd.MultiIndex)
            else idx_part
        )
        level_label = (
            idx_ref.names[level] if idx_ref.names[level] is not None else f"level {level}"
        )
        kind_ref = _column_dtype_kind(lvl_ref.dtype)
        kind_part = _column_dtype_kind(lvl_part.dtype)
        if kind_ref != kind_part:
            raise _schema_error(
                path_dataset,
                path_ref,
                path_part,
                f"index level {level_label!r} dtype {lvl_part.dtype} != {lvl_ref.dtype}",
            )
        if kind_ref == "datetime":
            tz_ref = getattr(lvl_ref.dtype, "tz", None)
            tz_part = getattr(lvl_part.dtype, "tz", None)
            if tz_ref != tz_part:
                raise _schema_error(
                    path_dataset,
                    path_ref,
                    path_part,
                    f"index level {level_label!r} timezone {tz_part!r} != {tz_ref!r}",
                )


def _validate_part_dtypes(
    path_dataset: Path,
    cols_ref: pd.Index,
    dict_ref_dtypes: dict[int, tuple[Path, Any, str]],
    path_part: Path,
    df_part: pd.DataFrame,
) -> None:
    """Validate column dtype compatibility across parts with non-null values.

    All-NaN columns in a part are tolerated so present-but-NaN fields flow to
    diagnostics rather than being misclassified as schema errors.
    """
    for pos in range(len(cols_ref)):
        ser_part = df_part.iloc[:, pos]
        if not ser_part.notna().any():
            continue
        kind_part = _column_dtype_kind(ser_part.dtype)
        ref_entry = dict_ref_dtypes.get(pos)
        if ref_entry is None:
            dict_ref_dtypes[pos] = (path_part, ser_part.dtype, kind_part)
            continue
        path_dtype_ref, dtype_ref, kind_ref = ref_entry
        if kind_part != kind_ref:
            raise _schema_error(
                path_dataset,
                path_dtype_ref,
                path_part,
                f"column {cols_ref[pos]!r} dtype {ser_part.dtype} != {dtype_ref}",
            )


@dataclass
class _DatasetSchemaRef:
    """Reference schema and invariant metadata accumulated across dataset parts."""

    path_dataset: Path
    path_ref: Path
    cols_ref: pd.Index
    attrs_ref: dict[str, Any]
    path_idx_ref: Path
    idx_ref: pd.Index
    idx_ref_has_rows: bool
    ref_dtypes: dict[int, tuple[Path, Any, str]] = field(default_factory=dict)
    interval_ref: tuple[Path, Any] | None = None
    arrow_version_ref: tuple[Path, str] | None = None
    arrow_fields_ref: dict[str, dict[str, tuple[Path, str]]] = field(
        default_factory=dict
    )

    @classmethod
    def from_first_part(
        cls,
        path_dataset: Path,
        path_ref: Path,
        df_ref: pd.DataFrame,
        arrow_version: str | None,
        dict_arrow_fields: dict[str, dict[str, str]],
    ) -> _DatasetSchemaRef:
        """Build the initial schema reference from the first committed part."""
        ref = cls(
            path_dataset=path_dataset,
            path_ref=path_ref,
            cols_ref=df_ref.columns,
            attrs_ref=dict(df_ref.attrs),
            path_idx_ref=path_ref,
            idx_ref=df_ref.index,
            idx_ref_has_rows=len(df_ref) > 0,
            interval_ref=(
                (
                    path_ref,
                    _normalise_interval_attr(df_ref.attrs["supy_output_interval_s"]),
                )
                if "supy_output_interval_s" in df_ref.attrs
                else None
            ),
            arrow_version_ref=(
                (path_ref, arrow_version) if arrow_version is not None else None
            ),
            arrow_fields_ref={
                name: {key: (path_ref, value) for key, value in meta.items()}
                for name, meta in dict_arrow_fields.items()
            },
        )
        for pos in range(len(df_ref.columns)):
            ser_ref = df_ref.iloc[:, pos]
            if ser_ref.notna().any():
                ref.ref_dtypes[pos] = (
                    path_ref,
                    ser_ref.dtype,
                    _column_dtype_kind(ser_ref.dtype),
                )
        return ref


def _read_arrow_contract_metadata(
    path_part: Path,
) -> tuple[str | None, dict[str, dict[str, str]]]:
    """Read invariant SUEWS Arrow schema and field metadata from a part footer."""
    pq = importlib.import_module("pyarrow.parquet")

    schema = pq.read_schema(path_part)
    schema_meta = schema.metadata or {}
    raw_version = schema_meta.get(b"output_contract_version")
    version = raw_version.decode("utf-8", errors="replace") if raw_version else None

    dict_field_meta: dict[str, dict[str, str]] = {}
    for arrow_field in schema:
        if not arrow_field.metadata:
            continue
        extracted = {
            key.decode("ascii"): arrow_field.metadata[key].decode(
                "utf-8", errors="replace"
            )
            for key in _INVARIANT_ARROW_FIELD_KEYS
            if key in arrow_field.metadata
        }
        if extracted:
            dict_field_meta[arrow_field.name] = extracted
    return version, dict_field_meta


def _validate_part_metadata(
    ref: _DatasetSchemaRef,
    path_part: Path,
    attrs_part: dict[str, Any],
    arrow_version_part: str | None,
    dict_arrow_fields_part: dict[str, dict[str, str]],
) -> None:
    """Validate invariant SUEWS metadata across parts while allowing coverage to vary."""
    if "supy_output_interval_s" in attrs_part:
        norm_interval = _normalise_interval_attr(attrs_part["supy_output_interval_s"])
        if ref.interval_ref is None:
            ref.interval_ref = (path_part, norm_interval)
        elif norm_interval != ref.interval_ref[1]:
            raise _schema_error(
                ref.path_dataset,
                ref.interval_ref[0],
                path_part,
                f"supy_output_interval_s {norm_interval!r} != {ref.interval_ref[1]!r}",
            )

    units_ref = _normalise_units_attr(ref.attrs_ref.get("supy_output_units"))
    units_part = _normalise_units_attr(attrs_part.get("supy_output_units"))
    if units_part != units_ref:
        raise _schema_error(
            ref.path_dataset,
            ref.path_ref,
            path_part,
            f"supy_output_units {units_part!r} != {units_ref!r}",
        )

    rates_ref = _normalise_water_rate_attr(ref.attrs_ref.get("supy_water_rate_columns"))
    rates_part = _normalise_water_rate_attr(attrs_part.get("supy_water_rate_columns"))
    if rates_part != rates_ref:
        raise _schema_error(
            ref.path_dataset,
            ref.path_ref,
            path_part,
            f"supy_water_rate_columns {sorted(rates_part)!r} != {sorted(rates_ref)!r}",
        )

    if arrow_version_part is not None:
        if ref.arrow_version_ref is None:
            ref.arrow_version_ref = (path_part, arrow_version_part)
        elif arrow_version_part != ref.arrow_version_ref[1]:
            raise _schema_error(
                ref.path_dataset,
                ref.arrow_version_ref[0],
                path_part,
                f"output_contract_version {arrow_version_part!r} != "
                f"{ref.arrow_version_ref[1]!r}",
            )

    for field_name, meta_part in dict_arrow_fields_part.items():
        dict_field_ref = ref.arrow_fields_ref.setdefault(field_name, {})
        for key in ("units", "interval", "cell_methods", "units_metadata"):
            val_part = meta_part.get(key)
            if val_part is None:
                continue
            ref_entry = dict_field_ref.get(key)
            if ref_entry is None:
                dict_field_ref[key] = (path_part, val_part)
                continue
            path_key_ref, val_ref = ref_entry
            if val_part != val_ref:
                raise _schema_error(
                    ref.path_dataset,
                    path_key_ref,
                    path_part,
                    f"column {field_name!r} metadata {key!r} {val_part!r} != {val_ref!r}",
                )


def _merge_part_attrs(list_frames: list[pd.DataFrame]) -> dict[str, Any]:
    """Merge attrs across validated parts, combining per-chunk coverage dicts."""
    dict_merged = deepcopy(dict(list_frames[0].attrs))
    if "supy_output_interval_s" not in dict_merged:
        for df_part in list_frames[1:]:
            if "supy_output_interval_s" in df_part.attrs:
                dict_merged["supy_output_interval_s"] = df_part.attrs[
                    "supy_output_interval_s"
                ]
                break

    for key in _COVERAGE_ATTR_KEYS:
        dict_combined: dict[str, Any] = {}
        saw_dict = False
        saw_none = False
        for df_part in list_frames:
            if key not in df_part.attrs:
                continue
            val = df_part.attrs[key]
            if isinstance(val, dict):
                saw_dict = True
                dict_combined.update(val)
            elif val is None:
                saw_none = True
        if saw_dict:
            dict_merged[key] = dict_combined
        elif saw_none:
            dict_merged[key] = None

    return dict_merged


def _read_streamed_parquet_dataset(path_dataset: Path) -> pd.DataFrame:
    """Read a streamed Parquet dataset directory from its committed chunk files.

    Committed parts are validated against the first part's schema before
    concatenation: column sets, index layout, non-null column dtype families,
    and invariant SUEWS metadata (interval, units, water-rate declarations,
    and Arrow contract field metadata) must agree. Harmless column reordering,
    varying row counts/grids, present-but-NaN values, and per-chunk coverage
    metadata dictionaries are preserved.
    """
    list_parts = _list_streamed_parquet_parts(path_dataset)
    if not list_parts:
        raise FileNotFoundError(
            f"No committed streamed Parquet parts (part-YYYY-CCC.parquet) under {path_dataset}"
        )
    if len(list_parts) == 1:
        return pd.read_parquet(list_parts[0])

    df_ref = pd.read_parquet(list_parts[0])
    arrow_version_first, dict_arrow_first = _read_arrow_contract_metadata(list_parts[0])
    ref = _DatasetSchemaRef.from_first_part(
        path_dataset, list_parts[0], df_ref, arrow_version_first, dict_arrow_first
    )

    list_frames: list[pd.DataFrame] = [df_ref]
    for path_part in list_parts[1:]:
        df_part = pd.read_parquet(path_part)
        arrow_version_part, dict_arrow_part = _read_arrow_contract_metadata(path_part)

        df_aligned = _validate_and_align_part_columns(
            ref.path_dataset, ref.path_ref, ref.cols_ref, path_part, df_part
        )
        _validate_part_index(
            ref.path_dataset,
            ref.path_idx_ref,
            ref.idx_ref,
            ref.idx_ref_has_rows,
            path_part,
            df_aligned,
        )
        if not ref.idx_ref_has_rows and len(df_aligned) > 0:
            ref.path_idx_ref = path_part
            ref.idx_ref = df_aligned.index
            ref.idx_ref_has_rows = True

        _validate_part_dtypes(
            ref.path_dataset, ref.cols_ref, ref.ref_dtypes, path_part, df_aligned
        )
        _validate_part_metadata(
            ref, path_part, df_aligned.attrs, arrow_version_part, dict_arrow_part
        )
        list_frames.append(df_aligned)

    df_merged = pd.concat(list_frames, axis=0)
    df_merged.attrs = _merge_part_attrs(list_frames)
    if isinstance(df_merged.index, (pd.MultiIndex, pd.DatetimeIndex)):
        df_merged = df_merged.sort_index(kind="mergesort")
    return df_merged


def _read_output_file(path_output: Path) -> pd.DataFrame:
    if _is_canonical_parquet_dataset_dir(path_output):
        return _read_streamed_parquet_dataset(path_output)
    if path_output.is_dir():
        raise FileNotFoundError(
            f"Expected a SUEWS output file or streamed Parquet dataset, got directory {path_output}"
        )
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

    A partition is one output artefact of the highest-priority format present
    (see :func:`_select_output_partitions`), restricted to the core ``SUEWS``
    column group when the file stacks several groups (see
    :func:`_select_core_group`), and, for outputs whose row index is a
    MultiIndex such as the canonical ``(grid, datetime)`` layout, one grid
    within that output. A canonical streamed Parquet dataset directory is
    assembled in ``(year, chunk)`` order from its committed
    ``part-YYYY-CCC.parquet`` files and presented as a single logical Parquet
    artefact (whether discovered inside a run directory or passed directly).
    Legacy text output is already split per grid and per year on disk. Labels
    are ``"<name>"`` or ``"<name>[grid=<grid>]"``.

    Raises ``FileNotFoundError`` when ``path`` is a directory without any
    recognised output artefact.
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
    flatten: bool = True,
) -> pd.DataFrame:
    """Load the highest-priority recognised output under ``path`` (or ``path`` itself).

    ``suews summarise`` reads every committed chunk in a streamed dataset.
    It passes ``flatten=False`` to preserve group and variable identities.
    The default keeps the flat layout for existing callers. Diagnostics and
    comparisons use :func:`_load_run_output_partitions` for every partition.
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

    df_output = _read_output_file(path)
    return _flatten_output_columns(df_output) if flatten else df_output


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
