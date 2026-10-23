"""Rust library backend for SUEWS simulation."""

from __future__ import annotations

from collections.abc import Iterator
from functools import cache
from importlib import import_module
from typing import TYPE_CHECKING, Any

import numpy as np
import pandas as pd
import json

import yaml

# Use C-based YAML dumper when available (5-10x faster than pure Python)
_yaml_Dumper = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

from ._env import logger_supy
from ._post import df_var, gen_index

from ._load import LAI_LANDCOVER_SUFFIXES, WUH_LANDCOVER_SUFFIXES

if TYPE_CHECKING:
    from .data_model import SUEWSConfig


LAI_KERNEL_COLUMNS = tuple(f"lai_{suffix}" for suffix in LAI_LANDCOVER_SUFFIXES)
WUH_KERNEL_COLUMNS = tuple(f"wuh_{suffix}" for suffix in WUH_LANDCOVER_SUFFIXES)

OUTPUT_TIME_COLS = 5
MET_FORCING_COLS = 22 + len(LAI_KERNEL_COLUMNS) + len(WUH_KERNEL_COLUMNS)

_GROUP_ORDER: tuple[str, ...] = (
    "SUEWS",
    "snow",
    "BEERS",
    "ESTM",
    "EHC",
    "DailyState",
    "RSL",
    "debug",
    "SPARTACUS",
    "STEBBS",
    "NHood",
)


def _registry_group_data_cols(group_name: str) -> int:
    """Return the number of registered data columns for one output group."""
    return int(df_var.xs(group_name, level="group").shape[0])


# Ordered list of (group_name, total_cols_including_datetime) matching
# the Fortran concatenation layout. BL is a registry group but is not part
# of the concatenated Rust output buffer.
OUTPUT_GROUP_LAYOUT: list[tuple[str, int]] = [
    (group_name, _registry_group_data_cols(group_name) + OUTPUT_TIME_COLS)
    for group_name in _GROUP_ORDER
]

# All concatenated output groups in the Rust flat buffer.
OUTPUT_ALL_COLS = sum(ncols for _, ncols in OUTPUT_GROUP_LAYOUT)
_GROUP_DATA_COLS_BY_NAME: dict[str, int] = {
    group_name: ncols - OUTPUT_TIME_COLS for group_name, ncols in OUTPUT_GROUP_LAYOUT
}
# Columns of the output frame: every group's data columns, no datetime prefixes.
OUTPUT_DATA_COLS = sum(_GROUP_DATA_COLS_BY_NAME.values())

# Columns per slab when replacing the -999 sentinel in place, which bounds the
# temporary comparison mask to a slab rather than the whole table (gh#1849).
_SENTINEL_SLAB_COLS = 64

_RUST_ERROR_MSG = (
    "Rust backend not available in this build.\n"
    "Rebuild/install SuPy with Meson Rust bridge enabled (e.g. make dev)."
)


def _load_rust_module():
    """Import suews_bridge and return the module."""
    last_exc: Exception | None = None
    for module_name in ("supy.suews_bridge", "suews_bridge"):
        try:
            return import_module(module_name)
        except Exception as exc:  # pragma: no cover - depends on local build
            last_exc = exc
    raise RuntimeError(_RUST_ERROR_MSG) from last_exc


def _check_rust_available():
    """Check suews_bridge.run_suews_into is available."""
    module = _load_rust_module()
    if not hasattr(module, "run_suews_into"):
        raise RuntimeError(_RUST_ERROR_MSG)
    return module


def _output_layout_update_hint(group_name: str) -> str:
    registry_file = f"src/supy/data_model/output/{group_name.lower()}_vars.py"
    return (
        f"Update `{registry_file}`, then keep "
        "`src/suews/src/suews_ctrl_const.f95` and the corresponding "
        "Fortran `dataOutLine*` population in `src/suews/src/` in step."
    )


@cache
def _validate_output_layout(rust_module: Any | None = None) -> None:
    """Validate Python registry column counts against compiled Fortran values."""
    if rust_module is None:
        rust_module = _load_rust_module()

    if not hasattr(rust_module, "output_group_ncolumns"):
        raise RuntimeError(
            "Rust backend does not expose `output_group_ncolumns()`.\n"
            "Rebuild/install SuPy with Meson Rust bridge enabled (e.g. make dev)."
        )

    try:
        dict_fortran_cols = {
            group_name: int(ncols)
            for group_name, ncols in rust_module.output_group_ncolumns()
        }
    except Exception as exc:  # pragma: no cover - depends on local build
        raise RuntimeError(
            "Failed to read compiled Fortran output column counts from the Rust bridge."
        ) from exc

    list_issues: list[str] = []
    for group_name in _GROUP_ORDER:
        expected_cols = _GROUP_DATA_COLS_BY_NAME[group_name]
        actual_cols = dict_fortran_cols.get(group_name)

        if actual_cols is None:
            list_issues.append(
                f"- {group_name}: missing from compiled Fortran output metadata. "
                f"{_output_layout_update_hint(group_name)}"
            )
            continue

        if actual_cols != expected_cols:
            list_issues.append(
                f"- {group_name}: Python registry expects {expected_cols} data "
                f"columns, but compiled Fortran reports {actual_cols}. "
                f"{_output_layout_update_hint(group_name)}"
            )

    if list_issues:
        raise RuntimeError(
            "Compiled Fortran output column counts do not match the Python "
            "registry:\n" + "\n".join(list_issues)
        )


KERNEL_WARNING_COLUMNS = ("grid", "datetime", "location", "message")


class KernelWarningLog:
    """Non-fatal warnings raised inside the Fortran kernel during a run.

    The kernel keeps one bounded log per grid and per bridge call (chunk).
    This container gathers those logs across grids and chunks, keeps the
    kernel's own occurrence counter so a capped log cannot under-report, and
    renders both a tidy :class:`pandas.DataFrame` and a deduplicated summary
    for the SuPy logger (GH#1737).
    """

    def __init__(self) -> None:
        self._records: list[dict[str, Any]] = []
        self._totals: dict[int, int] = {}

    def add(self, grid_id: int, raw: Any) -> None:
        """Append one bridge call's ``(total, entries)`` payload for *grid_id*."""
        if not raw:
            return
        total, entries = raw
        grid_id = _normalise_grid_id(grid_id)
        self._totals[grid_id] = self._totals.get(grid_id, 0) + int(total)
        for iy, id_, it, imin, location, message in entries:
            self._records.append({
                "grid": grid_id,
                "datetime": _kernel_timestamp(iy, id_, it, imin),
                "location": str(location),
                "message": str(message),
            })

    def extend(self, other: KernelWarningLog) -> None:
        self._records.extend(other._records)
        for grid_id, total in other._totals.items():
            self._totals[grid_id] = self._totals.get(grid_id, 0) + total

    def __len__(self) -> int:
        return len(self._records)

    def __bool__(self) -> bool:
        return bool(self._records) or any(self._totals.values())

    @property
    def totals(self) -> dict[int, int]:
        """Occurrences reported by the kernel per grid, including capped ones."""
        return dict(self._totals)

    def to_frame(self) -> pd.DataFrame:
        """One row per recorded warning, sorted by grid then time."""
        if not self._records:
            return pd.DataFrame({
                "grid": pd.Series(dtype="int64"),
                "datetime": pd.Series(dtype="datetime64[ns]"),
                "location": pd.Series(dtype="object"),
                "message": pd.Series(dtype="object"),
            })
        df = pd.DataFrame.from_records(self._records, columns=KERNEL_WARNING_COLUMNS)
        df["datetime"] = pd.to_datetime(df["datetime"])
        return df.sort_values(["grid", "datetime"], kind="stable").reset_index(
            drop=True
        )

    def summary_lines(self) -> list[str]:
        """Deduplicated, human-readable lines: one per (grid, location, message).

        Each line reports the number of recorded occurrences and the number of
        distinct timesteps they fall on, with the first and last timestep.
        """
        lines: list[str] = []
        if not self._records and not any(self._totals.values()):
            return lines
        df = self.to_frame()
        recorded: dict[int, int] = {}
        for (grid_id, location, message), group in df.groupby(
            ["grid", "location", "message"], sort=True
        ):
            recorded[grid_id] = recorded.get(grid_id, 0) + len(group)
            times = group["datetime"].dropna()
            if times.empty:
                span = "timestep unknown"
            elif len(times) == 1 or times.min() == times.max():
                span = f"at {times.min():%Y-%m-%d %H:%M}"
            else:
                span = (
                    f"from {times.min():%Y-%m-%d %H:%M} to {times.max():%Y-%m-%d %H:%M}"
                )
            label = f"{location}: {message}" if location else message
            # Rows are occurrences, not timesteps: iterative physics can raise
            # the same warning several times within one timestep, so count the
            # distinct stamped timesteps separately.
            n_steps = times.nunique()
            lines.append(
                f"Kernel warning (grid {grid_id}): {label} "
                f"[{len(group)} occurrence(s) over {n_steps} timestep(s), {span}]"
            )
        for grid_id in sorted(self._totals):
            total = self._totals[grid_id]
            if total > recorded.get(grid_id, 0):
                lines.append(
                    f"Kernel warnings (grid {grid_id}): {total} raised in total, "
                    f"{recorded.get(grid_id, 0)} recorded (per-chunk log cap)"
                )
        return lines

    def log(self, logger=logger_supy) -> None:
        for line in self.summary_lines():
            logger.warning(line)


def _kernel_timestamp(iy: int, id_: int, it: int, imin: int):
    """Convert a Fortran (year, day-of-year, hour, minute) stamp to a Timestamp."""
    if int(iy) <= 0 or int(id_) <= 0:
        return pd.NaT
    try:
        # A string, not an integer: pandas 3.1 deprecates parsing numbers
        # with a format.
        return pd.to_datetime(f"{int(iy):04d}{int(id_):03d}", format="%Y%j") + (
            pd.Timedelta(hours=int(it), minutes=int(imin))
        )
    except (ValueError, OverflowError):
        return pd.NaT


def _unpack_run_result(result: Any) -> tuple[Any, Any, int, Any]:
    """Accept the bridge's ``(output, state_json, len_sim[, warnings])`` tuple.

    Older bridge builds return three elements; the fourth carries the kernel
    warning payload (GH#1737).
    """
    if len(result) == 3:
        output_flat, state_json, len_sim = result
        return output_flat, state_json, len_sim, None
    output_flat, state_json, len_sim, warnings = result
    return output_flat, state_json, len_sim, warnings


def _normalise_grid_id(grid_id: Any) -> int:
    if hasattr(grid_id, "value"):
        return int(grid_id.value)
    return int(grid_id)


def _build_datetime_index(output_block: np.ndarray) -> pd.DatetimeIndex:
    year = output_block[:, 0].astype(np.int64)
    day_of_year = output_block[:, 1].astype(np.int64)
    hour = output_block[:, 2].astype(np.int64)
    minute = output_block[:, 3].astype(np.int64)

    # Guard against -999 sentinels from uninitialised output rows (e.g. when
    # the simulation errored and left output_line at its default values).
    valid = (year > 0) & (day_of_year > 0)
    if not valid.all():
        n_bad = int((~valid).sum())
        raise RuntimeError(
            f"{n_bad} of {len(year)} output rows contain sentinel datetime "
            f"values (year={year[~valid][0]}, doy={day_of_year[~valid][0]}). "
            "This usually means the Fortran simulation errored on an early "
            "timestep. Check stderr for OHM_DEBUG or SPARTACUS diagnostics."
        )

    # Midnight of each row's date from (year, day of year) by numpy date
    # arithmetic: pandas 3.1 deprecates parsing the integer year * 1000 + doy
    # with format "%Y%j". The result keeps the resolution that parse gives on
    # the installed pandas (ns on pandas 2, us on pandas 3).
    unit = pd.to_datetime(["2000001"], format="%Y%j").unit
    date_part = pd.DatetimeIndex(
        (
            (year - 1970).astype("datetime64[Y]").astype("datetime64[D]")
            + (day_of_year - 1).astype("timedelta64[D]")
        ).astype(f"datetime64[{unit}]")
    )
    return (
        date_part + pd.to_timedelta(hour, unit="h") + pd.to_timedelta(minute, unit="m")
    )


def _prepare_forcing_block(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare the forcing as the ``(len_sim, MET_FORCING_COLS)`` kernel block.

    The block is C-ordered: the bridge reads it row by row, so its memory is
    already the row-major layout the kernel expects and it can be handed over
    as a buffer without a copy in Python (gh#1849).
    """
    len_sim = len(df_forcing)
    columns_by_lower = {str(col).lower(): col for col in df_forcing.columns}

    fixed_col_map = [
        (4, "qn", 0.0),
        (5, "qh", 0.0),
        (6, "qe", 0.0),
        (7, "qs", 0.0),
        (8, "qf", 0.0),
        (9, "U", 0.0),
        (10, "RH", 0.0),
        (11, "Tair", 0.0),
        (12, "pres", 0.0),
        (13, "rain", 0.0),
        (14, "kdown", 0.0),
        (15, "snow", 0.0),
        (16, "ldown", 0.0),
        (17, "fcld", 0.0),
        (18, "Wuh", 0.0),
        (19, "xsmd", 0.0),
        (30, "kdiff", -999.0),
        (31, "kdir", -999.0),
    ]

    block = np.zeros((len_sim, MET_FORCING_COLS), dtype=np.float64, order="C")

    block[:, 0] = df_forcing.index.year
    block[:, 1] = df_forcing.index.day_of_year
    block[:, 2] = df_forcing.index.hour
    block[:, 3] = df_forcing.index.minute

    for idx, col, default in fixed_col_map:
        source_col = columns_by_lower.get(col.lower())
        if source_col is not None:
            block[:, idx] = df_forcing[source_col].values
        else:
            block[:, idx] = default

    bulk_lai_col = columns_by_lower.get("lai")
    for target_idx, lai_col in enumerate(LAI_KERNEL_COLUMNS, start=20):
        source_col = columns_by_lower.get(lai_col, bulk_lai_col)
        if source_col is not None:
            block[:, target_idx] = df_forcing[source_col].values
        else:
            block[:, target_idx] = -999.0

    bulk_wuh_col = columns_by_lower.get("wuh")
    for target_idx, wuh_col in enumerate(WUH_KERNEL_COLUMNS, start=23):
        source_col = columns_by_lower.get(wuh_col, bulk_wuh_col)
        if source_col is not None:
            block[:, target_idx] = df_forcing[source_col].values
        else:
            block[:, target_idx] = -999.0

    return block


def _parse_output_block(
    output_flat: bytes | list[float],
    len_sim: int,
    grid_id: int,
) -> pd.DataFrame:
    """Reshape the flat output buffer into a multi-group DataFrame.

    The flat buffer contains *len_sim* rows of ``OUTPUT_ALL_COLS`` columns.
    Each of the 11 output groups occupies a contiguous slice and carries its
    own 5-column datetime prefix.  We extract the datetime from the first
    group (SUEWS), then strip every group's datetime prefix and build one
    frame with ``(group, var)`` MultiIndex columns via :func:`gen_index`.

    The bridge hands the buffer over as native-endian ``bytes`` so that no
    boxed Python floats are created at the FFI boundary (GH-1718);
    ``np.frombuffer`` reconstructs the array zero-copy. A plain sequence is
    still accepted for older bridge builds.
    """
    if isinstance(output_flat, (bytes, bytearray, memoryview)):
        output_array = np.frombuffer(output_flat, dtype=np.float64)
    else:
        output_array = np.asarray(output_flat, dtype=np.float64)
    expected = len_sim * OUTPUT_ALL_COLS
    if output_array.size != expected:
        if len_sim > 0 and output_array.size % len_sim == 0:
            actual_cols = output_array.size // len_sim
            raise RuntimeError(
                "Rust backend output shape mismatch: received "
                f"{actual_cols} columns per timestep from the compiled backend, "
                f"but the Python registry-derived layout expects "
                f"{OUTPUT_ALL_COLS}. Run `_validate_output_layout()` to identify "
                "the drifting group."
            )

        raise RuntimeError(
            f"Rust backend output shape mismatch: got {output_array.size}, "
            f"expected {expected}"
        )

    output_block = output_array.reshape((len_sim, OUTPUT_ALL_COLS), order="C")
    # Datetime from the first group (SUEWS)
    datetime_index = _build_datetime_index(output_block)

    # Gather the data columns in ONE fancy-index copy (writable) instead of
    # building eleven per-group frames and concatenating them - the frame
    # pipeline previously held several whole-year copies at peak (GH-1718).
    list_col_idx: list[np.ndarray] = []
    col_offset = 0
    for _, ncols in OUTPUT_GROUP_LAYOUT:
        list_col_idx.append(
            np.arange(col_offset + OUTPUT_TIME_COLS, col_offset + ncols)
        )
        col_offset += ncols
    cols_all = _output_columns()

    ar_data = output_block[:, np.concatenate(list_col_idx)]
    _replace_sentinel_in_place(ar_data)
    return _wrap_output_frame(ar_data, cols_all, _grid_index(grid_id, datetime_index))


def _output_columns() -> pd.MultiIndex:
    """Return the ``(group, var)`` columns of the output frame, in kernel order."""
    list_group_cols: list[pd.MultiIndex] = []
    for group_name, ncols in OUTPUT_GROUP_LAYOUT:
        n_data = ncols - OUTPUT_TIME_COLS
        idx = gen_index(f"dataoutline{group_name.lower()}")
        if len(idx) != n_data:
            raise RuntimeError(
                f"Failed to parse Rust backend output group '{group_name}': "
                f"received {n_data} data columns, but the Python "
                f"registry expects {len(idx)}. "
                f"{_output_layout_update_hint(group_name)}"
            )
        list_group_cols.append(idx)

    cols_all = list_group_cols[0]
    for idx in list_group_cols[1:]:
        cols_all = cols_all.append(idx)
    return cols_all


def _replace_sentinel_in_place(ar_data: np.ndarray) -> None:
    """Expose the kernel's -999 missing-data sentinel as NaN, in place.

    Works a slab of columns at a time, so the comparison mask never spans
    the whole table (gh#1849); with a Fortran-ordered array each slab is
    contiguous.
    """
    for col_start in range(0, ar_data.shape[1], _SENTINEL_SLAB_COLS):
        ar_slab = ar_data[:, col_start : col_start + _SENTINEL_SLAB_COLS]
        ar_slab[ar_slab == -999.0] = np.nan


def _grid_index(grid_id: int, datetime_index: pd.DatetimeIndex) -> pd.MultiIndex:
    """Return the ``(grid, datetime)`` index of one grid's output rows."""
    return pd.MultiIndex.from_product(
        [[_normalise_grid_id(grid_id)], datetime_index],
        names=["grid", "datetime"],
    )


def _wrap_output_frame(
    ar_data: np.ndarray,
    columns: pd.MultiIndex,
    index: pd.MultiIndex,
) -> pd.DataFrame:
    """Wrap a 2-D float array as the output frame without copying it."""
    return pd.DataFrame(ar_data, columns=columns, index=index, copy=False)


def _alloc_output_buffers(n_rows: int) -> tuple[np.ndarray, np.ndarray]:
    """Allocate the arrays the kernel writes output into (gh#1849).

    Returns ``(ar_data, ar_datetime)``, shaped ``(n_rows, OUTPUT_DATA_COLS)``
    and ``(n_rows, OUTPUT_TIME_COLS)``: one grid's rows, or the rows of every
    grid of a run, one block of rows per grid. Both are Fortran-ordered, the
    layout the kernel writes, so ``ar_data`` becomes the output frame as it
    is: the kernel, the bridge and pandas share this one full-size buffer.
    ``np.zeros`` maps zeroed pages lazily, so memory is committed only as the
    kernel writes it, and a row the kernel never reaches keeps year 0, which
    :func:`_build_datetime_index` rejects.
    """
    ar_data = np.zeros((n_rows, OUTPUT_DATA_COLS), dtype=np.float64, order="F")
    ar_datetime = np.zeros((n_rows, OUTPUT_TIME_COLS), dtype=np.float64, order="F")
    return ar_data, ar_datetime


def _alloc_output_store(n_grid: int, len_sim: int) -> tuple[np.ndarray, np.ndarray]:
    """Allocate one output table shared by *n_grid* grids of *len_sim* rows each.

    Grid ``i`` writes rows ``i * len_sim`` to ``(i + 1) * len_sim``. With one
    grid this is :func:`_alloc_output_buffers`. With several, the arrays get
    one spare row when that makes the leading dimension odd, and the returned
    arrays are views of the first ``n_grid * len_sim`` rows. The kernel writes
    one row at a time across every column, so consecutive writes are a column
    apart; when that distance in bytes is a multiple of a large power of two,
    as ``n_grid * len_sim`` rows easily make it with eight grids, the writes
    of a row crowd into a few cache sets and the run is markedly slower. An
    odd leading dimension spreads them out.
    """
    n_rows = n_grid * len_sim
    if n_grid == 1:
        return _alloc_output_buffers(n_rows)
    ar_data, ar_datetime = _alloc_output_buffers(n_rows | 1)
    return ar_data[:n_rows], ar_datetime[:n_rows]


def _frame_from_output_store(
    ar_data: np.ndarray,
    ar_datetime: np.ndarray,
    list_grid_ids: list[int],
) -> pd.DataFrame:
    """Turn kernel-filled output arrays into the multi-group output frame.

    Grid ``list_grid_ids[i]`` holds the ``i``-th of ``len(list_grid_ids)``
    equal blocks of rows. The frame takes ``ar_data`` as its storage: the
    -999 sentinel is replaced in place and no copy is made. The per-grid
    indexes are joined as ``pd.concat`` joins the indexes of per-grid frames.
    """
    len_sim = ar_data.shape[0] // len(list_grid_ids)
    list_index = [
        _grid_index(
            grid_id,
            _build_datetime_index(ar_datetime[idx * len_sim : (idx + 1) * len_sim]),
        )
        for idx, grid_id in enumerate(list_grid_ids)
    ]
    index = (
        list_index[0].append(list_index[1:]) if len(list_index) > 1 else list_index[0]
    )
    _replace_sentinel_in_place(ar_data)
    return _wrap_output_frame(ar_data, _output_columns(), index)


def _frame_from_output_buffers(
    ar_data: np.ndarray,
    ar_datetime: np.ndarray,
    grid_id: int,
) -> pd.DataFrame:
    """Turn one grid's kernel-filled output arrays into its output frame."""
    return _frame_from_output_store(ar_data, ar_datetime, [grid_id])


def _write_grid_output(
    rust_module: Any,
    config_text: str,
    forcing_block: np.ndarray,
    len_forcing: int,
    ar_data: np.ndarray,
    ar_datetime: np.ndarray,
    state_json: str | None = None,
) -> tuple[str | None, Any]:
    """Run one grid with the kernel writing into *ar_data* and *ar_datetime*.

    Both may be a block of rows of a larger array shared by several grids.
    Returns ``(state_json, raw_warnings)``. With *state_json*, the run
    continues from that checkpoint.
    """
    state_json_out, len_sim, raw_warnings = rust_module.run_suews_into(
        config_text,
        forcing_block,
        len_forcing,
        ar_data,
        ar_datetime,
        state_json,
    )
    if len_sim != len_forcing:
        raise RuntimeError(
            f"Rust backend length mismatch: forcing={len_forcing}, output={len_sim}"
        )
    return state_json_out, raw_warnings


def _run_grid_into(
    rust_module: Any,
    config_text: str,
    forcing_block: np.ndarray,
    len_forcing: int,
    grid_id: int,
    state_json: str | None = None,
) -> tuple[pd.DataFrame, str | None, Any]:
    """Run one grid with the kernel writing straight into its output frame.

    Returns ``(df_output, state_json, raw_warnings)``. With *state_json*, the
    run continues from that checkpoint.
    """
    ar_data, ar_datetime = _alloc_output_buffers(len_forcing)
    state_json_out, raw_warnings = _write_grid_output(
        rust_module,
        config_text,
        forcing_block,
        len_forcing,
        ar_data,
        ar_datetime,
        state_json,
    )
    df_output = _frame_from_output_buffers(ar_data, ar_datetime, grid_id)
    return df_output, state_json_out, raw_warnings


def _run_grids_into(
    rust_module: Any,
    list_config_jsons: list[str],
    forcing_block: np.ndarray,
    len_forcing: int,
    list_grid_ids: list[int],
    list_state_jsons: list[str] | None,
    use_rayon: bool,
    max_workers: int | None,
) -> tuple[pd.DataFrame, dict[int, str] | None, KernelWarningLog]:
    """Run every grid, all writing into one shared output array (gh#1849).

    Each grid's kernel writes into its own block of rows of one preallocated
    Fortran-ordered array, which then becomes the output frame as it is, so
    no per-grid frames are joined afterwards and a multi-grid run needs about
    the memory of its output once rather than twice. The blocks follow grid
    ID, so the frame comes out in the order the joined per-grid frames were
    sorted into. With *use_rayon* the grids run in parallel inside the
    bridge; otherwise one after another. Returns
    ``(df_output, dict_state_json, warnings)``.
    """
    list_grid_ids_sorted = sorted(list_grid_ids)
    dict_row_start = {
        grid_id: rank * len_forcing for rank, grid_id in enumerate(list_grid_ids_sorted)
    }
    ar_data, ar_datetime = _alloc_output_store(len(list_grid_ids), len_forcing)
    list_rows = [
        slice(dict_row_start[grid_id], dict_row_start[grid_id] + len_forcing)
        for grid_id in list_grid_ids
    ]
    dict_state_json: dict[int, str] = {}
    kernel_warnings = KernelWarningLog()

    if use_rayon:
        raw_results = rust_module.run_suews_multi_into(
            list_config_jsons,
            forcing_block,
            len_forcing,
            [ar_data[rows] for rows in list_rows],
            [ar_datetime[rows] for rows in list_rows],
            list_state_jsons,
            max_workers,
        )
        # raw_results: list of (grid_index, state_json, len_sim, warnings)
        for idx, state_json, len_sim, raw_warnings in sorted(
            raw_results, key=lambda r: r[0]
        ):
            if len_sim != len_forcing:
                raise RuntimeError(
                    f"Rust backend length mismatch: forcing={len_forcing}, "
                    f"output={len_sim}"
                )
            grid_id = list_grid_ids[idx]
            if state_json is not None:
                dict_state_json[grid_id] = state_json
            kernel_warnings.add(grid_id, raw_warnings)
    else:
        for idx, config_json in enumerate(list_config_jsons):
            grid_id = list_grid_ids[idx]
            rows = list_rows[idx]
            state_json, raw_warnings = _write_grid_output(
                rust_module,
                config_json,
                forcing_block,
                len_forcing,
                ar_data[rows],
                ar_datetime[rows],
                list_state_jsons[idx] if list_state_jsons is not None else None,
            )
            if state_json is not None:
                dict_state_json[grid_id] = state_json
            kernel_warnings.add(grid_id, raw_warnings)

    df_output_all = _frame_from_output_store(ar_data, ar_datetime, list_grid_ids_sorted)
    # Unsorted forcing gives an unsorted frame; sort it, as joining did.
    if not df_output_all.index.is_monotonic_increasing:
        df_output_all = df_output_all.sort_index()
    return df_output_all, dict_state_json or None, kernel_warnings


def run_suews_rust(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
) -> tuple[pd.DataFrame, str | None, KernelWarningLog]:
    """Run SUEWS via Rust bridge library.

    Returns ``(df_output, state_json, kernel_warnings)`` where *state_json*
    is a JSON string encoding the post-simulation state (or ``None`` if
    unavailable) and *kernel_warnings* collects the non-fatal warnings the
    Fortran kernel raised (GH#1737).
    """
    rust_module = _check_rust_available()
    _validate_output_layout(rust_module)
    if df_forcing.empty:
        raise ValueError("forcing data is empty")

    config_yaml = yaml.dump(
        config.model_dump(exclude_none=True, mode="json"),
        default_flow_style=False,
        sort_keys=False,
        Dumper=_yaml_Dumper,
    )
    forcing_block = _prepare_forcing_block(df_forcing)

    df_output, state_json, raw_warnings = _run_grid_into(
        rust_module, config_yaml, forcing_block, len(df_forcing), grid_id
    )
    kernel_warnings = KernelWarningLog()
    kernel_warnings.add(grid_id, raw_warnings)
    return df_output, state_json, kernel_warnings


def _run_single_grid_worker(args: tuple) -> tuple[int, list, str | None, int, Any]:
    """Worker function for parallel multi-grid execution.

    Runs a single grid cell in a child process.  Accepts and returns only
    serialisable types (no Pydantic models or DataFrames) so it works with
    multiprocessing.

    Parameters
    ----------
    args : tuple
        (config_json, forcing_flat, len_forcing, grid_id)

    Returns
    -------
    tuple
        (grid_id, output_flat, state_json, len_sim, raw_warnings)
    """
    config_json, forcing_flat, len_forcing, grid_id = args
    rust_module = _check_rust_available()
    output_flat, state_json, len_sim, raw_warnings = _unpack_run_result(
        rust_module.run_suews(
            config_json,
            forcing_flat,
            len_forcing,
        )
    )
    return grid_id, output_flat, state_json, len_sim, raw_warnings


def run_suews_rust_multi(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    serial_mode: bool = False,
    max_workers: int | None = None,
) -> tuple[pd.DataFrame, dict[int, str] | None, KernelWarningLog]:
    """Run SUEWS via Rust bridge for all sites in configuration.

    Iterates over ``config.sites``, patches the serialised config dict
    per site, and calls the Rust bridge directly.  Shared data (forcing
    block, base config dict) is prepared once to avoid redundant deep
    copies and YAML serialisation.

    When *serial_mode* is False and there are multiple sites, grids are
    run in parallel using Rust/Rayon.  If *max_workers* is provided, the
    Rayon call is capped to that many threads.

    Returns ``(df_output, dict_state_json, kernel_warnings)`` where
    *dict_state_json* maps each grid ID to its post-simulation state JSON
    string and *kernel_warnings* collects the non-fatal warnings raised by
    the Fortran kernel for every grid (GH#1737).
    """
    sites = config.sites

    # Validate unique grid IDs
    list_gridiv = [s.gridiv for s in sites]
    list_dupes = [g for g in list_gridiv if list_gridiv.count(g) > 1]
    if list_dupes:
        raise ValueError(f"Duplicate gridiv values in config.sites: {set(list_dupes)}")

    rust_module = _check_rust_available()
    _validate_output_layout(rust_module)
    if df_forcing.empty:
        raise ValueError("forcing data is empty")
    if max_workers is not None and (
        isinstance(max_workers, bool)
        or not isinstance(max_workers, int)
        or max_workers < 1
    ):
        raise ValueError("max_workers must be a positive integer or None")

    # --- Prepare shared data once ---
    # The Rust bridge's YAML preprocessor accepts both new snake_case and
    # legacy fused spellings (gh#1322), so the Pydantic dump is submitted
    # verbatim.
    config_dict = config.model_dump(exclude_none=True, mode="json")
    list_site_dict = [s.model_dump(exclude_none=True, mode="json") for s in sites]
    forcing_block = _prepare_forcing_block(df_forcing)
    len_forcing = len(df_forcing)

    # Pre-serialise per-grid config JSON strings
    list_grid_ids = []
    list_config_jsons = []
    for idx, site_dict in enumerate(list_site_dict):
        grid_id = _normalise_grid_id(sites[idx].gridiv)
        list_grid_ids.append(grid_id)
        config_dict["sites"] = [site_dict]
        list_config_jsons.append(json.dumps(config_dict))

    # --- Execute grids ---
    # Use Rust Rayon parallelism (shared memory, no IPC overhead) when
    # multiple grids are present and serial_mode is not forced.
    use_rayon = not serial_mode and len(sites) > 1
    if use_rayon:
        if max_workers is None:
            logger_supy.info("Running %d grids in parallel (Rust/Rayon)", len(sites))
        else:
            logger_supy.info(
                "Running %d grids in parallel (Rust/Rayon, max_workers=%d)",
                len(sites),
                max_workers,
            )
    return _run_grids_into(
        rust_module,
        list_config_jsons,
        forcing_block,
        len_forcing,
        list_grid_ids,
        None,
        use_rayon,
        max_workers,
    )


def _concat_output_frames(list_df_output: list[pd.DataFrame]) -> pd.DataFrame:
    """Join per-chunk output frames into one sorted frame.

    ``sort_index`` always returns a full copy, which for a multi-year run at
    the model timestep is several GB (gh#1849). Sort only when the joined
    index is out of order, and skip ``pd.concat`` for a single frame.
    """
    if len(list_df_output) == 1:
        df_output_all = list_df_output[0]
    else:
        df_output_all = pd.concat(list_df_output)
    if not df_output_all.index.is_monotonic_increasing:
        df_output_all = df_output_all.sort_index()
    return df_output_all


def run_suews_rust_multi_with_state(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    dict_state_json_by_grid: dict[int, str],
    serial_mode: bool = False,
    max_workers: int | None = None,
) -> tuple[pd.DataFrame, dict[int, str] | None, KernelWarningLog]:
    """Run SUEWS via Rust bridge for all sites with injected states."""
    sites = config.sites

    list_gridiv = [_normalise_grid_id(s.gridiv) for s in sites]
    list_dupes = [g for g in list_gridiv if list_gridiv.count(g) > 1]
    if list_dupes:
        raise ValueError(f"Duplicate gridiv values in config.sites: {set(list_dupes)}")

    dict_state_json_by_grid = {
        _normalise_grid_id(grid_id): state_json
        for grid_id, state_json in dict_state_json_by_grid.items()
    }
    config_grid_ids = set(list_gridiv)
    state_grid_ids = set(dict_state_json_by_grid)
    if state_grid_ids != config_grid_ids:
        parts = []
        missing = sorted(config_grid_ids - state_grid_ids)
        unexpected = sorted(state_grid_ids - config_grid_ids)
        if missing:
            parts.append(f"missing states for grids {missing}")
        if unexpected:
            parts.append(f"unexpected states for grids {unexpected}")
        raise ValueError(
            "State grid IDs do not match config.sites: " + "; ".join(parts)
        )

    rust_module = _check_rust_available()
    _validate_output_layout(rust_module)
    if df_forcing.empty:
        raise ValueError("forcing data is empty")
    if max_workers is not None and (
        isinstance(max_workers, bool)
        or not isinstance(max_workers, int)
        or max_workers < 1
    ):
        raise ValueError("max_workers must be a positive integer or None")

    config_dict = config.model_dump(exclude_none=True, mode="json")
    list_site_dict = [s.model_dump(exclude_none=True, mode="json") for s in sites]
    forcing_block = _prepare_forcing_block(df_forcing)
    len_forcing = len(df_forcing)

    list_grid_ids = []
    list_config_jsons = []
    list_state_jsons = []
    for idx, site_dict in enumerate(list_site_dict):
        grid_id = list_gridiv[idx]
        list_grid_ids.append(grid_id)
        config_dict["sites"] = [site_dict]
        list_config_jsons.append(json.dumps(config_dict))
        list_state_jsons.append(dict_state_json_by_grid[grid_id])

    use_rayon = not serial_mode and len(sites) > 1
    if use_rayon:
        if max_workers is None:
            logger_supy.info(
                "Running %d checkpointed grids in parallel (Rust/Rayon)", len(sites)
            )
        else:
            logger_supy.info(
                "Running %d checkpointed grids in parallel "
                "(Rust/Rayon, max_workers=%d)",
                len(sites),
                max_workers,
            )
    return _run_grids_into(
        rust_module,
        list_config_jsons,
        forcing_block,
        len_forcing,
        list_grid_ids,
        list_state_jsons,
        use_rayon,
        max_workers,
    )


def run_suews_rust_with_state(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
    state_json: str = "",
) -> tuple[pd.DataFrame, str | None, KernelWarningLog]:
    """Run SUEWS via Rust bridge with injected state from a previous chunk."""
    rust_module = _check_rust_available()
    _validate_output_layout(rust_module)
    if df_forcing.empty:
        raise ValueError("forcing data is empty")

    config_yaml = yaml.dump(
        config.model_dump(exclude_none=True, mode="json"),
        default_flow_style=False,
        sort_keys=False,
        Dumper=_yaml_Dumper,
    )
    forcing_block = _prepare_forcing_block(df_forcing)

    df_output, new_state_json, raw_warnings = _run_grid_into(
        rust_module, config_yaml, forcing_block, len(df_forcing), grid_id, state_json
    )
    kernel_warnings = KernelWarningLog()
    kernel_warnings.add(grid_id, raw_warnings)
    return df_output, new_state_json, kernel_warnings


def _check_chunk_grid_ids(
    config: SUEWSConfig, initial_state_json_by_grid: dict[int, str]
) -> None:
    """Reject duplicate grid IDs and checkpoints that do not match the sites."""
    list_gridiv = [_normalise_grid_id(s.gridiv) for s in config.sites]
    list_dupes = [g for g in list_gridiv if list_gridiv.count(g) > 1]
    if list_dupes:
        raise ValueError(f"Duplicate gridiv values in config.sites: {set(list_dupes)}")

    if initial_state_json_by_grid:
        checkpoint_grid_ids = set(initial_state_json_by_grid)
        config_grid_ids = set(list_gridiv)
        if checkpoint_grid_ids != config_grid_ids:
            parts = []
            missing = sorted(config_grid_ids - checkpoint_grid_ids)
            unexpected = sorted(checkpoint_grid_ids - config_grid_ids)
            if missing:
                parts.append(f"missing checkpoint states for grids {missing}")
            if unexpected:
                parts.append(f"unexpected checkpoint states for grids {unexpected}")
            raise ValueError(
                "Checkpoint grid IDs do not match config.sites: " + "; ".join(parts)
            )


def _iter_suews_rust_chunks(
    config: SUEWSConfig,
    grp_forcing_chunk: Any,
    serial_mode: bool,
    max_workers: int | None,
    initial_state_json_by_grid: dict[int, str],
) -> Iterator[tuple[pd.DataFrame, dict[int, str], KernelWarningLog]]:
    """Run forcing chunks in order, threading each grid's state forward.

    *grp_forcing_chunk* is a pandas ``GroupBy`` over the forcing rows, one
    group per chunk in time order. Yields ``(df_output_chunk,
    dict_state_json, warnings_chunk)`` per chunk, where *dict_state_json* is
    the state reached at the end of that chunk. The caller decides whether
    to keep or drop each chunk's frame.
    """
    n_chunk = len(grp_forcing_chunk)
    dict_state_json: dict[int, str] = dict(initial_state_json_by_grid)
    for chunk_idx, grp in enumerate(grp_forcing_chunk.groups):
        df_forcing_chunk = grp_forcing_chunk.get_group(grp)
        logger_supy.debug(
            "Rust backend: chunk %d/%d  (%s -> %s, %d rows)",
            chunk_idx + 1,
            n_chunk,
            df_forcing_chunk.index[0],
            df_forcing_chunk.index[-1],
            len(df_forcing_chunk),
        )

        if dict_state_json:
            df_output_chunk, dict_state_json_chunk, warnings_chunk = (
                run_suews_rust_multi_with_state(
                    config=config,
                    df_forcing=df_forcing_chunk,
                    dict_state_json_by_grid=dict_state_json,
                    serial_mode=serial_mode,
                    max_workers=max_workers,
                )
            )
        else:
            df_output_chunk, dict_state_json_chunk, warnings_chunk = (
                run_suews_rust_multi(
                    config=config,
                    df_forcing=df_forcing_chunk,
                    serial_mode=serial_mode,
                    max_workers=max_workers,
                )
            )

        if dict_state_json_chunk:
            dict_state_json.update(dict_state_json_chunk)
        yield df_output_chunk, dict(dict_state_json), warnings_chunk
        # Release this chunk before the next one is allocated, so a caller
        # that drops each chunk holds one chunk at a time.
        del df_output_chunk


def run_suews_rust_chunked(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    chunk_day: int = 366,
    serial_mode: bool = False,
    max_workers: int | None = None,
    initial_state_json_by_grid: dict[int, str] | None = None,
) -> tuple[pd.DataFrame, dict[int, str] | None, KernelWarningLog]:
    """Run SUEWS via Rust bridge with multi-chunk state chaining.

    Splits forcing into chunks of *chunk_day* days, runs each chunk
    sequentially, and threads the final state of chunk N into chunk N+1
    for every grid.  When *initial_state_json_by_grid* is provided, the first
    chunk also runs through ``run_suews_with_state`` for those grids.
    """
    initial_state_json_by_grid = {
        _normalise_grid_id(grid_id): state_json
        for grid_id, state_json in (initial_state_json_by_grid or {}).items()
    }

    # Group forcing into chunks (same logic as traditional backend)
    idx_start = df_forcing.index.min()
    idx_all = df_forcing.index
    grp_forcing_chunk = df_forcing.groupby(
        (idx_all - idx_start) // pd.Timedelta(chunk_day, "D")
    )

    n_chunk = len(grp_forcing_chunk)
    if n_chunk <= 1 and not initial_state_json_by_grid:
        return run_suews_rust_multi(
            config, df_forcing, serial_mode=serial_mode, max_workers=max_workers
        )

    if n_chunk > 1:
        logger_supy.info(
            "Rust backend: forcing split into %d chunks of <= %d days.",
            n_chunk,
            chunk_day,
        )

    _check_chunk_grid_ids(config, initial_state_json_by_grid)

    dict_state_json: dict[int, str] = dict(initial_state_json_by_grid)
    list_df_output: list[pd.DataFrame] = []
    kernel_warnings = KernelWarningLog()
    for df_output_chunk, dict_state_chunk, warnings_chunk in _iter_suews_rust_chunks(
        config,
        grp_forcing_chunk,
        serial_mode,
        max_workers,
        initial_state_json_by_grid,
    ):
        list_df_output.append(df_output_chunk)
        kernel_warnings.extend(warnings_chunk)
        dict_state_json = dict_state_chunk

    df_output_all = _concat_output_frames(list_df_output)
    return df_output_all, dict_state_json or None, kernel_warnings


def forcing_year_keys(index: pd.DatetimeIndex, tstep_s: int) -> np.ndarray:
    """Return the calendar year of each forcing row's interval.

    Under the right-closed timestamp convention a row stamped ``t`` holds
    ``(t - tstep, t]``, so the row stamped ``Y+1-01-01 00:00`` is the last
    interval of year ``Y``. Rows are therefore keyed by the year of their
    interval start, ``t - tstep``: the rows ``Y-01-01 00:00 + tstep`` to
    ``Y+1-01-01 00:00`` share the key ``Y``, which is exactly the set of rows
    that ``save()`` writes into the year-``Y`` files.

    A leading key shorter than one day (for example a run that starts with
    the single row ``Y-01-01 00:00``, the last interval of ``Y-1``) is
    folded into the next year, so that no chunk is a stub of fewer than one
    day; ``save()`` then writes that stub exactly as an unchunked run does.
    """
    ar_year = (index - pd.Timedelta(seconds=int(tstep_s))).year.to_numpy()
    if len(ar_year) and (ar_year != ar_year[0]).any():
        n_first = int((ar_year == ar_year[0]).sum())
        if n_first * int(tstep_s) < 86400:
            ar_year = np.where(ar_year == ar_year[0], ar_year[n_first], ar_year)
    return ar_year


def iter_suews_rust_by_year(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    tstep_s: int,
    serial_mode: bool = False,
    max_workers: int | None = None,
    initial_state_json_by_grid: dict[int, str] | None = None,
) -> Iterator[tuple[pd.DataFrame, dict[int, str], KernelWarningLog, pd.Timestamp]]:
    """Run SUEWS one calendar year at a time, yielding each year's output.

    Same state chaining as :func:`run_suews_rust_chunked`, but chunks follow
    calendar years under the right-closed convention (see
    :func:`forcing_year_keys`) and no chunk is kept: the caller receives
    ``(df_output_year, dict_state_json, warnings_year, last_timestamp)`` per
    year, where *dict_state_json* is the state reached at *last_timestamp*
    (the year's last forcing row), and can write and drop the frame before
    the next year runs, so memory no longer grows with run length.
    """
    if df_forcing.empty:
        raise ValueError("forcing data is empty")
    initial_state_json_by_grid = {
        _normalise_grid_id(grid_id): state_json
        for grid_id, state_json in (initial_state_json_by_grid or {}).items()
    }
    _check_chunk_grid_ids(config, initial_state_json_by_grid)
    ar_year = forcing_year_keys(df_forcing.index, tstep_s)
    grp_forcing_chunk = df_forcing.groupby(ar_year)
    # Last forcing row of each year, in the group order the chunks run in.
    ser_last = pd.Series(df_forcing.index).groupby(ar_year).max()
    logger_supy.info(
        "Rust backend: streaming %d calendar-year chunks.", len(grp_forcing_chunk)
    )
    iter_chunk = _iter_suews_rust_chunks(
        config,
        grp_forcing_chunk,
        serial_mode,
        max_workers,
        initial_state_json_by_grid,
    )
    for last_timestamp in ser_last:
        df_output_year, dict_state_json, warnings_year = next(iter_chunk)
        yield df_output_year, dict_state_json, warnings_year, last_timestamp
        # Drop this frame before the next year is run, as the chunk
        # generator does, so only one year is held at a time.
        del df_output_year
