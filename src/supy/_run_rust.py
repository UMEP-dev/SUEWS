"""Rust library backend for SUEWS simulation."""

from __future__ import annotations

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

if TYPE_CHECKING:
    from .data_model import SUEWSConfig

OUTPUT_TIME_COLS = 5

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
    """Check suews_bridge.run_suews is available."""
    module = _load_rust_module()
    if not hasattr(module, "run_suews"):
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

    date_part = pd.to_datetime(year * 1000 + day_of_year, format="%Y%j")
    return (
        date_part + pd.to_timedelta(hour, unit="h") + pd.to_timedelta(minute, unit="m")
    )


def _prepare_forcing_block(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare forcing DataFrame as a Fortran-order array for the Rust bridge."""
    len_sim = len(df_forcing)
    block = np.zeros((len_sim, 23), dtype=np.float64, order="F")

    block[:, 0] = df_forcing.index.year
    block[:, 1] = df_forcing.index.dayofyear
    block[:, 2] = df_forcing.index.hour
    block[:, 3] = df_forcing.index.minute

    col_map = [
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
        (20, "lai_dectr", 0.0),
        (21, "lai_evetr", 0.0),
        (22, "lai_grass", 0.0),
    ]

    for idx, col, default in col_map:
        if col in df_forcing.columns:
            block[:, idx] = df_forcing[col].values
        else:
            block[:, idx] = default

    return block


def _parse_output_block(
    output_flat: list[float],
    len_sim: int,
    grid_id: int,
) -> pd.DataFrame:
    """Reshape the flat output buffer into a multi-group DataFrame.

    The flat buffer contains *len_sim* rows of ``OUTPUT_ALL_COLS`` columns.
    Each of the 11 output groups occupies a contiguous slice and carries its
    own 5-column datetime prefix.  We extract the datetime from the first
    group (SUEWS), then for every group strip its datetime prefix and build
    a ``(group, var)`` MultiIndex column set via :func:`gen_index`.
    """
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

    list_df_group: list[pd.DataFrame] = []
    col_offset = 0
    for group_name, ncols in OUTPUT_GROUP_LAYOUT:
        group_data = output_block[:, col_offset + OUTPUT_TIME_COLS : col_offset + ncols]
        idx = gen_index(f"dataoutline{group_name.lower()}")
        try:
            df_group = pd.DataFrame(group_data, columns=idx, index=datetime_index)
        except ValueError as exc:
            raise RuntimeError(
                f"Failed to parse Rust backend output group '{group_name}': "
                f"received {group_data.shape[1]} data columns, but the Python "
                f"registry expects {len(idx)}. "
                f"{_output_layout_update_hint(group_name)} "
                f"Original pandas error: {exc}"
            ) from exc
        # SUEWS uses -999 as missing-data sentinel; expose these as NaN in
        # Python outputs so downstream filtering/resampling behaves correctly.
        df_group = df_group.mask(df_group == -999.0)
        list_df_group.append(df_group)
        col_offset += ncols

    df_output = pd.concat(list_df_group, axis=1)
    df_output.index = pd.MultiIndex.from_product(
        [[_normalise_grid_id(grid_id)], datetime_index],
        names=["grid", "datetime"],
    )
    return df_output


def run_suews_rust(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
) -> tuple[pd.DataFrame, str | None]:
    """Run SUEWS via Rust bridge library.

    Returns ``(df_output, state_json)`` where *state_json* is a JSON string
    encoding the post-simulation state (or ``None`` if unavailable).
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
    forcing_flat = forcing_block.ravel(order="C").tolist()

    output_flat, state_json, len_sim = rust_module.run_suews(
        config_yaml,
        forcing_flat,
        len(df_forcing),
    )

    if len_sim != len(df_forcing):
        raise RuntimeError(
            f"Rust backend length mismatch: forcing={len(df_forcing)}, output={len_sim}"
        )

    df_output = _parse_output_block(output_flat, len_sim, grid_id)
    return df_output, state_json


def _run_single_grid_worker(args: tuple) -> tuple[int, list, str | None, int]:
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
        (grid_id, output_flat, state_json, len_sim)
    """
    config_json, forcing_flat, len_forcing, grid_id = args
    rust_module = _check_rust_available()
    output_flat, state_json, len_sim = rust_module.run_suews(
        config_json,
        forcing_flat,
        len_forcing,
    )
    return grid_id, output_flat, state_json, len_sim


def run_suews_rust_multi(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    serial_mode: bool = False,
    max_workers: int | None = None,
) -> tuple[pd.DataFrame, dict[int, str] | None]:
    """Run SUEWS via Rust bridge for all sites in configuration.

    Iterates over ``config.sites``, patches the serialised config dict
    per site, and calls the Rust bridge directly.  Shared data (forcing
    block, base config dict) is prepared once to avoid redundant deep
    copies and YAML serialisation.

    When *serial_mode* is False and there are multiple sites, grids are
    run in parallel using ``multiprocessing.Pool`` with the ``spawn``
    context (safe for Fortran SAVE variables — each process gets its
    own address space).

    Returns ``(df_output, dict_state_json)`` where *dict_state_json* maps
    each grid ID to its post-simulation state JSON string.
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

    # --- Prepare shared data once ---
    # The Rust bridge's YAML preprocessor accepts both new snake_case and
    # legacy fused spellings (gh#1322), so the Pydantic dump is submitted
    # verbatim.
    config_dict = config.model_dump(exclude_none=True, mode="json")
    list_site_dict = [
        s.model_dump(exclude_none=True, mode="json") for s in sites
    ]
    forcing_block = _prepare_forcing_block(df_forcing)
    forcing_flat = forcing_block.ravel(order="C").tolist()
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
    has_rayon = hasattr(rust_module, "run_suews_multi")

    if use_rayon and has_rayon:
        logger_supy.info(
            "Running %d grids in parallel (Rust/Rayon)",
            len(sites),
        )
        raw_results = rust_module.run_suews_multi(
            list_config_jsons,
            forcing_flat,
            len_forcing,
        )
        # raw_results: list of (grid_index, output_flat, state_json, len_sim)
        # Sort by original index to preserve grid ordering
        raw_results.sort(key=lambda r: r[0])
        results = [
            (list_grid_ids[idx], output_flat, state_json, len_sim)
            for idx, output_flat, state_json, len_sim in raw_results
        ]
    else:
        results = []
        for idx, config_json in enumerate(list_config_jsons):
            output_flat, state_json, len_sim = rust_module.run_suews(
                config_json,
                forcing_flat,
                len_forcing,
            )
            results.append((list_grid_ids[idx], output_flat, state_json, len_sim))

    # --- Collect results ---
    list_df_output = []
    dict_state_json: dict[int, str] = {}

    for grid_id, output_flat, state_json, len_sim in results:
        if len_sim != len_forcing:
            raise RuntimeError(
                f"Rust backend length mismatch: forcing={len_forcing}, output={len_sim}"
            )
        df_output = _parse_output_block(output_flat, len_sim, grid_id)
        list_df_output.append(df_output)
        if state_json is not None:
            dict_state_json[grid_id] = state_json

    df_output_all = pd.concat(list_df_output).sort_index()

    return df_output_all, dict_state_json or None


def run_suews_rust_with_state(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
    state_json: str = "",
) -> tuple[pd.DataFrame, str | None]:
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
    forcing_flat = forcing_block.ravel(order="C").tolist()

    output_flat, new_state_json, len_sim = rust_module.run_suews_with_state(
        config_yaml,
        forcing_flat,
        len(df_forcing),
        state_json,
    )

    if len_sim != len(df_forcing):
        raise RuntimeError(
            f"Rust backend length mismatch: forcing={len(df_forcing)}, output={len_sim}"
        )

    df_output = _parse_output_block(output_flat, len_sim, grid_id)
    return df_output, new_state_json


def run_suews_rust_chunked(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    chunk_day: int = 366,
    serial_mode: bool = False,
    initial_state_json_by_grid: dict[int, str] | None = None,
) -> tuple[pd.DataFrame, dict[int, str] | None]:
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
            config, df_forcing, serial_mode=serial_mode
        )

    if n_chunk > 1:
        logger_supy.info(
            "Rust backend: forcing split into %d chunks of <= %d days.",
            n_chunk,
            chunk_day,
        )

    sites = config.sites
    list_gridiv = [_normalise_grid_id(s.gridiv) for s in sites]
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
                "Checkpoint grid IDs do not match config.sites: "
                + "; ".join(parts)
            )

    dict_state_json: dict[int, str] = dict(initial_state_json_by_grid)
    list_df_output: list[pd.DataFrame] = []

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

        list_df_chunk: list[pd.DataFrame] = []

        for site in sites:
            grid_id = _normalise_grid_id(site.gridiv)
            config_single = config.model_copy(deep=True)
            config_single.sites = [site.model_copy(deep=True)]

            prev_state = dict_state_json.get(grid_id)
            if prev_state is not None:
                df_out, new_state = run_suews_rust_with_state(
                    config=config_single,
                    df_forcing=df_forcing_chunk,
                    grid_id=grid_id,
                    state_json=prev_state,
                )
            else:
                df_out, new_state = run_suews_rust(
                    config=config_single,
                    df_forcing=df_forcing_chunk,
                    grid_id=grid_id,
                )

            list_df_chunk.append(df_out)
            if new_state is not None:
                dict_state_json[grid_id] = new_state

        list_df_output.append(pd.concat(list_df_chunk))

    df_output_all = pd.concat(list_df_output).sort_index()
    return df_output_all, dict_state_json or None
