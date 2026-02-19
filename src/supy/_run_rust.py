"""Rust library backend for SUEWS simulation."""

from __future__ import annotations

from importlib import import_module
from typing import TYPE_CHECKING, Any

import numpy as np
import pandas as pd
import json as _json

import yaml

from ._env import logger_supy
from ._post import gen_index

if TYPE_CHECKING:
    from .data_model import SUEWSConfig

OUTPUT_SUEWS_COLS = 118
OUTPUT_TIME_COLS = 5

# All 11 output groups concatenated (match Rust/Fortran constants).
OUTPUT_ALL_COLS = 1134

# Ordered list of (group_name, total_cols_including_datetime) matching
# the Fortran concatenation layout.  Group names must match the
# ``dict_var_lower`` keys in ``_post.py`` (lowercased for lookup).
OUTPUT_GROUP_LAYOUT: list[tuple[str, int]] = [
    ("SUEWS", 118),
    ("snow", 103),
    ("BEERS", 34),
    ("ESTM", 32),
    ("EHC", 229),
    ("DailyState", 52),
    ("RSL", 140),
    ("debug", 136),
    ("SPARTACUS", 199),
    ("STEBBS", 85),
    ("NHood", 6),
]

_RUST_ERROR_MSG = (
    "Rust backend not available in this build.\n"
    "Rebuild/install SuPy with Meson Rust bridge enabled (e.g. make dev or make dev-dts)."
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


def _normalise_grid_id(grid_id: Any) -> int:
    if hasattr(grid_id, "value"):
        return int(grid_id.value)
    return int(grid_id)


def _build_datetime_index(output_block: np.ndarray) -> pd.DatetimeIndex:
    year = output_block[:, 0].astype(np.int64)
    day_of_year = output_block[:, 1].astype(np.int64)
    hour = output_block[:, 2].astype(np.int64)
    minute = output_block[:, 3].astype(np.int64)

    date_part = pd.to_datetime(year * 1000 + day_of_year, format="%Y%j")
    return date_part + pd.to_timedelta(hour, unit="h") + pd.to_timedelta(minute, unit="m")


def _prepare_forcing_block_fallback(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare forcing block locally when DTS runner helper is unavailable."""
    len_sim = len(df_forcing)
    block = np.zeros((len_sim, 21), dtype=np.float64, order="F")

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
        (20, "lai", 0.0),
    ]

    for idx, col, default in col_map:
        if col in df_forcing.columns:
            block[:, idx] = df_forcing[col].values
        else:
            block[:, idx] = default

    return block


def _prepare_forcing_block(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare forcing block using DTS helper when available."""
    try:
        from .dts._runner import _prepare_forcing_block as dts_prepare
    except Exception:
        return _prepare_forcing_block_fallback(df_forcing)
    return dts_prepare(df_forcing)


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
        df_group = pd.DataFrame(group_data, columns=idx, index=datetime_index)
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
    config: "SUEWSConfig",
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
) -> tuple[pd.DataFrame, str | None]:
    """Run SUEWS via Rust bridge library.

    Returns ``(df_output, state_json)`` where *state_json* is a JSON string
    encoding the post-simulation state (or ``None`` if unavailable).
    """
    _check_rust_available()
    if df_forcing.empty:
        raise ValueError("forcing data is empty")

    rust_module = _load_rust_module()
    config_yaml = yaml.dump(
        config.model_dump(exclude_none=True, mode="json"),
        default_flow_style=False,
        sort_keys=False,
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


def run_suews_rust_multi(
    config: "SUEWSConfig",
    df_forcing: pd.DataFrame,
) -> tuple[pd.DataFrame, dict[int, str] | None]:
    """Run SUEWS via Rust bridge for all sites in configuration.

    Iterates over ``config.sites``, creates a single-site config copy for
    each, calls :func:`run_suews_rust`, and concatenates the results into a
    single DataFrame with a ``(grid, datetime)`` MultiIndex.

    Returns ``(df_output, dict_state_json)`` where *dict_state_json* maps
    each grid ID to its post-simulation state JSON string.
    """
    sites = config.sites

    # Validate unique grid IDs
    list_gridiv = [s.gridiv for s in sites]
    list_dupes = [g for g in list_gridiv if list_gridiv.count(g) > 1]
    if list_dupes:
        raise ValueError(
            f"Duplicate gridiv values in config.sites: {set(list_dupes)}"
        )

    list_df_output = []
    dict_state_json: dict[int, str] = {}

    for idx, site in enumerate(sites):
        grid_id = _normalise_grid_id(site.gridiv)
        logger_supy.debug(
            "Rust backend: running site %d/%d (gridiv=%d)",
            idx + 1,
            len(sites),
            grid_id,
        )

        # Create a single-site config copy so the Rust bridge (which
        # always reads sites[0]) processes the correct site.
        config_single = config.model_copy(deep=True)
        config_single.sites = [site.model_copy(deep=True)]

        df_output, state_json = run_suews_rust(
            config=config_single,
            df_forcing=df_forcing,
            grid_id=grid_id,
        )
        list_df_output.append(df_output)
        if state_json is not None:
            dict_state_json[grid_id] = state_json

    # Concatenate all grids -- each df already has (grid, datetime) index
    df_output_all = pd.concat(list_df_output).sort_index()

    return df_output_all, dict_state_json or None


def run_suews_rust_with_state(
    config: "SUEWSConfig",
    df_forcing: pd.DataFrame,
    grid_id: int = 1,
    state_json: str = "",
) -> tuple[pd.DataFrame, str | None]:
    """Run SUEWS via Rust bridge with injected state from a previous chunk."""
    _check_rust_available()
    if df_forcing.empty:
        raise ValueError("forcing data is empty")

    rust_module = _load_rust_module()
    config_yaml = yaml.dump(
        config.model_dump(exclude_none=True, mode="json"),
        default_flow_style=False,
        sort_keys=False,
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
    config: "SUEWSConfig",
    df_forcing: pd.DataFrame,
    chunk_day: int = 366,
) -> tuple[pd.DataFrame, dict[int, str] | None]:
    """Run SUEWS via Rust bridge with multi-chunk state chaining.

    Splits forcing into chunks of *chunk_day* days, runs each chunk
    sequentially, and threads the final state of chunk N into chunk N+1
    for every grid.  Single-chunk forcing delegates without overhead.
    """
    # Group forcing into chunks (same logic as traditional backend)
    idx_start = df_forcing.index.min()
    idx_all = df_forcing.index
    grp_forcing_chunk = df_forcing.groupby(
        (idx_all - idx_start) // pd.Timedelta(chunk_day, "D")
    )

    n_chunk = len(grp_forcing_chunk)
    if n_chunk <= 1:
        return run_suews_rust_multi(config, df_forcing)

    logger_supy.info(
        "Rust backend: forcing split into %d chunks of <= %d days.",
        n_chunk,
        chunk_day,
    )

    sites = config.sites
    list_gridiv = [s.gridiv for s in sites]
    list_dupes = [g for g in list_gridiv if list_gridiv.count(g) > 1]
    if list_dupes:
        raise ValueError(
            f"Duplicate gridiv values in config.sites: {set(list_dupes)}"
        )

    dict_state_json: dict[int, str] = {}
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
