"""Shared finiteness, reduction, and conservation checks for Layer 5 physics tests.

Enforces three defensive contracts across all Layer 5 physics modules:
1. Active-variable, required-column, and expected-timestep completeness and
   finiteness: every active output group (``SUEWS``, ``DailyState``, ``BEERS``,
   ``RSL``, ``debug``, plus ``EHC``, ``SPARTACUS``, and ``snow`` when enabled)
   must match the forcing-derived timestamps and grid IDs exactly, contain all
   registered columns for that group, reject both non-finite values
   (``NaN``/``+/-Inf``) and exact ``-999.0`` missing-data sentinels on every
   active diagnostic, and preserve valid large negative fluxes (e.g. ``-950``).
   Every genuinely absent diagnostic is named explicitly with its Fortran
   source and verified to remain 100% ``NaN``.
2. Strict finite reductions: mean, sum, max-abs, and grouped reductions fail
   loudly if a single ``NaN``, ``+/-Inf``, or ``-999.0`` sentinel appears in
   the input slice, preventing silent ``skipna=True`` reduction masking.
3. Explicit named-surface and timestamp-aligned water-balance closure without
   positional dot products or ``dropna()`` masking.
"""

from __future__ import annotations

from collections.abc import Iterable
import re

import numpy as np
import pandas as pd

from supy.data_model.output.registry import OUTPUT_REGISTRY
from supy.data_model.output.variables import OutputGroup

# Missing-data sentinel emitted by the Fortran kernel and replaced in place by
# src/supy/_run_rust.py (`ar_slab == -999.0`). Exact equality avoids rejecting
# physically valid large negative fluxes such as -950 or -1087 W m-2.
SUEWS_MISSING_SENTINEL: float = -999.0

# SUEWS columns legitimately absent (set to NAN = -999 -> NaN) when observed
# downward longwave forcing is used (ldown_option == 1,
# src/suews/src/suews_ctrl_driver.f95:2108-2110).
SUEWS_ABSENT_OBSERVED_LDOWN: tuple[str, ...] = ("Fcld",)

# DailyState OHM regression coefficients legitimately set to NAN (-999 -> NaN)
# when StorageHeatMethod == EHC (src/suews/src/suews_ctrl_driver.f95:2459-2461).
DAILYSTATE_ABSENT_UNDER_EHC: tuple[str, ...] = ("a1", "a2", "a3")

# Explicit core SUEWS flux/state columns guaranteed to remain required.
REQUIRED_SUEWS_CORE_COLUMNS: tuple[str, ...] = (
    "Kdown",
    "Kup",
    "Ldown",
    "Lup",
    "Tsurf",
    "QN",
    "QF",
    "QS",
    "QH",
    "QE",
    "Rain",
    "Irr",
    "Evap",
    "RO",
    "State",
    "SMD",
    "T2",
    "RH2",
    "U10",
    "UStar",
)

# Explicit named pairing between debug soil-store columns and df_state sfr_surf
# keys (matching Fortran surface order PavSurf..WaterSurf and _forcing.py).
SURFACE_SOIL_STORAGE_MAP: tuple[tuple[str, str], ...] = (
    ("ss_Paved_next", "(0,)"),
    ("ss_Bldgs_next", "(1,)"),
    ("ss_EveTr_next", "(2,)"),
    ("ss_DecTr_next", "(3,)"),
    ("ss_Grass_next", "(4,)"),
    ("ss_BSoil_next", "(5,)"),
    ("ss_Water_next", "(6,)"),
)

_LAYER_SUFFIX_RE = re.compile(r"_(\d+)$")


def expected_group_columns(group: str) -> tuple[str, ...]:
    """Return authoritative registered column names for output ``group``."""
    cols = tuple(v.name for v in OUTPUT_REGISTRY.by_group(OutputGroup(group)))
    assert cols, f"Output registry returned no columns for group {group!r}"
    if group == "SUEWS":
        missing_core = sorted(set(REQUIRED_SUEWS_CORE_COLUMNS) - set(cols))
        assert not missing_core, (
            f"SUEWS registry missing core required columns: {missing_core}"
        )
    return cols


def _unused_vertical_layer_columns(
    columns: Iterable[object], nlayer: int
) -> tuple[str, ...]:
    """Return columns for unused fixed-width vertical-layer slots (index > ``nlayer``)."""
    unused = []
    for col in columns:
        match = _LAYER_SUFFIX_RE.search(str(col))
        if match is not None and int(match.group(1)) > nlayer:
            unused.append(str(col))
    return tuple(unused)


def strict_finite_array(values, *, label: str = "values") -> np.ndarray:
    """Convert ``values`` to float64 and reject NaN, +/-Inf, or exact -999.0."""
    arr = np.asarray(values, dtype=float)
    assert arr.size > 0, f"{label}: expected non-empty array for strict reduction"
    n_non_finite = int(np.count_nonzero(~np.isfinite(arr)))
    assert n_non_finite == 0, (
        f"{label}: found {n_non_finite}/{arr.size} non-finite entries "
        "(refusing silent NaN/Inf reduction masking)"
    )
    n_sentinel = int(np.count_nonzero(np.equal(arr, SUEWS_MISSING_SENTINEL)))
    assert n_sentinel == 0, (
        f"{label}: found {n_sentinel}/{arr.size} exact -999.0 missing-data sentinels"
    )
    return arr


def strict_mean(values, *, label: str = "values") -> float:
    """Arithmetic mean after asserting finite non-sentinel entries."""
    return float(np.mean(strict_finite_array(values, label=label)))


def strict_sum(values, *, label: str = "values") -> float:
    """Arithmetic sum after asserting finite non-sentinel entries."""
    return float(np.sum(strict_finite_array(values, label=label)))


def strict_max_abs(values, *, label: str = "values") -> float:
    """Maximum absolute value after asserting finite non-sentinel entries."""
    return float(np.max(np.abs(strict_finite_array(values, label=label))))


def strict_groupby_reduce(
    series: pd.Series,
    keys: pd.Index | pd.Series,
    *,
    agg: str,
    expected_counts: dict[object, int] | None = None,
    label: str = "series",
) -> pd.Series:
    """Group ``series`` by ``keys`` after proving zero NaN/Inf/-999 entries exist."""
    strict_finite_array(series.to_numpy(dtype=float), label=label)
    grouped = series.groupby(keys)
    if expected_counts is not None:
        counts = grouped.size()
        for key, expected_n in expected_counts.items():
            actual_n = int(counts.get(key, 0))
            assert actual_n == expected_n, (
                f"{label}: group {key!r} expected {expected_n} timesteps, got {actual_n}"
            )
    if agg == "sum":
        result = grouped.sum(min_count=1)
    elif agg == "mean":
        result = grouped.mean()
    else:
        raise ValueError(f"Unsupported agg {agg!r}; expected 'sum' or 'mean'")
    strict_finite_array(result.to_numpy(dtype=float), label=f"{label}.{agg}")
    return result


def expected_dailystate_mask(index: pd.DatetimeIndex, tstep: int) -> np.ndarray:
    """Return mask of exact end-of-day rows where Fortran writes ``DailyState``.

    Matches ``last_tstep_Q = time_now%getDay() /= time_next%getDay()`` in
    ``src/suews/src/suews_phys_dailystate.f95:269`` across every supported
    ``tstep`` (e.g. ``23:55`` for 300 s, ``23:00`` for 3600 s).
    """
    next_ts = index + pd.Timedelta(seconds=int(tstep))
    return np.asarray(next_ts.normalize() != index.normalize(), dtype=bool)


def _assert_group_finiteness(
    sub: pd.DataFrame,
    *,
    group: str,
    expected_columns: tuple[str, ...],
    expected_all_nan: tuple[str, ...] = (),
) -> None:
    """Verify exact required columns, documented absent NaNs, and active finiteness."""
    assert len(sub) > 0, f"{group}: expected at least one row to check"
    assert sub.columns.is_unique, f"{group}: duplicate column names detected"

    missing_required = sorted(set(expected_columns) - set(sub.columns))
    assert not missing_required, (
        f"{group}: missing required output columns: {missing_required}"
    )

    absent_set = set(expected_all_nan)
    missing_declared = sorted(absent_set - set(sub.columns))
    assert not missing_declared, (
        f"{group}: declared absent columns not in output: {missing_declared}"
    )

    unexpectedly_populated = [
        col for col in expected_all_nan if not sub[col].isna().all()
    ]
    assert not unexpectedly_populated, (
        f"{group}: columns declared absent actually hold values: "
        f"{unexpectedly_populated}"
    )

    active_cols = [col for col in sub.columns if col not in absent_set]
    assert active_cols, f"{group}: no active columns remaining to check"
    active_arr = sub[active_cols].to_numpy(dtype=float)
    non_finite_mask = ~np.isfinite(active_arr)
    sentinel_mask = np.equal(active_arr, SUEWS_MISSING_SENTINEL)
    bad_cols_mask = (non_finite_mask | sentinel_mask).any(axis=0)
    if np.any(bad_cols_mask):
        bad_details = {}
        for col_idx in np.flatnonzero(bad_cols_mask):
            col_name = active_cols[int(col_idx)]
            col_vals = active_arr[:, int(col_idx)]
            bad_details[col_name] = {
                "nan": int(np.isnan(col_vals).sum()),
                "inf": int(np.isinf(col_vals).sum()),
                "sentinel_-999": int(
                    np.count_nonzero(np.equal(col_vals, SUEWS_MISSING_SENTINEL))
                ),
                "total": int(col_vals.size),
            }
        raise AssertionError(
            f"{group}: non-finite or -999 sentinel values in active columns: "
            f"{bad_details}"
        )


def _extract_required_group(grid_df: pd.DataFrame, group: str) -> pd.DataFrame:
    """Return output ``group`` from ``grid_df`` or fail loudly if absent."""
    top_groups = set(grid_df.columns.get_level_values(0))
    assert group in top_groups, f"Required output group {group!r} missing from output"
    return grid_df[group]


def assert_active_output_finiteness(
    grid_df: pd.DataFrame,
    *,
    expected_timestamps: pd.DatetimeIndex,
    tstep: int = 300,
    storage_heat: int = 1,
    net_radiation: int = 3,
    snow_use: int = 0,
    nlayer: int = 3,
    suews_absent: tuple[str, ...] = SUEWS_ABSENT_OBSERVED_LDOWN,
) -> None:
    """Assert exact timestamp alignment, required columns, and finiteness for one grid."""
    expected_idx = pd.DatetimeIndex(expected_timestamps)
    assert len(expected_idx) > 0, "expected_timestamps must be non-empty"
    assert expected_idx.is_unique, "expected_timestamps must be unique"
    assert grid_df.index.is_unique, (
        "Output grid DataFrame contains duplicate timestamps"
    )
    assert len(grid_df) == len(expected_idx), (
        f"Output row count mismatch: got {len(grid_df)}, expected {len(expected_idx)}"
    )
    assert grid_df.index.equals(expected_idx), (
        "Output timestamps do not match forcing-derived expected timestamps "
        f"(got {grid_df.index[0]}..{grid_df.index[-1]}, "
        f"expected {expected_idx[0]}..{expected_idx[-1]})"
    )

    _assert_group_finiteness(
        _extract_required_group(grid_df, "SUEWS"),
        group="SUEWS",
        expected_columns=expected_group_columns("SUEWS"),
        expected_all_nan=suews_absent,
    )
    for always_active in ("BEERS", "RSL", "debug"):
        _assert_group_finiteness(
            _extract_required_group(grid_df, always_active),
            group=always_active,
            expected_columns=expected_group_columns(always_active),
        )

    ds = _extract_required_group(grid_df, "DailyState")
    eod_mask = expected_dailystate_mask(expected_idx, tstep)
    assert bool(eod_mask.any()), (
        f"DailyState: expected at least one end-of-day row for tstep={tstep}s"
    )
    non_eod = ds.loc[~eod_mask]
    if len(non_eod) > 0:
        non_eod_populated = [
            col for col in non_eod.columns if not non_eod[col].isna().all()
        ]
        assert not non_eod_populated, (
            f"DailyState: unexpected values on non-EOD rows: {non_eod_populated}"
        )
    ds_absent = DAILYSTATE_ABSENT_UNDER_EHC if int(storage_heat) == 5 else ()
    _assert_group_finiteness(
        ds.loc[eod_mask],
        group="DailyState[EOD]",
        expected_columns=expected_group_columns("DailyState"),
        expected_all_nan=ds_absent,
    )

    if int(storage_heat) == 5:
        ehc = _extract_required_group(grid_df, "EHC")
        ehc_cols = expected_group_columns("EHC")
        _assert_group_finiteness(
            ehc,
            group="EHC",
            expected_columns=ehc_cols,
            expected_all_nan=_unused_vertical_layer_columns(ehc_cols, nlayer),
        )
    if int(net_radiation) > 1000:
        sp = _extract_required_group(grid_df, "SPARTACUS")
        sp_cols = expected_group_columns("SPARTACUS")
        _assert_group_finiteness(
            sp,
            group="SPARTACUS",
            expected_columns=sp_cols,
            expected_all_nan=_unused_vertical_layer_columns(sp_cols, nlayer),
        )
    if int(snow_use) == 1:
        _assert_group_finiteness(
            _extract_required_group(grid_df, "snow"),
            group="snow",
            expected_columns=expected_group_columns("snow"),
        )


def assert_simulation_output_finiteness(
    df_output: pd.DataFrame,
    *,
    expected_grids: Iterable[int],
    expected_timestamps: pd.DatetimeIndex,
    tstep: int = 300,
    storage_heat: int = 1,
    net_radiation: int = 3,
    snow_use: int = 0,
    nlayer: int = 3,
    suews_absent: tuple[str, ...] = SUEWS_ABSENT_OBSERVED_LDOWN,
) -> None:
    """Assert exact ``(grid, datetime)`` MultiIndex integrity and per-grid finiteness."""
    grids = tuple(expected_grids)
    assert grids, "expected_grids must be non-empty"
    assert len(set(grids)) == len(grids), f"Duplicate expected grid IDs: {grids}"
    assert isinstance(df_output.index, pd.MultiIndex), (
        "Simulation output must use a (grid, datetime) MultiIndex"
    )
    assert df_output.index.nlevels == 2, (
        f"Expected 2-level MultiIndex, got {df_output.index.nlevels} levels"
    )
    assert df_output.index.is_unique, (
        "Simulation output contains duplicate (grid, datetime) index rows"
    )

    actual_grids = set(df_output.index.get_level_values(0).unique())
    assert actual_grids == set(grids), (
        f"Output grid IDs mismatch: got {sorted(actual_grids)}, "
        f"expected {sorted(grids)}"
    )
    expected_idx = pd.DatetimeIndex(expected_timestamps)
    expected_rows = len(grids) * len(expected_idx)
    assert len(df_output) == expected_rows, (
        f"Total output row count mismatch: got {len(df_output)}, "
        f"expected {expected_rows} ({len(grids)} grids x {len(expected_idx)} steps)"
    )

    for grid_id in grids:
        assert_active_output_finiteness(
            df_output.xs(grid_id, level=0),
            expected_timestamps=expected_idx,
            tstep=tstep,
            storage_heat=storage_heat,
            net_radiation=net_radiation,
            snow_use=snow_use,
            nlayer=nlayer,
            suews_absent=suews_absent,
        )


def _aligned_soil_storage_series(
    debug: pd.DataFrame,
    df_state: pd.DataFrame,
    *,
    grid: int,
    expected_index: pd.DatetimeIndex,
) -> pd.Series:
    """Compute area-weighted soil storage aligned explicitly by named surface and time."""
    missing_debug = [
        col for col, _ in SURFACE_SOIL_STORAGE_MAP if col not in debug.columns
    ]
    assert not missing_debug, (
        f"debug missing named soil-storage columns: {missing_debug}"
    )

    sfr_row = df_state.loc[grid, "sfr_surf"]
    expected_sfr_keys = tuple(key for _, key in SURFACE_SOIL_STORAGE_MAP)
    missing_sfr = [key for key in expected_sfr_keys if key not in sfr_row.index]
    assert not missing_sfr, f"df_state.sfr_surf missing surface keys: {missing_sfr}"
    assert len(sfr_row) == len(SURFACE_SOIL_STORAGE_MAP), (
        f"df_state.sfr_surf expected {len(SURFACE_SOIL_STORAGE_MAP)} surfaces, "
        f"got {len(sfr_row)}"
    )

    sfr_sum = 0.0
    soil_store = pd.Series(0.0, index=expected_index, dtype=float)
    for debug_col, sfr_key in SURFACE_SOIL_STORAGE_MAP:
        sfr_val = float(
            strict_finite_array([sfr_row.loc[sfr_key]], label=f"sfr_surf[{sfr_key}]")[0]
        )
        sfr_sum += sfr_val
        col_series = debug[debug_col]
        assert col_series.index.equals(expected_index), (
            f"debug.{debug_col} index misaligned with SUEWS"
        )
        col_arr = strict_finite_array(
            col_series.to_numpy(dtype=float), label=f"debug.{debug_col}"
        )
        soil_store += pd.Series(col_arr * sfr_val, index=expected_index)
    assert abs(sfr_sum - 1.0) < 1e-6, (
        f"Surface fractions do not sum to 1.0 (got {sfr_sum:.6f})"
    )
    return soil_store


def water_balance_residual_mm(
    df_output: pd.DataFrame,
    df_state: pd.DataFrame,
    *,
    grid: int = 1,
    expected_timestamps: pd.DatetimeIndex | None = None,
) -> float:
    """Maximum per-timestep water-balance residual (mm) aligned by named surface and time."""
    assert isinstance(df_output.index, pd.MultiIndex)
    assert df_output.index.is_unique, "df_output contains duplicate index rows"
    assert grid in df_output.index.get_level_values(0), (
        f"Grid {grid} missing from df_output"
    )
    assert grid in df_state.index, f"Grid {grid} missing from df_state"

    grid_df = df_output.xs(grid, level=0)
    suews = _extract_required_group(grid_df, "SUEWS")
    debug = _extract_required_group(grid_df, "debug")

    assert suews.index.is_unique, "SUEWS output contains duplicate timestamps"
    assert debug.index.is_unique, "debug output contains duplicate timestamps"
    assert suews.index.equals(debug.index), (
        "SUEWS and debug timestamps are not identically aligned"
    )
    if expected_timestamps is not None:
        assert suews.index.equals(pd.DatetimeIndex(expected_timestamps)), (
            "Water-balance timestamps do not match expected forcing timestamps"
        )
    assert len(suews) >= 2, "Water balance requires at least two timesteps"

    missing_suews = [
        col
        for col in ("State", "Rain", "Irr", "Evap", "RO")
        if col not in suews.columns
    ]
    assert not missing_suews, f"SUEWS missing water-balance columns: {missing_suews}"

    soil_store = _aligned_soil_storage_series(
        debug, df_state, grid=grid, expected_index=suews.index
    )
    total_store = soil_store + pd.Series(
        strict_finite_array(suews["State"].to_numpy(dtype=float), label="SUEWS.State"),
        index=suews.index,
    )
    net_input = pd.Series(
        strict_finite_array(suews["Rain"].to_numpy(dtype=float), label="SUEWS.Rain")
        + strict_finite_array(suews["Irr"].to_numpy(dtype=float), label="SUEWS.Irr")
        - strict_finite_array(suews["Evap"].to_numpy(dtype=float), label="SUEWS.Evap")
        - strict_finite_array(suews["RO"].to_numpy(dtype=float), label="SUEWS.RO"),
        index=suews.index,
    )
    eval_ts = suews.index[1:]
    delta_store = total_store.loc[eval_ts].to_numpy(dtype=float) - total_store.loc[
        suews.index[:-1]
    ].to_numpy(dtype=float)
    assert len(delta_store) == len(suews) - 1
    return strict_max_abs(
        delta_store - net_input.loc[eval_ts].to_numpy(dtype=float),
        label="water_balance_residual",
    )
