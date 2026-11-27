"""Shared finiteness, reduction, and conservation checks for Layer 5 physics tests.

Enforces three defensive contracts across all Layer 5 physics modules:
1. Active-variable, required-column, and expected-timestep completeness and
   finiteness: every active output group (``SUEWS``, ``DailyState``, ``BEERS``,
   ``RSL``, ``debug``, plus ``EHC``, ``SPARTACUS``, ``STEBBS``, and ``snow``
   when enabled)
   must match the forcing-derived timestamps and grid IDs exactly, contain all
   registered columns for that group, reject both non-finite values
   (``NaN``/``+/-Inf``) and exact ``-999.0`` missing-data sentinels on every
   active diagnostic, and preserve valid large negative fluxes (e.g. ``-950``).
   Fcld values and missing positions must match supplied public forcing.
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
import math
from pathlib import Path
import re
from typing import NamedTuple

import numpy as np
import pandas as pd

from supy.data_model.forcing import FORCING_REGISTRY
from supy.data_model.output.registry import OUTPUT_REGISTRY
from supy.data_model.output.variables import OutputGroup

# Missing-data sentinel emitted by the Fortran kernel and replaced in place by
# src/supy/_run_rust.py (`ar_slab == -999.0`). Exact equality avoids rejecting
# physically valid large negative fluxes such as -950 or -1087 W m-2.
SUEWS_MISSING_SENTINEL: float = float(FORCING_REGISTRY.missing_value)

_FCLD_SPEC = FORCING_REGISTRY.by_file_name("fcld")
assert _FCLD_SPEC is not None and _FCLD_SPEC.validation_range is not None, (
    "Forcing registry missing fcld validation range"
)
_FCLD_MIN: float = float(_FCLD_SPEC.validation_range[0])
_FCLD_MAX: float = float(_FCLD_SPEC.validation_range[1])

# Saved SUEWS text tables write non-water columns with %.4f and missing cells
# as -999.0000 (src/supy/_save.py:_write_df_txt). Allow half a unit in the
# final printed decimal place (5e-5) plus a 1e-12 summation margin. At an
# exact midpoint, either adjacent four-decimal token may pass.
_SAVED_DECIMAL_RE = re.compile(r"^-?\d+\.\d{4}$")
_SAVED_MISSING_TOKEN = "-999.0000"
_SAVED_TEXT_ROUNDING_ATOL: float = 5e-5 + 1e-12
_SAVED_REQUIRED_HEADER_COLUMNS: tuple[str, ...] = (
    "Year",
    "DOY",
    "Hour",
    "Min",
    "Dectime",
    "Fcld",
)

# DailyState OHM regression coefficients legitimately set to NAN (-999 -> NaN)
# when OHM is bypassed: StorageHeatMethod == EHC (5,
# src/suews/src/suews_ctrl_driver.f95:2459-2461) or OBSERVED (0,
# src/suews/src/suews_ctrl_driver.f95:2553-2585).
DAILYSTATE_ABSENT_UNDER_EHC: tuple[str, ...] = ("a1", "a2", "a3")

# SUEWS radiation-component outputs legitimately set to NAN (-999 -> NaN)
# when NetRadiationMethod == 0 and SnowUse == 0 (observed QN without snow,
# src/suews/src/suews_ctrl_driver.f95:2168-2181; when SnowUse == 1, RadMethod
# in src/suews/src/suews_phys_narp.f95:65-69 routes to sentinel 3000 and NARP
# populates these diagnostics).
SUEWS_ABSENT_UNDER_OBSERVED_QN: tuple[str, ...] = ("Kup", "Ldown", "Lup", "Tsurf")

# SUEWS per-surface soil moisture deficits legitimately set to NAN
# (-999 -> NaN) when SMDMethod == 1 (observed bulk soil moisture,
# src/suews/src/suews_phys_waterdist.f95:970-974).
SUEWS_ABSENT_UNDER_OBSERVED_SMD: tuple[str, ...] = (
    "SMDPaved",
    "SMDBldgs",
    "SMDEveTr",
    "SMDDecTr",
    "SMDGrass",
    "SMDBSoil",
)

# RSL canopy-parameter diagnostics legitimately set to -999 -> NaN when
# RSLProfile selects the MOST branch (flag_RSL == 0.0,
# src/suews/src/suews_phys_rslprof.f95:521-525).
RSL_ABSENT_UNDER_MOST: tuple[str, ...] = ("Lc", "beta", "elm", "Scc", "f")

# Non-SUEWS, non-DailyState groups populated at every kernel timestep in full
# combined SUEWS output (matching assert_active_output_finiteness).
ALWAYS_ACTIVE_NON_SUEWS_GROUPS: tuple[str, ...] = ("BEERS", "RSL", "debug")

# Conditionally enabled right-labelled groups explicitly allowed to remain 100%
# NaN natively and after resampling when their physics option is disabled:
# snow (snow_use == 1), ESTM (storage_heat == ESTM), EHC (storage_heat == EHC),
# SPARTACUS (net_radiation > 1000), STEBBS (stebbs enabled), NHood (neighbourhood
# output enabled), and BL (boundary-layer registry group when present).
OPTIONAL_RIGHT_LABELLED_GROUPS: tuple[str, ...] = (
    "snow",
    "ESTM",
    "EHC",
    "SPARTACUS",
    "STEBBS",
    "NHood",
    "BL",
)

_REGISTERED_OUTPUT_GROUPS: frozenset[str] = frozenset(
    group.value for group in OutputGroup
)

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


_DATETIME_UNIT_ORDER: dict[str, int] = {"s": 0, "ms": 1, "us": 2, "ns": 3}


def _align_datetime_indexes(
    *indexes: pd.DatetimeIndex,
) -> tuple[pd.DatetimeIndex, ...]:
    """Cast DatetimeIndex clocks to their finest storage unit without downcasting."""
    assert indexes, "At least one DatetimeIndex is required"
    units = [idx.unit for idx in indexes]
    for unit in units:
        assert unit in _DATETIME_UNIT_ORDER, (
            f"Unsupported datetime storage resolution: {unit!r}"
        )
    target_unit = max(units, key=_DATETIME_UNIT_ORDER.__getitem__)
    return tuple(idx.as_unit(target_unit) for idx in indexes)


def _same_datetime_instants(left: pd.Index, right: pd.Index) -> bool:
    """Compare ordered datetime instants while ignoring storage resolution."""
    if not (isinstance(left, pd.DatetimeIndex) and isinstance(right, pd.DatetimeIndex)):
        return False
    if left.hasnans or right.hasnans or left.tz != right.tz:
        return False
    try:
        aligned_left, aligned_right = _align_datetime_indexes(left, right)
    except pd.errors.OutOfBoundsDatetime:
        return False
    return aligned_left.equals(aligned_right)


def expected_dailystate_mask(index: pd.DatetimeIndex, tstep: int) -> np.ndarray:
    """Return mask of exact end-of-day rows where Fortran writes ``DailyState``.

    Matches ``last_tstep_Q`` in ``src/suews/src/suews_phys_dailystate.f95``
    under the unified right-closed day-ownership rule ``date(t - 1 s)``
    across every supported ``tstep`` on both midnight-aligned (``D+1 00:00``)
    and offset (e.g. ``D 23:57``) grids.
    """
    one_sec = pd.Timedelta(seconds=1).as_unit(index.unit)
    step_dt = pd.Timedelta(seconds=int(tstep)).as_unit(index.unit)
    day_now = (index - one_sec).normalize()
    day_next = (index + step_dt - one_sec).normalize()
    return np.asarray(day_now != day_next, dtype=bool)


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


def _assert_beers_finiteness(grid_df: pd.DataFrame, *, has_buildings: bool) -> None:
    """Verify BEERS finiteness when buildings exist, or all-NaN when absent."""
    beers_cols = expected_group_columns("BEERS")
    beers = _extract_required_group(grid_df, "BEERS")
    if has_buildings:
        _assert_group_finiteness(beers, group="BEERS", expected_columns=beers_cols)
        return
    # BEERS_cal_main (src/suews/src/suews_phys_beers.f95:184,391-394)
    # emits -999 -> NaN across all columns when sfr_surf(BldgSurf) <= 0.
    missing_beers = sorted(set(beers_cols) - set(beers.columns))
    assert not missing_beers, f"BEERS: missing required output columns: {missing_beers}"
    unexpectedly_populated = [col for col in beers_cols if not beers[col].isna().all()]
    assert not unexpectedly_populated, (
        "BEERS: expected all-NaN output when bldgs.sfr == 0, "
        f"but populated columns found: {unexpectedly_populated}"
    )


def _assert_rsl_finiteness(grid_df: pd.DataFrame) -> None:
    """Verify RSL finiteness, accounting for MOST-only absent canopy diagnostics."""
    rsl = _extract_required_group(grid_df, "RSL")
    rsl_cols = expected_group_columns("RSL")
    flag_rsl = strict_finite_array(rsl["flag_RSL"], label="RSL.flag_RSL")
    rsl_mask = np.equal(flag_rsl, 1.0)
    most_mask = np.equal(flag_rsl, 0.0)
    assert bool((rsl_mask | most_mask).all()), (
        "RSL: flag_RSL must be 0.0 (MOST) or 1.0 (RSL)"
    )
    if bool(rsl_mask.any()):
        _assert_group_finiteness(
            rsl.loc[rsl_mask],
            group="RSL[flag_RSL=1]",
            expected_columns=rsl_cols,
        )
    if bool(most_mask.any()):
        _assert_group_finiteness(
            rsl.loc[most_mask],
            group="RSL[flag_RSL=0]",
            expected_columns=rsl_cols,
            expected_all_nan=RSL_ABSENT_UNDER_MOST,
        )


class _CloudSurface(NamedTuple):
    """Extracted cloud series and optional multi-group context for one grid."""

    cloud: pd.Series
    suews_frame: pd.DataFrame | None
    dailystate_frame: pd.DataFrame | None
    has_dailystate: bool | None
    right_labelled_groups: dict[str, pd.DataFrame] | None
    observed_frame: pd.DataFrame | None
    is_multi_group: bool


class _NativeGroupEvidence(NamedTuple):
    """Native column schema and expected resampled bins for one output group."""

    columns: tuple[str, ...]
    expected_bins: pd.DatetimeIndex
    is_active: bool


def _validate_timestep_pair(
    tstep: int,
    output_step: int | None,
    *,
    label: str,
) -> None:
    """Validate native ``tstep`` and optional fixed-frequency ``output_step``."""
    assert (
        not isinstance(tstep, bool)
        and isinstance(tstep, int)
        and tstep > 0
        and 86400 % tstep == 0
    ), f"{label}: unsupported native timestep"
    if output_step is not None:
        assert (
            not isinstance(output_step, bool)
            and isinstance(output_step, int)
            and output_step >= tstep
            and output_step % tstep == 0
        ), f"{label}: output_step must be a fixed integer multiple of tstep"


def _validate_single_grid_clock(
    index: pd.Index,
    *,
    label: str,
    kind: str,
) -> pd.DatetimeIndex:
    """Validate a naive, unique, NaT-free single-grid ``DatetimeIndex``."""
    assert isinstance(index, pd.DatetimeIndex), (
        f"{label}: {kind} needs a single-grid datetime clock"
    )
    assert index.tz is None, f"{label}: timezone-aware clocks are unsupported"
    assert index.is_unique, f"{label}: duplicate timestamps in {kind} clock"
    assert not index.hasnans, f"{label}: {kind} clock contains missing timestamps (NaT)"
    return index


def _validate_multiindex_grid_frame(
    frame: pd.DataFrame,
    *,
    label: str,
    kind: str,
) -> None:
    """Validate a single-grid DataFrame with MultiIndex ``(group, var)`` columns."""
    assert isinstance(frame, pd.DataFrame), (
        f"{label}: {kind} must be a single-grid DataFrame"
    )
    _validate_single_grid_clock(frame.index, label=label, kind=kind)
    assert frame.columns.is_unique, f"{label}: duplicate columns in {kind} DataFrame"
    assert isinstance(frame.columns, pd.MultiIndex), (
        f"{label}: {kind} DataFrame must use MultiIndex (group, var) columns"
    )


def _expected_dailystate_bins(
    eod_stamps: pd.DatetimeIndex,
    *,
    origin: pd.Timestamp,
    tstep: int,
    output_step: int | None,
) -> pd.DatetimeIndex:
    """Map native DailyState occurrence timestamps to their right-labelled output bins."""
    return _expected_right_labelled_bins(
        eod_stamps,
        origin=origin,
        tstep=tstep,
        output_step=output_step,
    )


def _expected_right_labelled_bins(
    native_stamps: pd.DatetimeIndex,
    *,
    origin: pd.Timestamp,
    tstep: int,
    output_step: int | None,
) -> pd.DatetimeIndex:
    """Map native right-labelled group timestamps to their expected output bins."""
    if len(native_stamps) == 0:
        return pd.DatetimeIndex([], dtype=native_stamps.dtype)
    if output_step is None or output_step == tstep:
        return pd.DatetimeIndex(native_stamps).unique().sort_values()

    unit = native_stamps.unit
    tick = pd.Timedelta(1, unit).as_unit(unit)
    step = pd.Timedelta(seconds=output_step).as_unit(unit)
    origin_aligned = pd.DatetimeIndex([origin]).as_unit(unit)[0]
    labels = pd.DatetimeIndex(
        origin_aligned + (((native_stamps - origin_aligned) - tick) // step + 1) * step
    ).as_unit(unit)
    return labels.unique().sort_values()


def _dailystate_only_timestamps(
    expected_ds_all: pd.DatetimeIndex,
    suews_index: pd.DatetimeIndex,
) -> pd.DatetimeIndex:
    """Return expected DailyState timestamps that fall outside ``suews_index``."""
    ds_all_cmp, suews_cmp = _align_datetime_indexes(expected_ds_all, suews_index)
    return ds_all_cmp.difference(suews_cmp)


def _extract_group_valid_stamps(
    native_group: pd.DataFrame,
    *,
    group_name: str,
    label: str,
) -> pd.DatetimeIndex:
    """Return populated native timestamps for ``group_name``, rejecting invalid-only rows."""
    assert len(native_group.columns) > 0, (
        f"{label}: native {group_name} group has no columns"
    )
    has_any = np.asarray(native_group.notna().any(axis=1), dtype=bool)
    has_valid = _rows_have_valid_observation(native_group)
    invalid_only = has_any & ~has_valid
    assert not bool(invalid_only.any()), (
        f"{label}: native {group_name} row(s) contain only non-finite or -999 "
        f"sentinel values: {native_group.index[invalid_only].tolist()}"
    )
    return native_group.index[has_valid]


def _extract_native_dailystate_stamps(
    native_grid: pd.DataFrame,
    *,
    label: str,
) -> pd.DatetimeIndex:
    """Return populated native DailyState timestamps, rejecting invalid-only rows."""
    _validate_multiindex_grid_frame(native_grid, label=label, kind="native")
    top_groups = set(native_grid.columns.get_level_values(0))
    assert "DailyState" in top_groups, (
        f"{label}: required output group 'DailyState' missing from native"
    )
    return _extract_group_valid_stamps(
        native_grid["DailyState"], group_name="DailyState", label=label
    )


def _extract_native_group_evidence(
    native_grid: pd.DataFrame,
    *,
    tstep: int,
    output_step: int | None,
    label: str,
) -> dict[str, _NativeGroupEvidence]:
    """Derive per-group columns and expected resampled bins from native output."""
    _validate_multiindex_grid_frame(native_grid, label=label, kind="native")
    assert len(native_grid) > 0, f"{label}: native DataFrame must be non-empty"
    origin = (
        native_grid.index.min() - pd.Timedelta(1, native_grid.index.unit)
    ).normalize()
    evidence: dict[str, _NativeGroupEvidence] = {}
    for group_name in native_grid.columns.get_level_values(0).unique():
        native_group = native_grid[group_name]
        native_stamps = _extract_group_valid_stamps(
            native_group, group_name=str(group_name), label=label
        )
        if group_name == "DailyState":
            expected_bins = _expected_dailystate_bins(
                native_stamps, origin=origin, tstep=tstep, output_step=output_step
            )
        else:
            expected_bins = _expected_right_labelled_bins(
                native_stamps,
                origin=origin,
                tstep=tstep,
                output_step=output_step,
            )
        evidence[str(group_name)] = _NativeGroupEvidence(
            columns=tuple(native_group.columns),
            expected_bins=expected_bins,
            is_active=len(expected_bins) > 0,
        )
    return evidence


def _assert_group_bins_populated(
    group_frame: pd.DataFrame,
    *,
    expected_bins: pd.DatetimeIndex,
    group_name: str,
    label: str,
) -> None:
    """Assert populated ``group_name`` at every expected bin and all-NaN elsewhere."""
    actual_index = group_frame.index
    try:
        expected_cmp, actual_cmp = _align_datetime_indexes(expected_bins, actual_index)
    except pd.errors.OutOfBoundsDatetime as exc:
        raise AssertionError(
            f"{label}: output clock does not align with expected {group_name} bins"
        ) from exc

    missing_bins = expected_cmp.difference(actual_cmp)
    assert len(missing_bins) == 0, (
        f"{label}: output clock missing expected {group_name} bin(s): "
        f"{missing_bins.tolist()}"
    )
    grp_pos = actual_cmp.get_indexer(expected_cmp)
    assert (grp_pos >= 0).all(), (
        f"{label}: output clock missing expected {group_name} bin(s)"
    )
    grp_times_actual = actual_index[grp_pos]
    if len(grp_times_actual) > 0:
        has_valid = _rows_have_valid_observation(group_frame.loc[grp_times_actual])
        assert bool(has_valid.all()), (
            f"{label}: expected {group_name} row(s) lack valid {group_name} values: "
            f"{grp_times_actual[~has_valid].tolist()}"
        )

    non_grp_mask = np.ones(len(actual_index), dtype=bool)
    if len(grp_pos) > 0:
        non_grp_mask[grp_pos] = False
    if bool(non_grp_mask.any()):
        non_grp_frame = group_frame.iloc[non_grp_mask]
        unexpected = np.asarray(non_grp_frame.notna().any(axis=1), dtype=bool)
        assert not bool(unexpected.any()), (
            f"{label}: unexpected {group_name} values outside expected {group_name} "
            f"bin(s): {non_grp_frame.index[unexpected].tolist()}"
        )


def _assert_native_group_evidence_grid(
    observed_grid: pd.DataFrame,
    native_evidence: dict[str, _NativeGroupEvidence],
    *,
    label: str,
) -> None:
    """Verify every active and inactive native group against one observed grid."""
    _validate_multiindex_grid_frame(observed_grid, label=label, kind="output")
    obs_groups = set(observed_grid.columns.get_level_values(0).unique())
    unexpected_groups = sorted(obs_groups - set(native_evidence))
    assert not unexpected_groups, (
        f"{label}: unexpected output group(s) absent from native: {unexpected_groups}"
    )
    for group_name, ev in native_evidence.items():
        if ev.is_active:
            assert group_name in obs_groups, (
                f"{label}: expected active output group {group_name!r} missing from output"
            )
            obs_group = observed_grid[group_name]
            missing_cols = sorted(set(ev.columns) - set(obs_group.columns))
            assert not missing_cols, (
                f"{label}: active output group {group_name!r} missing columns: "
                f"{missing_cols}"
            )
            _assert_group_bins_populated(
                obs_group,
                expected_bins=ev.expected_bins,
                group_name=group_name,
                label=label,
            )
        elif group_name in obs_groups:
            # Explicit allowance for inactive groups: zero expected bins;
            # every observed row must remain NaN with no fabricated entries.
            _assert_group_bins_populated(
                observed_grid[group_name],
                expected_bins=ev.expected_bins,
                group_name=group_name,
                label=label,
            )


def _expected_cloud_bins(
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_step: int | None,
) -> tuple[pd.Series, pd.DatetimeIndex, pd.DatetimeIndex]:
    """Derive required SUEWS cloud bins and expected DailyState bin timestamps.

    Expectations are computed directly from public kernel-cadence forcing using
    interval-end arithmetic, independently of ``pandas.Series.resample`` and
    ``supy._post._resample_output``:

    - Native output (``output_step is None`` or ``output_step == tstep``) keeps
      the forcing clock with zero extra structural rows and expects native
      ``DailyState`` only on Fortran end-of-day steps.
    - Fixed-frequency output (``output_step > tstep``) assigns each forcing row
      ``t`` to the right-closed, right-labelled bin ``(b - output_step, b]``
      anchored at ``forcing.index[0].normalize()`` and averages finite cloud
      observations per bin while retaining all-missing bins as ``NaN``.
    - Combined ``SUEWS`` + ``DailyState`` resampled frames map each end-of-day
      step to its right-labelled ``DailyState`` bin sharing the same fixed
      origin as ``SUEWS`` (aligned onto ``suews_index`` with zero exclusive
      structural rows for forcing-aligned EOD timestamps).
      Returns ``(expected_suews, expected_ds_all, expected_ds_only)``.
    """
    index = forcing.index
    assert isinstance(index, pd.DatetimeIndex), "Fcld: forcing needs a datetime clock"
    assert index.tz is None, "Fcld: timezone-aware clocks are unsupported"
    assert len(index) and index.is_unique, "Fcld: empty or duplicate forcing clock"
    assert not index.hasnans, "Fcld: forcing clock contains missing timestamps (NaT)"
    _validate_timestep_pair(tstep, output_step, label="Fcld")
    expected_native_clock = pd.date_range(
        index[0], periods=len(index), freq=f"{tstep}s", unit=index.unit
    )
    assert _same_datetime_instants(index, expected_native_clock), (
        "Fcld: forcing must be at the native timestep"
    )
    assert "fcld" in forcing, "Fcld: missing public forcing column fcld"

    expected_native = (
        forcing["fcld"].astype(float).replace(SUEWS_MISSING_SENTINEL, np.nan)
    )
    valid = expected_native.dropna()
    assert np.isfinite(valid).all() and valid.between(_FCLD_MIN, _FCLD_MAX).all(), (
        "Fcld: invalid supplied cloud fraction"
    )

    eod_stamps = index[expected_dailystate_mask(index, tstep)]
    if output_step is None or output_step == tstep:
        return expected_native, eod_stamps, pd.DatetimeIndex([], dtype=index.dtype)

    unit = index.unit
    tick = pd.Timedelta(1, unit).as_unit(unit)
    origin = (index.min() - tick).normalize()
    step = pd.Timedelta(seconds=output_step).as_unit(unit)
    bin_labels = pd.DatetimeIndex(
        origin + (((index - origin) - tick) // step + 1) * step
    ).as_unit(unit)
    # Every occupied measurement bin survives, including all-missing bins (NaN).
    expected_suews = expected_native.groupby(bin_labels, sort=True).mean()
    expected_ds_all = _expected_dailystate_bins(
        eod_stamps, origin=origin, tstep=tstep, output_step=output_step
    )
    expected_ds_only = _dailystate_only_timestamps(
        expected_ds_all, expected_suews.index
    )
    return expected_suews, expected_ds_all, expected_ds_only


def _extract_cloud_surface(observed: pd.Series | pd.DataFrame) -> _CloudSurface:
    """Normalise a single-grid Series or DataFrame input for cloud validation."""
    assert isinstance(observed, (pd.Series, pd.DataFrame)), (
        "Fcld: observed must be a pandas Series or single-grid DataFrame"
    )
    assert isinstance(observed.index, pd.DatetimeIndex), (
        "Fcld: output needs a single-grid datetime clock"
    )
    assert observed.index.tz is None, "Fcld: timezone-aware clocks are unsupported"
    assert observed.index.is_unique, "Fcld: duplicate timestamps in output clock"
    assert not observed.index.hasnans, (
        "Fcld: output clock contains missing timestamps (NaT)"
    )

    if isinstance(observed, pd.Series):
        return _CloudSurface(
            cloud=observed,
            suews_frame=None,
            dailystate_frame=None,
            has_dailystate=None,
            right_labelled_groups=None,
            observed_frame=None,
            is_multi_group=False,
        )

    assert observed.columns.is_unique, "Fcld: duplicate columns in output DataFrame"
    if isinstance(observed.columns, pd.MultiIndex):
        top_groups = tuple(observed.columns.get_level_values(0).unique())
        assert "SUEWS" in top_groups, "Fcld: required output group 'SUEWS' missing"
        suews_frame = observed["SUEWS"]
        assert "Fcld" in suews_frame.columns, (
            "Fcld: missing required output column Fcld"
        )
        has_dailystate = "DailyState" in top_groups
        dailystate_frame = observed["DailyState"] if has_dailystate else None
        right_labelled_groups = {
            grp: observed[grp] for grp in top_groups if grp != "DailyState"
        }
        return _CloudSurface(
            cloud=suews_frame["Fcld"],
            suews_frame=suews_frame,
            dailystate_frame=dailystate_frame,
            has_dailystate=has_dailystate,
            right_labelled_groups=right_labelled_groups,
            observed_frame=observed,
            is_multi_group=len(top_groups) > 1,
        )

    assert "Fcld" in observed.columns, "Fcld: missing required output column Fcld"
    return _CloudSurface(
        cloud=observed["Fcld"],
        suews_frame=observed,
        dailystate_frame=None,
        has_dailystate=None,
        right_labelled_groups=None,
        observed_frame=observed,
        is_multi_group=False,
    )


def _rows_have_valid_observation(frame: pd.DataFrame) -> np.ndarray:
    """Return per-row mask where at least one entry is finite and not -999.0."""
    arr = frame.to_numpy(dtype=float)
    return np.asarray(
        (np.isfinite(arr) & ~np.equal(arr, SUEWS_MISSING_SENTINEL)).any(axis=1),
        dtype=bool,
    )


def _assert_right_labelled_groups_without_native(
    right_labelled_groups: dict[str, pd.DataFrame],
    *,
    expected_suews_index: pd.DatetimeIndex,
    extra_times: pd.DatetimeIndex,
    is_multi_group: bool,
    label: str,
) -> None:
    """Check non-DailyState group occurrence and columns when ``native`` is absent."""
    if is_multi_group:
        missing_suews_cols = sorted(
            set(expected_group_columns("SUEWS"))
            - set(right_labelled_groups["SUEWS"].columns)
        )
        assert not missing_suews_cols, (
            f"{label}: active output group 'SUEWS' missing columns: "
            f"{missing_suews_cols}"
        )
        for always_grp in ALWAYS_ACTIVE_NON_SUEWS_GROUPS:
            assert always_grp in right_labelled_groups, (
                f"{label}: expected active output group {always_grp!r} missing from output"
            )
            grp_df = right_labelled_groups[always_grp]
            missing_cols = sorted(
                set(expected_group_columns(always_grp)) - set(grp_df.columns)
            )
            assert not missing_cols, (
                f"{label}: active output group {always_grp!r} missing columns: "
                f"{missing_cols}"
            )
            _assert_group_bins_populated(
                grp_df,
                expected_bins=expected_suews_index,
                group_name=always_grp,
                label=label,
            )

    checked_groups = {"SUEWS", *ALWAYS_ACTIVE_NON_SUEWS_GROUPS}
    for grp_name, grp_df in right_labelled_groups.items():
        if grp_name in checked_groups:
            continue
        if len(extra_times) > 0:
            populated_extra = [
                col
                for col in grp_df.columns
                if not grp_df.loc[extra_times, col].isna().all()
            ]
            assert not populated_extra, (
                f"{label}: DailyState-only structural row(s) contain {grp_name} "
                f"observations in columns: {populated_extra}"
            )
        meas_frame = grp_df.loc[expected_suews_index]
        has_any = np.asarray(meas_frame.notna().any(axis=1), dtype=bool)
        has_valid = _rows_have_valid_observation(meas_frame)
        invalid_only = has_any & ~has_valid
        assert not bool(invalid_only.any()), (
            f"{label}: {grp_name} measurement bin(s) contain only non-finite or "
            f"-999 sentinel values: {expected_suews_index[invalid_only].tolist()}"
        )
        if bool(has_valid.any()):
            if grp_name in _REGISTERED_OUTPUT_GROUPS:
                missing_cols = sorted(
                    set(expected_group_columns(grp_name)) - set(grp_df.columns)
                )
                assert not missing_cols, (
                    f"{label}: active output group {grp_name!r} missing columns: "
                    f"{missing_cols}"
                )
            _assert_group_bins_populated(
                grp_df,
                expected_bins=expected_suews_index,
                group_name=grp_name,
                label=label,
            )


def _assert_structural_rows(
    surface: _CloudSurface,
    *,
    expected_suews_index: pd.DatetimeIndex,
    extra_times: pd.DatetimeIndex,
    expected_ds_bins: pd.DatetimeIndex,
    native_evidence: dict[str, _NativeGroupEvidence] | None = None,
) -> None:
    """Keep DailyState rows (exclusive and overlapping) and active groups valid."""
    if len(extra_times) > 0:
        structural_cloud = surface.cloud.loc[extra_times]
        assert structural_cloud.isna().all(), (
            "Fcld: unexpected non-NaN cloud value on DailyState-only structural "
            f"row(s): {structural_cloud.dropna().to_dict()}"
        )
        if surface.suews_frame is not None:
            populated_suews = [
                col
                for col in surface.suews_frame.columns
                if not surface.suews_frame.loc[extra_times, col].isna().all()
            ]
            assert not populated_suews, (
                "Fcld: DailyState-only structural row(s) contain SUEWS "
                f"observations in columns: {populated_suews}"
            )

    if native_evidence is not None:
        assert surface.observed_frame is not None
        _assert_native_group_evidence_grid(
            surface.observed_frame, native_evidence, label="Fcld"
        )
    else:
        if surface.right_labelled_groups is not None:
            _assert_right_labelled_groups_without_native(
                surface.right_labelled_groups,
                expected_suews_index=expected_suews_index,
                extra_times=extra_times,
                is_multi_group=surface.is_multi_group,
                label="Fcld",
            )
        if surface.dailystate_frame is not None:
            if surface.is_multi_group:
                missing_ds_cols = sorted(
                    set(expected_group_columns("DailyState"))
                    - set(surface.dailystate_frame.columns)
                )
                assert not missing_ds_cols, (
                    "Fcld: active output group 'DailyState' missing columns: "
                    f"{missing_ds_cols}"
                )
            _assert_group_bins_populated(
                surface.dailystate_frame,
                expected_bins=expected_ds_bins,
                group_name="DailyState",
                label="Fcld",
            )

    if surface.suews_frame is not None:
        non_cloud_suews = surface.suews_frame.drop(columns="Fcld")
        if not non_cloud_suews.empty:
            has_suews_obs = _rows_have_valid_observation(
                non_cloud_suews.loc[expected_suews_index]
            )
            assert bool(has_suews_obs.all()), (
                "Fcld: required SUEWS measurement bin(s) contain no valid SUEWS "
                f"observations: {expected_suews_index[~has_suews_obs].tolist()}"
            )


def _iter_aligned_grids(
    observed: pd.DataFrame,
    native: pd.DataFrame,
    *,
    label: str,
) -> list[tuple[str, pd.DataFrame, pd.DataFrame]]:
    """Pair single-grid or multi-grid observed/native DataFrames with grid labels."""
    assert isinstance(observed, pd.DataFrame) and isinstance(native, pd.DataFrame), (
        f"{label}: observed and native must both be pandas DataFrames"
    )
    obs_multi = isinstance(observed.index, pd.MultiIndex)
    nat_multi = isinstance(native.index, pd.MultiIndex)
    assert obs_multi == nat_multi, (
        f"{label}: observed and native must both be single-grid or both multi-grid"
    )
    if not obs_multi:
        return [(label, observed, native)]

    assert observed.index.nlevels == 2 and native.index.nlevels == 2, (
        f"{label}: multi-grid frames must use a 2-level (grid, datetime) MultiIndex"
    )
    assert observed.index.is_unique and native.index.is_unique, (
        f"{label}: duplicate (grid, datetime) index rows detected"
    )
    obs_grids = set(observed.index.get_level_values(0).unique())
    nat_grids = set(native.index.get_level_values(0).unique())
    assert obs_grids == nat_grids, (
        f"{label}: output grid IDs {sorted(obs_grids)} do not match "
        f"native grid IDs {sorted(nat_grids)}"
    )
    return [
        (
            f"{label}[grid={grid_id}]",
            observed.xs(grid_id, level=0),
            native.xs(grid_id, level=0),
        )
        for grid_id in sorted(nat_grids)
    ]


def assert_dailystate_group_occurrence(
    observed: pd.DataFrame,
    native: pd.DataFrame,
    *,
    tstep: int = 300,
    output_step: int | None = None,
) -> None:
    """Compare native ``DailyState`` group occurrence with expected output bins.

    Supports both single-grid (``DatetimeIndex``) and multi-grid
    (``(grid, datetime)`` ``MultiIndex``) output frames. Each grid is checked
    independently: every native row holding at least one valid (finite,
    non-``-999.0``) ``DailyState`` observation is mapped to its fixed-frequency
    right-labelled bin sharing the grid's origin (or retained verbatim at
    native cadence), and the corresponding output bin must hold at least one
    valid ``DailyState`` observation, while all other output rows must remain
    ``NaN`` in ``DailyState``.

    Scoped strictly to populated-group existence rather than per-field
    completeness across all 47 fields under every physical configuration.
    """
    _validate_timestep_pair(tstep, output_step, label="DailyState")
    for grid_label, obs_grid, nat_grid in _iter_aligned_grids(
        observed, native, label="DailyState"
    ):
        _validate_multiindex_grid_frame(obs_grid, label=grid_label, kind="output")
        obs_ds = _extract_required_group(obs_grid, "DailyState")
        native_stamps = _extract_native_dailystate_stamps(nat_grid, label=grid_label)
        origin = (
            nat_grid.index.min() - pd.Timedelta(1, nat_grid.index.unit)
        ).normalize()
        expected_bins = _expected_dailystate_bins(
            native_stamps, origin=origin, tstep=tstep, output_step=output_step
        )
        if len(expected_bins) > 0:
            missing_cols = sorted(
                set(nat_grid["DailyState"].columns) - set(obs_ds.columns)
            )
            assert not missing_cols, (
                f"{grid_label}: active output group 'DailyState' missing columns: "
                f"{missing_cols}"
            )
        _assert_group_bins_populated(
            obs_ds,
            expected_bins=expected_bins,
            group_name="DailyState",
            label=grid_label,
        )


def assert_output_group_occurrence(
    observed: pd.DataFrame,
    native: pd.DataFrame,
    *,
    tstep: int = 300,
    output_step: int | None = None,
) -> None:
    """Verify occurrence and column retention for every native output group.

    Supports both single-grid (``DatetimeIndex``) and multi-grid
    (``(grid, datetime)`` ``MultiIndex``) output frames. Each grid is compared
    against its native evidence across ``DailyState`` and every
    non-``DailyState`` group (``SUEWS``, ``BEERS``, ``RSL``, ``debug``, plus
    enabled ``snow``, ``ESTM``, ``EHC``, ``SPARTACUS``, ``STEBBS``, ``NHood``,
    ``BL``, all right-labelled under the shared per-grid origin):

    - Every active group in ``native`` must remain present in ``observed`` with
      all native columns retained, holding at least one finite non-``-999.0``
      observation at every expected output bin and ``NaN`` on all other rows.
    - Every inactive group in ``native`` (zero valid native observations, such
      as disabled optional physics groups) is explicitly allowed to remain 100%
      ``NaN`` and fails if spurious or non-finite values appear.
    """
    _validate_timestep_pair(tstep, output_step, label="OutputGroup")
    for grid_label, obs_grid, nat_grid in _iter_aligned_grids(
        observed, native, label="OutputGroup"
    ):
        evidence = _extract_native_group_evidence(
            nat_grid, tstep=tstep, output_step=output_step, label=grid_label
        )
        _assert_native_group_evidence_grid(obs_grid, evidence, label=grid_label)


def _resolve_cloud_clock_slices(
    actual_index: pd.DatetimeIndex,
    *,
    expected_suews_index: pd.DatetimeIndex,
    expected_ds_only: pd.DatetimeIndex,
    has_dailystate: bool | None,
) -> tuple[pd.DatetimeIndex, pd.DatetimeIndex]:
    """Validate observed clock instants regardless of datetime storage resolution.

    Returns ``(suews_times_actual, extra_times_actual)`` sliced from
    ``actual_index`` in its own storage unit so downstream ``.loc`` access does
    not rely on cross-resolution coercion.
    """
    try:
        expected_suews_cmp, expected_ds_cmp, actual_cmp = _align_datetime_indexes(
            expected_suews_index, expected_ds_only, actual_index
        )
    except pd.errors.OutOfBoundsDatetime as exc:
        raise AssertionError(
            "Fcld: output clock does not match supplied forcing"
        ) from exc

    missing_bins = expected_suews_cmp.difference(actual_cmp)
    assert len(missing_bins) == 0, (
        f"Fcld: output clock missing required measurement bins: {missing_bins.tolist()}"
    )

    extra_times_cmp = actual_cmp.difference(expected_suews_cmp)
    unexpected_times = extra_times_cmp.difference(expected_ds_cmp)
    assert len(unexpected_times) == 0, (
        "Fcld: output clock contains unexpected timestamps outside forcing bins "
        f"and DailyState structural rows: {unexpected_times.tolist()}"
    )
    if has_dailystate is False:
        assert len(extra_times_cmp) == 0, (
            "Fcld: output clock has extra structural rows without a DailyState "
            f"group: {extra_times_cmp.tolist()}"
        )
    elif has_dailystate is True:
        assert extra_times_cmp.equals(expected_ds_cmp), (
            "Fcld: combined output structural rows do not match expected "
            f"DailyState-only timestamps (got {extra_times_cmp.tolist()}, "
            f"expected {expected_ds_cmp.tolist()})"
        )
    elif len(extra_times_cmp) > 0:
        assert extra_times_cmp.equals(expected_ds_cmp), (
            "Fcld: structural rows do not match expected DailyState-only "
            f"timestamps (got {extra_times_cmp.tolist()}, "
            f"expected {expected_ds_cmp.tolist()})"
        )

    expected_order = expected_suews_cmp.union(extra_times_cmp)
    assert actual_cmp.equals(expected_order), (
        "Fcld: output clock does not match supplied forcing"
    )

    suews_pos = actual_cmp.get_indexer(expected_suews_cmp)
    extra_pos = actual_cmp.get_indexer(extra_times_cmp)
    assert (suews_pos >= 0).all() and (extra_pos >= 0).all(), (
        "Fcld: output clock does not match supplied forcing"
    )
    return actual_index[suews_pos], actual_index[extra_pos]


def assert_forcing_cloud(
    observed: pd.Series | pd.DataFrame,
    forcing: pd.DataFrame,
    *,
    tstep: int = 300,
    output_step: int | None = None,
    native: pd.DataFrame | None = None,
) -> None:
    """Check Fcld against public, kernel-cadence forcing, never against output.

    The writer binds ``Fcld`` to ``forcing%f_cloud`` (``suews_ctrl_driver.f95``),
    not ``atmState%f_cloud``. Native rows retain the forcing clock with no extra
    rows. Fixed-frequency aggregates preserve every required right-closed,
    right-labelled SUEWS measurement bin (including all-missing bins) and place
    ``DailyState`` on the same right-labelled bin grid (leaving zero extra
    structural rows when native EOD steps sit on the forcing clock). Required
    SUEWS bins must stay distinguishable as SUEWS observations, and whenever
    ``observed`` carries a ``DailyState`` group, every expected ``DailyState``
    bin must hold at least one valid observation while all other rows remain
    ``NaN`` in ``DailyState``. Multi-group frames also enforce group occurrence
    and column retention for active non-``DailyState`` groups (always-active
    ``BEERS``, ``RSL``, ``debug``, plus enabled optional groups such as
    ``snow``, ``ESTM``, ``EHC``, ``SPARTACUS``, ``STEBBS``, ``NHood``, ``BL``)
    with explicit all-``NaN`` allowances for inactive optional groups. Callers
    that pass a Series or flat SUEWS DataFrame without ``native`` retain pure
    timestamp/forcing-only cloud validation. When ``native`` is supplied,
    expected bins, active groups, and required columns are derived directly
    from native output evidence.

    Naive clocks following the documented ``model.control.forcing.timestamp_reference``
    contract (``local_standard_time`` or ``utc``) are supported across valid
    datetime storage resolutions (``s``, ``ms``, ``us``, ``ns``) by comparing
    temporal instants at their common finer resolution without downcasting.
    Calendar aggregation and timezone-aware ``DatetimeIndex`` clocks are
    deliberately unsupported because ``SUEWSSimulation`` currently has no
    documented timezone-aware forcing clock contract. Pass only the forcing
    window actually simulated; do not interpolate here.
    """
    expected_suews, expected_ds_all, expected_ds_only = _expected_cloud_bins(
        forcing, tstep=tstep, output_step=output_step
    )
    surface = _extract_cloud_surface(observed)
    cloud = surface.cloud

    native_evidence: dict[str, _NativeGroupEvidence] | None = None
    if native is not None:
        assert surface.dailystate_frame is not None, (
            "Fcld: native comparison requires a DailyState group in observed"
        )
        native_evidence = _extract_native_group_evidence(
            native, tstep=tstep, output_step=output_step, label="Fcld"
        )
        assert "DailyState" in native_evidence, (
            "Fcld: required output group 'DailyState' missing from native"
        )
        assert _same_datetime_instants(native.index, forcing.index), (
            "Fcld: native output clock does not match supplied forcing"
        )
        expected_ds_all = native_evidence["DailyState"].expected_bins
        if output_step is None or output_step == tstep:
            expected_ds_only = pd.DatetimeIndex([], dtype=forcing.index.dtype)
        else:
            expected_ds_only = _dailystate_only_timestamps(
                expected_ds_all, expected_suews.index
            )

    suews_times_actual, extra_times_actual = _resolve_cloud_clock_slices(
        cloud.index,
        expected_suews_index=expected_suews.index,
        expected_ds_only=expected_ds_only,
        has_dailystate=surface.has_dailystate,
    )

    _assert_structural_rows(
        surface,
        expected_suews_index=suews_times_actual,
        extra_times=extra_times_actual,
        expected_ds_bins=expected_ds_all,
        native_evidence=native_evidence,
    )

    observed_suews = cloud.loc[suews_times_actual]
    np.testing.assert_array_equal(
        observed_suews.isna(),
        expected_suews.isna(),
        err_msg="Fcld: missing positions differ from forcing",
    )
    # Native transport is exact; a mean allows only floating-point round-off.
    np.testing.assert_allclose(
        observed_suews.to_numpy(dtype=float),
        expected_suews.to_numpy(dtype=float),
        rtol=0,
        atol=0 if output_step in {None, tstep} else 1e-15,
        equal_nan=True,
        err_msg="Fcld: values differ from forcing",
    )


def _independent_saved_cloud_bins(
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_step: int,
) -> list[tuple[pd.Timestamp, float]]:
    """Compute ordered SUEWS cloud bins directly from forcing without resample/writer."""
    index = forcing.index
    assert isinstance(index, pd.DatetimeIndex), "Fcld: forcing needs a datetime clock"
    assert index.tz is None, "Fcld: timezone-aware clocks are unsupported"
    assert len(index) and index.is_unique, "Fcld: empty or duplicate forcing clock"
    assert not index.hasnans, "Fcld: forcing clock contains missing timestamps (NaT)"
    assert (
        not isinstance(tstep, bool)
        and isinstance(tstep, int)
        and tstep > 0
        and 86400 % tstep == 0
    ), "Fcld: unsupported native timestep"
    assert (
        not isinstance(output_step, bool)
        and isinstance(output_step, int)
        and output_step >= tstep
        and output_step % tstep == 0
    ), "Fcld: output_step must be a fixed integer multiple of tstep"
    assert _same_datetime_instants(
        index,
        pd.date_range(index[0], periods=len(index), freq=f"{tstep}s", unit=index.unit),
    ), "Fcld: forcing must be at the native timestep"
    assert "fcld" in forcing, "Fcld: missing public forcing column fcld"

    origin = index[0].normalize()
    grouped: dict[pd.Timestamp, list[float]] = {}
    for stamp, raw_value in zip(
        index, forcing["fcld"].to_numpy(dtype=float), strict=True
    ):
        delta = stamp - origin
        elapsed_s = int(delta.total_seconds())
        assert delta == pd.Timedelta(seconds=elapsed_s), (
            f"Fcld: sub-second forcing timestamp unsupported for text output: {stamp}"
        )
        label = origin + pd.Timedelta(
            seconds=((elapsed_s - 1) // output_step + 1) * output_step
        )
        bucket = grouped.setdefault(label, [])
        if not math.isnan(raw_value) and raw_value != SUEWS_MISSING_SENTINEL:
            assert math.isfinite(raw_value) and _FCLD_MIN <= raw_value <= _FCLD_MAX, (
                f"Fcld: invalid supplied cloud fraction {raw_value!r}"
            )
            bucket.append(float(raw_value))

    return [
        (label, math.fsum(vals) / len(vals) if vals else float("nan"))
        for label, vals in sorted(grouped.items())
    ]


def _parse_saved_suews_txt_row(
    fields: list[str], col_pos: dict[str, int], row_no: int
) -> tuple[pd.Timestamp, str, float]:
    """Parse and validate one saved SUEWS text row's clock, Dectime, and Fcld."""
    try:
        year = int(fields[col_pos["Year"]])
        doy = int(fields[col_pos["DOY"]])
        hour = int(fields[col_pos["Hour"]])
        minute = int(fields[col_pos["Min"]])
    except ValueError as exc:
        raise AssertionError(
            f"Fcld: saved text row {row_no} has non-integer timestamp field"
        ) from exc

    assert 1 <= doy <= 366 and 0 <= hour <= 23 and 0 <= minute <= 59, (
        f"Fcld: saved text row {row_no} has out-of-range clock "
        f"(Year={year}, DOY={doy}, Hour={hour}, Min={minute})"
    )
    stamp = pd.Timestamp(year=year, month=1, day=1) + pd.Timedelta(
        days=doy - 1, hours=hour, minutes=minute
    )
    assert stamp.year == year and stamp.day_of_year == doy, (
        f"Fcld: saved text row {row_no} has invalid calendar DOY {doy} for year {year}"
    )

    dectime_token = fields[col_pos["Dectime"]]
    assert _SAVED_DECIMAL_RE.match(dectime_token), (
        f"Fcld: saved text row {row_no} has malformed Dectime token {dectime_token!r}"
    )
    expected_dectime = (doy - 1) + (hour * 3600 + minute * 60) / 86400.0
    assert abs(float(dectime_token) - expected_dectime) <= _SAVED_TEXT_ROUNDING_ATOL, (
        f"Fcld: saved text row {row_no} Dectime {dectime_token!r} disagrees with "
        f"clock {stamp} (expected {expected_dectime:.4f})"
    )

    fcld_token = fields[col_pos["Fcld"]]
    assert _SAVED_DECIMAL_RE.match(fcld_token), (
        f"Fcld: saved text row {row_no} has malformed %.4f Fcld token {fcld_token!r}"
    )
    fcld_val = float(fcld_token)
    assert math.isfinite(fcld_val), (
        f"Fcld: saved text row {row_no} has non-finite Fcld value {fcld_token!r}"
    )
    return stamp, fcld_token, fcld_val


def _read_saved_suews_txt_cloud(path: Path) -> list[tuple[pd.Timestamp, str, float]]:
    """Reopen a saved SUEWS text file and parse timestamp and Fcld fields directly."""
    lines = [
        line
        for line in Path(path).read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]
    assert len(lines) >= 2, f"Fcld: saved SUEWS file {path} has no data rows"
    headers = lines[0].split()
    assert len(headers) == len(set(headers)), (
        f"Fcld: saved SUEWS file {path} contains duplicate header columns"
    )
    missing_cols = [col for col in _SAVED_REQUIRED_HEADER_COLUMNS if col not in headers]
    assert not missing_cols, (
        f"Fcld: saved SUEWS file {path} missing required columns: {missing_cols}"
    )
    col_pos = {name: idx for idx, name in enumerate(headers)}
    rows = []
    for row_no, line in enumerate(lines[1:], start=2):
        fields = line.split()
        assert len(fields) == len(headers), (
            f"Fcld: saved SUEWS file {path} row {row_no} has {len(fields)} fields, "
            f"expected {len(headers)}"
        )
        rows.append(_parse_saved_suews_txt_row(fields, col_pos, row_no))
    return rows


def assert_saved_suews_txt_cloud(
    path: Path,
    forcing: pd.DataFrame,
    *,
    tstep: int = 300,
    output_step: int = 3600,
) -> None:
    """Verify saved ``_SUEWS_<min>.txt`` timestamps, Fcld mapping, rounding, and sentinel.

    Expectations are computed directly from public kernel-cadence forcing via
    interval-end arithmetic and ``math.fsum``, independently of both
    ``pandas.resample`` / ``supy._post._resample_output`` and ``supy._save``.
    """
    expected_bins = _independent_saved_cloud_bins(
        forcing, tstep=tstep, output_step=output_step
    )
    saved_rows = _read_saved_suews_txt_cloud(path)

    actual_index = pd.DatetimeIndex([stamp for stamp, _, _ in saved_rows])
    expected_index = pd.DatetimeIndex([stamp for stamp, _ in expected_bins])
    assert actual_index.is_unique, (
        f"Fcld: saved SUEWS file {path} contains duplicate timestamps"
    )
    assert len(saved_rows) == len(expected_bins) and _same_datetime_instants(
        actual_index, expected_index
    ), (
        f"Fcld: saved SUEWS timestamps in {path} do not match forcing bins "
        f"(got {len(saved_rows)} rows {actual_index[0]}..{actual_index[-1]}, "
        f"expected {len(expected_bins)} rows {expected_index[0]}..{expected_index[-1]})"
    )

    for (expected_ts, expected_val), (_, token, actual_val) in zip(
        expected_bins, saved_rows, strict=True
    ):
        if math.isnan(expected_val):
            assert (
                token == _SAVED_MISSING_TOKEN and actual_val == SUEWS_MISSING_SENTINEL
            ), (
                f"Fcld: saved missing bin at {expected_ts} wrote {token!r}, "
                f"expected {_SAVED_MISSING_TOKEN!r}"
            )
        else:
            assert (
                actual_val != SUEWS_MISSING_SENTINEL
                and _FCLD_MIN <= actual_val <= _FCLD_MAX
            ), (
                f"Fcld: saved finite bin at {expected_ts} wrote invalid cloud "
                f"{token!r} (expected {expected_val:.6f})"
            )
            assert abs(actual_val - expected_val) <= _SAVED_TEXT_ROUNDING_ATOL, (
                f"Fcld: saved value {token!r} at {expected_ts} differs from forcing "
                f"expectation {expected_val:.6f} beyond %.4f rounding"
            )


def assert_active_output_finiteness(
    grid_df: pd.DataFrame,
    *,
    expected_timestamps: pd.DatetimeIndex,
    tstep: int = 300,
    storage_heat: int = 1,
    net_radiation: int = 3,
    snow_use: int = 0,
    soil_moisture: int = 0,
    stebbs_enabled: bool = False,
    nlayer: int = 3,
    has_buildings: bool = True,
    forcing: pd.DataFrame,
) -> None:
    """Check one native grid; aggregated DailyState uses a different clock.

    ``forcing`` is the public, kernel-cadence forcing for the simulated window.
    Use ``assert_forcing_cloud`` separately for fixed-frequency aggregates.
    """
    expected_idx = pd.DatetimeIndex(expected_timestamps)
    assert len(expected_idx) > 0, "expected_timestamps must be non-empty"
    assert not expected_idx.hasnans, "expected_timestamps must not contain NaT"
    assert expected_idx.is_unique, "expected_timestamps must be unique"
    assert isinstance(grid_df.index, pd.DatetimeIndex), (
        "Output grid DataFrame must use a DatetimeIndex"
    )
    assert not grid_df.index.hasnans, (
        "Output grid DataFrame contains missing timestamps (NaT)"
    )
    assert grid_df.index.is_unique, (
        "Output grid DataFrame contains duplicate timestamps"
    )
    assert len(grid_df) == len(expected_idx), (
        f"Output row count mismatch: got {len(grid_df)}, expected {len(expected_idx)}"
    )
    assert _same_datetime_instants(grid_df.index, expected_idx), (
        "Output timestamps do not match forcing-derived expected timestamps "
        f"(got {grid_df.index[0]}..{grid_df.index[-1]}, "
        f"expected {expected_idx[0]}..{expected_idx[-1]})"
    )

    suews = _extract_required_group(grid_df, "SUEWS")
    assert "Fcld" in suews, "SUEWS: missing required output column Fcld"
    assert_forcing_cloud(suews["Fcld"], forcing, tstep=tstep)
    suews_absent: tuple[str, ...] = ()
    if int(net_radiation) == 0 and int(snow_use) == 0:
        suews_absent += SUEWS_ABSENT_UNDER_OBSERVED_QN
    if int(soil_moisture) == 1:
        suews_absent += SUEWS_ABSENT_UNDER_OBSERVED_SMD
    # Cloud has already been checked cell by cell, including missing positions.
    _assert_group_finiteness(
        suews.drop(columns="Fcld"),
        group="SUEWS",
        expected_columns=tuple(
            c for c in expected_group_columns("SUEWS") if c != "Fcld"
        ),
        expected_all_nan=suews_absent,
    )
    _assert_beers_finiteness(grid_df, has_buildings=has_buildings)
    _assert_rsl_finiteness(grid_df)
    _assert_group_finiteness(
        _extract_required_group(grid_df, "debug"),
        group="debug",
        expected_columns=expected_group_columns("debug"),
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
    ds_absent = DAILYSTATE_ABSENT_UNDER_EHC if int(storage_heat) in {0, 5} else ()
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
    if stebbs_enabled:
        _assert_group_finiteness(
            _extract_required_group(grid_df, "STEBBS"),
            group="STEBBS",
            expected_columns=expected_group_columns("STEBBS"),
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
    soil_moisture: int = 0,
    stebbs_enabled: bool = False,
    nlayer: int = 3,
    has_buildings: bool = True,
    forcing: pd.DataFrame | dict[int, pd.DataFrame],
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

    if isinstance(forcing, dict):
        assert set(forcing) == set(grids), "Fcld: forcing grid IDs mismatch"
    for grid_id in grids:
        assert_active_output_finiteness(
            df_output.xs(grid_id, level=0),
            expected_timestamps=expected_idx,
            tstep=tstep,
            storage_heat=storage_heat,
            net_radiation=net_radiation,
            snow_use=snow_use,
            soil_moisture=soil_moisture,
            stebbs_enabled=stebbs_enabled,
            nlayer=nlayer,
            has_buildings=has_buildings,
            forcing=forcing[grid_id] if isinstance(forcing, dict) else forcing,
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
        assert _same_datetime_instants(col_series.index, expected_index), (
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
    assert _same_datetime_instants(suews.index, debug.index), (
        "SUEWS and debug timestamps are not identically aligned or contain NaT"
    )
    if expected_timestamps is not None:
        assert _same_datetime_instants(
            suews.index, pd.DatetimeIndex(expected_timestamps)
        ), "Water-balance timestamps do not match expected forcing timestamps"
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
