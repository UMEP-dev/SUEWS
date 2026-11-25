"""Exercise the forcing-cloud contract on real public runs, resampler boundaries, and damage."""

from functools import partial
import math

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import (
    ALWAYS_ACTIVE_NON_SUEWS_GROUPS,
    SUEWS_MISSING_SENTINEL,
    assert_active_output_finiteness,
    assert_dailystate_group_occurrence,
    assert_forcing_cloud,
    assert_output_group_occurrence,
    assert_simulation_output_finiteness,
    expected_group_columns,
    water_balance_residual_mm,
)
import numpy as np
import pandas as pd
import pytest
import test_option_sweep as sweep

from supy import SUEWSOutput, SUEWSSimulation
from supy.data_model.core.model import (
    ForcingTimestampReference,
    SnowUse,
    StorageHeatMethod,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.core,
    pytest.mark.contract("C-CLOCK", "C-FORCING", "C-OUTPUT"),
]

_STORAGE_UNIT_CASES: tuple[tuple[str, str | None], ...] = (
    ("ns", "datetime"),
    ("us", "datetime"),
    ("ms", "time"),
    ("s", None),
)


def _run_forcing_window(
    base_config,
    forcing_slice: pd.DataFrame,
    cloud_values,
    *,
    timestamp_reference: ForcingTimestampReference | None = None,
    site_timezone: float | None = None,
    tstep: int | None = None,
):
    """Run one validated forcing window through normal public ``SUEWSSimulation`` APIs."""
    forcing = forcing_slice.copy()
    forcing["fcld"] = cloud_values
    config = base_config.model_copy(deep=True)
    if timestamp_reference is not None:
        config.model.control.forcing.timestamp_reference = timestamp_reference
    if site_timezone is not None:
        config.sites[0].properties.timezone.value = site_timezone
    if tstep is not None:
        config.model.control.tstep = tstep
    sim = SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(forcing)
    output = sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    return sim.forcing.df.copy(), output


@pytest.fixture(scope="module", params=["missing", "finite", "mixed"])
def cloud_run(request, sample_config_loaded, sample_data_loaded):
    """Mode 3 allows missing cloud observations; use ordinary validated forcing."""
    _, sample = sample_data_loaded
    forcing = sample.iloc[:TIMESTEPS_PER_DAY].copy()
    cloud = np.linspace(0.1, 0.9, len(forcing))
    if request.param == "missing":
        cloud[:] = SUEWS_MISSING_SENTINEL
    elif request.param == "mixed":
        cloud[::3] = SUEWS_MISSING_SENTINEL
        cloud[12:24] = SUEWS_MISSING_SENTINEL
    return _run_forcing_window(sample_config_loaded, forcing, cloud)


@pytest.fixture(scope="module")
def storage_unit_runs(sample_config_loaded, sample_data_loaded):
    """Genuine public runs for identical midnight-crossing forcing in ns, us, ms, s."""
    _, sample = sample_data_loaded
    midnight_slice = sample.loc["2012-01-01 23:35":"2012-01-02 01:30"]
    # Includes zero cloud (0.0), finite values, NaN, -999 sentinel, partial
    # first/last hours, an all-missing middle hour, and an EOD DailyState row.
    cloud_values = np.array([
        0.0,
        0.17,
        np.nan,
        0.43,
        SUEWS_MISSING_SENTINEL,
        1.0,
        *([SUEWS_MISSING_SENTINEL] * 12),
        0.25,
        SUEWS_MISSING_SENTINEL,
        0.75,
        0.0,
        1.0,
        0.5,
    ])

    runs = {}
    for unit, index_name in _STORAGE_UNIT_CASES:
        forcing = midnight_slice.copy()
        forcing["fcld"] = cloud_values
        forcing.index = forcing.index.as_unit(unit).rename(index_name)
        sim = SUEWSSimulation()
        sim.update_config(
            sample_config_loaded.model_copy(deep=True), auto_load_forcing=False
        )
        sim.update_forcing(forcing)
        output = sim.run(
            start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
        )
        runs[unit] = (sim.forcing.df.copy(), output, sim.state_init.copy())
    return runs


def test_native_cloud_entrypoints_and_corruptions(cloud_run):
    """Both native finiteness entry points pass intact cloud and reject all corruptions."""
    forcing, output = cloud_run
    frame = output.df
    check_sim = partial(
        assert_simulation_output_finiteness,
        expected_grids=(1,),
        expected_timestamps=forcing.index,
        forcing=forcing,
    )
    check_sim(frame)
    assert_active_output_finiteness(
        frame.loc[1], expected_timestamps=forcing.index, forcing=forcing
    )

    finite = np.flatnonzero(forcing["fcld"].to_numpy() != SUEWS_MISSING_SENTINEL)
    missing = np.flatnonzero(forcing["fcld"].to_numpy() == SUEWS_MISSING_SENTINEL)
    col = frame.columns.get_loc(("SUEWS", "Fcld"))
    variants: list[tuple[str, pd.DataFrame]] = []
    if len(finite):
        for label, value in (
            ("finite_to_nan", np.nan),
            ("finite_to_inf", np.inf),
            ("finite_to_sentinel", SUEWS_MISSING_SENTINEL),
        ):
            bad = frame.copy()
            bad.iloc[finite[0], col] = value
            variants.append((label, bad))
        erased = frame.copy()
        erased["SUEWS", "Fcld"] = np.nan
        variants.append(("all_erased", erased))
        swapped = frame.copy()
        swapped.iloc[finite[:2], col] = frame.iloc[finite[:2][::-1], col].to_numpy()
        variants.append(("swapped_finite_pair", swapped))
    if len(missing):
        filled = frame.copy()
        filled.iloc[missing[0], col] = 0.5
        variants.append(("missing_filled", filled))
    variants.append(("dropped_fcld", frame.drop(columns=[("SUEWS", "Fcld")])))

    for label, bad in variants:
        with pytest.raises(AssertionError, match="Fcld"):
            check_sim(bad)
        with pytest.raises(AssertionError, match="Fcld"):
            assert_active_output_finiteness(
                bad.loc[1], expected_timestamps=forcing.index, forcing=forcing
            )
        assert label


def test_cloud_grid_alignment(cloud_run):
    """Multi-grid dict forcing checks each grid ID and per-grid cloud values."""
    forcing, output = cloud_run
    frame = pd.concat(
        {7: output.df.loc[1], 1: output.df.loc[1]}, names=["grid", "datetime"]
    )
    forcing_by_grid = {1: forcing, 7: forcing.copy()}
    check = partial(
        assert_simulation_output_finiteness,
        expected_grids=(1, 7),
        expected_timestamps=forcing.index,
        forcing=forcing_by_grid,
    )
    check(frame)

    with pytest.raises(AssertionError, match="Fcld"):
        assert_simulation_output_finiteness(
            frame,
            expected_grids=(1, 7),
            expected_timestamps=forcing.index,
            forcing={1: forcing},
        )

    forcing_by_grid[7]["fcld"] = 0.99
    with pytest.raises(AssertionError, match="Fcld"):
        check(frame)


def test_option_sweep_cloud_contract(monkeypatch, cloud_run, sample_config_loaded):
    """The ordinary sweep entry checks real output before and after Fcld damage."""
    forcing, output = cloud_run
    state = sample_config_loaded.to_df_state()
    value = sample_config_loaded.model.physics.net_radiation
    checked = output.df.copy()
    monkeypatch.setattr(
        sweep, "run_simulation", lambda *args, **kwargs: (checked, None)
    )
    run = partial(
        sweep.test_option_value_runs_finite_and_conserves_water,
        "net_radiation",
        value,
        sample_config_loaded,
        (state, forcing),
    )
    run()
    checked.iloc[0, checked.columns.get_loc(("SUEWS", "Fcld"))] = 0.99
    with pytest.raises(AssertionError, match="Fcld"):
        run()


@pytest.mark.parametrize("unit", ["ns", "us", "ms", "s"])
def test_native_storage_unit_clocks_and_cross_unit_pairs(storage_unit_runs, unit):
    """Native cloud, zero/NaN/-999 round-trip, and all 16 cross-unit pairs pass."""
    _, ns_output, _ = storage_unit_runs["ns"]
    forcing, output, state_init = storage_unit_runs[unit]
    native_combined = output.df.loc[1]

    assert forcing.index.unit == unit
    pd.testing.assert_frame_equal(output.df, ns_output.df)
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=forcing.index,
        forcing=forcing,
        tstep=300,
    )
    assert_active_output_finiteness(
        native_combined,
        expected_timestamps=forcing.index,
        forcing=forcing,
        tstep=300,
    )
    assert (
        water_balance_residual_mm(
            output.df, state_init, expected_timestamps=forcing.index
        )
        < 1e-4
    )
    assert_forcing_cloud(native_combined, forcing, tstep=300)
    assert_forcing_cloud(native_combined["SUEWS"], forcing, tstep=300)
    assert_forcing_cloud(native_combined["SUEWS", "Fcld"], forcing, tstep=300)

    for obs_unit in ("s", "ms", "us", "ns"):
        obs = native_combined.copy()
        obs.index = obs.index.as_unit(obs_unit)
        assert_forcing_cloud(obs, forcing, tstep=300)
        assert_forcing_cloud(obs["SUEWS", "Fcld"], forcing, tstep=300)


@pytest.mark.parametrize("unit", ["s", "ms", "us", "ns"])
def test_native_cloud_rejects_value_cadence_duplicate_and_shift_controls(
    storage_unit_runs, unit
):
    """Zero/missing damage, irregular/duplicate/shifted/reversed/extra clocks fail."""
    forcing, output, _ = storage_unit_runs[unit]
    native_cloud = output.df.loc[1, ("SUEWS", "Fcld")]

    # Zero-value (index 0 == 0.0) and missing-value (index 2 == NaN) controls.
    for bad_zero in (np.nan, 0.01, SUEWS_MISSING_SENTINEL):
        mutated = native_cloud.copy()
        mutated.iloc[0] = bad_zero
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(mutated, forcing, tstep=300)
    for bad_missing in (0.0, 0.5, SUEWS_MISSING_SENTINEL):
        mutated = native_cloud.copy()
        mutated.iloc[2] = bad_missing
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(mutated, forcing, tstep=300)

    # Irregular cadence (subsampled and +1-tick jitter) and unsupported tstep.
    with pytest.raises(AssertionError, match="native timestep"):
        assert_forcing_cloud(native_cloud, forcing.iloc[::2], tstep=300)
    jittered = forcing.copy()
    jittered_idx = jittered.index.to_list()
    jittered_idx[1] += pd.Timedelta(1, unit)
    jittered.index = pd.DatetimeIndex(jittered_idx).as_unit(unit)
    with pytest.raises(AssertionError, match="native timestep"):
        assert_forcing_cloud(native_cloud, jittered, tstep=300)
    with pytest.raises(AssertionError, match="unsupported native timestep"):
        assert_forcing_cloud(native_cloud, forcing, tstep=700)

    # Duplicates, shifts, reversals, and extra structural rows at native cadence.
    with pytest.raises(AssertionError, match="empty or duplicate forcing clock"):
        assert_forcing_cloud(
            native_cloud, pd.concat([forcing, forcing.iloc[-1:]]), tstep=300
        )
    with pytest.raises(AssertionError, match="duplicate timestamps in output clock"):
        assert_forcing_cloud(
            pd.concat([native_cloud, native_cloud.iloc[-1:]]), forcing, tstep=300
        )
    shifted_300 = forcing.copy()
    shifted_300.index = (
        shifted_300.index + pd.Timedelta(seconds=300).as_unit(unit)
    ).as_unit(unit)
    with pytest.raises(AssertionError, match="missing required measurement bins"):
        assert_forcing_cloud(native_cloud, shifted_300, tstep=300)
    with pytest.raises(AssertionError, match="native timestep"):
        assert_forcing_cloud(native_cloud, forcing.iloc[::-1], tstep=300)
    with pytest.raises(AssertionError, match="does not match supplied forcing"):
        assert_forcing_cloud(native_cloud.iloc[::-1], forcing, tstep=300)
    native_with_extra = pd.concat([
        pd.Series([np.nan], index=pd.DatetimeIndex(["2012-01-01 23:00:00"])),
        native_cloud,
    ])
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(native_with_extra, forcing, tstep=300)


@pytest.mark.parametrize("unit", ["s", "ms", "us", "ns"])
def test_native_cloud_rejects_nat_aware_and_invalid_forcing_controls(
    storage_unit_runs, unit
):
    """NaT clocks, timezone-aware clocks, and out-of-range forcing cloud fail."""
    forcing, output, _ = storage_unit_runs[unit]
    native_cloud = output.df.loc[1, ("SUEWS", "Fcld")]

    nat_forcing = forcing.copy()
    nat_f_idx = nat_forcing.index.to_list()
    nat_f_idx[0] = pd.NaT
    nat_forcing.index = pd.DatetimeIndex(nat_f_idx).as_unit(unit)
    with pytest.raises(AssertionError, match="NaT"):
        assert_forcing_cloud(native_cloud, nat_forcing, tstep=300)
    nat_cloud = native_cloud.copy()
    nat_c_idx = nat_cloud.index.to_list()
    nat_c_idx[-1] = pd.NaT
    nat_cloud.index = pd.DatetimeIndex(nat_c_idx).as_unit(unit)
    with pytest.raises(AssertionError, match="NaT"):
        assert_forcing_cloud(nat_cloud, forcing, tstep=300)
    for tz_name in ("UTC", "Asia/Kolkata"):
        with pytest.raises(AssertionError, match="timezone-aware"):
            assert_forcing_cloud(native_cloud, forcing.tz_localize(tz_name), tstep=300)
    with pytest.raises(AssertionError, match="timezone-aware"):
        assert_forcing_cloud(
            native_cloud.set_axis(native_cloud.index.as_unit(unit)).tz_localize("UTC"),
            forcing,
            tstep=300,
        )
    for invalid_fcld in (-0.1, 1.2, np.inf):
        bad_forcing = forcing.copy()
        bad_forcing.iloc[0, bad_forcing.columns.get_loc("fcld")] = invalid_fcld
        with pytest.raises(AssertionError, match="invalid supplied cloud fraction"):
            assert_forcing_cloud(native_cloud, bad_forcing, tstep=300)


@pytest.mark.parametrize("fine_unit", ["ms", "us", "ns"])
def test_native_subsecond_shifts_never_masked_by_coarser_unit(
    storage_unit_runs, fine_unit
):
    """Comparing s against ms/us/ns never downcasts away a native sub-second shift."""
    forcing_s, output_s, _ = storage_unit_runs["s"]
    forcing_fine, _, _ = storage_unit_runs[fine_unit]
    native_cloud = output_s.df.loc[1, ("SUEWS", "Fcld")]
    tick = pd.Timedelta(1, fine_unit)

    shifted_forcing = forcing_fine.copy()
    shifted_forcing.index = (shifted_forcing.index + tick).as_unit(fine_unit)
    with pytest.raises(AssertionError, match="missing required measurement bins"):
        assert_forcing_cloud(native_cloud, shifted_forcing, tstep=300)

    shifted_native = native_cloud.copy()
    shifted_native.index = (shifted_native.index.as_unit(fine_unit) + tick).as_unit(
        fine_unit
    )
    with pytest.raises(AssertionError, match="missing required measurement bins"):
        assert_forcing_cloud(shifted_native, forcing_s, tstep=300)


def _right_closed_interval_containing(
    instant: pd.Timestamp,
    *,
    origin: pd.Timestamp,
    step: pd.Timedelta,
) -> tuple[pd.Timestamp, pd.Timestamp]:
    """Locate the unique right-closed interval ``(left, right]`` by stepping."""
    left = origin - step
    right = origin
    while instant > right:
        left = right
        right = left + step
    while instant <= left:
        right = left
        left = right - step
    assert left < instant <= right
    return left, right


def _independent_dailystate_bins(
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_step: int,
    native: pd.DataFrame | None = None,
) -> list[pd.Timestamp]:
    """Derive all right-labelled DailyState bins by interval walking."""
    if native is not None:
        ds_native = native["DailyState"]
        ds_arr = ds_native.to_numpy(dtype=float)
        valid_mask = (
            np.isfinite(ds_arr) & ~np.equal(ds_arr, SUEWS_MISSING_SENTINEL)
        ).any(axis=1)
        eod_stamps = list(ds_native.index[valid_mask])
        origin_index = native.index
    else:
        native_step = pd.Timedelta(seconds=tstep)
        eod_stamps = [
            stamp
            for stamp in forcing.index
            if (stamp + native_step).normalize() != stamp.normalize()
        ]
        origin_index = forcing.index
    if not eod_stamps:
        return []
    if output_step == tstep:
        return sorted(set(eod_stamps))

    origin = (origin_index.min() - pd.Timedelta(1, origin_index.unit)).normalize()
    step = pd.Timedelta(seconds=output_step)
    ds_all: set[pd.Timestamp] = set()
    for stamp in eod_stamps:
        _, right = _right_closed_interval_containing(stamp, origin=origin, step=step)
        ds_all.add(right)
    return sorted(ds_all)


def _independent_cloud_expectations(
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_step: int,
    native: pd.DataFrame | None = None,
) -> tuple[dict[pd.Timestamp, float], list[pd.Timestamp], list[pd.Timestamp]]:
    """Compute SUEWS bins and all/exclusive DailyState bins by interval walking."""
    step = pd.Timedelta(seconds=output_step)
    origin = (forcing.index.min() - pd.Timedelta(1, forcing.index.unit)).normalize()

    grouped: dict[pd.Timestamp, list[float]] = {}
    for stamp, raw_value in zip(forcing.index, forcing["fcld"].to_numpy(dtype=float)):
        _, right = _right_closed_interval_containing(stamp, origin=origin, step=step)
        bucket = grouped.setdefault(right, [])
        if not np.isnan(raw_value) and raw_value != SUEWS_MISSING_SENTINEL:
            bucket.append(float(raw_value))

    expected_bins = {
        label: (math.fsum(vals) / len(vals) if vals else float("nan"))
        for label, vals in sorted(grouped.items())
    }
    ds_labels = _independent_dailystate_bins(
        forcing, tstep=tstep, output_step=output_step, native=native
    )
    ds_only = [stamp for stamp in ds_labels if stamp not in expected_bins]
    return expected_bins, ds_labels, ds_only


def _boundary_case_spec(sample: pd.DataFrame, case_name: str):
    """Return ``(slice, cloud_values, tstep, kwargs)`` for one named boundary window."""
    midnight_slice = sample.loc["2012-01-01 23:35":"2012-01-02 01:30"]
    midnight_mixed = np.array([
        0.0,
        0.17,
        SUEWS_MISSING_SENTINEL,
        0.43,
        0.86,
        1.0,  # partial first hour (23:35..00:00)
        *([SUEWS_MISSING_SENTINEL] * 12),  # full all-missing middle hour (00:05..01:00)
        0.25,
        SUEWS_MISSING_SENTINEL,
        0.75,
        0.0,
        1.0,
        0.5,  # partial last hour (01:05..01:30)
    ])
    leap_midnight_slice = sample.loc["2012-02-29 23:35":"2012-03-01 01:30"]
    leap_full_day_slice = sample.loc["2012-02-29 00:05":"2012-03-01 00:00"]
    leap_full_day_mixed = np.resize(
        [0.0, np.nan, 0.17, SUEWS_MISSING_SENTINEL, 0.63, 1.0],
        len(leap_full_day_slice),
    )
    partial_no_eod_slice = sample.loc["2012-01-01 00:20":"2012-01-01 02:40"]
    partial_no_eod_mixed = np.full(
        len(partial_no_eod_slice), SUEWS_MISSING_SENTINEL, dtype=float
    )
    partial_no_eod_mixed[:8] = [0.0, 0.2, -999.0, 0.6, 1.0, -999.0, 0.4, 0.8]
    partial_no_eod_mixed[-8:] = [0.1, -999.0, 0.9, 0.3, -999.0, 0.7, 0.0, 1.0]
    utc_kwargs = {
        "timestamp_reference": ForcingTimestampReference.UTC,
        "site_timezone": 5.5,
    }
    specs = {
        "midnight_missing": (midnight_slice, SUEWS_MISSING_SENTINEL, 300, {}),
        "midnight_mixed": (midnight_slice, midnight_mixed, 300, {}),
        "leap_midnight_missing": (leap_midnight_slice, SUEWS_MISSING_SENTINEL, 300, {}),
        "leap_midnight_mixed": (leap_midnight_slice, midnight_mixed, 300, {}),
        "leap_full_day_mixed": (leap_full_day_slice, leap_full_day_mixed, 300, {}),
        "leap_two_day_mixed": (
            sample.loc["2012-02-28 00:05":"2012-03-01 00:00"],
            np.concatenate([
                np.full(TIMESTEPS_PER_DAY, SUEWS_MISSING_SENTINEL, dtype=float),
                leap_full_day_mixed,
            ]),
            300,
            {},
        ),
        "partial_no_eod_mixed": (partial_no_eod_slice, partial_no_eod_mixed, 300, {}),
        "partial_no_eod_missing": (
            partial_no_eod_slice,
            SUEWS_MISSING_SENTINEL,
            300,
            {},
        ),
        "hourly_native_mixed": (
            sample.iloc[11:TIMESTEPS_PER_DAY:12],
            np.resize([0.1, SUEWS_MISSING_SENTINEL, 0.8], 24),
            3600,
            {"tstep": 3600},
        ),
        "utc_midnight_mixed": (midnight_slice, midnight_mixed, 300, utc_kwargs),
        "utc_full_day_mixed": (
            sample.iloc[:TIMESTEPS_PER_DAY],
            np.resize(
                [0.0, 0.35, SUEWS_MISSING_SENTINEL, 0.75, 1.0], TIMESTEPS_PER_DAY
            ),
            300,
            utc_kwargs,
        ),
    }
    return specs[case_name]


@pytest.fixture(scope="module")
def boundary_run(sample_config_loaded, sample_data_loaded):
    """Lazy per-case cache of real boundary runs (avoids eager 11-run startup)."""
    _, sample = sample_data_loaded
    cache: dict[str, tuple[pd.DataFrame, object, int]] = {}

    def _get(case_name: str):
        if case_name not in cache:
            forcing_slice, cloud_values, tstep, kwargs = _boundary_case_spec(
                sample, case_name
            )
            forcing_used, output = _run_forcing_window(
                sample_config_loaded, forcing_slice, cloud_values, **kwargs
            )
            cache[case_name] = (forcing_used, output, tstep)
        return cache[case_name]

    return _get


def _series_corruptions(
    series: pd.Series,
    *,
    stray_ds_stamp: pd.Timestamp,
    finite_bin: pd.Timestamp,
    all_missing_bin: pd.Timestamp,
) -> list[tuple[str, pd.Series]]:
    """Build named clock, missingness, and value corruptions for a resampled Series."""
    unit = series.index.unit
    stray_idx = pd.DatetimeIndex([stray_ds_stamp]).as_unit(unit)
    variants: list[tuple[str, pd.Series]] = [
        ("drop_finite_bin", series.drop(index=finite_bin)),
        ("drop_all_missing_bin", series.drop(index=all_missing_bin)),
        ("reversed_index", series.iloc[::-1]),
        ("duplicate_last", pd.concat([series, series.iloc[-1:]])),
        (
            "stray_leading_nan",
            pd.concat([
                pd.Series(
                    [np.nan],
                    index=pd.DatetimeIndex(["2012-01-01 22:00:00"]).as_unit(unit),
                ),
                series,
            ]),
        ),
        (
            "stray_trailing_nan",
            pd.concat([
                series,
                pd.Series(
                    [np.nan],
                    index=pd.DatetimeIndex(["2012-01-02 03:00:00"]).as_unit(unit),
                ),
            ]),
        ),
        (
            "stray_legacy_ds_nan",
            pd.concat([pd.Series([np.nan], index=stray_idx), series]),
        ),
    ]
    for label, value in (
        ("nan", np.nan),
        ("zero", 0.0),
        ("sentinel", SUEWS_MISSING_SENTINEL),
        ("inf", np.inf),
    ):
        bad_finite = series.copy()
        bad_finite.loc[finite_bin] = value
        variants.append((f"finite_to_{label}", bad_finite))
    bad_pert = series.copy()
    bad_pert.loc[finite_bin] += 1e-6
    variants.append(("finite_plus_1e-6", bad_pert))
    for label, value in (
        ("zero", 0.0),
        ("half", 0.5),
        ("sentinel", SUEWS_MISSING_SENTINEL),
        ("inf", np.inf),
    ):
        filled_missing = series.copy()
        filled_missing.loc[all_missing_bin] = value
        variants.append((f"all_missing_filled_{label}", filled_missing))
        extra_finite = pd.concat([pd.Series([value], index=stray_idx), series])
        variants.append((f"stray_legacy_ds_filled_{label}", extra_finite))
    shifted = pd.concat([
        pd.Series([series.loc[finite_bin]], index=stray_idx),
        series.copy(),
    ])
    shifted.loc[finite_bin] = np.nan
    variants.append(("shifted_finite_to_legacy_ds", shifted))
    return variants


def _nonvalid_structural_corruptions(
    orphan_ds: pd.DataFrame,
    blank_missing_suews_bin: pd.DataFrame,
    *,
    ds_bin: pd.Timestamp,
    all_missing_bin: pd.Timestamp,
) -> list[tuple[str, pd.DataFrame]]:
    """Build Inf/-Inf/-999-only DailyState-bin and all-missing-bin corruptions."""
    variants: list[tuple[str, pd.DataFrame]] = []
    for tag, bad_val in (
        ("pos_inf", np.inf),
        ("neg_inf", -np.inf),
        ("sentinel", SUEWS_MISSING_SENTINEL),
    ):
        ds_only_bad = orphan_ds.copy()
        ds_only_bad.loc[ds_bin, ("DailyState", "LAI_Grass")] = bad_val
        suews_bin_only_bad = blank_missing_suews_bin.copy()
        suews_bin_only_bad.loc[all_missing_bin, ("SUEWS", "QN")] = bad_val
        variants.append((f"ds_bin_only_{tag}", ds_only_bad))
        variants.append((f"suews_missing_bin_only_{tag}", suews_bin_only_bad))
    return variants


def _flat_suews_corruptions(
    suews_flat: pd.DataFrame,
    *,
    stray_ds_stamp: pd.Timestamp,
    all_missing_bin: pd.Timestamp,
) -> list[tuple[str, pd.DataFrame]]:
    """Build named corruptions for flat SUEWS frames including Inf/-999-only bins."""
    stray_row = pd.DataFrame(
        np.nan,
        index=pd.DatetimeIndex([stray_ds_stamp]).as_unit(suews_flat.index.unit),
        columns=suews_flat.columns,
    )
    stray_row.loc[:, "QN"] = 10.0
    flat_suews_on_stray_ds = pd.concat([stray_row, suews_flat])
    flat_blank_bin = suews_flat.copy()
    flat_blank_bin.loc[all_missing_bin, :] = np.nan
    variants: list[tuple[str, pd.DataFrame]] = [
        ("flat_suews_on_stray_ds", flat_suews_on_stray_ds),
        ("flat_blank_bin", flat_blank_bin),
    ]
    for tag, bad_val in (
        ("pos_inf", np.inf),
        ("neg_inf", -np.inf),
        ("sentinel", SUEWS_MISSING_SENTINEL),
    ):
        flat_only_bad = flat_blank_bin.copy()
        flat_only_bad.loc[all_missing_bin, "QN"] = bad_val
        variants.append((f"flat_suews_bin_only_{tag}", flat_only_bad))
    return variants


def _combined_frame_corruptions(
    combined: pd.DataFrame,
    *,
    ds_bin: pd.Timestamp,
    stray_ds_stamp: pd.Timestamp,
    all_missing_bin: pd.Timestamp,
) -> list[tuple[str, pd.DataFrame]]:
    """Build named combined-frame corruptions mixing DailyState and SUEWS bins."""
    unit = combined.index.unit
    stray_ds_row = pd.DataFrame(
        np.nan,
        index=pd.DatetimeIndex([stray_ds_stamp]).as_unit(unit),
        columns=combined.columns,
    )
    suews_on_stray_ds = stray_ds_row.copy()
    suews_on_stray_ds.loc[:, ("SUEWS", "QN")] = 10.0
    beers_on_stray_ds = stray_ds_row.copy()
    beers_on_stray_ds.loc[:, ("BEERS", "Zenith")] = 45.0
    orphan_ds = combined.copy()
    orphan_ds.loc[ds_bin, "DailyState"] = np.nan
    spurious_ds_on_missing = combined.copy()
    for col in combined["DailyState"].columns:
        spurious_ds_on_missing.at[all_missing_bin, ("DailyState", col)] = combined.at[
            ds_bin, ("DailyState", col)
        ]
    stray_row = pd.DataFrame(
        np.nan,
        index=pd.DatetimeIndex(["2012-01-01 22:00:00"]).as_unit(unit),
        columns=combined.columns,
    )
    stray_row.loc[:, ("BEERS", "Zenith")] = 45.0
    stray_other_group = pd.concat([stray_row, combined])
    blank_missing_suews_bin = combined.copy()
    blank_missing_suews_bin.loc[all_missing_bin, "SUEWS"] = np.nan
    return [
        ("stray_legacy_ds_empty", pd.concat([stray_ds_row, combined])),
        ("suews_populated_on_stray_ds", pd.concat([suews_on_stray_ds, combined])),
        ("beers_populated_on_stray_ds", pd.concat([beers_on_stray_ds, combined])),
        ("orphan_empty_ds", orphan_ds),
        ("spurious_ds_on_non_ds_bin", spurious_ds_on_missing),
        ("stray_other_group_row", stray_other_group),
        ("dropped_ds_row", combined.drop(index=ds_bin)),
        ("blank_all_missing_suews_bin", blank_missing_suews_bin),
        *_nonvalid_structural_corruptions(
            orphan_ds,
            blank_missing_suews_bin,
            ds_bin=ds_bin,
            all_missing_bin=all_missing_bin,
        ),
    ]


@pytest.mark.parametrize("output_step", [300, 600, 1800, 3600, 86400])
def test_public_aggregation_cloud(cloud_run, output_step):
    """Fixed-frequency aggregation matches forcing across Series, SUEWS, and combined."""
    forcing, output = cloud_run
    resampled = output.resample(freq=f"{output_step}s")
    combined = resampled.df.loc[1]
    suews_group = resampled.get_group("SUEWS").xs(1, level="grid")
    observed = combined["SUEWS", "Fcld"]

    assert_forcing_cloud(observed, forcing, output_step=output_step)
    assert_forcing_cloud(suews_group, forcing, output_step=output_step)
    assert_forcing_cloud(combined, forcing, output_step=output_step)

    for position in (0, -1):
        bad = observed.copy()
        bad.iloc[position] = 0.99
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(bad, forcing, output_step=output_step)


def _row_has_valid_scalar(frame: pd.DataFrame, stamp: pd.Timestamp) -> bool:
    """Return True if row ``stamp`` holds at least one finite non--999 value."""
    vals = frame.loc[stamp].to_numpy(dtype=float)
    return bool((np.isfinite(vals) & ~np.equal(vals, SUEWS_MISSING_SENTINEL)).any())


def _independent_right_labelled_bins(
    native: pd.DataFrame,
    group: str,
    *,
    output_step: int,
) -> list[pd.Timestamp]:
    """Derive expected right-labelled bins for ``group`` by interval walking."""
    grp_native = native[group]
    arr = grp_native.to_numpy(dtype=float)
    valid_mask = (np.isfinite(arr) & ~np.equal(arr, SUEWS_MISSING_SENTINEL)).any(axis=1)
    populated = grp_native.index[valid_mask]
    if len(populated) == 0:
        return []
    origin = (native.index.min() - pd.Timedelta(1, native.index.unit)).normalize()
    step = pd.Timedelta(seconds=output_step)
    bins: set[pd.Timestamp] = set()
    for stamp in populated:
        _, right = _right_closed_interval_containing(stamp, origin=origin, step=step)
        bins.add(right)
    return sorted(bins)


def _assert_oracle_non_dailystate_groups(
    combined: pd.DataFrame,
    *,
    bin_index: pd.DatetimeIndex,
    ds_only_index: pd.DatetimeIndex,
    output_step: int,
    label: str,
    native: pd.DataFrame | None,
) -> None:
    """Independently check active/inactive non-DailyState groups and columns."""
    obs_groups = set(combined.columns.get_level_values(0).unique())
    if native is not None:
        nat_groups = tuple(native.columns.get_level_values(0).unique())
        assert not (obs_groups - set(nat_groups)), (
            f"{label}@{output_step}s: unexpected groups absent from native"
        )
        for grp in nat_groups:
            if grp == "DailyState":
                ds_nat = native["DailyState"]
                ds_arr = ds_nat.to_numpy(dtype=float)
                if (
                    np.isfinite(ds_arr) & ~np.equal(ds_arr, SUEWS_MISSING_SENTINEL)
                ).any():
                    missing_ds = set(ds_nat.columns) - set(
                        combined["DailyState"].columns
                    )
                    assert not missing_ds, (
                        f"{label}@{output_step}s: DailyState missing columns {missing_ds}"
                    )
                continue
            expected_grp_bins = _independent_right_labelled_bins(
                native, str(grp), output_step=output_step
            )
            if expected_grp_bins:
                assert grp in obs_groups, (
                    f"{label}@{output_step}s: active group {grp!r} missing"
                )
                missing_cols = set(native[grp].columns) - set(combined[grp].columns)
                assert not missing_cols, (
                    f"{label}@{output_step}s: active group {grp!r} missing columns "
                    f"{sorted(missing_cols)}"
                )
                expected_set = set(expected_grp_bins)
                for stamp in expected_grp_bins:
                    assert _row_has_valid_scalar(combined[grp], pd.Timestamp(stamp)), (
                        f"{label}@{output_step}s {stamp}: expected populated {grp}"
                    )
                for stamp in combined.index:
                    if stamp not in expected_set:
                        assert combined[grp].loc[pd.Timestamp(stamp)].isna().all(), (
                            f"{label}@{output_step}s {stamp}: unexpected {grp} values"
                        )
            elif grp in obs_groups:
                assert combined[grp].isna().all().all(), (
                    f"{label}@{output_step}s: inactive group {grp!r} unexpectedly populated"
                )
        return

    for always_grp in ALWAYS_ACTIVE_NON_SUEWS_GROUPS:
        assert always_grp in obs_groups, (
            f"{label}@{output_step}s: active group {always_grp!r} missing"
        )
        missing_cols = set(expected_group_columns(always_grp)) - set(
            combined[always_grp].columns
        )
        assert not missing_cols, (
            f"{label}@{output_step}s: active group {always_grp!r} missing columns "
            f"{sorted(missing_cols)}"
        )
        for stamp in bin_index:
            assert _row_has_valid_scalar(combined[always_grp], pd.Timestamp(stamp)), (
                f"{label}@{output_step}s {stamp}: expected populated {always_grp}"
            )
        for stamp in ds_only_index:
            assert combined[always_grp].loc[pd.Timestamp(stamp)].isna().all(), (
                f"{label}@{output_step}s {stamp}: unexpected {always_grp} on DS row"
            )
    for grp in obs_groups - {"SUEWS", "DailyState", *ALWAYS_ACTIVE_NON_SUEWS_GROUPS}:
        for stamp in ds_only_index:
            assert combined[grp].loc[pd.Timestamp(stamp)].isna().all(), (
                f"{label}@{output_step}s {stamp}: unexpected {grp} on DS row"
            )
        meas_valid = [
            _row_has_valid_scalar(combined[grp], pd.Timestamp(stamp))
            for stamp in bin_index
        ]
        meas_any = [
            bool(combined[grp].loc[pd.Timestamp(stamp)].notna().any())
            for stamp in bin_index
        ]
        assert not any(a and not v for a, v in zip(meas_any, meas_valid, strict=True))
        if any(meas_valid):
            assert all(meas_valid), (
                f"{label}@{output_step}s: active optional group {grp!r} missing bin(s)"
            )


def _assert_matches_independent_oracle(
    combined: pd.DataFrame,
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_step: int,
    label: str,
    native: pd.DataFrame | None = None,
) -> tuple[pd.DatetimeIndex, pd.DatetimeIndex]:
    """Assert combined resampled index, cloud values, and group bins independently."""
    expected_bins, expected_ds_all, expected_ds_only = _independent_cloud_expectations(
        forcing, tstep=tstep, output_step=output_step, native=native
    )
    bin_index = pd.DatetimeIndex(sorted(expected_bins))
    ds_all_index = pd.DatetimeIndex(expected_ds_all)
    ds_only_index = pd.DatetimeIndex(expected_ds_only)
    assert combined.index.equals(bin_index.union(ds_only_index))
    for stamp, expected_val in expected_bins.items():
        actual_val = combined.loc[pd.Timestamp(stamp), ("SUEWS", "Fcld")]
        if np.isnan(expected_val):
            assert pd.isna(actual_val), f"{label}@{output_step}s {stamp}: expected NaN"
        else:
            assert actual_val == pytest.approx(expected_val, abs=1e-15, rel=0)
        # These controlled sample runs populate every non-cloud SUEWS diagnostic.
        # The shared structural check requires just one valid diagnostic per row.
        non_cloud = (
            combined["SUEWS"]
            .drop(columns="Fcld")
            .loc[pd.Timestamp(stamp)]
            .to_numpy(dtype=float)
        )
        assert (
            np.isfinite(non_cloud).all()
            and not np.equal(non_cloud, SUEWS_MISSING_SENTINEL).any()
        )
    for stamp in ds_only_index:
        assert combined["SUEWS"].loc[pd.Timestamp(stamp)].isna().all()
    for stamp in ds_all_index:
        assert _row_has_valid_scalar(combined["DailyState"], pd.Timestamp(stamp)), (
            f"{label}@{output_step}s {stamp}: expected populated DailyState"
        )
    expected_ds_set = set(ds_all_index)
    for stamp in combined.index:
        if stamp not in expected_ds_set:
            assert combined["DailyState"].loc[pd.Timestamp(stamp)].isna().all(), (
                f"{label}@{output_step}s {stamp}: unexpected DailyState population"
            )
    _assert_oracle_non_dailystate_groups(
        combined,
        bin_index=bin_index,
        ds_only_index=ds_only_index,
        output_step=output_step,
        label=label,
        native=native,
    )
    return bin_index, ds_only_index


@pytest.mark.parametrize(
    ("case_name", "output_step"),
    [
        ("midnight_missing", 1800),
        ("midnight_missing", 3600),
        ("midnight_missing", 86400),
        ("midnight_mixed", 1800),
        ("midnight_mixed", 3600),
        ("midnight_mixed", 86400),
        ("leap_midnight_missing", 1800),
        ("leap_midnight_missing", 3600),
        ("leap_midnight_missing", 86400),
        ("leap_midnight_mixed", 1800),
        ("leap_midnight_mixed", 3600),
        ("leap_midnight_mixed", 86400),
        ("leap_full_day_mixed", 3600),
        ("leap_full_day_mixed", 86400),
        ("leap_two_day_mixed", 3600),
        ("leap_two_day_mixed", 86400),
        ("leap_two_day_mixed", 172800),
        ("partial_no_eod_mixed", 1800),
        ("partial_no_eod_mixed", 3600),
        ("partial_no_eod_mixed", 86400),
        ("partial_no_eod_missing", 1800),
        ("partial_no_eod_missing", 3600),
        ("partial_no_eod_missing", 86400),
        ("hourly_native_mixed", 3600),
        ("hourly_native_mixed", 86400),
    ],
)
def test_boundary_resampling_matches_independent_forcing_oracle(
    boundary_run, case_name, output_step
):
    """Exercise public resampling across midnight, leap-day, partial, and daily boundaries."""
    forcing, output, tstep = boundary_run(case_name)
    native = output.df.loc[1]
    resampled = output.resample(freq=f"{output_step}s")
    combined = resampled.df.loc[1]
    suews_group = resampled.get_group("SUEWS").xs(1, level="grid")
    cloud_series = combined["SUEWS", "Fcld"]

    bin_index, _ = _assert_matches_independent_oracle(
        combined,
        forcing,
        tstep=tstep,
        output_step=output_step,
        label=case_name,
        native=native,
    )

    assert_forcing_cloud(cloud_series, forcing, tstep=tstep, output_step=output_step)
    assert_forcing_cloud(suews_group, forcing, tstep=tstep, output_step=output_step)
    assert_forcing_cloud(combined, forcing, tstep=tstep, output_step=output_step)
    assert_forcing_cloud(
        combined, forcing, tstep=tstep, output_step=output_step, native=native
    )
    assert_dailystate_group_occurrence(
        combined, native, tstep=tstep, output_step=output_step
    )
    assert_dailystate_group_occurrence(
        resampled.df, output.df, tstep=tstep, output_step=output_step
    )
    assert_output_group_occurrence(
        combined, native, tstep=tstep, output_step=output_step
    )
    assert_output_group_occurrence(
        resampled.df, output.df, tstep=tstep, output_step=output_step
    )

    # A SUEWS-only slice without DailyState carries only required measurement bins.
    suews_only_frame = combined.drop(columns="DailyState", level=0).loc[bin_index]
    assert_forcing_cloud(
        suews_only_frame, forcing, tstep=tstep, output_step=output_step
    )
    assert_forcing_cloud(
        cloud_series.loc[bin_index], forcing, tstep=tstep, output_step=output_step
    )


def test_native_nan_and_sentinel_missing_cloud_roundtrip(boundary_run):
    """Both NaN and -999 missing cloud forcing round-trip natively and resampled."""
    forcing, output, tstep = boundary_run("leap_full_day_mixed")
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=forcing.index,
        forcing=forcing,
        tstep=tstep,
    )
    for output_step in (300, 3600, 86400):
        resampled = output.resample(freq=f"{output_step}s").df.loc[1]
        assert_forcing_cloud(resampled, forcing, tstep=tstep, output_step=output_step)


def test_documented_utc_timestamp_reference_cloud_contract(boundary_run):
    """Documented UTC forcing keeps naive UTC output and UTC DailyState boundaries."""
    for case_name, steps in (
        ("utc_midnight_mixed", (3600, 86400)),
        ("utc_full_day_mixed", (3600, 86400)),
    ):
        forcing, output, tstep = boundary_run(case_name)
        assert forcing.index.tz is None
        assert output.df.loc[1].index.tz is None
        assert_simulation_output_finiteness(
            output.df,
            expected_grids=(1,),
            expected_timestamps=forcing.index,
            forcing=forcing,
            tstep=tstep,
        )
        for output_step in steps:
            combined = output.resample(freq=f"{output_step}s").df.loc[1]
            _assert_matches_independent_oracle(
                combined, forcing, tstep=tstep, output_step=output_step, label=case_name
            )
            assert_forcing_cloud(
                combined, forcing, tstep=tstep, output_step=output_step
            )
            assert_forcing_cloud(
                combined["SUEWS", "Fcld"],
                forcing,
                tstep=tstep,
                output_step=output_step,
            )


def test_all_missing_no_eod_and_native_step_reject_corruptions(boundary_run):
    """Preserve all-missing bins and reject stray rows on no-EOD and native outputs."""
    ds_bin = pd.Timestamp("2012-01-02 00:00:00")
    stray_stamp = pd.Timestamp("2012-01-01 23:00:00")
    all_missing_bin = pd.Timestamp("2012-01-02 01:00:00")

    # All-missing partial midnight @ 3600s: dropping any required bin must fail.
    forcing_miss, output_miss, tstep = boundary_run("midnight_missing")
    series_miss = output_miss.resample("3600s").df.loc[1, ("SUEWS", "Fcld")]
    assert series_miss.isna().all()
    assert_forcing_cloud(series_miss, forcing_miss, tstep=tstep, output_step=3600)
    for bad_miss in (
        series_miss.drop(index=all_missing_bin),
        series_miss.loc[[ds_bin]],
        series_miss.dropna(),
    ):
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(bad_miss, forcing_miss, tstep=tstep, output_step=3600)

    # Partial window with no EOD row: zero DailyState structural rows allowed.
    forcing_no_eod, output_no_eod, _ = boundary_run("partial_no_eod_mixed")
    series_no_eod = output_no_eod.resample("3600s").df.loc[1, ("SUEWS", "Fcld")]
    assert_forcing_cloud(series_no_eod, forcing_no_eod, tstep=tstep, output_step=3600)
    stray_leading = pd.concat([
        pd.Series([np.nan], index=pd.DatetimeIndex(["2012-01-01 00:00:00"])),
        series_no_eod,
    ])
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(
            stray_leading, forcing_no_eod, tstep=tstep, output_step=3600
        )

    # Native cadence with explicit output_step == tstep still forbids extra rows.
    forcing_mix, output_mix, _ = boundary_run("midnight_mixed")
    native_with_extra = pd.concat([
        pd.Series([np.nan], index=pd.DatetimeIndex([stray_stamp])),
        output_mix.df.loc[1, ("SUEWS", "Fcld")],
    ])
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(
            native_with_extra, forcing_mix, tstep=tstep, output_step=tstep
        )


@pytest.mark.parametrize("unit", ["ns", "us", "ms", "s"])
def test_storage_unit_aggregated_clocks_and_cross_unit_pairs(storage_unit_runs, unit):
    """Aggregated cloud checks and all 16 cross-unit hourly pairs pass across units."""
    forcing, output, _ = storage_unit_runs[unit]
    for output_step in (300, 1800, 3600, 86400):
        resampled = output.resample(freq=f"{output_step}s")
        combined = resampled.df.loc[1]
        suews_group = resampled.get_group("SUEWS").xs(1, level="grid")
        cloud_series = combined["SUEWS", "Fcld"]

        assert_forcing_cloud(cloud_series, forcing, tstep=300, output_step=output_step)
        assert_forcing_cloud(suews_group, forcing, tstep=300, output_step=output_step)
        assert_forcing_cloud(combined, forcing, tstep=300, output_step=output_step)

        if output_step > 300:
            _assert_matches_independent_oracle(
                combined, forcing, tstep=300, output_step=output_step, label=unit
            )

    hourly_res = output.resample(freq="3600s")
    hourly_combined_base = hourly_res.df.loc[1]
    hourly_suews_base = hourly_res.get_group("SUEWS").xs(1, level="grid")
    for obs_unit in ("s", "ms", "us", "ns"):
        hourly_comb = hourly_combined_base.copy()
        hourly_comb.index = hourly_comb.index.as_unit(obs_unit)
        hourly_suews = hourly_suews_base.copy()
        hourly_suews.index = hourly_suews.index.as_unit(obs_unit)
        assert_forcing_cloud(hourly_comb, forcing, tstep=300, output_step=3600)
        assert_forcing_cloud(hourly_suews, forcing, tstep=300, output_step=3600)
        assert_forcing_cloud(
            hourly_comb["SUEWS", "Fcld"], forcing, tstep=300, output_step=3600
        )


@pytest.mark.parametrize("unit", ["ns", "us", "ms", "s"])
def test_storage_unit_aggregated_clock_negative_controls(storage_unit_runs, unit):
    """Bad output_step, duplicate hourly stamps, and shifted hourly forcing fail."""
    forcing, output, _ = storage_unit_runs[unit]
    native_cloud = output.df.loc[1, ("SUEWS", "Fcld")]
    hourly_cloud = output.resample("3600s").df.loc[1, ("SUEWS", "Fcld")]

    for bad_step in (450, True):
        with pytest.raises(AssertionError, match="fixed integer multiple"):
            assert_forcing_cloud(native_cloud, forcing, tstep=300, output_step=bad_step)

    with pytest.raises(AssertionError, match="duplicate timestamps in output clock"):
        assert_forcing_cloud(
            pd.concat([hourly_cloud, hourly_cloud.iloc[-1:]]),
            forcing,
            tstep=300,
            output_step=3600,
        )

    shifted_600 = forcing.copy()
    shifted_600.index = (
        shifted_600.index + pd.Timedelta(seconds=600).as_unit(unit)
    ).as_unit(unit)
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(hourly_cloud, shifted_600, tstep=300, output_step=3600)


@pytest.mark.parametrize("unit", ["ns", "us", "ms", "s"])
def test_storage_unit_structural_and_series_corruptions(storage_unit_runs, unit):
    """All Series, combined-frame, and flat-SUEWS structural corruptions fail across units."""
    forcing, output, _ = storage_unit_runs[unit]
    native = output.df.loc[1]
    resampled = output.resample("3600s")
    combined = resampled.df.loc[1]
    suews_flat = resampled.get_group("SUEWS").xs(1, level="grid")
    ds_bin = pd.Timestamp("2012-01-02 00:00:00")
    stray_ds_stamp = pd.Timestamp("2012-01-01 23:00:00")
    all_missing_bin = pd.Timestamp("2012-01-02 01:00:00")

    for label, bad_series in _series_corruptions(
        combined["SUEWS", "Fcld"],
        stray_ds_stamp=stray_ds_stamp,
        finite_bin=ds_bin,
        all_missing_bin=all_missing_bin,
    ):
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(bad_series, forcing, tstep=300, output_step=3600)
        assert label

    for label, bad_frame in _combined_frame_corruptions(
        combined,
        ds_bin=ds_bin,
        stray_ds_stamp=stray_ds_stamp,
        all_missing_bin=all_missing_bin,
    ):
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(bad_frame, forcing, tstep=300, output_step=3600)
        assert label

    with pytest.raises(AssertionError, match="DailyState"):
        assert_forcing_cloud(
            combined.drop(columns="DailyState", level=0),
            forcing,
            tstep=300,
            output_step=3600,
            native=native,
        )

    for label, bad_flat in _flat_suews_corruptions(
        suews_flat,
        stray_ds_stamp=stray_ds_stamp,
        all_missing_bin=all_missing_bin,
    ):
        with pytest.raises(AssertionError, match="Fcld"):
            assert_forcing_cloud(bad_flat, forcing, tstep=300, output_step=3600)
        assert label


@pytest.mark.parametrize("fine_unit", ["ms", "us", "ns"])
def test_aggregated_subsecond_shifts_never_masked_by_coarser_unit(
    storage_unit_runs, fine_unit
):
    """Comparing s against ms/us/ns never downcasts away an hourly sub-second shift."""
    forcing_s, output_s, _ = storage_unit_runs["s"]
    forcing_fine, _, _ = storage_unit_runs[fine_unit]
    hourly_cloud = output_s.resample("3600s").df.loc[1, ("SUEWS", "Fcld")]
    tick = pd.Timedelta(1, fine_unit)

    shifted_forcing = forcing_fine.copy()
    shifted_forcing.index = (shifted_forcing.index + tick).as_unit(fine_unit)
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(hourly_cloud, shifted_forcing, tstep=300, output_step=3600)

    shifted_hourly = hourly_cloud.copy()
    shifted_hourly.index = (shifted_hourly.index.as_unit(fine_unit) + tick).as_unit(
        fine_unit
    )
    with pytest.raises(AssertionError, match="missing required measurement bins"):
        assert_forcing_cloud(shifted_hourly, forcing_s, tstep=300, output_step=3600)


_SEVEN_HOUR_CLOUD_OBSERVATIONS: dict[str, float] = {
    "2012-01-04 23:50": 0.20,
    "2012-01-05 04:00": 0.60,
    "2012-01-05 04:05": 1.00,
    "2012-01-05 11:00": 0.00,
    "2012-01-05 11:05": 0.35,
    "2012-01-05 18:00": 0.85,
    "2012-01-05 18:05": 0.80,
    "2012-01-06 00:10": 0.80,
}


def _seven_hour_overlap_forcing(sample: pd.DataFrame) -> pd.DataFrame:
    """Build the 293-step partial-interval forcing window with mixed cloud values."""
    forcing = sample.loc["2012-01-04 23:50":"2012-01-06 00:10"].copy()
    forcing["fcld"] = SUEWS_MISSING_SENTINEL
    for stamp, value in _SEVEN_HOUR_CLOUD_OBSERVATIONS.items():
        forcing.loc[pd.Timestamp(stamp), "fcld"] = value
    return forcing


def _erase_group_row(
    frame: pd.DataFrame, group: str, row_key, *, fill_value: float = np.nan
) -> pd.DataFrame:
    """Return a copy with every field of ``group`` at ``row_key`` set to ``fill_value``."""
    mutated = frame.copy()
    for col in mutated[group].columns:
        mutated.at[row_key, (group, col)] = fill_value
    return mutated


def _erase_dailystate_row(
    frame: pd.DataFrame, row_key, *, fill_value: float = np.nan
) -> pd.DataFrame:
    """Return a copy with every DailyState field at ``row_key`` set to ``fill_value``."""
    return _erase_group_row(frame, "DailyState", row_key, fill_value=fill_value)


def _assert_active_group_mutation_rejected(
    mutated: pd.DataFrame,
    forcing: pd.DataFrame,
    native: pd.DataFrame,
    *,
    output_step: int,
    group: str,
    label: str,
    check_without_native: bool = True,
) -> None:
    """Verify aggregate checks reject an active-group erasure or column drop."""
    if check_without_native:
        with pytest.raises(AssertionError, match=group):
            assert_forcing_cloud(mutated, forcing, tstep=300, output_step=output_step)
    with pytest.raises(AssertionError, match=group):
        assert_forcing_cloud(
            mutated, forcing, tstep=300, output_step=output_step, native=native
        )
    with pytest.raises(AssertionError, match=group):
        assert_output_group_occurrence(
            mutated, native, tstep=300, output_step=output_step
        )
    with pytest.raises(AssertionError):
        _assert_matches_independent_oracle(
            mutated,
            forcing,
            tstep=300,
            output_step=output_step,
            label=label,
            native=native,
        )


def _assert_active_group_accepted(
    frame: pd.DataFrame,
    forcing: pd.DataFrame,
    native: pd.DataFrame,
    *,
    output_step: int,
    label: str,
) -> None:
    """Verify all aggregate checks pass intact or partially-missing active groups."""
    assert_forcing_cloud(frame, forcing, tstep=300, output_step=output_step)
    assert_forcing_cloud(
        frame, forcing, tstep=300, output_step=output_step, native=native
    )
    assert_output_group_occurrence(frame, native, tstep=300, output_step=output_step)
    _assert_matches_independent_oracle(
        frame,
        forcing,
        tstep=300,
        output_step=output_step,
        label=label,
        native=native,
    )


def _assert_dailystate_mutation_rejected(
    mutated: pd.DataFrame,
    forcing: pd.DataFrame,
    native: pd.DataFrame,
    *,
    output_step: int,
    label: str,
) -> None:
    """Verify every aggregate checker rejects a DailyState group-occurrence defect."""
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(mutated, forcing, tstep=300, output_step=output_step)
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(
            mutated, forcing, tstep=300, output_step=output_step, native=native
        )
    with pytest.raises(AssertionError, match="DailyState"):
        assert_dailystate_group_occurrence(
            mutated, native, tstep=300, output_step=output_step
        )
    with pytest.raises(AssertionError):
        _assert_matches_independent_oracle(
            mutated,
            forcing,
            tstep=300,
            output_step=output_step,
            label=label,
            native=native,
        )


def _assert_dailystate_mutation_accepted(
    mutated: pd.DataFrame,
    forcing: pd.DataFrame,
    native: pd.DataFrame,
    *,
    output_step: int,
    label: str,
) -> None:
    """Verify group-existence checks accept legitimate partial DailyState missingness."""
    assert_forcing_cloud(mutated, forcing, tstep=300, output_step=output_step)
    assert_forcing_cloud(
        mutated, forcing, tstep=300, output_step=output_step, native=native
    )
    assert_dailystate_group_occurrence(
        mutated, native, tstep=300, output_step=output_step
    )
    _assert_matches_independent_oracle(
        mutated,
        forcing,
        tstep=300,
        output_step=output_step,
        label=label,
        native=native,
    )


@pytest.fixture(scope="module")
def seven_hour_overlap_run(sample_config_loaded, sample_data_loaded):
    """Authentic public run producing both exclusive and overlapping DailyState rows."""
    _, sample = sample_data_loaded
    forcing = _seven_hour_overlap_forcing(sample)
    cfg = sample_config_loaded.model_copy(deep=True)
    payload = cfg.model_dump(by_alias=True, round_trip=True)
    payload["model"]["control"]["output"]["freq"] = 25200
    sim = SUEWSSimulation()
    sim.update_config(type(cfg).model_validate(payload), auto_load_forcing=False)
    sim.update_forcing(forcing)
    output = sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    return sim.forcing.df.copy(), output


def _assert_row_dailystate_mutations(
    combined: pd.DataFrame,
    forcing: pd.DataFrame,
    native: pd.DataFrame,
    *,
    target_ts: pd.Timestamp,
    tag: str,
) -> None:
    """Exercise full erasure, non-valid-only, and partial-missingness row controls."""
    erased = _erase_dailystate_row(combined, target_ts)
    _assert_dailystate_mutation_rejected(
        erased, forcing, native, output_step=25200, label=f"7h-{tag}-erased"
    )
    for bad_tag, bad_val in (
        ("pos_inf", np.inf),
        ("neg_inf", -np.inf),
        ("sentinel", SUEWS_MISSING_SENTINEL),
    ):
        bad_only = erased.copy()
        bad_only.at[target_ts, ("DailyState", "LAI_Grass")] = bad_val
        _assert_dailystate_mutation_rejected(
            bad_only, forcing, native, output_step=25200, label=f"7h-{tag}-{bad_tag}"
        )

    # Scoped to group existence: partial missingness with >=1 valid scalar passes.
    rm_one = combined.copy()
    rm_one.at[target_ts, ("DailyState", "LAI_Grass")] = np.nan
    _assert_dailystate_mutation_accepted(
        rm_one, forcing, native, output_step=25200, label=f"7h-{tag}-rm1"
    )
    keep_one = erased.copy()
    keep_one.at[target_ts, ("DailyState", "LAI_Grass")] = combined.at[
        target_ts, ("DailyState", "LAI_Grass")
    ]
    _assert_dailystate_mutation_accepted(
        keep_one, forcing, native, output_step=25200, label=f"7h-{tag}-keep1"
    )


def test_overlapping_and_exclusive_dailystate_resample_controls(seven_hour_overlap_run):
    """Right-labelled DailyState aligns with SUEWS bins across 7-hour and 25-hour cadences."""
    forcing, output = seven_hour_overlap_run
    native = output.df.loc[1]
    combined = output.resample("25200s").df.loc[1]

    day1_ts = pd.Timestamp("2012-01-05 04:00:00")
    day2_ts = pd.Timestamp("2012-01-06 01:00:00")
    suews_only_ts = pd.Timestamp("2012-01-05 11:00:00")

    # Confirm the 7-hour right-labelled resample aligns both DailyState rows onto
    # SUEWS bins with zero exclusive legacy left-labelled rows.
    assert pd.Timestamp("2012-01-04 21:00:00") not in combined.index
    assert combined["SUEWS"].loc[day1_ts].notna().all()
    assert combined["SUEWS"].loc[day2_ts].notna().all()
    assert combined["SUEWS", "Fcld"].loc[day1_ts] == pytest.approx(
        0.4, abs=1e-15, rel=0
    )
    assert combined["SUEWS", "Fcld"].loc[day2_ts] == pytest.approx(
        0.8, abs=1e-15, rel=0
    )
    np.testing.assert_array_equal(
        native["DailyState"].loc[pd.Timestamp("2012-01-04 23:55:00")].to_numpy(),
        combined["DailyState"].loc[day1_ts].to_numpy(),
    )
    np.testing.assert_array_equal(
        native["DailyState"].loc[pd.Timestamp("2012-01-05 23:55:00")].to_numpy(),
        combined["DailyState"].loc[day2_ts].to_numpy(),
    )

    # Intact positive controls pass across 7-hour and 25-hour cadences.
    _assert_dailystate_mutation_accepted(
        combined, forcing, native, output_step=25200, label="7h-intact"
    )
    combined_25h = output.resample("90000s").df.loc[1]
    _assert_dailystate_mutation_accepted(
        combined_25h, forcing, native, output_step=90000, label="25h-intact"
    )
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(combined_25h, pd.Timestamp("2012-01-05 01:00:00")),
        forcing,
        native,
        output_step=90000,
        label="25h-day1-erased",
    )

    # Full erasure and non-valid-only corruptions fail on BOTH DailyState rows.
    for tag, target_ts in (("day1", day1_ts), ("day2", day2_ts)):
        _assert_row_dailystate_mutations(
            combined, forcing, native, target_ts=target_ts, tag=tag
        )

    # Spurious or shifted DailyState on a SUEWS-only row must fail.
    spurious = combined.copy()
    for col in combined["DailyState"].columns:
        spurious.at[suews_only_ts, ("DailyState", col)] = combined.at[
            day2_ts, ("DailyState", col)
        ]
    _assert_dailystate_mutation_rejected(
        spurious, forcing, native, output_step=25200, label="7h-spurious"
    )
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(spurious, day2_ts),
        forcing,
        native,
        output_step=25200,
        label="7h-shifted",
    )

    # Cloud and SUEWS discriminators on a shared SUEWS+DailyState row remain enforced.
    wrong_fcld = combined.copy()
    wrong_fcld.at[day2_ts, ("SUEWS", "Fcld")] = 0.83
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(wrong_fcld, forcing, tstep=300, output_step=25200)
    with pytest.raises(AssertionError):
        _assert_matches_independent_oracle(
            wrong_fcld, forcing, tstep=300, output_step=25200, label="7h-wrong-fcld"
        )
    erased_suews = combined.copy()
    for col in combined["SUEWS"].columns:
        erased_suews.at[day2_ts, ("SUEWS", col)] = np.nan
    with pytest.raises(AssertionError, match="Fcld"):
        assert_forcing_cloud(erased_suews, forcing, tstep=300, output_step=25200)


def test_native_dailystate_subset_origin_and_contract_boundaries(
    seven_hour_overlap_run,
):
    """Native subset keeps the shared grid origin and preserves SUEWS-only caller contracts."""
    forcing, output = seven_hour_overlap_run
    native = output.df.loc[1]
    resampled = output.resample("25200s")
    combined = resampled.df.loc[1]
    day1_ts = pd.Timestamp("2012-01-05 04:00:00")
    day2_ts = pd.Timestamp("2012-01-06 01:00:00")

    # Pure SUEWS-only, flat SUEWS, and Series callers keep forcing-only cloud contracts.
    suews_only = combined.drop(columns="DailyState", level=0)
    assert_forcing_cloud(suews_only, forcing, tstep=300, output_step=25200)
    assert_forcing_cloud(combined["SUEWS"], forcing, tstep=300, output_step=25200)
    assert_forcing_cloud(
        combined["SUEWS", "Fcld"], forcing, tstep=300, output_step=25200
    )
    with pytest.raises(AssertionError, match="requires a DailyState group"):
        assert_forcing_cloud(
            suews_only, forcing, tstep=300, output_step=25200, native=native
        )
    with pytest.raises(AssertionError, match="requires a DailyState group"):
        assert_forcing_cloud(
            combined["SUEWS", "Fcld"],
            forcing,
            tstep=300,
            output_step=25200,
            native=native,
        )

    # Native subset occurrence: if Day 1 DailyState is absent natively, public resample
    # keeps the shared per-grid origin so Day 2 stays on 2012-01-06 01:00 (matching SUEWS).
    day2_only_native_df = _erase_dailystate_row(
        output.df, (1, pd.Timestamp("2012-01-04 23:55:00"))
    )
    day2_only_res = SUEWSOutput(
        day2_only_native_df, output.state_final, config=output.config
    ).resample("25200s")
    day2_only_native = day2_only_native_df.loc[1]
    day2_only_comb = day2_only_res.df.loc[1]
    assert day1_ts in day2_only_comb.index
    assert day2_ts in day2_only_comb.index
    assert day2_only_comb["DailyState"].loc[day1_ts].isna().all()
    assert day2_only_comb["DailyState"].loc[day2_ts].notna().any()

    assert_dailystate_group_occurrence(
        day2_only_comb, day2_only_native, tstep=300, output_step=25200
    )
    assert_forcing_cloud(
        day2_only_comb,
        forcing,
        tstep=300,
        output_step=25200,
        native=day2_only_native,
    )
    _assert_matches_independent_oracle(
        day2_only_comb,
        forcing,
        tstep=300,
        output_step=25200,
        label="7h-day2-only-native",
        native=day2_only_native,
    )
    with pytest.raises(AssertionError, match="DailyState"):
        assert_dailystate_group_occurrence(
            _erase_dailystate_row(day2_only_comb, day2_ts),
            day2_only_native,
            tstep=300,
            output_step=25200,
        )

    # Invalid-only native DailyState corruption must be rejected rather than ignored.
    bad_native = day2_only_native.copy()
    bad_native.at[pd.Timestamp("2012-01-04 23:55:00"), ("DailyState", "LAI_Grass")] = (
        np.inf
    )
    with pytest.raises(AssertionError, match="non-finite or -999"):
        assert_dailystate_group_occurrence(
            combined, bad_native, tstep=300, output_step=25200
        )


def _assert_active_bl_resample_controls(
    forcing: pd.DataFrame, output: SUEWSOutput, overlap_ts: pd.Timestamp
) -> None:
    """Exercise active registry group BL through public SUEWSOutput.resample()."""
    bl_cols = expected_group_columns("BL")
    bl_native_block = pd.DataFrame(
        1.5,
        index=output.df.index,
        columns=pd.MultiIndex.from_product([["BL"], bl_cols], names=["group", "var"]),
    )
    native_with_bl_full = pd.concat([output.df, bl_native_block], axis=1)
    res_bl = SUEWSOutput(
        native_with_bl_full, output.state_final, config=output.config
    ).resample("25200s")
    native_bl = native_with_bl_full.loc[1]
    comb_bl = res_bl.df.loc[1]
    _assert_active_group_accepted(
        comb_bl, forcing, native_bl, output_step=25200, label="7h-BL-intact"
    )
    _assert_active_group_mutation_rejected(
        _erase_group_row(comb_bl, "BL", overlap_ts),
        forcing,
        native_bl,
        output_step=25200,
        group="BL",
        label="7h-BL-overlap-erased",
    )
    wiped_bl = comb_bl.copy()
    wiped_bl["BL"] = np.nan
    _assert_active_group_mutation_rejected(
        wiped_bl,
        forcing,
        native_bl,
        output_step=25200,
        group="BL",
        label="7h-BL-all-wiped",
        check_without_native=False,
    )
    _assert_active_group_mutation_rejected(
        comb_bl.drop(columns="BL", level=0),
        forcing,
        native_bl,
        output_step=25200,
        group="BL",
        label="7h-BL-dropped-group",
        check_without_native=False,
    )
    _assert_active_group_mutation_rejected(
        comb_bl.drop(columns=[("BL", bl_cols[0])]),
        forcing,
        native_bl,
        output_step=25200,
        group="BL",
        label="7h-BL-dropped-col",
    )


def test_active_non_dailystate_group_occurrence_and_column_controls(
    seven_hour_overlap_run,
):
    """Later-row erasures, dropped columns, and inactive-group rules are enforced."""
    forcing, output = seven_hour_overlap_run
    native = output.df.loc[1]
    combined = output.resample("25200s").df.loc[1]
    overlap_ts = pd.Timestamp("2012-01-06 01:00:00")
    non_overlap_ts = pd.Timestamp("2012-01-05 18:00:00")

    # Intact frame passes with active BEERS/RSL/debug and inactive optional groups.
    _assert_active_group_accepted(
        combined, forcing, native, output_step=25200, label="7h-active-intact"
    )

    # Erasing BEERS, RSL, or debug on the overlapping 01:00 row while keeping
    # SUEWS and DailyState intact must fail across all aggregate checks.
    for grp, sample_col in (
        ("BEERS", "azimuth"),
        ("RSL", "U_1"),
        ("debug", "ss_Paved_next"),
    ):
        erased = _erase_group_row(combined, grp, overlap_ts)
        _assert_active_group_mutation_rejected(
            erased,
            forcing,
            native,
            output_step=25200,
            group=grp,
            label=f"7h-{grp}-overlap-erased",
        )
        for bad_tag, bad_val in (
            ("pos_inf", np.inf),
            ("neg_inf", -np.inf),
            ("sentinel", SUEWS_MISSING_SENTINEL),
        ):
            bad_only = erased.copy()
            bad_only.at[overlap_ts, (grp, sample_col)] = bad_val
            _assert_active_group_mutation_rejected(
                bad_only,
                forcing,
                native,
                output_step=25200,
                group=grp,
                label=f"7h-{grp}-{bad_tag}",
            )
        # Dropping the entire active group or a single column must fail.
        _assert_active_group_mutation_rejected(
            combined.drop(columns=grp, level=0),
            forcing,
            native,
            output_step=25200,
            group=grp,
            label=f"7h-{grp}-dropped-group",
        )
        _assert_active_group_mutation_rejected(
            combined.drop(columns=[(grp, sample_col)]),
            forcing,
            native,
            output_step=25200,
            group=grp,
            label=f"7h-{grp}-dropped-col",
        )

    # Erasing BEERS on a non-overlap bin and dropping SUEWS/DailyState columns.
    _assert_active_group_mutation_rejected(
        _erase_group_row(combined, "BEERS", non_overlap_ts),
        forcing,
        native,
        output_step=25200,
        group="BEERS",
        label="7h-BEERS-non-overlap-erased",
    )
    for grp, col in (("SUEWS", "QH"), ("DailyState", "LAI_Grass")):
        _assert_active_group_mutation_rejected(
            combined.drop(columns=[(grp, col)]),
            forcing,
            native,
            output_step=25200,
            group=grp,
            label=f"7h-{grp}-dropped-col",
        )

    # Partial missingness with remaining valid diagnostics in BEERS still passes.
    partial_beers = combined.copy()
    partial_beers.at[overlap_ts, ("BEERS", "azimuth")] = np.nan
    _assert_active_group_accepted(
        partial_beers, forcing, native, output_step=25200, label="7h-BEERS-partial"
    )

    # Spurious finite or non-finite values in inactive groups (ESTM, STEBBS) must fail.
    for inactive_grp, col in (("ESTM", "QS"), ("STEBBS", "Tair_2")):
        for bad_tag, bad_val in (
            ("spurious", 12.5),
            ("inf", np.inf),
            ("sentinel", -999.0),
        ):
            corrupted = combined.copy()
            corrupted.at[overlap_ts, (inactive_grp, col)] = bad_val
            _assert_active_group_mutation_rejected(
                corrupted,
                forcing,
                native,
                output_step=25200,
                group=inactive_grp,
                label=f"7h-{inactive_grp}-{bad_tag}",
            )

    _assert_active_bl_resample_controls(forcing, output, overlap_ts)


def test_dailystate_occurrence_ehc_legitimate_missing_fields(
    sample_config_loaded, sample_data_loaded
):
    """Authentic EHC leaves DailyState a1/a2/a3 NaN; intact passes and erasures fail."""
    _, sample = sample_data_loaded
    forcing = _seven_hour_overlap_forcing(sample)
    overlap_ts = pd.Timestamp("2012-01-06 01:00:00")

    cfg_ehc = sample_config_loaded.model_copy(deep=True)
    cfg_ehc.model.physics.storage_heat = StorageHeatMethod.EHC
    sim_ehc = SUEWSSimulation()
    sim_ehc.update_config(cfg_ehc, auto_load_forcing=False)
    sim_ehc.update_forcing(forcing.copy())
    out_ehc = sim_ehc.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    )
    native_ehc = out_ehc.df.loc[1]
    res_ehc = out_ehc.resample("25200s").df.loc[1]
    assert native_ehc["DailyState"][["a1", "a2", "a3"]].isna().all().all()
    assert res_ehc["DailyState"][["a1", "a2", "a3"]].isna().all().all()
    _assert_dailystate_mutation_accepted(
        res_ehc, sim_ehc.forcing.df, native_ehc, output_step=25200, label="ehc-7h"
    )
    assert_output_group_occurrence(res_ehc, native_ehc, tstep=300, output_step=25200)
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(res_ehc, overlap_ts),
        sim_ehc.forcing.df,
        native_ehc,
        output_step=25200,
        label="ehc-7h-overlap-erased",
    )
    # Enabled EHC group erasure, full wipe, and column drops must fail.
    _assert_active_group_mutation_rejected(
        _erase_group_row(res_ehc, "EHC", overlap_ts),
        sim_ehc.forcing.df,
        native_ehc,
        output_step=25200,
        group="EHC",
        label="ehc-7h-group-overlap-erased",
    )
    wiped_ehc = res_ehc.copy()
    wiped_ehc["EHC"] = np.nan
    _assert_active_group_mutation_rejected(
        wiped_ehc,
        sim_ehc.forcing.df,
        native_ehc,
        output_step=25200,
        group="EHC",
        label="ehc-7h-group-wiped",
        check_without_native=False,
    )
    _assert_active_group_mutation_rejected(
        res_ehc.drop(columns="EHC", level=0),
        sim_ehc.forcing.df,
        native_ehc,
        output_step=25200,
        group="EHC",
        label="ehc-7h-group-dropped",
        check_without_native=False,
    )
    ehc_first_col = str(res_ehc["EHC"].columns[0])
    _assert_active_group_mutation_rejected(
        res_ehc.drop(columns=[("EHC", ehc_first_col)]),
        sim_ehc.forcing.df,
        native_ehc,
        output_step=25200,
        group="EHC",
        label="ehc-7h-col-dropped",
    )


def test_active_snow_group_occurrence_and_erasure_controls(
    sample_config_loaded, sample_data_loaded
):
    """Authentic snow_use=1 enforces resampled snow group occurrence and columns."""
    _, sample = sample_data_loaded
    forcing = _seven_hour_overlap_forcing(sample)
    overlap_ts = pd.Timestamp("2012-01-06 01:00:00")

    cfg_snow = sample_config_loaded.model_copy(deep=True)
    cfg_snow.model.physics.snow_use = SnowUse(1)
    sim_snow = SUEWSSimulation()
    sim_snow.update_config(cfg_snow, auto_load_forcing=False)
    sim_snow.update_forcing(forcing.copy())
    out_snow = sim_snow.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    )
    native_snow = out_snow.df.loc[1]
    res_snow = out_snow.resample("25200s").df.loc[1]

    _assert_active_group_accepted(
        res_snow, sim_snow.forcing.df, native_snow, output_step=25200, label="snow-7h"
    )
    _assert_active_group_mutation_rejected(
        _erase_group_row(res_snow, "snow", overlap_ts),
        sim_snow.forcing.df,
        native_snow,
        output_step=25200,
        group="snow",
        label="snow-7h-overlap-erased",
    )
    wiped_snow = res_snow.copy()
    wiped_snow["snow"] = np.nan
    _assert_active_group_mutation_rejected(
        wiped_snow,
        sim_snow.forcing.df,
        native_snow,
        output_step=25200,
        group="snow",
        label="snow-7h-wiped",
        check_without_native=False,
    )
    _assert_active_group_mutation_rejected(
        res_snow.drop(columns="snow", level=0),
        sim_snow.forcing.df,
        native_snow,
        output_step=25200,
        group="snow",
        label="snow-7h-dropped-group",
        check_without_native=False,
    )
    snow_first_col = str(res_snow["snow"].columns[0])
    _assert_active_group_mutation_rejected(
        res_snow.drop(columns=[("snow", snow_first_col)]),
        sim_snow.forcing.df,
        native_snow,
        output_step=25200,
        group="snow",
        label="snow-7h-dropped-col",
    )


def test_dailystate_occurrence_multiday_and_no_eod_partial_bins(boundary_run):
    """Daily bins, two-day merged bins, and no-EOD partial windows enforce occurrence."""
    forcing_2d, out_2d, tstep_2d = boundary_run("leap_two_day_mixed")
    assert tstep_2d == 300
    native_2d = out_2d.df.loc[1]
    daily_2d = out_2d.resample("86400s").df.loc[1]
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(daily_2d, pd.Timestamp("2012-02-29 00:00:00")),
        forcing_2d,
        native_2d,
        output_step=86400,
        label="2d-daily-day1-erased",
    )
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(daily_2d, pd.Timestamp("2012-03-01 00:00:00")),
        forcing_2d,
        native_2d,
        output_step=86400,
        label="2d-daily-day2-erased",
    )
    merged_2d = out_2d.resample("172800s").df.loc[1]
    _assert_dailystate_mutation_rejected(
        _erase_dailystate_row(merged_2d, pd.Timestamp("2012-03-01 00:00:00")),
        forcing_2d,
        native_2d,
        output_step=172800,
        label="2d-merged-erased",
    )

    forcing_ne, out_ne, _ = boundary_run("partial_no_eod_mixed")
    res_ne = out_ne.resample("3600s").df.loc[1]
    spurious_ne = res_ne.copy()
    spurious_ne.at[res_ne.index[0], ("DailyState", "LAI_Grass")] = 1.6
    _assert_dailystate_mutation_rejected(
        spurious_ne, forcing_ne, out_ne.df.loc[1], output_step=3600, label="no-eod-bad"
    )


def test_dailystate_occurrence_distinguishable_multigrid(
    sample_config_loaded, sample_data_loaded
):
    """Distinguishable grids check DailyState and active groups across units and mutations."""
    _, sample = sample_data_loaded
    forcing = _seven_hour_overlap_forcing(sample)
    day1_ts = pd.Timestamp("2012-01-05 04:00:00")
    day2_ts = pd.Timestamp("2012-01-06 01:00:00")

    cfg_mg = sample_config_loaded.model_copy(deep=True)
    site_11 = cfg_mg.sites[0].model_copy(deep=True)
    site_11.gridiv = 11
    site_11.name = "daily-overlap-11"
    site_11.initial_states.grass.lai_id.value = 1.6
    site_29 = cfg_mg.sites[0].model_copy(deep=True)
    site_29.gridiv = 29
    site_29.name = "daily-overlap-29"
    site_29.initial_states.grass.lai_id.value = 3.5
    cfg_mg.sites = [site_11, site_29]

    sim_mg = SUEWSSimulation()
    sim_mg.update_config(cfg_mg, auto_load_forcing=False)
    sim_mg.update_forcing(forcing.copy())
    out_mg = sim_mg.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    )
    res_mg = out_mg.resample("25200s").df
    assert not np.isclose(
        res_mg.at[(11, day2_ts), ("DailyState", "LAI_Grass")],
        res_mg.at[(29, day2_ts), ("DailyState", "LAI_Grass")],
    )
    assert_dailystate_group_occurrence(res_mg, out_mg.df, tstep=300, output_step=25200)
    assert_output_group_occurrence(res_mg, out_mg.df, tstep=300, output_step=25200)

    # Cross-unit multi-grid check (ns native vs s resampled) succeeds without downcasting.
    res_mg_s = res_mg.copy()
    res_mg_s.index = pd.MultiIndex.from_arrays(
        [
            res_mg_s.index.get_level_values(0),
            res_mg_s.index.get_level_values(1).as_unit("s"),
        ],
        names=res_mg_s.index.names,
    )
    assert_dailystate_group_occurrence(
        res_mg_s, out_mg.df, tstep=300, output_step=25200
    )
    assert_output_group_occurrence(res_mg_s, out_mg.df, tstep=300, output_step=25200)

    # Erasing Grid 29 day-2 DailyState or Grid 11 day-1 DailyState fails loudly.
    with pytest.raises(AssertionError, match=r"DailyState\[grid=29\]"):
        assert_dailystate_group_occurrence(
            _erase_dailystate_row(res_mg, (29, day2_ts)),
            out_mg.df,
            tstep=300,
            output_step=25200,
        )
    with pytest.raises(AssertionError, match=r"DailyState\[grid=11\]"):
        assert_dailystate_group_occurrence(
            _erase_dailystate_row(res_mg, (11, day1_ts)),
            out_mg.df,
            tstep=300,
            output_step=25200,
        )
    # Erasing Grid 29 BEERS at day-2 bin or dropping RSL fails multi-grid group occurrence.
    with pytest.raises(AssertionError, match=r"OutputGroup\[grid=29\].*BEERS"):
        assert_output_group_occurrence(
            _erase_group_row(res_mg, "BEERS", (29, day2_ts)),
            out_mg.df,
            tstep=300,
            output_step=25200,
        )
    with pytest.raises(AssertionError, match=r"OutputGroup\[grid=11\].*RSL"):
        assert_output_group_occurrence(
            res_mg.drop(columns="RSL", level=0),
            out_mg.df,
            tstep=300,
            output_step=25200,
        )
    # Dropping a grid from observed fails grid-ID parity.
    with pytest.raises(AssertionError, match="grid IDs"):
        assert_dailystate_group_occurrence(
            res_mg.loc[[11]], out_mg.df, tstep=300, output_step=25200
        )
    with pytest.raises(AssertionError, match="grid IDs"):
        assert_output_group_occurrence(
            res_mg.loc[[11]], out_mg.df, tstep=300, output_step=25200
        )
