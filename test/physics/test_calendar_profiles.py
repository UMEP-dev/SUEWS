"""Asymmetric weekday/holiday calendar and weekly irrigation schedule tests (W2-4a).

Many shipped fixtures assign identical or uniform profiles across working days
and holidays (and leave ``irrigation.daywat`` all zero), so a calendar dispatch
regression or profile permutation bug can pass the sample tests silently.

These end-to-end ``SUEWSSimulation`` (YAML -> Rust engine) tests construct
strongly asymmetric working-day vs holiday schedules on repeated, identical
daily meteorology so that any diurnal or day-to-day difference comes purely
from calendar selection:

- Anthropogenic heat (``ahprof_24hr`` and ``qf_a``): weekday morning peak vs
  weekend evening peak, permutation non-invariance under ``working_day`` <->
  ``holiday`` swap, and calendar-date shift invariance.
- Modelled irrigation diurnal profiles (``wuprofa_24hr``, ``wuprofm_24hr``):
  disjoint weekday morning vs weekend evening water delivery, plus permutation
  non-invariance under swap.
- Weekly irrigation schedules (``daywat``, ``daywatper``): single-day watering
  is not permutation-invariant, scales linearly with ``daywatper`` when
  ponding demand is zero, and applies each named weekday on its own calendar
  day (regression for UMEP-dev/SUEWS#2049).
"""

from __future__ import annotations

import copy

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import (
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    strict_groupby_reduce,
    strict_max_abs,
    strict_mean,
    strict_sum,
    water_balance_residual_mm,
)
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.data_model.core.model import ForcingTimestampReference, WaterUseMethod

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-CLOCK", "C-BIND", "C-BALANCE"),
]

WATER_CLOSURE_TOLERANCE_MM = 1e-4
WEEKDAYS = (
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
)


def _hourly_window_profile(active_hours: set[int], weight: float) -> dict[str, float]:
    """Build a 24-hour SUEWS profile (keys '1'..'24') active on ``active_hours``."""
    return {
        str(hour): (weight if hour in active_hours else 0.0) for hour in range(1, 25)
    }


def _make_repeated_forcing(
    base_day: pd.DataFrame, start_date: str, n_days: int
) -> pd.DataFrame:
    """Repeat one 24-hour 5-minute forcing day across ``n_days`` calendar days.

    Uses SUEWS interval-end timestamps starting at ``start_date 00:05`` so each
    calendar day ``D`` spans ``(D 00:00, D+1 00:00]``.
    """
    start_ts = pd.Timestamp(start_date) + pd.Timedelta(minutes=5)
    index = pd.date_range(start_ts, periods=TIMESTEPS_PER_DAY * n_days, freq="300s")
    df_forcing = pd.concat(
        [base_day.reset_index(drop=True) for _ in range(n_days)],
        ignore_index=True,
    )
    df_forcing.index = index
    df_forcing["iy"] = index.year
    df_forcing["id"] = index.day_of_year
    df_forcing["it"] = index.hour
    df_forcing["imin"] = index.minute
    df_forcing["isec"] = index.second
    return df_forcing


def _run_config(
    config: SUEWSConfig, df_forcing: pd.DataFrame
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Execute ``config`` through ``SUEWSSimulation`` and check active finiteness."""
    sim = SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=df_forcing.index,
        forcing=sim.forcing.df,
        tstep=300,
    )
    return output.df, sim.state_init


def _interval_day(index: pd.DatetimeIndex) -> pd.Series:
    """Map interval-end timestamps to the calendar date of each interval."""
    # Interval-end stamp YYYY-MM-DD 00:00 closes the preceding calendar day.
    shifted = index - pd.Timedelta(seconds=1)
    return pd.Series(shifted.normalize(), index=index)


def _configure_modelled_irrigation(
    config: SUEWSConfig,
    *,
    daywat_enabled: dict[str, float],
    daywatper_values: dict[str, float],
    working_profile: dict[str, float],
    holiday_profile: dict[str, float],
) -> SUEWSConfig:
    """Return a deep copy of ``config`` with deterministic modelled irrigation."""
    cfg = copy.deepcopy(config)
    cfg.model.physics.water_use = WaterUseMethod.MODELLED
    props = cfg.sites[0].properties
    irr = props.irrigation
    # Sentinel -999 disables ponding demand (WUDay_P == 0) so daily water use
    # scales strictly proportionally with daywatper.
    irr.h_maintain.value = -999.0
    irr.faut.value = 0.5
    irr.ie_start.value = 1.0
    irr.ie_end.value = 366.0
    irr.internalwateruse_h.value = 0.0
    for day in WEEKDAYS:
        setattr(irr.daywat, day, float(daywat_enabled.get(day, 0.0)))
        setattr(irr.daywatper, day, float(daywatper_values.get(day, 0.0)))
    irr.wuprofa_24hr.working_day = dict(working_profile)
    irr.wuprofa_24hr.holiday = dict(holiday_profile)
    irr.wuprofm_24hr.working_day = dict(working_profile)
    irr.wuprofm_24hr.holiday = dict(holiday_profile)
    # Constant polynomial term (a0) lives on evetr.ie_a / evetr.ie_m.
    props.land_cover.evetr.ie_a.value = 4.0
    props.land_cover.evetr.ie_m.value = 4.0
    props.land_cover.dectr.ie_a.value = 0.0
    props.land_cover.dectr.ie_m.value = 0.0
    props.land_cover.grass.ie_a.value = 0.0
    props.land_cover.grass.ie_m.value = 0.0
    props.land_cover.grass.irrigation_fraction.value = 1.0
    props.land_cover.evetr.irrigation_fraction.value = 0.0
    props.land_cover.dectr.irrigation_fraction.value = 0.0
    return cfg


@pytest.fixture(scope="module")
def calendar_base(short_sample_yaml_path) -> tuple[SUEWSConfig, pd.DataFrame]:
    """Validated short sample config and one controlled dry forcing day."""
    sim = SUEWSSimulation(str(short_sample_yaml_path))
    base_day = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    base_day["rain"] = 0.0
    base_day["Tair"] = 18.0
    base_day["RH"] = 60.0
    base_day["U"] = 3.0
    return SUEWSConfig.from_yaml(str(short_sample_yaml_path)), base_day


def _build_qf_config(
    base_cfg: SUEWSConfig,
    wd_prof: dict[str, float],
    hol_prof: dict[str, float],
    wd_qfa: float,
    hol_qfa: float,
) -> SUEWSConfig:
    """Return a deep copy of ``base_cfg`` with asymmetric weekday/holiday QF inputs."""
    cfg = copy.deepcopy(base_cfg)
    heat = cfg.sites[0].properties.anthropogenic_emissions.heat
    heat.qf_a.working_day = wd_qfa
    heat.qf_a.holiday = hol_qfa
    heat.qf_b.working_day = 0.0
    heat.qf_b.holiday = 0.0
    heat.qf_c.working_day = 0.0
    heat.qf_c.holiday = 0.0
    heat.ahprof_24hr.working_day = dict(wd_prof)
    heat.ahprof_24hr.holiday = dict(hol_prof)
    return cfg


def _assert_diurnal_peak(
    ser_qf: pd.Series, date_str: str, *, expect_morning: bool
) -> None:
    """Assert that ``ser_qf`` on ``date_str`` peaks in morning (7..10) or evening (18..21)."""
    days = _interval_day(ser_qf.index)
    hours = (ser_qf.index - pd.Timedelta(seconds=1)).hour
    day_mask = days == pd.Timestamp(date_str)
    morning_slice = ser_qf[day_mask & (hours >= 7) & (hours <= 10)]
    evening_slice = ser_qf[day_mask & (hours >= 18) & (hours <= 21)]
    assert len(morning_slice) == 4 * 12
    assert len(evening_slice) == 4 * 12
    morning_mean = strict_mean(morning_slice, label=f"QF[{date_str}].morning")
    evening_mean = strict_mean(evening_slice, label=f"QF[{date_str}].evening")
    if expect_morning:
        assert morning_mean > 2.0 * evening_mean, (
            f"{date_str} should peak in morning (morning={morning_mean:.2f}, "
            f"evening={evening_mean:.2f})"
        )
    else:
        assert evening_mean > 2.0 * morning_mean, (
            f"{date_str} should peak in evening (morning={morning_mean:.2f}, "
            f"evening={evening_mean:.2f})"
        )


def _daily_totals_in_window(
    df_out: pd.DataFrame, col: str, start_date: str, end_date: str
) -> pd.Series:
    """Aggregate ``SUEWS.<col>`` by interval calendar date within ``[start_date, end_date]``."""
    ser = df_out.loc[1, ("SUEWS", col)]
    expected_days = pd.date_range(start_date, end_date, freq="D")
    totals = strict_groupby_reduce(
        ser,
        _interval_day(ser.index),
        agg="sum",
        expected_counts=dict.fromkeys(expected_days, TIMESTEPS_PER_DAY),
        label=f"SUEWS.{col}",
    )
    return totals.loc[pd.Timestamp(start_date) : pd.Timestamp(end_date)]


@pytest.mark.core
def test_asymmetric_weekday_holiday_qf_diurnal_profile(calendar_base):
    """Weekday and weekend anthropogenic profiles must produce distinct QF responses.

    Checks three falsifiable properties on identical repeated daily forcing:
    1. Working days (Fri, Mon) carry the morning-peaked high-amplitude schedule
       and weekend days (Sat, Sun) carry the evening-peaked low-amplitude one.
    2. Swapping ``working_day`` and ``holiday`` reverses both the diurnal peak
       window and daily mean ``QF`` (not invariant under permutation).
    3. Shifting the same forcing by +2 calendar days moves the weekend signature
       onto the newly aligned weekend day.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 2012-01-06 is Friday (wd, hol, hol, wd); 2012-01-08 is Sunday (hol, wd, wd, wd).
    df_forcing_fri = _make_repeated_forcing(base_day, "2012-01-06", n_days=4)

    morning_prof = {str(h): (2.5 if 8 <= h <= 11 else 0.5) for h in range(1, 25)}
    evening_prof = {str(h): (2.5 if 19 <= h <= 22 else 0.5) for h in range(1, 25)}

    cfg_base = _build_qf_config(base_cfg, morning_prof, evening_prof, 0.60, 0.25)
    cfg_swapped = _build_qf_config(base_cfg, evening_prof, morning_prof, 0.25, 0.60)

    # ACT
    run_base = _run_config(cfg_base, df_forcing_fri)
    run_swap = _run_config(cfg_swapped, df_forcing_fri)
    qf_shift = _run_config(
        cfg_base, _make_repeated_forcing(base_day, "2012-01-08", n_days=4)
    )[0].loc[1, ("SUEWS", "QF")]

    # ASSERT
    qf_base = run_base[0].loc[1, ("SUEWS", "QF")]
    qf_swap = run_swap[0].loc[1, ("SUEWS", "QF")]

    for date_str, expect_morning in (
        ("2012-01-06", True),
        ("2012-01-09", True),
        ("2012-01-07", False),
        ("2012-01-08", False),
    ):
        _assert_diurnal_peak(qf_base, date_str, expect_morning=expect_morning)
        _assert_diurnal_peak(qf_swap, date_str, expect_morning=not expect_morning)

    # Daily mean QF tracks qf_a (0.60 working_day vs 0.25 holiday) and swaps cleanly.
    expected_counts = dict.fromkeys(
        pd.date_range("2012-01-06", "2012-01-09", freq="D"), TIMESTEPS_PER_DAY
    )
    means_base = strict_groupby_reduce(
        qf_base,
        _interval_day(qf_base.index),
        agg="mean",
        expected_counts=expected_counts,
        label="QF.base",
    )
    means_swap = strict_groupby_reduce(
        qf_swap,
        _interval_day(qf_swap.index),
        agg="mean",
        expected_counts=expected_counts,
        label="QF.swap",
    )
    assert means_base[pd.Timestamp("2012-01-06")] > (
        1.8 * means_base[pd.Timestamp("2012-01-07")]
    )
    assert means_swap[pd.Timestamp("2012-01-07")] > (
        1.8 * means_swap[pd.Timestamp("2012-01-06")]
    )
    assert strict_max_abs(qf_swap - qf_base, label="dQF_swap") > 5.0

    # Calendar-date shift: 2012-01-08 (Sun) gets evening peak, 2012-01-09 (Mon) morning.
    _assert_diurnal_peak(qf_shift, "2012-01-08", expect_morning=False)
    _assert_diurnal_peak(qf_shift, "2012-01-09", expect_morning=True)

    assert (
        water_balance_residual_mm(*run_base, expected_timestamps=df_forcing_fri.index)
        < WATER_CLOSURE_TOLERANCE_MM
    )
    assert (
        water_balance_residual_mm(*run_swap, expected_timestamps=df_forcing_fri.index)
        < WATER_CLOSURE_TOLERANCE_MM
    )


def _assert_disjoint_delivery(
    suews: pd.DataFrame, date_str: str, col: str, *, expect_morning: bool
) -> None:
    """Assert irrigation ``col`` on ``date_str`` is positive in one window and zero in the other."""
    days = _interval_day(suews.index)
    hours = (suews.index - pd.Timedelta(seconds=1)).hour
    day_mask = days == pd.Timestamp(date_str)
    morning_vals = suews.loc[day_mask & (hours >= 6) & (hours <= 8), col].to_numpy()
    evening_vals = suews.loc[day_mask & (hours >= 17) & (hours <= 19), col].to_numpy()
    assert len(morning_vals) == 3 * 12
    assert len(evening_vals) == 3 * 12
    if expect_morning:
        assert strict_sum(morning_vals, label=f"{col}[{date_str}].morning") > 0.0
        np.testing.assert_array_equal(evening_vals, 0.0)
    else:
        assert strict_sum(evening_vals, label=f"{col}[{date_str}].evening") > 0.0
        np.testing.assert_array_equal(morning_vals, 0.0)


@pytest.mark.core
def test_asymmetric_weekday_holiday_irrigation_diurnal_profile(calendar_base):
    """Weekday and weekend water-use profiles must deliver water in distinct hours.

    Uses disjoint morning (working day) and evening (holiday) irrigation windows
    separated by multi-hour zero buffers, and verifies that swapping the two day
    types swaps delivery windows between weekdays and weekends.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # Thu 2012-01-05 warms up daily WUDay_id; Fri..Mon are evaluated.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=5)

    # Keys '7'..'9' cover 06:00-09:00; keys '18'..'20' cover 17:00-20:00.
    morning_wu = _hourly_window_profile({7, 8, 9}, weight=1.0 / 3.0)
    evening_wu = _hourly_window_profile({18, 19, 20}, weight=1.0 / 3.0)
    all_days = dict.fromkeys(WEEKDAYS, 1.0)

    cfg_base = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled=all_days,
        daywatper_values=all_days,
        working_profile=morning_wu,
        holiday_profile=evening_wu,
    )
    cfg_swap = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled=all_days,
        daywatper_values=all_days,
        working_profile=evening_wu,
        holiday_profile=morning_wu,
    )

    # ACT
    df_out_base, df_state_base = _run_config(cfg_base, df_forcing)
    df_out_swap, df_state_swap = _run_config(cfg_swap, df_forcing)

    # ASSERT
    suews_base = df_out_base.loc[1, "SUEWS"]
    suews_swap = df_out_swap.loc[1, "SUEWS"]

    for col in ("Irr", "WUGrass"):
        for wd_date in ("2012-01-06", "2012-01-09"):
            _assert_disjoint_delivery(suews_base, wd_date, col, expect_morning=True)
            _assert_disjoint_delivery(suews_swap, wd_date, col, expect_morning=False)
        for hol_date in ("2012-01-07", "2012-01-08"):
            _assert_disjoint_delivery(suews_base, hol_date, col, expect_morning=False)
            _assert_disjoint_delivery(suews_swap, hol_date, col, expect_morning=True)
        assert (
            strict_max_abs(suews_swap[col] - suews_base[col], label=f"d{col}_swap")
            > 1e-4
        )

    assert (
        water_balance_residual_mm(
            df_out_base, df_state_base, expected_timestamps=df_forcing.index
        )
        < WATER_CLOSURE_TOLERANCE_MM
    )
    assert (
        water_balance_residual_mm(
            df_out_swap, df_state_swap, expected_timestamps=df_forcing.index
        )
        < WATER_CLOSURE_TOLERANCE_MM
    )


@pytest.mark.core
def test_asymmetric_weekly_daywat_schedule_is_not_permutation_invariant(calendar_base):
    """Single-day ``daywat`` schedules must trigger one day per week and shift on permutation.

    Also verifies proportional scaling with ``daywatper`` when
    ``h_maintain=-999`` disables ponding demand.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 9 days: Thu 2012-01-05 (warm-up) + full 7-day cycle Sat 2012-01-07..Fri 2012-01-13.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=9)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)

    runs: dict[str, tuple[pd.DataFrame, pd.DataFrame]] = {}
    for label, day_name, frac in (
        ("wed", "wednesday", 1.0),
        ("fri", "friday", 1.0),
        ("wed_half", "wednesday", 0.5),
    ):
        cfg = _configure_modelled_irrigation(
            base_cfg,
            daywat_enabled={day_name: 1.0},
            daywatper_values={day_name: frac},
            working_profile=midday,
            holiday_profile=midday,
        )
        # ACT
        runs[label] = _run_config(cfg, df_forcing)

    # ASSERT
    irr_wed = _daily_totals_in_window(runs["wed"][0], "Irr", "2012-01-07", "2012-01-13")
    irr_fri = _daily_totals_in_window(runs["fri"][0], "Irr", "2012-01-07", "2012-01-13")
    irr_half = _daily_totals_in_window(
        runs["wed_half"][0], "Irr", "2012-01-07", "2012-01-13"
    )

    active_wed = irr_wed[irr_wed > 0.0]
    active_fri = irr_fri[irr_fri > 0.0]
    assert len(active_wed) == 1, f"Expected exactly 1 irrigated day, got {active_wed}"
    assert len(active_fri) == 1, f"Expected exactly 1 irrigated day, got {active_fri}"

    # Absolute delivery dates must match the configured named weekdays (gh#2381).
    assert active_wed.index[0] == pd.Timestamp("2012-01-11"), (
        f"Expected Wednesday delivery on 2012-01-11, got {active_wed.index[0].date()}"
    )
    assert active_fri.index[0] == pd.Timestamp("2012-01-13"), (
        f"Expected Friday delivery on 2012-01-13, got {active_fri.index[0].date()}"
    )
    assert strict_max_abs(irr_wed - irr_fri, label="dIrr_wed_fri") > 0.1

    # Halving daywatper halves daily irrigation delivery with zero ponding demand.
    np.testing.assert_allclose(
        irr_half.to_numpy(),
        0.5 * irr_wed.to_numpy(),
        rtol=1e-9,
        atol=1e-12,
    )
    assert (
        water_balance_residual_mm(*runs["wed"], expected_timestamps=df_forcing.index)
        < WATER_CLOSURE_TOLERANCE_MM
    )
    assert (
        water_balance_residual_mm(*runs["fri"], expected_timestamps=df_forcing.index)
        < WATER_CLOSURE_TOLERANCE_MM
    )


@pytest.mark.core
def test_weekly_daywat_dailystate_matches_named_weekday_gh2049(calendar_base):
    """Enabling any single named weekday must both record and deliver on that day.

    Regression for UMEP-dev/SUEWS#2049 (Monday/Sunday array packing) and
    UMEP-dev/SUEWS#2381 (one-day delivery lag from end-of-day ``update_WaterUse``):
    every named weekday ``sunday``..``saturday`` must produce positive
    ``DailyState.WU_Grass1`` and positive ``SUEWS.Irr`` / ``SUEWS.WUGrass`` on
    its own calendar day and exact zero on the other six days of the cycle.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 2012-01-05 (Thu) .. 2012-01-14 (Sat): contains a complete Sat..Fri cycle
    # covering every weekday exactly once in 2012-01-07..2012-01-13.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=10)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)
    cycle_days = pd.date_range("2012-01-07", "2012-01-13", freq="D")
    weekday_dates = (
        ("saturday", pd.Timestamp("2012-01-07")),
        ("sunday", pd.Timestamp("2012-01-08")),
        ("monday", pd.Timestamp("2012-01-09")),
        ("tuesday", pd.Timestamp("2012-01-10")),
        ("wednesday", pd.Timestamp("2012-01-11")),
        ("thursday", pd.Timestamp("2012-01-12")),
        ("friday", pd.Timestamp("2012-01-13")),
    )

    # ACT & ASSERT
    for label, active_day in weekday_dates:
        cfg = _configure_modelled_irrigation(
            base_cfg,
            daywat_enabled={label: 1.0},
            daywatper_values={label: 1.0},
            working_profile=midday,
            holiday_profile=midday,
        )
        df_out, _ = _run_config(cfg, df_forcing)

        ds_all = df_out.loc[1, "DailyState"]
        df_daily = ds_all.loc[expected_dailystate_mask(ds_all.index, tstep=300)]
        assert len(df_daily) == 10
        wu_by_day = strict_groupby_reduce(
            df_daily["WU_Grass1"],
            _interval_day(df_daily.index),
            agg="sum",
            label=f"DailyState.WU_Grass1[{label}]",
        )
        irr_by_day = _daily_totals_in_window(df_out, "Irr", "2012-01-07", "2012-01-13")
        wug_by_day = _daily_totals_in_window(
            df_out, "WUGrass", "2012-01-07", "2012-01-13"
        )

        assert float(wu_by_day.loc[active_day]) == pytest.approx(4.0, rel=1e-9), (
            f"Expected DailyState WU_Grass1=4.0 on {label} ({active_day.date()}), "
            f"got {wu_by_day.loc[cycle_days].to_dict()}"
        )
        assert float(irr_by_day.loc[active_day]) > 0.0, (
            f"Expected positive SUEWS.Irr on {label} ({active_day.date()}), "
            f"got {irr_by_day.to_dict()}"
        )
        assert float(wug_by_day.loc[active_day]) == pytest.approx(4.0, rel=1e-9), (
            f"Expected SUEWS.WUGrass=4.0 on {label} ({active_day.date()}), "
            f"got {wug_by_day.to_dict()}"
        )
        for other_day in cycle_days:
            if other_day == active_day:
                continue
            assert np.array_equal(float(wu_by_day.loc[other_day]), 0.0), (
                f"Expected exact zero DailyState WU_Grass1 on {other_day.date()} when only "
                f"{label} is enabled, got {wu_by_day.loc[cycle_days].to_dict()}"
            )
            assert np.array_equal(float(irr_by_day.loc[other_day]), 0.0), (
                f"Expected exact zero SUEWS.Irr on {other_day.date()} when only "
                f"{label} is enabled, got {irr_by_day.to_dict()}"
            )
            assert np.array_equal(float(wug_by_day.loc[other_day]), 0.0), (
                f"Expected exact zero SUEWS.WUGrass on {other_day.date()} when only "
                f"{label} is enabled, got {wug_by_day.to_dict()}"
            )


@pytest.mark.core
def test_irrigation_midnight_profiles_do_not_bleed_across_days_gh2381(calendar_base):
    """Hour-24 and hour-1 profiles must not leak across 23:55 or 00:00 boundaries.

    Regression for UMEP-dev/SUEWS#2381: updating ``WUDay_id`` at ``23:55`` before
    ``SUEWS_cal_WaterUse`` previously leaked the next day's budget onto ``D 23:55``
    and truncated ``D+1 23:55`` to zero.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=10)
    monday = pd.Timestamp("2012-01-09")
    cycle_days = pd.date_range("2012-01-07", "2012-01-13", freq="D")

    for profile_name, hour_key in (("hour24", 24), ("hour1", 1)):
        prof = _hourly_window_profile({hour_key}, weight=1.0)
        cfg = _configure_modelled_irrigation(
            base_cfg,
            daywat_enabled={"monday": 1.0},
            daywatper_values={"monday": 1.0},
            working_profile=prof,
            holiday_profile=prof,
        )

        # ACT
        df_out, _ = _run_config(cfg, df_forcing)

        # ASSERT
        suews = df_out.loc[1, "SUEWS"]
        wug_by_day = _daily_totals_in_window(
            df_out, "WUGrass", "2012-01-07", "2012-01-13"
        )
        assert float(wug_by_day.loc[monday]) == pytest.approx(4.0, rel=1e-9), (
            f"{profile_name}: expected full 4.0 mm on Monday, got {wug_by_day.to_dict()}"
        )
        for other_day in cycle_days:
            if other_day == monday:
                continue
            assert np.array_equal(float(wug_by_day.loc[other_day]), 0.0), (
                f"{profile_name}: expected exact zero WUGrass on {other_day.date()}, "
                f"got {wug_by_day.to_dict()}"
            )
        # No pre-midnight bleed onto Sunday 23:55 and no post-opening bleed onto Tuesday 00:05.
        assert np.array_equal(float(suews.loc["2012-01-08 23:55:00", "WUGrass"]), 0.0)
        assert np.array_equal(float(suews.loc["2012-01-10 00:05:00", "WUGrass"]), 0.0)
        # Monday 23:55 must remain active (not truncated) for both hour-24 and hour-1 ramps.
        assert float(suews.loc["2012-01-09 23:55:00", "WUGrass"]) > 0.0


@pytest.mark.core
def test_irrigation_asymmetric_sunday_monday_midnight_profile_gh2381(calendar_base):
    """Sunday's closing 00:00 interval must keep Sunday's holiday profile, not Monday's."""
    # ARRANGE
    base_cfg, base_day = calendar_base
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=10)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)
    hour1 = _hourly_window_profile({1}, weight=1.0)
    cfg = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"sunday": 1.0},
        daywatper_values={"sunday": 1.0},
        working_profile=midday,
        holiday_profile=hour1,
    )

    # ACT
    df_out, _ = _run_config(cfg, df_forcing)

    # ASSERT: full 4.0 mm delivered on Sunday (including closing Mon 00:00), 0 on Monday.
    suews = df_out.loc[1, "SUEWS"]
    wug = _daily_totals_in_window(df_out, "WUGrass", "2012-01-07", "2012-01-13")
    assert float(wug.loc[pd.Timestamp("2012-01-08")]) == pytest.approx(4.0, rel=1e-9)
    assert float(suews.loc["2012-01-09 00:00:00", "WUGrass"]) > 0.0
    assert np.array_equal(float(wug.loc[pd.Timestamp("2012-01-09")]), 0.0)


@pytest.mark.core
def test_irrigation_season_bounds_inclusive_nh_and_sh_gh2381(calendar_base):
    """``ie_start`` and ``ie_end`` must act as inclusive delivery DOY bounds in NH and SH.

    Regression for UMEP-dev/SUEWS#2381: legacy ``Ie_start - 1 .. Ie_end + 1`` in NH
    combined with the next-day delivery lag extended irrigation to ``ie_end + 2``,
    while SH ``.NOT. (id >= Ie_end .AND. id <= Ie_start)`` skipped ``ie_start`` and
    ``ie_start + 1``.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 2012-01-05 (DOY 5) .. 2012-01-14 (DOY 14)
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=10)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)
    all_days = dict.fromkeys(WEEKDAYS, 1.0)

    cases = (
        # (label, lat, ie_start, ie_end, expected_active_doys)
        ("nh_inclusive", 51.5, 10, 11, {10, 11}),
        ("nh_single_day", 51.5, 10, 10, {10}),
        ("equator_inclusive", 0.0, 10, 11, {10, 11}),
        ("nh_reversed_closed", 51.5, 12, 9, set()),
        ("sh_wrap_inclusive", -37.8, 10, 8, {5, 6, 7, 8, 10, 11, 12, 13, 14}),
        ("sh_within_year_inclusive", -37.8, 10, 11, {10, 11}),
        ("sh_single_day", -37.8, 10, 10, {10}),
    )

    for label, lat, ie_start, ie_end, expected_doys in cases:
        cfg = _configure_modelled_irrigation(
            base_cfg,
            daywat_enabled=all_days,
            daywatper_values=all_days,
            working_profile=midday,
            holiday_profile=midday,
        )
        cfg.sites[0].properties.lat.value = lat
        cfg.sites[0].properties.irrigation.ie_start.value = ie_start
        cfg.sites[0].properties.irrigation.ie_end.value = ie_end
        # Clear initial rain history (HDD_id(11)) so Day 1 is not manually suppressed.
        cfg.sites[0].initial_states.hdd_id.precip_daily_total = 0.0

        # ACT
        df_out, _ = _run_config(cfg, df_forcing)

        # ASSERT
        wug_by_day = _daily_totals_in_window(
            df_out, "WUGrass", "2012-01-05", "2012-01-14"
        )
        ds_all = df_out.loc[1, "DailyState"]
        df_daily = ds_all.loc[expected_dailystate_mask(ds_all.index, tstep=300)]
        ds_by_day = strict_groupby_reduce(
            df_daily["WU_Grass1"],
            _interval_day(df_daily.index),
            agg="sum",
            label=f"DailyState.WU_Grass1[{label}]",
        )
        for dt in wug_by_day.index:
            doy = int(dt.dayofyear)
            if doy in expected_doys:
                assert float(wug_by_day.loc[dt]) == pytest.approx(4.0, rel=1e-9), (
                    f"{label}: expected delivery on DOY {doy}, got {wug_by_day.to_dict()}"
                )
                assert float(ds_by_day.loc[dt]) == pytest.approx(4.0, rel=1e-9), (
                    f"{label}: expected DailyState on DOY {doy}, got {ds_by_day.to_dict()}"
                )
            else:
                assert np.array_equal(float(wug_by_day.loc[dt]), 0.0), (
                    f"{label}: expected zero delivery on DOY {doy}, got {wug_by_day.to_dict()}"
                )
                assert np.array_equal(float(ds_by_day.loc[dt]), 0.0), (
                    f"{label}: expected zero DailyState on DOY {doy}, got {ds_by_day.to_dict()}"
                )


@pytest.mark.core
def test_irrigation_cold_start_and_utc_local_clock_gh2381(calendar_base):
    """Day-1 cold starts and UTC forcing must irrigate on the named local-standard day."""
    # ARRANGE 1: cold start directly on Monday 2012-01-09 with daywat.monday=1
    base_cfg, base_day = calendar_base
    df_mon_start = _make_repeated_forcing(base_day, "2012-01-09", n_days=3)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)

    cfg_cold = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )
    # Include a non-zero temperature coefficient (ie_a2) so HDD_id(9) path is exercised.
    cfg_cold.sites[0].properties.land_cover.dectr.ie_a.value = 0.05
    cfg_cold.sites[0].properties.land_cover.dectr.ie_m.value = 0.05

    # ACT 1
    df_out_cold, _ = _run_config(cfg_cold, df_mon_start)

    # ASSERT 1: sample_config_short starts with HDD_id = [13.5] * 12, so Day 1
    # uses initial temp_avg=13.5 (giving auto = 0.5 * (4.0 + 0.05 * 13.5) = 2.3375)
    # while initial rain_prev=13.5 > 2 mm suppresses manual irrigation on Day 1.
    wug_cold = _daily_totals_in_window(
        df_out_cold, "WUGrass", "2012-01-09", "2012-01-11"
    )
    assert float(wug_cold.loc[pd.Timestamp("2012-01-09")]) == pytest.approx(
        0.5 * (4.0 + 0.05 * 13.5), rel=1e-9
    )
    assert np.array_equal(float(wug_cold.loc[pd.Timestamp("2012-01-10")]), 0.0)
    assert np.array_equal(float(wug_cold.loc[pd.Timestamp("2012-01-11")]), 0.0)

    # ARRANGE 2: UTC forcing with timezone=+8h (local standard Monday 2012-01-09
    # spans 2012-01-08 16:05 UTC .. 2012-01-09 16:00 UTC).
    df_utc = _make_repeated_forcing(base_day, "2012-01-07", n_days=4)
    cfg_utc = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )
    cfg_utc.model.control.forcing.timestamp_reference = ForcingTimestampReference.UTC
    cfg_utc.sites[0].properties.timezone.value = 8.0

    # ACT 2
    df_out_utc, _ = _run_config(cfg_utc, df_utc)

    # ASSERT 2: group by local standard interval day (index + 8h - 1s).
    suews_utc = df_out_utc.loc[1, "SUEWS"]
    lst_day = pd.Series(
        (suews_utc.index + pd.Timedelta(hours=8) - pd.Timedelta(seconds=1)).normalize(),
        index=suews_utc.index,
    )
    wug_lst = suews_utc["WUGrass"].groupby(lst_day).sum()
    assert float(wug_lst.loc[pd.Timestamp("2012-01-09")]) == pytest.approx(
        4.0, rel=1e-9
    )
    for dt in (
        pd.Timestamp("2012-01-08"),
        pd.Timestamp("2012-01-10"),
    ):
        assert np.array_equal(float(wug_lst.loc[dt]), 0.0), (
            f"UTC+8 expected zero WUGrass on local {dt.date()}, got {wug_lst.to_dict()}"
        )


def _assert_checkpoint_split_bit_identical(
    cfg_path,
    df_forcing: pd.DataFrame,
    df_ref: pd.DataFrame,
    split_idx: int,
    ckpt_path,
) -> None:
    """Resume from checkpoint at ``split_idx`` and verify bit-identical irrigation output."""
    part1 = df_forcing.iloc[:split_idx].copy()
    part2 = df_forcing.iloc[split_idx:].copy()

    sim1 = SUEWSSimulation()
    sim1.update_config(cfg_path, auto_load_forcing=False)
    sim1.update_forcing(part1)
    out1 = sim1.run(start_date=part1.index[0], end_date=part1.index[-1], n_jobs=1)

    sim2 = SUEWSSimulation.from_checkpoint(cfg_path, sim1.checkpoint.to_file(ckpt_path))
    sim2.update_forcing(part2)
    out2 = sim2.run(start_date=part2.index[0], end_date=part2.index[-1], n_jobs=1)

    combined = pd.concat([out1.df, out2.df])
    for grp, col in (
        ("SUEWS", "Irr"),
        ("SUEWS", "WUGrass"),
        ("DailyState", "WU_Grass1"),
    ):
        a = combined.loc[1, (grp, col)].to_numpy()
        b = df_ref.loc[1, (grp, col)].to_numpy()
        equal = (a.view(np.uint64) == b.view(np.uint64)) | (np.isnan(a) & np.isnan(b))
        assert equal.all(), (
            f"Checkpoint split at idx={split_idx} ({part1.index[-1]}) diverged on "
            f"{grp}.{col}"
        )


@pytest.mark.core
def test_irrigation_checkpoint_continuation_across_midnight_gh2381(
    calendar_base, tmp_path
):
    """Checkpoint resume across 23:55, 00:00, 00:05 and midday must stay bit-identical."""
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 2012-01-08 (Sun) .. 2012-01-10 (Tue): Monday 2012-01-09 is the sole irrigated day.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-08", n_days=3)
    mixed_prof = _hourly_window_profile({1, 12, 24}, weight=1.0 / 3.0)
    cfg = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=mixed_prof,
        holiday_profile=mixed_prof,
    )
    cfg_path = tmp_path / "irrig_ckpt_case.yml"
    cfg.to_yaml(cfg_path)

    sim_ref = SUEWSSimulation()
    sim_ref.update_config(cfg_path, auto_load_forcing=False)
    sim_ref.update_forcing(df_forcing.copy())
    df_ref = sim_ref.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    # ACT & ASSERT: split points immediately around Sunday->Monday midnight and midday:
    # 2012-01-08 23:55 (idx 286), 2012-01-09 00:00 (idx 287),
    # 2012-01-09 00:05 (idx 288), 2012-01-09 12:20 (idx 435).
    for split_idx in (287, 288, 289, 436):
        _assert_checkpoint_split_bit_identical(
            cfg_path, df_forcing, df_ref, split_idx, tmp_path / f"ckpt_{split_idx}.json"
        )


def _assert_irrigation_cols_bit_identical(
    actual: pd.DataFrame, expected: pd.DataFrame, label: str
) -> None:
    """Assert exact uint64 bit equality on irrigation and DailyState columns."""
    for grp, col in (
        ("SUEWS", "Irr"),
        ("SUEWS", "WUGrass"),
        ("DailyState", "WU_Grass1"),
    ):
        a = actual.loc[1, (grp, col)].to_numpy()
        b = expected.loc[1, (grp, col)].to_numpy()
        equal = (a.view(np.uint64) == b.view(np.uint64)) | (np.isnan(a) & np.isnan(b))
        assert equal.all(), f"{label} diverged on {grp}.{col}"


def _assert_stream_irrigation_bit_identical(
    cfg_path,
    df_forcing: pd.DataFrame,
    sim_ref: SUEWSSimulation,
    tmp_path,
    label: str,
) -> None:
    """Assert streamed parquet output matches uninterrupted parquet save bit-for-bit."""
    stream_dir = tmp_path / "stream_pond"
    stream_out = {
        "format": "parquet",
        "freq": 300,
        "dir": str(stream_dir),
        "groups": ["SUEWS", "DailyState"],
        "stream_chunk": 1,
    }
    sim_stream = SUEWSSimulation()
    sim_stream.update_config(cfg_path, auto_load_forcing=False)
    sim_stream.update_config(
        {"model": {"control": {"output": stream_out}}}, auto_load_forcing=False
    )
    sim_stream.update_forcing(df_forcing.copy())
    sim_stream.run(
        start_date=df_forcing.index[0], end_date=df_forcing.index[-1], n_jobs=1
    )
    (stream_pq,) = stream_dir.glob("*SUEWS_output*.parquet")

    ref_pq_dir = tmp_path / "ref_pond"
    ref_pq_dir.mkdir()
    sim_ref.config.model.control.output.freq = 300
    sim_ref.config.model.control.output.groups = ["SUEWS", "DailyState"]
    sim_ref.save(ref_pq_dir, format="parquet")
    (ref_pq,) = ref_pq_dir.glob("*SUEWS_output*.parquet")
    _assert_irrigation_cols_bit_identical(
        pd.read_parquet(stream_pq).sort_index(),
        pd.read_parquet(ref_pq).sort_index(),
        label,
    )


@pytest.mark.core
def test_irrigation_ponding_continuation_modes_gh2381(calendar_base, tmp_path):
    """Active ponding (h_maintain > 0) stays bit-identical across checkpoint, chunk, and stream."""
    base_cfg, base_day = calendar_base
    # 2012-01-08 (Sun) .. 2012-01-10 (Tue): Monday 2012-01-09 has active ponding demand.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-08", n_days=3)
    mixed_prof = _hourly_window_profile({1, 12, 24}, weight=1.0 / 3.0)
    cfg = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=mixed_prof,
        holiday_profile=mixed_prof,
    )
    cfg.sites[0].properties.irrigation.h_maintain.value = 5.0
    pond_path = tmp_path / "irrig_pond.yml"
    cfg.to_yaml(pond_path)

    sim_ref = SUEWSSimulation()
    sim_ref.update_config(pond_path, auto_load_forcing=False)
    sim_ref.update_forcing(df_forcing.copy())
    df_ref = sim_ref.run(
        start_date=df_forcing.index[0], end_date=df_forcing.index[-1], n_jobs=1
    ).df

    # Confirm ponding raised Monday demand above the 4.0 mm baseline and already
    # delivered water before the mid-Monday split (idx 436 -> 2012-01-09 12:20).
    wug = df_ref.loc[1, ("SUEWS", "WUGrass")]
    assert float(wug.sum()) > 4.0
    assert float(wug.iloc[288:436].sum()) > 1.0
    assert float(wug.iloc[436:576].sum()) > 1.0

    # 1. Checkpoint resume across midnight (288) and mid-Monday active delivery (436).
    for split_idx in (288, 436):
        _assert_checkpoint_split_bit_identical(
            pond_path,
            df_forcing,
            df_ref,
            split_idx,
            tmp_path / f"ckpt_pond_{split_idx}.json",
        )

    # 2. Chunked execution (chunk_day=1).
    sim_chunk = SUEWSSimulation()
    sim_chunk.update_config(pond_path, auto_load_forcing=False)
    sim_chunk.update_forcing(df_forcing.copy())
    df_chunk = sim_chunk.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
        chunk_day=1,
    ).df
    _assert_irrigation_cols_bit_identical(df_chunk, df_ref, "chunk_day=1 ponding")

    # 3. Streamed parquet execution (stream_chunk=1).
    _assert_stream_irrigation_bit_identical(
        pond_path, df_forcing, sim_ref, tmp_path, "stream_chunk=1 ponding"
    )


@pytest.mark.core
def test_irrigation_fractional_tz_hourly_midnight_straddle_gh2381(calendar_base):
    """Hourly UTC forcing in UTC+05:30 straddles local midnight at 00:30 local time.

    Under right-closed local-standard delivery days ``(D 00:00, D+1 00:00]``,
    the interval ending ``Mon 00:30`` (``Sun 19:00 UTC``) belongs to Monday,
    not Sunday. Enabling only Sunday must therefore stop delivering at
    ``Mon 00:30``, and enabling both Sunday (holiday) and Monday (working_day)
    with asymmetric profiles must switch profiles on ``Mon 00:30``.
    """
    base_cfg, base_day = calendar_base
    # 72 hourly UTC steps: 2012-01-07 19:00 UTC .. 2012-01-10 18:00 UTC
    # -> local UTC+05:30: 2012-01-08 00:30 (Sun) .. 2012-01-10 23:30 (Tue).
    df_hr = _make_repeated_forcing(base_day, "2012-01-07", n_days=4).iloc[11::12].copy()
    df_hr = df_hr.loc["2012-01-07 19:00:00":"2012-01-10 18:00:00"].copy()
    assert len(df_hr) == 72

    uniform_prof = {str(h): 1.0 / 24.0 for h in range(1, 25)}
    cfg = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"sunday": 1.0},
        daywatper_values={"sunday": 1.0},
        working_profile=uniform_prof,
        holiday_profile=uniform_prof,
    )
    cfg.model.control.tstep = 3600
    cfg.model.control.forcing.timestamp_reference = ForcingTimestampReference.UTC
    cfg.sites[0].properties.timezone.value = 5.5
    cfg.sites[0].initial_states.hdd_id.precip_daily_total = 0.0

    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_hr)
    wug = sim.run(start_date=df_hr.index[0], end_date=df_hr.index[-1], n_jobs=1).df.loc[
        1, ("SUEWS", "WUGrass")
    ]

    # Sun 23:30 IST (Sun 18:00 UTC) belongs to Sunday; Mon 00:30 IST (Sun 19:00 UTC)
    # straddles local midnight and belongs to Monday -> must be exact zero.
    assert float(wug.loc[pd.Timestamp("2012-01-08 18:00:00")]) > 0.0
    assert np.array_equal(float(wug.loc[pd.Timestamp("2012-01-08 19:00:00")]), 0.0)

    local_idx = df_hr.index + pd.Timedelta(hours=5, minutes=30)
    by_local = wug.groupby(_interval_day(local_idx).to_numpy()).sum()
    assert float(by_local.loc[pd.Timestamp("2012-01-08")]) == pytest.approx(
        4.0, rel=1e-9
    )
    assert np.array_equal(float(by_local.loc[pd.Timestamp("2012-01-09")]), 0.0)
    assert np.array_equal(float(by_local.loc[pd.Timestamp("2012-01-10")]), 0.0)

    # Also verify asymmetric Sunday (holiday) -> Monday (working_day) profile switch
    # on the midnight-straddling Mon 00:30 IST (Sun 19:00 UTC) step.
    cfg_asym = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"sunday": 1.0, "monday": 1.0},
        daywatper_values={"sunday": 1.0, "monday": 1.0},
        working_profile=_hourly_window_profile({1, 2}, weight=0.5),
        holiday_profile=_hourly_window_profile({12, 13}, weight=0.5),
    )
    cfg_asym.model.control.tstep = 3600
    cfg_asym.model.control.forcing.timestamp_reference = ForcingTimestampReference.UTC
    cfg_asym.sites[0].properties.timezone.value = 5.5
    cfg_asym.sites[0].initial_states.hdd_id.precip_daily_total = 0.0
    sim_asym = SUEWSSimulation()
    sim_asym.update_config(cfg_asym, auto_load_forcing=False)
    sim_asym.update_forcing(df_hr)
    wug_asym = sim_asym.run(
        start_date=df_hr.index[0], end_date=df_hr.index[-1], n_jobs=1
    ).df.loc[1, ("SUEWS", "WUGrass")]
    assert float(wug_asym.loc[pd.Timestamp("2012-01-08 19:00:00")]) == pytest.approx(
        2.0, rel=1e-9
    )


@pytest.mark.core
def test_irrigation_year_rollover_and_leap_day_gh2381(calendar_base):
    """Year rollover (leap->non-leap) and 29 Feb transitions keep exact delivery days."""
    base_cfg, base_day = calendar_base
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)
    all_days = dict.fromkeys(WEEKDAYS, 1.0)

    # 1. Year rollover 2012-12-30 (DOY 365) .. 2013-01-02 (DOY 2) with SH wrap [365..1]
    df_roll = _make_repeated_forcing(base_day, "2012-12-30", n_days=4)
    cfg_roll = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled=all_days,
        daywatper_values=all_days,
        working_profile=midday,
        holiday_profile=midday,
    )
    cfg_roll.sites[0].properties.lat.value = -37.8
    cfg_roll.sites[0].properties.irrigation.ie_start.value = 365
    cfg_roll.sites[0].properties.irrigation.ie_end.value = 1
    cfg_roll.sites[0].initial_states.hdd_id.precip_daily_total = 0.0
    wug_roll = _daily_totals_in_window(
        _run_config(cfg_roll, df_roll)[0], "WUGrass", "2012-12-30", "2013-01-02"
    )
    for active in ("2012-12-30", "2012-12-31", "2013-01-01"):
        assert float(wug_roll.loc[pd.Timestamp(active)]) == pytest.approx(4.0, rel=1e-9)
    assert np.array_equal(float(wug_roll.loc[pd.Timestamp("2013-01-02")]), 0.0)

    # 2. Leap-day transition: 2012-02-29 is Wednesday; only Wednesday enabled.
    df_leap = _make_repeated_forcing(base_day, "2012-02-28", n_days=3)
    cfg_leap = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"wednesday": 1.0},
        daywatper_values={"wednesday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )
    wug_leap = _daily_totals_in_window(
        _run_config(cfg_leap, df_leap)[0], "WUGrass", "2012-02-28", "2012-03-01"
    )
    assert float(wug_leap.loc[pd.Timestamp("2012-02-29")]) == pytest.approx(
        4.0, rel=1e-9
    )
    assert np.array_equal(float(wug_leap.loc[pd.Timestamp("2012-02-28")]), 0.0)
    assert np.array_equal(float(wug_leap.loc[pd.Timestamp("2012-03-01")]), 0.0)


@pytest.mark.core
def test_irrigation_hourly_tstep_matches_named_weekday_gh2381(calendar_base):
    """Hourly timestep (tstep = 3600 s) must deliver Monday-only irrigation on Monday."""
    base_cfg, base_day = calendar_base
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)
    df_hr = _make_repeated_forcing(base_day, "2012-01-08", n_days=3).iloc[11::12].copy()
    cfg_hr = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )
    cfg_hr.model.control.tstep = 3600
    sim_hr = SUEWSSimulation()
    sim_hr.update_config(cfg_hr, auto_load_forcing=False)
    sim_hr.update_forcing(df_hr)
    out_hr = sim_hr.run(start_date=df_hr.index[0], end_date=df_hr.index[-1], n_jobs=1).df
    wug_hr = out_hr.loc[1, ("SUEWS", "WUGrass")].groupby(_interval_day(df_hr.index)).sum()
    assert float(wug_hr.loc[pd.Timestamp("2012-01-09")]) == pytest.approx(4.0, rel=1e-9)
    assert np.array_equal(float(wug_hr.loc[pd.Timestamp("2012-01-08")]), 0.0)
    assert np.array_equal(float(wug_hr.loc[pd.Timestamp("2012-01-10")]), 0.0)
