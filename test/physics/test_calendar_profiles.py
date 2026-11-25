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
from supy.data_model.core.model import WaterUseMethod

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

    # Shifting enabled weekday from Wednesday to Friday shifts delivery by +2 days mod 7.
    day_offset = (active_fri.index[0] - active_wed.index[0]).days % 7
    assert day_offset == 2, (
        f"Wednesday -> Friday shift should move irrigation by 2 days mod 7, "
        f"got {active_wed.index[0].date()} -> {active_fri.index[0].date()}"
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
    """Enabling only ``monday`` must record DailyState water use on Monday, not Sunday.

    Regression for UMEP-dev/SUEWS#2049: ``update_WaterUse`` previously packed
    ``DayWat`` and ``DayWatPer`` Monday-first while indexing them with
    Sunday-first ``DayofWeek_id(1)`` (1=Sunday..7=Saturday), shifting every
    named weekday one day earlier.
    """
    # ARRANGE
    base_cfg, base_day = calendar_base
    # 2012-01-05 (Thu) .. 2012-01-14 (Sat): contains a complete Sun..Sat cycle
    # with Sunday 2012-01-08 and Monday 2012-01-09.
    df_forcing = _make_repeated_forcing(base_day, "2012-01-05", n_days=10)
    midday = _hourly_window_profile({11, 12, 13, 14}, weight=0.25)

    cfg_mon = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"monday": 1.0},
        daywatper_values={"monday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )
    cfg_sun = _configure_modelled_irrigation(
        base_cfg,
        daywat_enabled={"sunday": 1.0},
        daywatper_values={"sunday": 1.0},
        working_profile=midday,
        holiday_profile=midday,
    )

    # ACT
    df_out_mon, _ = _run_config(cfg_mon, df_forcing)
    df_out_sun, _ = _run_config(cfg_sun, df_forcing)

    # ASSERT
    for label, df_out, active_day in (
        ("monday", df_out_mon, pd.Timestamp("2012-01-09")),
        ("sunday", df_out_sun, pd.Timestamp("2012-01-08")),
    ):
        ds_all = df_out.loc[1, "DailyState"]
        df_daily = ds_all.loc[expected_dailystate_mask(ds_all.index, tstep=300)]
        assert len(df_daily) == 10
        wu_by_day = strict_groupby_reduce(
            df_daily["WU_Grass1"],
            _interval_day(df_daily.index),
            agg="sum",
            label=f"DailyState.WU_Grass1[{label}]",
        )
        cycle_days = pd.date_range("2012-01-07", "2012-01-13", freq="D")
        assert float(wu_by_day.loc[active_day]) > 0.0, (
            f"Expected positive DailyState WU_Grass1 on {label} ({active_day.date()}), "
            f"got {wu_by_day.loc[cycle_days].to_dict()}"
        )
        for other_day in cycle_days:
            if other_day == active_day:
                continue
            assert np.array_equal(float(wu_by_day.loc[other_day]), 0.0), (
                f"Expected exact zero DailyState WU_Grass1 on {other_day.date()} when only "
                f"{label} is enabled, got {wu_by_day.loc[cycle_days].to_dict()}"
            )
