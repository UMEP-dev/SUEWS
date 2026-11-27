"""Verify Fortran DailyState right-closed accumulation window ``(D 00:00, D+1 00:00]``.

Under SUEWS's ``closed="right", label="right"`` convention, every timestep
stamped ``t`` belongs to calendar day ``date(t - 1 s)`` (matching pandas
``resample(..., closed="right", label="right")`` and
``SUEWS_cal_interval_day_st``). On midnight-aligned grids, day ``D`` therefore
opens on ``D 00:00 + dt`` (``00:05`` at 5-minute resolution, ``01:00`` at
hourly resolution), accumulates through ``D+1 00:00``, and emits day ``D``'s
``DailyState`` row directly at ``D+1 00:00`` using day ``D``'s DOY and weekday;
on offset grids (e.g. ``00:02, 00:07, ..., 23:57``), day ``D`` opens at
``D 00:02`` and emits on ``D 23:57``, keeping ``DailyState`` and ``SUEWS``
aligned across every resample frequency.
"""

from __future__ import annotations

import copy
import importlib
import json

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import expected_dailystate_mask
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.suews_checkpoint import SUEWSCheckpoint

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-CLOCK", "C-RESTART"),
]


def _make_sim_and_forcing(
    start: str,
    n_steps: int,
    *,
    tstep: int = 300,
    rsl_level: str = "none",
) -> tuple[SUEWSSimulation, pd.DataFrame]:
    """Create a sample simulation and regular forcing slice starting at *start*."""
    sim_template = SUEWSSimulation.from_sample_data()
    df_base = sim_template.forcing.df
    cfg = copy.deepcopy(sim_template.config)
    cfg.model.control.tstep = tstep
    cfg.model.physics.roughness_sublayer_level = rsl_level
    sim = SUEWSSimulation(cfg)
    if tstep == 300:
        df_slice = df_base.iloc[:n_steps].copy()
    else:
        # Repeat/stride from sample forcing so values remain physically valid.
        stride = max(1, tstep // 300)
        df_slice = df_base.iloc[: n_steps * stride : stride].copy()
        assert len(df_slice) == n_steps

    idx = pd.date_range(start, periods=n_steps, freq=f"{tstep}s")
    df_slice.index = idx
    df_slice["iy"] = idx.year
    df_slice["id"] = idx.day_of_year
    df_slice["it"] = idx.hour
    df_slice["imin"] = idx.minute
    return sim, df_slice


def test_native_dailystate_accumulates_right_closed_window_and_emits_at_midnight():
    """Midnight step D+1 00:00 belongs to day D for P_day, Tmin/Tmax, DLHrs and HDD."""
    sim, df_forcing = _make_sim_and_forcing("2012-01-01 00:05", TIMESTEPS_PER_DAY * 3)
    # Use distinct weekday/weekend base temperatures and a large sentinel in initial hdd_id.
    cfg = copy.deepcopy(sim.config)
    cfg.sites[0].initial_states.hdd_id.temp_5day_accum = 99.0
    cfg.sites[0].initial_states.hdd_id.temp_5day_mean = 99.0
    sim = SUEWSSimulation(cfg)

    # Zero rain everywhere except distinct spikes at each closing midnight and mid-day.
    df_forcing["rain"] = 0.0
    df_forcing.loc["2012-01-01 12:00", "rain"] = 1.0
    df_forcing.loc["2012-01-02 00:00", "rain"] = 2.5  # belongs to Jan 1
    df_forcing.loc["2012-01-02 12:00", "rain"] = 0.5
    df_forcing.loc["2012-01-03 00:00", "rain"] = 4.0  # belongs to Jan 2
    df_forcing.loc["2012-01-04 00:00", "rain"] = 7.0  # belongs to Jan 3

    # Inject unique extremes at closing midnight so a misassigned midnight fails loudly.
    df_forcing["Tair"] = 10.0
    df_forcing.loc["2012-01-02 00:00", "Tair"] = 2.0   # min of Jan 1
    df_forcing.loc["2012-01-01 12:00", "Tair"] = 18.0  # max of Jan 1
    df_forcing.loc["2012-01-03 00:00", "Tair"] = 25.0  # max of Jan 2
    df_forcing.loc["2012-01-02 06:00", "Tair"] = 4.0   # min of Jan 2
    df_forcing.loc["2012-01-04 00:00", "Tair"] = -1.0  # min of Jan 3

    # Controlled kdown (> 10 W m-2 counts towards DLHrs) including closing midnight.
    df_forcing["kdown"] = 0.0
    df_forcing.loc["2012-01-01 12:05":"2012-01-01 13:00", "kdown"] = 50.0  # 12 steps = 1.0 h
    df_forcing.loc["2012-01-02 00:00", "kdown"] = 50.0  # +1 step -> 13/12 h on Jan 1
    df_forcing.loc["2012-01-03 00:00", "kdown"] = 50.0  # 1/12 h on Jan 2

    sim.update_forcing(df_forcing)
    output = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    df_grid = output.df.xs(1, level="grid")
    df_ds = df_grid["DailyState"].dropna(how="all")

    expected_stamps = pd.DatetimeIndex([
        "2012-01-02 00:00",
        "2012-01-03 00:00",
        "2012-01-04 00:00",
    ])
    assert df_ds.index.equals(expected_stamps)
    # 23:55 must remain NaN natively.
    assert df_grid["DailyState"].loc["2012-01-01 23:55"].isna().all()
    assert df_grid["DailyState"].loc["2012-01-02 23:55"].isna().all()

    expected_p = [3.5, 4.5, 7.0]
    expected_dlhrs = [13.0 / 12.0, 1.0 / 12.0, 0.0]
    expected_tmeans = []
    for i, ts_end in enumerate(expected_stamps):
        day_start = ts_end - pd.Timedelta(days=1)
        window = df_forcing.loc[(df_forcing.index > day_start) & (df_forcing.index <= ts_end)]
        assert len(window) == TIMESTEPS_PER_DAY
        row = df_ds.loc[ts_end]
        np.testing.assert_allclose(row["P_day"], expected_p[i], rtol=0, atol=1e-12)
        np.testing.assert_allclose(
            row["P_day"],
            df_grid.loc[window.index, ("SUEWS", "Rain")].sum(),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(row["Tmin"], window["Tair"].min(), rtol=0, atol=1e-12)
        np.testing.assert_allclose(row["Tmax"], window["Tair"].max(), rtol=0, atol=1e-12)
        np.testing.assert_allclose(row["HDD3_Tmean"], window["Tair"].mean(), rtol=1e-12)
        np.testing.assert_allclose(row["DLHrs"], expected_dlhrs[i], rtol=0, atol=1e-12)
        expected_tmeans.append(float(window["Tair"].mean()))

    # Day 1's 5-day running mean must equal Day 1's mean (not blended with initial 99.0).
    np.testing.assert_allclose(
        df_ds.loc["2012-01-02 00:00", "HDD4_T5d"], expected_tmeans[0], rtol=1e-12
    )
    np.testing.assert_allclose(
        df_ds.loc["2012-01-03 00:00", "HDD4_T5d"], np.mean(expected_tmeans[:2]), rtol=1e-12
    )
    np.testing.assert_allclose(
        df_ds.loc["2012-01-04 00:00", "HDD4_T5d"], np.mean(expected_tmeans[:3]), rtol=1e-12
    )


def test_closing_midnight_uses_accumulated_day_weekday_for_hdd_and_water_use():
    """Friday 23:55 -> Saturday 00:00 uses Friday's weekday for HDD and WUDay."""
    # 2012-01-06 is Friday (weekday); 2012-01-07 is Saturday (weekend).
    sim, df_forcing = _make_sim_and_forcing("2012-01-06 00:05", TIMESTEPS_PER_DAY * 2)
    cfg = copy.deepcopy(sim.config)
    cfg.model.physics.water_use = 0  # modelled

    # Distinct working-day vs weekend heating base temperatures.
    ah = cfg.sites[0].properties.anthropogenic_emissions.heat
    ah.baset_heating.working_day = 20.0
    ah.baset_heating.holiday = 10.0

    # Permit irrigation ONLY on Friday so Saturday 00:00 (closing Friday) irrigates
    # while Sunday 00:00 (closing Saturday) does not.
    props = cfg.sites[0].properties
    irr = props.irrigation
    irr.ie_start.value = 1
    irr.ie_end.value = 366
    irr.faut.value = 1.0
    props.land_cover.evetr.ie_a.value = 5.0
    props.land_cover.dectr.ie_a.value = 0.0
    props.land_cover.grass.ie_a.value = 0.0
    props.land_cover.evetr.ie_m.value = 0.0
    props.land_cover.dectr.ie_m.value = 0.0
    props.land_cover.grass.ie_m.value = 0.0
    irr.h_maintain.value = -999.0
    for day_name in ("sunday", "monday", "tuesday", "wednesday", "thursday", "saturday"):
        setattr(irr.daywat, day_name, 0.0)
        setattr(irr.daywatper, day_name, 0.0)
    irr.daywat.friday = 1.0
    irr.daywatper.friday = 1.0

    sim = SUEWSSimulation(cfg)
    df_forcing["Tair"] = 15.0
    df_forcing["rain"] = 0.0
    # Make Friday's closing midnight cold (5 degC) so working (20-5=15) vs holiday (10-5=5)
    # produces a clear signature in HDD1_h.
    df_forcing.loc["2012-01-07 00:00", "Tair"] = 5.0

    sim.update_forcing(df_forcing)
    output = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    df_ds = output.df.xs(1, level="grid")["DailyState"].dropna(how="all")

    assert df_ds.index.equals(
        pd.DatetimeIndex(["2012-01-07 00:00", "2012-01-08 00:00"])
    )

    # Friday (closing at Saturday 00:00): 287 steps at (20 - 15) + 1 step at (20 - 5).
    expected_friday_hdd = (287 * 5.0 + 15.0) / 288.0
    np.testing.assert_allclose(
        df_ds.loc["2012-01-07 00:00", "HDD1_h"], expected_friday_hdd, rtol=1e-12
    )
    # Saturday (closing at Sunday 00:00): all 288 steps at T=15 > holiday base_t=10 -> 0.
    np.testing.assert_allclose(
        df_ds.loc["2012-01-08 00:00", "HDD1_h"], 0.0, rtol=0, atol=1e-12
    )

    # Irrigation on Friday (closing Saturday 00:00) is active; Saturday is zero.
    assert df_ds.loc["2012-01-07 00:00", "WU_Grass1"] == pytest.approx(5.0, rel=1e-9)
    assert df_ds.loc["2012-01-08 00:00", "WU_Grass1"] == pytest.approx(0.0, abs=1e-12)


@pytest.mark.parametrize(
    ("start", "close_day1", "close_day2", "ie_start", "ie_end", "allowed_weekday"),
    [
        # Non-leap year-end: 2011-12-31 is DOY 365 (Saturday); 2012-01-01 is DOY 1 (Sunday).
        ("2011-12-31 00:05", "2012-01-01 00:00", "2012-01-02 00:00", 365, 365, "saturday"),
        # Leap year-end: 2012-12-31 is DOY 366 (Monday); 2013-01-01 is DOY 1 (Tuesday).
        ("2012-12-31 00:05", "2013-01-01 00:00", "2013-01-02 00:00", 366, 366, "monday"),
        # Leap day: 2012-02-29 is DOY 60 (Wednesday); 2012-03-01 is DOY 61 (Thursday).
        ("2012-02-29 00:05", "2012-03-01 00:00", "2012-03-02 00:00", 60, 60, "wednesday"),
    ],
    ids=["non-leap-year-end-doy365", "leap-year-end-doy366", "leap-day-doy60"],
)
def test_year_end_and_leap_day_doy_and_weekday_at_closing_midnight(
    start, close_day1, close_day2, ie_start, ie_end, allowed_weekday
):
    """Year-end (DOY 365/366) and leap-day (DOY 60) closings use day D's DOY and weekday."""
    sim, df_forcing = _make_sim_and_forcing(start, TIMESTEPS_PER_DAY * 2)
    cfg = copy.deepcopy(sim.config)
    cfg.model.physics.water_use = 0
    cfg.model.physics.laimethod = 1

    props = cfg.sites[0].properties
    irr = props.irrigation
    # Inclusive NH irrigation season gate [ie_start, ie_end] (gh#2381):
    # (365, 365) -> DOY 365 only, (366, 366) -> DOY 366 only, (60, 60) -> DOY 60 only.
    irr.ie_start.value = ie_start
    irr.ie_end.value = ie_end
    irr.faut.value = 1.0
    props.land_cover.evetr.ie_a.value = 4.0
    props.land_cover.dectr.ie_a.value = 0.0
    props.land_cover.grass.ie_a.value = 0.0
    props.land_cover.evetr.ie_m.value = 0.0
    props.land_cover.dectr.ie_m.value = 0.0
    props.land_cover.grass.ie_m.value = 0.0
    irr.h_maintain.value = -999.0
    for day_name in (
        "sunday",
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
    ):
        flag = 1.0 if day_name == allowed_weekday else 0.0
        setattr(irr.daywat, day_name, flag)
        setattr(irr.daywatper, day_name, flag)

    sim = SUEWSSimulation(cfg)
    df_forcing["rain"] = 0.0
    df_forcing["Tair"] = 12.0
    sim.update_forcing(df_forcing)
    output = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    df_ds = output.df.xs(1, level="grid")["DailyState"].dropna(how="all")

    assert df_ds.index.equals(pd.DatetimeIndex([close_day1, close_day2]))
    assert df_ds.loc[close_day1, "WU_Grass1"] == pytest.approx(4.0, rel=1e-9)
    assert df_ds.loc[close_day2, "WU_Grass1"] == pytest.approx(0.0, abs=1e-12)
    assert np.isfinite(df_ds[["LAI_EveTr", "LAI_DecTr", "LAI_Grass"]].to_numpy()).all()


def test_partial_day_start_at_midnight_and_midday():
    """Starting at 00:00 (1-step closing midnight) and 12:00 (145 steps) avoids NaN/bias."""
    # Case A: Start at 2012-01-01 00:00 (1 step closing 2011-12-31) + full 2012-01-01.
    sim_a, df_a = _make_sim_and_forcing("2012-01-01 00:00", TIMESTEPS_PER_DAY + 1)
    df_a["rain"] = 0.0
    df_a.iloc[0, df_a.columns.get_loc("rain")] = 1.25
    df_a.iloc[-1, df_a.columns.get_loc("rain")] = 3.75
    df_a["Tair"] = np.linspace(5.0, 15.0, len(df_a))
    df_a.iloc[0, df_a.columns.get_loc("Tair")] = -4.0  # isolated to 2011-12-31

    sim_a.update_forcing(df_a)
    out_a = sim_a.run(start_date=df_a.index[0], end_date=df_a.index[-1])
    ds_a = out_a.df.xs(1, level="grid")["DailyState"].dropna(how="all")

    assert ds_a.index.equals(
        pd.DatetimeIndex(["2012-01-01 00:00", "2012-01-02 00:00"])
    )
    # 1-step partial closing day at 2012-01-01 00:00: finite, tstepcount == 1.
    row0 = ds_a.loc["2012-01-01 00:00"]
    assert np.isfinite(row0[["HDD1_h", "HDD2_c", "HDD3_Tmean", "Tmin", "Tmax", "P_day"]]).all()
    np.testing.assert_allclose(row0["HDD3_Tmean"], -4.0, rtol=1e-12)
    np.testing.assert_allclose(row0["Tmin"], -4.0, rtol=1e-12)
    np.testing.assert_allclose(row0["Tmax"], -4.0, rtol=1e-12)
    np.testing.assert_allclose(row0["P_day"], 1.25, rtol=1e-12)

    # Full 288-step day 2012-01-01 (00:05..00:00) must NOT include 2012-01-01 00:00.
    day1_forcing = df_a.iloc[1:]
    row1 = ds_a.loc["2012-01-02 00:00"]
    np.testing.assert_allclose(row1["P_day"], 3.75, rtol=1e-12)
    np.testing.assert_allclose(row1["Tmin"], day1_forcing["Tair"].min(), rtol=1e-12)
    np.testing.assert_allclose(row1["Tmax"], day1_forcing["Tair"].max(), rtol=1e-12)
    np.testing.assert_allclose(row1["HDD3_Tmean"], day1_forcing["Tair"].mean(), rtol=1e-12)

    # Case B: Start mid-day at 2012-01-01 12:00 (145 steps to 2012-01-02 00:00) + full day 2.
    n_partial = 12 * 12 + 1  # 12:00 .. 00:00 inclusive = 145 steps
    sim_b, df_b = _make_sim_and_forcing("2012-01-01 12:00", n_partial + TIMESTEPS_PER_DAY)
    df_b["Tair"] = np.linspace(2.0, 20.0, len(df_b))
    df_b["rain"] = 0.1
    sim_b.update_forcing(df_b)
    out_b = sim_b.run(start_date=df_b.index[0], end_date=df_b.index[-1])
    ds_b = out_b.df.xs(1, level="grid")["DailyState"].dropna(how="all")

    assert ds_b.index.equals(
        pd.DatetimeIndex(["2012-01-02 00:00", "2012-01-03 00:00"])
    )
    w1 = df_b.iloc[:n_partial]
    w2 = df_b.iloc[n_partial:]
    np.testing.assert_allclose(ds_b.loc["2012-01-02 00:00", "HDD3_Tmean"], w1["Tair"].mean(), rtol=1e-12)
    np.testing.assert_allclose(ds_b.loc["2012-01-02 00:00", "P_day"], w1["rain"].sum(), rtol=1e-12)
    np.testing.assert_allclose(ds_b.loc["2012-01-03 00:00", "HDD3_Tmean"], w2["Tair"].mean(), rtol=1e-12)
    np.testing.assert_allclose(ds_b.loc["2012-01-03 00:00", "P_day"], w2["rain"].sum(), rtol=1e-12)


def test_hourly_timestep_right_closed_dailystate():
    """Hourly (3600 s) DailyState accumulates 01:00..00:00 (24 steps) and emits at 00:00."""
    sim, df_forcing = _make_sim_and_forcing("2012-01-01 01:00", 24 * 3, tstep=3600)
    df_forcing["Tair"] = np.linspace(3.0, 18.0, len(df_forcing))
    df_forcing["rain"] = 0.0
    df_forcing.loc["2012-01-02 00:00", "rain"] = 2.0
    df_forcing.loc["2012-01-03 00:00", "rain"] = 5.0
    df_forcing.loc["2012-01-04 00:00", "rain"] = 9.0
    df_forcing["kdown"] = 0.0
    df_forcing.loc["2012-01-01 10:00":"2012-01-01 15:00", "kdown"] = 80.0  # 6 hours

    sim.update_forcing(df_forcing)
    output = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    ds = output.df.xs(1, level="grid")["DailyState"].dropna(how="all")

    expected_stamps = pd.DatetimeIndex([
        "2012-01-02 00:00",
        "2012-01-03 00:00",
        "2012-01-04 00:00",
    ])
    assert ds.index.equals(expected_stamps)
    np.testing.assert_allclose(ds["P_day"].to_numpy(), [2.0, 5.0, 9.0], rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc["2012-01-02 00:00", "DLHrs"], 6.0, rtol=0, atol=1e-12)
    for ts_end in expected_stamps:
        day_start = ts_end - pd.Timedelta(days=1)
        w = df_forcing.loc[(df_forcing.index > day_start) & (df_forcing.index <= ts_end)]
        assert len(w) == 24
        np.testing.assert_allclose(ds.loc[ts_end, "HDD3_Tmean"], w["Tair"].mean(), rtol=1e-12)
        np.testing.assert_allclose(ds.loc[ts_end, "Tmin"], w["Tair"].min(), rtol=1e-12)
        np.testing.assert_allclose(ds.loc[ts_end, "Tmax"], w["Tair"].max(), rtol=1e-12)


@pytest.fixture(scope="module")
def _checkpoint_reference_run():
    """Shared uninterrupted 3-day run reused across checkpoint split parametrisation."""
    sim_full, df_forcing = _make_sim_and_forcing(
        "2012-01-01 00:05", TIMESTEPS_PER_DAY * 3, rsl_level="basic"
    )
    sim_full.update_forcing(df_forcing)
    out_full = sim_full.run(
        start_date=df_forcing.index[0], end_date=df_forcing.index[-1]
    )
    return sim_full.config, df_forcing, out_full


@pytest.mark.parametrize(
    "split_ts",
    ["2012-01-02 00:00", "2012-01-02 12:00"],
    ids=["split-at-closing-midnight", "split-midday"],
)
def test_checkpoint_restart_across_midnight_and_midday_is_bit_identical(
    tmp_path, _checkpoint_reference_run, split_ts
):
    """Splitting a run at closing midnight or mid-day reproduces an unsplit run bit-for-bit."""
    base_config, df_forcing, out_full = _checkpoint_reference_run

    split = pd.Timestamp(split_ts)
    df_part1 = df_forcing.loc[:split].copy()
    df_part2 = df_forcing.loc[df_forcing.index > split].copy()

    sim_p1 = SUEWSSimulation(copy.deepcopy(base_config))
    sim_p1.update_forcing(df_part1)
    out_p1 = sim_p1.run(start_date=df_part1.index[0], end_date=df_part1.index[-1])

    ckpt_path = tmp_path / "restart.ckpt.json"
    out_p1.checkpoint.to_file(ckpt_path)

    sim_p2 = SUEWSSimulation.from_checkpoint(copy.deepcopy(base_config), ckpt_path)
    sim_p2.update_forcing(df_part2)
    out_p2 = sim_p2.run(start_date=df_part2.index[0], end_date=df_part2.index[-1])

    combined = pd.concat([out_p1.df, out_p2.df]).sort_index()
    for group in ("SUEWS", "DailyState"):
        np.testing.assert_allclose(
            combined[group].to_numpy(dtype=float),
            out_full.df[group].to_numpy(dtype=float),
            rtol=0.0,
            atol=0.0,
            equal_nan=True,
            err_msg=f"{group} diverged across checkpoint split at {split_ts}",
        )


@pytest.fixture(scope="module")
def _offset_grid_run():
    """Shared 3-day offset-grid (00:02, 00:07, ..., 23:57) run crossing Jan 31 -> Feb 1."""
    # Under the unified right-closed day-ownership rule date(t - 1 s), each day D
    # on a 5-minute +2 min offset grid spans D 00:02 .. D 23:57 (288 steps),
    # matching pandas resample(..., closed="right", label="right") across all groups:
    # day 1 (2012-01-30): 2012-01-30 00:02 .. 2012-01-30 23:57 (hot & sunny);
    # day 2 (2012-01-31): 2012-01-31 00:02 .. 2012-01-31 23:57 (cool & dark);
    # day 3 (2012-02-01): 2012-02-01 00:02 .. 2012-02-01 23:57 (mild, crosses month).
    n_steps = TIMESTEPS_PER_DAY * 3
    sim, df_forcing = _make_sim_and_forcing("2012-01-30 00:02", n_steps, tstep=300)
    idx = df_forcing.index

    day1 = idx[:TIMESTEPS_PER_DAY]
    day2 = idx[TIMESTEPS_PER_DAY : TIMESTEPS_PER_DAY * 2]
    day3 = idx[TIMESTEPS_PER_DAY * 2 :]

    # Day 1 (Jan 30): hot (25..32 degC), 10 daylight steps, rain at both 00:02 and 23:57
    df_forcing.loc[day1, "Tair"] = np.linspace(25.0, 32.0, len(day1))
    df_forcing.loc[day1, "rain"] = 0.0
    df_forcing.loc[day1[0], "rain"] = 1.0
    df_forcing.loc[day1[-1], "rain"] = 5.0
    df_forcing.loc[day1, "kdown"] = 0.0
    df_forcing.loc[day1[100:110], "kdown"] = 120.0

    # Day 2 (Jan 31): cool (4..8 degC), 4 daylight steps, rain at 00:02 and 23:57
    df_forcing.loc[day2, "Tair"] = np.linspace(4.0, 8.0, len(day2))
    df_forcing.loc[day2, "rain"] = 0.0
    df_forcing.loc[day2[0], "rain"] = 0.4
    df_forcing.loc[day2[-1], "rain"] = 1.1
    df_forcing.loc[day2, "kdown"] = 0.0
    df_forcing.loc[day2[120:124], "kdown"] = 80.0

    # Day 3 (Feb 1): mild (12..16 degC), 2 daylight steps, rain at midnight-straddling 00:02
    df_forcing.loc[day3, "Tair"] = np.linspace(12.0, 16.0, len(day3))
    df_forcing.loc[day3, "rain"] = 0.0
    df_forcing.loc[day3[0], "rain"] = 3.25
    df_forcing.loc[day3, "kdown"] = 0.0
    df_forcing.loc[day3[130:132], "kdown"] = 90.0

    sim.update_forcing(df_forcing)
    output = sim.run(start_date=idx[0], end_date=idx[-1])
    return output, df_forcing, (day1, day2, day3)


def test_offset_timestep_grid_resets_dailystate_each_day(_offset_grid_run):
    """Regular offset grids (e.g. 00:02..23:57) reset DailyState cleanly each day."""
    output, df_forcing, (day1, day2, day3) = _offset_grid_run
    df_grid = output.df.xs(1, level="grid")
    ds = df_grid["DailyState"].dropna(how="all")

    assert ds.index.equals(pd.DatetimeIndex([day1[-1], day2[-1], day3[-1]]))
    # Shared layer-5 DailyState emission oracle must match offset-grid output too.
    oracle_mask = expected_dailystate_mask(df_grid.index, 300)
    assert df_grid.index[oracle_mask].equals(ds.index)

    # Day 1 must match its 288-step window (00:02..23:57)
    np.testing.assert_allclose(ds.loc[day1[-1], "Tmin"], 25.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day1[-1], "Tmax"], 32.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(
        ds.loc[day1[-1], "HDD3_Tmean"], df_forcing.loc[day1, "Tair"].mean(), rtol=1e-12
    )
    np.testing.assert_allclose(ds.loc[day1[-1], "P_day"], 6.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day1[-1], "DLHrs"], 10.0 / 12.0, rtol=0, atol=1e-12)

    # Day 2 must reset cleanly with zero carryover from hot Day 1
    np.testing.assert_allclose(ds.loc[day2[-1], "Tmin"], 4.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day2[-1], "Tmax"], 8.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(
        ds.loc[day2[-1], "HDD3_Tmean"], df_forcing.loc[day2, "Tair"].mean(), rtol=1e-12
    )
    np.testing.assert_allclose(ds.loc[day2[-1], "P_day"], 1.5, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day2[-1], "DLHrs"], 4.0 / 12.0, rtol=0, atol=1e-12)

    # Day 3 (Feb 1) must capture midnight-straddling 00:02 rain cleanly
    np.testing.assert_allclose(ds.loc[day3[-1], "Tmin"], 12.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day3[-1], "Tmax"], 16.0, rtol=0, atol=1e-12)
    np.testing.assert_allclose(ds.loc[day3[-1], "P_day"], 3.25, rtol=0, atol=1e-12)

    # Daily coverage metadata must align with offset-grid day ownership
    cov = output.df.attrs["supy_output_daily_coverage_s"]
    assert cov[f"1|{day1[-1].isoformat()}"] == pytest.approx(86400.0)
    assert cov[f"1|{day2[-1].isoformat()}"] == pytest.approx(86400.0)
    assert cov[f"1|{day3[-1].isoformat()}"] == pytest.approx(86400.0)


def test_offset_timestep_grid_resamples_and_saves_consistently_across_groups(
    tmp_path, _offset_grid_run
):
    """Offset-grid DailyState.P_day and SUEWS.Rain agree on 1D, 1ME, and saved output."""
    output, _, _ = _offset_grid_run

    # Daily resampling must place Jan 30, Jan 31, Feb 01 on closing midnights
    # (Jan 31, Feb 01, Feb 02 00:00) and keep DailyState.P_day == SUEWS.Rain.
    daily = output.resample("1D").df.xs(1, level="grid")
    daily_ds = daily["DailyState"].dropna(how="all")
    expected_daily = pd.DatetimeIndex([
        "2012-01-31 00:00",
        "2012-02-01 00:00",
        "2012-02-02 00:00",
    ])
    assert daily_ds.index.equals(expected_daily)
    assert daily["SUEWS"].index.equals(expected_daily)
    np.testing.assert_allclose(
        daily_ds["P_day"].to_numpy(), [6.0, 1.5, 3.25], rtol=0, atol=1e-12
    )
    np.testing.assert_allclose(
        daily["SUEWS", "Rain"].to_numpy(),
        daily_ds["P_day"].to_numpy(),
        rtol=0,
        atol=1e-12,
    )
    np.testing.assert_allclose(
        daily_ds["Tmax"].to_numpy(), [32.0, 8.0, 16.0], rtol=0, atol=1e-12
    )

    # Monthly resampling must keep Jan 30-31 in January and Feb 01 (including
    # midnight-straddling 2012-02-01 00:02) in February across both groups.
    monthly = output.resample("1ME").df.xs(1, level="grid")
    monthly_ds = monthly["DailyState"].dropna(how="all")
    expected_monthly = pd.DatetimeIndex(["2012-01-31", "2012-02-29"], freq="ME")
    assert monthly_ds.index.equals(expected_monthly)
    np.testing.assert_allclose(
        monthly_ds["P_day"].to_numpy(), [7.5, 3.25], rtol=0, atol=1e-12
    )
    np.testing.assert_allclose(
        monthly["SUEWS", "Rain"].to_numpy(),
        monthly_ds["P_day"].to_numpy(),
        rtol=0,
        atol=1e-12,
    )

    # Daily text output must also carry the aligned Jan 31..Feb 02 00:00 rows.
    saved = output.save(
        tmp_path, format="txt", freq_s=3600, groups=["SUEWS", "DailyState"]
    )
    ds_files = [p for p in saved if p.name.endswith("_DailyState.txt")]
    assert len(ds_files) == 1
    df_txt = pd.read_csv(ds_files[0], sep=r"\s+")
    assert list(df_txt["DOY"]) == [31, 32, 33]
    assert list(df_txt["Hour"]) == [0, 0, 0]
    assert list(df_txt["Min"]) == [0, 0, 0]
    np.testing.assert_allclose(
        df_txt["P_day"].to_numpy(), [6.0, 1.5, 3.25], rtol=0, atol=1e-4
    )


def test_legacy_v2_checkpoint_rejected_at_python_and_rust_boundaries(tmp_path):
    """Pre-migration schema-v2 checkpoints are refused by both Python and Rust."""
    sim, df_forcing = _make_sim_and_forcing("2012-01-01 00:05", TIMESTEPS_PER_DAY)
    sim.update_forcing(df_forcing.iloc[:-1])
    out = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-2])

    assert out.checkpoint.checkpoint_schema_version == 3

    downgraded_states = {}
    for grid_id, state_json in out.checkpoint.grid_states.items():
        payload = json.loads(state_json)
        payload["checkpoint_schema_version"] = 2
        downgraded_states[grid_id] = json.dumps(payload)

    legacy_ckpt = SUEWSCheckpoint.from_grid_states(
        downgraded_states,
        last_timestamp=out.checkpoint.last_timestamp,
    )
    with pytest.raises(ValueError, match=r"schema version 2.*DailyState.*version 3"):
        SUEWSSimulation.from_checkpoint(sim.config, legacy_ckpt)

    rust_module = importlib.import_module("supy._run_rust")
    bridge = rust_module._check_rust_available()
    cfg_path = tmp_path / "config.yml"
    sim.config.to_yaml(cfg_path)
    yaml_str = cfg_path.read_text(encoding="utf-8")
    # Direct Rust boundary call with v2 JSON must also fail with version mismatch
    with pytest.raises(
        RuntimeError, match=r"unsupported checkpoint schema version 2; expected version 3"
    ):
        bridge.run_suews_with_state(
            yaml_str,
            [0.0] * 32,
            1,
            next(iter(downgraded_states.values())),
        )
