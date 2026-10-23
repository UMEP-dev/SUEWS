"""Daily Tmin/Tmax come from the temperature DailyState uses all day (gh#1882).

With ``roughness_sublayer_level`` basic, DailyState takes its air
temperature from the T2 diagnostic. At the first step of each day the
running minimum and maximum used to be reset to the forcing temperature
instead, so on any day whose forcing temperature at 00:00 lay outside the
day's T2 range, Tmin (or Tmax) was the forcing value rather than a T2
value. GDD and SDD, and through them LAI, inherit that bias.

DailyState runs before the RSL profile in each step, so the T2 it sees at
step t is the value diagnosed at step t-1: the comparison below lags the
``T2`` output by one step.

The window starts on 15 January 2012 so that it holds 18 January, a day on
which the forcing temperature at 00:00 (5.17 degC) is below every T2 value
DailyState sees that day; the guard asserts at least one such day, so the
test cannot pass vacuously on a quiet window.
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics

START = pd.Timestamp("2012-01-15 00:05")
N_DAYS = 5


@pytest.mark.core
def test_daily_extremes_come_from_the_diagnosed_temperature():
    # ARRANGE
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config({"model": {"physics": {"roughness_sublayer_level": "basic"}}})
    df_forcing_all = sim.forcing.df
    i_start = df_forcing_all.index.get_loc(START)
    df_forcing = df_forcing_all.iloc[i_start : i_start + TIMESTEPS_PER_DAY * N_DAYS].copy()
    sim.update_forcing(df_forcing)

    # ACT
    results = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

    # ASSERT
    df_daily = (
        results.xs("DailyState", level="group", axis=1)
        .xs(1, level="grid")
        .dropna(how="all")
    )
    ser_t2_seen = (
        results.xs("SUEWS", level="group", axis=1).xs(1, level="grid")["T2"].shift(1)
    )
    ser_tair = df_forcing["Tair"]
    n_full_days = 0
    n_days_forcing_outside = 0
    for ts_end, row in df_daily.iterrows():
        day = ts_end.normalize()
        ser_day = ser_t2_seen[
            (ser_t2_seen.index >= day) & (ser_t2_seen.index < day + pd.Timedelta("1D"))
        ]
        # the first day starts after 00:00 and is never reset; skip it
        if len(ser_day) < TIMESTEPS_PER_DAY or ser_day.isna().any():
            continue
        n_full_days += 1
        t_forcing_first = ser_tair.loc[day]
        if not ser_day.min() <= t_forcing_first <= ser_day.max():
            n_days_forcing_outside += 1
        np.testing.assert_allclose(
            [row["Tmin"], row["Tmax"]],
            [ser_day.min(), ser_day.max()],
            rtol=0,
            atol=1e-9,
            err_msg=(
                f"Tmin/Tmax on {day:%Y-%m-%d} are not the extremes of the T2 "
                "that DailyState used that day; the daily reset is taking the "
                "forcing temperature (regression of gh#1882)"
            ),
        )
    assert n_full_days >= N_DAYS - 2
    assert n_days_forcing_outside >= 1, (
        "no day in the window has a forcing temperature at 00:00 outside the "
        "day's T2 range, so this test no longer exercises gh#1882"
    )
