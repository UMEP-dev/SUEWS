"""DailyState daily statistics cover the timesteps the day accumulated.

``update_DailyState_Day`` adds every timestep of a day into the heating and
cooling degree-day sums and the temperature sum, from the step that starts the
day (where ``update_DailyState_Start`` resets them) to the last step of the
day inclusive, and tracks the day's minimum and maximum temperature.
``update_HDD`` turns the sums into means at the last step.

Two defects are pinned here:

- gh#1878: the count, ``(it*60 + imin)*60/tstep``, used to be one short, so
  ``HDD1_h``, ``HDD2_c`` and ``HDD3_Tmean`` came out a factor ``n/(n-1)`` too
  high (288/287 with five-minute steps, 24/23 with hourly ones).
- gh#1919: a run whose first step is after 00:00 (the sample forcing starts
  at 00:05) never reset its first day, so that day's ``Tmin``/``Tmax`` and
  sums started from the initial state, and its means were divided by a full
  day's count. The first step of a run now starts a day as 00:00 does.

Which forcing rows make up a model day is taken as the model has it (rows
stamped 00:00 to 23:55 of the date). The forcing temperature drives DailyState
directly (``roughness_sublayer_level: none``), so each day's statistics can be
computed from the forcing.
"""

import numpy as np
import pytest

from supy import SUEWSSimulation

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics

N_DAYS = 4


def _run_daily_state(first_step):
    """Run N_DAYS of sample forcing from the first row at *first_step* (HH:MM).

    Returns the DailyState rows and the forcing air temperature.
    """
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config({"model": {"physics": {"roughness_sublayer_level": "none"}}})
    df_forcing = sim.forcing.df.copy()
    n_skip = int(np.argmax(df_forcing.index.strftime("%H:%M") == first_step))
    df_forcing = df_forcing.iloc[n_skip : n_skip + TIMESTEPS_PER_DAY * N_DAYS]
    sim.update_forcing(df_forcing)
    results = sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])
    assert results.index.get_level_values("datetime")[0] == df_forcing.index[0]
    df_daily = (
        results.xs("DailyState", level="group", axis=1)
        .xs(1, level="grid")
        .dropna(how="all")
    )
    return df_daily, df_forcing["Tair"]


@pytest.mark.core
@pytest.mark.parametrize(
    "first_step",
    ["00:05", "00:00"],
    ids=["run-starts-after-midnight", "run-starts-at-midnight"],
)
def test_daily_statistics_cover_the_steps_the_day_accumulated(first_step):
    # ARRANGE + ACT: the sample forcing starts at 00:05, so "00:05" gives a
    # partial first day of 287 steps and "00:00" a complete one
    df_daily, ser_tair = _run_daily_state(first_step)

    # ASSERT
    assert f"{ser_tair.index[0]:%H:%M}" == first_step
    n_days = 0
    for ts_end, row in df_daily.iterrows():
        day = ts_end.normalize()
        ser_day = ser_tair[(ser_tair.index >= day) & (ser_tair.index <= ts_end)]
        n_days += 1
        np.testing.assert_allclose(
            row["HDD3_Tmean"],
            ser_day.mean(),
            rtol=1e-9,
            err_msg=(
                f"HDD3_Tmean on {day:%Y-%m-%d} is not the mean of the day's "
                f"{len(ser_day)} forcing temperatures: a ratio of n/(n-1) means "
                "the timestep count is one short (gh#1878); on a run's first "
                "day it means the day was not started at the run's first step "
                "(gh#1919)"
            ),
        )
        for col, expected in (("Tmin", ser_day.min()), ("Tmax", ser_day.max())):
            np.testing.assert_allclose(
                row[col],
                expected,
                rtol=1e-12,
                err_msg=(
                    f"{col} on {day:%Y-%m-%d} is not the extreme of the day's "
                    f"{len(ser_day)} forcing temperatures; on a run's first day "
                    "this means it was carried from the initial state (gh#1919)"
                ),
            )
    # the first (possibly partial) day is checked as well as the full ones
    assert df_daily.index[0].normalize() == ser_tair.index[0].normalize()
    assert n_days >= N_DAYS - 1
