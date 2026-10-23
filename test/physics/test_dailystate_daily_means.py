"""DailyState daily means divide by the number of timesteps accumulated (gh#1878).

``update_DailyState_Day`` adds every timestep of a day into the heating and
cooling degree-day sums and the temperature sum, from the 00:00 step (where
``update_DailyState_Start`` resets them) to the last step of the day
inclusive. ``update_HDD`` turns those sums into means at the last step. Its
count, ``(it*60 + imin)*60/tstep``, used to be one short, so ``HDD1_h``,
``HDD2_c`` and ``HDD3_Tmean`` came out a factor ``n/(n-1)`` too high
(288/287 with five-minute steps, 24/23 with hourly ones).

The test pins the count only. Which forcing rows make up a model day is
taken as the model has it (rows stamped 00:00 to 23:55 of the date).
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics

N_DAYS = 4


@pytest.mark.core
def test_daily_mean_temperature_uses_every_accumulated_timestep():
    # ARRANGE: forcing temperature drives DailyState directly
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config({"model": {"physics": {"roughness_sublayer_level": "none"}}})
    df_forcing = sim.forcing.df.copy().iloc[: TIMESTEPS_PER_DAY * N_DAYS]
    sim.update_forcing(df_forcing)

    # ACT
    results = sim.run(end_date=df_forcing.index[-1])

    # ASSERT
    df_daily = (
        results.xs("DailyState", level="group", axis=1)
        .xs(1, level="grid")
        .dropna(how="all")
    )
    ser_tair = df_forcing["Tair"]
    n_full_days = 0
    for ts_end, row in df_daily.iterrows():
        day = ts_end.normalize()
        ser_day = ser_tair[(ser_tair.index >= day) & (ser_tair.index < day + pd.Timedelta("1D"))]
        # the first day starts after 00:00 and is never reset; skip it
        if len(ser_day) < TIMESTEPS_PER_DAY:
            continue
        n_full_days += 1
        np.testing.assert_allclose(
            row["HDD3_Tmean"],
            ser_day.mean(),
            rtol=1e-9,
            err_msg=(
                f"HDD3_Tmean on {day:%Y-%m-%d} is not the mean of the day's "
                f"{TIMESTEPS_PER_DAY} forcing temperatures; a ratio of "
                f"{TIMESTEPS_PER_DAY}/{TIMESTEPS_PER_DAY - 1} means the "
                "timestep count is one short (regression of gh#1878)"
            ),
        )
    assert n_full_days >= N_DAYS - 2
