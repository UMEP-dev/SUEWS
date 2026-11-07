"""Hourly water-body flow must be integrated once per timestep (gh#2012)."""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

pytestmark = pytest.mark.physics


def _run_flow(make_sim, flow, snow_use, tstep):
    sim = make_sim()
    list_sites = [site.model_dump() for site in sim.config.sites]
    for site in list_sites:
        # Use the public configuration path, including its normal bridge mapping.
        site["properties"]["land_cover"]["water"]["flowchange"] = {"value": flow}
    sim.update_config(
        {
            "sites": list_sites,
            "model": {
                "physics": {"snow_use": snow_use},
                "control": {"tstep": tstep, "output": {"freq": tstep}},
            },
        },
        auto_load_forcing=False,
    )
    df_forcing = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    df_forcing = df_forcing.resample(f"{tstep}s", closed="right", label="right").mean()
    # Warm, dry conditions isolate the imposed water-body input from snowfall
    # and freezing, while snow_use still selects the two hydrology paths.
    df_forcing["Tair"] = 5.0
    df_forcing["rain"] = 0.0
    df_forcing["RH"] = 80.0
    df_forcing["U"] = 3.0
    df_forcing["pres"] = 1013.0
    sim.update_forcing(df_forcing)
    df_output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df
    df_suews = df_output.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    return df_suews, sim.config.sites[0].properties.land_cover.water.sfr.value


@pytest.mark.core
@pytest.mark.parametrize("snow_use", [0, 1], ids=["snow-off", "snow-on"])
@pytest.mark.parametrize("tstep", [300, 600], ids=["5min", "10min"])
def test_water_flow_integrates_hourly_rate(short_sample_sim, snow_use, tstep):
    df_zero, sfr_water = _run_flow(short_sample_sim, 0.0, snow_use, tstep)
    df_flow, _ = _run_flow(short_sample_sim, 1.0, snow_use, tstep)

    ser_storage = (df_flow["StWater"] - df_zero["StWater"]) * sfr_water
    ser_extra_runoff = df_flow["RO"] - df_zero["RO"]
    ser_increment = ser_storage.diff().fillna(ser_storage.iloc[0])
    expected = tstep / 3600.0 * sfr_water

    # Both storage and runoff are grid-average depths; the prescribed rate is
    # per unit water area. The paired runs cancel their common water fluxes.
    np.testing.assert_allclose(
        ser_increment + ser_extra_runoff, expected, rtol=0, atol=1e-9
    )
    np.testing.assert_allclose(
        df_flow["FlowCh"] - df_zero["FlowCh"],
        tstep / 3600.0,
        rtol=0,
        atol=1e-12,
    )
