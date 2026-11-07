"""Inactive initial snow must not affect a snow-disabled run (gh#2010)."""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

pytestmark = pytest.mark.physics

SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
SNOW_STATE = {
    "snowfrac": 1.0,
    "snowpack": 10.0,
    "snowdens": 100.0,
    "snowwater": 1.0,
    "icefrac": 0.2,
}


def _run_initial_snow(make_sim, fields, snow_use=0):
    sim = make_sim()
    list_sites = [site.model_dump() for site in sim.config.sites]
    for site in list_sites:
        state = site["initial_states"]
        state["snowalb"] = 0.8 if "snowalb" in fields else 0.0
        for surface in SURFACES:
            for field, value in SNOW_STATE.items():
                state[surface][field] = value if field in fields else 0.0
    sim.update_config(
        {
            "model": {
                "physics": {"snow_use": snow_use},
                "control": {"output": {"freq": 300}},
            },
            "sites": list_sites,
        },
        auto_load_forcing=False,
    )
    df_forcing = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    # A cold, dry day preserves an initial pack when snow is enabled; no new
    # snowfall can make the positive control pass without the supplied state.
    df_forcing["Tair"] = -5.0
    df_forcing["rain"] = 0.0
    sim.update_forcing(df_forcing)
    return sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df


@pytest.mark.core
@pytest.mark.parametrize(
    "fields",
    [
        {"snowfrac"},
        {"snowpack", "snowalb"},
        {"snowdens"},
        set(SNOW_STATE) | {"snowalb"},
    ],
    ids=["fraction", "pack-and-albedo", "density", "all-snow-state"],
)
def test_snow_off_ignores_initial_snow(short_sample_sim, fields):
    df_clear = _run_initial_snow(short_sample_sim, set())
    df_snow = _run_initial_snow(short_sample_sim, fields)

    # Inactive input is removed before any calculation. Exact equality is the
    # contract here, including diagnostics, rather than a measurement tolerance.
    pd.testing.assert_frame_equal(df_snow, df_clear, check_exact=True)


@pytest.mark.core
def test_snow_on_keeps_initial_pack(short_sample_sim):
    df_clear = _run_initial_snow(short_sample_sim, set(), snow_use=1)
    df_snow = _run_initial_snow(
        short_sample_sim, set(SNOW_STATE) | {"snowalb"}, snow_use=1
    )
    ser_clear = df_clear.xs("snow", level="group", axis=1)["SWE_Paved"]
    ser_snow = df_snow.xs("snow", level="group", axis=1)["SWE_Paved"]

    np.testing.assert_allclose(ser_clear, 0.0, rtol=0, atol=1e-12)
    assert ser_snow.iloc[0] > 0.0, "snow-on must retain the supplied initial pack"
