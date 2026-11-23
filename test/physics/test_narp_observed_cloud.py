"""Observed cloud fraction must change the incoming radiation, day and night."""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

from supy import SUEWSSimulation

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-BIND"),
]


def _incoming_longwave(cloud, tair, rh, pressure):
    """Return the independently calculated flux in W m-2 for each cloud fraction."""
    # Buck saturation pressure (hPa), then Prata's clear-sky atmospheric
    # emissivity. Full cloud gives the Stefan-Boltzmann blackbody limit.
    temp_k = tair + 273.15
    sat_hpa = 6.1121 * np.exp((18.678 - tair / 234.5) * tair / (tair + 257.14))
    sat_hpa *= 1.00072 + (pressure / 10) * (3.2e-6 + 5.9e-10 * tair**2)
    water = 46.5 * (rh / 100) * sat_hpa / temp_k
    clear_emissivity = 1 - (1 + water) * np.exp(-np.sqrt(1.2 + 3 * water))
    return 5.67e-8 * temp_k**4 * (clear_emissivity + (1 - clear_emissivity) * cloud)


@pytest.mark.parametrize("method", [2, 12, 200, 1002])
def test_observed_cloud_sets_incoming_longwave(
    method, sample_config_loaded, sample_forcing_parsed
):
    """Check every forcing step against the cloud-corrected Prata emissivity."""
    config = sample_config_loaded.model_copy(deep=True)
    config.model.physics.net_radiation = method
    if method == 1002:
        # Urban SPARTACUS requires a facet-resolved storage scheme.
        config.model.physics.storage_heat = 5
        # The top layer of the sample grid lies above both tree crowns.
        layers = config.sites[0].properties.vertical_layers
        layers.veg_frac.value[-1] = 0.0
        layers.veg_scale.value[-1] = 0.0
    _, weather = sample_forcing_parsed(TIMESTEPS_PER_DAY)
    cloud = np.resize([0.0, 0.2, 0.8, 1.0], len(weather))
    tair, rh, pressure = 20.0, 50.0, 1013.0
    weather = weather.assign(Tair=tair, RH=rh, pres=pressure, fcld=cloud)
    simulation = SUEWSSimulation()
    simulation.update_config(config, auto_load_forcing=False)
    simulation.update_forcing(weather)
    output = simulation.run(start_date=weather.index[0], end_date=weather.index[-1]).df
    flux = output.loc[config.sites[0].gridiv, "SUEWS"]

    assert flux.index.equals(weather.index)
    day = flux["Zenith"].to_numpy() < 90
    for period in (day, ~day):
        assert set(cloud[period]) == {0.0, 0.2, 0.8, 1.0}
    np.testing.assert_array_equal(flux["Fcld"], cloud)
    # The kernel's legacy radiation and thermodynamic constants are rounded.
    expected = _incoming_longwave(cloud, tair, rh, pressure)
    np.testing.assert_allclose(flux["Ldown"], expected, rtol=0, atol=0.001)


def test_air_ldown_method_ignores_forcing_cloud_fraction(
    sample_config_loaded, sample_forcing_parsed
):
    """Ensure ldown_option == 3 remains invariant to forcing fcld."""
    _, base_weather = sample_forcing_parsed(TIMESTEPS_PER_DAY)
    tair, rh, pressure = 20.0, 50.0, 1013.0

    results = {}
    for cloud_value in (0.0, 1.0):
        config = sample_config_loaded.model_copy(deep=True)
        config.model.physics.net_radiation = 3
        weather = base_weather.assign(Tair=tair, RH=rh, pres=pressure, fcld=cloud_value)
        simulation = SUEWSSimulation()
        simulation.update_config(config, auto_load_forcing=False)
        simulation.update_forcing(weather)
        output = simulation.run(
            start_date=weather.index[0], end_date=weather.index[-1]
        ).df
        results[cloud_value] = output.loc[config.sites[0].gridiv, "SUEWS"]

    np.testing.assert_allclose(
        results[0.0]["Ldown"], results[1.0]["Ldown"], rtol=0, atol=0
    )
    np.testing.assert_allclose(results[0.0]["QN"], results[1.0]["QN"], rtol=0, atol=0)
