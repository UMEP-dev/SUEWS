"""General-model CO2 has no photosynthesis without vegetation (#2101)."""

from copy import deepcopy
from functools import cache

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.physics


@pytest.fixture(scope="module")
def general_co2_run(sample_config_loaded, sample_data_loaded):
    """Public dict path, supplied coefficients, and one complete summer day."""
    base = sample_config_loaded.model_dump(mode="json")
    forcing = (
        sample_data_loaded[1].loc["2012-06-15 00:05:00":"2012-06-16 00:00:00"].copy()
    )
    assert len(forcing) == 288

    @cache
    def run(endpoint, grass_fraction=0.0, theta=0.96, method=32, zero_grass_lai=False):
        config = deepcopy(base)
        config["model"]["physics"]["emissions"] = {"value": method}
        config["model"]["control"]["start_time"] = "2012-06-15"
        config["model"]["control"]["end_time"] = "2012-06-15"
        land = config["sites"][0]["properties"]["land_cover"]
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            land[surface]["sfr"] = {"value": 0.0}
        land[endpoint]["sfr"] = {"value": 1.0 - grass_fraction}
        land["grass"]["sfr"] = {"value": grass_fraction}
        land["grass"]["theta_bio_co2"] = {"value": theta}
        if zero_grass_lai:
            for name in ("lai_min", "lai_max"):
                land["grass"]["lai"][name] = {"value": 0.0}
        simulation = SUEWSSimulation()
        simulation.update_config(config, auto_load_forcing=False)
        simulation.update_forcing(forcing)
        output = simulation.run(n_jobs=1).df
        pd.testing.assert_index_equal(
            output.index.get_level_values("datetime"),
            forcing.index,
            check_names=False,
        )
        return output

    return run


@pytest.mark.parametrize("endpoint", ["paved", "bsoil"])
@pytest.mark.parametrize("theta", [0.96, 0.83], ids=["equal", "unequal"])
def test_general_co2_empty_vegetation(endpoint, theta, general_co2_run):
    output = general_co2_run(endpoint, theta=theta)["SUEWS"]
    assert np.isfinite(output[["Fc", "FcPhoto", "FcRespi"]]).all().all()
    np.testing.assert_array_equal(output["FcPhoto"], 0.0)
    np.testing.assert_array_equal(output["FcRespi"], 0.0)
    # Independent component identity: a guard must not erase anthropogenic CO2.
    components = output[["FcMetab", "FcTraff", "FcBuild", "FcPoint"]]
    values = components.to_numpy()
    assert np.isfinite(values).all() and not np.equal(values, -999.0).any()
    anthropogenic = components.sum(axis=1, skipna=False)
    assert np.any(anthropogenic.to_numpy())
    np.testing.assert_allclose(output["Fc"], anthropogenic, rtol=0, atol=1e-12)


@pytest.mark.parametrize("endpoint", ["paved", "bsoil"])
def test_general_co2_dormant_coefficients_do_not_change_output(
    endpoint, general_co2_run
):
    # All output groups, columns, rows and missing-value masks must agree.
    # The endpoint's existing non-CO2 missing diagnostics are not a finite oracle.
    pd.testing.assert_frame_equal(
        general_co2_run(endpoint, theta=0.96),
        general_co2_run(endpoint, theta=0.83),
        check_exact=True,
    )


@pytest.mark.parametrize("fraction", [0.05, 1e-6, 1e-12, 1e-20])
def test_general_co2_positive_vegetation_is_not_thresholded(fraction, general_co2_run):
    output = general_co2_run("paved", grass_fraction=fraction, theta=0.83)["SUEWS"]
    photo = output["FcPhoto"].to_numpy()
    assert np.isfinite(photo).all()
    assert np.any(photo), "Every positive vegetation fraction must still be evaluated"


@pytest.mark.parametrize("theta", [0.96, 0.83])
def test_general_co2_empty_vegetation_with_zero_grass_lai(theta, general_co2_run):
    output = general_co2_run("paved", theta=theta, zero_grass_lai=True)["SUEWS"]
    for name in ("FcPhoto", "FcRespi"):
        np.testing.assert_array_equal(output[name], 0.0)
    assert np.isfinite(output["Fc"]).all()
    assert output["Fc"].ne(0).any()
