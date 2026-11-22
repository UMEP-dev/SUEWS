"""An absent vegetation type contributes no biogenic carbon flux (#2101)."""

from copy import deepcopy
from functools import cache

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE")]


@pytest.fixture(scope="module")
def co2_run(sample_config_loaded, sample_data_loaded):
    """Public dict path, supplied coefficients, and one complete summer day."""
    base = sample_config_loaded.model_dump(mode="json")
    forcing = (
        sample_data_loaded[1].loc["2012-06-15 00:05:00":"2012-06-16 00:00:00"].copy()
    )
    assert len(forcing) == 288

    @cache
    def run(
        endpoint,
        grass_fraction=0.0,
        theta=0.96,
        method=32,
        zero_lai=None,
        zero_evergreen_coefficients=False,
    ):
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
        if zero_lai is not None:
            for name in ("lai_min", "lai_max"):
                land[zero_lai]["lai"][name] = {"value": 0.0}
        if zero_evergreen_coefficients:
            for name in ("alpha_bio_co2", "beta_bio_co2", "theta_bio_co2"):
                land["evetr"][name] = {"value": 0.0}
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


def carbon_fluxes(output):
    """Each timestep has complete carbon fluxes and closes independently."""
    names = ["Fc", "FcPhoto", "FcRespi", "FcMetab", "FcTraff", "FcBuild", "FcPoint"]
    fluxes = output["SUEWS"][names]
    values = fluxes.to_numpy()
    assert np.isfinite(values).all()
    assert not np.equal(values, -999.0).any()
    anthropogenic = fluxes[["FcMetab", "FcTraff", "FcBuild", "FcPoint"]].sum(
        axis=1, skipna=False
    )
    assert anthropogenic.ne(0).any()
    np.testing.assert_allclose(
        fluxes["Fc"],
        anthropogenic + fluxes["FcPhoto"] + fluxes["FcRespi"],
        rtol=0,
        atol=1e-12,
    )
    return fluxes


@pytest.mark.parametrize("endpoint", ["paved", "bsoil"])
@pytest.mark.parametrize("theta", [0.96, 0.83], ids=["equal", "unequal"])
def test_general_co2_empty_vegetation(endpoint, theta, co2_run):
    fluxes = carbon_fluxes(co2_run(endpoint, theta=theta))
    np.testing.assert_array_equal(fluxes[["FcPhoto", "FcRespi"]].to_numpy(), 0.0)


@pytest.mark.parametrize("endpoint", ["paved", "bsoil"])
def test_general_co2_dormant_coefficients_do_not_change_output(endpoint, co2_run):
    # All output groups, columns, rows and missing-value masks must agree.
    # The endpoint's existing non-CO2 missing diagnostics are not a finite oracle.
    pd.testing.assert_frame_equal(
        co2_run(endpoint, theta=0.96),
        co2_run(endpoint, theta=0.83),
        check_exact=True,
    )


@pytest.mark.parametrize("fraction", [0.05, 1e-6, 1e-12, 1e-20])
def test_general_co2_positive_vegetation_is_not_thresholded(fraction, co2_run):
    fluxes = carbon_fluxes(co2_run("paved", grass_fraction=fraction, theta=0.83))
    assert fluxes["FcPhoto"].ne(0).any()


@pytest.mark.parametrize("method", [12, 22, 32, 42])
@pytest.mark.parametrize("theta", [0.96, 0.83], ids=["equal", "unequal"])
def test_empty_vegetation_with_zero_grass_lai(method, theta, co2_run):
    fluxes = carbon_fluxes(
        co2_run("paved", method=method, theta=theta, zero_lai="grass")
    )
    np.testing.assert_array_equal(fluxes[["FcPhoto", "FcRespi"]].to_numpy(), 0.0)


@pytest.mark.parametrize("method", [12, 22, 32, 42])
@pytest.mark.parametrize("endpoint,fraction", [("paved", 0.3), ("bsoil", 1e-6)])
def test_absent_evergreen_lai_preserves_active_grass(
    endpoint, fraction, method, co2_run
):
    reference = carbon_fluxes(co2_run(endpoint, fraction, method=method))
    dormant = carbon_fluxes(
        co2_run(endpoint, fraction, method=method, zero_lai="evetr")
    )
    # Method 42 also uses grid-scale conductance, which can respond to LAI.
    # Local and general uptake depend directly on the grass that is present.
    if method != 42:
        pd.testing.assert_series_equal(
            dormant["FcPhoto"], reference["FcPhoto"], check_exact=True
        )
    assert dormant["FcPhoto"].ne(0).any() and reference["FcPhoto"].ne(0).any()
    if fraction > 0.01:
        assert dormant["FcRespi"].gt(0).any()


@pytest.mark.parametrize("method", [12, 22])
def test_absent_evergreen_local_coefficients_do_not_affect_grass(method, co2_run):
    reference = carbon_fluxes(co2_run("paved", 0.3, method=method))
    dormant = carbon_fluxes(
        co2_run("paved", 0.3, method=method, zero_evergreen_coefficients=True)
    )
    pd.testing.assert_frame_equal(dormant, reference, check_exact=True)
