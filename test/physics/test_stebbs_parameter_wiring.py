"""Regression tests for STEBBS physics parameter wiring."""

from importlib import import_module
from pathlib import Path
import warnings

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]

STEBBS_CONFIG = (
    Path(__file__).parents[1]
    / "fixtures"
    / "data_test"
    / "stebbs_test"
    / "sample_config.yml"
)


def _rust_library_available() -> bool:
    """Return True when the Rust Python bridge exposes run_suews()."""
    for module_name in ("supy.suews_bridge", "suews_bridge"):
        try:
            module = import_module(module_name)
        except Exception:
            continue
        if hasattr(module, "run_suews"):
            return True
    return False


def _run_one_second_stebbs_probe(external_ground_conductivity=None):
    """Run a short STEBBS case with one internal update per forcing row."""
    simulation = sp.SUEWSSimulation(STEBBS_CONFIG)
    simulation.config.model.control.tstep = 1
    if external_ground_conductivity is not None:
        stebbs_properties = simulation.config.sites[0].properties.stebbs
        stebbs_properties.thermal_conductivity_ground.value = (
            external_ground_conductivity
        )
    simulation._df_state_init = simulation.config.to_df_state()

    forcing = simulation.forcing.df.loc["2017-08-26"].iloc[:8].copy()
    simulation._df_forcing = forcing
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        output = simulation.run(
            start_date=forcing.index[0],
            end_date=forcing.index[-1],
            n_jobs=1,
            _validate_forcing=False,
        )
    return output.df.STEBBS


def _run_daytime_shading_probe(
    internal_shading,
    reduction_factor=1.0,
    temperature_threshold=0.0,
    radiation_threshold=0.0,
):
    """Run STEBBS near peak irradiance with explicit curtain settings."""
    simulation = sp.SUEWSSimulation(STEBBS_CONFIG)
    stebbs = simulation.config.sites[0].properties.stebbs
    stebbs.internal_shading = internal_shading
    stebbs.reduction_factor_shading = reduction_factor
    stebbs.temperature_threshold_shading = temperature_threshold
    stebbs.radiation_threshold_shading = radiation_threshold
    simulation._df_state_init = simulation.config.to_df_state()

    forcing = simulation.forcing.df.loc["2017-08-26 10:55":"2017-08-26 11:10"].copy()
    simulation._df_forcing = forcing
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        output = simulation.run(
            start_date=forcing.index[0],
            end_date=forcing.index[-1],
            n_jobs=1,
            _validate_forcing=False,
        )
    return output.df.STEBBS[["Qsw_trans_win_FA", "Qsw_abs_win_FA"]]


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_roof_convection_uses_roof_temperature_and_orientation():
    """The roof flux must use the roof, rather than window, TARP coefficient."""
    output = _run_one_second_stebbs_probe()
    previous = output.iloc[-2]
    current = output.iloc[-1]

    roof_surface_to_air = previous["Tintroof"] - previous["Tair_ind"]
    assert roof_surface_to_air < 0.0

    # TARP's stable downward-facing correlation for an internal roof surface.
    roof_coefficient = 1.810 * abs(roof_surface_to_air) ** (1.0 / 3.0) / 1.382
    expected_flux = roof_coefficient * -roof_surface_to_air

    assert current["QHconv_introof_FA"] == pytest.approx(
        expected_flux,
        rel=2.0e-6,
        abs=1.0e-10,
    )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_ground_conduction_uses_external_ground_conductivity():
    """Ground heat flux must use the external-soil conductivity parameter."""
    external_ground_conductivity = 1.93
    output = _run_one_second_stebbs_probe(external_ground_conductivity)
    previous = output.iloc[-2]
    current = output.iloc[-1]

    deep_ground_temperature = 273.15 + 10.738
    ground_depth = 2.0
    expected_flux = (
        external_ground_conductivity
        * (previous["Textgrndflr"] - deep_ground_temperature)
        / ground_depth
    )

    assert current["QHcond_ground_FA"] == pytest.approx(
        expected_flux,
        rel=2.0e-6,
        abs=1.0e-10,
    )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_constant_internal_shading_reduces_only_transmitted_solar_gain():
    reduction_factor = 0.4
    unshaded = _run_daytime_shading_probe(internal_shading=0)
    shaded = _run_daytime_shading_probe(
        internal_shading=1,
        reduction_factor=reduction_factor,
    )

    unshaded_gain = unshaded["Qsw_trans_win_FA"].to_numpy()
    shaded_gain = shaded["Qsw_trans_win_FA"].to_numpy()
    assert np.any(unshaded_gain > 0.0)
    np.testing.assert_allclose(
        shaded_gain,
        reduction_factor * unshaded_gain,
        rtol=2.0e-6,
        atol=1.0e-10,
    )
    np.testing.assert_allclose(
        shaded["Qsw_abs_win_FA"],
        unshaded["Qsw_abs_win_FA"],
        rtol=2.0e-6,
        atol=1.0e-10,
    )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_controlled_internal_shading_requires_temperature_and_radiation():
    reduction_factor = 0.4
    unshaded = _run_daytime_shading_probe(internal_shading=0)
    both_satisfied = _run_daytime_shading_probe(
        internal_shading=2,
        reduction_factor=reduction_factor,
        temperature_threshold=-100.0,
        radiation_threshold=0.0,
    )
    temperature_not_satisfied = _run_daytime_shading_probe(
        internal_shading=2,
        reduction_factor=reduction_factor,
        temperature_threshold=100.0,
        radiation_threshold=0.0,
    )
    radiation_not_satisfied = _run_daytime_shading_probe(
        internal_shading=2,
        reduction_factor=reduction_factor,
        temperature_threshold=-100.0,
        radiation_threshold=1.0e6,
    )
    unshaded_gain = unshaded["Qsw_trans_win_FA"].to_numpy()

    np.testing.assert_allclose(
        both_satisfied["Qsw_trans_win_FA"],
        reduction_factor * unshaded_gain,
        rtol=2.0e-6,
        atol=1.0e-10,
    )
    np.testing.assert_allclose(
        temperature_not_satisfied["Qsw_trans_win_FA"],
        unshaded_gain,
        rtol=2.0e-6,
        atol=1.0e-10,
    )
    np.testing.assert_allclose(
        radiation_not_satisfied["Qsw_trans_win_FA"],
        unshaded_gain,
        rtol=2.0e-6,
        atol=1.0e-10,
    )
