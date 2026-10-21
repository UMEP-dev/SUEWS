"""Check the Reindl split through the compiled SUEWS/SPARTACUS driver."""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.core]


def _run_split(method, kdown):
    state, forcing = load_sample_frames()
    state = state.iloc[[0]].copy()
    state.loc[:, ("netradiationmethod", "0")] = 1003
    state.loc[:, ("kdown_split_method", "0")] = method
    # The sample's highest layer lies above both tree crowns.
    state.loc[:, ("veg_frac", "(2,)")] = 0.0
    state.loc[:, ("veg_scale", "(2,)")] = 0.0
    forcing = forcing.iloc[:288].copy()
    forcing.loc[:, "kdown"] = kdown
    forcing.loc[:, "Tair"] = 20.0
    forcing.loc[:, "RH"] = 50.0
    output, _ = run_simulation(forcing, state, serial_mode=True)
    return output


@pytest.mark.parametrize("kdown", [50.0, 200.0, 400.0])
def test_reindl_driver_uses_published_diffuse_correlation(kdown):
    output = _run_split(4, kdown)
    zenith = output["SUEWS", "Zenith"].to_numpy()
    # Independent reference: Reindl et al. (1990), four-predictor model,
    # with T=20 C and RH=0.5. Spencer day-1 irradiance factor is 1.03505.
    elevation_sine = np.cos(np.deg2rad(zenith))
    day = elevation_sine > np.sin(np.deg2rad(0.1))
    assert day.any()
    kt = kdown / (1370.0 * 1.03505 * elevation_sine[day])
    diffuse_fraction = np.select(
        [kt <= 0.3, kt < 0.78],
        [0.99611 - 0.232 * kt + 0.0239 * elevation_sine[day],
         1.3106 - 1.716 * kt + 0.267 * elevation_sine[day]],
        default=0.1065 + 0.426 * kt - 0.256 * elevation_sine[day],
    )
    expected_diffuse = kdown * np.clip(diffuse_fraction, 0.0, 1.0)
    diffuse = output["SPARTACUS", "KTopDnDif"].to_numpy()
    direct = output["SPARTACUS", "KTopDnDir"].to_numpy()
    # 0.02 W m-2 covers the rounded independent orbital factor and BEERS'
    # legacy single-precision coefficients, well below forcing precision.
    np.testing.assert_allclose(diffuse[day], expected_diffuse, rtol=0, atol=0.02)
    np.testing.assert_allclose(direct + diffuse, kdown, rtol=0, atol=1e-8)
    np.testing.assert_allclose(direct[~day], 0.0, rtol=0, atol=1e-8)
    assert np.isfinite(output["SUEWS", "QN"]).all()


def test_reindl_zero_shortwave_has_zero_components():
    output = _run_split(4, 0.0)
    components = output["SPARTACUS"][["KTopDnDir", "KTopDnDif"]]
    np.testing.assert_allclose(components, 0.0, rtol=0, atol=1e-8)
