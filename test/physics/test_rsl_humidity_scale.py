"""Humidity scale of the roughness-sublayer profiles.

``RSLProfile`` scales one dimensionless stability profile by ``TStar_RSL`` to
give the temperature profile and by ``qStar_RSL`` to give the humidity
profile, so between any two levels

    (q_k - q_m) / (T_k - T_m) = qStar_RSL / TStar_RSL.

``qStar_RSL`` is the kinematic moisture flux over the friction velocity,
``-QE/(lv*rho)/UStar_heat``, as ``TStar_RSL`` is ``-QH/(cp*rho)/UStar_heat``.
It was coded as ``-(QE/lv*rho)/UStar_heat``, which multiplies by air density
instead of dividing and made the humidity profile, Q2 and RH2 depart from
the forcing humidity by ``rho**2`` (about 1.4) times too much (gh#1867).
"""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

pytestmark = pytest.mark.physics

# Two levels above the canopy, where both profiles follow the same
# stability function.
UPPER, LOWER = 30, 21


@pytest.mark.core
def test_humidity_scale_is_kinematic_moisture_flux(sample_run_cached):
    """``qStar_RSL`` recovered from the profiles equals ``-QE/(lv*rho)/UStar_heat``."""
    df_output, _ = sample_run_cached(TIMESTEPS_PER_DAY)
    grid = df_output.xs(1, level="grid")
    rsl = grid["RSL"]
    debug = grid["debug"]
    qe = grid["SUEWS"]["QE"]

    dt = rsl[f"T_{UPPER}"] - rsl[f"T_{LOWER}"]
    # Steps whose profiles carry a resolvable gradient and a real moisture flux.
    resolved = (dt.abs() > 1e-3) & (qe.abs() > 5.0) & (rsl["TStar_RSL"].abs() > 1e-3)
    assert resolved.sum() > 10

    dq = (rsl[f"q_{UPPER}"] - rsl[f"q_{LOWER}"]) / 1000.0  # g kg-1 -> kg kg-1
    q_star_from_profile = (rsl["TStar_RSL"] * dq / dt)[resolved]
    q_star_expected = (
        -qe / (debug["lv_J_kg"] * debug["avdens"]) / rsl["UStar_heat"]
    )[resolved]

    np.testing.assert_allclose(
        q_star_from_profile,
        q_star_expected,
        rtol=1e-6,
        err_msg="humidity profile is not scaled by -QE/(lv*rho)/UStar_heat (gh#1867)",
    )
