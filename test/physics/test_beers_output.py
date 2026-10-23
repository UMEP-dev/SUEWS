"""Meaning of the BEERS radiation-component columns.

BEERS splits global shortwave radiation into a diffuse part on the horizontal,
``radD``, and a direct beam on a surface normal to the sun,
``radI = (radG - radD)/sin(altitude)`` (``diffusefraction`` in
``suews_phys_beers.f95``). The output line wrote ``radI`` under ``DiffuseRad``
and ``radD`` under ``DirectRad`` (gh#1866).
"""

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pytest

pytestmark = pytest.mark.physics

# Below 1 degree of solar altitude the kernel caps the direct beam at the
# global radiation, so the decomposition identity does not hold there.
MIN_ALTITUDE_DEG = 1.0
# Rebuilding radI*sin(altitude) from the output altitude leaves residuals of
# 1e-7 to 1e-5 W m-2 in the sample; the swapped columns miss by up to
# several hundred.
ATOL_W_M2 = 1e-3


@pytest.mark.core
def test_diffuse_plus_horizontal_direct_is_global(sample_run_cached):
    """``DiffuseRad + DirectRad*sin(altitude) == GlobalRad`` in daylight."""
    df_output, _ = sample_run_cached(TIMESTEPS_PER_DAY)
    beers = df_output.xs("BEERS", level="group", axis=1).xs(1, level="grid")

    day = (beers["altitude"] >= MIN_ALTITUDE_DEG) & (beers["GlobalRad"] > 0)
    assert day.sum() > 0
    b = beers[day]
    sin_altitude = np.sin(np.deg2rad(b["altitude"]))

    np.testing.assert_allclose(
        b["DiffuseRad"] + b["DirectRad"] * sin_altitude,
        b["GlobalRad"],
        rtol=0,
        atol=ATOL_W_M2,
        err_msg="DiffuseRad and DirectRad do not decompose GlobalRad (gh#1866)",
    )
    # The diffuse part is a fraction of the global radiation.
    assert (b["DiffuseRad"] <= b["GlobalRad"] + 1e-9).all()
    assert (b["DiffuseRad"] >= 0).all()
