"""Tests for thermodynamic helper functions."""

import numpy as np
import pytest

from suews_scm import thermo


def test_theta_temperature_round_trip():
    p = 95000.0
    t = 293.15
    theta = thermo.theta_from_t(t, p)
    assert theta > t  # below reference pressure level p0=1e5 -> theta > T at p < p0
    np.testing.assert_allclose(thermo.t_from_theta(theta, p), t, rtol=1e-12)


def test_theta_equals_t_at_reference_pressure():
    np.testing.assert_allclose(thermo.theta_from_t(300.0, 1.0e5), 300.0, rtol=1e-12)


def test_saturation_vapour_pressure_at_20c():
    # Standard value: e_s(20 degC) ~ 2339 Pa (e.g. Alduchov & Eskridge 1996)
    es = thermo.esat_pa(293.15)
    assert 2300.0 < es < 2380.0


def test_qsat_magnitude_at_20c_sea_level():
    # ~14.7 g/kg at 20 degC, 1013.25 hPa
    qs = thermo.qsat(293.15, 101325.0)
    assert 0.0140 < qs < 0.0152


def test_rh_q_round_trip():
    t, p, rh = 298.15, 100000.0, 65.0
    q = thermo.q_from_rh(rh, t, p)
    np.testing.assert_allclose(thermo.rh_from_q(q, t, p), rh, rtol=1e-10)


def test_moist_density_below_dry_density():
    t, p = 293.15, 100000.0
    rho_dry = thermo.rho_air(t, p, 0.0)
    rho_moist = thermo.rho_air(t, p, 0.010)
    assert rho_moist < rho_dry
    assert 1.0 < rho_moist < 1.3


def test_hydrostatic_pressure_profile_scale_height():
    z = np.array([0.0, 1000.0, 3000.0])
    p = thermo.pressure_profile(101325.0, 288.0, z)
    assert p[0] == pytest.approx(101325.0)
    # scale height ~ 8.4 km at 288 K -> p(1 km) ~ 89-90 kPa
    assert 88000.0 < p[1] < 91000.0
    assert np.all(np.diff(p) < 0)
