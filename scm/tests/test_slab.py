"""Tests for the CLASS-style slab convective boundary-layer model.

Analytic references (Tennekes 1973; Garratt 1992, ch. 6):

- encroachment (beta = 0):  h(t) = sqrt(h0^2 + 2 F t / gamma)
- equilibrium entrainment (beta):
    h(t) ~ sqrt(h0^2 + 2 (1 + 2 beta) F t / gamma)
"""

import numpy as np
import pytest

from suews_scm.slab import SlabCBL


def test_encroachment_matches_analytic_solution():
    f_heat = 0.08  # K m s-1
    gamma = 0.005
    slab = SlabCBL(h0=100.0, theta0=290.0, dtheta0=0.0, gamma_theta=gamma, beta=0.0)
    dt, hours = 30.0, 8.0
    n = int(hours * 3600 / dt)
    for _ in range(n):
        out = slab.step(dt, wth=f_heat)
    h_exact = np.sqrt(100.0**2 + 2.0 * f_heat * n * dt / gamma)
    assert out["h"] == pytest.approx(h_exact, rel=0.01)


def test_entrainment_growth_matches_equilibrium_law():
    f_heat = 0.1
    gamma = 0.006
    beta = 0.2
    slab = SlabCBL(h0=200.0, theta0=288.0, dtheta0=1.0, gamma_theta=gamma, beta=beta)
    dt, hours = 30.0, 6.0
    n = int(hours * 3600 / dt)
    for _ in range(n):
        out = slab.step(dt, wth=f_heat)
    h_eq = np.sqrt(200.0**2 + 2.0 * (1.0 + 2.0 * beta) * f_heat * n * dt / gamma)
    assert out["h"] == pytest.approx(h_eq, rel=0.05)


def test_heat_budget_closed():
    """ML heat content change minus heat swept from aloft = surface input.

    For the slab ODEs (no subsidence) the exact budget is
        d(h theta_m)/dt = F + w_e theta_ft(h),
    so integrated:  h theta_m - h0 theta0 - swept = F t,
    where ``swept`` is the heat of the free-tropospheric air between h0
    and h (trapezoid is exact for a linear profile).
    """
    f_heat = 0.1
    gamma = 0.006
    slab = SlabCBL(h0=200.0, theta0=288.0, dtheta0=1.0, gamma_theta=gamma, beta=0.2)
    dt, n = 30.0, 720
    for _ in range(n):
        out = slab.step(dt, wth=f_heat)
    dh_content = out["h"] * out["theta_m"] - 200.0 * 288.0
    theta_ft = lambda z: 288.0 + 1.0 + gamma * (z - 200.0)
    swept = (out["h"] - 200.0) * 0.5 * (theta_ft(200.0) + theta_ft(out["h"]))
    np.testing.assert_allclose(dh_content - swept, f_heat * n * dt, rtol=0.01)


def test_entrainment_dries_mixed_layer_without_surface_moisture():
    slab = SlabCBL(
        h0=200.0, theta0=288.0, dtheta0=1.0, gamma_theta=0.006,
        q0=0.008, dq0=-0.003, gamma_q=0.0, beta=0.2,
    )
    q_start = slab.q_m
    for _ in range(240):
        slab.step(60.0, wth=0.1, wq=0.0)
    assert slab.q_m < q_start
