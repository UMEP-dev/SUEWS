"""Tests for the K-profile column model: conservation, stability, physics."""

import numpy as np
import pytest

from suews_scm.column import ColumnModel
from suews_scm.grid import Grid
from suews_scm.constants import VONK


def _make_column(**kwargs):
    grid = Grid(dz0=20.0, ztop=2500.0, stretch=1.05)
    defaults = dict(f_coriolis=1.0e-4, ug=0.0, vg=0.0, z0m=0.1, p_sfc=1.0e5)
    defaults.update(kwargs)
    col = ColumnModel(grid, **defaults)
    theta0 = 288.0 + 0.006 * np.maximum(grid.z - 200.0, 0.0)
    col.set_initial_profiles(theta=theta0, q=np.full(grid.n, 0.005), u=np.zeros(grid.n), v=np.zeros(grid.n))
    return col


def test_heat_conserved_with_zero_surface_flux():
    col = _make_column()
    content0 = np.sum(col.theta * col.grid.dz)
    for _ in range(20):
        col.step(60.0, surface=dict(wth=0.0, wq=0.0, ustar=0.1))
    content1 = np.sum(col.theta * col.grid.dz)
    assert abs(content1 - content0) < 1.0e-8 * abs(content0)


def test_heat_content_increases_by_exact_surface_flux_integral():
    col = _make_column()
    content0 = np.sum(col.theta * col.grid.dz)
    wth, dt, nstep = 0.12, 60.0, 30
    for _ in range(nstep):
        col.step(dt, surface=dict(wth=wth, wq=0.0, ustar=0.3))
    content1 = np.sum(col.theta * col.grid.dz)
    np.testing.assert_allclose(content1 - content0, wth * dt * nstep, rtol=1.0e-10)


def test_moisture_conserved_and_incremented_consistently():
    col = _make_column()
    q0 = np.sum(col.q * col.grid.dz)
    wq, dt, nstep = 5.0e-5, 60.0, 10
    for _ in range(nstep):
        col.step(dt, surface=dict(wth=0.05, wq=wq, ustar=0.3))
    q1 = np.sum(col.q * col.grid.dz)
    np.testing.assert_allclose(q1 - q0, wq * dt * nstep, rtol=1.0e-10)


def test_no_nans_or_oscillations_with_large_timestep():
    col = _make_column()
    for _ in range(10):
        diag = col.step(600.0, surface=dict(wth=0.2, wq=1.0e-4, ustar=0.5))
    assert np.all(np.isfinite(col.theta))
    assert np.all(np.isfinite(col.u))
    # mixed layer must remain monotonic-ish: no grid-scale sawtooth
    dtheta = np.diff(col.theta[:10])
    assert np.all(np.abs(dtheta) < 1.0)
    assert diag["h"] > 0


def test_neutral_most_recovers_log_law_friction_velocity():
    col = _make_column(z0m=0.1)
    z1 = col.grid.z[0]
    col.u[:] = 5.0
    col.v[:] = 0.0
    # neutral: surface theta equals air theta -> ustar = k U / ln(z1/z0)
    diag = col.step(1.0, surface=dict(theta_s=col.theta[0] * (col.p_levels[0] / 1.0e5) ** (287.05 / 1005.0)))
    ustar_expected = VONK * 5.0 / np.log(z1 / 0.1)
    assert diag["ustar"] == pytest.approx(ustar_expected, rel=0.05)


def test_cbl_growth_against_mixed_layer_theory():
    """Constant surface heat flux CBL: h(t) and mixed-layer theta vs theory.

    Analytic slab solution for constant kinematic heat flux F, lapse rate
    gamma, entrainment ratio beta (Tennekes 1973; Garratt 1992):
        h(t) = sqrt(h0^2 + 2 (1 + 2 beta) F t / gamma)
    With beta = 0.2 (LES consensus, e.g. Sullivan et al. 1998) the
    K-profile column should track h within ~15 % and the mixed-layer
    mean theta within ~0.5 K.
    """
    col = _make_column()
    flux = 0.1  # K m s-1
    dt, hours = 60.0, 6.0
    nstep = int(hours * 3600 / dt)
    h_diag = 0.0
    for _ in range(nstep):
        diag = col.step(dt, surface=dict(wth=flux, wq=0.0, ustar=0.3))
        h_diag = diag["h"]

    gamma, h0, beta = 0.006, 200.0, 0.2
    t_tot = nstep * dt
    h_theory = np.sqrt(h0**2 + 2.0 * (1.0 + 2.0 * beta) * flux * t_tot / gamma)
    assert h_diag == pytest.approx(h_theory, rel=0.15)

    # mixed-layer mean theta from the bulk heat budget with entrainment
    in_ml = col.grid.z < 0.8 * h_diag
    theta_ml = np.mean(col.theta[in_ml])
    theta_theory = (
        288.0
        + gamma * (h_theory - h0) ** 2 / (2.0 * h_theory)
        + (1.0 + beta) * flux * t_tot / h_theory
    )
    assert theta_ml == pytest.approx(theta_theory, abs=0.5)
