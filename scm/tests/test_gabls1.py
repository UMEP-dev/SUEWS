"""Fast regression test of the GABLS1 stable boundary-layer case.

Coarsened version (dz = 12.5 m, dt = 30 s) of the benchmark in
``benchmarks/run_gabls1.py``. The LES ensemble of Beare et al. (2006)
gives a quasi-steady SBL after 8-9 h with depth ~150-250 m, friction
velocity ~0.25-0.30 m s-1 and a low-level jet near the BL top.
"""

import numpy as np

from suews_scm import ColumnModel, Grid


def test_gabls1_against_les_ensemble_ranges():
    grid = Grid(dz0=12.5, ztop=400.0, stretch=1.0)
    col = ColumnModel(
        grid, f_coriolis=1.39e-4, ug=8.0, vg=0.0, z0m=0.1,
        p_sfc=1.0e5, t_ref=263.0, stable_fn="sharp",
    )
    theta0 = np.where(grid.z <= 100.0, 265.0, 265.0 + 0.01 * (grid.z - 100.0))
    col.set_initial_profiles(theta=theta0, q=np.zeros(grid.n),
                             u=np.full(grid.n, 8.0), v=np.zeros(grid.n))
    dt = 30.0
    nstep = int(9 * 3600 / dt)
    for i in range(1, nstep + 1):
        theta_s = 265.0 - 0.25 * (i * dt) / 3600.0
        diag = col.step(dt, surface=dict(theta_s=theta_s))

    assert 120.0 < diag["h"] < 280.0
    assert 0.20 < diag["ustar"] < 0.35
    # surface heat flux: downward, order 10 W m-2 in kinematic units
    assert -0.03 < diag["wth"] < -0.005
    # low-level jet: supergeostrophic maximum below 300 m
    wind = np.hypot(col.u, col.v)
    k_jet = int(np.argmax(wind))
    assert wind[k_jet] > 8.2
    assert grid.z[k_jet] < 300.0
