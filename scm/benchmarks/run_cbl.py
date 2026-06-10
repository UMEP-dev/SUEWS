"""Convective boundary-layer growth benchmark.

Idealised dry CBL driven by a constant kinematic surface heat flux
(0.1 K m s-1) into a linearly stratified atmosphere (6 K km-1), the
classical mixed-layer test of Tennekes (1973; see also Garratt 1992).

References checked:
- analytic encroachment solution (beta = 0) and the equilibrium
  entrainment growth law h(t) = sqrt(h0^2 + 2 (1+2 beta) F t / gamma);
- the LES consensus entrainment ratio beta = -wth(h)/wth(0) ~ 0.2
  (Sullivan et al. 1998 and many others).

The multi-level K-profile column, the slab model, and the analytic
curves are compared; the column's effective entrainment ratio is
diagnosed from its heat-flux profile.

Usage: python benchmarks/run_cbl.py
Outputs: benchmarks/results/cbl_metrics.json, site/assets/fig_cbl.png
"""

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from suews_scm import ColumnModel, Grid, SlabCBL  # noqa: E402
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, FOREST_GREEN, MUTED, FG,
)

import matplotlib.pyplot as plt  # noqa: E402

F_HEAT = 0.1  # K m s-1
GAMMA = 0.006  # K m-1
H0 = 200.0
THETA0 = 288.0
HOURS = 6.0
DT = 60.0


def analytic_h(t, beta):
    return np.sqrt(H0**2 + 2.0 * (1.0 + 2.0 * beta) * F_HEAT * t / GAMMA)


def run_column():
    grid = Grid(dz0=20.0, ztop=2500.0, stretch=1.04)
    col = ColumnModel(grid, f_coriolis=0.0, z0m=0.1)
    theta_init = THETA0 + GAMMA * np.maximum(grid.z - H0, 0.0)
    col.set_initial_profiles(theta=theta_init, q=np.zeros(grid.n))

    n = int(HOURS * 3600 / DT)
    t_h, h_col = [], []
    snapshots = {}
    flux_profile = None
    for i in range(1, n + 1):
        diag = col.step(DT, surface=dict(wth=F_HEAT, wq=0.0, ustar=0.3))
        t_h.append(i * DT / 3600.0)
        h_col.append(diag["h"])
        hour = i * DT / 3600.0
        if hour in (1.0, 3.0, 6.0):
            snapshots[hour] = col.theta.copy()
        if hour == 4.0:
            # diagnose the turbulent heat-flux profile at the interfaces
            dth = np.diff(col.theta) / grid.dzc
            flux = -col.k_h_last[1:-1] * dth
            # counter-gradient contribution
            wstar = (9.81 / col.theta[0] * diag["wthv"] * diag["h"]) ** (1.0 / 3.0)
            w_m = (0.3**3 + 0.6 * wstar**3) ** (1.0 / 3.0)
            gamma_cg = col.cg_a * wstar * F_HEAT / (w_m**2 * diag["h"])
            zi_in = grid.zi[1:-1]
            inside = (zi_in > 0) & (zi_in < diag["h"])
            flux[inside] += col.k_h_last[1:-1][inside] * gamma_cg
            flux_profile = (np.concatenate([[F_HEAT], flux]), np.concatenate([[0.0], zi_in]))
    return dict(grid=grid, col=col, t=np.array(t_h), h=np.array(h_col),
                snapshots=snapshots, flux_profile=flux_profile)


def run_slab():
    slab = SlabCBL(h0=H0, theta0=THETA0, dtheta0=0.1, gamma_theta=GAMMA, beta=0.2)
    n = int(HOURS * 3600 / DT)
    t_h, h_s, th_s = [], [], []
    for i in range(1, n + 1):
        out = slab.step(DT, wth=F_HEAT)
        t_h.append(i * DT / 3600.0)
        h_s.append(out["h"])
        th_s.append(out["theta_m"])
    return dict(t=np.array(t_h), h=np.array(h_s), theta_m=np.array(th_s))


def main():
    apply_style()
    colres = run_column()
    slabres = run_slab()

    t_s = colres["t"] * 3600.0
    h_b02 = analytic_h(t_s, 0.2)
    h_b00 = analytic_h(t_s, 0.0)

    # effective entrainment ratio from the column heat-flux profile at 4 h
    flux, z_f = colres["flux_profile"]
    beta_eff = -float(flux.min()) / F_HEAT

    err_col = float(abs(colres["h"][-1] - h_b02[-1]) / h_b02[-1])
    err_slab = float(abs(slabres["h"][-1] - h_b02[-1]) / h_b02[-1])

    res = {
        "case": "dry CBL, constant surface heat flux 0.1 K m s-1, gamma 6 K km-1",
        "reference": {
            "growth_law": "h = sqrt(h0^2 + 2 (1+2 beta) F t / gamma), Tennekes 1973 / Garratt 1992",
            "beta_les": 0.2,
            "beta_les_source": "Sullivan et al. 1998 (LES consensus 0.15-0.25)",
        },
        "h_analytic_beta02_6h_m": round(float(h_b02[-1]), 1),
        "h_column_6h_m": round(float(colres["h"][-1]), 1),
        "h_slab_6h_m": round(float(slabres["h"][-1]), 1),
        "rel_error_column": round(err_col, 4),
        "rel_error_slab": round(err_slab, 4),
        "beta_effective_column": round(beta_eff, 3),
    }

    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.2))

    ax = axes[0]
    ax.plot(colres["t"], colres["h"], color=GOLDEN_SUN, label="K-profile column")
    ax.plot(slabres["t"], slabres["h"], color=SUN_CORE, label="slab (beta = 0.2)")
    ax.plot(colres["t"], h_b02, color=FG, ls="--", lw=1.2, label="analytic, beta = 0.2")
    ax.plot(colres["t"], h_b00, color=MUTED, ls=":", lw=1.2, label="encroachment (beta = 0)")
    ax.set_xlabel("time (h)")
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(a) CBL growth")
    ax.legend(fontsize=8.5)

    ax = axes[1]
    colors = {1.0: SKY_BLUE, 3.0: OCEAN_BLUE, 6.0: GOLDEN_SUN}
    z = colres["grid"].z
    theta_init = THETA0 + GAMMA * np.maximum(z - H0, 0.0)
    ax.plot(theta_init, z, color=MUTED, ls=":", lw=1.2, label="initial")
    for hour, th in colres["snapshots"].items():
        ax.plot(th, z, color=colors[hour], label=f"{hour:.0f} h")
    ax.set_xlabel("potential temperature (K)")
    ax.set_ylabel("height (m)")
    ax.set_title("(b) column theta profiles")
    ax.set_ylim(0, 1800)
    ax.set_xlim(287.5, 295)
    ax.legend(fontsize=8.5)

    ax = axes[2]
    ax.plot(flux / F_HEAT, z_f, color=GOLDEN_SUN)
    ax.axvline(0.0, color=MUTED, lw=0.8)
    ax.axvline(-0.2, color=FOREST_GREEN, ls="--", lw=1.2)
    ax.text(-0.19, 1500, "LES entrainment\nratio (-0.2)", color=FOREST_GREEN, fontsize=8.5)
    ax.set_xlabel("normalised heat flux  $\\overline{w\\theta}/\\overline{w\\theta}_s$")
    ax.set_ylabel("height (m)")
    ax.set_title("(c) heat-flux profile at 4 h")
    ax.set_ylim(0, 1800)

    fig.suptitle("Convective boundary-layer growth: column vs slab vs analytic", color=FG)
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_cbl.png")

    results_dir = Path(__file__).resolve().parent / "results"
    results_dir.mkdir(exist_ok=True)
    with open(results_dir / "cbl_metrics.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
