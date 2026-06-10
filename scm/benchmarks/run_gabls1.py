"""GABLS1 stable boundary-layer benchmark.

Case definition (Beare et al. 2006; Cuxart et al. 2006, Boundary-Layer
Meteorology): barotropic stable boundary layer, geostrophic wind
8 m s-1, Coriolis parameter 1.39e-4 s-1 (73 deg N), initial potential
temperature 265 K mixed up to 100 m with a 0.01 K m-1 lapse above, and
a prescribed surface cooling of 0.25 K h-1 over 9 hours; z0 = 0.1 m.

Reference: the LES ensemble reaches a quasi-steady state after ~8-9 h
with a boundary-layer depth of about 150-250 m (~200 m for the 1-2 m
resolution runs), a low-level jet near the boundary-layer top, and
surface friction velocity of about 0.25-0.30 m s-1. The SCM
intercomparison (Cuxart et al. 2006) showed that schemes with
"long-tail" stability functions over-deepen the SBL, while sharper
cut-off functions reproduce the LES ensemble - both behaviours are
demonstrated here.

Usage: python benchmarks/run_gabls1.py
Outputs: benchmarks/results/gabls1_metrics.json, site/assets/fig_gabls1.png
"""

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from suews_scm import ColumnModel, Grid  # noqa: E402
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, FOREST_GREEN, MUTED, FG,
)

import matplotlib.pyplot as plt  # noqa: E402


def run_case(stable_fn="sharp", dt=10.0, hours=9.0):
    grid = Grid(dz0=6.25, ztop=400.0, stretch=1.0)
    col = ColumnModel(
        grid,
        f_coriolis=1.39e-4,
        ug=8.0,
        vg=0.0,
        z0m=0.1,
        p_sfc=1.0e5,
        t_ref=263.0,
        lambda_mix=40.0,
        stable_fn=stable_fn,
    )
    theta0 = np.where(grid.z <= 100.0, 265.0, 265.0 + 0.01 * (grid.z - 100.0))
    col.set_initial_profiles(theta=theta0, q=np.zeros(grid.n), u=np.full(grid.n, 8.0), v=np.zeros(grid.n))

    nstep = int(hours * 3600 / dt)
    t_hist, h_hist, ustar_hist, wth_hist = [], [], [], []
    for i in range(1, nstep + 1):
        t = i * dt
        theta_s = 265.0 - 0.25 * t / 3600.0
        diag = col.step(dt, surface=dict(theta_s=theta_s))
        if i % 30 == 0:
            t_hist.append(t / 3600.0)
            h_hist.append(diag["h"])
            ustar_hist.append(diag["ustar"])
            wth_hist.append(diag["wth"])

    # stress-based BL height as used in the GABLS intercomparisons:
    # h = z(tau = 0.05 tau_sfc) / 0.95
    du = np.gradient(col.u, grid.z)
    dv = np.gradient(col.v, grid.z)
    k_m_c = 0.5 * (col.k_m_last[:-1] + col.k_m_last[1:])
    stress = k_m_c * np.hypot(du, dv)
    tau_s = stress[0]
    idx = np.nonzero(stress <= 0.05 * tau_s)[0]
    h_stress = grid.z[idx[0]] / 0.95 if idx.size else np.nan

    wind = np.hypot(col.u, col.v)
    k_jet = int(np.argmax(wind))
    return dict(
        col=col,
        grid=grid,
        t=np.array(t_hist),
        h=np.array(h_hist),
        ustar=np.array(ustar_hist),
        wth=np.array(wth_hist),
        h_stress=float(h_stress),
        jet_height=float(grid.z[k_jet]),
        jet_speed=float(wind[k_jet]),
    )


def main():
    apply_style()
    out_sharp = run_case("sharp")
    out_tail = run_case("long_tail")

    res = {
        "case": "GABLS1 (Beare et al. 2006; Cuxart et al. 2006)",
        "reference": {
            "h_les_range_m": [150, 250],
            "h_les_typical_m": 200,
            "ustar_les_range_ms": [0.25, 0.30],
            "notes": "LES ensemble, quasi-steady after 8-9 h; "
                     "long-tail SCM schemes over-deepen the SBL",
        },
        "scm_sharp": {
            "h_ri_m": round(float(out_sharp["h"][-1]), 1),
            "h_stress_m": round(out_sharp["h_stress"], 1),
            "ustar_ms": round(float(out_sharp["ustar"][-1]), 3),
            "sfc_heat_flux_Kms": round(float(out_sharp["wth"][-1]), 4),
            "jet_height_m": round(out_sharp["jet_height"], 1),
            "jet_speed_ms": round(out_sharp["jet_speed"], 2),
        },
        "scm_long_tail": {
            "h_ri_m": round(float(out_tail["h"][-1]), 1),
            "h_stress_m": round(out_tail["h_stress"], 1),
            "ustar_ms": round(float(out_tail["ustar"][-1]), 3),
            "jet_height_m": round(out_tail["jet_height"], 1),
            "jet_speed_ms": round(out_tail["jet_speed"], 2),
        },
    }

    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.2))

    ax = axes[0]
    for out, color, label in [
        (out_sharp, GOLDEN_SUN, "SCM sharp cut-off"),
        (out_tail, SKY_BLUE, "SCM long-tail"),
    ]:
        ax.plot(out["col"].theta, out["grid"].z, color=color, label=label)
    z = out_sharp["grid"].z
    theta0 = np.where(z <= 100.0, 265.0, 265.0 + 0.01 * (z - 100.0))
    ax.plot(theta0, z, color=MUTED, ls=":", lw=1.2, label="initial")
    ax.axhspan(150, 250, color=FOREST_GREEN, alpha=0.12)
    ax.text(263.05, 252, "LES ensemble h range", color=FOREST_GREEN, fontsize=8.5)
    ax.set_xlabel("potential temperature (K)")
    ax.set_ylabel("height (m)")
    ax.set_title("(a) theta after 9 h")
    ax.set_ylim(0, 400)
    ax.legend(fontsize=8.5, loc="upper left")

    ax = axes[1]
    for out, color, label in [
        (out_sharp, GOLDEN_SUN, "sharp"),
        (out_tail, SKY_BLUE, "long-tail"),
    ]:
        wind = np.hypot(out["col"].u, out["col"].v)
        ax.plot(wind, out["grid"].z, color=color, label=label)
    ax.axvline(8.0, color=MUTED, ls=":", lw=1.2)
    ax.text(8.05, 360, "geostrophic", color=MUTED, fontsize=8.5, rotation=90, va="top")
    ax.set_xlabel("wind speed (m s$^{-1}$)")
    ax.set_title("(b) wind after 9 h (low-level jet)")
    ax.set_ylim(0, 400)

    ax = axes[2]
    ax.plot(out_sharp["t"], out_sharp["h"], color=GOLDEN_SUN, label="sharp")
    ax.plot(out_tail["t"], out_tail["h"], color=SKY_BLUE, label="long-tail")
    ax.axhspan(150, 250, color=FOREST_GREEN, alpha=0.12)
    ax.set_xlabel("time (h)")
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(c) SBL depth evolution")
    ax.set_ylim(0, 400)
    ax.legend(fontsize=8.5)

    fig.suptitle("GABLS1 stable boundary layer: suews-scm column vs LES ensemble", color=FG)
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_gabls1.png")

    results_dir = Path(__file__).resolve().parent / "results"
    results_dir.mkdir(exist_ok=True)
    with open(results_dir / "gabls1_metrics.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
