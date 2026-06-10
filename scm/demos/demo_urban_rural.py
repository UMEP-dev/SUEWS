"""Demo 1: what does urbanisation do to the boundary layer?

Two coupled SCM runs over the same clear July 2012 episode with
identical radiation, precipitation and wind constraint:

- ``urban``: the KCL central-London configuration (43 % paved,
  38 % buildings, anthropogenic heat on);
- ``rural``: a grass-dominated configuration (70 % grass, 15 %
  deciduous trees) with no anthropogenic heat and low roughness.

Because air temperature and humidity are fully prognostic in the
coupled system, the urban-rural near-surface temperature difference -
the urban heat island - and the contrast in boundary-layer depth are
*emergent* properties, not prescribed ones.

Usage: python demos/demo_urban_rural.py
Outputs: demos/results/demo_urban_rural.json, site/assets/fig_demo_urban_rural.png
"""

import json
from pathlib import Path

import numpy as np

from _common import (
    EVAL_START, get_rural_run, get_sample_and_state, run_coupled,
)

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, FOREST_GREEN, MUTED, FG,
)
import matplotlib.pyplot as plt  # noqa: E402
import matplotlib.dates as mdates  # noqa: E402


def main():
    apply_style()
    state, df_forcing = get_sample_and_state()

    print("running rural (closed, provides background air)...")
    df_r, bg = get_rural_run(state, df_forcing)
    print("running urban (ventilated by rural background)...")
    df_u, _ = run_coupled(state, df_forcing, z0m_wind=1.0, background=bg)

    ev_u = df_u.loc[EVAL_START:]
    ev_r = df_r.loc[EVAL_START:]
    uhi2 = ev_u["t2_mod"] - ev_r["t2_mod"]  # canonical screen-level UHI
    uhi40 = ev_u["tair_mod"] - ev_r["tair_mod"]  # at the forcing height
    hours = ev_u.index.hour
    night = (hours >= 22) | (hours <= 5)

    res = {
        "case": "urban (KCL London) vs rural (grassland) under identical "
                "synoptic forcing, 23-26 July 2012, coupled SCM",
        "uhi2m_mean_nocturnal_K": round(float(uhi2[night].mean()), 2),
        "uhi2m_max_K": round(float(uhi2.max()), 2),
        "uhi2m_mean_K": round(float(uhi2.mean()), 2),
        "uhi40m_mean_nocturnal_K": round(float(uhi40[night].mean()), 2),
        "bl_height_max_urban_m": round(float(ev_u["h_bl"].max()), 0),
        "bl_height_max_rural_m": round(float(ev_r["h_bl"].max()), 0),
        "qh_daytime_mean_urban": round(float(ev_u["qh"][ev_u["qn"] > 50].mean()), 1),
        "qh_daytime_mean_rural": round(float(ev_r["qh"][ev_r["qn"] > 50].mean()), 1),
        "qe_daytime_mean_urban": round(float(ev_u["qe"][ev_u["qn"] > 50].mean()), 1),
        "qe_daytime_mean_rural": round(float(ev_r["qe"][ev_r["qn"] > 50].mean()), 1),
    }

    fig, axes = plt.subplots(2, 2, figsize=(11.5, 7.2))

    ax = axes[0, 0]
    ax.plot(df_u.index, df_u["t2_mod"], color=SUN_CORE, label="urban")
    ax.plot(df_r.index, df_r["t2_mod"], color=FOREST_GREEN, label="rural")
    ax.set_ylabel("screen-level air temperature (degC)")
    ax.set_title("(a) emergent air temperature (2 m)")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[0, 1]
    hr = ev_u.index.hour + ev_u.index.minute / 60.0
    comp2 = uhi2.groupby(np.floor(hr)).mean()
    comp40 = uhi40.groupby(np.floor(hr)).mean()
    ax.fill_between(comp2.index, comp2.values, color=SUN_CORE, alpha=0.35)
    ax.plot(comp2.index, comp2.values, color=SUN_CORE, label="2 m (screen level)")
    ax.plot(comp40.index, comp40.values, color=SKY_BLUE, lw=1.3, label="40 m")
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.set_xlabel("hour (local standard time)")
    ax.set_ylabel("urban - rural temperature (K)")
    ax.set_title("(b) emergent urban heat island cycle")
    ax.legend(fontsize=8.5)

    ax = axes[1, 0]
    ax.plot(df_u.index, df_u["h_bl"], color=SUN_CORE, label="urban")
    ax.plot(df_r.index, df_r["h_bl"], color=FOREST_GREEN, label="rural")
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(c) boundary-layer depth")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[1, 1]
    for df_x, color, label in [(df_u, SUN_CORE, "urban"), (df_r, FOREST_GREEN, "rural")]:
        ax.plot(df_x.index, df_x["qh"], color=color, lw=1.3, label=f"QH {label}")
        ax.plot(df_x.index, df_x["qe"], color=color, lw=1.0, ls=":", label=f"QE {label}")
    ax.set_ylabel("flux (W m$^{-2}$)")
    ax.set_title("(d) surface turbulent fluxes")
    ax.legend(fontsize=8, ncol=2)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    fig.suptitle("Urbanisation and the boundary layer: coupled SUEWS-SCM experiment", color=FG)
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_demo_urban_rural.png")

    results = Path(__file__).resolve().parent / "results"
    results.mkdir(exist_ok=True)
    with open(results / "demo_urban_rural.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
