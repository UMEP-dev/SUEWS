"""Demo 3: can cool roofs cool the air a city breathes?

Two coupled runs of the KCL London configuration: bulk building-facet
albedo at its baseline (0.12) versus a high-albedo "cool roof"
scenario (0.55). With the atmosphere coupled, reflecting shortwave at
the surface propagates through the whole column: storage and sensible
heat fall, the boundary layer shallows, and the air itself cools -
an effect an offline (prescribed-forcing) run cannot represent.

Note the bulk-surface approximation: SUEWS's building facet aggregates
roofs and walls, so 0.55 is an upper-bound scenario.

Usage: python demos/demo_cool_roofs.py
Outputs: demos/results/demo_cool_roofs.json, site/assets/fig_demo_cool_roofs.png
"""

import json
from pathlib import Path

import numpy as np

from _common import (
    EVAL_START, get_rural_run, get_sample_and_state, run_coupled,
    set_building_albedo,
)

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, MUTED, FG,
)
import matplotlib.pyplot as plt  # noqa: E402
import matplotlib.dates as mdates  # noqa: E402

ALB_BASE = 0.12
ALB_COOL = 0.55


def main():
    apply_style()
    state, df_forcing = get_sample_and_state()
    _, bg = get_rural_run(state, df_forcing)

    print("running baseline albedo...")
    df_b, _ = run_coupled(set_building_albedo(state, ALB_BASE), df_forcing, background=bg)
    print("running cool-roof albedo...")
    df_c, _ = run_coupled(set_building_albedo(state, ALB_COOL), df_forcing, background=bg)

    ev_b = df_b.loc[EVAL_START:]
    ev_c = df_c.loc[EVAL_START:]
    dtair = ev_c["t2_mod"] - ev_b["t2_mod"]
    kdown = df_forcing["kdown"].loc[ev_b.index]
    day = kdown > 20.0

    res = {
        "case": f"building-facet albedo {ALB_BASE} -> {ALB_COOL}, KCL London, "
                "23-26 July 2012, coupled SCM",
        "dT_daytime_mean_K": round(float(dtair[day].mean()), 2),
        "dT_max_cooling_K": round(float(dtair.min()), 2),
        "dQn_daytime_mean_wm2": round(float((ev_c["qn"] - ev_b["qn"])[day].mean()), 1),
        "dQh_daytime_mean_wm2": round(float((ev_c["qh"] - ev_b["qh"])[day].mean()), 1),
        "dQs_daytime_mean_wm2": round(float((ev_c["qs"] - ev_b["qs"])[day].mean()), 1),
        "bl_height_max_base_m": round(float(ev_b["h_bl"].max()), 0),
        "bl_height_max_cool_m": round(float(ev_c["h_bl"].max()), 0),
    }

    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.0))

    ax = axes[0]
    hr = ev_b.index.hour + ev_b.index.minute / 60.0
    comp = dtair.groupby(np.floor(hr)).mean()
    ax.fill_between(comp.index, comp.values, color=OCEAN_BLUE, alpha=0.35)
    ax.plot(comp.index, comp.values, color=OCEAN_BLUE)
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.set_xlabel("hour (local standard time)")
    ax.set_ylabel("temperature change (K)")
    ax.set_title("(a) screen-level air-temperature response")

    ax = axes[1]
    for var, color, label in [("qn", GOLDEN_SUN, "net radiation"),
                              ("qs", SUN_CORE, "storage heat"),
                              ("qh", OCEAN_BLUE, "sensible heat")]:
        delta = (ev_c[var] - ev_b[var]).groupby(np.floor(hr)).mean()
        ax.plot(delta.index, delta.values, color=color, label=label)
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.set_xlabel("hour (local standard time)")
    ax.set_ylabel("flux change (W m$^{-2}$)")
    ax.set_title("(b) energy-balance response")
    ax.legend(fontsize=8.5)

    ax = axes[2]
    ax.plot(df_b.index, df_b["h_bl"], color=GOLDEN_SUN, label=f"albedo {ALB_BASE}")
    ax.plot(df_c.index, df_c["h_bl"], color=OCEAN_BLUE, label=f"albedo {ALB_COOL}")
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(c) boundary-layer response")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    fig.suptitle("Cool-roof scenario: coupled SUEWS-SCM", color=FG)
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_demo_cool_roofs.png")

    results = Path(__file__).resolve().parent / "results"
    results.mkdir(exist_ok=True)
    with open(results / "demo_cool_roofs.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
