"""Demo 2: how much does anthropogenic heat warm the urban atmosphere?

Three coupled runs of the KCL London configuration with the Jarvi et
al. (2011) anthropogenic heat coefficients scaled by 0, 1 and 2 -
representing decarbonised/low-energy, present-day and intensified
energy use. Everything else (radiation, wind constraint, land cover)
identical. The temperature response at 40 m is fully emergent.

Usage: python demos/demo_anthropogenic_heat.py
Outputs: demos/results/demo_qf.json, site/assets/fig_demo_qf.png
"""

import json
from pathlib import Path

import numpy as np

from _common import (
    EVAL_START, get_rural_run, get_sample_and_state, run_coupled,
    scale_anthropogenic_heat,
)

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, MUTED, FG,
)
import matplotlib.pyplot as plt  # noqa: E402
import matplotlib.dates as mdates  # noqa: E402


def main():
    apply_style()
    state, df_forcing = get_sample_and_state()
    _, bg = get_rural_run(state, df_forcing)

    runs = {}
    for factor, label in [(0.0, "QF x0"), (1.0, "QF x1"), (2.0, "QF x2")]:
        print(f"running {label}...")
        df, _ = run_coupled(
            scale_anthropogenic_heat(state, factor), df_forcing, background=bg
        )
        runs[label] = df

    base = runs["QF x1"].loc[EVAL_START:]
    ev = {k: v.loc[EVAL_START:] for k, v in runs.items()}
    hours = base.index.hour
    night = (hours >= 22) | (hours <= 5)

    d0 = ev["QF x0"]["t2_mod"] - base["t2_mod"]
    d2 = ev["QF x2"]["t2_mod"] - base["t2_mod"]

    res = {
        "case": "anthropogenic heat scaling (0x, 1x, 2x), KCL London, "
                "23-26 July 2012, coupled SCM",
        "qf_mean_x1_wm2": round(float(base["qf"].mean()), 1),
        "dT_x0_mean_K": round(float(d0.mean()), 2),
        "dT_x0_nocturnal_K": round(float(d0[night].mean()), 2),
        "dT_x2_mean_K": round(float(d2.mean()), 2),
        "dT_x2_nocturnal_K": round(float(d2[night].mean()), 2),
        "sensitivity_K_per_10Wm2": round(
            float(d2[night].mean() / max((ev["QF x2"]["qf"] - base["qf"])[night].mean(), 0.1) * 10.0), 3
        ),
    }

    colors = {"QF x0": SKY_BLUE, "QF x1": GOLDEN_SUN, "QF x2": SUN_CORE}
    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.0))

    ax = axes[0]
    for label, df in runs.items():
        ax.plot(df.index, df["qf"], color=colors[label], label=label, lw=1.3)
    ax.set_ylabel("anthropogenic heat flux (W m$^{-2}$)")
    ax.set_title("(a) imposed QF scenarios")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[1]
    hr = base.index.hour + base.index.minute / 60.0
    for series, label, color in [(d0, "QF x0 - x1", SKY_BLUE), (d2, "QF x2 - x1", SUN_CORE)]:
        comp = series.groupby(np.floor(hr)).mean()
        ax.plot(comp.index, comp.values, color=color, label=label)
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.set_xlabel("hour (local standard time)")
    ax.set_ylabel("temperature response (K)")
    ax.set_title("(b) diurnal screen-level temperature response")
    ax.legend(fontsize=8.5)

    ax = axes[2]
    for label, df in runs.items():
        ax.plot(df.index, df["h_bl"], color=colors[label], label=label, lw=1.2)
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(c) boundary-layer response")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    fig.suptitle("Anthropogenic heat and urban air temperature: coupled SUEWS-SCM", color=FG)
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_demo_qf.png")

    results = Path(__file__).resolve().parent / "results"
    results.mkdir(exist_ok=True)
    with open(results / "demo_qf.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
