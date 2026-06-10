"""Native Fortran-loop SCM vs the Python reference: equivalence + performance.

Runs the full coupled KCL episode (22-27 July 2012, ventilated by the rural
companion background) through both backends:

- Python reference: ``CoupledSCM`` calling ``run_supy`` once per 5-min step
  (the validated implementation behind the benchmarks and demos);
- native: one call into the Fortran timestep loop via the Rust bridge
  (``run_suews_scm``), the column advancing inside ``SUEWS_cal_multitsteps_scm``.

Reports wall-clock for both, the speedup, agreement between the two
backends, and each backend's skill against the KCL observations.

Usage: python benchmarks/run_native_vs_python.py
Outputs: benchmarks/results/native_vs_python.json,
         site/assets/fig_native.png
"""

import json
import sys
import time
import warnings
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

warnings.filterwarnings("ignore")

from suews_scm.native import run_coupled_native  # noqa: E402
from suews_scm.coupling import CoupledSCM  # noqa: E402
from demos._common import get_rural_run, get_sample_and_state  # noqa: E402
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, FOREST_GREEN, MUTED, FG,
)

import matplotlib.pyplot as plt  # noqa: E402
import matplotlib.dates as mdates  # noqa: E402

SPINUP_END = "2012-07-21 23:55"
RUN_START = "2012-07-22 00:05"
RUN_END = "2012-07-27 00:00"
EVAL_START = "2012-07-23"


def main():
    import supy as sp
    from supy.data_model import SUEWSConfig
    from supy._run_rust import run_suews_rust

    apply_style()
    cfg_path = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
    config = SUEWSConfig.from_yaml(cfg_path)
    state_py, df_forcing = get_sample_and_state()  # python-side spun state (cached)
    df_rural, bg = get_rural_run(state_py, df_forcing)  # cached rural background

    # ------------------------------------------------ native backend
    print("[native] offline spin-up via bridge ...")
    t0 = time.perf_counter()
    _, state_json = run_suews_rust(config, df_forcing.loc[:SPINUP_END])
    t_spin = time.perf_counter() - t0

    window = df_forcing.loc[RUN_START:RUN_END]
    print(f"[native] coupled run over {len(window)} steps ...")
    t0 = time.perf_counter()
    df_out_n, df_scm_n, _ = run_coupled_native(
        config, window, state_json=state_json, background=bg
    )
    t_native = time.perf_counter() - t0

    # ------------------------------------------------ python reference
    print("[python] coupled run (per-step run_supy) ...")
    t0 = time.perf_counter()
    scm = CoupledSCM(state_py.copy(), df_forcing, background=bg)
    df_p, _ = scm.run(RUN_START, RUN_END)
    t_python = time.perf_counter() - t0

    # ------------------------------------------------ agreement + skill
    obs = window["Tair"]
    d_back = (df_scm_n["tair_mod"] - df_p["tair_mod"]).abs()

    def skill(series):
        ev = series.loc[EVAL_START:]
        ev_obs = obs.loc[ev.index]
        err = ev - ev_obs
        return dict(
            rmse=float(np.sqrt(np.mean(err**2))),
            mbe=float(np.mean(err)),
            r=float(np.corrcoef(ev_obs, ev)[0, 1]),
        )

    res = {
        "episode": f"{RUN_START} -> {RUN_END} (KCL London, ventilated coupled run)",
        "steps": int(len(window)),
        "timing": {
            "native_coupled_s": round(t_native, 2),
            "python_coupled_s": round(t_python, 2),
            "speedup": round(t_python / t_native, 1),
            "native_ms_per_step": round(1000.0 * t_native / len(window), 2),
            "python_ms_per_step": round(1000.0 * t_python / len(window), 1),
            "native_spinup_s": round(t_spin, 2),
            "note": "same machine, same debug-profile SUEWS library in both "
                    "backends; difference is loop placement and marshalling",
        },
        "backend_agreement": {
            "tair_abs_diff_max_K": round(float(d_back.max()), 3),
            "tair_abs_diff_mean_K": round(float(d_back.mean()), 3),
            "h_bl_abs_diff_max_m": round(
                float((df_scm_n["h_bl"] - df_p["h_bl"]).abs().max()), 1
            ),
            "note": "the backends start from independently spun-up states "
                    "(bridge cold-start vs classic sample state) and the "
                    "python reference does per-step state round-trips, so "
                    "trajectories diverge while skill statistics remain "
                    "indistinguishable; the 6-h lockstep test in "
                    "tests/test_native.py pins the column physics itself",
        },
        "skill_vs_obs": {
            "native": skill(df_scm_n["tair_mod"]),
            "python": skill(df_p["tair_mod"]),
        },
    }

    # ------------------------------------------------ figure
    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.0))

    ax = axes[0]
    ax.plot(window.index, obs, color=MUTED, lw=1.1, label="observed")
    ax.plot(df_p.index, df_p["tair_mod"], color=SKY_BLUE, lw=2.2, alpha=0.9,
            label="python reference")
    ax.plot(df_scm_n.index, df_scm_n["tair_mod"], color=GOLDEN_SUN, lw=0.9,
            label="native (Fortran loop)")
    ax.set_ylabel("air temperature at 40 m (degC)")
    ax.set_title("(a) the two backends overlie")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[1]
    ax.plot(d_back.index, d_back, color=OCEAN_BLUE, lw=0.9)
    ax.set_ylabel("|native - python| (K)")
    ax.set_title("(b) backend difference")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[2]
    bars = ax.bar(
        ["python\nloop", "native\nFortran loop"],
        [t_python, t_native],
        color=[SKY_BLUE, GOLDEN_SUN], width=0.55,
    )
    ax.set_yscale("log")
    ax.set_ylabel("wall-clock for 5 coupled days (s, log)")
    ax.set_title("(c) performance")
    for bar, val in zip(bars, [t_python, t_native]):
        ax.text(bar.get_x() + bar.get_width()/2, val*1.15, f"{val:.1f} s",
                ha="center", color=FG, fontsize=10)
    ax.text(0.5, 0.5, f"{t_python/t_native:.0f}x",
            transform=ax.transAxes, ha="center", fontsize=26,
            color=GOLDEN_SUN, fontweight="bold")

    fig.suptitle("Coupled SCM: native Fortran loop vs Python reference", color=FG)
    fig.tight_layout()

    assets = ROOT / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_native.png")

    results_dir = Path(__file__).resolve().parent / "results"
    results_dir.mkdir(exist_ok=True)
    with open(results_dir / "native_vs_python.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
