"""Coupled SUEWS-column benchmark against KCL (London) observations.

The bundled SuPy sample is the KCL site (central London) for 2012 with
observation-based forcing at 40 m. Here SUEWS is spun up offline from
1 January, then the two-way coupled SCM is run over the clearest
4-5 day summer window (22-27 July 2012, selected by maximum 4-day mean
incoming shortwave). In coupled mode the air temperature, humidity and
(nudged) wind that SUEWS sees come from the column, NOT from the
observations -- so comparing the modelled forcing-height air
temperature against the observed one is a genuine test of the coupled
land-atmosphere system (cf. the BLUEWS evaluation strategy of
Onomura et al. 2015, Urban Climate 11:1-23).

The first simulated day is treated as column spin-up and excluded from
the metrics.

Usage: python benchmarks/run_coupled_kcl.py
Outputs: benchmarks/results/coupled_kcl_metrics.json,
         site/assets/fig_coupled_kcl.png
"""

import json
import sys
import warnings
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

warnings.filterwarnings("ignore")

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
EVAL_START = "2012-07-23"  # first day excluded as column spin-up
CITY_LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 15000.0  # m
TAG = "" if CITY_LENGTH == 15000.0 else f"_L{int(CITY_LENGTH/1000)}km"


def metrics(obs, mod):
    err = mod - obs
    return dict(
        rmse=float(np.sqrt(np.mean(err**2))),
        mbe=float(np.mean(err)),
        r=float(np.corrcoef(obs, mod)[0, 1]),
        n=int(len(err)),
    )


def main():
    import supy as sp

    apply_style()
    print("[1/3] SUEWS offline spin-up to", SPINUP_END, "(cached)")
    state, df_forcing = get_sample_and_state()

    print("[2/3] rural background + coupled urban run", RUN_START, "->", RUN_END)
    df_rural, bg = get_rural_run(state, df_forcing)
    scm = CoupledSCM(state.copy(), df_forcing, background=bg, city_length=CITY_LENGTH)
    df, snaps = scm.run(RUN_START, RUN_END)

    print("[3/3] offline reference run (observed forcing)")
    df_off, _ = sp.run_supy(
        df_forcing.loc[RUN_START:RUN_END], state.copy(), logging_level=50
    )
    off = df_off["SUEWS"].droplevel(0)

    ev = df.loc[EVAL_START:]
    kdown = df_forcing["kdown"].loc[ev.index]
    day = kdown > 20.0

    res = {
        "site": "KCL, central London (SuPy sample), forcing height 40 m",
        "episode": f"{RUN_START} to {RUN_END} (clearest 4-day summer window; "
                   f"day 1 excluded from metrics as spin-up)",
        "configuration": "two-way coupled T and q; wind nudged to observations "
                         "(tau = 30 min); radiation and precipitation prescribed; "
                         "clear-sky radiative cooling 2 K/day; urban column "
                         "ventilated by a companion rural column "
                         "(tau_adv = 15 km / U)",
        "tau_adv_mean_s": None,  # filled below
        "tair": {
            "all": metrics(ev["tair_obs"], ev["tair_mod"]),
            "daytime": metrics(ev.loc[day, "tair_obs"], ev.loc[day, "tair_mod"]),
            "nighttime": metrics(ev.loc[~day, "tair_obs"], ev.loc[~day, "tair_mod"]),
            "obs_diurnal_amplitude": float(
                ev["tair_obs"].groupby(ev.index.date).agg(np.ptp).mean()
            ),
            "mod_diurnal_amplitude": float(
                ev["tair_mod"].groupby(ev.index.date).agg(np.ptp).mean()
            ),
        },
        "rh": {"all": metrics(ev["rh_obs"], ev["rh_mod"])},
        "qh_coupled_vs_offline_rmse": float(
            np.sqrt(np.mean((df["qh"] - off["QH"].reindex(df.index)) ** 2))
        ),
        "qe_coupled_vs_offline_rmse": float(
            np.sqrt(np.mean((df["qe"] - off["QE"].reindex(df.index)) ** 2))
        ),
        "h_bl_max_m": float(df["h_bl"].resample("1D").max().mean()),
        "lineage": "coupling strategy follows BLUEWS / SUEWS-CBL "
                   "(Onomura et al. 2015, Urban Climate 11:1-23), extended "
                   "with a rural-background ventilation term",
    }
    res["tau_adv_mean_s"] = round(float(df["tau_adv"].mean()), 0)

    # ----------------------------------------------------------- figure
    fig, axes = plt.subplots(2, 2, figsize=(11.5, 7.2))

    ax = axes[0, 0]
    ax.plot(df.index, df["tair_obs"], color=MUTED, lw=1.2, label="observed")
    ax.plot(df.index, df["tair_mod"], color=GOLDEN_SUN, lw=1.4, label="coupled SCM")
    ax.axvspan(df.index[0], pd.Timestamp(EVAL_START), color="#000000", alpha=0.25)
    ax.text(df.index[10], ax.get_ylim()[0] + 0.5, "spin-up", fontsize=8, color=MUTED)
    ax.set_ylabel("air temperature at 40 m (degC)")
    ax.set_title("(a) modelled vs observed air temperature")
    ax.legend(fontsize=8.5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    ax = axes[0, 1]
    hours = ev.index.hour + ev.index.minute / 60.0
    comp_obs = ev["tair_obs"].groupby(np.floor(hours)).mean()
    comp_mod = ev["tair_mod"].groupby(np.floor(hours)).mean()
    ax.plot(comp_obs.index, comp_obs, color=MUTED, label="observed")
    ax.plot(comp_mod.index, comp_mod, color=GOLDEN_SUN, label="coupled SCM")
    ax.set_xlabel("hour (local standard time)")
    ax.set_ylabel("air temperature (degC)")
    ax.set_title("(b) mean diurnal cycle (eval days)")
    ax.legend(fontsize=8.5)

    ax = axes[1, 0]
    ax.plot(df.index, df["h_bl"], color=SKY_BLUE)
    ax.set_ylabel("boundary-layer height (m)")
    ax.set_title("(c) diagnosed boundary-layer height")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))
    ax2 = ax.twinx()
    ax2.fill_between(df.index, df_forcing["kdown"].loc[df.index], color=GOLDEN_SUN, alpha=0.15, lw=0)
    ax2.set_ylabel("incoming shortwave (W m$^{-2}$)", color=MUTED)
    ax2.grid(False)

    ax = axes[1, 1]
    ax.plot(df.index, df["qh"], color=SUN_CORE, lw=1.2, label="QH coupled")
    ax.plot(off.index, off["QH"], color=SUN_CORE, lw=1.0, ls=":", alpha=0.8, label="QH offline")
    ax.plot(df.index, df["qe"], color=OCEAN_BLUE, lw=1.2, label="QE coupled")
    ax.plot(off.index, off["QE"], color=OCEAN_BLUE, lw=1.0, ls=":", alpha=0.8, label="QE offline")
    ax.set_ylabel("flux (W m$^{-2}$)")
    ax.set_title("(d) surface fluxes: coupled vs offline SUEWS")
    ax.legend(fontsize=8, ncol=2)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))

    fig.suptitle(
        "Coupled SUEWS-SCM vs observations, KCL London, 22-27 July 2012", color=FG
    )
    fig.tight_layout()

    assets = Path(__file__).resolve().parents[1] / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / f"fig_coupled_kcl{TAG}.png")

    results_dir = Path(__file__).resolve().parent / "results"
    results_dir.mkdir(exist_ok=True)
    with open(results_dir / f"coupled_kcl_metrics{TAG}.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))

    df.to_parquet(results_dir / f"coupled_kcl_diag{TAG}.parquet")


if __name__ == "__main__":
    main()
