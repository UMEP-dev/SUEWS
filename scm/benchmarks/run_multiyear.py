"""Multi-year coupled climate run - where the native speedup pays off.

Three years of fully coupled urban land-atmosphere simulation (the 2012
KCL forcing year recycled as 2012-2014, leap day handled), entirely
through the native Fortran loop:

1. rural companion column, quasi-closed, hourly profile snapshots
   -> the ventilating background for the city;
2. urban column, ventilated, over the same three years.

Both columns carry SUEWS states spun up offline over 2012 with their own
configurations. At the measured Python-loop cost (~130 ms per coupled
step) this experiment would take ~23 hours; natively it takes seconds.

The output is an emergent three-year urban boundary-layer climatology:
the seasonal cycles of the screen-level heat island and of boundary-layer
depth are nowhere prescribed.

Usage: python benchmarks/run_multiyear.py
Outputs: benchmarks/results/multiyear.json, site/assets/fig_multiyear.png,
         site/assets/scm_data.js (data for the interactive site features)
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
from suews_scm.coupling import make_background  # noqa: E402
from benchmarks._style import (  # noqa: E402
    apply_style, GOLDEN_SUN, SUN_CORE, OCEAN_BLUE, SKY_BLUE, FOREST_GREEN, MUTED, FG,
)

import matplotlib.pyplot as plt  # noqa: E402

YEARS = [2012, 2013, 2014]
PY_MS_PER_STEP = None  # read from native_vs_python.json (measured)


def tile_forcing(df_forcing, years):
    """Recycle the 2012 forcing year over several calendar years."""
    blocks = []
    for year in years:
        df = df_forcing.copy()
        leap = (year % 4 == 0) and (year % 100 != 0 or year % 400 == 0)
        if not leap:
            df = df[~((df.index.month == 2) & (df.index.day == 29))]
        idx = pd.date_range(
            start=f"{year}-01-01 00:05", periods=len(df), freq="5min"
        )
        df = df.set_axis(idx)
        blocks.append(df)
    return pd.concat(blocks)


def _set(obj, attr, value):
    """Set a config field that may be a plain value or a RefValue wrapper."""
    cur = getattr(obj, attr)
    if hasattr(cur, "value"):
        cur.value = value
    else:
        setattr(obj, attr, value)


def _scale(obj, attr, factor):
    cur = getattr(obj, attr)
    if hasattr(cur, "working_day"):  # DayProfile
        for day in ("working_day", "holiday"):
            _scale(cur, day, factor)
        return
    tgt = cur.value if hasattr(cur, "value") else cur
    if isinstance(tgt, (list, tuple)):
        scaled = [v * factor for v in tgt]
    else:
        scaled = tgt * factor
    if hasattr(cur, "value"):
        cur.value = scaled
    else:
        setattr(obj, attr, scaled)


def make_rural_config(config):
    """Grass-dominated, low-roughness, no-QF, well-watered configuration
    (config-level equivalent of demos/_common.make_rural_state)."""
    cfg = config.model_copy(deep=True)
    site = cfg.sites[0]
    lc = site.properties.land_cover
    fractions = dict(paved=0.10, bldgs=0.0, evetr=0.0, dectr=0.15,
                     grass=0.70, bsoil=0.05, water=0.0)
    for name, frac in fractions.items():
        _set(getattr(lc, name), "sfr", frac)
    _set(site.properties, "z0m_in", 0.1)
    _set(site.properties, "zdm_in", 0.5)
    heat = site.properties.anthropogenic_emissions.heat
    for attr in ("qf_a", "qf_b", "qf_c", "qf0_beu"):
        _scale(heat, attr, 0.0)
    # well-watered vegetated soils (cf. the wet Bowen-ratio caveat)
    for name in ("grass", "dectr", "evetr", "bsoil"):
        cap = getattr(lc, name).soil_store_capacity
        cap_val = cap.value if hasattr(cap, "value") else cap
        _set(getattr(site.initial_states, name), "soilstore", 0.8 * float(cap_val))
    return cfg


def main():
    import supy as sp
    from supy.data_model import SUEWSConfig
    from supy._run_rust import run_suews_rust

    apply_style()
    cfg_path = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
    config_u = SUEWSConfig.from_yaml(cfg_path)
    config_r = make_rural_config(config_u)
    _, df_forcing = sp.load_sample_data()

    forcing3y = tile_forcing(df_forcing, YEARS)
    n_steps = len(forcing3y)
    print(f"[setup] {len(YEARS)} years = {n_steps} coupled steps")

    # ---------------- spin-up over the first year, per configuration
    t0 = time.perf_counter()
    _, state_u = run_suews_rust(config_u, df_forcing)
    _, state_r = run_suews_rust(config_r, df_forcing)
    t_spin = time.perf_counter() - t0
    print(f"[spin-up] two offline years in {t_spin:.1f} s")

    # Multi-season closure choices (documented):
    # - long-tail stability function (stable_fn=1): the sharp cut-off
    #   decouples the winter stable boundary layer and lets the near-surface
    #   air cool without bound - exactly the runaway that pushed operational
    #   NWP to long tails (cf. the GABLS line of work);
    # - the rural companion is synoptically anchored to the observed air
    #   state (tau = 24 h, so its own diurnal cycle survives): a closed
    #   column has no large-scale advection and drifts cold through winter.
    #   The urban column remains fully prognostic.
    season_kwargs = dict(stable_fn=1)

    # ---------------- rural companion: anchored, snapshots hourly
    t0 = time.perf_counter()
    df_out_r, df_scm_r, snaps_r, _ = run_coupled_native(
        config_r, forcing3y, state_json=state_r,
        snapshot_every_h=1.0, z0m_wind=0.1,
        obs_anchor_tau=86400.0, **season_kwargs,
    )
    t_rural = time.perf_counter() - t0
    print(f"[rural] 3-year coupled run in {t_rural:.1f} s")

    bg = make_background(snaps_r)

    # ---------------- urban column, ventilated, fully prognostic
    t0 = time.perf_counter()
    df_out_u, df_scm_u, snaps_u, _ = run_coupled_native(
        config_u, forcing3y, state_json=state_u,
        background=bg, snapshot_every_h=1.0, **season_kwargs,
    )
    t_urban = time.perf_counter() - t0
    print(f"[urban] 3-year coupled run in {t_urban:.1f} s")

    # ---------------- climatology
    t2_u = df_out_u["SUEWS"]["T2"].droplevel(0)
    t2_r = df_out_r["SUEWS"]["T2"].droplevel(0)
    uhi = (t2_u - t2_r).rename("uhi")
    hours = uhi.index.hour
    night = (hours >= 22) | (hours <= 5)
    uhi_noct_monthly = uhi[night].groupby(uhi[night].index.month).mean()
    h_u_daymax = df_scm_u["h_bl"].resample("1D").max()
    h_r_daymax = df_scm_r["h_bl"].resample("1D").max()

    # measured python per-step cost for the projection
    nat_json = Path(__file__).resolve().parent / "results" / "native_vs_python.json"
    py_ms = json.load(open(nat_json))["timing"]["python_ms_per_step"]
    t_total_native = t_rural + t_urban
    t_python_proj = 2.0 * n_steps * py_ms / 1000.0  # two columns, projected

    res = {
        "experiment": f"{len(YEARS)}-year coupled urban + rural companion "
                      "(2012 KCL forcing recycled; leap day handled)",
        "coupled_steps_total": int(2 * n_steps),
        "timing": {
            "native_rural_s": round(t_rural, 1),
            "native_urban_s": round(t_urban, 1),
            "native_total_s": round(t_total_native, 1),
            "spinup_offline_s": round(t_spin, 1),
            "python_projected_h": round(t_python_proj / 3600.0, 1),
            "python_ms_per_step_measured": py_ms,
            "effective_speedup": round(t_python_proj / t_total_native, 0),
            "sim_years_per_wallclock_min": round(
                2 * len(YEARS) / (t_total_native / 60.0), 1
            ),
        },
        "climatology": {
            "uhi2m_nocturnal_mean_K": round(float(uhi[night].mean()), 2),
            "uhi2m_nocturnal_jja_K": round(
                float(uhi[night & uhi.index.month.isin([6, 7, 8])].mean()), 2
            ),
            "uhi2m_nocturnal_djf_K": round(
                float(uhi[night & uhi.index.month.isin([12, 1, 2])].mean()), 2
            ),
            "h_bl_daymax_mean_urban_m": round(float(h_u_daymax.mean()), 0),
            "h_bl_daymax_mean_rural_m": round(float(h_r_daymax.mean()), 0),
            "h_bl_daymax_jja_urban_m": round(
                float(h_u_daymax[h_u_daymax.index.month.isin([6, 7, 8])].mean()), 0
            ),
            "h_bl_daymax_djf_urban_m": round(
                float(h_u_daymax[h_u_daymax.index.month.isin([12, 1, 2])].mean()), 0
            ),
        },
    }

    # ---------------- figure
    fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.0))

    ax = axes[0]
    t2m_u = t2_u.resample("1ME").mean()
    t2m_r = t2_r.resample("1ME").mean()
    ax.plot(t2m_u.index, t2m_u, color=SUN_CORE, label="urban")
    ax.plot(t2m_r.index, t2m_r, color=FOREST_GREEN, label="rural")
    ax.set_ylabel("monthly mean 2-m temperature (degC)")
    ax.set_title("(a) three emergent years")
    ax.legend(fontsize=8.5)

    ax = axes[1]
    ax.bar(uhi_noct_monthly.index, uhi_noct_monthly.values, color=GOLDEN_SUN, width=0.7)
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.set_xlabel("month")
    ax.set_ylabel("nocturnal heat island (K)")
    ax.set_title("(b) UHI seasonality (emergent)")
    ax.set_xticks(range(1, 13))

    ax = axes[2]
    bars = ax.bar(
        ["python loop\n(projected)", "native\n(measured)"],
        [t_python_proj, t_total_native],
        color=[SKY_BLUE, GOLDEN_SUN], width=0.55,
    )
    ax.set_yscale("log")
    ax.set_ylabel("wall-clock, 6 column-years (s, log)")
    ax.set_title("(c) the speedup at climate scale")
    labels = [f"{t_python_proj/3600.0:.0f} h", f"{t_total_native:.0f} s"]
    for bar, lab in zip(bars, labels):
        ax.text(bar.get_x() + bar.get_width()/2, bar.get_height()*1.15, lab,
                ha="center", color=FG, fontsize=10)

    fig.suptitle(
        "Three coupled years, urban + rural companion: native Fortran loop", color=FG
    )
    fig.tight_layout()

    assets = ROOT / "site" / "assets"
    assets.mkdir(parents=True, exist_ok=True)
    fig.savefig(assets / "fig_multiyear.png")

    results_dir = Path(__file__).resolve().parent / "results"
    results_dir.mkdir(exist_ok=True)
    with open(results_dir / "multiyear.json", "w") as f:
        json.dump(res, f, indent=2)
    print(json.dumps(res, indent=2))

    # ---------------- data export for the interactive site features
    # hourly profiles + diagnostics for the July heat-spell week of year 1
    t_lo, t_hi = pd.Timestamp("2012-07-22"), pd.Timestamp("2012-07-27")
    times = pd.DatetimeIndex(snaps_u["times"])
    sel = (times >= t_lo) & (times < t_hi)
    idx_sel = np.nonzero(sel)[0]
    prof_times = times[idx_sel]
    z = np.asarray(snaps_u["z"], dtype=float)
    keep = z <= 2600.0  # display window
    theta_u = np.array([snaps_u["theta"][i] for i in idx_sel])[:, keep]
    theta_r_all = np.array(
        [snaps_r["theta"][i] for i in idx_sel])[:, keep]
    h_series = df_scm_u["h_bl"].reindex(prof_times).to_numpy()
    t2_series = t2_u.reindex(prof_times).to_numpy()

    payload = {
        "z": [round(float(v), 1) for v in z[keep]],
        "times": [t.strftime("%d %b %H:%M") for t in prof_times],
        "theta_urban": [[round(float(v), 2) for v in row] for row in theta_u],
        "theta_rural": [[round(float(v), 2) for v in row] for row in theta_r_all],
        "h_bl": [round(float(v), 0) if np.isfinite(v) else None for v in h_series],
        "t2": [round(float(v), 1) if np.isfinite(v) else None for v in t2_series],
    }
    with open(assets / "scm_data.js", "w") as f:
        f.write("window.SCM_DATA = ")
        json.dump(payload, f, separators=(",", ":"))
        f.write(";\n")
    print(f"site data: {len(prof_times)} hourly profiles x {int(keep.sum())} levels "
          f"-> assets/scm_data.js")


if __name__ == "__main__":
    main()
