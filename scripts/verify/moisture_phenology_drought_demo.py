"""GH-1292 PR3 drought demonstration figure (four-panel).

Produces a single four-panel PNG that shows both control branches of the
Design C LAI phenology -- the moisture branch (rainfall -> SMD) and the
thermal branch (air temperature -> GDD / SDD accumulators) -- converging
on the LAI trajectory.

Takes the bundled London sample (Ward et al. 2016 KCL benchmark), clones the forcing with rainfall zeroed
for a spring-through-early-summer window (so SUEWS's internal water
balance drains the root-zone store during the growth phase), runs the
model at ``LAIType = 0`` and ``LAIType = 2``, and overlays:

* Panel A -- MOISTURE INPUT: daily rainfall (baseline vs drought
  forcing). Shows the intervention directly.
* Panel B -- MOISTURE STATE: daily vegetation-surface SMD with the
  Jarvis threshold markers (w_opt, w_wilt as SMD-equivalents on this
  SoilStoreCap). The drought forcing drives SMD up toward capacity
  during the zeroed-rainfall window.
* Panel C -- THERMAL STATE: daily mean air temperature on the left
  axis together with the vegetation GDD and SDD accumulators on the
  right axis. Shows that the thermal driver is identical between the
  two LAIType runs -- any LAI divergence must come from the moisture
  control.
* Panel D -- OUTCOME: simulated LAI under ``LAIType = 0`` (thermal
  only) and ``LAIType = 2`` (moisture-aware). LAIType=2 flatlines at
  LAImin during the drought window because the CLM5-style latch
  prevents leaf-on while wbar_id sits below w_off.

Output: ``.context/gh1292/drought-demo/moisture_control.png`` plus a
JSON summary. The figure is what gets embedded in the dashboard.

Why synthetic rather than a real dryland forcing
------------------------------------------------
A real FLUXNET dryland site (AU-ASM, US-SRG) would be the scientifically
correct test, but it requires a dedicated SUEWS configuration (site
geometry, land cover, thermal GDD fits) that does not ship with this
repo. The London sample config is UK-urban, so a synthetic drought
imposed through the rainfall forcing is the cleanest way to demonstrate
the moisture control within the shipped model without muddying the
message with a site-config mismatch.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd

OUT_DIR = Path(".context/gh1292/drought-demo")
LAI_TYPE_COLS = [("laitype", f"({i},)") for i in range(3)]
# Drought window placed EARLY (late winter through late spring) so it catches
# the spring leaf-out phase. London's thermal GDD saturates in mid-to-late
# spring; the drought blocks that. Ending the window in May (DOY ~130) rather
# than late June gives all three vegetation surfaces enough time to catch up
# through summer (they have staggered recovery because per-surface soil stores
# drain and refill at slightly different rates) and then senesce normally via
# SDD in autumn. Longer drought windows leave DecTr still in catch-up growth
# through October/November, at which point the cumulative-GDD scheme has no way
# of knowing it is calendar autumn -- the resulting late "growth" is a known
# cumulative-GDD-scheme limitation, not a moisture-control bug.
DROUGHT_DOY_START = 30   # 30 Jan
DROUGHT_DOY_END = 130    #  9 May

# London's soil-store capacity is 150 mm, so the drought forcing pushes SMD
# to ~80 mm (w ~ 0.47). Default moisture thresholds (w_wilt=0.15, w_opt=0.40)
# would leave the Jarvis factor saturated at 1.0 throughout -- the scheme
# would degrade gracefully to LAIType=0, which is the correct physics but a
# boring demo. To make the moisture control *visible* we tighten the thresholds
# so the Jarvis curve engages in the actual w range the drought produces.
DEMO_MOISTURE_PARAMS = {
    "w_wilt": 0.65,  # below this -> f_w = 0 (complete shutdown)
    "w_opt":  0.95,  # above this -> f_w = 1 (no stress)
    "f_shape": 1.0,
    "w_on":  0.85,   # leaf-on persistence threshold
    "w_off": 0.60,   # drop below -> CLM5 latch flips off
    "tau_w": 10.0,
}


def _build_drought_forcing(df_forcing: pd.DataFrame) -> pd.DataFrame:
    df_drought = df_forcing.copy()
    # The forcing DataFrame index typically has datetime as level 1 (grid, datetime);
    # support either shape defensively.
    if isinstance(df_drought.index, pd.MultiIndex):
        dt = df_drought.index.get_level_values("datetime")
    else:
        dt = df_drought.index
    doy = dt.dayofyear
    mask = (doy >= DROUGHT_DOY_START) & (doy <= DROUGHT_DOY_END)
    if "rain" in df_drought.columns:
        df_drought.loc[mask, "rain"] = 0.0
    return df_drought


def _run_scenario(
    laitype: int,
    df_forcing: pd.DataFrame,
    moisture_params: dict | None = None,
    dry_start: bool = False,
):
    """Return (SUEWS, DailyState) output frames for the given laitype."""

    from supy import SUEWSSimulation

    sim = SUEWSSimulation.from_sample_data()
    state = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state.loc[:, col] = laitype
    if moisture_params and laitype == 2:
        for name, value in moisture_params.items():
            for i in range(3):
                state.loc[:, (name, f"({i},)")] = value
    if dry_start:
        # Pydantic enforces soilstore >= 10 mm; use 10 to start as close to
        # full depletion as the data model allows.
        for veg_idx in (2, 3, 4):
            state.loc[:, ("soilstore_surf", f"({veg_idx},)")] = 10.0

    scenario = SUEWSSimulation.from_state(state)
    scenario.update_forcing(df_forcing)
    output = scenario.run()
    return output.SUEWS, output.DailyState


def _daily(series_index, values) -> pd.Series:
    s = pd.Series(values, index=series_index)
    if isinstance(s.index, pd.MultiIndex):
        s = s.droplevel(0)
    s = s[~s.index.duplicated(keep="first")]
    return s.resample("1D").mean()


def _daily_sum(series_index, values) -> pd.Series:
    s = pd.Series(values, index=series_index)
    if isinstance(s.index, pd.MultiIndex):
        s = s.droplevel(0)
    s = s[~s.index.duplicated(keep="first")]
    return s.resample("1D").sum()


def main() -> int:
    from supy import SUEWSSimulation

    OUT_DIR.mkdir(parents=True, exist_ok=True)

    print("[drought-demo] Building baseline and drought forcings...")
    sample = SUEWSSimulation.from_sample_data()
    df_forcing_baseline = sample.forcing
    # supy exposes forcing via a wrapper object with a DataFrame-like API.
    # Pull the raw DataFrame so we can mutate the rain column deterministically.
    df_baseline_raw = pd.DataFrame(
        {col: df_forcing_baseline[col] for col in df_forcing_baseline.columns}
    )
    df_drought_raw = _build_drought_forcing(df_baseline_raw)

    print("[drought-demo] Running LAIType=0 under drought forcing (thermal baseline, dry-start)...")
    out_l0, daily_l0 = _run_scenario(0, df_drought_raw, dry_start=True)
    print(
        "[drought-demo] Running LAIType=2 under drought forcing "
        f"with tightened demo parameters {DEMO_MOISTURE_PARAMS} + dry-start..."
    )
    out_l2, daily_l2 = _run_scenario(
        2, df_drought_raw, moisture_params=DEMO_MOISTURE_PARAMS, dry_start=True
    )

    # Aggregate to daily series for plotting clarity.
    dt_idx = out_l0.index
    rain_daily_baseline = _daily_sum(
        pd.MultiIndex.from_frame(
            pd.DataFrame({"grid": [1] * len(df_baseline_raw), "datetime": df_baseline_raw.index})
        )
        if not isinstance(df_baseline_raw.index, pd.MultiIndex)
        else df_baseline_raw.index,
        df_baseline_raw["rain"].values,
    )
    rain_daily_drought = _daily_sum(
        pd.MultiIndex.from_frame(
            pd.DataFrame({"grid": [1] * len(df_drought_raw), "datetime": df_drought_raw.index})
        )
        if not isinstance(df_drought_raw.index, pd.MultiIndex)
        else df_drought_raw.index,
        df_drought_raw["rain"].values,
    )
    # SMD is identical under the same forcing regardless of LAIType -- use either run.
    smd_grass = _daily(dt_idx, out_l0["SMDGrass"].values)
    lai_l0 = _daily(dt_idx, out_l0["LAI"].values)
    lai_l2 = _daily(dt_idx, out_l2["LAI"].values)

    # Thermal series (same for both LAIType runs since air-temperature forcing is identical).
    tair_daily = _daily(dt_idx, out_l0["T2"].values)
    # DailyState is emitted once per day at the end-of-day timestep; drop duplicated grid
    # level and subset to the grass surface which is the representative vegetation type.
    ds = daily_l0.copy()
    if isinstance(ds.index, pd.MultiIndex):
        ds = ds.droplevel(0)
    ds = ds[~ds.index.duplicated(keep="first")]
    gdd_grass = ds["GDD_Grass"].dropna()
    sdd_grass = ds["SDD_Grass"].dropna()

    # --- Figure ---------------------------------------------------------------
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates

    fig, axes = plt.subplots(
        4, 1, figsize=(11, 11.5), sharex=True,
        gridspec_kw={"hspace": 0.13},
    )

    def panel_label(ax, letter, description=None, x=-0.06, fontsize_solo=13, fontsize_titled=11):
        """Panel label with left edge aligned to the y-axis label's left edge.

        Two scenarios, one alignment principle (``x`` always fixed):

        * **Solo** (``description=None``) -- the letter alone sits at the
          upper-left corner of the panel, top edge on the axes top spine
          (``va="top"``, ``y=1.0``). Use when the panel has no descriptive
          title (e.g. stacked time-series with side labels).
        * **Labelled title** (``description`` given) -- the letter is
          concatenated with the description into one bold line placed above
          the axes (``va="bottom"``, ``y=1.02``). Use when the panel's title
          serves as the panel label.

        In both cases the left edge lines up with the y-axis label's left
        edge (default ``x=-0.06`` in axes fraction); adjust per-figure if
        the y-label sits at a different x.
        """
        if description:
            text = f"{letter}) {description}"
            y, va, fs = 1.02, "bottom", fontsize_titled
        else:
            text = f"{letter})"
            y, va, fs = 1.0, "top", fontsize_solo
        ax.text(
            x, y, text,
            transform=ax.transAxes,
            ha="left", va=va,
            fontsize=fs, fontweight="bold",
            color="#1a1a1a",
        )

    # Shared drought-window shading
    def _shade(ax):
        year = int(rain_daily_drought.index[0].year)
        xmin = pd.Timestamp(year=year, month=1, day=1) + pd.Timedelta(days=DROUGHT_DOY_START - 1)
        xmax = pd.Timestamp(year=year, month=1, day=1) + pd.Timedelta(days=DROUGHT_DOY_END - 1)
        ax.axvspan(xmin, xmax, color="#d94922", alpha=0.06, lw=0)
        return xmin, xmax

    # Section labels down the left-hand side for visual grouping.
    def _side_label(ax, text, colour):
        ax.text(
            -0.075, 0.5, text,
            transform=ax.transAxes, rotation=90, ha="center", va="center",
            fontsize=9, color=colour, fontweight="bold",
        )

    # Panel A -- moisture input (rainfall)
    ax = axes[0]
    ax.plot(
        rain_daily_baseline.index, rain_daily_baseline.values,
        color="#5a9db5", lw=0.9, alpha=0.7, label="Baseline forcing",
    )
    ax.plot(
        rain_daily_drought.index, rain_daily_drought.values,
        color="#d94922", lw=0.9,
        label=f"Drought forcing (DOY {DROUGHT_DOY_START}-{DROUGHT_DOY_END} rainfall zeroed)",
    )
    _shade(ax)
    ax.set_ylabel("Daily rainfall\n[mm]")
    ax.legend(loc="upper right", fontsize=9, frameon=False)
    ax.grid(True, alpha=0.25)

    # Panel B -- moisture state (SMD + thresholds)
    ax = axes[1]
    ax.plot(smd_grass.index, smd_grass.values, color="#b06e21", lw=1.2, label="SMD (grass surface)")
    _shade(ax)
    ax.set_ylabel("SMD [mm]\n(drier upward)")
    smdcap = 150.0
    smd_w_opt = smdcap * (1.0 - DEMO_MOISTURE_PARAMS["w_opt"])
    smd_w_wilt = smdcap * (1.0 - DEMO_MOISTURE_PARAMS["w_wilt"])
    ax.axhline(smd_w_opt, color="#6da33a", lw=0.8, ls="--", alpha=0.7)
    ax.axhline(smd_w_wilt, color="#d94922", lw=0.8, ls="--", alpha=0.7)
    ax.text(
        rain_daily_drought.index[-40], smd_w_opt + 1,
        f"w_opt = {DEMO_MOISTURE_PARAMS['w_opt']}  (SMD ~ {smd_w_opt:.0f} mm)",
        color="#6da33a", fontsize=8, va="bottom", ha="right",
    )
    ax.text(
        rain_daily_drought.index[-40], smd_w_wilt + 1,
        f"w_wilt = {DEMO_MOISTURE_PARAMS['w_wilt']}  (SMD ~ {smd_w_wilt:.0f} mm)",
        color="#d94922", fontsize=8, va="bottom", ha="right",
    )
    ax.legend(loc="upper left", fontsize=9, frameon=False)
    ax.grid(True, alpha=0.25)

    # Panel C -- thermal state (air temperature + GDD / SDD on twin axis)
    ax = axes[2]
    ax.plot(tair_daily.index, tair_daily.values,
            color="#5a9db5", lw=1.0, alpha=0.8, label="Daily mean air T (T2)")
    _shade(ax)
    ax.set_ylabel("Air temperature\n[\u00b0C]", color="#5a9db5")
    ax.tick_params(axis="y", labelcolor="#5a9db5")
    ax.axhline(0, color="#5a9db5", lw=0.4, alpha=0.35)
    ax.grid(True, alpha=0.25)

    ax_right = ax.twinx()
    ax_right.plot(gdd_grass.index, gdd_grass.values,
                  color="#6da33a", lw=1.3, label="GDD (grass)")
    ax_right.plot(sdd_grass.index, sdd_grass.values,
                  color="#a34b6d", lw=1.3, label="SDD (grass)")
    ax_right.set_ylabel("GDD / SDD\n[\u00b0C day]", color="#4a3a2a")
    ax_right.axhline(0, color="#6c6456", lw=0.4, alpha=0.35)
    lines1, labels1 = ax.get_legend_handles_labels()
    lines2, labels2 = ax_right.get_legend_handles_labels()
    ax.legend(lines1 + lines2, labels1 + labels2, loc="upper left", fontsize=9, frameon=False, ncol=3)

    # Panel D -- outcome (LAI) with phase annotations
    ax = axes[3]
    ax.plot(lai_l0.index, lai_l0.values, color="#3a342d", lw=1.5, label="LAIType=0 (thermal only)")
    ax.plot(lai_l2.index, lai_l2.values, color="#c4841d", lw=1.5, label="LAIType=2 (moisture-aware)")
    _shade(ax)
    ax.set_ylabel("LAI\n[m$^2$ m$^{-2}$]")
    ax.set_xlabel("Date")
    ax.legend(loc="lower center", fontsize=9, frameon=False, ncol=2)
    ax.grid(True, alpha=0.25)
    ax.xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1, 3, 5, 7, 9, 11)))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))

    # --- Phase band on Panel D ---
    # Keep the top band (visual anchor); the per-phase rationale lives in the
    # caption (see dashboard.html) so the figure itself stays clean.
    year = int(rain_daily_drought.index[0].year)
    def _ts(doy):
        return pd.Timestamp(year=year, month=1, day=1) + pd.Timedelta(days=doy - 1)

    # Four phases, boundaries picked from the simulation itself:
    #   P1 winter  (DOY 1   - 95):  T below BaseT_GDD, no leaf-on from either scheme
    #   P2 spring-drought (95 - DROUGHT_DOY_END): GDD crosses BaseT_GDD but moisture latch closed
    #   P3 recovery (DROUGHT_DOY_END - 220):     rain returns, latch reopens, LAI2 catches up
    #   P4 summer -> autumn (220 - end): both at LAImax, SDD drives senescence
    phase_bounds = [1, 95, DROUGHT_DOY_END, 220, 365]
    phase_short = [
        "P1 winter",
        "P2 spring drought",
        "P3 recovery",
        "P4 summer -> autumn",
    ]
    phase_colours = ["#5a9db5", "#d94922", "#6da33a", "#a07050"]
    y_lo, y_hi = ax.get_ylim()
    # Make room for the label band above the data.
    ax.set_ylim(y_lo, y_hi + 0.95)
    label_y = y_hi + 0.32
    band_y = y_hi + 0.78

    # Vertical dotted boundaries behind the data.
    for doy in phase_bounds[1:-1]:
        ax.axvline(_ts(doy), color="#6c6456", lw=0.6, ls=":", alpha=0.7, zorder=0)

    # Coloured phase band at the top with short PX labels.
    for (lo, hi), short, col in zip(
        zip(phase_bounds[:-1], phase_bounds[1:]), phase_short, phase_colours
    ):
        ax.plot([_ts(lo), _ts(hi)], [band_y, band_y], color=col, lw=5, solid_capstyle="butt")
        ax.text(
            _ts((lo + hi) / 2), label_y, short,
            ha="center", va="bottom", fontsize=9,
            color=col, fontweight="bold",
        )

    # Section labels down the left-hand side for visual grouping.
    _side_label(axes[0], "MOISTURE INPUT",  "#5a9db5")
    _side_label(axes[1], "MOISTURE STATE",  "#b06e21")
    _side_label(axes[2], "THERMAL STATE",   "#6da33a")
    _side_label(axes[3], "LAI OUTCOME",     "#c4841d")

    # Panel labels A)/B)/C)/D) at the upper-left corner of each panel, outside
    # the axes (aligned roughly with the y-label's left edge). Matches the
    # convention used by moisture_phenology_sensitivity_summary.py.
    for letter, axN in zip("ABCD", axes):
        panel_label(axN, letter)

    # Figure-level caption -- describes the whole four-panel composition.
    fig.suptitle(
        "GH-1292 moisture-aware LAI phenology: two control branches on the London sample",
        fontsize=11, color="#2a2a2a", x=0.06, y=0.995, ha="left",
    )

    # Tight margins so panel letters hug their panels rather than floating in
    # the inter-panel gap.
    fig.subplots_adjust(left=0.13, right=0.95, top=0.96, bottom=0.06, hspace=0.13)
    png_path = OUT_DIR / "moisture_control.png"
    fig.savefig(png_path, dpi=150, facecolor="white")
    plt.close(fig)

    # --- Numeric summary ------------------------------------------------------
    drought_mask = (lai_l0.index.dayofyear >= DROUGHT_DOY_START) & (
        lai_l0.index.dayofyear <= DROUGHT_DOY_END
    )
    summary = {
        "drought_window_doy": [DROUGHT_DOY_START, DROUGHT_DOY_END],
        "smd_mean_outside_drought_mm": float(smd_grass[~drought_mask].mean()),
        "smd_mean_inside_drought_mm": float(smd_grass[drought_mask].mean()),
        "lai_mean_laitype0_inside_drought": float(lai_l0[drought_mask].mean()),
        "lai_mean_laitype2_inside_drought": float(lai_l2[drought_mask].mean()),
        "lai_mean_diff_inside_drought": float(lai_l0[drought_mask].mean() - lai_l2[drought_mask].mean()),
        "lai_max_laitype0": float(lai_l0.max()),
        "lai_max_laitype2": float(lai_l2.max()),
    }
    (OUT_DIR / "moisture_control.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8"
    )

    print(f"[drought-demo] Wrote {png_path}")
    print(f"[drought-demo] Summary: {json.dumps(summary, indent=2)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
