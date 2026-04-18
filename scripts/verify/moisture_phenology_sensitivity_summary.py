"""GH-1292 PR3 consolidated sensitivity figure.

Replaces the six-panel sweep grid with a single, self-explanatory
figure that answers one question: *which of the six moisture-aware
LAI parameters actually move the needle on the dry-start London
sample, and in what direction?*

Design (figure-craft principles)
--------------------------------
* One clear message: rank the parameters by sensitivity.
* Two stacked panels with a shared narrative:
    - Top: horizontal bar chart of |mean LAI deviation from baseline|
      across the parameter's sweep range -- a ranking at a glance.
    - Bottom: dose-response lines for the three parameters that matter.
      X-axis is the normalised sweep position (0 = lowest value in the
      sweep, 1 = highest), labelled with the actual numeric values
      underneath.
* Legends pulled outside the plot area; no overlap with data.
* Consistent typography and muted palette tied to the dashboard.

Expects JSON artefacts under ``.context/gh1292/<site>/sweep_<param>.json``
produced by ``moisture_phenology_sweep.py --all --dry-start``.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, List, Tuple

import matplotlib
import matplotlib.pyplot as plt
import numpy as np

matplotlib.use("Agg")

SITE = "London"
OUT_DIR = Path(".context/gh1292") / SITE
PARAM_ORDER = ["w_wilt", "w_opt", "w_off", "w_on", "f_shape", "tau_w"]
PARAM_LABELS = {
    "w_wilt": "w_wilt",
    "w_opt":  "w_opt",
    "w_off":  "w_off",
    "w_on":   "w_on",
    "f_shape": "f_shape",
    "tau_w":  "tau_w",
}


def _load_sweeps() -> Tuple[Dict[str, List[dict]], float]:
    sweeps: Dict[str, List[dict]] = {}
    baseline_mean = None
    for p in PARAM_ORDER:
        path = OUT_DIR / f"sweep_{p}.json"
        if not path.exists():
            raise FileNotFoundError(
                f"Missing sweep artefact {path}. "
                "Run `uv run python scripts/verify/moisture_phenology_sweep.py --all --dry-start` first."
            )
        payload = json.loads(path.read_text(encoding="utf-8"))
        sweeps[p] = payload["points"]
        baseline_mean = float(payload["baseline_mean_lai"])
    assert baseline_mean is not None
    return sweeps, baseline_mean


def _sensitivity(points: List[dict], baseline_mean: float) -> float:
    """Return max |mean LAI - baseline| over the sweep, i.e. worst-case deviation."""
    devs = [abs(pt["mean_lai"] - baseline_mean) for pt in points]
    return max(devs) if devs else 0.0


def main() -> int:
    sweeps, baseline_mean = _load_sweeps()
    sensitivities = {p: _sensitivity(sweeps[p], baseline_mean) for p in PARAM_ORDER}
    ranked = sorted(sensitivities.items(), key=lambda kv: -kv[1])

    # Split into sensitive vs silent using a fixed threshold (0.02 m2 m-2 ≈ RMSE noise floor).
    THRESHOLD = 0.02
    sensitive = [p for p, s in ranked if s >= THRESHOLD]
    silent = [p for p, s in ranked if s < THRESHOLD]

    fig = plt.figure(figsize=(10.5, 7.2), facecolor="white")
    gs = fig.add_gridspec(
        2, 1, height_ratios=[1.1, 1.6], hspace=0.32, left=0.13, right=0.83, top=0.92, bottom=0.10,
    )

    def panel_label(ax, letter, description=None, x=-0.06, fontsize_solo=13, fontsize_titled=11):
        """Panel label with left edge aligned to the y-axis label's left edge.

        Two scenarios, one alignment principle (``x`` always fixed):

        * **Solo** (``description=None``) -- letter alone at the upper-left
          corner, top edge on the axes top spine (``va="top"``, ``y=1.0``).
          Use when the panel has no descriptive title.
        * **Labelled title** (``description`` given) -- letter concatenated
          with the description into one bold line placed above the axes
          (``va="bottom"``, ``y=1.02``). Use when the panel's title
          doubles as the panel label.

        Shared signature with moisture_phenology_drought_demo.py so the
        two figures look identical under both scenarios.
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

    # ---------------- Panel A: sensitivity ranking bar chart ----------------
    ax_bar = fig.add_subplot(gs[0])
    names = [p for p, _ in ranked]
    vals = [s for _, s in ranked]
    colours = ["#c4841d" if s >= THRESHOLD else "#b7ae9e" for _, s in ranked]
    bars = ax_bar.barh(
        range(len(names)), vals, color=colours, height=0.6, edgecolor="none",
    )
    ax_bar.set_yticks(range(len(names)))
    ax_bar.set_yticklabels(
        [PARAM_LABELS[p] for p in names], fontsize=10, fontfamily="monospace",
    )
    ax_bar.invert_yaxis()
    ax_bar.set_xlabel(
        "max |mean LAI deviation from baseline|  [m$^2$ m$^{-2}$]",
        fontsize=9,
    )
    panel_label(
        ax_bar, "A",
        description="Parameter sensitivity ranking (London dry-start drought sweep)",
    )
    ax_bar.axvline(THRESHOLD, color="#6c6456", lw=0.8, ls=":", alpha=0.8)
    ax_bar.text(
        THRESHOLD, len(names) - 0.25,
        f"  noise floor ({THRESHOLD:.2f})",
        fontsize=8, color="#6c6456", va="center", ha="left",
    )
    for bar, val in zip(bars, vals):
        ax_bar.text(
            val + 0.005, bar.get_y() + bar.get_height() / 2,
            f"{val:.3f}",
            va="center", ha="left", fontsize=8.5, color="#2a2a2a",
        )
    ax_bar.spines["top"].set_visible(False)
    ax_bar.spines["right"].set_visible(False)
    ax_bar.grid(True, axis="x", alpha=0.25)

    # Panel-level legend (outside plot, right-hand gutter).
    legend_ax = fig.add_axes([0.845, 0.63, 0.14, 0.22])
    legend_ax.axis("off")
    legend_ax.add_patch(plt.Rectangle((0.05, 0.60), 0.18, 0.08, color="#c4841d"))
    legend_ax.text(0.28, 0.64, "sensitive", fontsize=9, va="center")
    legend_ax.add_patch(plt.Rectangle((0.05, 0.42), 0.18, 0.08, color="#b7ae9e"))
    legend_ax.text(0.28, 0.46, "silent", fontsize=9, va="center")
    legend_ax.text(
        0.05, 0.28,
        f"n = {len(sensitive)} above\nnoise, {len(silent)} below.",
        fontsize=8, color="#6c6456", va="top",
    )
    legend_ax.text(
        0.05, 0.08,
        f"baseline mean LAI\n{baseline_mean:.3f} m$^2$ m$^{{-2}}$",
        fontsize=7.5, color="#6c6456", va="top", style="italic",
    )

    # ---------------- Panel B: dose-response lines for the sensitive trio ----
    ax = fig.add_subplot(gs[1])
    palette = {"w_wilt": "#d94922", "w_opt": "#c4841d", "w_off": "#8a4a8a", "w_on": "#5a9db5", "f_shape": "#6da33a", "tau_w": "#a07050"}
    # Only plot the sensitive ones in full colour; draw the silent set as faint grey references.
    for p in PARAM_ORDER:
        pts = sweeps[p]
        xs = np.linspace(0, 1, len(pts))
        ys = [pt["mean_lai"] for pt in pts]
        is_sensitive = sensitivities[p] >= THRESHOLD
        col = palette[p] if is_sensitive else "#d0c9b8"
        lw = 1.8 if is_sensitive else 1.0
        alpha = 1.0 if is_sensitive else 0.55
        ax.plot(xs, ys, "o-", color=col, lw=lw, markersize=5, label=PARAM_LABELS[p], alpha=alpha)

    ax.axhline(
        baseline_mean, color="#3a342d", lw=0.9, ls="--", alpha=0.75,
        label="LAIType=0 baseline",
    )
    ax.set_xticks([0, 0.25, 0.5, 0.75, 1.0])
    ax.set_xticklabels(["min", "", "mid", "", "max"], fontsize=9)
    ax.set_xlabel(
        "sweep position (per-parameter value range normalised, 0 = low end, 1 = high end)",
        fontsize=9,
    )
    ax.set_ylabel("mean annual LAI  [m$^2$ m$^{-2}$]", fontsize=9)
    panel_label(
        ax, "B",
        description="Dose-response across the sweep: tightening the sensitive trio pulls mean LAI below baseline",
    )
    ax.grid(True, alpha=0.25)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # Legend for dose-response (outside right).
    ax.legend(
        loc="center left", bbox_to_anchor=(1.02, 0.5),
        fontsize=9, frameon=False, borderaxespad=0.0,
    )

    out_path = OUT_DIR / "sensitivity_summary.png"
    fig.savefig(out_path, dpi=160, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"[sensitivity-summary] Wrote {out_path}")
    print(f"[sensitivity-summary] Sensitive parameters: {sensitive}")
    print(f"[sensitivity-summary] Silent parameters   : {silent}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
