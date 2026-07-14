"""Generate the OHM coefficient-transition figure used in the documentation.

The figure contrasts the previous abrupt switching between the four OHM
coefficient sets with the clamped linear blending introduced in GH-473, and is
referenced from ``docs/source/inputs/tables/SUEWS_SiteInfo/SUEWS_OHMCoefficients.rst``.

The transition widths below MUST match the parameters declared in
``src/suews/src/suews_phys_ohm.f95``:

    OHM_TEMP_TRANSITION_HALF_WIDTH        = 2.0   [degC]
    OHM_SOIL_TRANSITION_HALF_WIDTH        = 0.1   [-]
    OHM_SURFACE_WETNESS_TRANSITION_WIDTH  = 0.5   [mm]

Run from the repository root:

    python docs/plot_ohm_transitions.py
"""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

# Transition parameters -- keep in step with suews_phys_ohm.f95
TEMP_HALF_WIDTH = 2.0  # degC
SOIL_HALF_WIDTH = 0.1  # dimensionless soil-moisture ratio
SURFACE_WETNESS_WIDTH = 0.5  # mm

# Illustrative thresholds and coefficients (documentation only)
THRESH_SW = 10.0  # degC, OHMThresh_SW
THRESH_WD = 0.9  # -, OHMThresh_WD
A1_SUMMER_DRY = 0.70
A1_WINTER_DRY = 0.35

OUT_PATH = Path(__file__).parent / "source" / "assets" / "img" / "ohm_coefficient_transitions.png"

STEP_STYLE = dict(color="0.55", linestyle="--", linewidth=1.6, label="Previous: abrupt switch")
BLEND_STYLE = dict(color="#1f77b4", linewidth=2.2, label="Current: clamped blend")


def clamp(x):
    """Clamp to the interval [0, 1], as the Fortran does with MAX/MIN."""
    return np.clip(x, 0.0, 1.0)


def weight_summer(t_air):
    """Summer weight from the 5-day running-mean air temperature."""
    return clamp((t_air - THRESH_SW + TEMP_HALF_WIDTH) / (2.0 * TEMP_HALF_WIDTH))


def weight_surface_wet(state):
    """Wet weight from the surface water store [mm]."""
    return clamp(state / SURFACE_WETNESS_WIDTH)


def weight_soil_wet(ratio):
    """Wet weight from the soil-moisture ratio (vegetated and bare-soil surfaces)."""
    return clamp((ratio - THRESH_WD + SOIL_HALF_WIDTH) / (2.0 * SOIL_HALF_WIDTH))


def shade_transition(ax, centre, half_width):
    ax.axvspan(
        centre - half_width,
        centre + half_width,
        color="#1f77b4",
        alpha=0.08,
        zorder=0,
    )
    ax.axvline(centre, color="#d62728", linewidth=1.0, linestyle=":", zorder=3)


def main():
    fig, axes = plt.subplots(2, 2, figsize=(9.6, 6.8))

    # (a) summer/winter weight against 5-day mean air temperature
    ax = axes[0, 0]
    t_air = np.linspace(THRESH_SW - 6.0, THRESH_SW + 6.0, 601)
    shade_transition(ax, THRESH_SW, TEMP_HALF_WIDTH)
    ax.plot(t_air, (t_air >= THRESH_SW).astype(float), **STEP_STYLE)
    ax.plot(t_air, weight_summer(t_air), **BLEND_STYLE)
    ax.set_xlabel("5-day mean air temperature (degC)")
    ax.set_ylabel("Summer weight $w_s$")
    ax.set_title("(a) Summer/winter transition", loc="left")
    ax.annotate(
        f"+/-{TEMP_HALF_WIDTH:.0f} degC about OHMThresh_SW",
        xy=(THRESH_SW, 0.5),
        xytext=(THRESH_SW + 0.6, 0.22),
        fontsize=8,
        color="0.3",
    )

    # (b) wet weight against the surface water store
    ax = axes[0, 1]
    state = np.linspace(0.0, 1.0, 601)
    ax.axvspan(0.0, SURFACE_WETNESS_WIDTH, color="#1f77b4", alpha=0.08, zorder=0)
    ax.axvline(0.0, color="#d62728", linewidth=1.0, linestyle=":", zorder=3)
    ax.plot(state, (state > 0.0).astype(float), **STEP_STYLE)
    ax.plot(state, weight_surface_wet(np.clip(state, 0.0, None)), **BLEND_STYLE)
    ax.set_xlabel("Surface water store (mm)")
    ax.set_ylabel("Wet weight $w_w$")
    ax.set_title("(b) Surface-wetness transition", loc="left")
    ax.annotate(
        f"linear over 0 to {SURFACE_WETNESS_WIDTH:.1f} mm",
        xy=(0.25, 0.5),
        xytext=(0.55, 0.22),
        fontsize=8,
        color="0.3",
    )

    # (c) wet weight against the soil-moisture ratio
    ax = axes[1, 0]
    ratio = np.linspace(THRESH_WD - 0.3, THRESH_WD + 0.3, 601)
    shade_transition(ax, THRESH_WD, SOIL_HALF_WIDTH)
    ax.plot(ratio, (ratio >= THRESH_WD).astype(float), **STEP_STYLE)
    ax.plot(ratio, weight_soil_wet(ratio), **BLEND_STYLE)
    ax.set_xlabel("Soil-moisture ratio (-)")
    ax.set_ylabel("Wet weight $w_w$")
    ax.set_title("(c) Soil-moisture transition (vegetated and bare-soil only)", loc="left")
    ax.annotate(
        f"+/-{SOIL_HALF_WIDTH:.1f} about OHMThresh_WD",
        xy=(THRESH_WD, 0.5),
        xytext=(THRESH_WD + 0.03, 0.22),
        fontsize=8,
        color="0.3",
    )

    # (d) the consequence: a1 for a dry surface across the seasonal threshold
    ax = axes[1, 1]
    a1_step = np.where(t_air >= THRESH_SW, A1_SUMMER_DRY, A1_WINTER_DRY)
    w_s = weight_summer(t_air)
    a1_blend = w_s * A1_SUMMER_DRY + (1.0 - w_s) * A1_WINTER_DRY
    shade_transition(ax, THRESH_SW, TEMP_HALF_WIDTH)
    ax.plot(t_air, a1_step, **STEP_STYLE)
    ax.plot(t_air, a1_blend, **BLEND_STYLE)
    ax.set_xlabel("5-day mean air temperature (degC)")
    ax.set_ylabel("$a_1$ (-)")
    ax.set_title("(d) Resulting coefficient, dry surface", loc="left")
    ax.annotate(
        "no discontinuity at the threshold",
        xy=(THRESH_SW, 0.5 * (A1_SUMMER_DRY + A1_WINTER_DRY)),
        xytext=(THRESH_SW + 0.6, A1_WINTER_DRY + 0.03),
        fontsize=8,
        color="0.3",
    )

    for ax in axes.flat:
        ax.grid(alpha=0.25, linewidth=0.6)
        ax.margins(x=0)

    for ax in axes.flat[:3]:
        ax.set_ylim(-0.08, 1.08)

    handles, labels = axes[0, 0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="lower center", ncol=2, frameon=False, bbox_to_anchor=(0.5, -0.01))

    fig.suptitle(
        "OHM coefficient blending: the thresholds are transition centres, not switches",
        fontsize=11,
    )
    fig.text(
        0.5,
        0.945,
        "The effective wet weight is the larger of (b) and (c). Coefficients in (d) are illustrative.",
        ha="center",
        fontsize=8.5,
        color="0.35",
    )
    fig.tight_layout(rect=(0, 0.04, 1, 0.97))

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_PATH, dpi=200)
    print(f"[OK] wrote {OUT_PATH}")


if __name__ == "__main__":
    main()
