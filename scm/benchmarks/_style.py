"""Shared figure style for SCM benchmarks and demos (SUEWS palette)."""

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# SUEWS brand palette
DEEP_BLUE = "#2D3142"
GOLDEN_SUN = "#F7B538"
SUN_CORE = "#E85D04"
FOREST_GREEN = "#09a25c"
OCEAN_BLUE = "#0077B6"
SKY_BLUE = "#5DADE2"
WAVE_BLUE = "#0558a5"

BG = "#1b1e2b"
PANEL = "#232634"
FG = "#e8eaf2"
MUTED = "#9aa0b4"


def apply_style():
    plt.rcParams.update({
        "figure.facecolor": BG,
        "axes.facecolor": PANEL,
        "savefig.facecolor": BG,
        "text.color": FG,
        "axes.edgecolor": MUTED,
        "axes.labelcolor": FG,
        "xtick.color": MUTED,
        "ytick.color": MUTED,
        "axes.grid": True,
        "grid.color": "#3a3f55",
        "grid.linewidth": 0.5,
        "grid.alpha": 0.6,
        "font.family": "sans-serif",
        "font.sans-serif": ["Instrument Sans", "Helvetica Neue", "Arial"],
        "font.size": 11,
        "axes.titlesize": 12,
        "legend.frameon": False,
        "figure.dpi": 150,
        "savefig.dpi": 150,
        "savefig.bbox": "tight",
        "lines.linewidth": 1.8,
    })
