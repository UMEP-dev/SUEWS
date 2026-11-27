"""Regression tests for ``supy.util.plot_comp`` KDE overlay on seaborn>=0.13."""

from __future__ import annotations

import io
import warnings

import matplotlib

matplotlib.use("Agg")

from matplotlib.contour import QuadContourSet
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from supy.util import plot_comp

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API", "C-OUTPUT")]


@pytest.fixture(autouse=True)
def _close_figures():
    """Close all matplotlib figures after each test to prevent resource leaks."""
    yield
    plt.close("all")


def _make_sample_comparison_df(col_x: str = "Obs", col_y: str = "Sim") -> pd.DataFrame:
    """Create a deterministic bivariate comparison DataFrame with one NaN row."""
    rng = np.random.default_rng(2026)
    n_samples = 200
    obs = rng.normal(100.0, 20.0, size=n_samples)
    sim = 0.95 * obs + 5.0 + rng.normal(0.0, 8.0, size=n_samples)
    df = pd.DataFrame({col_x: obs, col_y: sim})
    df.loc[n_samples] = [np.nan, 42.0]
    return df


def test_plot_comp_show_pdf_default_draws_filled_kde_contours():
    """plot_comp(..., show_pdf=True) renders filled KDE contours cleanly on seaborn>=0.13."""
    df_var = _make_sample_comparison_df()

    fig_base, ax_base = plot_comp(df_var, show_pdf=False)

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        fig_pdf, ax_pdf = plot_comp(df_var, show_pdf=True)

    kde_deprecation_warnings = [
        str(item.message) for item in caught if "shade" in str(item.message).lower()
    ]
    assert kde_deprecation_warnings == []

    assert isinstance(fig_pdf, plt.Figure)
    assert len(ax_pdf.collections) > len(ax_base.collections)

    contour_sets = [
        coll for coll in ax_pdf.collections if isinstance(coll, QuadContourSet)
    ]
    assert len(contour_sets) == 1
    contour = contour_sets[0]
    assert contour.filled is True
    assert len(contour.levels) == 4
    assert contour.get_zorder() == 0
    assert len(contour.get_paths()) > 0

    # Regression statistics, equal 1:1 limits, and aspect ratio stay unchanged.
    assert (
        ax_pdf.get_legend().get_texts()[0].get_text()
        == ax_base.get_legend().get_texts()[0].get_text()
    )
    assert "n=200" in ax_pdf.get_legend().get_texts()[0].get_text()
    assert ax_pdf.get_xlim() == ax_pdf.get_ylim()
    assert ax_pdf.get_aspect() == 1.0

    # Confirm the figure renders to a valid non-empty PNG artefact.
    buf = io.BytesIO()
    fig_pdf.savefig(buf, format="png")
    png_bytes = buf.getvalue()
    assert png_bytes.startswith(b"\x89PNG\r\n\x1a\n")
    assert len(png_bytes) > 1000

    # Base figure also remains a valid matplotlib Figure.
    assert isinstance(fig_base, plt.Figure)


def test_plot_comp_show_pdf_fallback_column_names():
    """plot_comp(..., show_pdf=True) works when DataFrame columns are not Obs/Sim."""
    df_var = _make_sample_comparison_df(col_x="Observed", col_y="Simulated")

    fig, ax = plot_comp(df_var, show_pdf=True)

    assert isinstance(fig, plt.Figure)
    contour_sets = [coll for coll in ax.collections if isinstance(coll, QuadContourSet)]
    assert len(contour_sets) == 1
    assert contour_sets[0].filled is True
    assert ax.get_xlabel() == "Observed"
    assert ax.get_ylabel() == "Simulated"


def test_plot_comp_show_pdf_custom_kde_kws_without_mutating_caller_dict():
    """Custom kde_kws override defaults (including color/fill) without mutating caller input."""
    df_var = _make_sample_comparison_df()
    custom_kws = {"fill": False, "thresh": 0.1, "levels": 5, "color": "C2"}
    original_snapshot = dict(custom_kws)

    fig, ax = plot_comp(df_var, show_pdf=True, kde_kws=custom_kws)

    assert custom_kws == original_snapshot
    assert isinstance(fig, plt.Figure)
    contour_sets = [coll for coll in ax.collections if isinstance(coll, QuadContourSet)]
    assert len(contour_sets) == 1
    contour = contour_sets[0]
    assert contour.filled is False
    assert len(contour.levels) == 5
    assert contour.get_zorder() == 0
