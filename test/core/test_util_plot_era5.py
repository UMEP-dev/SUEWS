"""Offline behavioural tests for ``supy.util._plot`` and ``supy.util._era5``."""

from __future__ import annotations

import itertools
import logging
from pathlib import Path
import sys
from unittest.mock import Mock, patch
import zipfile

import matplotlib

matplotlib.use("Agg")

import matplotlib.colorbar as mcolorbar
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from supy.util._era5 import (
    Rmax_WGS84,
    Rmin_WGS84,
    _geoid_radius,
    chdir,
    download_cds,
    download_era5_timeseries,
    format_df_forcing,
    gen_dict_dt,
    gen_dict_dt_sub,
    gen_dict_proc,
    gen_fn,
    gen_forcing_era5,
    geometric2geopotential,
    geopotential2geometric,
    roundPartial,
    safe_stdout_stderr,
    save_forcing_era5,
)
from supy.util._plot import (
    Hex_to_RGB,
    RGB_list_to_Hex,
    RGB_to_Hex,
    colorbar,
    gradient_color,
    plot_colortable,
    plot_comp,
    plot_day_clm,
    plot_rsl,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-FORCING", "C-OUTPUT")]


@pytest.fixture(autouse=True)
def _close_figures():
    """Close all matplotlib figures after each test to prevent resource leaks."""
    yield
    plt.close("all")


@pytest.fixture(autouse=True)
def _restore_supy_logger_level():
    """Preserve process-wide SuPy logger level across ERA5 helper calls."""
    supy_logger = logging.getLogger("SuPy")
    orig_level = supy_logger.level
    try:
        yield
    finally:
        supy_logger.setLevel(orig_level)


@pytest.fixture
def supy_caplog(caplog):
    """Capture SuPy logger records even though SuPy defaults to propagate=False."""
    supy_logger = logging.getLogger("SuPy")
    original = supy_logger.propagate
    supy_logger.propagate = True
    try:
        yield caplog
    finally:
        supy_logger.propagate = original


def _make_synthetic_rsl_output(n_times: int = 2) -> pd.DataFrame:
    """Build a minimal DataFrame matching ``proc_df_rsl`` column expectations."""
    idx = pd.date_range("2023-07-01 12:00", periods=n_times, freq="h")
    levels = np.arange(1, 31)
    heights = np.linspace(1.0, 60.0, 30)

    cols: dict[str, np.ndarray] = {}
    for lvl, z_val in zip(levels, heights):
        cols[f"z_{lvl}"] = np.full(n_times, z_val)
    for lvl, z_val in zip(levels, heights):
        cols[f"U_{lvl}"] = np.array([
            1.0 + 0.1 * z_val + 0.5 * t for t in range(n_times)
        ])
    for lvl, z_val in zip(levels, heights):
        cols[f"T_{lvl}"] = np.array([20.0 - 0.01 * z_val + t for t in range(n_times)])
    for lvl, z_val in zip(levels, heights):
        cols[f"q_{lvl}"] = np.array([
            8.0 - 0.02 * z_val + 0.2 * t for t in range(n_times)
        ])

    # Debug variable sits after the 120 (30*4) profile columns.
    cols["zH_RSL"] = np.full(n_times, 25.0)
    return pd.DataFrame(cols, index=idx)


class TestPlotUtilities:
    """Tests for plotting and colour helpers in ``supy.util._plot``."""

    def test_colorbar_attaches_axes_and_restores_current_axes(self):
        """colorbar creates a Colorbar on a new axes and restores plt.gca()."""
        fig, ax = plt.subplots()
        mappable = ax.imshow(np.arange(16, dtype=float).reshape(4, 4))

        cbar = colorbar(mappable)

        assert isinstance(cbar, mcolorbar.Colorbar)
        assert len(fig.axes) == 2
        assert plt.gca() is ax

    def test_plot_day_clm_median_iqr_and_diff_modes(self):
        """plot_day_clm plots exact diurnal medians, supports fig/ax injection, and show_dif."""
        idx = pd.date_range("2023-07-01", periods=72, freq="h")
        hours = idx.hour.to_numpy(dtype=float)
        day_offset = np.repeat([-10.0, 0.0, 10.0], 24)
        obs = 100.0 + 20.0 * np.sin(2.0 * np.pi * hours / 24.0) + day_offset
        sim = obs + 5.0  # Constant +5 bias across every timestep.
        df_var = pd.DataFrame({"Obs": obs, "Sim": sim}, index=idx)

        # Default mode (creates fig & ax).
        fig, ax = plot_day_clm(df_var, show_dif=False)
        assert isinstance(fig, plt.Figure)
        assert ax.get_legend().get_title().get_text() == "variable"
        line_map = {line.get_label(): line.get_ydata() for line in ax.get_lines()}
        expected_obs_median = 100.0 + 20.0 * np.sin(2.0 * np.pi * np.arange(24) / 24.0)
        np.testing.assert_allclose(line_map["Obs"], expected_obs_median, rtol=1e-12)
        np.testing.assert_allclose(
            line_map["Sim"], expected_obs_median + 5.0, rtol=1e-12
        )
        # Verify hourly IQR shading polygons at every hour (Q25 = median - 5,
        # Q75 = median + 5 for offsets -10/0/+10).
        assert len(ax.collections) == 2
        expected_x = ax.get_lines()[0].get_xdata()
        for coll, shift in zip(ax.collections, (0.0, 5.0)):
            verts = coll.get_paths()[0].vertices
            # Group polygon y-values by each of the 24 hourly x-positions.
            lows = []
            highs = []
            for x_val in expected_x:
                ys = verts[np.isclose(verts[:, 0], x_val, rtol=0.0, atol=1e-9), 1]
                assert len(ys) >= 2
                lows.append(np.min(ys))
                highs.append(np.max(ys))
            np.testing.assert_allclose(
                lows, expected_obs_median + shift - 5.0, atol=1e-9
            )
            np.testing.assert_allclose(
                highs, expected_obs_median + shift + 5.0, atol=1e-9
            )

        # Pre-existing ax (fig=None) and pre-existing fig (ax=None) branches.
        fig_a, ax_a = plt.subplots()
        fig_a_out, ax_a_out = plot_day_clm(df_var, fig=None, ax=ax_a)
        assert fig_a_out is fig_a
        assert ax_a_out is ax_a

        fig_b, _ = plt.subplots()
        fig_b_out, ax_b_out = plot_day_clm(df_var, fig=fig_b, ax=None)
        assert fig_b_out is fig_b
        assert ax_b_out in fig_b.axes

        # Difference mode returns (fig, ax, ax2); Sim - Obs == +5 everywhere.
        fig_d, ax_d, ax2_d = plot_day_clm(df_var, show_dif=True, col_ref="Obs")
        assert isinstance(fig_d, plt.Figure)
        diff_lines = {line.get_label(): line.get_ydata() for line in ax_d.get_lines()}
        np.testing.assert_allclose(diff_lines["Sim"], np.full(24, 5.0), atol=1e-12)
        ref_lines = {line.get_label(): line for line in ax2_d.get_lines()}
        assert "Obs" in ref_lines
        assert ref_lines["Obs"].get_linestyle() == "-."

    def test_plot_comp_regression_stats_fallback_columns_and_axes_injection(self):
        """plot_comp computes exact regression stats, 1:1 limits, and column fallback."""
        obs = np.linspace(10.0, 100.0, 19)
        sim_pos = 1.2 * obs + 5.0
        df_pos = pd.DataFrame({"Obs": obs, "Sim": sim_pos})
        df_pos.loc[19] = [np.nan, 40.0]  # Dropped before regression.

        fig, ax = plot_comp(df_pos)
        assert isinstance(fig, plt.Figure)
        assert ax.get_xlim() == ax.get_ylim()
        assert ax.get_aspect() == 1.0
        legend_label = ax.get_legend().get_texts()[0].get_text()
        assert "y=1.20x+5.00" in legend_label
        assert "$R^2$=1.0000" in legend_label
        assert "n=19" in legend_label

        # Negative intercept + non-Obs/Sim column names fallback + existing ax.
        df_fallback = pd.DataFrame({"X": obs, "Y": 0.8 * obs - 3.0})
        fig_ax, ax_in = plt.subplots()
        fig_out, ax_out = plot_comp(df_fallback, fig=None, ax=ax_in)
        assert fig_out is fig_ax
        assert ax_out is ax_in
        legend_neg = ax_out.get_legend().get_texts()[0].get_text()
        assert "y=0.80x-3.00" in legend_neg

        # Existing fig with ax=None branch.
        fig_only, _ = plt.subplots()
        fig_only_out, _ = plot_comp(df_pos, fig=fig_only, ax=None)
        assert fig_only_out is fig_only

    def test_colour_helpers_and_colortable(self):
        """RGB/Hex conversions round-trip, gradients interpolate, and colortable renders."""
        assert RGB_to_Hex("255,0,15") == "#FF000F"
        assert RGB_list_to_Hex([0, 128, 255]) == "#0080FF"

        rgb_str, rgb_list = Hex_to_RGB("#FF000F")
        assert rgb_str == "255,0,15"
        assert rgb_list == [255, 0, 15]
        assert RGB_to_Hex(rgb_str) == "#FF000F"

        grad = gradient_color(["#000000", "#808080", "#FFFFFF"], color_sum=10)
        assert len(grad) == 10
        assert len(set(grad)) == 10
        assert grad[0] == "#000000"
        assert grad[5] == "#808080"
        reds = [Hex_to_RGB(c)[1][0] for c in grad]
        assert all(b > a for a, b in itertools.pairwise(reds))
        assert reds[-1] > 200

        palette = {"red": "#FF0000", "green": "#00FF00", "blue": "#0000FF"}
        fig_sorted = plot_colortable(palette, title="Sorted Palette", sort_colors=True)
        assert isinstance(fig_sorted, plt.Figure)
        ax_s = fig_sorted.axes[0]
        assert ax_s.get_title(loc="left") == "Sorted Palette"
        assert {t.get_text() for t in ax_s.texts} == set(palette.keys())

        fig_unsorted = plot_colortable(
            palette, title="Unsorted", sort_colors=False, emptycols=1
        )
        assert [t.get_text() for t in fig_unsorted.axes[0].texts] == list(
            palette.keys()
        )

    def test_plot_rsl_all_single_injected_and_invalid_var(self, supy_caplog):
        """plot_rsl plots all 3 profiles, single profiles with fig/ax, and logs bad var."""
        df_rsl = _make_synthetic_rsl_output(n_times=2)
        expected_z = np.linspace(1.0, 60.0, 30)
        expected_u0 = 1.0 + 0.1 * expected_z
        expected_t0 = 20.0 - 0.01 * expected_z
        expected_q0 = 8.0 - 0.02 * expected_z

        # var=None: 3-panel figure for U, T, q.
        fig_all, axes_all = plot_rsl(df_rsl, var=None)
        assert isinstance(fig_all, plt.Figure)
        assert len(axes_all) == 3
        xlabels = [a.get_xlabel() for a in axes_all]
        assert xlabels[0].startswith("U ")
        assert xlabels[1].startswith("T ")
        assert xlabels[2].startswith("q ")
        assert all(a.get_ylabel() == "z (m)" for a in axes_all)

        # Verify actual plotted profile coordinates and horizontal diagnostic/RSL lines.
        for ax_panel, expected_x0, expected_diag_z in zip(
            axes_all, [expected_u0, expected_t0, expected_q0], [10.0, 2.0, 2.0]
        ):
            profile_lines = [
                ln for ln in ax_panel.get_lines() if len(ln.get_xdata()) == 30
            ]
            assert len(profile_lines) == 2
            np.testing.assert_allclose(profile_lines[0].get_ydata(), expected_z)
            np.testing.assert_allclose(profile_lines[0].get_xdata(), expected_x0)
            hline_ys = {
                float(ln.get_ydata()[0])
                for ln in ax_panel.get_lines()
                if len(ln.get_ydata()) == 2 and ln.get_ydata()[0] == ln.get_ydata()[1]
            }
            assert expected_diag_z in hline_ys
            assert 25.0 in hline_ys

        # Single variable: default, existing ax, existing fig.
        fig_u, ax_u = plot_rsl(df_rsl, var="U")
        assert isinstance(fig_u, plt.Figure)
        assert ax_u.get_xlabel().startswith("U ")
        assert len(ax_u.get_lines()) == 2
        np.testing.assert_allclose(ax_u.get_lines()[0].get_xdata(), expected_u0)
        np.testing.assert_allclose(ax_u.get_lines()[0].get_ydata(), expected_z)

        fig_t_in, ax_t_in = plt.subplots()
        fig_t_out, ax_t_out = plot_rsl(df_rsl, var="T", fig=None, ax=ax_t_in)
        assert fig_t_out is fig_t_in
        assert ax_t_out is ax_t_in
        assert len(ax_t_out.get_lines()) == 2

        fig_q_in, _ = plt.subplots()
        fig_q_out, ax_q_out = plot_rsl(df_rsl, var="q", fig=fig_q_in, ax=None)
        assert fig_q_out is fig_q_in
        assert ax_q_out.get_xlabel().startswith("q ")
        assert len(ax_q_out.get_lines()) == 2

        # Invalid variable logs an error and returns None.
        with supy_caplog.at_level(logging.ERROR, logger="SuPy"):
            res_bad = plot_rsl(df_rsl, var="invalid_var")
        assert res_bad is None
        assert any("invalid_var" in rec.message for rec in supy_caplog.records)


class TestEra5OfflineUtilities:
    """Offline tests for ``supy.util._era5`` (strictly zero network calls)."""

    def test_geopotential_and_ellipsoid_invariants(self):
        """Geoid radius matches WGS84 axes and geopotential<->geometric round-trips."""
        assert roundPartial(51.37, 0.25) == pytest.approx(51.25)
        assert roundPartial(-0.13, 0.25) == pytest.approx(-0.25)

        assert _geoid_radius(0.0) == pytest.approx(Rmax_WGS84, rel=1e-12)
        assert _geoid_radius(90.0) == pytest.approx(Rmin_WGS84, rel=1e-12)
        assert _geoid_radius(-45.0) == pytest.approx(_geoid_radius(45.0), rel=1e-12)
        assert Rmin_WGS84 < _geoid_radius(45.0) < Rmax_WGS84

        for lat in (-90.0, -33.8, 0.0, 51.5, 90.0):
            assert geometric2geopotential(0.0, lat) == pytest.approx(0.0, abs=1e-12)
            for z in (10.0, 100.0, 5000.0):
                h = geometric2geopotential(z, lat)
                z_back = geopotential2geometric(h, lat)
                assert z_back == pytest.approx(z, rel=1e-12)

    def test_request_metadata_helpers_gen_dict_and_gen_fn(self):
        """Datetime partitioning, filename formatting, and CDS dataset selection."""
        ser_dt = pd.Series(pd.date_range("2023-01-31 22:00", periods=4, freq="h"))
        dt_dict = gen_dict_dt(ser_dt)
        assert dt_dict["year"] == ["2023"]
        assert dt_dict["month"] == ["01", "02"]
        assert dt_dict["day"] == ["31", "01"]
        assert dt_dict["time"] == ["22:00", "23:00", "00:00", "01:00"]

        sub = gen_dict_dt_sub(ser_dt)
        assert set(sub.keys()) == {"202301", "202302"}
        assert sub["202301"]["day"] == ["31"]
        assert sub["202302"]["day"] == ["01"]

        req_sfc = {
            "area": [52.0, -1.0, 51.0, 0.0],
            "year": ["2023"],
            "month": ["07"],
            "variable": ["2m_temperature", "surface_pressure"],
        }
        assert gen_fn(req_sfc) == "51.5N0.5W-202307-sfc.nc"
        proc_sfc = gen_dict_proc(req_sfc)
        assert proc_sfc["name"] == "reanalysis-era5-single-levels"
        assert proc_sfc["target"] == "51.5N0.5W-202307-sfc.nc"

        req_ml = {
            "area": [-33.0, 150.0, -34.0, 152.0],
            "year": ["2023"],
            "month": ["01"],
            "variable": ["temperature", "geopotential"],
        }
        assert gen_fn(req_ml) == "33.5S151.0E-202301-ml.nc"
        proc_ml = gen_dict_proc(req_ml)
        assert proc_ml["name"] == "reanalysis-era5-pressure-levels"

    def test_chdir_and_safe_stdout_stderr_restore_on_normal_and_error_exit(
        self, tmp_path, monkeypatch
    ):
        """chdir and safe_stdout_stderr always restore state on both normal and error exits."""
        orig_cwd = Path.cwd()
        with chdir(tmp_path):
            assert Path.cwd().resolve() == tmp_path.resolve()
        assert Path.cwd().resolve() == orig_cwd.resolve()

        with pytest.raises(RuntimeError, match="boom"), chdir(tmp_path):
            raise RuntimeError("boom")
        assert Path.cwd().resolve() == orig_cwd.resolve()

        # When both streams are present, safe_stdout_stderr is a no-op.
        orig_out, orig_err = sys.stdout, sys.stderr
        with safe_stdout_stderr():
            assert sys.stdout is orig_out
            assert sys.stderr is orig_err

        # When streams are None (headless GUI), devnull UTF-8 streams are supplied and restored.
        monkeypatch.setattr(sys, "stdout", None)
        monkeypatch.setattr(sys, "stderr", None)
        with safe_stdout_stderr():
            assert sys.stdout is not None
            assert sys.stderr is not None
            sys.stdout.write("progress check\n")
            sys.stderr.write("stderr check\n")
        assert sys.stdout is None
        assert sys.stderr is None

    def test_download_cds_skips_existing_and_moves_new_file(self, tmp_path):
        """download_cds skips existing files and retrieves+moves missing files."""
        existing = tmp_path / "already.nc"
        existing.write_bytes(b"existing-data")
        mock_client = Mock()

        with patch("cdsapi.Client", return_value=mock_client):
            download_cds(
                str(existing), {"name": "ds", "request": {}, "target": "already.nc"}
            )
        mock_client.retrieve.assert_not_called()

        target = tmp_path / "downloaded.nc"

        def fake_retrieve(**kwargs):
            Path(kwargs["target"]).write_bytes(b"synthetic-nc-bytes")

        mock_client.retrieve.side_effect = fake_retrieve
        with patch("cdsapi.Client", return_value=mock_client):
            download_cds(
                str(target),
                {
                    "name": "reanalysis-era5-single-levels",
                    "request": {},
                    "target": target.name,
                },
            )

        assert target.is_file()
        assert target.read_bytes() == b"synthetic-nc-bytes"

    def test_download_era5_timeseries_existing_mkdir_and_empty_zip_error(
        self, tmp_path
    ):
        """download_era5_timeseries handles nested dir creation, cache hit, and empty zip."""
        nested_dir = tmp_path / "nested" / "era5"
        existing_csv = nested_dir / "33.5S151.2E-2022-sfc.csv"
        nested_dir.mkdir(parents=True)
        existing_csv.write_text("cached\n", encoding="utf-8")

        mock_client = Mock()
        with patch("cdsapi.Client", return_value=mock_client):
            res = download_era5_timeseries(
                lat_x=-33.5,
                lon_x=151.2,
                start="2022-01-01",
                end="2022-01-02",
                dir_save=nested_dir,
            )
        assert Path(res) == existing_csv.resolve()
        mock_client.retrieve.assert_not_called()

        # Non-existent dir_save + zip archive without any CSV raises ValueError.
        fresh_dir = tmp_path / "auto_created" / "sub"

        def write_empty_zip(download_path):
            with zipfile.ZipFile(download_path, "w") as zf:
                zf.writestr("readme.txt", "no csv here")

        mock_retrieve = Mock()
        mock_retrieve.download.side_effect = write_empty_zip
        mock_client.retrieve.return_value = mock_retrieve

        with (
            patch("cdsapi.Client", return_value=mock_client),
            pytest.raises(ValueError, match="No CSV file found"),
        ):
            download_era5_timeseries(
                lat_x=51.5,
                lon_x=-0.1,
                start="2022-01-01",
                end="2022-01-02",
                dir_save=fresh_dir,
            )
        assert fresh_dir.is_dir()

    def test_format_df_forcing_unit_conversions_and_clamping(self):
        """format_df_forcing applies exact SI->SUEWS conversions, sign flips, and bounds."""
        idx = pd.date_range("2023-06-15 10:00", periods=3, freq="h")
        raw = pd.DataFrame(
            {
                "ssrd": [
                    -3600.0,
                    360000.0,
                    720000.0,
                ],  # J/m2 -> -1 (clamped 0), 100, 200 W/m2
                "strd": [1080000.0, 1116000.0, 1152000.0],  # 300, 310, 320 W/m2
                "sshf": [
                    -180000.0,
                    -360000.0,
                    72000.0,
                ],  # flipped sign -> +50, +100, -20
                "slhf": [-90000.0, -180000.0, 36000.0],  # flipped sign -> +25, +50, -10
                "tp": [0.0, 0.0025, 0.01],  # m -> 0, 2.5, 10.0 mm
                "t_z": [273.15, 293.15, 303.15],  # K -> 0, 20, 30 degC
                "p_z": [101325.0, 100000.0, 99500.0],  # Pa -> 101.33, 100.0, 99.5 kPa
                "uv_z": [2.5, 4.0, 6.25],
                "RH_z": [0.0, 65.5, 120.0],  # out-of-range clamped to 105
                "alt_z": [100.0, 100.0, 100.0],
            },
            index=idx,
        )

        formatted = format_df_forcing(raw)

        np.testing.assert_allclose(formatted["kdown"].values, [0.0, 100.0, 200.0])
        np.testing.assert_allclose(formatted["ldown"].values, [300.0, 310.0, 320.0])
        np.testing.assert_allclose(formatted["qh"].values, [50.0, 100.0, -20.0])
        np.testing.assert_allclose(formatted["qe"].values, [25.0, 50.0, -10.0])
        np.testing.assert_allclose(formatted["rain"].values, [0.0, 2.5, 10.0])
        np.testing.assert_allclose(formatted["Tair"].values, [0.0, 20.0, 30.0])
        np.testing.assert_allclose(formatted["pres"].values, [101.32, 100.0, 99.5])
        np.testing.assert_allclose(formatted["RH"].values, [105.0, 65.5, 105.0])
        # Unprovided SUEWS columns are filled with -999 sentinel.
        assert (formatted["qn"] == -999).all()
        assert formatted["iy"].dtype == np.int32
        assert list(formatted["it"]) == [10, 11, 12]

    def test_save_forcing_era5_multi_grid_and_year_boundary_single_row(self, tmp_path):
        """save_forcing_era5 splits by grid and year, handling single-row years cleanly."""
        times = pd.DatetimeIndex([
            "2022-12-31 23:00",  # single row in 2022 -> defaults to 60 min
            "2023-01-01 00:00",
            "2023-01-01 01:00",
        ])
        frames = []
        grid_temps = {
            (51.5, 0.25): [5.0, 4.5, 4.0],
            (-33.5, -70.5): [25.0, 24.5, 24.0],
        }
        for (lat, lon), temps in grid_temps.items():
            df_g = pd.DataFrame(
                {
                    "iy": times.year,
                    "id": times.dayofyear,
                    "it": times.hour,
                    "imin": times.minute,
                    "Tair": temps,
                    "alt_z": [100.0, 100.0, 100.0],
                    "latitude": lat,
                    "longitude": lon,
                },
                index=times,
            )
            df_g.index.name = "time"
            frames.append(
                df_g.reset_index().set_index(["latitude", "longitude", "time"])
            )
        df_all = pd.concat(frames)

        files = save_forcing_era5(df_all, tmp_path)

        by_name = {Path(f).name: Path(f) for f in files}
        expected_partitions = {
            "ERA5_UTC-33.5S-70.5W-100.0A_2022_data_60.txt": ([2022], [25.0]),
            "ERA5_UTC-33.5S-70.5W-100.0A_2023_data_60.txt": (
                [2023, 2023],
                [24.5, 24.0],
            ),
            "ERA5_UTC-51.5N-0.25E-100.0A_2022_data_60.txt": ([2022], [5.0]),
            "ERA5_UTC-51.5N-0.25E-100.0A_2023_data_60.txt": (
                [2023, 2023],
                [4.5, 4.0],
            ),
        }
        assert set(by_name.keys()) == set(expected_partitions.keys())
        for fname, (exp_years, exp_tair) in expected_partitions.items():
            df_read = pd.read_csv(by_name[fname], sep=" ")
            assert "alt_z" not in df_read.columns
            assert list(df_read["iy"]) == exp_years
            np.testing.assert_allclose(df_read["Tair"].values, exp_tair)

    def test_gen_forcing_era5_end_to_end_offline_with_mocked_cds(self, tmp_path):
        """gen_forcing_era5 chains download -> diagnostics -> format -> save offline."""
        csv_data = (
            "valid_time,latitude,longitude,sp,u10,v10,t2m,d2m,ssrd,strd,tp\n"
            "2023-07-01 00:00:00,51.5,-0.1,101325,3.0,4.0,288.15,283.15,0,1080000,0\n"
            "2023-07-01 01:00:00,51.5,-0.1,101300,3.0,4.0,289.15,284.15,360000,1116000,0.002\n"
        )

        def write_mock_zip(download_path):
            with zipfile.ZipFile(download_path, "w") as zf:
                zf.writestr("era5_point.csv", csv_data)

        mock_client = Mock()
        mock_retrieve = Mock()
        mock_retrieve.download.side_effect = write_mock_zip
        mock_client.retrieve.return_value = mock_retrieve

        with patch("cdsapi.Client", return_value=mock_client):
            files = gen_forcing_era5(
                lat_x=51.5,
                lon_x=-0.1,
                start="2023-07-01",
                end="2023-07-01",
                dir_save=tmp_path,
                hgt_agl_diag=100.0,
                logging_level=logging.WARNING,
            )

        assert len(files) == 1
        out_path = Path(files[0])
        assert out_path.name == "ERA5_UTC-51.5N-0.1W-100.0A_2023_data_60.txt"
        df_out = pd.read_csv(out_path, sep=" ")
        assert len(df_out) == 2
        np.testing.assert_allclose(df_out["kdown"].values, [0.0, 100.0])
        np.testing.assert_allclose(df_out["rain"].values, [0.0, 2.0])
        # Temperature at 100 m is ~0.64 K cooler than 2 m (15.0 C -> ~14.36 C).
        assert df_out["Tair"].iloc[0] == pytest.approx(15.0 - 98.0 * 0.0065, abs=0.02)
