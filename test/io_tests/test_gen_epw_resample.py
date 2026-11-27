"""Test gen_epw with resampling functionality (GitHub issue #150)."""

from importlib import import_module
import sys
import types

import atmosp
from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

import supy as sp

_umep2epw = import_module("supy.util._UMEP2epw")

pytestmark = [pytest.mark.native, pytest.mark.contract("C-RESAMPLE", "C-SAVE")]


@pytest.fixture(scope="module")
def pvlib_available():
    """Skip only the gen_epw tests when pvlib is unavailable."""
    pytest.importorskip(
        "pvlib",
        reason="gen_epw delegates to pvlib for solar geometry; skip only the gen_epw tests when the optional dependency is missing",
    )


class TestGenEpwResample:
    """Tests for gen_epw with the object-oriented resampling interface."""

    def test_resample_output_uses_output_object(self, sample_run_cached):
        """Test that resampling is available only on ``SUEWSOutput``."""
        assert not hasattr(sp.util, "resample_output")

        df_output, df_state = sample_run_cached(48)
        df_hourly = sp.SUEWSOutput(df_output, df_state).resample("h").df

        # Should have fewer rows after resampling
        assert len(df_hourly) < len(df_output)

        # Structure should be preserved
        assert isinstance(df_hourly.index, pd.MultiIndex)
        assert "grid" in df_hourly.index.names

    def test_resample_output_frequency_aliases(self, sample_run_cached):
        """Test that different frequency aliases work correctly."""
        df_output, df_state = sample_run_cached(144)  # 12 hours
        output = sp.SUEWSOutput(df_output, df_state)

        # Test various frequency aliases
        for freq in ["30min", "60min", "h", "1h"]:
            df_resampled = output.resample(freq).df
            assert len(df_resampled) > 0

        # Hourly should have fewer rows than 30-minute
        df_30min = output.resample("30min").df
        df_hourly = output.resample("h").df
        assert len(df_hourly) < len(df_30min)

    def test_resample_aggregation_methods(self, sample_run_cached):
        """Test that aggregation methods are applied correctly."""
        df_output, df_state = sample_run_cached(TIMESTEPS_PER_DAY)  # 1 day
        df_hourly = sp.SUEWSOutput(df_output, df_state).resample("h").df

        # Get first grid
        grid = df_hourly.index.get_level_values("grid")[0]

        # Check that SUEWS variables exist
        assert "SUEWS" in df_hourly.columns.get_level_values("group").unique()

        # Variables used by gen_epw should be present
        suews_vars = df_hourly.loc[grid, "SUEWS"].columns.tolist()
        for var in ["T2", "RH2", "U10", "Kdown"]:
            assert var in suews_vars, f"Variable {var} not found in resampled output"


def test_gen_epw_converts_q2_to_si_without_cfunits(
    tmp_path,
    monkeypatch: pytest.MonkeyPatch,
):
    """gen_epw converts Q2 (g kg-1 -> kg kg-1) without requiring optional cfunits."""
    idx = pd.date_range("2012-01-01 01:00:00", periods=23, freq="h")
    df_suews = pd.DataFrame(
        {
            "T2": np.linspace(5.0, 15.0, len(idx)),
            "RH2": np.full(len(idx), 65.0),
            "U10": np.full(len(idx), 3.0),
            "Q2": np.linspace(4.0, 8.0, len(idx)),
            "Kdown": np.maximum(0.0, 250.0 * np.sin(np.linspace(0, np.pi, len(idx)))),
            "Ldown": np.full(len(idx), 310.0),
        },
        index=idx,
    )

    # Stub only pvlib so this unit test runs on every CI lane without pvlib
    # while exercising real atmosp.calculate without optional cfunits.
    fake_pvlib = types.ModuleType("pvlib")
    fake_solar = types.ModuleType("pvlib.solarposition")
    fake_irrad = types.ModuleType("pvlib.irradiance")
    fake_solar.get_solarposition = lambda times, lat, lon: pd.DataFrame(
        {"zenith": np.full(len(times), 45.0)}, index=times
    )
    fake_irrad.dirint = lambda ghi, times, **kwargs: pd.Series(
        np.zeros(len(times)), index=times
    )
    fake_pvlib.solarposition = fake_solar
    fake_pvlib.irradiance = fake_irrad
    monkeypatch.setitem(sys.modules, "pvlib", fake_pvlib)

    seen_calls: list[dict] = []
    real_calc = atmosp.calculate

    def _spy_calc(var, **kwargs):
        seen_calls.append({"var": var, "kwargs": dict(kwargs)})
        return real_calc(var, **kwargs)

    monkeypatch.setattr(atmosp, "calculate", _spy_calc)

    df_epw, _meta, out_path = sp.util.gen_epw(
        df_suews,
        lat=51.5,
        lon=-0.1,
        path_epw=tmp_path / "si_units.epw",
    )
    assert out_path.is_file()
    assert len(seen_calls) == 2
    for call in seen_calls:
        assert not any("unit" in key for key in call["kwargs"]), (
            f"atmosp.calculate must not rely on optional cfunits unit kwargs: {call}"
        )
        np.testing.assert_allclose(
            call["kwargs"]["qv"],
            df_suews["Q2"].values * 1e-3,
            rtol=1e-12,
            atol=0.0,
        )

    expected_td = (
        real_calc(
            "Td",
            T=df_suews["T2"].values + 273.15,
            qv=df_suews["Q2"].values * 1e-3,
            RH=df_suews["RH2"].values,
            rho=1.23,
        )
        - 273.15
    )
    expected_p = real_calc(
        "p",
        T=df_suews["T2"].values + 273.15,
        qv=df_suews["Q2"].values * 1e-3,
        RH=df_suews["RH2"].values,
        rho=1.23,
    )
    np.testing.assert_allclose(
        df_epw["Dew Point Temperature"].values, expected_td, rtol=1e-12
    )
    np.testing.assert_allclose(
        df_epw["Atmospheric Station Pressure"].values, expected_p, rtol=1e-12
    )


@pytest.mark.usefixtures("pvlib_available")
class TestGenEpwMultiIndexInput:
    """Tests for gen_epw handling MultiIndex input directly."""

    @pytest.fixture
    def sample_output(self, sample_run_cached):
        """Create sample SUEWS output for testing.

        1 day (``TIMESTEPS_PER_DAY`` steps) - enough data for a meaningful
        test but not too much. Function-scoped so each test gets its own
        copy, backed by the shared session cache.
        """
        df_output, _ = sample_run_cached(TIMESTEPS_PER_DAY)  # 1 day
        return df_output

    def test_gen_epw_accepts_multiindex(self, sample_output, tmp_path):
        """Test that gen_epw accepts MultiIndex input without freq."""
        grid = sample_output.index.get_level_values("grid")[0]

        df_epw, meta, path = sp.util.gen_epw(
            sample_output.loc[grid, "SUEWS"],
            lat=51.5,
            lon=-0.1,
            path_epw=tmp_path / "test.epw",
        )
        assert isinstance(df_epw, pd.DataFrame)

    def test_gen_epw_with_grid_extraction(self, sample_output, tmp_path):
        """Test that gen_epw extracts grid automatically from MultiIndex."""
        df_epw, meta, path = sp.util.gen_epw(
            sample_output,
            lat=51.5,
            lon=-0.1,
            path_epw=tmp_path / "test_auto.epw",
        )
        assert isinstance(df_epw, pd.DataFrame)

    def test_gen_epw_with_freq_param(self, sample_output, tmp_path):
        """Test that gen_epw accepts freq parameter for resampling."""
        df_epw, meta, path = sp.util.gen_epw(
            sample_output,
            lat=51.5,
            lon=-0.1,
            freq="h",
            path_epw=tmp_path / "test_freq.epw",
        )
        assert isinstance(df_epw, pd.DataFrame)

    def test_gen_epw_with_specific_grid(self, sample_output, tmp_path):
        """Test that gen_epw accepts specific grid parameter."""
        grid = sample_output.index.get_level_values("grid")[0]

        df_epw, meta, path = sp.util.gen_epw(
            sample_output,
            lat=51.5,
            lon=-0.1,
            grid=grid,
            path_epw=tmp_path / "test_grid.epw",
        )
        assert isinstance(df_epw, pd.DataFrame)

    def test_gen_epw_freq_with_extracted_data(self, sample_output, tmp_path):
        """Test that freq works with pre-extracted single-grid data."""
        grid = sample_output.index.get_level_values("grid")[0]
        df_single = sample_output.loc[grid, "SUEWS"]

        df_epw, meta, path = sp.util.gen_epw(
            df_single,
            lat=51.5,
            lon=-0.1,
            freq="h",
            path_epw=tmp_path / "test_extracted_freq.epw",
        )
        assert isinstance(df_epw, pd.DataFrame)


def test_umep_patchup_epw_writes_valid_lf_epw_without_metpy(tmp_path):
    """``patchup_epw`` uses pandas ``lineterminator`` and portable dates without requiring metpy (#2111)."""
    idx = pd.date_range("2013-01-01 01:00:00", periods=3, freq="h")
    cols = [f"c{i}" for i in range(35)]
    data = np.ones((len(idx), 35), dtype=float)
    data[:, 0] = 2013
    data[:, 1] = 1
    data[:, 2] = 1
    data[:, 3] = [1, 2, 3]
    data[:, 4] = 0
    data[:, 6] = [12.34, 13.45, 14.56]
    data[:, 7] = [8.12, 8.23, 8.34]
    data[:, 21] = [2.34, 3.45, 4.56]
    # Include NaN values in columns patchup_epw replaces before integer rounding.
    data[:, 16:19] = np.nan
    data[:, 26] = np.nan
    data[:, 27] = np.nan
    data[:, 29] = np.nan
    data[:, 34] = np.nan

    df_epw = pd.DataFrame(data, index=idx, columns=cols)
    df_epw["c5"] = "?9?9?9?9E0"
    header = ["placeholder"] * 8
    out_epw = tmp_path / "repaired.epw"

    df_fixed, header_fixed, path_fixed = _umep2epw.patchup_epw(
        df_epw, header, out_epw, lat=47.5, lon=19.12, tz=1.0, alt=163.0
    )

    assert path_fixed == out_epw
    assert out_epw.is_file()
    raw_bytes = out_epw.read_bytes()
    assert b"\r\n" not in raw_bytes
    lines = raw_bytes.decode("utf-8").splitlines()
    assert len(lines) == 8 + len(idx)
    assert header_fixed[0] == "LOCATION,NA,NA,NA,UMEP/SuPy,NA,47.5,19.12,1.0,163.0"
    assert header_fixed[7] == "DATA PERIODS,1,1,Year_2013,Tuesday,1/1,1/1"
    assert (df_fixed.iloc[:, 16:19] == 999999).all().all()
    assert (df_fixed.iloc[:, 26] == 9).all()
    assert (df_fixed.iloc[:, 27] == 999999999).all()
    assert np.allclose(df_fixed.iloc[:, 29], 0.999)
    assert (df_fixed.iloc[:, 34] == 99).all()
