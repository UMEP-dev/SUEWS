"""Test gen_epw with resampling functionality (GitHub issue #150)."""

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pytest

import supy as sp

pytestmark = pytest.mark.api


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
