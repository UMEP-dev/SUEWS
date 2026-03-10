"""Test gen_epw with resampling functionality (GitHub issue #150)."""

import numpy as np
import pandas as pd
import pytest

import supy as sp
from conftest import TIMESTEPS_PER_DAY


class TestGenEpwResample:
    """Tests for gen_epw with frequency parameter and resample_output exposure."""

    def test_resample_output_in_util(self):
        """Test that resample_output is accessible via supy.util."""
        assert hasattr(sp.util, "resample_output")

        # Test it works with actual data
        df_state_init, df_forcing = sp.load_SampleData()
        df_output, _ = sp.run_supy(df_forcing.iloc[:48], df_state_init)

        df_hourly = sp.util.resample_output(df_output, freq="h")

        # Should have fewer rows after resampling
        assert len(df_hourly) < len(df_output)

        # Structure should be preserved
        assert isinstance(df_hourly.index, pd.MultiIndex)
        assert "grid" in df_hourly.index.names

    def test_resample_output_frequency_aliases(self):
        """Test that different frequency aliases work correctly."""
        df_state_init, df_forcing = sp.load_SampleData()
        df_output, _ = sp.run_supy(df_forcing.iloc[:144], df_state_init)  # 12 hours

        # Test various frequency aliases
        for freq in ["30min", "60min", "h", "1h"]:
            df_resampled = sp.util.resample_output(df_output, freq=freq)
            assert len(df_resampled) > 0

        # Hourly should have fewer rows than 30-minute
        df_30min = sp.util.resample_output(df_output, freq="30min")
        df_hourly = sp.util.resample_output(df_output, freq="h")
        assert len(df_hourly) < len(df_30min)

    def test_resample_aggregation_methods(self):
        """Test that aggregation methods are applied correctly."""
        df_state_init, df_forcing = sp.load_SampleData()
        df_output, _ = sp.run_supy(
            df_forcing.iloc[:TIMESTEPS_PER_DAY], df_state_init
        )  # 1 day

        df_hourly = sp.util.resample_output(df_output, freq="h")

        # Get first grid
        grid = df_hourly.index.get_level_values("grid")[0]

        # Check that SUEWS variables exist
        assert "SUEWS" in df_hourly.columns.get_level_values("group").unique()

        # Variables used by gen_epw should be present
        suews_vars = df_hourly.loc[grid, "SUEWS"].columns.tolist()
        for var in ["T2", "RH2", "U10", "Kdown"]:
            assert var in suews_vars, f"Variable {var} not found in resampled output"


class TestGenEpwMultiIndexInput:
    """Tests for gen_epw handling MultiIndex input directly."""

    @pytest.fixture
    def sample_output(self):
        """Create sample SUEWS output for testing."""
        df_state_init, df_forcing = sp.load_SampleData()
        # Use enough data for meaningful test but not too much
        df_output, _ = sp.run_supy(
            df_forcing.iloc[:TIMESTEPS_PER_DAY], df_state_init
        )  # 1 day
        return df_output

    def test_gen_epw_accepts_multiindex(self, sample_output, tmp_path):
        """Test that gen_epw accepts MultiIndex input without freq."""
        grid = sample_output.index.get_level_values("grid")[0]

        try:
            # This should work - providing extracted data (original behaviour)
            df_epw, meta, path = sp.util.gen_epw(
                sample_output.loc[grid, "SUEWS"],
                lat=51.5,
                lon=-0.1,
                path_epw=tmp_path / "test.epw",
            )
            assert isinstance(df_epw, pd.DataFrame)
        except ImportError:
            pytest.skip("pvlib not installed")

    def test_gen_epw_with_grid_extraction(self, sample_output, tmp_path):
        """Test that gen_epw extracts grid automatically from MultiIndex."""
        try:
            # This should auto-extract first grid
            df_epw, meta, path = sp.util.gen_epw(
                sample_output,  # Full MultiIndex
                lat=51.5,
                lon=-0.1,
                path_epw=tmp_path / "test_auto.epw",
            )
            assert isinstance(df_epw, pd.DataFrame)
        except ImportError:
            pytest.skip("pvlib not installed")

    def test_gen_epw_with_freq_param(self, sample_output, tmp_path):
        """Test that gen_epw accepts freq parameter for resampling."""
        try:
            df_epw, meta, path = sp.util.gen_epw(
                sample_output,  # Full MultiIndex
                lat=51.5,
                lon=-0.1,
                freq="h",  # Resample to hourly
                path_epw=tmp_path / "test_freq.epw",
            )
            assert isinstance(df_epw, pd.DataFrame)
        except ImportError:
            pytest.skip("pvlib not installed")

    def test_gen_epw_with_specific_grid(self, sample_output, tmp_path):
        """Test that gen_epw accepts specific grid parameter."""
        grid = sample_output.index.get_level_values("grid")[0]

        try:
            df_epw, meta, path = sp.util.gen_epw(
                sample_output,
                lat=51.5,
                lon=-0.1,
                grid=grid,  # Specify grid explicitly
                path_epw=tmp_path / "test_grid.epw",
            )
            assert isinstance(df_epw, pd.DataFrame)
        except ImportError:
            pytest.skip("pvlib not installed")

    def test_gen_epw_freq_with_extracted_data(self, sample_output, tmp_path):
        """Test that freq works with pre-extracted single-grid data."""
        grid = sample_output.index.get_level_values("grid")[0]
        df_single = sample_output.loc[grid, "SUEWS"]

        try:
            df_epw, meta, path = sp.util.gen_epw(
                df_single,
                lat=51.5,
                lon=-0.1,
                freq="h",  # Should still work with simple resampling
                path_epw=tmp_path / "test_extracted_freq.epw",
            )
            assert isinstance(df_epw, pd.DataFrame)
        except ImportError:
            pytest.skip("pvlib not installed")
