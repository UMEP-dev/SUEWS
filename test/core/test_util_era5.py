"""Tests for ERA5 utility functions."""

import pytest
from supy.util._era5 import gen_forcing_era5


class TestERA5Import:
    """Test ERA5 module imports correctly."""

    def test_import_gen_forcing_era5(self):
        """Test that gen_forcing_era5 can be imported."""
        assert callable(gen_forcing_era5)

    def test_function_signature(self):
        """Test that gen_forcing_era5 has expected simplified signature."""
        import inspect

        sig = inspect.signature(gen_forcing_era5)
        params = list(sig.parameters.keys())

        # Check simplified parameters (removed: grid, scale, force_download, simple_mode, pressure_level, data_source)
        assert "lat_x" in params
        assert "lon_x" in params
        assert "start" in params
        assert "end" in params
        assert "dir_save" in params
        assert "hgt_agl_diag" in params
        assert "logging_level" in params

        # Ensure removed parameters are gone
        assert "data_source" not in params
        assert "simple_mode" not in params
        assert "pressure_level" not in params
        assert "grid" not in params
        assert "scale" not in params


class TestERA5Integration:
    """Integration tests for ERA5 forcing generation (require CDS credentials)."""

    def test_timeseries_download(self):
        """Test that timeseries download works (requires CDS credentials)."""
        pytest.skip("Requires actual download and CDS credentials")

    def test_hgt_agl_diag_parameter(self):
        """Test hgt_agl_diag parameter works."""
        pytest.skip("Requires actual download")
