"""Tests for ERA5 utility functions."""

import pytest
from pathlib import Path
from supy.util._era5 import gen_forcing_era5


def has_cds_credentials():
    """Check if CDS API credentials are configured."""
    cdsapirc = Path.home() / ".cdsapirc"
    return cdsapirc.exists()


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


@pytest.mark.skipif(not has_cds_credentials(), reason="Requires CDS API credentials (~/.cdsapirc)")
class TestERA5Integration:
    """Integration tests for ERA5 forcing generation.

    These tests run only when CDS API credentials are available.
    Configure credentials at: https://cds.climate.copernicus.eu/api-how-to
    """

    def test_timeseries_download(self, tmp_path):
        """Test ERA5 timeseries download and forcing generation.

        Downloads 2 days of data (~2-3 seconds) to verify:
        - CDS API timeseries download works
        - CSV processing succeeds
        - SUEWS forcing files are generated
        """
        import logging

        list_fn = gen_forcing_era5(
            lat_x=51.5,  # London
            lon_x=-0.1,
            start="2020-01-01",
            end="2020-01-02",
            dir_save=tmp_path,
            logging_level=logging.WARNING,
        )

        # Verify output files exist
        assert len(list_fn) > 0
        assert all(Path(fn).exists() for fn in list_fn)

    def test_hgt_agl_diag_parameter(self, tmp_path):
        """Test hgt_agl_diag parameter for diagnostic height adjustment.

        The hgt_agl_diag parameter controls the height above ground level (AGL)
        where meteorological variables are diagnosed. This requires actual
        atmospheric data to test the vertical extrapolation:

        - Temperature: extrapolated using environmental lapse rate (6.5 K/km)
        - Wind speed: extrapolated using log-law (neutral MOST)
        - Pressure: calculated using barometric formula
        - Humidity: calculated assuming constant relative humidity

        Default is 100m, but can be adjusted for different applications
        (e.g., 10m for urban canopy, 2m for surface layer).
        """
        import logging

        # Test with non-default diagnostic height (50m instead of 100m)
        list_fn = gen_forcing_era5(
            lat_x=51.5,
            lon_x=-0.1,
            start="2020-01-01",
            end="2020-01-02",
            dir_save=tmp_path,
            hgt_agl_diag=50.0,  # 50m diagnostic height
            logging_level=logging.WARNING,
        )

        # Verify files generated with custom height
        assert len(list_fn) > 0

        # Read one file to verify structure
        import pandas as pd
        df = pd.read_csv(list_fn[0], sep=" ")

        # Verify required SUEWS forcing columns exist
        required_cols = ["iy", "id", "it", "imin", "qn", "qh", "qe",
                        "qs", "qf", "U", "RH", "Tair", "pres", "rain", "kdown"]
        for col in required_cols:
            assert col in df.columns, f"Missing column: {col}"
