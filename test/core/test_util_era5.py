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

    def test_hgt_agl_diag_parameter(self):
        """Test hgt_agl_diag parameter for diagnostic height adjustment.

        The hgt_agl_diag parameter controls the height above ground level (AGL)
        where meteorological variables are diagnosed using vertical extrapolation:

        - Temperature: extrapolated using environmental lapse rate (6.5 K/km)
        - Wind speed: extrapolated using log-law (neutral MOST)
        - Pressure: calculated using barometric formula
        - Humidity: calculated assuming constant relative humidity

        Default is 100m, but can be adjusted for different applications
        (e.g., 10m for urban canopy, 2m for surface layer).
        """
        from supy.util._era5 import gen_df_diag_era5_csv

        # Use fixture data (24h sample from London, Jan 2020)
        fixture_path = Path(__file__).parent.parent / "fixtures" / "data_test" / "era5" / "era5_sample_24h.csv"

        # Test with two different diagnostic heights
        df_50m = gen_df_diag_era5_csv(fixture_path, hgt_agl_diag=50.0)
        df_100m = gen_df_diag_era5_csv(fixture_path, hgt_agl_diag=100.0)

        # Verify output structure
        required_cols = ["uv_z", "t_z", "q_z", "RH_z", "p_z", "alt_z", "ssrd", "strd", "tp", "sshf", "slhf"]
        for col in required_cols:
            assert col in df_50m.columns, f"Missing column in 50m output: {col}"
            assert col in df_100m.columns, f"Missing column in 100m output: {col}"

        # Verify same number of timesteps
        assert len(df_50m) == len(df_100m) == 24

        # Verify vertical extrapolation works correctly
        # Temperature should DECREASE with height (lapse rate ~6.5 K/km)
        # 50m difference should give ~0.325K difference
        temp_diff = (df_50m["t_z"] - df_100m["t_z"]).mean()
        assert 0.2 < temp_diff < 0.5, f"Temperature lapse rate incorrect: {temp_diff:.3f}K (expected ~0.325K)"

        # Wind speed should INCREASE with height (log law)
        wind_diff = (df_100m["uv_z"] - df_50m["uv_z"]).mean()
        assert wind_diff > 0, "Wind speed should increase with height (log law)"

        # Pressure should DECREASE with height (barometric formula)
        pres_diff = (df_50m["p_z"] - df_100m["p_z"]).mean()
        assert pres_diff > 0, "Pressure should decrease with height"

        # Altitude should differ by exactly 50m
        alt_diff = (df_100m["alt_z"] - df_50m["alt_z"]).mean()
        assert abs(alt_diff - 50.0) < 0.01, f"Altitude difference should be 50m, got {alt_diff:.2f}m"

        # Verify coordinates preserved in attrs
        assert df_50m.attrs["latitude"] == 51.5
        assert df_50m.attrs["longitude"] == -0.1


@pytest.mark.skipif(
    not has_cds_credentials(), reason="Requires CDS API credentials (~/.cdsapirc)"
)
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
