"""Tests for ERA5 utility functions."""

import pytest
import tempfile
import zipfile
from pathlib import Path
from unittest.mock import Mock, patch
from supy.util._era5 import gen_forcing_era5, download_era5_timeseries


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

        # Use fixture data (real ERA5 download: Copenhagen, 2003, full year)
        fixture_path = (
            Path(__file__).parent.parent
            / "fixtures"
            / "data_test"
            / "era5"
            / "57.7081N11.9653E-2003-sfc.csv"
        )

        # Test with two different diagnostic heights
        df_50m = gen_df_diag_era5_csv(fixture_path, hgt_agl_diag=50.0)
        df_100m = gen_df_diag_era5_csv(fixture_path, hgt_agl_diag=100.0)

        # Verify output structure
        required_cols = [
            "uv_z",
            "t_z",
            "q_z",
            "RH_z",
            "p_z",
            "alt_z",
            "ssrd",
            "strd",
            "tp",
            "sshf",
            "slhf",
        ]
        for col in required_cols:
            assert col in df_50m.columns, f"Missing column in 50m output: {col}"
            assert col in df_100m.columns, f"Missing column in 100m output: {col}"

        # Verify same number of timesteps (full year 2003: 8760 hours)
        assert len(df_50m) == len(df_100m) == 8760

        # Verify vertical extrapolation works correctly
        # Temperature should DECREASE with height (lapse rate ~6.5 K/km)
        # 50m difference should give ~0.325K difference
        temp_diff = (df_50m["t_z"] - df_100m["t_z"]).mean()
        assert 0.2 < temp_diff < 0.5, (
            f"Temperature lapse rate incorrect: {temp_diff:.3f}K (expected ~0.325K)"
        )

        # Wind speed should INCREASE with height (log law)
        wind_diff = (df_100m["uv_z"] - df_50m["uv_z"]).mean()
        assert wind_diff > 0, "Wind speed should increase with height (log law)"

        # Pressure should DECREASE with height (barometric formula)
        pres_diff = (df_50m["p_z"] - df_100m["p_z"]).mean()
        assert pres_diff > 0, "Pressure should decrease with height"

        # Altitude should differ by exactly 50m
        alt_diff = (df_100m["alt_z"] - df_50m["alt_z"]).mean()
        assert abs(alt_diff - 50.0) < 0.01, (
            f"Altitude difference should be 50m, got {alt_diff:.2f}m"
        )

        # Verify coordinates preserved in attrs (Copenhagen location)
        assert df_50m.attrs["latitude"] == 57.75
        assert df_50m.attrs["longitude"] == 12.0


class TestERA5FileCleanup:
    """Test file cleanup behaviour (cross-platform, including Windows)."""

    def test_download_timeseries_cleanup(self, tmp_path):
        """Test that temporary files are properly cleaned up after download.

        This test verifies the fix for GH-891 where Windows file locking
        prevented cleanup of temporary zip files. The TemporaryDirectory
        pattern ensures automatic cleanup on all platforms.
        """
        import pandas as pd
        import io

        # Create mock CSV data matching ERA5 timeseries format
        csv_data = """valid_time,latitude,longitude,sp,u10,v10,t2m,d2m,ssrd,strd,tp
2020-01-01 00:00:00,51.5,-0.1,101325,2.5,1.5,280.15,275.15,0,250,0
2020-01-01 01:00:00,51.5,-0.1,101320,2.6,1.6,280.25,275.25,50,255,0.001
"""

        # Create a mock zip file with the CSV data
        def create_mock_download(download_path):
            """Mock CDS client download that creates a zip with CSV."""
            zip_path = Path(download_path)
            with zipfile.ZipFile(zip_path, "w") as zf:
                zf.writestr("data.csv", csv_data)

        # Mock the CDS client
        mock_client = Mock()
        mock_retrieve = Mock()
        mock_client.retrieve.return_value = mock_retrieve
        mock_retrieve.download.side_effect = create_mock_download

        # Track temporary directories created during test
        temp_dirs_created = []
        original_tempdir = tempfile.TemporaryDirectory

        def track_tempdir(*args, **kwargs):
            td = original_tempdir(*args, **kwargs)
            temp_dirs_created.append(Path(td.name))
            return td

        with patch("cdsapi.Client", return_value=mock_client):
            with patch(
                "tempfile.TemporaryDirectory", side_effect=track_tempdir
            ):
                # Call the download function
                result_path = download_era5_timeseries(
                    lat_x=51.5,
                    lon_x=-0.1,
                    start="2020-01-01",
                    end="2020-01-02",
                    dir_save=tmp_path,
                )

        # Verify the output CSV was created
        assert Path(result_path).exists()
        assert result_path.endswith(".csv")

        # Verify all temporary directories were cleaned up
        for temp_dir in temp_dirs_created:
            assert not temp_dir.exists(), (
                f"Temporary directory not cleaned up: {temp_dir} "
                "(this would cause WinError 32 on Windows)"
            )

    def test_download_timeseries_no_leftover_files(self, tmp_path):
        """Test that no temporary files leak into system temp directory.

        Specifically checks that TemporaryDirectory pattern doesn't leave
        behind .zip files that would accumulate over time.
        """
        import pandas as pd

        csv_data = """valid_time,latitude,longitude,sp,u10,v10,t2m,d2m,ssrd,strd,tp
2020-01-01 00:00:00,51.5,-0.1,101325,2.5,1.5,280.15,275.15,0,250,0
"""

        def create_mock_download(download_path):
            zip_path = Path(download_path)
            with zipfile.ZipFile(zip_path, "w") as zf:
                zf.writestr("data.csv", csv_data)

        mock_client = Mock()
        mock_retrieve = Mock()
        mock_client.retrieve.return_value = mock_retrieve
        mock_retrieve.download.side_effect = create_mock_download

        # Get system temp directory
        system_temp = Path(tempfile.gettempdir())

        # Count .zip files before test
        zip_files_before = list(system_temp.glob("*.zip"))

        with patch("cdsapi.Client", return_value=mock_client):
            download_era5_timeseries(
                lat_x=51.5,
                lon_x=-0.1,
                start="2020-01-01",
                end="2020-01-02",
                dir_save=tmp_path,
            )

        # Count .zip files after test
        zip_files_after = list(system_temp.glob("*.zip"))

        # Should not have created any new .zip files in system temp
        new_zip_files = set(zip_files_after) - set(zip_files_before)
        assert len(new_zip_files) == 0, (
            f"Temporary .zip files leaked: {new_zip_files} "
            "(TemporaryDirectory should clean these up)"
        )


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
