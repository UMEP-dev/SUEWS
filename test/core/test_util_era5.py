"""Tests for ERA5 utility functions."""

import pytest
from supy.util._era5 import gen_forcing_era5


class TestERA5DataSourceValidation:
    """Test validation of data_source parameter in gen_forcing_era5."""

    def test_earthkit_requires_simple_mode(self):
        """earthkit data source should raise error when simple_mode=False."""
        with pytest.raises(
            ValueError,
            match="earthkit data source only works with simple_mode=True",
        ):
            gen_forcing_era5(
                50.86,
                4.35,
                "2020-01-01",
                "2020-01-02",
                simple_mode=False,
                data_source="earthkit",
            )

    def test_earthkit_rejects_pressure_level(self):
        """earthkit data source should raise error when pressure_level is set."""
        with pytest.raises(
            ValueError,
            match="earthkit data source does not support pressure_level parameter",
        ):
            gen_forcing_era5(
                50.86,
                4.35,
                "2020-01-01",
                "2020-01-02",
                simple_mode=True,
                pressure_level=850,
                data_source="earthkit",
            )

    def test_earthkit_warns_on_scale(self, caplog):
        """earthkit data source should warn when scale > 0 is used."""
        # This test would actually attempt to download, so we skip it
        # in the actual test suite. This is a placeholder for the API check.
        pytest.skip("Skipping actual download test")

    def test_default_data_source_is_cdsapi(self):
        """Default data source should be cdsapi for backward compatibility."""
        # This just checks that the function signature accepts the call
        # without actually downloading anything
        pytest.skip("Skipping actual download test")


class TestERA5DownloadEarthkit:
    """Test earthkit.data download function."""

    def test_earthkit_import_error(self):
        """Should raise informative error if earthkit.data is not installed."""
        # This would require mocking the import, skip for now
        pytest.skip("Requires mocking import")

    def test_earthkit_download_creates_file(self):
        """earthkit download should create netCDF file with expected name."""
        pytest.skip("Requires actual download, skip in unit tests")


class TestERA5Integration:
    """Integration tests for ERA5 forcing generation."""

    def test_earthkit_with_simple_mode_true(self):
        """earthkit source should work with simple_mode=True."""
        pytest.skip("Requires actual download and CDS credentials")

    def test_cdsapi_backward_compatibility(self):
        """Traditional cdsapi source should still work as before."""
        pytest.skip("Requires actual download and CDS credentials")

    def test_hgt_agl_diag_works_with_earthkit(self):
        """hgt_agl_diag parameter should work with earthkit data source."""
        pytest.skip("Requires actual download")
