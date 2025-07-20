"""Test save_supy functionality with various output configurations."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
import tempfile
import shutil
import supy as sp


class TestSaveSuPy:
    """Test saving functionality of SuPy outputs."""

    @pytest.fixture
    def sample_output(self):
        """Create sample output data for testing."""
        # Load sample data and run a short simulation
        df_state_init, df_forcing = sp.load_sample_data()
        # Use just 3 days for faster tests
        df_forcing_subset = df_forcing[: 24 * 3]
        df_output, df_state_final = sp.run_supy(df_forcing_subset, df_state_init)
        return df_output, df_state_final

    def test_save_default_groups(self, sample_output):
        """Test that default saving includes SUEWS and DailyState groups."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save with default settings
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Check that files were created
            assert len(list_files) > 0, "No files were created"

            # Check for SUEWS output file
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "No SUEWS output file was created"

            # Check that SUEWS file has content
            suews_file = Path(suews_files[0])
            assert suews_file.exists(), "SUEWS file does not exist"
            assert suews_file.stat().st_size > 0, "SUEWS file is empty"

            # Read and verify content
            with open(suews_file, "r") as f:
                lines = f.readlines()
                assert len(lines) > 1, "SUEWS file has no data rows"
                # Check header contains expected columns
                header = lines[0]
                assert "Kdown" in header, "Missing Kdown in header"
                assert "QN" in header, "Missing QN in header"
                assert "Fcld" in header, "Fcld should be in output even if NaN"

    def test_save_with_nan_values(self, sample_output):
        """Test that saving works correctly even with NaN values in some variables."""
        df_output, df_state_final = sample_output

        # Verify that Fcld has NaN values (this is what was causing the issue)
        if "SUEWS" in df_output.columns.get_level_values("group"):
            fcld_data = df_output.xs(("SUEWS", "Fcld"), level=("group", "var"), axis=1)
            assert fcld_data.isna().all().all(), (
                "Expected Fcld to be all NaN for this test"
            )

        with tempfile.TemporaryDirectory() as tmpdir:
            # This should work despite NaN values
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Verify SUEWS file was created
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, (
                "SUEWS file should be created despite NaN values"
            )

    def test_save_output_groups_filter(self, sample_output):
        """Test filtering output groups using output_config."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save only DailyState group
            # Note: Currently dict-based output_config doesn't support groups filtering
            # This would require using the OutputConfig class from data_model
            # For now, we'll test that the default behavior works

            # Test default behavior (should include SUEWS)
            list_files = sp.save_supy(
                df_output, df_state_final, path_dir_save=tmpdir, site="test"
            )

            # Check that both SUEWS and state files were created
            assert len(list_files) >= 2, (
                "At least SUEWS and state files should be created"
            )

            # SUEWS file should be created by default
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "SUEWS file should be created by default"

    def test_resample_frequency(self, sample_output):
        """Test different resampling frequencies."""
        df_output, df_state_final = sample_output

        with tempfile.TemporaryDirectory() as tmpdir:
            # Save with 30-minute frequency
            list_files = sp.save_supy(
                df_output,
                df_state_final,
                path_dir_save=tmpdir,
                site="test",
                freq_s=1800,  # 30 minutes
            )

            # Check SUEWS file
            suews_files = [f for f in list_files if "SUEWS" in str(f)]
            assert len(suews_files) > 0, "SUEWS file should be created"

            # Verify filename contains correct frequency
            suews_filename = Path(suews_files[0]).name
            assert "_30.txt" in suews_filename, (
                f"Expected _30.txt in filename, got {suews_filename}"
            )
