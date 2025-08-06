"""Test RSL convergence improvements for high z0/low FAI scenarios (Issue #338)"""

import numpy as np
import pytest
import supy as sp
import pandas as pd
from pathlib import Path


class TestRSLConvergence:
    """Test suite for RSL convergence improvements"""

    @pytest.fixture
    def base_config(self):
        """Load base configuration from benchmark"""
        df_state_init, df_forcing = sp.load_SampleData()
        # Use only first few timesteps for convergence testing
        df_forcing = df_forcing.iloc[:48]  # 2 days of hourly data
        return df_state_init, df_forcing

    def create_high_z0_low_fai_config(self, df_state_init):
        """Create configuration with high z0 and low FAI that triggers convergence issues"""
        df_modified = df_state_init.copy()

        # Set high roughness length (z0m) relative to low frontal area index
        # These values are known to cause convergence issues
        df_modified["z0m_In"] = 2.5  # High roughness length
        df_modified["FAI"] = 0.15  # Low frontal area index
        df_modified["PAI"] = 0.2  # Low plan area index
        df_modified["H_Bldg"] = 15.0  # Building height

        # Ensure RSL is used (DiagMethod = 2 for automatic selection)
        df_modified["DiagMethod"] = 2

        return df_modified

    def test_rsl_convergence_high_z0_low_fai(self, base_config):
        """Test that RSL calculations converge for problematic high z0/low FAI case"""
        df_state_init, df_forcing = base_config

        # Create problematic configuration
        df_state_high_z0 = self.create_high_z0_low_fai_config(df_state_init)

        # Run simulation - should not crash or produce NaN values
        df_output, df_state_final = sp.run_supy(
            df_forcing, df_state_high_z0, save_state=True
        )

        # Check for convergence - no NaN values in key outputs
        assert not df_output["QH"].isna().any(), (
            "QH contains NaN values - convergence failed"
        )
        assert not df_output["QE"].isna().any(), (
            "QE contains NaN values - convergence failed"
        )
        assert not df_output["U10"].isna().any(), (
            "U10 contains NaN values - convergence failed"
        )
        assert not df_output["T2"].isna().any(), (
            "T2 contains NaN values - convergence failed"
        )

        # Check reasonable value ranges
        assert df_output["QH"].abs().max() < 1000, "QH values unreasonably large"
        assert df_output["QE"].abs().max() < 1000, "QE values unreasonably large"
        assert (df_output["U10"] >= 0).all() and (df_output["U10"] < 50).all(), (
            "U10 out of reasonable range"
        )
        assert (df_output["T2"] > -50).all() and (df_output["T2"] < 60).all(), (
            "T2 out of reasonable range"
        )

    def test_rsl_neutral_stability_handling(self, base_config):
        """Test improved neutral stability handling in RSL calculations"""
        df_state_init, df_forcing = base_config

        # Create configuration with conditions near neutral stability
        df_modified = self.create_high_z0_low_fai_config(df_state_init)

        # Modify forcing to create near-neutral conditions
        df_forcing_neutral = df_forcing.copy()
        df_forcing_neutral["U"] = 5.0  # Moderate wind speed
        df_forcing_neutral["dq"] = 0.0  # No moisture gradient

        # Run simulation
        df_output, df_state_final = sp.run_supy(
            df_forcing_neutral, df_modified, save_state=True
        )

        # Check outputs are reasonable under neutral conditions
        assert not df_output["L_mod"].isna().any(), "L_mod contains NaN values"
        # In neutral conditions, |L_mod| should be large
        assert (df_output["L_mod"].abs() > 100).sum() > 0, (
            "No neutral conditions detected"
        )

    def test_rsl_comparison_with_most(self, base_config):
        """Compare RSL and MOST approaches for the same configuration"""
        df_state_init, df_forcing = base_config

        # Configuration with problematic parameters
        df_state_test = self.create_high_z0_low_fai_config(df_state_init)

        # Run with RSL (DiagMethod = 1 forces RSL)
        df_state_rsl = df_state_test.copy()
        df_state_rsl["DiagMethod"] = 1
        df_output_rsl, _ = sp.run_supy(df_forcing, df_state_rsl)

        # Run with MOST (DiagMethod = 0 forces MOST)
        df_state_most = df_state_test.copy()
        df_state_most["DiagMethod"] = 0
        df_output_most, _ = sp.run_supy(df_forcing, df_state_most)

        # Both should produce valid results
        assert not df_output_rsl["QH"].isna().any(), "RSL: QH contains NaN"
        assert not df_output_most["QH"].isna().any(), "MOST: QH contains NaN"

        # Results should be different but in same order of magnitude
        qh_diff = (df_output_rsl["QH"] - df_output_most["QH"]).abs().mean()
        qh_mean = (
            df_output_rsl["QH"].abs().mean() + df_output_most["QH"].abs().mean()
        ) / 2
        relative_diff = qh_diff / qh_mean if qh_mean > 0 else 0

        assert relative_diff < 2.0, (
            f"RSL and MOST results differ too much: {relative_diff:.2f}"
        )

    @pytest.mark.parametrize(
        "fai,z0m",
        [
            (0.1, 3.0),  # Very low FAI, very high z0
            (0.15, 2.5),  # Original problematic case
            (0.2, 2.0),  # Slightly higher FAI
            (0.25, 1.5),  # Moderate FAI
        ],
    )
    def test_rsl_convergence_parameter_sweep(self, base_config, fai, z0m):
        """Test RSL convergence across a range of problematic FAI/z0 combinations"""
        df_state_init, df_forcing = base_config

        # Create configuration with specified parameters
        df_modified = df_state_init.copy()
        df_modified["z0m_In"] = z0m
        df_modified["FAI"] = fai
        df_modified["PAI"] = fai + 0.05  # PAI slightly higher than FAI
        df_modified["H_Bldg"] = 15.0
        df_modified["DiagMethod"] = 2  # Auto-select

        # Run simulation
        df_output, df_state_final = sp.run_supy(
            df_forcing, df_modified, save_state=True
        )

        # Check convergence
        assert not df_output["QH"].isna().any(), f"QH NaN for FAI={fai}, z0m={z0m}"
        assert not df_output["QE"].isna().any(), f"QE NaN for FAI={fai}, z0m={z0m}"
        assert df_output["QH"].abs().max() < 1000, (
            f"QH unreasonable for FAI={fai}, z0m={z0m}"
        )
