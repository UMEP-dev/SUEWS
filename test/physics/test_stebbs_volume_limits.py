"""
Test STEBBS vessel water volume limits (GH-340).

Validates that the maximum volume constraint for domestic hot water (DHW) vessels
works correctly, preventing unbounded accumulation when supply > drain flow rates.
"""

from pathlib import Path
from unittest import TestCase

import numpy as np
import pytest

import supy as sp


class TestSTEBBSVolumeConstraints(TestCase):
    """Test STEBBS DHW vessel volume constraints."""

    @pytest.mark.core
    def test_maximum_volume_capping(self):
        """
        Test that vessel water volume is capped at MaximumVolumeOfDHWinUse.

        This test validates the fix for GH-340:
        - Sets a low maximum volume threshold (2.0 m3)
        - Runs a simulation that would normally accumulate water
        - Verifies Vwater_vessel never exceeds the maximum

        The implementation uses a two-layer approach:
        1. Preventive: Adjusts flow rates to prevent exceeding max
        2. Corrective: Safety cap that clamps volume if exceeded
        """
        # Load STEBBS test configuration
        stebbs_test_dir = (
            Path(__file__).parent.parent / "fixtures" / "data_test" / "stebbs_test"
        )
        config_path = stebbs_test_dir / "sample_config.yml"

        # Initialise simulation
        df_state_init = sp.init_supy(str(config_path))

        # Set a low maximum volume to test capping behaviour
        # Default is 100.0 m3, we set to 2.0 m3 to ensure capping is triggered
        max_volume = 2.0
        df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = max_volume

        # Load forcing data
        df_forcing_full = sp.load_forcing_grid(
            str(config_path), df_state_init.index[0], df_state_init=df_state_init
        )

        # Use one day of data (sufficient to test volume accumulation)
        df_forcing = df_forcing_full.loc["2017-08-26":"2017-08-26"]

        # Run simulation
        df_output, df_state = sp.run_supy(df_forcing, df_state_init)

        # Extract vessel water volume from STEBBS output
        vwater_vessel = df_output.STEBBS["Vwater_vessel"]

        # Verify volume never exceeds maximum
        max_observed = vwater_vessel.max()
        self.assertLessEqual(
            max_observed,
            max_volume + 0.001,  # Small tolerance for floating-point
            f"Vessel water volume ({max_observed:.4f} m3) exceeded maximum "
            f"({max_volume} m3). GH-340 fix not working correctly.",
        )

        # Verify we have valid output (not all zeros/NaN)
        self.assertFalse(
            vwater_vessel.isna().all(),
            "Vwater_vessel is all NaN - STEBBS may not be running correctly",
        )

        # Print diagnostic info
        print(f"\nGH-340 Maximum Volume Test:")
        print(f"  Max volume setting: {max_volume} m3")
        print(f"  Observed max: {max_observed:.4f} m3")
        print(f"  Observed min: {vwater_vessel.min():.4f} m3")
        print(f"  Observed mean: {vwater_vessel.mean():.4f} m3")

    @pytest.mark.core
    def test_maximum_volume_disabled_when_zero(self):
        """
        Test that maximum volume constraint is disabled when set to 0.0.

        Per the implementation, MaximumVolumeOfDHWinUse = 0.0 means
        the upper limit is disabled (no cap applied).
        """
        # Load STEBBS test configuration
        stebbs_test_dir = (
            Path(__file__).parent.parent / "fixtures" / "data_test" / "stebbs_test"
        )
        config_path = stebbs_test_dir / "sample_config.yml"

        # Initialise simulation
        df_state_init = sp.init_supy(str(config_path))

        # Set maximum volume to 0.0 to disable the cap
        df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = 0.0

        # Load forcing data (short period)
        df_forcing_full = sp.load_forcing_grid(
            str(config_path), df_state_init.index[0], df_state_init=df_state_init
        )
        df_forcing = df_forcing_full.loc["2017-08-26":"2017-08-26"]

        # Run simulation - should complete without error
        df_output, df_state = sp.run_supy(df_forcing, df_state_init)

        # Extract vessel water volume
        vwater_vessel = df_output.STEBBS["Vwater_vessel"]

        # With cap disabled, volume can exceed any threshold
        # Just verify simulation ran successfully
        self.assertFalse(
            vwater_vessel.isna().all(),
            "Vwater_vessel is all NaN when max volume disabled",
        )

        print(f"\nDisabled Max Volume Test (MaximumVolumeOfDHWinUse=0.0):")
        print(f"  Observed max: {vwater_vessel.max():.4f} m3")
        print(f"  Observed min: {vwater_vessel.min():.4f} m3")

    @pytest.mark.core
    def test_minimum_volume_still_enforced(self):
        """
        Test that minimum volume constraint still works alongside maximum.

        Ensures the new maximum volume logic doesn't break the existing
        minimum volume constraint (MinimumVolumeOfDHWinUse).
        """
        # Load STEBBS test configuration
        stebbs_test_dir = (
            Path(__file__).parent.parent / "fixtures" / "data_test" / "stebbs_test"
        )
        config_path = stebbs_test_dir / "sample_config.yml"

        # Initialise simulation
        df_state_init = sp.init_supy(str(config_path))

        # Get minimum volume from config (typically 1.0 m3 in test fixtures)
        min_volume = df_state_init.loc[:, ("minimumvolumeofdhwinuse", "0")].iloc[0]

        # Set a reasonable maximum that won't interfere
        df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = 50.0

        # Load forcing data
        df_forcing_full = sp.load_forcing_grid(
            str(config_path), df_state_init.index[0], df_state_init=df_state_init
        )
        df_forcing = df_forcing_full.loc["2017-08-26":"2017-08-26"]

        # Run simulation
        df_output, df_state = sp.run_supy(df_forcing, df_state_init)

        # Extract vessel water volume
        vwater_vessel = df_output.STEBBS["Vwater_vessel"]

        # Verify volume never goes below minimum
        min_observed = vwater_vessel.min()
        self.assertGreaterEqual(
            min_observed,
            min_volume - 0.001,  # Small tolerance for floating-point
            f"Vessel water volume ({min_observed:.4f} m3) went below minimum "
            f"({min_volume} m3). Minimum constraint may be broken.",
        )

        print(f"\nMinimum Volume Constraint Test:")
        print(f"  Min volume setting: {min_volume} m3")
        print(f"  Observed min: {min_observed:.4f} m3")


if __name__ == "__main__":
    import unittest

    unittest.main()
