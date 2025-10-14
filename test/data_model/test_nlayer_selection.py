"""
Test nlayer-based sample config selection and structure validation.

This test suite validates:
- Vertical layer structure for each nlayer value
- Detection and selection functions work correctly
"""

from pathlib import Path
from unittest import TestCase
import warnings

import supy as sp
from supy.data_model import SUEWSConfig
from supy.data_model.validation.pipeline.orchestrator import (
    detect_nlayer_from_user_yaml,
    select_sample_config_by_nlayer,
)


class TestNlayerStructureValidation(TestCase):
    """Test that sample configs have correct vertical layer structure."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)
        self.sample_data_dir = Path(sp.__file__).parent / "sample_data"

    def _validate_structure(self, nlayer):
        """Validate vertical layer structure for given nlayer."""
        config_path = self.sample_data_dir / f"sample_config_{nlayer}.yml"
        config = SUEWSConfig.from_yaml(config_path)
        vl = config.sites[0].properties.vertical_layers

        # Check nlayer value
        self.assertEqual(vl.nlayer.value, nlayer)

        # Check array sizes
        self.assertEqual(len(vl.height.value), nlayer + 1, "height array size")
        self.assertEqual(len(vl.veg_frac.value), nlayer, "veg_frac array size")
        self.assertEqual(
            len(vl.building_frac.value), nlayer, "building_frac array size"
        )
        self.assertEqual(len(vl.roofs), nlayer, "roofs array size")
        self.assertEqual(len(vl.walls), nlayer, "walls array size")

        # Check initial states
        initial_states = config.sites[0].initial_states
        self.assertEqual(len(initial_states.roofs), nlayer, "initial_states roofs")
        self.assertEqual(len(initial_states.walls), nlayer, "initial_states walls")

    def test_structure_all_nlayers(self):
        """Test structure for all nlayer values (1-7)."""
        print("\n========================================")
        print("Testing vertical layer structure for all nlayer values...")

        for nlayer in range(1, 8):
            with self.subTest(nlayer=nlayer):
                self._validate_structure(nlayer)

        print("✓ All nlayer structures validated successfully")


class TestNlayerDetectionAndSelection(TestCase):
    """Test nlayer detection and sample config selection functions."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)
        self.sample_data_dir = Path(sp.__file__).parent / "sample_data"

    def test_detection_and_selection_workflow(self):
        """Test complete workflow: detect nlayer -> select config."""
        print("\n========================================")
        print("Testing nlayer detection and selection workflow...")

        for nlayer in range(1, 8):
            config_path = self.sample_data_dir / f"sample_config_{nlayer}.yml"

            # Detect nlayer
            detected = detect_nlayer_from_user_yaml(str(config_path))
            self.assertEqual(detected, nlayer, f"Detection failed for nlayer={nlayer}")

            # Select config
            selected = select_sample_config_by_nlayer(detected)
            expected = f"sample_config_{nlayer}.yml"
            self.assertEqual(
                selected, expected, f"Selection failed for nlayer={nlayer}"
            )

        print("✓ Detection and selection workflow successful")

    def test_default_behavior(self):
        """Test default behavior for missing files and unsupported values."""
        print("\n========================================")
        print("Testing default behavior...")

        # Missing file defaults to nlayer=3
        detected = detect_nlayer_from_user_yaml("/nonexistent/file.yml")
        self.assertEqual(detected, 3, "Should default to nlayer=3 for missing file")

        # Unsupported values default to sample_config_3.yml
        for unsupported in [0, 8, -1, 100]:
            selected = select_sample_config_by_nlayer(unsupported)
            self.assertEqual(
                selected,
                "sample_config_3.yml",
                f"Should default to sample_config_3.yml for nlayer={unsupported}",
            )

        print("✓ Default behavior works correctly")


if __name__ == "__main__":
    import unittest

    unittest.main()
