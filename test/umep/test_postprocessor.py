"""Tests for UMEP Post-processor API compatibility (GH-901).

This module tests the output path handling used by UMEP Post-processor:
- yaml_dict['model']['control']['output_file']['path']

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

from pathlib import Path
from unittest import TestCase

import supy as sp


class TestOutputPathHandling(TestCase):
    """Test output path handling used by UMEP Post-processor.

    UMEP Post-processor requires the YAML config to include output path:
    yaml_dict['model']['control']['output_file']['path']
    """

    def test_config_contains_output_path(self):
        """Test that config structure includes output path."""
        from supy.data_model import init_config_from_yaml

        sample_config = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"

        if not sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(sample_config)

        # Navigate to output path as UMEP does
        self.assertTrue(hasattr(config, "model"))
        self.assertTrue(hasattr(config.model, "control"))
        self.assertTrue(hasattr(config.model.control, "output_file"))

        # The output_file should have a path attribute
        output_file = config.model.control.output_file
        self.assertTrue(
            hasattr(output_file, "path"),
            "output_file must have 'path' attribute for UMEP compatibility",
        )
