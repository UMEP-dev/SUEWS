"""Tests for UMEP Post-processor API compatibility (GH-901).

This module tests the output path handling used by UMEP Post-processor:
- yaml_dict['model']['control']['output']['dir']  (schema 2026.5.dev8+)

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

from importlib.resources import as_file
from unittest import TestCase

from supy._env import trv_supy_module


class TestOutputPathHandling(TestCase):
    """Test output path handling used by UMEP Post-processor.

    UMEP Post-processor requires the YAML config to include output path:
    yaml_dict['model']['control']['output']['dir']
    """

    def test_config_contains_output_path(self):
        """Test that config structure includes output directory."""
        from supy.data_model import init_config_from_yaml

        sample_resource = trv_supy_module / "sample_data" / "sample_config.yml"

        if not sample_resource.is_file():
            self.skipTest("Sample config not available")

        # `init_config_from_yaml` opens the path itself, so hand it a
        # real file rather than a packaged-resource handle.
        with as_file(sample_resource) as sample_config:
            config = init_config_from_yaml(sample_config)

        # Navigate to output dir as UMEP does
        self.assertTrue(hasattr(config, "model"))
        self.assertTrue(hasattr(config.model, "control"))
        self.assertTrue(hasattr(config.model.control, "output"))

        # The output block should have a dir attribute
        output = config.model.control.output
        self.assertTrue(
            hasattr(output, "dir"),
            "output must have 'dir' attribute for UMEP compatibility",
        )
