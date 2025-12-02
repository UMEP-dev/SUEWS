"""Tests for UMEP import path compatibility (GH-901).

This module verifies that all import paths used by UMEP plugins work correctly.

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

from unittest import TestCase


class TestImportPaths(TestCase):
    """Test that all UMEP-used import paths work correctly."""

    def test_import_suews(self):
        """Test basic SUEWS package import."""
        import supy

        self.assertIsNotNone(supy)

    def test_import_suews_data_model(self):
        """Test SUEWS data_model import."""
        from supy import data_model

        self.assertIsNotNone(data_model)

    def test_import_init_config_from_yaml(self):
        """Test sp.data_model.init_config_from_yaml import path."""
        from supy.data_model import init_config_from_yaml

        self.assertIsNotNone(init_config_from_yaml)

    def test_import_generate_json_schema(self):
        """Test supy.data_model.schema.publisher.generate_json_schema import path."""
        from supy.data_model.schema.publisher import generate_json_schema

        self.assertIsNotNone(generate_json_schema)

    def test_import_validate_single_file(self):
        """Test supy.cmd.validate_config.validate_single_file import path."""
        from supy.cmd.validate_config import validate_single_file

        self.assertIsNotNone(validate_single_file)

    def test_import_gen_forcing_era5(self):
        """Test supy.util.gen_forcing_era5 import path."""
        from supy.util import gen_forcing_era5

        self.assertIsNotNone(gen_forcing_era5)
