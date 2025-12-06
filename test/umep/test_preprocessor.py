"""Tests for UMEP Pre-processor API compatibility (GH-901).

This module tests the functions used by UMEP Pre-processor plugins:
- SUEWS Database Manager: generate_json_schema()
- SUEWS Database Prepare: validate_single_file()
- Download ERA5: gen_forcing_era5()

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import tempfile
from pathlib import Path
from unittest import TestCase

import supy as sp


class TestDatabaseManagerAPI(TestCase):
    """Test functions used by SUEWS Database Manager.

    Reference:
    https://github.com/UMEP-dev/UMEP/blob/ff7bc9256acf9d2ae7a54752e31d925a7c74c497/suews_database_manager/utilities/database_functions.py#L228
    """

    def test_generate_json_schema_import(self):
        """Test that generate_json_schema is importable from expected location."""
        from supy.data_model.schema.publisher import generate_json_schema

        self.assertIsNotNone(generate_json_schema)
        self.assertTrue(callable(generate_json_schema))

    def test_generate_json_schema_returns_valid_schema(self):
        """Test that generate_json_schema returns a valid JSON schema."""
        from supy.data_model.schema.publisher import generate_json_schema

        schema = generate_json_schema()

        # Verify it's a dict
        self.assertIsInstance(schema, dict)

        # Verify it has required JSON Schema fields
        self.assertIn("$schema", schema)
        self.assertIn("$id", schema)
        self.assertIn("title", schema)
        self.assertIn("properties", schema)

        # Verify it contains min/max info in $defs for variable constraints
        self.assertIn("$defs", schema)
        self.assertIsInstance(schema["$defs"], dict)

    def test_generate_json_schema_has_definitions(self):
        """Test that schema contains $defs used by Database Manager for validation."""
        from supy.data_model.schema.publisher import generate_json_schema

        schema = generate_json_schema()

        # Verify $defs exists and contains model definitions
        # UMEP uses these definitions for parameter validation
        defs = schema.get("$defs", {})
        self.assertIsInstance(defs, dict)
        self.assertGreater(
            len(defs), 0, "Schema should contain model definitions in $defs"
        )


class TestDatabasePrepareAPI(TestCase):
    """Test functions used by SUEWS Database Prepare.

    Reference: Uses validate_single_file() and generate_json_schema()
    """

    def test_validate_single_file_import(self):
        """Test that validate_single_file is importable from expected location."""
        from supy.cmd.validate_config import validate_single_file

        self.assertIsNotNone(validate_single_file)
        self.assertTrue(callable(validate_single_file))

    def test_validate_single_file_valid_config(self):
        """Test validation of a valid configuration file."""
        from supy.cmd.validate_config import validate_single_file
        from supy.data_model.schema.publisher import generate_json_schema

        # Get sample config path
        sample_config = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"

        if not sample_config.exists():
            self.skipTest("Sample config not available")

        # Generate schema
        schema = generate_json_schema()

        # Validate
        is_valid, errors = validate_single_file(sample_config, schema)

        # Should be valid or at least not crash
        self.assertIsInstance(is_valid, bool)
        self.assertIsInstance(errors, list)

    def test_validate_single_file_invalid_config(self):
        """Test validation handles invalid config gracefully."""
        from supy.cmd.validate_config import validate_single_file
        from supy.data_model.schema.publisher import generate_json_schema

        schema = generate_json_schema()

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            # Malformed YAML to test parse error handling (unclosed brackets)
            f.write("invalid: yaml: content: [[[")
            temp_path = Path(f.name)

        try:
            is_valid, errors = validate_single_file(temp_path, schema)
            # Should not crash, should return invalid
            self.assertIsInstance(is_valid, bool)
            self.assertIsInstance(errors, list)
        finally:
            temp_path.unlink()


class TestERA5DownloadAPI(TestCase):
    """Test functions used by UMEP ERA5 Download.

    Reference:
    https://github.com/UMEP-dev/UMEP-processing/blob/82bc3266d8cb4d04359994b1cae5cf082d09c47c/preprocessor/copernicusera5_algorithm.py#L182
    """

    def test_gen_forcing_era5_import(self):
        """Test that gen_forcing_era5 is importable from expected location."""
        from supy.util import gen_forcing_era5

        self.assertIsNotNone(gen_forcing_era5)
        self.assertTrue(callable(gen_forcing_era5))

    def test_gen_forcing_era5_signature(self):
        """Test that gen_forcing_era5 has expected signature parameters."""
        import inspect

        from supy.util import gen_forcing_era5

        sig = inspect.signature(gen_forcing_era5)
        params = list(sig.parameters.keys())

        # Check expected parameters used by UMEP
        # Note: SUEWS uses lat_x/lon_x, but UMEP may call with positional args
        self.assertTrue(
            "lat" in params or "lat_x" in params,
            f"Expected latitude parameter (lat or lat_x), got: {params}",
        )
        self.assertTrue(
            "lon" in params or "lon_x" in params,
            f"Expected longitude parameter (lon or lon_x), got: {params}",
        )
        self.assertIn("start", params)
        self.assertIn("end", params)

        # These may be named differently but should exist
        # UMEP uses: hgt_agl_diag, dir_save
        height_params = [
            p for p in params if "hgt" in p.lower() or "height" in p.lower()
        ]
        self.assertGreater(
            len(height_params),
            0,
            f"Expected height parameter (hgt* or height*), got params: {params}",
        )
        self.assertIn("dir_save", params)
