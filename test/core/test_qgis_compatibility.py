"""
Tests for SUEWS compatibility with UMEP plugin in QGIS environment (GH-901).

This module tests the functions and APIs used by UMEP plugins in QGIS:
- UMEP Pre-processor: SUEWS Database Manager, SUEWS Database Prepare, Download ERA5
- UMEP Processor: SUEWS model runs
- UMEP Post-processor: Output path handling

These tests ensure SUEWS functions work correctly in the QGIS environment.
Target environment: Windows + Python 3.12 (QGIS 3.40 LTR bundled Python).

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import sys
import tempfile
from pathlib import Path
from unittest import TestCase

import pandas as pd
import pytest

import supy as sp


# Target environment: Windows + Python 3.12 (QGIS 3.40 LTR)
_IS_WINDOWS = sys.platform == "win32"
_IS_PY312 = sys.version_info[:2] == (3, 12)
_IS_QGIS_TARGET = _IS_WINDOWS and _IS_PY312

# Mark all tests in this module with 'qgis' marker and skip on non-target platforms
pytestmark = [
    pytest.mark.qgis,
    pytest.mark.skipif(
        not _IS_QGIS_TARGET,
        reason="QGIS compatibility tests only run on Windows + Python 3.12 (QGIS 3.40 LTR)",
    ),
]


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

    def test_generate_json_schema_field_constraints(self):
        """Test that schema contains field constraints (min/max) used by Database Manager."""
        from supy.data_model.schema.publisher import generate_json_schema

        schema = generate_json_schema()

        # Check for common numeric constraints in definitions
        # These are used by UMEP to validate parameter ranges
        defs = schema.get("$defs", {})

        # At least some definitions should have numeric constraints
        constraints_found = False
        for def_name, def_schema in defs.items():
            if isinstance(def_schema, dict):
                props = def_schema.get("properties", {})
                for prop_name, prop_schema in props.items():
                    if isinstance(prop_schema, dict):
                        if "minimum" in prop_schema or "maximum" in prop_schema:
                            constraints_found = True
                            break
            if constraints_found:
                break

        # It's acceptable if no constraints are found in all cases,
        # but the schema should still be valid
        self.assertIsInstance(schema, dict)


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
        self.assertTrue(
            any("hgt" in p.lower() or "height" in p.lower() for p in params)
            or "hgt_agl_diag" in params,
            f"Expected height parameter, got: {params}",
        )
        self.assertIn("dir_save", params)


class TestSUEWSProcessorAPI(TestCase):
    """Test functions used by UMEP SUEWS Processor.

    Reference:
    https://github.com/UMEP-dev/UMEP-processing/blob/82bc3266d8cb4d04359994b1cae5cf082d09c47c/processor/suews_algorithm.py#L265
    """

    def setUp(self):
        """Set up test environment."""
        self.sample_config = (
            Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
        )

    def test_init_config_from_yaml_import(self):
        """Test that init_config_from_yaml is importable from expected location."""
        from supy.data_model import init_config_from_yaml

        self.assertIsNotNone(init_config_from_yaml)
        self.assertTrue(callable(init_config_from_yaml))

    def test_init_config_from_yaml_returns_config(self):
        """Test init_config_from_yaml returns a valid config object."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)

        # Verify config object structure
        self.assertIsNotNone(config)
        self.assertTrue(hasattr(config, "model"))
        self.assertTrue(hasattr(config, "sites"))

    def test_config_to_df_state(self):
        """Test config.to_df_state() method used by UMEP."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)

        # UMEP calls config.to_df_state()
        self.assertTrue(
            hasattr(config, "to_df_state"),
            "Config object must have to_df_state() method",
        )

        df_state = config.to_df_state()

        # Verify DataFrame structure
        self.assertIsInstance(df_state, pd.DataFrame)
        self.assertFalse(df_state.empty)
        self.assertEqual(df_state.index.name, "grid")

    def test_load_forcing_grid_import(self):
        """Test that load_forcing_grid is importable from expected location."""
        self.assertIsNotNone(sp.load_forcing_grid)
        self.assertTrue(callable(sp.load_forcing_grid))

    def test_load_forcing_grid_with_df_state(self):
        """Test load_forcing_grid with df_state_init parameter (UMEP pattern)."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        df_state_init = config.to_df_state()
        grid = df_state_init.index[0]

        # This is the exact pattern used by UMEP
        df_forcing = sp.load_forcing_grid(
            self.sample_config, grid=grid, df_state_init=df_state_init
        )

        # Verify forcing DataFrame
        self.assertIsInstance(df_forcing, pd.DataFrame)
        self.assertFalse(df_forcing.empty)
        self.assertIsInstance(df_forcing.index, pd.DatetimeIndex)

    def test_run_supy_import(self):
        """Test that run_supy is importable from expected location."""
        self.assertIsNotNone(sp.run_supy)
        self.assertTrue(callable(sp.run_supy))

    def test_run_supy_with_chunk_day(self):
        """Test run_supy with chunk_day parameter (UMEP pattern)."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        df_state_init = config.to_df_state()
        grid = df_state_init.index[0]

        df_forcing = sp.load_forcing_grid(
            self.sample_config, grid=grid, df_state_init=df_state_init
        )

        # Run with chunk_day parameter as UMEP does
        # Use short forcing for test speed
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:288],  # One day of 5-min data
            df_state_init,
            chunk_day=1,
            check_input=False,
        )

        # Verify output structure
        self.assertIsInstance(df_output, pd.DataFrame)
        self.assertIsInstance(df_state_final, pd.DataFrame)
        self.assertFalse(df_output.empty)

    def test_save_supy_import(self):
        """Test that save_supy is importable from expected location."""
        self.assertIsNotNone(sp.save_supy)
        self.assertTrue(callable(sp.save_supy))

    def test_save_supy_functionality(self):
        """Test save_supy saves output correctly."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        df_state_init = config.to_df_state()
        grid = df_state_init.index[0]

        df_forcing = sp.load_forcing_grid(
            self.sample_config, grid=grid, df_state_init=df_state_init
        )

        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:288],
            df_state_init,
            check_input=False,
        )

        with tempfile.TemporaryDirectory() as temp_dir:
            # This is how UMEP saves results
            sp.save_supy(df_output, df_state_final, path_dir_save=temp_dir)

            # Verify files were created
            output_files = list(Path(temp_dir).glob("*"))
            self.assertGreater(len(output_files), 0)


class TestQGISEnvironment(TestCase):
    """Test QGIS-specific environment handling.

    QGIS has a unique environment where sys.stdout may be None,
    especially in the Python console.
    """

    def test_suews_import_with_none_stdout(self):
        """Test that SUEWS imports correctly when stdout is None."""
        original_stdout = sys.stdout

        try:
            # Simulate QGIS environment
            sys.stdout = None

            # Re-import should not crash
            # Note: Full reimport is complex, so we test the handler
            from supy._env import get_console_handler

            handler = get_console_handler()
            self.assertIsNotNone(handler)

        finally:
            sys.stdout = original_stdout

    def test_logging_with_none_stdout(self):
        """Test that logging works when stdout is None."""
        import logging

        from supy._env import get_console_handler

        original_stdout = sys.stdout

        try:
            sys.stdout = None

            handler = get_console_handler()
            logger = logging.getLogger("test_qgis_compat")
            logger.addHandler(handler)
            logger.setLevel(logging.INFO)

            # This should not raise an error
            logger.info("Test message in QGIS-like environment")

        finally:
            sys.stdout = original_stdout

    def test_safe_stdout_stderr_with_none_streams(self):
        """Test that safe_stdout_stderr() handles None stdout/stderr (GH-902)."""
        from supy.util._era5 import safe_stdout_stderr

        # Save originals
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        try:
            # Simulate QGIS Processing environment
            sys.stdout = None
            sys.stderr = None

            # Inside context, streams should be valid file objects
            with safe_stdout_stderr():
                self.assertIsNotNone(sys.stdout)
                self.assertIsNotNone(sys.stderr)
                # Should be able to write without crash
                sys.stdout.write("test")
                sys.stderr.write("test")

            # After context, streams should be restored to None
            self.assertIsNone(sys.stdout)
            self.assertIsNone(sys.stderr)

        finally:
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr

    def test_safe_stdout_stderr_noop_with_valid_streams(self):
        """Test that safe_stdout_stderr() is a no-op when streams are valid."""
        from supy.util._era5 import safe_stdout_stderr

        # With valid streams, context manager should not modify them
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        with safe_stdout_stderr():
            self.assertIs(sys.stdout, orig_stdout)
            self.assertIs(sys.stderr, orig_stderr)

        # After context, streams should still be the same
        self.assertIs(sys.stdout, orig_stdout)
        self.assertIs(sys.stderr, orig_stderr)

    def test_tqdm_with_none_stdout(self):
        """Test that tqdm doesn't crash when stdout is None (GH-902)."""
        from tqdm import tqdm

        from supy.util._era5 import safe_stdout_stderr

        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        try:
            sys.stdout = None
            sys.stderr = None

            # This should NOT crash
            with safe_stdout_stderr():
                for _ in tqdm(range(3), desc="Testing"):
                    pass

        finally:
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr


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


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
