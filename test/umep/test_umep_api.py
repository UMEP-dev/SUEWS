"""UMEP API compatibility tests (GH-901, GH-902).

Consolidated tests for SUEWS compatibility with UMEP plugins in QGIS.
Skipped unless running on Windows + QGIS LTR Python version.

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import inspect
import sys
import tempfile
from pathlib import Path
from unittest import TestCase

import pandas as pd
import pytest

import supy as sp

from . import IS_QGIS_TARGET, QGIS_LTR_PYTHON_VERSION

# Check if safe_stdout_stderr is available
try:
    from supy.util._era5 import safe_stdout_stderr  # noqa: F401
    _HAS_SAFE_STDOUT_STDERR = True
except ImportError:
    _HAS_SAFE_STDOUT_STDERR = False

# Skip all tests in this module unless on QGIS target environment
pytestmark = [
    pytest.mark.qgis,
    pytest.mark.skipif(
        not IS_QGIS_TARGET,
        reason=f"UMEP API tests only run on Windows + Python {QGIS_LTR_PYTHON_VERSION[0]}.{QGIS_LTR_PYTHON_VERSION[1]} (QGIS LTR)",
    ),
]


# =============================================================================
# QGIS Environment Tests
# =============================================================================


class TestQGISEnvironment(TestCase):
    """Test QGIS-specific environment handling.

    QGIS has a unique environment where sys.stdout may be None,
    especially in the Python console.
    """

    def test_suews_import_with_none_stdout(self):
        """Test that SUEWS imports correctly when stdout is None."""
        original_stdout = sys.stdout
        try:
            sys.stdout = None
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
            logger.info("Test message in QGIS-like environment")
        finally:
            sys.stdout = original_stdout

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_safe_stdout_stderr_with_none_streams(self):
        """Test that safe_stdout_stderr() handles None stdout/stderr (GH-902)."""
        from supy.util._era5 import safe_stdout_stderr

        orig_stdout, orig_stderr = sys.stdout, sys.stderr
        try:
            sys.stdout = sys.stderr = None
            with safe_stdout_stderr():
                self.assertIsNotNone(sys.stdout)
                self.assertIsNotNone(sys.stderr)
                sys.stdout.write("test")
                sys.stderr.write("test")
            self.assertIsNone(sys.stdout)
            self.assertIsNone(sys.stderr)
        finally:
            sys.stdout, sys.stderr = orig_stdout, orig_stderr

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_safe_stdout_stderr_noop_with_valid_streams(self):
        """Test that safe_stdout_stderr() is a no-op when streams are valid."""
        from supy.util._era5 import safe_stdout_stderr

        orig_stdout, orig_stderr = sys.stdout, sys.stderr
        with safe_stdout_stderr():
            self.assertIs(sys.stdout, orig_stdout)
            self.assertIs(sys.stderr, orig_stderr)
        self.assertIs(sys.stdout, orig_stdout)
        self.assertIs(sys.stderr, orig_stderr)

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_tqdm_with_none_stdout(self):
        """Test that tqdm doesn't crash when stdout is None (GH-902)."""
        from tqdm import tqdm

        from supy.util._era5 import safe_stdout_stderr

        orig_stdout, orig_stderr = sys.stdout, sys.stderr
        try:
            sys.stdout = sys.stderr = None
            with safe_stdout_stderr():
                for _ in tqdm(range(3), desc="Testing"):
                    pass
        finally:
            sys.stdout, sys.stderr = orig_stdout, orig_stderr


# =============================================================================
# Pre-processor API Tests
# =============================================================================


class TestDatabaseManagerAPI(TestCase):
    """Test functions used by SUEWS Database Manager."""

    def test_generate_json_schema_returns_valid_schema(self):
        """Test that generate_json_schema returns a valid JSON schema."""
        from supy.data_model.schema.publisher import generate_json_schema

        schema = generate_json_schema()
        self.assertIsInstance(schema, dict)
        self.assertIn("$schema", schema)
        self.assertIn("$id", schema)
        self.assertIn("title", schema)
        self.assertIn("properties", schema)
        self.assertIn("$defs", schema)
        self.assertGreater(len(schema["$defs"]), 0)


class TestDatabasePrepareAPI(TestCase):
    """Test functions used by SUEWS Database Prepare."""

    def test_validate_single_file_valid_config(self):
        """Test validation of a valid configuration file."""
        from supy.cmd.validate_config import validate_single_file
        from supy.data_model.schema.publisher import generate_json_schema

        sample_config = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
        if not sample_config.exists():
            self.skipTest("Sample config not available")

        schema = generate_json_schema()
        is_valid, errors = validate_single_file(sample_config, schema)
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
            self.assertIsInstance(is_valid, bool)
            self.assertIsInstance(errors, list)
        finally:
            temp_path.unlink()


class TestERA5DownloadAPI(TestCase):
    """Test functions used by UMEP ERA5 Download."""

    def test_gen_forcing_era5_signature(self):
        """Test that gen_forcing_era5 has expected signature parameters."""
        from supy.util import gen_forcing_era5

        sig = inspect.signature(gen_forcing_era5)
        params = list(sig.parameters.keys())

        self.assertTrue("lat" in params or "lat_x" in params)
        self.assertTrue("lon" in params or "lon_x" in params)
        self.assertIn("start", params)
        self.assertIn("end", params)
        self.assertIn("dir_save", params)
        height_params = [p for p in params if "hgt" in p.lower() or "height" in p.lower()]
        self.assertGreater(len(height_params), 0)


# =============================================================================
# Processor API Tests
# =============================================================================


class TestSUEWSProcessorAPI(TestCase):
    """Test functions used by UMEP SUEWS Processor."""

    def setUp(self):
        """Set up test environment."""
        self.sample_config = (
            Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
        )

    def test_init_config_from_yaml_returns_config(self):
        """Test init_config_from_yaml returns a valid config object."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        self.assertIsNotNone(config)
        self.assertTrue(hasattr(config, "model"))
        self.assertTrue(hasattr(config, "sites"))

    def test_config_to_df_state(self):
        """Test config.to_df_state() method used by UMEP."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        self.assertTrue(hasattr(config, "to_df_state"))
        df_state = config.to_df_state()
        self.assertIsInstance(df_state, pd.DataFrame)
        self.assertFalse(df_state.empty)
        self.assertEqual(df_state.index.name, "grid")

    def test_load_forcing_grid_with_df_state(self):
        """Test load_forcing_grid with df_state_init parameter (UMEP pattern)."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        df_state_init = config.to_df_state()
        grid = df_state_init.index[0]
        df_forcing = sp.load_forcing_grid(
            self.sample_config, grid=grid, df_state_init=df_state_init
        )
        self.assertIsInstance(df_forcing, pd.DataFrame)
        self.assertFalse(df_forcing.empty)
        self.assertIsInstance(df_forcing.index, pd.DatetimeIndex)

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
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:288],
            df_state_init,
            chunk_day=1,
            check_input=False,
        )
        self.assertIsInstance(df_output, pd.DataFrame)
        self.assertIsInstance(df_state_final, pd.DataFrame)
        self.assertFalse(df_output.empty)

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
            sp.save_supy(df_output, df_state_final, path_dir_save=temp_dir)
            output_files = list(Path(temp_dir).glob("*"))
            self.assertGreater(len(output_files), 0)

    def test_config_contains_output_path(self):
        """Test config structure includes output path for UMEP Post-processor."""
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)
        self.assertTrue(hasattr(config, "model"))
        self.assertTrue(hasattr(config.model, "control"))
        self.assertTrue(hasattr(config.model.control, "output_file"))
        output_file = config.model.control.output_file
        self.assertTrue(
            hasattr(output_file, "path"),
            "output_file must have 'path' attribute for UMEP compatibility",
        )
