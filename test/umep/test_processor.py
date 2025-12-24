"""Tests for UMEP Processor API compatibility (GH-901).

This module tests the functions used by UMEP Processor plugin:
- init_config_from_yaml(): Load configuration
- config.to_df_state(): Convert to DataFrame
- load_forcing_grid(): Load forcing data
- run_supy(): Execute model
- save_supy(): Save output

Reference:
https://github.com/UMEP-dev/UMEP-processing/blob/82bc3266d8cb4d04359994b1cae5cf082d09c47c/processor/suews_algorithm.py#L265

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import tempfile
from pathlib import Path
from unittest import TestCase

import pandas as pd

import supy as sp


class TestSUEWSProcessorAPI(TestCase):
    """Test functions used by UMEP SUEWS Processor."""

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
        # Use short forcing for test speed: 288 = 24h x 12 intervals/h (5-min data)
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:288],
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

        # 288 = 24h x 12 intervals/h (5-min data)
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

    def test_config_contains_output_path(self):
        """Test that config structure includes output path for UMEP Post-processor.

        UMEP Post-processor requires:
        yaml_dict['model']['control']['output_file']['path']
        """
        from supy.data_model import init_config_from_yaml

        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        config = init_config_from_yaml(self.sample_config)

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
