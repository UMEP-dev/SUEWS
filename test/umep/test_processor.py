"""Tests for UMEP Processor API compatibility (GH-901).

This module tests the functions used by UMEP Processor plugin:
- init_config_from_yaml(): Load configuration
- SUEWSSimulation(config.yml): Preferred YAML-backed runtime
- load_forcing_grid(): Load forcing data
- SUEWSSimulation.run()/SUEWSOutput.save(): Runtime and output handling

Reference:
https://github.com/UMEP-dev/UMEP-processing/blob/82bc3266d8cb4d04359994b1cae5cf082d09c47c/processor/suews_algorithm.py#L265

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

from importlib.resources import as_file
from pathlib import Path
import tempfile
from unittest import TestCase

from conftest import TIMESTEPS_PER_DAY
import pandas as pd

import supy as sp
from supy._env import trv_supy_module


class TestSUEWSProcessorAPI(TestCase):
    """Test functions used by UMEP SUEWS Processor."""

    def setUp(self):
        """Set up test environment."""
        # `as_file` because the UMEP entry points below take real filesystem
        # paths, not packaged-resource handles. The whole directory, because
        # load_forcing_grid resolves the forcing file as a sibling of the config.
        sample_dir = self.enterContext(as_file(trv_supy_module / "sample_data"))
        self.sample_config = sample_dir / "sample_config.yml"

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

    def test_yaml_config_runtime(self):
        """Test the preferred YAML-backed runtime used by UMEP."""
        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        sim = sp.SUEWSSimulation(self.sample_config)
        self.assertIsNotNone(sim.config)
        self.assertIsNotNone(sim.state_init)
        self.assertIsNotNone(sim.forcing)

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
        expected = sp.SUEWSSimulation(self.sample_config).forcing.to_dataframe(
            include_extras=True
        )
        pd.testing.assert_frame_equal(df_forcing, expected)

    def test_simulation_run_with_chunk_day(self):
        """Test the current UMEP simulation runtime pattern."""
        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        simulation = sp.SUEWSSimulation(self.sample_config)

        # Use short forcing for test speed (one day of 5-min data)
        simulation.update_forcing(simulation.forcing.df.iloc[:TIMESTEPS_PER_DAY])
        output = simulation.run(chunk_day=1)

        # Verify output structure
        self.assertIsInstance(output.df, pd.DataFrame)
        self.assertIsInstance(output.state_final, pd.DataFrame)
        self.assertFalse(output.df.empty)

    def test_output_save_functionality(self):
        """Test the current UMEP output-saving pattern."""
        if not self.sample_config.exists():
            self.skipTest("Sample config not available")

        simulation = sp.SUEWSSimulation(self.sample_config)
        simulation.update_forcing(simulation.forcing.df.iloc[:TIMESTEPS_PER_DAY])
        output = simulation.run()

        with tempfile.TemporaryDirectory() as temp_dir:
            output.save(temp_dir)

            # Verify files were created
            output_files = list(Path(temp_dir).glob("*"))
            self.assertGreater(len(output_files), 0)
