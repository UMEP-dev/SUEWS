"""Tests for DTS multi-grid support.

Verifies that run_dts_multi iterates over all sites in a configuration,
produces output with a grid-level MultiIndex, and matches single-grid
run_dts results for the single-site case.

Note: These tests require a full build with DTS type wrappers (make dev-dts).
      They are automatically skipped when running with a fast build (make dev).
"""

import copy
import os

import pandas as pd
import pytest

from supy import load_SampleData
from supy.data_model import SUEWSConfig
from supy.dts import _DTS_AVAILABLE, run_dts, run_dts_multi

# Skip all tests in this module if DTS is not available (fast build)
pytestmark = pytest.mark.skipif(
    not _DTS_AVAILABLE, reason="DTS not available (fast build without type wrappers)"
)

# Number of timesteps for quick tests (4 hours at 5-min resolution)
N_TIMESTEPS = 48


@pytest.fixture
def sample_data():
    """Load sample data and build a single-site SUEWSConfig."""
    df_state_init, df_forcing = load_SampleData()
    grid_id = df_state_init.index.get_level_values("grid")[0]
    config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])
    df_forcing_subset = df_forcing.iloc[:N_TIMESTEPS]
    return config, df_forcing_subset


@pytest.fixture
def multi_site_config(sample_data):
    """Create a two-site config by duplicating the sample site with a different gridiv."""
    config, df_forcing = sample_data
    # Deep-copy the existing site and assign a new grid id
    site_copy = copy.deepcopy(config.sites[0])
    site_copy.gridiv = 99
    site_copy.name = "duplicate site"
    config_multi = copy.deepcopy(config)
    config_multi.sites.append(site_copy)
    return config_multi, df_forcing


class TestRunDtsMultiSingleGrid:
    """Tests for run_dts_multi with a single-site config."""

    @pytest.mark.core
    def test_single_grid_output_has_grid_index(self, sample_data):
        """Output from single-site run_dts_multi has a grid level in the index."""
        config, df_forcing = sample_data
        df_output, _ = run_dts_multi(df_forcing, config)

        assert isinstance(df_output.index, pd.MultiIndex)
        assert "grid" in df_output.index.names
        assert "datetime" in df_output.index.names

    @pytest.mark.core
    def test_single_grid_matches_run_dts(self, sample_data):
        """Single-site run_dts_multi matches run_dts output values."""
        config, df_forcing = sample_data

        df_single, _ = run_dts(df_forcing, config, site_index=0)
        df_multi, _ = run_dts_multi(df_forcing, config)

        # Both should have identical values
        pd.testing.assert_frame_equal(df_multi, df_single)

    @pytest.mark.core
    def test_single_grid_state_dict(self, sample_data):
        """Final states dict has one entry for the single grid."""
        config, df_forcing = sample_data
        _, dict_states = run_dts_multi(df_forcing, config)

        grid_id = config.sites[0].gridiv
        assert grid_id in dict_states
        assert len(dict_states) == 1
        assert "initial_states" in dict_states[grid_id]


class TestRunDtsMultiMultiGrid:
    """Tests for run_dts_multi with a multi-site config."""

    @pytest.mark.core
    def test_multi_grid_output_has_grid_index(self, multi_site_config):
        """Output from multi-site run has expected grid values in the index."""
        config, df_forcing = multi_site_config
        df_output, _ = run_dts_multi(df_forcing, config)

        grids = df_output.index.get_level_values("grid").unique()
        expected_grids = {s.gridiv for s in config.sites}
        assert set(grids) == expected_grids

    @pytest.mark.core
    def test_multi_grid_output_shape(self, multi_site_config):
        """Multi-grid output has n_timesteps * n_grids rows."""
        config, df_forcing = multi_site_config
        df_output, _ = run_dts_multi(df_forcing, config)

        n_grids = len(config.sites)
        assert len(df_output) == N_TIMESTEPS * n_grids

    @pytest.mark.core
    def test_multi_grid_state_per_grid(self, multi_site_config):
        """Final states dict has one entry per grid."""
        config, df_forcing = multi_site_config
        _, dict_states = run_dts_multi(df_forcing, config)

        assert len(dict_states) == len(config.sites)
        for site in config.sites:
            assert site.gridiv in dict_states
            assert "initial_states" in dict_states[site.gridiv]

    @pytest.mark.core
    @pytest.mark.skipif(
        os.name == "nt" or os.environ.get("CI") == "true",
        reason="Parallel DTS execution skipped on Windows and in CI environments",
    )
    def test_multi_grid_parallel_matches_serial(self, multi_site_config):
        """Parallel run matches serial output for multi-site configs."""
        config, df_forcing = multi_site_config

        df_serial, _ = run_dts_multi(df_forcing, config, n_jobs=1)
        df_parallel, dict_states = run_dts_multi(df_forcing, config, n_jobs=2)

        pd.testing.assert_frame_equal(df_parallel, df_serial)

        for site in config.sites:
            assert site.gridiv in dict_states
            assert "initial_states" in dict_states[site.gridiv]

    @pytest.mark.core
    @pytest.mark.skipif(
        os.name == "nt" or os.environ.get("CI") == "true",
        reason="Parallel DTS execution skipped on Windows and in CI environments",
    )
    def test_multi_grid_parallel_njobs_clamped(self, multi_site_config):
        """n_jobs larger than site count is clamped without error."""
        config, df_forcing = multi_site_config

        df_output, dict_states = run_dts_multi(df_forcing, config, n_jobs=8)

        n_grids = len(config.sites)
        assert len(df_output) == N_TIMESTEPS * n_grids
        for site in config.sites:
            assert site.gridiv in dict_states


class TestSimulationDtsMultiGrid:
    """Tests for SUEWSSimulation.run(backend='dts') with multi-grid config."""

    @pytest.mark.core
    def test_simulation_dts_multi_grid(self, multi_site_config):
        """SUEWSSimulation.run(backend='dts') works with multi-site config."""
        from supy import SUEWSSimulation

        config, df_forcing = multi_site_config

        sim = SUEWSSimulation(config=config)
        sim.update_forcing(df_forcing)

        output = sim.run(backend="dts")
        df = output.df

        grids = df.index.get_level_values("grid").unique()
        expected_grids = {s.gridiv for s in config.sites}
        assert set(grids) == expected_grids
        assert len(df) == N_TIMESTEPS * len(config.sites)
