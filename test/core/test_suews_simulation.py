"""Concise test suite for SUEWSSimulation class using sample data."""

from pathlib import Path
import time

import pytest

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

import supy as sp
from supy.suews_sim import SUEWSSimulation
from supy.data_model import SUEWSConfig


class TestInit:
    """Test initialization."""

    def test_empty_init(self):
        """Test empty initialization."""
        sim = SUEWSSimulation()
        assert sim.config is None
        assert sim.forcing is None
        assert sim.results is None

    def test_yaml_init(self):
        """Test initialization from YAML."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))
        assert sim.config is not None
        assert sim._df_state_init is not None


class TestConfig:
    """Test configuration updates."""

    def test_update_config_yaml(self):
        """Test updating config from another YAML file."""
        # Create initial simulation
        sim = SUEWSSimulation()
        assert sim.config is None

        # Update with YAML config
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim.update_config(str(yaml_path))

        assert sim.config is not None
        assert sim._df_state_init is not None
        assert len(sim._df_state_init) > 0

    def test_invalid_config_path(self):
        """Test invalid config path."""
        with pytest.raises(FileNotFoundError):
            SUEWSSimulation("nonexistent.yml")


class TestForcing:
    """Test forcing data loading."""

    def test_dataframe_forcing(self):
        """Test loading forcing from DataFrame."""
        _, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim.update_forcing(df_forcing.iloc[:24])  # 2 hours only
        assert len(sim.forcing) == 24

    def test_invalid_forcing_path(self):
        """Test invalid forcing path."""
        sim = SUEWSSimulation()
        with pytest.raises(FileNotFoundError):
            sim.update_forcing("nonexistent.txt")


class TestRun:
    """Test simulation execution."""

    def test_basic_run(self):
        """Test basic simulation run."""
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])  # 2 hours

        results = sim.run()
        assert results is not None
        assert len(results) > 0
        assert "QH" in results.columns.get_level_values("var")

    def test_run_without_forcing(self):
        """Test run fails without forcing."""
        sim = SUEWSSimulation()
        sim._df_state_init, _ = sp.load_SampleData()

        with pytest.raises(RuntimeError, match="No forcing"):
            sim.run()


class TestSave:
    """Test result saving."""

    def test_save_default(self, tmp_path):
        """Test saving results."""
        # Quick run
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])
        sim.run()

        # Save
        paths = sim.save(tmp_path)
        assert isinstance(paths, list)
        assert len(paths) > 0
        assert any(Path(p).exists() for p in paths)

    def test_save_without_results(self):
        """Test save fails without results."""
        sim = SUEWSSimulation()
        with pytest.raises(RuntimeError, match="No simulation results"):
            sim.save()


class TestReset:
    """Test reset functionality."""

    def test_reset_clears_results(self):
        """Test reset clears results."""
        # Run simulation
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])
        sim.run()

        # Reset
        sim.reset()
        assert sim._df_output is None
        assert sim._run_completed is False

        # Can run again
        results = sim.run()
        assert results is not None


class TestBypassValidators:
    """Test validator bypass functionality."""

    def test_bypass_validators_init(self):
        """Test initialization with bypass_validators flag."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Test with validators enabled (default)
        sim_with_validation = SUEWSSimulation(str(yaml_path), bypass_validators=False)
        assert sim_with_validation.config is not None
        assert sim_with_validation._bypass_validators is False

        # Test with validators bypassed
        sim_bypass = SUEWSSimulation(str(yaml_path), bypass_validators=True)
        assert sim_bypass.config is not None
        assert sim_bypass._bypass_validators is True

    def test_bypass_validators_performance(self):
        """Test that bypassing validators improves performance."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Measure time with validation
        start_time = time.time()
        for _ in range(3):
            sim = SUEWSSimulation(str(yaml_path), bypass_validators=False)
        time_with_validation = time.time() - start_time

        # Measure time without validation
        start_time = time.time()
        for _ in range(3):
            sim = SUEWSSimulation(str(yaml_path), bypass_validators=True)
        time_without_validation = time.time() - start_time

        # Bypassing validators should be faster
        # Allow some tolerance for system variations
        assert time_without_validation <= time_with_validation * 1.1

    def test_bypass_validators_from_yaml(self):
        """Test SUEWSConfig.from_yaml with bypass_validators."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Test normal loading
        config_validated = SUEWSConfig.from_yaml(
            str(yaml_path), bypass_validators=False
        )
        assert config_validated is not None

        # Test bypass loading
        config_bypass = SUEWSConfig.from_yaml(str(yaml_path), bypass_validators=True)
        assert config_bypass is not None

    def test_bypass_validators_to_df_state(self):
        """Test to_df_state with bypass_validators."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        config = SUEWSConfig.from_yaml(str(yaml_path), bypass_validators=True)

        # Test conversion without validation
        df_state = config.to_df_state(bypass_validators=True)
        assert df_state is not None
        assert len(df_state) > 0

        # Test conversion with validation (should still work)
        df_state_validated = config.to_df_state(bypass_validators=False)
        assert df_state_validated is not None
        assert len(df_state_validated) > 0


class TestIntegration:
    """Test complete workflows."""

    def test_yaml_workflow(self, tmp_path):
        """Test YAML config → run → save workflow."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Load from YAML
        sim = SUEWSSimulation(str(yaml_path))

        # Override with short forcing
        _, df_forcing = sp.load_SampleData()
        sim.update_forcing(df_forcing.iloc[:48])  # 4 hours

        # Run and save
        results = sim.run()
        paths = sim.save(tmp_path)

        assert len(results) > 0
        assert len(paths) > 0

    def test_bypass_validators_workflow(self, tmp_path):
        """Test complete workflow with bypass_validators enabled."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Load with validators bypassed
        sim = SUEWSSimulation(str(yaml_path), bypass_validators=True)

        # Add forcing
        _, df_forcing = sp.load_SampleData()
        sim.update_forcing(df_forcing.iloc[:48])  # 4 hours

        # Run and save
        results = sim.run()
        paths = sim.save(tmp_path)

        assert len(results) > 0
        assert len(paths) > 0
