"""
Simple, focused test suite for SUEWSSimulation class using real benchmark data.

Tests the core functionality without complex mocks or fixtures.
"""

import pytest
import pandas as pd
from pathlib import Path
import sys

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from supy.suews_sim import SUEWSSimulation


class TestSUEWSSimulationBasic:
    """Test basic SUEWSSimulation functionality with real data."""

    @pytest.fixture
    def benchmark_config(self):
        """Path to benchmark configuration file."""
        return Path("test/benchmark1/benchmark1.yml")

    @pytest.fixture
    def benchmark_forcing(self):
        """Path to benchmark forcing file."""
        return Path("test/benchmark1/forcing/Kc1_2011_data_5.txt")

    def test_init_from_yaml(self, benchmark_config):
        """Test initialization from YAML config."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")

        sim = SUEWSSimulation(benchmark_config)
        assert sim._config is not None
        assert sim._df_state_init is not None
        assert sim._df_state_init.shape[0] >= 1  # At least one grid
        assert isinstance(sim._df_state_init.columns, pd.MultiIndex)

    def test_setup_forcing(self, benchmark_config, benchmark_forcing):
        """Test forcing data setup."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.setup_forcing(benchmark_forcing)

        assert sim._df_forcing is not None
        assert len(sim._df_forcing) > 0
        assert isinstance(sim._df_forcing.index, pd.DatetimeIndex)

    def test_simulation_run(self, benchmark_config, benchmark_forcing):
        """Test complete simulation run."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.setup_forcing(benchmark_forcing)

        # Run short simulation
        start_date = pd.Timestamp("2011-01-01 00:05:00")
        end_date = pd.Timestamp("2011-01-01 01:00:00")

        results = sim.run(start_date=start_date, end_date=end_date)

        assert results is not None
        assert len(results) > 0
        assert isinstance(results.columns, pd.MultiIndex)
        assert "group" in results.columns.names
        assert "var" in results.columns.names

    def test_expected_output_variables(self, benchmark_config, benchmark_forcing):
        """Test that expected SUEWS output variables are present."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.setup_forcing(benchmark_forcing)

        start_date = pd.Timestamp("2011-01-01 00:05:00")
        end_date = pd.Timestamp("2011-01-01 01:00:00")

        results = sim.run(start_date=start_date, end_date=end_date)
        variables = results.columns.get_level_values("var")

        # Check for key SUEWS output variables
        expected_vars = ["QH", "QE", "QS"]  # Core energy balance components
        for var in expected_vars:
            assert var in variables, f"Expected variable {var} not found in results"

    def test_results_format(self, benchmark_config, benchmark_forcing):
        """Test that results are in expected format."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.setup_forcing(benchmark_forcing)

        start_date = pd.Timestamp("2011-01-01 00:05:00")
        end_date = pd.Timestamp("2011-01-01 02:00:00")  # 2 hours

        results = sim.run(start_date=start_date, end_date=end_date)

        # Check result structure - SuPy returns MultiIndex (grid, datetime)
        assert isinstance(results.index, pd.MultiIndex)
        assert "grid" in results.index.names
        assert "datetime" in results.index.names

        # Check datetime range
        datetime_values = results.index.get_level_values("datetime")
        assert datetime_values[0] >= start_date
        assert datetime_values[-1] <= end_date

        # Check for reasonable values (not all NaN or zero)
        # Energy fluxes should be in SUEWS group
        qh_values = results[("SUEWS", "QH")]
        assert not qh_values.isna().all().all(), "QH values are all NaN"
        assert (qh_values != 0).any().any(), "QH values are all zero"


class TestSUEWSSimulationError:
    """Test error handling."""

    def test_invalid_config_path(self):
        """Test initialization with invalid config path."""
        with pytest.raises(FileNotFoundError):
            SUEWSSimulation("nonexistent_config.yml")

    def test_run_without_forcing(self):
        """Test run without forcing data."""
        benchmark_config = Path("test/benchmark1/benchmark1.yml")
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")

        sim = SUEWSSimulation(benchmark_config)

        with pytest.raises(RuntimeError, match="validation failed"):
            sim.run()
