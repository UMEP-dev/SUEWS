"""
Simple, focused test suite for SUEWSSimulation class using real benchmark data.

Tests the core functionality without complex mocks or fixtures.
"""

import pytest
import pandas as pd
from pathlib import Path
import sys
import tempfile
import shutil

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from supy.suews_sim import SUEWSSimulation


class TestSUEWSSimulationBasic:
    """Test basic SUEWSSimulation functionality with real data."""

    @pytest.fixture
    def benchmark_config(self):
        """Path to benchmark configuration file."""
        return Path(__file__).parent / "benchmark1" / "benchmark1.yml"

    @pytest.fixture
    def benchmark_forcing(self):
        """Path to benchmark forcing file."""
        return Path(__file__).parent / "benchmark1" / "forcing" / "Kc1_2011_data_5.txt"

    def test_init_from_yaml(self, benchmark_config):
        """Test initialization from YAML config."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")

        sim = SUEWSSimulation(benchmark_config)
        assert sim.config is not None
        assert sim._df_state_init is not None
        assert sim._df_state_init.shape[0] >= 1  # At least one grid
        assert isinstance(sim._df_state_init.columns, pd.MultiIndex)

    def test_update_forcing(self, benchmark_config, benchmark_forcing):
        """Test forcing data update."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.update_forcing(benchmark_forcing)

        assert sim.forcing is not None
        assert len(sim.forcing) > 0
        assert isinstance(sim.forcing.index, pd.DatetimeIndex)

    def test_simulation_run(self, benchmark_config, benchmark_forcing):
        """Test complete simulation run."""
        if not benchmark_config.exists() or not benchmark_forcing.exists():
            pytest.skip("Benchmark files not found")

        sim = SUEWSSimulation(benchmark_config)
        sim.update_forcing(benchmark_forcing)

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
        sim.update_forcing(benchmark_forcing)

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
        sim.update_forcing(benchmark_forcing)

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
        benchmark_config = Path(__file__).parent / "benchmark1" / "benchmark1.yml"
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")

        sim = SUEWSSimulation(benchmark_config)

        with pytest.raises(RuntimeError, match="No forcing data loaded"):
            sim.run()


class TestSUEWSSimulationForcing:
    """Test forcing data loading scenarios."""

    @pytest.fixture
    def benchmark_config(self):
        """Path to benchmark configuration file."""
        return Path(__file__).parent / "benchmark1" / "benchmark1.yml"

    @pytest.fixture
    def forcing_dir(self):
        """Path to forcing directory."""
        return Path(__file__).parent / "benchmark1" / "forcing"

    def test_single_file_forcing(self, benchmark_config, forcing_dir):
        """Test loading a single forcing file."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")
        
        forcing_file = forcing_dir / "Kc1_2011_data_5.txt"
        sim = SUEWSSimulation(benchmark_config)
        sim.update_forcing(str(forcing_file))
        
        assert sim._df_forcing is not None
        assert len(sim._df_forcing) == 105120  # One year of 5-min data

    def test_list_of_files_forcing(self, benchmark_config, forcing_dir):
        """Test loading a list of forcing files."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")
        
        forcing_files = [
            str(forcing_dir / "Kc1_2011_data_5.txt"),
            str(forcing_dir / "Kc1_2012_data_5.txt")
        ]
        sim = SUEWSSimulation(benchmark_config)
        sim.update_forcing(forcing_files)
        
        assert sim._df_forcing is not None
        # Two years of 5-min data (2012 is leap year)
        assert len(sim._df_forcing) == 105120 + 105408

    def test_directory_forcing_with_warning(self, benchmark_config, forcing_dir):
        """Test loading from directory issues deprecation warning."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")
        
        import warnings
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            sim = SUEWSSimulation(benchmark_config)
            sim.update_forcing(str(forcing_dir))
            
            # Check deprecation warning was issued
            assert any(issubclass(warning.category, DeprecationWarning) for warning in w)
            assert "deprecated" in str(w[-1].message).lower()
        
        assert sim._df_forcing is not None
        # Should load all 3 years
        assert len(sim._df_forcing) == 315648

    def test_mixed_directory_and_files_rejected(self, benchmark_config, forcing_dir):
        """Test that mixed directories and files are rejected."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")
        
        mixed_list = [
            str(forcing_dir),  # Directory
            str(forcing_dir / "Kc1_2011_data_5.txt")  # File
        ]
        
        sim = SUEWSSimulation(benchmark_config)
        with pytest.raises(ValueError, match="Directory.*not allowed in lists"):
            sim.update_forcing(mixed_list)

    def test_nonexistent_file_rejected(self, benchmark_config):
        """Test that nonexistent files are rejected."""
        if not benchmark_config.exists():
            pytest.skip("Benchmark config file not found")
        
        sim = SUEWSSimulation(benchmark_config)
        with pytest.raises(FileNotFoundError):
            sim.update_forcing("nonexistent.txt")

    def test_forcing_fallback_from_config(self, forcing_dir):
        """Test that forcing is loaded from config when not explicitly provided."""
        # Use the real benchmark config which has forcing_file: value: forcing/
        config_path = Path(__file__).parent / "benchmark1" / "benchmark1.yml"
        if not config_path.exists():
            pytest.skip("Benchmark config file not found")
        
        # Don't provide forcing_file parameter
        import warnings
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            sim = SUEWSSimulation(config_path)
        
        # Should load from config (which points to directory)
        assert sim._df_forcing is not None
        # Should have issued deprecation warning for directory
        assert any(issubclass(warning.category, DeprecationWarning) for warning in w)


class TestSUEWSSimulationOutputFormats:
    """Test output format functionality including OutputConfig integration."""

    @pytest.fixture
    def sim_with_results(self):
        """Create a simulation with results ready to save."""
        config_path = Path(__file__).parent / "benchmark1" / "benchmark1.yml"
        forcing_path = Path(__file__).parent / "benchmark1" / "forcing" / "Kc1_2011_data_5.txt"
        
        if not config_path.exists() or not forcing_path.exists():
            pytest.skip("Benchmark files not found")
        
        sim = SUEWSSimulation(config_path)
        sim.update_forcing(forcing_path)
        
        # Run short simulation
        start_date = pd.Timestamp("2011-01-01 00:05:00")
        end_date = pd.Timestamp("2011-01-01 02:00:00")
        sim.run(start_date=start_date, end_date=end_date)
        
        return sim
    
    def test_save_parquet_format(self, sim_with_results):
        """Test saving results in Parquet format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results.parquet"
            saved_path = sim_with_results.save(output_path, format="parquet")
            
            assert saved_path.exists()
            assert saved_path.suffix == ".parquet"
            
            # Verify content
            df = pd.read_parquet(saved_path)
            assert len(df) > 0
    
    def test_save_txt_format(self, sim_with_results):
        """Test saving results in legacy TXT format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "txt_output"
            saved_paths = sim_with_results.save(output_dir, format="txt")
            
            # save_supy returns a list of paths
            assert isinstance(saved_paths, list)
            assert len(saved_paths) > 0
            
            # Check that output directory exists
            assert output_dir.exists()
            assert output_dir.is_dir()
            
            # Check for output files (save_supy creates files with site code and timestamp)
            txt_files = list(output_dir.glob("*.txt"))
            assert len(txt_files) >= 1  # At least one output file
            
            # Check that some files were created
            csv_files = list(output_dir.glob("*.csv"))  # State files are CSV
            all_files = txt_files + csv_files
            assert len(all_files) >= 2  # Output + state files
    
    def test_save_default_format(self, sim_with_results):
        """Test that default format is parquet when not specified."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results_default.parquet"
            saved_path = sim_with_results.save(output_path)  # No format specified
            
            assert saved_path.exists()
            # Should default to parquet
            df = pd.read_parquet(saved_path)
            assert len(df) > 0
    
    def test_invalid_format_rejected(self, sim_with_results):
        """Test that invalid formats are rejected."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results.csv"
            
            # Test various invalid formats
            for invalid_format in ["csv", "excel", "pickle", "netcdf", "json"]:
                with pytest.raises(ValueError, match="Unsupported format"):
                    sim_with_results.save(output_path, format=invalid_format)
