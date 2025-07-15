"""
Simple, focused test suite for SUEWSSimulation class using sample data.

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
import supy


class TestSUEWSSimulationBasic:
    """Test basic SUEWSSimulation functionality with sample data."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data and create minimal forcing for fast tests."""
        df_state_init, df_forcing = supy.load_sample_data()
        # Use only 2 days of data for faster tests
        df_forcing_short = df_forcing.loc["2012-01-01":"2012-01-02"]
        return df_state_init, df_forcing_short
    
    @pytest.fixture
    def sample_config_path(self):
        """Path to sample configuration file."""
        import supy
        return Path(supy.__file__).parent / "sample_run" / "sample_config.yml"

    def test_init_from_yaml(self, sample_config_path):
        """Test initialization from YAML config."""
        sim = SUEWSSimulation(sample_config_path)
        assert sim.config is not None
        assert sim._df_state_init is not None
        assert sim._df_state_init.shape[0] >= 1  # At least one grid
        assert isinstance(sim._df_state_init.columns, pd.MultiIndex)

    def test_update_forcing(self, sample_config_path, sample_data):
        """Test forcing data update."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)

        assert sim.forcing is not None
        assert len(sim.forcing) == len(df_forcing)
        assert isinstance(sim.forcing.index, pd.DatetimeIndex)

    def test_simulation_run(self, sample_config_path, sample_data):
        """Test complete simulation run."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)

        results = sim.run()

        assert results is not None
        assert len(results) > 0
        assert isinstance(results.columns, pd.MultiIndex)
        assert "group" in results.columns.names
        assert "var" in results.columns.names

    def test_expected_output_variables(self, sample_config_path, sample_data):
        """Test that expected SUEWS output variables are present."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)

        results = sim.run()
        variables = results.columns.get_level_values("var")

        # Check for key SUEWS output variables
        expected_vars = ["QH", "QE", "QS"]  # Core energy balance components
        for var in expected_vars:
            assert var in variables, f"Expected variable {var} not found in results"

    def test_results_format(self, sample_config_path, sample_data):
        """Test that results are in expected format."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)

        results = sim.run()

        # Check result structure - SuPy returns MultiIndex (grid, datetime)
        assert isinstance(results.index, pd.MultiIndex)
        assert "grid" in results.index.names
        assert "datetime" in results.index.names

        # Check datetime range
        datetime_values = results.index.get_level_values("datetime")
        assert datetime_values[0] >= df_forcing.index[0]
        assert datetime_values[-1] <= df_forcing.index[-1]

        # Check for reasonable values (not all NaN or zero)
        # Energy fluxes should be in SUEWS group
        qh_values = results[("SUEWS", "QH")]
        assert not qh_values.isna().all().all(), "QH values are all NaN"
        assert (qh_values != 0).any().any(), "QH values are all zero"


class TestSUEWSSimulationError:
    """Test error handling."""

    @pytest.fixture
    def sample_config_path(self):
        """Path to sample configuration file."""
        import supy
        return Path(supy.__file__).parent / "sample_run" / "sample_config.yml"

    def test_invalid_config_path(self):
        """Test initialization with invalid config path."""
        with pytest.raises(FileNotFoundError):
            SUEWSSimulation("nonexistent_config.yml")

    def test_run_without_forcing(self, sample_config_path):
        """Test run without forcing data."""
        sim = SUEWSSimulation(sample_config_path)
        # Clear the forcing that was auto-loaded from config
        sim._df_forcing = None

        with pytest.raises(RuntimeError, match="No forcing data loaded"):
            sim.run()


class TestSUEWSSimulationForcing:
    """Test forcing data handling."""

    @pytest.fixture
    def sample_config_path(self):
        """Path to sample configuration file."""
        import supy
        return Path(supy.__file__).parent / "sample_run" / "sample_config.yml"
    
    @pytest.fixture
    def sample_data(self):
        """Load sample data and create minimal forcing for fast tests."""
        df_state_init, df_forcing = supy.load_sample_data()
        # Use only 2 days of data for faster tests
        df_forcing_short = df_forcing.loc["2012-01-01":"2012-01-02"]
        return df_state_init, df_forcing_short

    @pytest.fixture
    def sample_forcing_file(self, tmp_path):
        """Create a temporary forcing file for testing."""
        # Load sample data and save to file
        _, df_forcing = supy.load_sample_data()
        # Use only 1 day for test file
        df_forcing_day = df_forcing.loc["2012-01-01":"2012-01-01"]
        
        forcing_file = tmp_path / "test_forcing.txt"
        # Save just the forcing data as a simple CSV
        df_forcing_day.to_csv(forcing_file, sep='\t')
        return forcing_file

    def test_single_file_forcing(self, sample_config_path, tmp_path):
        """Test loading a single forcing file."""
        # Use the actual sample forcing file that comes with supy
        import supy
        sample_forcing = Path(supy.__file__).parent / "sample_run" / "Input" / "Kc_2012_data_60.txt"
        
        if sample_forcing.exists():
            sim = SUEWSSimulation(sample_config_path)
            sim.update_forcing(str(sample_forcing))
            
            assert sim._df_forcing is not None
            # Should have loaded data
            assert len(sim._df_forcing) > 0
        else:
            pytest.skip("Sample forcing file not found")

    def test_dataframe_forcing(self, sample_config_path, sample_data):
        """Test loading forcing from DataFrame."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)
        
        assert sim._df_forcing is not None
        assert len(sim._df_forcing) == len(df_forcing)

    def test_nonexistent_file_rejected(self, sample_config_path):
        """Test that nonexistent files are rejected."""
        sim = SUEWSSimulation(sample_config_path)
        with pytest.raises(FileNotFoundError):
            sim.update_forcing("nonexistent.txt")


class TestSUEWSSimulationOutputFormats:
    """Test output format functionality including OutputConfig integration."""

    @pytest.fixture
    def sample_config_path(self):
        """Path to sample configuration file."""
        import supy
        return Path(supy.__file__).parent / "sample_run" / "sample_config.yml"
    
    @pytest.fixture
    def sample_data(self):
        """Load sample data and create minimal forcing for fast tests."""
        df_state_init, df_forcing = supy.load_sample_data()
        # Use only 2 days of data for faster tests
        df_forcing_short = df_forcing.loc["2012-01-01":"2012-01-02"]
        return df_state_init, df_forcing_short

    @pytest.fixture
    def sim_with_results(self, sample_config_path, sample_data):
        """Create a simulation with results ready to save."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)
        sim.run()
        
        return sim
    
    def test_save_parquet_format(self, sim_with_results):
        """Test saving results in Parquet format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results.parquet"
            # Save as parquet directly
            sim_with_results._df_output.to_parquet(output_path)
            
            assert output_path.exists()
            assert output_path.suffix == ".parquet"
            
            # Verify we can read the file
            df_loaded = pd.read_parquet(output_path)
            assert len(df_loaded) > 0
    
    def test_save_txt_format(self, sim_with_results):
        """Test saving results in text format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir)
            saved_paths = sim_with_results.save(str(output_path))
            
            # Should return list of paths
            assert isinstance(saved_paths, list)
            assert len(saved_paths) > 0
            
            # Check files exist  
            for path in saved_paths:
                assert path.exists()
                assert path.suffix in [".txt", ".csv"]  # Could be either
    
    def test_save_respects_output_config(self, sample_config_path, sample_data):
        """Test that save respects OutputConfig settings."""
        _, df_forcing = sample_data
        
        sim = SUEWSSimulation(sample_config_path)
        sim.update_forcing(df_forcing)
        sim.run()
        
        # The sample config specifies txt format and SUEWS group
        with tempfile.TemporaryDirectory() as tmpdir:
            saved_paths = sim.save(tmpdir)
            
            # Should save as txt (from config)
            assert all(p.suffix in [".txt", ".csv"] for p in saved_paths if isinstance(p, Path))
            
            # Should have SUEWS output file
            suews_files = [p for p in saved_paths if isinstance(p, Path) and "SUEWS" in p.name]
            assert len(suews_files) > 0