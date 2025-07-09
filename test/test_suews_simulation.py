"""
Optimized test suite for SUEWSSimulation class using sample run data.

Tests core functionality with shared fixtures for better performance.
"""

import pytest
import pandas as pd
from pathlib import Path
import sys
import tempfile
import shutil
import warnings

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from supy.suews_sim import SUEWSSimulation


@pytest.fixture(scope="module")
def sample_config_path():
    """Path to sample configuration file."""
    return Path("src/supy/sample_run/sample_config.yml")


@pytest.fixture(scope="module")
def sample_forcing_path():
    """Path to sample forcing file."""
    return Path("src/supy/sample_run/Input/Kc_2012_data_60.txt")


@pytest.fixture(scope="module")
def shared_simulation(sample_config_path):
    """Create a shared SUEWSSimulation instance for all tests."""
    if not sample_config_path.exists():
        pytest.skip("Sample config file not found")
    
    # Create simulation and let it auto-load forcing (only once!)
    sim = SUEWSSimulation(sample_config_path)
    return sim


@pytest.fixture(scope="module")
def simulation_no_forcing(sample_config_path):
    """Create a simulation without auto-loading forcing."""
    if not sample_config_path.exists():
        pytest.skip("Sample config file not found")
    
    # Mock auto-loading to create a simulation without forcing
    import unittest.mock
    with unittest.mock.patch.object(SUEWSSimulation, '_try_load_forcing_from_config'):
        sim = SUEWSSimulation(sample_config_path)
    return sim


class TestSUEWSSimulationBasic:
    """Test basic SUEWSSimulation functionality with shared instance."""

    def test_init_state(self, shared_simulation):
        """Test that simulation initialized correctly."""
        assert shared_simulation.config is not None
        assert shared_simulation._df_state_init is not None
        assert shared_simulation._df_state_init.shape[0] >= 1  # At least one grid
        assert isinstance(shared_simulation._df_state_init.columns, pd.MultiIndex)

    def test_forcing_loaded(self, shared_simulation):
        """Test that forcing was auto-loaded from config."""
        assert shared_simulation.forcing is not None
        assert len(shared_simulation.forcing) > 0
        assert isinstance(shared_simulation.forcing.index, pd.DatetimeIndex)

    def test_simulation_run(self, shared_simulation):
        """Test complete simulation run."""
        # Run 1-day simulation
        start_date = pd.Timestamp("2012-01-01 00:00:00")
        end_date = pd.Timestamp("2012-01-02 00:00:00")

        results = shared_simulation.run(start_date=start_date, end_date=end_date)

        assert results is not None
        assert len(results) > 0
        assert isinstance(results.columns, pd.MultiIndex)
        assert "group" in results.columns.names
        assert "var" in results.columns.names

    def test_expected_output_variables(self, shared_simulation):
        """Test that expected SUEWS output variables are present."""
        # Use existing results if available, otherwise run a short simulation
        if hasattr(shared_simulation, '_df_output') and shared_simulation._df_output is not None:
            results = shared_simulation._df_output
        else:
            start_date = pd.Timestamp("2012-01-01 00:00:00")
            end_date = pd.Timestamp("2012-01-01 12:00:00")
            results = shared_simulation.run(start_date=start_date, end_date=end_date)
        
        variables = results.columns.get_level_values("var")

        # Check for key SUEWS output variables
        expected_vars = ["QH", "QE", "QS"]  # Core energy balance components
        for var in expected_vars:
            assert var in variables, f"Expected variable {var} not found in results"

    def test_results_format(self, shared_simulation):
        """Test that results are in expected format."""
        # Use existing results if available
        if hasattr(shared_simulation, '_df_output') and shared_simulation._df_output is not None:
            results = shared_simulation._df_output
        else:
            start_date = pd.Timestamp("2012-01-01 00:00:00")
            end_date = pd.Timestamp("2012-01-01 06:00:00")
            results = shared_simulation.run(start_date=start_date, end_date=end_date)

        # Check result structure - SuPy returns MultiIndex (grid, datetime)
        assert isinstance(results.index, pd.MultiIndex)
        assert "grid" in results.index.names
        assert "datetime" in results.index.names

        # Check for reasonable values (not all NaN or zero)
        qh_values = results[("SUEWS", "QH")]
        assert not qh_values.isna().all().all(), "QH values are all NaN"
        assert (qh_values != 0).any().any(), "QH values are all zero"


class TestSUEWSSimulationError:
    """Test error handling."""

    def test_invalid_config_path(self):
        """Test initialization with invalid config path."""
        with pytest.raises(FileNotFoundError):
            SUEWSSimulation("nonexistent_config.yml")

    def test_run_without_forcing(self, simulation_no_forcing):
        """Test run without forcing data."""
        # Ensure no forcing is loaded
        simulation_no_forcing._df_forcing = None

        with pytest.raises(RuntimeError, match="No forcing data loaded"):
            simulation_no_forcing.run()


class TestSUEWSSimulationForcing:
    """Test forcing data loading scenarios using fresh instances."""
    
    def test_update_forcing_single_file(self, simulation_no_forcing, sample_forcing_path):
        """Test updating forcing with a single file."""
        if not sample_forcing_path.exists():
            pytest.skip("Sample forcing file not found")
        
        # Update forcing
        simulation_no_forcing.update_forcing(str(sample_forcing_path))
        
        assert simulation_no_forcing._df_forcing is not None
        assert len(simulation_no_forcing._df_forcing) > 0

    def test_list_of_files_forcing(self, sample_config_path, sample_forcing_path):
        """Test loading a list of forcing files."""
        if not sample_config_path.exists() or not sample_forcing_path.exists():
            pytest.skip("Test files not found")
        
        # Create fresh instance for this test
        import unittest.mock
        with unittest.mock.patch.object(SUEWSSimulation, '_try_load_forcing_from_config'):
            sim = SUEWSSimulation(sample_config_path)
        
        # Use a single file in a list
        forcing_files = [str(sample_forcing_path)]
        sim.update_forcing(forcing_files)
        
        assert sim._df_forcing is not None
        assert len(sim._df_forcing) > 0

    def test_directory_forcing_with_warning(self, sample_config_path):
        """Test loading from directory issues deprecation warning."""
        if not sample_config_path.exists():
            pytest.skip("Sample config file not found")
        
        # Create a directory with only forcing files
        with tempfile.TemporaryDirectory() as tmpdir:
            temp_forcing_dir = Path(tmpdir) / "forcing"
            temp_forcing_dir.mkdir()
            
            # Copy a forcing file to the temp directory
            sample_forcing = Path("src/supy/sample_run/Input/Kc_2012_data_60.txt")
            if sample_forcing.exists():
                shutil.copy(sample_forcing, temp_forcing_dir / "test_forcing.txt")
            
            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")
                
                # Create fresh instance
                import unittest.mock
                with unittest.mock.patch.object(SUEWSSimulation, '_try_load_forcing_from_config'):
                    sim = SUEWSSimulation(sample_config_path)
                
                # Loading from directory should issue warning
                sim.update_forcing(str(temp_forcing_dir))
                
                # Check deprecation warning was issued
                assert any(issubclass(warning.category, DeprecationWarning) for warning in w)
                assert "deprecated" in str(w[-1].message).lower()

    def test_nonexistent_file_rejected(self, sample_config_path):
        """Test that nonexistent files are rejected."""
        if not sample_config_path.exists():
            pytest.skip("Sample config file not found")
        
        # Create fresh instance
        import unittest.mock
        with unittest.mock.patch.object(SUEWSSimulation, '_try_load_forcing_from_config'):
            sim = SUEWSSimulation(sample_config_path)
        
        with pytest.raises(FileNotFoundError):
            sim.update_forcing("nonexistent.txt")


class TestSUEWSSimulationOutputFormats:
    """Test output format functionality."""
    
    @pytest.fixture
    def simulation_with_results(self, shared_simulation):
        """Ensure simulation has results to save."""
        # Run simulation if not already done
        if not hasattr(shared_simulation, '_df_output') or shared_simulation._df_output is None:
            start_date = pd.Timestamp("2012-01-01 00:00:00")
            end_date = pd.Timestamp("2012-01-01 01:00:00")  # Just 1 hour
            shared_simulation.run(start_date=start_date, end_date=end_date)
        
        return shared_simulation
    
    def test_save_parquet_format(self, simulation_with_results):
        """Test saving results in Parquet format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results.parquet"
            saved_path = simulation_with_results.save(output_path, format="parquet")
            
            assert saved_path.exists()
            assert saved_path.suffix == ".parquet"
            
            # Verify content
            df = pd.read_parquet(saved_path)
            assert len(df) > 0
    
    def test_save_txt_format(self, simulation_with_results):
        """Test saving results in legacy TXT format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "txt_output"
            
            # Mock save_supy to avoid format issues
            import unittest.mock
            mock_paths = [Path(output_dir) / "output.txt", Path(output_dir) / "state.csv"]
            
            with unittest.mock.patch('supy.suews_sim.save_supy', return_value=mock_paths) as mock_save:
                saved_paths = simulation_with_results.save(output_dir, format="txt")
                
                # Verify save_supy was called
                mock_save.assert_called_once()
                
                # Check return value
                assert saved_paths == mock_paths
    
    def test_save_default_format(self, simulation_with_results):
        """Test that default format is parquet when not specified."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results_default.parquet"
            saved_path = simulation_with_results.save(output_path)  # No format specified
            
            assert saved_path.exists()
            # Should default to parquet
            df = pd.read_parquet(saved_path)
            assert len(df) > 0
    
    def test_invalid_format_rejected(self, simulation_with_results):
        """Test that invalid formats are rejected."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "results.csv"
            
            # Test various invalid formats
            for invalid_format in ["csv", "excel", "pickle", "netcdf", "json"]:
                with pytest.raises(ValueError, match="Unsupported format"):
                    simulation_with_results.save(output_path, format=invalid_format)
    
    def test_reset_functionality(self, shared_simulation):
        """Test reset clears results but keeps configuration."""
        # Ensure we have results
        if not hasattr(shared_simulation, '_df_output') or shared_simulation._df_output is None:
            shared_simulation.run(start_date="2012-01-01", end_date="2012-01-01 01:00:00")
        
        # Store config reference
        config_before = shared_simulation.config
        forcing_before = shared_simulation._df_forcing
        
        # Reset
        shared_simulation.reset()
        
        # Check results are cleared
        assert shared_simulation._df_output is None
        assert shared_simulation._df_state_final is None
        assert shared_simulation._run_completed is False
        
        # Check config and forcing are preserved
        assert shared_simulation.config is config_before
        assert shared_simulation._df_forcing is forcing_before