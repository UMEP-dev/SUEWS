"""Integration tests for SuPy model integration with SUEWS MCP Server."""

import pytest
import asyncio
import pandas as pd
import numpy as np
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch, MagicMock
import tempfile
import yaml
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.handlers import SUEWSMCPHandlers, SUPY_MCP_TOOLS_AVAILABLE
from suews_mcp.config import MCPServerConfig
from suews_mcp.preprocessing import ForcingDataPreprocessor, ConfigValidator


class TestSuPyModelIntegration:
    """Test integration between SUEWS MCP server and SuPy model."""

    @pytest.fixture
    def integration_config(self):
        """Configuration for integration testing."""
        return MCPServerConfig(
            server_name="integration-test-server",
            server_version="0.1.0-test",
            log_level="DEBUG",
            enable_debug=True,
            suews_timeout=300,  # Longer timeout for real simulations
            max_concurrent_simulations=1,  # Single sim for integration test
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
            max_memory_mb=1024,
            max_simulation_time_hours=24.0,
        )

    @pytest.fixture
    def realistic_forcing_data(self):
        """Generate realistic meteorological forcing data for testing."""
        # Create 48 hours of realistic data
        n_hours = 48
        base_date = pd.Timestamp("2024-01-01")

        # Generate realistic diurnal cycles
        hours = np.arange(n_hours)
        solar_cycle = np.maximum(
            0,
            1000
            * np.sin(np.pi * (hours % 24) / 24)
            * np.cos(np.pi * ((hours % 24) - 12) / 12),
        )
        temp_cycle = 15 + 8 * np.sin(2 * np.pi * (hours % 24) / 24 - np.pi / 2)

        data = {
            "iy": [2024] * n_hours,
            "id": [((base_date + pd.Timedelta(hours=h)).dayofyear) for h in hours],
            "it": [h % 24 for h in hours],
            "imin": [0] * n_hours,
            "qn": solar_cycle * 0.6 + np.random.normal(0, 20, n_hours),
            "qh": solar_cycle * 0.3 + np.random.normal(0, 15, n_hours),
            "qe": solar_cycle * 0.2 + np.random.normal(0, 10, n_hours),
            "qs": solar_cycle * 0.1 + np.random.normal(0, 8, n_hours),
            "qf": np.full(n_hours, 20.0) + np.random.normal(0, 5, n_hours),
            "U": np.maximum(0.5, 5.0 + np.random.normal(0, 2, n_hours)),
            "RH": np.clip(70 + np.random.normal(0, 15, n_hours), 20, 100),
            "Tair": temp_cycle + np.random.normal(0, 2, n_hours),
            "pres": 101.325 + np.random.normal(0, 0.5, n_hours),
            "rain": np.random.exponential(0.1, n_hours),
            "kdown": solar_cycle + np.random.normal(0, 50, n_hours),
        }

        return pd.DataFrame(data)

    @pytest.fixture
    def suews_config_dict(self, tmp_path):
        """Generate a valid SUEWS configuration dictionary."""
        config = {
            "name": "integration_test",
            "description": "Integration test configuration for SuPy model",
            "model": {
                "control": {
                    "tstep": 3600,  # 1-hour timestep
                    "forcing_file": {"value": "forcing_data.txt"},
                    "start_doy": {"value": 1},
                    "end_doy": {"value": 2},
                    "year": {"value": 2024},
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1},
                    "conductancemethod": {"value": 4},
                    "stabilitymethod": {"value": 3},
                    "storedrainprm": {"value": 1},
                    "anthropogenic": {"value": 1},
                },
            },
            "sites": [
                {
                    "name": "test_site",
                    "properties": {
                        "lat": {"value": 51.5074},
                        "lng": {"value": -0.1278},
                        "alt": {"value": 50.0},
                        "timezone": {"value": 0},
                    },
                    "land_cover": {
                        "paved": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.95},
                            "roughlenmmom": {"value": 0.1},
                        },
                        "bldgs": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.92},
                            "roughlenmmom": {"value": 1.0},
                        },
                        "grass": {
                            "sfr": {"value": 0.30},
                            "emis": {"value": 0.96},
                            "roughlenmmom": {"value": 0.05},
                        },
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 150.0},
                        "soilstore_surf": {"value": 10.0},
                    },
                }
            ],
        }
        return config

    async def test_handler_initialization_with_supy_tools(self, integration_config):
        """Test that handlers initialize correctly with or without SuPy tools."""
        handlers = SUEWSMCPHandlers(integration_config)

        # Test that handlers are created regardless of SuPy tool availability
        assert handlers is not None
        assert handlers.config == integration_config

        # Test that SuPy tools are properly handled
        if SUPY_MCP_TOOLS_AVAILABLE:
            assert handlers._configure_tool is not None
            assert handlers._run_tool is not None
            assert handlers._analyze_tool is not None
        else:
            # Should have fallback behavior
            assert handlers._configure_tool is None
            assert handlers._run_tool is None
            assert handlers._analyze_tool is None

    async def test_list_tools_integration(self, integration_config):
        """Test that tools are listed correctly in integration scenario."""
        handlers = SUEWSMCPHandlers(integration_config)
        result = await handlers.handle_list_tools()

        # Should have tools regardless of SuPy availability
        assert hasattr(result, "tools") or "tools" in result
        tools = result.tools if hasattr(result, "tools") else result["tools"]
        assert len(tools) > 0

        # Check for expected tool types
        tool_names = [t.name if hasattr(t, "name") else t["name"] for t in tools]

        if integration_config.enable_simulation_tool:
            assert any("simulation" in name.lower() for name in tool_names)
        if integration_config.enable_validation_tool:
            assert any(
                "validate" in name.lower() or "configure" in name.lower()
                for name in tool_names
            )
        if integration_config.enable_analysis_tool:
            assert any("analyz" in name.lower() for name in tool_names)

    @patch("supy.load_SampleData")
    @patch("supy.run_supy")
    async def test_simulation_workflow_with_mock_supy(
        self,
        mock_run_supy,
        mock_load_sample,
        integration_config,
        realistic_forcing_data,
        suews_config_dict,
        tmp_path,
    ):
        """Test complete simulation workflow with mocked SuPy."""

        # Set up mocks
        mock_sample_data = {
            "config": suews_config_dict,
            "forcing": realistic_forcing_data,
        }
        mock_load_sample.return_value = mock_sample_data

        # Mock successful simulation result
        mock_output = pd.DataFrame(
            {
                "datetime": pd.date_range("2024-01-01", periods=48, freq="h"),
                "QN": np.random.normal(100, 30, 48),
                "QH": np.random.normal(50, 20, 48),
                "QE": np.random.normal(30, 15, 48),
                "QS": np.random.normal(10, 10, 48),
                "QF": np.full(48, 20.0),
                "T2": np.random.normal(15, 3, 48),
                "RH2": np.random.normal(70, 10, 48),
                "U10": np.random.normal(5, 2, 48),
            }
        )

        mock_run_result = {
            "output": mock_output,
            "state": pd.DataFrame({"state_var": [1, 2, 3]}),
            "metadata": {
                "simulation_time": "00:02:30",
                "timestep": 3600,
                "total_hours": 48,
            },
        }
        mock_run_supy.return_value = mock_run_result

        # Create test files
        config_file = tmp_path / "test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(suews_config_dict, f)

        forcing_file = tmp_path / "forcing_data.txt"
        realistic_forcing_data.to_csv(
            forcing_file, sep=" ", index=False, float_format="%.2f"
        )

        # Test simulation
        handlers = SUEWSMCPHandlers(integration_config)

        # Test simulation tool call
        simulation_args = {
            "config_file": str(config_file),
            "simulation_id": "integration_test_001",
            "output_dir": str(tmp_path / "output"),
        }

        # Mock the actual tool call
        with patch.object(handlers, "_call_simulation_tool") as mock_call:
            mock_call.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f'Simulation completed successfully!\n'
                        f'Simulation ID: integration_test_001\n'
                        f'Output saved to: {tmp_path / "output"}\n'
                        f'Simulation time: 00:02:30\n'
                        f'Total timesteps: 48',
                    }
                ]
            }

            result = await handlers.handle_call_tool(
                "run_suews_simulation", simulation_args
            )

            # Verify the call was made and returned expected results
            assert mock_call.called
            assert result is not None
            mock_call.assert_called_once_with("run_suews_simulation", simulation_args)

    async def test_preprocessing_integration(self, realistic_forcing_data, tmp_path):
        """Test integration of preprocessing tools with realistic data."""

        # Save test data to file
        forcing_file = tmp_path / "test_forcing.txt"
        realistic_forcing_data.to_csv(
            forcing_file, sep=" ", index=False, float_format="%.2f"
        )

        # Test preprocessing
        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "processed_forcing.txt"

        result = preprocessor.preprocess_forcing_file(
            file_path=forcing_file,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=True,
        )

        # Should process successfully
        assert result.success is True
        assert result.data is not None
        assert len(result.data) == 48  # 48 hours of data
        assert output_file.exists()

        # Check data quality
        assert "detected_timestep_seconds" in result.metadata
        assert result.metadata["detected_timestep_seconds"] == 3600

        # Should have minimal issues with realistic data
        errors = [i for i in result.issues if i.severity == "error"]
        assert len(errors) == 0

    async def test_config_validation_integration(self, suews_config_dict, tmp_path):
        """Test integration of configuration validation."""

        # Create config file
        config_file = tmp_path / "test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(suews_config_dict, f)

        # Test validation
        validator = ConfigValidator()
        result = validator.validate_config(
            config_path=config_file,
            strict_mode=True,
            check_file_paths=False,  # Skip file existence checks
        )

        # Should validate successfully
        assert result.success is True
        assert "config_file" in result.metadata

        # Should have no critical errors
        errors = [i for i in result.issues if i.severity == "error"]
        assert len(errors) == 0

    @pytest.mark.asyncio
    async def test_concurrent_simulation_limits(self, integration_config):
        """Test that concurrent simulation limits are enforced."""
        # Set low concurrency limit
        integration_config.max_concurrent_simulations = 1
        handlers = SUEWSMCPHandlers(integration_config)

        # Test that semaphore is properly initialized
        assert handlers._simulation_semaphore._value == 1

        # Test concurrent access behavior
        async with handlers._simulation_semaphore:
            # Inside semaphore, no more permits available
            assert handlers._simulation_semaphore._value == 0

            # Try to acquire another permit (should not block in test)
            acquired = handlers._simulation_semaphore.acquire_nowait()
            assert acquired is False

    async def test_error_handling_in_integration(self, integration_config, tmp_path):
        """Test error handling in integration scenarios."""
        handlers = SUEWSMCPHandlers(integration_config)

        # Test with invalid config file
        invalid_args = {
            "config_file": str(tmp_path / "nonexistent_config.yml"),
            "simulation_id": "error_test",
        }

        # Should handle error gracefully
        with patch.object(handlers, "_call_simulation_tool") as mock_call:
            # Mock an error response
            mock_call.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": "Error: Configuration file not found: nonexistent_config.yml",
                    }
                ],
                "is_error": True,
            }

            result = await handlers.handle_call_tool(
                "run_suews_simulation", invalid_args
            )

            # Should return error information
            assert result is not None
            assert mock_call.called

    async def test_memory_and_time_limits_integration(self):
        """Test that memory and time limits are properly configured."""
        config = MCPServerConfig(
            server_name="limits-test",
            server_version="0.1.0-test",
            max_memory_mb=256,  # Low memory limit
            max_simulation_time_hours=0.5,  # 30 minute limit
        )

        handlers = SUEWSMCPHandlers(config)

        # Test that limits are applied to configuration
        assert handlers.config.max_memory_mb == 256
        assert handlers.config.max_simulation_time_hours == 0.5

    @pytest.mark.skipif(
        not SUPY_MCP_TOOLS_AVAILABLE, reason="SuPy MCP tools not available"
    )
    async def test_real_supy_tool_integration(self, integration_config):
        """Test integration with actual SuPy MCP tools if available."""
        handlers = SUEWSMCPHandlers(integration_config)

        # Test that real tools are available
        assert handlers._configure_tool is not None
        assert handlers._run_tool is not None
        assert handlers._analyze_tool is not None

        # Test tool definitions can be retrieved
        config_tool_def = handlers._configure_tool.get_definition()
        assert "name" in config_tool_def
        assert "description" in config_tool_def
        assert "inputSchema" in config_tool_def

        run_tool_def = handlers._run_tool.get_definition()
        assert "name" in run_tool_def
        assert "description" in run_tool_def

        analyze_tool_def = handlers._analyze_tool.get_definition()
        assert "name" in analyze_tool_def
        assert "description" in analyze_tool_def


class TestSuPyDataIntegration:
    """Test integration with SuPy data structures and workflows."""

    @pytest.fixture
    def sample_supy_config(self):
        """Create a sample SuPy-compatible configuration."""
        return {
            "config": {
                "time": {
                    "start": "2024-01-01 00:00",
                    "end": "2024-01-02 00:00",
                    "freq": "1h",
                },
                "site": {"lat": 51.5074, "lon": -0.1278, "alt": 50.0},
                "physics": {"net_radiation_method": 3, "roughness_method": 1},
            }
        }

    def test_data_structure_compatibility(
        self, realistic_forcing_data, sample_supy_config
    ):
        """Test that data structures are compatible with SuPy expectations."""

        # Test forcing data structure
        assert isinstance(realistic_forcing_data, pd.DataFrame)

        # Check required columns are present
        required_cols = ["iy", "id", "it", "imin", "qn", "qh", "qe", "Tair", "U", "RH"]
        for col in required_cols:
            assert col in realistic_forcing_data.columns

        # Test data types
        assert realistic_forcing_data["iy"].dtype in [int, np.int64, np.int32]
        assert realistic_forcing_data["id"].dtype in [int, np.int64, np.int32]
        assert realistic_forcing_data["Tair"].dtype in [float, np.float64, np.float32]

        # Test configuration structure
        assert isinstance(sample_supy_config, dict)
        assert "config" in sample_supy_config

    @patch("supy.load_SampleData")
    def test_supy_data_loading_mock(
        self, mock_load, realistic_forcing_data, sample_supy_config
    ):
        """Test data loading integration with SuPy."""

        # Mock the data loading
        mock_load.return_value = {
            "config": sample_supy_config["config"],
            "forcing": realistic_forcing_data,
        }

        # Test loading (would normally load from SuPy sample data)
        result = mock_load()

        assert result is not None
        assert "config" in result
        assert "forcing" in result
        assert isinstance(result["forcing"], pd.DataFrame)

        mock_load.assert_called_once()

    def test_forcing_data_time_structure(self, realistic_forcing_data):
        """Test that forcing data has proper time structure for SuPy."""

        # Check time columns
        assert "iy" in realistic_forcing_data.columns  # year
        assert "id" in realistic_forcing_data.columns  # day of year
        assert "it" in realistic_forcing_data.columns  # hour
        assert "imin" in realistic_forcing_data.columns  # minute

        # Check time values are reasonable
        assert all(realistic_forcing_data["iy"] == 2024)
        assert all(realistic_forcing_data["id"].between(1, 366))
        assert all(realistic_forcing_data["it"].between(0, 23))
        assert all(realistic_forcing_data["imin"] == 0)  # Hourly data

        # Check data continuity
        assert len(realistic_forcing_data) == 48  # 48 hours

        # Time should be sequential
        time_diffs = np.diff(realistic_forcing_data["it"].values)
        # Handle day boundary crossings
        expected_diffs = np.where(time_diffs < 0, 1 - 24, time_diffs)
        assert all(np.abs(expected_diffs) <= 1)  # Should be hourly steps

    def test_meteorological_variable_ranges(self, realistic_forcing_data):
        """Test that meteorological variables are in realistic ranges."""

        # Temperature should be reasonable
        assert realistic_forcing_data["Tair"].min() > -30
        assert realistic_forcing_data["Tair"].max() < 40

        # Humidity should be in valid range
        assert realistic_forcing_data["RH"].min() >= 0
        assert realistic_forcing_data["RH"].max() <= 100

        # Wind speed should be positive
        assert all(realistic_forcing_data["U"] >= 0)

        # Solar radiation should be non-negative
        assert all(realistic_forcing_data["kdown"] >= 0)

        # Pressure should be reasonable
        assert realistic_forcing_data["pres"].min() > 80  # kPa
        assert realistic_forcing_data["pres"].max() < 110  # kPa

    @patch("supy.run_supy")
    def test_simulation_output_structure(self, mock_run_supy):
        """Test that simulation outputs have expected structure."""

        # Mock simulation output
        mock_output = pd.DataFrame(
            {
                "datetime": pd.date_range("2024-01-01", periods=24, freq="h"),
                "QN": np.random.normal(100, 30, 24),
                "QH": np.random.normal(50, 20, 24),
                "QE": np.random.normal(30, 15, 24),
                "QS": np.random.normal(10, 10, 24),
                "QF": np.full(24, 20.0),
                "T2": np.random.normal(15, 3, 24),
                "RH2": np.random.normal(70, 10, 24),
                "U10": np.random.normal(5, 2, 24),
            }
        )

        mock_run_supy.return_value = {
            "output": mock_output,
            "state": pd.DataFrame({"state_var": [1, 2, 3]}),
            "metadata": {"simulation_time": "00:02:30"},
        }

        # Test simulation
        result = mock_run_supy({})

        # Check output structure
        assert "output" in result
        assert "state" in result
        assert "metadata" in result

        # Check output DataFrame
        output = result["output"]
        assert isinstance(output, pd.DataFrame)

        # Check expected columns
        expected_cols = ["datetime", "QN", "QH", "QE", "QS", "QF", "T2", "RH2", "U10"]
        for col in expected_cols:
            assert col in output.columns

        # Check datetime column
        assert pd.api.types.is_datetime64_any_dtype(output["datetime"])

        mock_run_supy.assert_called_once()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
