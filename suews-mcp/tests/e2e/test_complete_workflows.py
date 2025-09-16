"""End-to-end tests for complete SUEWS workflows through MCP Server."""

import pytest
import asyncio
import tempfile
import pandas as pd
import numpy as np
import yaml
import time
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch, MagicMock
from typing import Dict, Any, List
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.server import SUEWSMCPServer
from suews_mcp.handlers import SUEWSMCPHandlers, SUPY_MCP_TOOLS_AVAILABLE
from suews_mcp.config import MCPServerConfig
from suews_mcp.preprocessing import ForcingDataPreprocessor, ConfigValidator
from suews_mcp.validation import validate_configuration, validate_forcing_data


class TestCompleteWorkflows:
    """Test complete end-to-end SUEWS workflows."""

    @pytest.fixture
    def e2e_config(self):
        """Configuration optimized for end-to-end testing."""
        return MCPServerConfig(
            server_name="e2e-test-server",
            server_version="0.1.0-e2e",
            log_level="INFO",
            enable_debug=True,
            suews_timeout=600,  # Longer timeout for complete workflows
            max_concurrent_simulations=2,
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
            max_memory_mb=2048,
            max_simulation_time_hours=24.0,
        )

    @pytest.fixture
    def comprehensive_forcing_data(self):
        """Generate comprehensive forcing data for complete workflow testing."""
        # Generate one week of realistic hourly data
        n_hours = 168  # 7 days * 24 hours
        base_date = pd.Timestamp("2024-07-15")  # Summer data for realistic solar

        hours = np.arange(n_hours)

        # Realistic diurnal and multi-day patterns
        # Solar radiation with diurnal cycle
        solar_base = 1000  # Peak solar at noon
        solar_cycle = np.maximum(
            0,
            solar_base
            * np.sin(np.pi * (hours % 24) / 24)
            * np.maximum(0, np.cos(np.pi * ((hours % 24) - 12) / 12)),
        )

        # Temperature with diurnal cycle and slight weekly variation
        temp_base = 22  # Base temperature
        temp_diurnal = 10 * np.sin(2 * np.pi * (hours % 24) / 24 - np.pi / 2)
        temp_weekly = 3 * np.sin(2 * np.pi * hours / (7 * 24))
        temp_cycle = temp_base + temp_diurnal + temp_weekly

        # Wind with random variation but realistic bounds
        wind_base = np.clip(3.0 + np.random.normal(0, 1.5, n_hours), 0.5, 15.0)

        # Humidity with inverse temperature correlation
        rh_base = np.clip(
            75 - 0.8 * temp_diurnal + np.random.normal(0, 10, n_hours), 30, 95
        )

        # Realistic energy fluxes based on solar radiation
        qn_values = solar_cycle * 0.6 + np.random.normal(0, 30, n_hours)
        qh_values = np.maximum(0, qn_values * 0.4 + np.random.normal(0, 20, n_hours))
        qe_values = np.maximum(0, qn_values * 0.3 + np.random.normal(0, 15, n_hours))
        qs_values = qn_values * 0.15 + np.random.normal(0, 10, n_hours)
        qf_values = np.full(n_hours, 25.0) + np.random.normal(0, 5, n_hours)

        # Occasional precipitation events
        rain_prob = np.random.random(n_hours)
        rain_values = np.where(
            rain_prob < 0.05, np.random.exponential(2.0, n_hours), 0.0
        )

        data = {
            "iy": [2024] * n_hours,
            "id": [((base_date + pd.Timedelta(hours=h)).dayofyear) for h in hours],
            "it": [h % 24 for h in hours],
            "imin": [0] * n_hours,
            "qn": qn_values,
            "qh": qh_values,
            "qe": qe_values,
            "qs": qs_values,
            "qf": qf_values,
            "U": wind_base,
            "RH": rh_base,
            "Tair": temp_cycle + np.random.normal(0, 1.5, n_hours),
            "pres": 101.325 + np.random.normal(0, 0.8, n_hours),
            "rain": rain_values,
            "kdown": solar_cycle + np.random.normal(0, 50, n_hours),
            "ldown": 350
            + 20 * np.sin(2 * np.pi * (hours % 24) / 24)
            + np.random.normal(0, 20, n_hours),
            "fcld": np.clip(np.random.beta(2, 5, n_hours), 0, 1),
            "wuh": np.maximum(0, 8.0 + np.random.normal(0, 3, n_hours)),
            "xsmd": np.clip(np.random.normal(0.3, 0.1, n_hours), 0.1, 0.8),
        }

        return pd.DataFrame(data)

    @pytest.fixture
    def complete_suews_config(self, tmp_path):
        """Generate complete SUEWS configuration for workflow testing."""
        config = {
            "name": "e2e_workflow_test",
            "description": "Complete end-to-end workflow test configuration",
            "model": {
                "control": {
                    "tstep": 3600,  # 1-hour timestep
                    "forcing_file": {"value": "forcing_data.txt"},
                    "start_doy": {"value": 196},  # July 15
                    "end_doy": {"value": 202},  # July 21 (7 days)
                    "year": {"value": 2024},
                    "resolutionfilesout": {"value": 3600},
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 2},
                    "conductancemethod": {"value": 4},
                    "stabilitymethod": {"value": 3},
                    "storedrainprm": {"value": 2},
                    "anthropogenic": {"value": 1},
                    "snowuse": {"value": 1},
                    "conductance_choice": {"value": 1},
                },
                "output": {
                    "writefiles": {"value": 1},
                    "files_text": {"value": 1},
                    "files_rdf": {"value": 0},
                },
            },
            "sites": [
                {
                    "name": "london_test_site",
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
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 2.0},
                            "wetthresh": {"value": 0.2},
                            "statepaved": {"value": 1.0},
                        },
                        "bldgs": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.92},
                            "roughlenmmom": {"value": 1.0},
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 2.5},
                            "wetthresh": {"value": 0.15},
                            "statebldgs": {"value": 2.0},
                        },
                        "evetr": {
                            "sfr": {"value": 0.05},
                            "emis": {"value": 0.97},
                            "roughlenmmom": {"value": 2.0},
                            "lai": {"value": 4.5},
                            "storemin": {"value": 1.0},
                            "storemax": {"value": 5.0},
                            "wetthresh": {"value": 0.3},
                            "stateevetr": {"value": 3.0},
                        },
                        "dectr": {
                            "sfr": {"value": 0.10},
                            "emis": {"value": 0.96},
                            "roughlenmmom": {"value": 1.5},
                            "lai": {"value": 2.5},
                            "storemin": {"value": 1.0},
                            "storemax": {"value": 4.0},
                            "wetthresh": {"value": 0.25},
                            "statedectr": {"value": 2.5},
                        },
                        "grass": {
                            "sfr": {"value": 0.15},
                            "emis": {"value": 0.96},
                            "roughlenmmom": {"value": 0.05},
                            "lai": {"value": 3.0},
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 3.0},
                            "wetthresh": {"value": 0.35},
                            "stategrass": {"value": 1.5},
                        },
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 150.0},
                        "soilstore_surf": {"value": 15.0},
                        "qn1_snowpack": {"value": 0.0},
                        "qf_a": {"value": 25.0},
                        "qf_b": {"value": 0.5},
                    },
                    "anthropogenic": {
                        "qf_a": {"value": 25.0},
                        "qf_b": {"value": 0.5},
                        "qf_c": {"value": 0.0},
                        "profilesfile": {"value": "profiles.txt"},
                    },
                }
            ],
        }
        return config

    @pytest.mark.e2e
    @pytest.mark.slow
    @pytest.mark.asyncio
    async def test_complete_setup_to_analysis_workflow(
        self, e2e_config, comprehensive_forcing_data, complete_suews_config, tmp_path
    ):
        """Test complete workflow from setup through analysis."""

        # Step 1: Setup - Create server and handlers
        server = SUEWSMCPServer(e2e_config)
        handlers = server.handlers

        assert server is not None
        assert handlers is not None

        # Step 2: Initialize server
        init_params = {
            "protocol_version": "2024-11-05",
            "capabilities": {"roots": {"list_changed": False}},
            "client_info": {"name": "e2e-test-client", "version": "1.0.0"},
        }

        init_result = await handlers.handle_initialize(init_params)
        assert init_result["protocol_version"] == "2024-11-05"
        assert "server_info" in init_result
        assert init_result["server_info"]["name"] == "e2e-test-server"

        # Step 3: List available tools
        tools_result = await handlers.handle_list_tools()
        tools = tools_result["tools"] if "tools" in tools_result else tools_result.tools

        # Should have at least health_check plus enabled tools
        tool_names = [t["name"] if isinstance(t, dict) else t.name for t in tools]
        assert "health_check" in tool_names
        assert len(tool_names) >= 1

        # Step 4: Prepare test data
        config_file = tmp_path / "test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(complete_suews_config, f, default_flow_style=False)

        forcing_file = tmp_path / "forcing_data.txt"
        comprehensive_forcing_data.to_csv(
            forcing_file, sep=" ", index=False, float_format="%.2f"
        )

        output_dir = tmp_path / "output"
        output_dir.mkdir()

        # Step 5: Validate configuration (if validation tool is available)
        if e2e_config.enable_validation_tool:
            validation_args = {"config_file": str(config_file), "strict": True}

            with patch.object(handlers, "_call_validation_tool") as mock_validate:
                mock_validate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Configuration validation completed successfully!\n"
                            f"Config file: {config_file}\n"
                            f"Status: VALID\n"
                            f"Issues found: 0 errors, 2 warnings\n"
                            f"Ready for simulation.",
                        }
                    ],
                    "is_error": False,
                }

                validation_result = await handlers.handle_call_tool(
                    "validate_suews_config", validation_args
                )

                assert not validation_result.get("is_error", False)
                assert "VALID" in validation_result["content"][0]["text"]

        # Step 6: Run simulation (if simulation tool is available)
        if e2e_config.enable_simulation_tool:
            simulation_args = {
                "config_file": str(config_file),
                "simulation_id": "e2e_workflow_test_001",
                "output_dir": str(output_dir),
            }

            with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
                # Mock a realistic simulation response
                mock_simulate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"SUEWS simulation completed successfully!\n"
                            f"Simulation ID: e2e_workflow_test_001\n"
                            f"Configuration: {config_file}\n"
                            f"Output directory: {output_dir}\n"
                            f"Simulation period: 2024-07-15 to 2024-07-21 (7 days)\n"
                            f"Timestep: 3600 seconds\n"
                            f"Total timesteps: 168\n"
                            f"Execution time: 00:03:45\n"
                            f"Memory usage: 156 MB peak\n"
                            f"Status: SUCCESS",
                        }
                    ],
                    "is_error": False,
                }

                simulation_result = await handlers.handle_call_tool(
                    "run_suews_simulation", simulation_args
                )

                assert not simulation_result.get("is_error", False)
                assert "SUCCESS" in simulation_result["content"][0]["text"]
                assert (
                    "e2e_workflow_test_001" in simulation_result["content"][0]["text"]
                )

        # Step 7: Analyze results (if analysis tool is available)
        if e2e_config.enable_analysis_tool:
            # Create mock output file for analysis
            mock_output_file = tmp_path / "test_output.csv"
            mock_output_data = pd.DataFrame(
                {
                    "datetime": pd.date_range("2024-07-15", periods=168, freq="h"),
                    "QN": np.random.normal(150, 40, 168),
                    "QH": np.random.normal(60, 25, 168),
                    "QE": np.random.normal(40, 20, 168),
                    "QS": np.random.normal(25, 15, 168),
                    "QF": np.full(168, 25.0),
                    "T2": np.random.normal(22, 5, 168),
                    "RH2": np.random.normal(65, 15, 168),
                    "U10": np.random.normal(4, 2, 168),
                }
            )
            mock_output_data.to_csv(mock_output_file, index=False)

            analysis_args = {
                "output_file": str(mock_output_file),
                "metrics": ["QH", "QE", "QN", "T2"],
                "time_period": "daily",
            }

            with patch.object(handlers, "_call_analysis_tool") as mock_analyze:
                mock_analyze.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Analysis of {mock_output_file} completed successfully!\n"
                            f"Time period: 2024-07-15 to 2024-07-21 (7 days)\n"
                            f"Variables analyzed: QH, QE, QN, T2\n"
                            f"Statistics:\n"
                            f"  QN: mean=149.2 W/m², std=39.8 W/m², min=45.2 W/m², max=245.1 W/m²\n"
                            f"  QH: mean=59.8 W/m², std=24.7 W/m², min=12.3 W/m², max=112.4 W/m²\n"
                            f"  QE: mean=40.1 W/m², std=19.9 W/m², min=5.7 W/m², max=89.3 W/m²\n"
                            f"  T2: mean=22.1°C, std=4.8°C, min=14.2°C, max=31.7°C\n"
                            f"Energy balance closure: 89.2%\n"
                            f"Data quality: Excellent (99.4% valid data points)\n"
                            f"Analysis completed in 00:00:12",
                        }
                    ],
                    "is_error": False,
                }

                analysis_result = await handlers.handle_call_tool(
                    "analyze_suews_output", analysis_args
                )

                assert not analysis_result.get("is_error", False)
                assert "completed successfully" in analysis_result["content"][0]["text"]
                assert "Statistics:" in analysis_result["content"][0]["text"]

        # Step 8: Health check to verify system state
        health_result = await handlers.handle_call_tool("health_check", {})
        assert not health_result.get("is_error", False)
        assert "Status: healthy" in health_result["content"][0]["text"]

    @pytest.mark.e2e
    @pytest.mark.asyncio
    async def test_multi_step_error_recovery_workflow(self, e2e_config, tmp_path):
        """Test workflow with errors and recovery mechanisms."""

        handlers = SUEWSMCPHandlers(e2e_config)

        # Step 1: Start with invalid configuration
        invalid_config = {
            "name": "invalid_test",
            "model": {
                "control": {
                    # Missing required fields
                    "tstep": 3600
                    # Missing: forcing_file, start_doy, end_doy, year
                }
            },
            # Missing: sites section
        }

        config_file = tmp_path / "invalid_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(invalid_config, f)

        # Step 2: Try validation - should fail
        validation_args = {"config_file": str(config_file), "strict": True}

        with patch.object(handlers, "_call_validation_tool") as mock_validate:
            mock_validate.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f"Configuration validation failed!\n"
                        f"Config file: {config_file}\n"
                        f"Status: INVALID\n"
                        f"Errors found:\n"
                        f"  - Missing required field: model.control.forcing_file\n"
                        f"  - Missing required field: model.control.start_doy\n"
                        f"  - Missing required field: model.control.end_doy\n"
                        f"  - Missing required field: model.control.year\n"
                        f"  - Missing required section: sites\n"
                        f"Please fix these errors before proceeding.",
                    }
                ],
                "is_error": True,
            }

            result = await handlers.handle_call_tool(
                "validate_suews_config", validation_args
            )

            assert result.get("is_error", True)
            assert "INVALID" in result["content"][0]["text"]
            assert "Missing required field" in result["content"][0]["text"]

        # Step 3: Try simulation with invalid config - should fail
        simulation_args = {
            "config_file": str(config_file),
            "simulation_id": "error_recovery_test",
        }

        with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
            mock_simulate.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f"SUEWS simulation failed!\n"
                        f"Error: Configuration validation failed\n"
                        f"Details: Invalid configuration file: {config_file}\n"
                        f"Please validate and fix configuration before retrying.\n"
                        f"Use the validate_suews_config tool for detailed error information.",
                    }
                ],
                "is_error": True,
            }

            result = await handlers.handle_call_tool(
                "run_suews_simulation", simulation_args
            )

            assert result.get("is_error", True)
            assert "simulation failed" in result["content"][0]["text"]

        # Step 4: Create corrected configuration
        corrected_config = {
            "name": "corrected_test",
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": "forcing.txt"},
                    "start_doy": {"value": 1},
                    "end_doy": {"value": 2},
                    "year": {"value": 2024},
                },
                "physics": {"netradiationmethod": {"value": 3}},
            },
            "sites": [
                {
                    "name": "test_site",
                    "properties": {
                        "lat": {"value": 51.5074},
                        "lng": {"value": -0.1278},
                        "alt": {"value": 50.0},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.5}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.2}},
                    },
                    "initial_conditions": {"soilstore_id": {"value": 150.0}},
                }
            ],
        }

        corrected_config_file = tmp_path / "corrected_config.yml"
        with open(corrected_config_file, "w") as f:
            yaml.dump(corrected_config, f)

        # Step 5: Validate corrected configuration - should succeed
        corrected_validation_args = {
            "config_file": str(corrected_config_file),
            "strict": True,
        }

        with patch.object(handlers, "_call_validation_tool") as mock_validate:
            mock_validate.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f"Configuration validation completed successfully!\n"
                        f"Config file: {corrected_config_file}\n"
                        f"Status: VALID\n"
                        f"Issues found: 0 errors, 1 warning\n"
                        f"Warning: Using minimal physics configuration\n"
                        f"Ready for simulation.",
                    }
                ],
                "is_error": False,
            }

            result = await handlers.handle_call_tool(
                "validate_suews_config", corrected_validation_args
            )

            assert not result.get("is_error", False)
            assert "VALID" in result["content"][0]["text"]

        # Step 6: Run simulation with corrected config - should succeed
        corrected_simulation_args = {
            "config_file": str(corrected_config_file),
            "simulation_id": "error_recovery_success",
        }

        with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
            mock_simulate.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f"SUEWS simulation completed successfully!\n"
                        f"Simulation ID: error_recovery_success\n"
                        f"Configuration: {corrected_config_file}\n"
                        f"Status: SUCCESS\n"
                        f"Recovery workflow completed successfully.",
                    }
                ],
                "is_error": False,
            }

            result = await handlers.handle_call_tool(
                "run_suews_simulation", corrected_simulation_args
            )

            assert not result.get("is_error", False)
            assert "SUCCESS" in result["content"][0]["text"]
            assert "error_recovery_success" in result["content"][0]["text"]

    @pytest.mark.e2e
    @pytest.mark.asyncio
    async def test_concurrent_multi_step_workflows(self, e2e_config, tmp_path):
        """Test multiple concurrent workflows with different configurations."""

        # Set up for concurrent workflows
        e2e_config.max_concurrent_simulations = 3
        handlers = SUEWSMCPHandlers(e2e_config)

        # Create three different configurations
        configs = []
        for i in range(3):
            config = {
                "name": f"concurrent_test_{i+1}",
                "model": {
                    "control": {
                        "tstep": 3600,
                        "forcing_file": {"value": f"forcing_{i+1}.txt"},
                        "start_doy": {"value": 1 + i},
                        "end_doy": {"value": 2 + i},
                        "year": {"value": 2024},
                    },
                    "physics": {"netradiationmethod": {"value": 3}},
                },
                "sites": [
                    {
                        "name": f"site_{i+1}",
                        "properties": {
                            "lat": {"value": 51.5074 + i * 0.01},
                            "lng": {"value": -0.1278 - i * 0.01},
                            "alt": {"value": 50.0 + i * 10},
                        },
                        "land_cover": {
                            "paved": {"sfr": {"value": 0.4 + i * 0.05}},
                            "grass": {"sfr": {"value": 0.6 - i * 0.05}},
                        },
                        "initial_conditions": {
                            "soilstore_id": {"value": 150.0 + i * 25}
                        },
                    }
                ],
            }

            config_file = tmp_path / f"config_{i+1}.yml"
            with open(config_file, "w") as f:
                yaml.dump(config, f)
            configs.append(str(config_file))

        # Run concurrent simulations
        async def run_workflow(config_file: str, sim_id: str):
            """Run a complete workflow for one configuration."""

            # Validate
            validation_args = {"config_file": config_file, "strict": False}

            with patch.object(handlers, "_call_validation_tool") as mock_validate:
                mock_validate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Configuration {config_file} validated successfully!",
                        }
                    ],
                    "is_error": False,
                }

                validation_result = await handlers.handle_call_tool(
                    "validate_suews_config", validation_args
                )

                assert not validation_result.get("is_error", False)

            # Simulate
            simulation_args = {"config_file": config_file, "simulation_id": sim_id}

            with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
                # Simulate variable execution times
                execution_time = 0.1 + np.random.random() * 0.2
                await asyncio.sleep(execution_time)

                mock_simulate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Simulation {sim_id} completed successfully!\n"
                            f"Config: {config_file}\n"
                            f"Execution time: {execution_time:.3f}s",
                        }
                    ],
                    "is_error": False,
                }

                simulation_result = await handlers.handle_call_tool(
                    "run_suews_simulation", simulation_args
                )

                assert not simulation_result.get("is_error", False)
                assert sim_id in simulation_result["content"][0]["text"]

            return sim_id

        # Execute workflows concurrently
        tasks = [
            run_workflow(configs[0], "concurrent_sim_001"),
            run_workflow(configs[1], "concurrent_sim_002"),
            run_workflow(configs[2], "concurrent_sim_003"),
        ]

        # All workflows should complete successfully
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # Verify all completed successfully
        for i, result in enumerate(results):
            assert not isinstance(result, Exception), f"Workflow {i+1} failed: {result}"
            assert result == f"concurrent_sim_00{i+1}"

        # Check that simulations were tracked
        assert len(handlers._active_simulations) == 3

        # Health check should show all simulations
        health_result = await handlers.handle_call_tool("health_check", {})
        health_text = health_result["content"][0]["text"]
        assert (
            "Active simulations: 0/3" in health_text
            or "Completed simulations: 3" in health_text
        )

    @pytest.mark.e2e
    @pytest.mark.asyncio
    async def test_data_preprocessing_workflow(
        self, e2e_config, comprehensive_forcing_data, tmp_path
    ):
        """Test complete data preprocessing workflow."""

        handlers = SUEWSMCPHandlers(e2e_config)

        # Step 1: Create raw forcing data with issues
        raw_forcing = comprehensive_forcing_data.copy()

        # Introduce some data quality issues
        raw_forcing.loc[10:12, "qn"] = np.nan  # Missing values
        raw_forcing.loc[50, "Tair"] = -999.0  # Unrealistic temperature
        raw_forcing.loc[100:102, "U"] = -5.0  # Negative wind speed
        raw_forcing.loc[25, "RH"] = 150.0  # Invalid humidity

        raw_file = tmp_path / "raw_forcing.txt"
        raw_forcing.to_csv(raw_file, sep=" ", index=False, float_format="%.2f")

        # Step 2: Preprocess forcing data
        preprocessor = ForcingDataPreprocessor()
        processed_file = tmp_path / "processed_forcing.txt"

        preprocessing_result = preprocessor.preprocess_forcing_file(
            file_path=raw_file,
            output_path=processed_file,
            validate_energy_balance=True,
            auto_fix_issues=True,
        )

        # Should detect and fix issues
        assert preprocessing_result.success is True
        assert len(preprocessing_result.issues) > 0
        assert processed_file.exists()

        # Check that issues were detected
        issue_types = [issue.issue_type for issue in preprocessing_result.issues]
        assert any("missing" in itype.lower() for itype in issue_types)
        assert any(
            "invalid" in itype.lower() or "range" in itype.lower()
            for itype in issue_types
        )

        # Step 3: Validate processed data
        processed_data = pd.read_csv(processed_file, sep=" ")

        # Should have no missing values in critical columns
        critical_cols = ["qn", "qh", "qe", "Tair", "U", "RH"]
        for col in critical_cols:
            if col in processed_data.columns:
                assert (
                    not processed_data[col].isna().any()
                ), f"Column {col} still has missing values"

        # Should have realistic ranges
        if "Tair" in processed_data.columns:
            assert processed_data["Tair"].min() > -50
            assert processed_data["Tair"].max() < 60

        if "U" in processed_data.columns:
            assert all(processed_data["U"] >= 0)

        if "RH" in processed_data.columns:
            assert all(processed_data["RH"].between(0, 100))

        # Step 4: Create configuration with processed data
        config_with_processed = {
            "name": "preprocessing_workflow_test",
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": str(processed_file.name)},
                    "start_doy": {"value": 196},
                    "end_doy": {"value": 202},
                    "year": {"value": 2024},
                },
                "physics": {"netradiationmethod": {"value": 3}},
            },
            "sites": [
                {
                    "name": "preprocessing_test_site",
                    "properties": {
                        "lat": {"value": 51.5074},
                        "lng": {"value": -0.1278},
                        "alt": {"value": 50.0},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}},
                        "grass": {"sfr": {"value": 0.6}},
                    },
                    "initial_conditions": {"soilstore_id": {"value": 150.0}},
                }
            ],
        }

        config_file = tmp_path / "preprocessing_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(config_with_processed, f)

        # Step 5: Validate configuration with processed data
        validator = ConfigValidator()
        validation_result = validator.validate_config(
            config_path=config_file,
            strict_mode=False,
            check_file_paths=False,  # Skip file existence checks
        )

        assert validation_result.success is True

        # Step 6: Run simulation with preprocessed data
        simulation_args = {
            "config_file": str(config_file),
            "simulation_id": "preprocessing_workflow_001",
        }

        with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
            mock_simulate.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": f"SUEWS simulation completed successfully!\n"
                        f"Simulation ID: preprocessing_workflow_001\n"
                        f"Used preprocessed forcing data: {processed_file}\n"
                        f"Data quality: HIGH (issues automatically resolved)\n"
                        f"Status: SUCCESS",
                    }
                ],
                "is_error": False,
            }

            result = await handlers.handle_call_tool(
                "run_suews_simulation", simulation_args
            )

            assert not result.get("is_error", False)
            assert "SUCCESS" in result["content"][0]["text"]
            assert "preprocessing_workflow_001" in result["content"][0]["text"]


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
