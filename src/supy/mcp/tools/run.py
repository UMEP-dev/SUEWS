"""
Run Simulation MCP Tool.

Wraps SuPy simulation execution functionality.
"""

from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    from ..._supy_module import run_supy, load_SampleData
    from ...util._io import read_forcing
    from ...suews_sim import SUEWSSimulation
except ImportError:
    # Handle imports for development/testing
    run_supy = None
    load_SampleData = None
    read_forcing = None
    SUEWSSimulation = None

import pandas as pd

from .base import MCPTool


class RunSimulationTool(MCPTool):
    """
    MCP tool for running SUEWS simulations.

    Wraps SuPy's run_supy function and SUEWSSimulation.run() method.
    Supports running with custom forcing data or sample data.
    """

    def __init__(self):
        super().__init__(
            name="run_simulation",
            description="Execute SUEWS urban climate model simulations with forcing data",
        )

    def get_parameters(self) -> List[Dict[str, Any]]:
        """Get tool parameter definitions."""
        return [
            {
                "name": "forcing_path",
                "type": "string",
                "description": "Path to forcing data file (.txt, .csv, or .nc) - optional if use_sample_data=true",
                "required": False,
            },
            {
                "name": "config_path",
                "type": "string",
                "description": "Path to YAML configuration file - optional if using sample data",
                "required": False,
            },
            {
                "name": "use_sample_data",
                "type": "boolean",
                "description": "Use SuPy sample data for quick testing (default: false)",
                "required": False,
            },
            {
                "name": "start_time",
                "type": "string",
                "description": "Start time for simulation (ISO format, e.g., '2012-01-01T00:00:00')",
                "required": False,
            },
            {
                "name": "end_time",
                "type": "string",
                "description": "End time for simulation (ISO format, e.g., '2012-12-31T23:00:00')",
                "required": False,
            },
            {
                "name": "time_step",
                "type": "integer",
                "description": "Time step in seconds (default: 3600 for hourly)",
                "required": False,
            },
            {
                "name": "save_state",
                "type": "boolean",
                "description": "Whether to save final model state (default: true)",
                "required": False,
            },
            {
                "name": "parallel",
                "type": "boolean",
                "description": "Use parallel processing if available (default: false)",
                "required": False,
            },
        ]

    async def _execute(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute simulation tool."""
        # Check if required modules are available
        if run_supy is None:
            return self.translator.create_structured_response(
                success=False,
                errors=[
                    "SuPy run modules not available. This may be a development environment."
                ],
            )

        # Extract and validate parameters
        forcing_path = arguments.get("forcing_path")
        config_path = arguments.get("config_path")
        use_sample_data = (
            self._validate_parameter(arguments, "use_sample_data", bool, required=False)
            or False
        )
        start_time = arguments.get("start_time")
        end_time = arguments.get("end_time")
        time_step = (
            self._validate_parameter(arguments, "time_step", int, required=False)
            or 3600
        )
        save_state = (
            self._validate_parameter(arguments, "save_state", bool, required=False)
            or True
        )
        parallel = (
            self._validate_parameter(arguments, "parallel", bool, required=False)
            or False
        )

        try:
            simulation_info = {
                "time_step": time_step,
                "save_state": save_state,
                "parallel": parallel,
            }

            # Load forcing data and initial state
            if use_sample_data:
                if load_SampleData is None:
                    return self.translator.create_structured_response(
                        success=False, errors=["Sample data loading not available"]
                    )

                # Use SuPy sample data
                df_state_init, df_forcing = load_SampleData()
                simulation_info["data_source"] = "sample_data"
                simulation_info["forcing_shape"] = df_forcing.shape
                simulation_info["forcing_period"] = {
                    "start": df_forcing.index[0].isoformat(),
                    "end": df_forcing.index[-1].isoformat(),
                }

            else:
                # Load custom data
                if not forcing_path:
                    return self.translator.create_structured_response(
                        success=False,
                        errors=[
                            "Either forcing_path must be provided or use_sample_data must be true"
                        ],
                    )

                # Validate and load forcing data
                forcing_path_obj = self.translator.validate_file_path(forcing_path)
                simulation_info["forcing_path"] = str(forcing_path_obj)

                # Load forcing data
                if read_forcing:
                    df_forcing = read_forcing(forcing_path_obj)
                else:
                    # Fallback to basic pandas loading
                    if forcing_path_obj.suffix == ".csv":
                        df_forcing = pd.read_csv(
                            forcing_path_obj, index_col=0, parse_dates=True
                        )
                    elif forcing_path_obj.suffix == ".txt":
                        # Try to read as space/tab delimited
                        df_forcing = pd.read_csv(
                            forcing_path_obj, sep=r"\s+", index_col=0, parse_dates=True
                        )
                    else:
                        return self.translator.create_structured_response(
                            success=False,
                            errors=[
                                f"Unsupported forcing file format: {forcing_path_obj.suffix}"
                            ],
                        )

                simulation_info["data_source"] = "custom_file"
                simulation_info["forcing_shape"] = df_forcing.shape

                # Initialize default state if no config provided
                if config_path:
                    config_path_obj = self.translator.validate_file_path(config_path)
                    simulation_info["config_path"] = str(config_path_obj)

                    # Use SUEWSSimulation for advanced configuration
                    if SUEWSSimulation:
                        sim = SUEWSSimulation(config_path_obj)
                        sim.update_forcing(df_forcing)
                        result = sim.run()

                        simulation_info["method"] = "SUEWSSimulation"
                        simulation_info["simulation_completed"] = True

                        return self.translator.create_structured_response(
                            success=True,
                            data=simulation_info,
                            message="Simulation completed successfully using SUEWSSimulation",
                        )
                else:
                    # Use basic initialization for simple runs
                    from ..._supy_module import init_config

                    if init_config:
                        df_state_init = init_config()
                    else:
                        return self.translator.create_structured_response(
                            success=False,
                            errors=[
                                "Cannot initialize default state without config file"
                            ],
                        )

            # Filter data by time range if specified
            start_ts, end_ts = self.translator.validate_time_range(start_time, end_time)

            if start_ts or end_ts:
                original_shape = df_forcing.shape

                if start_ts:
                    df_forcing = df_forcing[df_forcing.index >= start_ts]
                if end_ts:
                    df_forcing = df_forcing[df_forcing.index <= end_ts]

                simulation_info["time_filtering"] = {
                    "original_shape": original_shape,
                    "filtered_shape": df_forcing.shape,
                    "start_time": start_ts.isoformat() if start_ts else None,
                    "end_time": end_ts.isoformat() if end_ts else None,
                }

            # Validate we have data to run
            if df_forcing.empty:
                return self.translator.create_structured_response(
                    success=False, errors=["No forcing data available after filtering"]
                )

            # Run simulation
            simulation_info["method"] = "run_supy"
            simulation_info["execution_start"] = pd.Timestamp.now().isoformat()

            if parallel:
                # Note: Parallel execution would need additional setup
                simulation_info["parallel_note"] = (
                    "Parallel execution requested but may not be available"
                )

            # Execute simulation
            if save_state:
                df_output, df_state_final = run_supy(
                    df_forcing, df_state_init, save_state=True
                )
                simulation_info["state_saved"] = True
            else:
                df_output = run_supy(df_forcing, df_state_init, save_state=False)
                df_state_final = None
                simulation_info["state_saved"] = False

            simulation_info["execution_end"] = pd.Timestamp.now().isoformat()
            simulation_info["simulation_completed"] = True

            # Prepare output information
            output_info = self.translator.serialize_dataframe(df_output, max_rows=10)
            simulation_info["output"] = output_info

            if df_state_final is not None:
                state_info = self.translator.serialize_dataframe(
                    df_state_final, max_rows=10
                )
                simulation_info["final_state"] = state_info

            # Calculate basic statistics
            if not df_output.empty:
                simulation_info["statistics"] = self._calculate_basic_statistics(
                    df_output
                )

            return self.translator.create_structured_response(
                success=True,
                data=simulation_info,
                message="Simulation completed successfully",
            )

        except Exception as e:
            return self.translator.create_structured_response(
                success=False, errors=[f"Simulation execution failed: {str(e)}"]
            )

    def _calculate_basic_statistics(self, df_output: pd.DataFrame) -> Dict[str, Any]:
        """
        Calculate basic statistics for simulation output.

        Parameters
        ----------
        df_output : pd.DataFrame
            Simulation output data

        Returns
        -------
        dict
            Basic statistics
        """
        stats = {
            "duration": {
                "start": df_output.index[0].isoformat(),
                "end": df_output.index[-1].isoformat(),
                "n_timesteps": len(df_output),
            },
            "variables": list(df_output.columns),
            "n_variables": len(df_output.columns),
        }

        # Calculate statistics for key variables if they exist
        key_vars = ["QH", "QE", "QS", "QN", "QF", "T2", "RH2", "U10"]
        available_key_vars = [var for var in key_vars if var in df_output.columns]

        if available_key_vars:
            stats["key_variables"] = {}
            for var in available_key_vars:
                series = df_output[var]
                stats["key_variables"][var] = {
                    "mean": float(series.mean()),
                    "std": float(series.std()),
                    "min": float(series.min()),
                    "max": float(series.max()),
                    "units": self._get_variable_units(var),
                }

        return stats

    def _get_variable_units(self, var_name: str) -> str:
        """Get units for common SUEWS variables."""
        units_map = {
            "QH": "W m-2",  # Sensible heat flux
            "QE": "W m-2",  # Latent heat flux
            "QS": "W m-2",  # Storage heat flux
            "QN": "W m-2",  # Net all-wave radiation
            "QF": "W m-2",  # Anthropogenic heat flux
            "T2": "°C",  # 2m air temperature
            "RH2": "%",  # 2m relative humidity
            "U10": "m s-1",  # 10m wind speed
        }
        return units_map.get(var_name, "unknown")
