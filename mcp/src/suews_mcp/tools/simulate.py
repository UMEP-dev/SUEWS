"""Simulation execution tools."""

from pathlib import Path
from typing import Dict, Any, Optional
import tempfile

from supy.data_model.core.config import SUEWSConfig
from ..utils.helpers import load_yaml_file, format_results_summary


async def run_simulation(
    config_path: str,
    output_dir: Optional[str] = None,
) -> Dict[str, Any]:
    """Run a SUEWS simulation from configuration file.

    Args:
        config_path: Path to YAML configuration file
        output_dir: Optional output directory for results

    Returns:
        Dictionary with simulation results and summary
    """
    try:
        # Import SUEWSSimulation class
        from supy import SUEWSSimulation

        # Create simulation from config
        # This loads config, creates df_state_init, and tries to load forcing
        sim = SUEWSSimulation(config_path)

        # Check if forcing data is loaded
        if sim.forcing is None:
            return {
                "success": False,
                "error": "No forcing data available. Please specify forcing file in config or use update_forcing()",
            }

        # Extract start and end dates from config
        start_date = None
        end_date = None
        if hasattr(sim.config, 'model') and hasattr(sim.config.model, 'control'):
            control = sim.config.model.control
            start_date = getattr(control, 'start_time', None)
            end_date = getattr(control, 'end_time', None)

        # Set up output directory
        if output_dir is None:
            output_dir = Path(config_path).parent / "output"
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Run simulation with config dates
        try:
            df_output = sim.run(start_date=start_date, end_date=end_date)
        except IndexError as e:
            # This typically means forcing data doesn't cover requested period
            # Get forcing data time range for helpful error message
            forcing_start = sim.forcing.index.min()
            forcing_end = sim.forcing.index.max()

            return {
                "success": False,
                "error": "Forcing data does not cover the requested simulation period",
                "details": {
                    "requested_period": {
                        "start": str(start_date) if start_date else "not specified",
                        "end": str(end_date) if end_date else "not specified",
                    },
                    "available_forcing_data": {
                        "start": str(forcing_start),
                        "end": str(forcing_end),
                    },
                    "suggestion": "Update config start_time and end_time to match forcing data period, or provide forcing data that covers the requested period",
                },
                "original_error": str(e),
            }

        # Save results
        saved_files = sim.save(output_dir)

        # Generate summary (convert all Path objects to strings for JSON serialization)
        summary = {
            "success": True,
            "message": "Simulation completed successfully",
            "config": sim.config.name if sim.config else "Unknown",
            "output_dir": str(output_dir),
            "num_timesteps": len(df_output) if df_output is not None else 0,
            "saved_files": [str(f) for f in saved_files] if saved_files else [],
        }

        # Add basic results statistics
        if df_output is not None:
            summary["results_summary"] = format_results_summary(df_output)

        return summary

    except FileNotFoundError as e:
        return {
            "success": False,
            "error": f"File not found: {str(e)}",
        }
    except RuntimeError as e:
        return {
            "success": False,
            "error": f"Runtime error: {str(e)}",
        }
    except Exception as e:
        return {
            "success": False,
            "error": f"Simulation failed: {str(e)}",
        }


async def check_simulation_status(simulation_id: str) -> Dict[str, Any]:
    """Check status of a running simulation.

    Args:
        simulation_id: Unique simulation identifier

    Returns:
        Dictionary with simulation status
    """
    # TODO: Implement simulation tracking if needed
    return {
        "status": "not_implemented",
        "message": "Simulation tracking not yet implemented",
    }


async def estimate_runtime(config_path: str) -> Dict[str, Any]:
    """Estimate simulation runtime based on configuration.

    Args:
        config_path: Path to configuration file

    Returns:
        Dictionary with runtime estimate
    """
    try:
        config_data = load_yaml_file(config_path)
        config = SUEWSConfig.model_validate(config_data)

        # Simple estimation based on number of sites and timesteps
        num_sites = len(config.sites)

        # Get time period info if available
        total_timesteps = 8760  # Default: 1 year hourly
        if hasattr(config.model, "time"):
            # Calculate from start/end dates if available
            pass

        # Rough estimate: ~0.1 seconds per site per timestep
        estimated_seconds = num_sites * total_timesteps * 0.1

        return {
            "num_sites": num_sites,
            "estimated_timesteps": total_timesteps,
            "estimated_runtime_seconds": estimated_seconds,
            "estimated_runtime_minutes": estimated_seconds / 60,
        }

    except Exception as e:
        return {"error": str(e)}
