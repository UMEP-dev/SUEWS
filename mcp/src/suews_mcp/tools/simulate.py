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
        # Import SuPy components
        from supy import run_supy

        # Load and validate config
        config_data = load_yaml_file(config_path)
        config = SUEWSConfig.model_validate(config_data)

        # Set up output directory
        if output_dir is None:
            output_dir = Path(config_path).parent / "output"
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Run simulation using SuPy
        # Note: run_supy interface may vary - adjust as needed
        results = run_supy(config)

        # Generate summary
        summary = {
            "success": True,
            "message": "Simulation completed successfully",
            "config": config.name,
            "output_dir": str(output_dir),
        }

        # Add results summary if available
        if results is not None:
            summary["results"] = format_results_summary(results)

        return summary

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
