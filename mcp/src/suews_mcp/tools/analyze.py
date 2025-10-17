"""Analysis and results processing tools."""

from pathlib import Path
from typing import Dict, Any, List, Optional
import pandas as pd


async def load_results(
    results_path: str,
    variables: Optional[List[str]] = None,
) -> Dict[str, Any]:
    """Load simulation results from file.

    Args:
        results_path: Path to results file (CSV, NetCDF, etc.)
        variables: Optional list of variables to load

    Returns:
        Dictionary with loaded results information
    """
    try:
        results_path = Path(results_path)

        if not results_path.exists():
            return {"error": f"Results file not found: {results_path}"}

        # Load based on file extension
        if results_path.suffix == ".csv":
            df = pd.read_csv(results_path)
        elif results_path.suffix in [".nc", ".nc4"]:
            import xarray as xr

            ds = xr.open_dataset(results_path)
            if variables:
                ds = ds[variables]
            df = ds.to_dataframe()
        else:
            return {"error": f"Unsupported file format: {results_path.suffix}"}

        # Filter variables if requested
        if variables:
            available_vars = [v for v in variables if v in df.columns]
            df = df[available_vars]

        return {
            "success": True,
            "num_rows": len(df),
            "num_columns": len(df.columns),
            "columns": list(df.columns),
            "time_range": {
                "start": str(df.index.min()) if hasattr(df.index, "min") else None,
                "end": str(df.index.max()) if hasattr(df.index, "max") else None,
            },
        }

    except Exception as e:
        return {"error": str(e)}


async def compute_statistics(
    results_path: str,
    variables: List[str],
    aggregation: str = "mean",
) -> Dict[str, Any]:
    """Compute statistics on simulation results.

    Args:
        results_path: Path to results file
        variables: List of variables to analyze
        aggregation: Aggregation method (mean, sum, min, max, std)

    Returns:
        Dictionary with computed statistics
    """
    try:
        # Load results
        results_path = Path(results_path)
        if results_path.suffix == ".csv":
            df = pd.read_csv(results_path)
        else:
            return {"error": "Only CSV format supported currently"}

        # Check variables exist
        missing_vars = [v for v in variables if v not in df.columns]
        if missing_vars:
            return {"error": f"Variables not found: {missing_vars}"}

        # Compute statistics
        stats = {}
        for var in variables:
            if aggregation == "mean":
                stats[var] = float(df[var].mean())
            elif aggregation == "sum":
                stats[var] = float(df[var].sum())
            elif aggregation == "min":
                stats[var] = float(df[var].min())
            elif aggregation == "max":
                stats[var] = float(df[var].max())
            elif aggregation == "std":
                stats[var] = float(df[var].std())
            else:
                return {"error": f"Unknown aggregation: {aggregation}"}

        return {
            "success": True,
            "aggregation": aggregation,
            "statistics": stats,
        }

    except Exception as e:
        return {"error": str(e)}


async def create_plot(
    results_path: str,
    variables: List[str],
    output_path: str,
    plot_type: str = "timeseries",
) -> Dict[str, Any]:
    """Create a plot from simulation results.

    Args:
        results_path: Path to results file
        variables: Variables to plot
        output_path: Where to save plot
        plot_type: Type of plot (timeseries, scatter, histogram)

    Returns:
        Dictionary with plot creation results
    """
    try:
        import matplotlib.pyplot as plt

        # Load results
        results_path = Path(results_path)
        if results_path.suffix == ".csv":
            df = pd.read_csv(results_path, index_col=0, parse_dates=True)
        else:
            return {"error": "Only CSV format supported currently"}

        # Check variables
        missing_vars = [v for v in variables if v not in df.columns]
        if missing_vars:
            return {"error": f"Variables not found: {missing_vars}"}

        # Create plot
        fig, ax = plt.subplots(figsize=(10, 6))

        if plot_type == "timeseries":
            for var in variables:
                ax.plot(df.index, df[var], label=var)
            ax.set_xlabel("Time")
            ax.set_ylabel("Value")
            ax.legend()

        elif plot_type == "scatter":
            if len(variables) != 2:
                return {"error": "Scatter plot requires exactly 2 variables"}
            ax.scatter(df[variables[0]], df[variables[1]])
            ax.set_xlabel(variables[0])
            ax.set_ylabel(variables[1])

        elif plot_type == "histogram":
            df[variables].hist(ax=ax, bins=30)

        else:
            return {"error": f"Unknown plot type: {plot_type}"}

        # Save plot
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_path, dpi=150, bbox_inches="tight")
        plt.close()

        return {
            "success": True,
            "plot_path": str(output_path),
            "plot_type": plot_type,
            "variables": variables,
        }

    except Exception as e:
        return {"error": str(e)}


async def export_results(
    results_path: str,
    output_path: str,
    format: str = "csv",
    variables: Optional[List[str]] = None,
) -> Dict[str, Any]:
    """Export results to different format.

    Args:
        results_path: Path to source results
        output_path: Output path
        format: Output format (csv, netcdf, json)
        variables: Optional variable subset

    Returns:
        Dictionary with export results
    """
    try:
        # Load results
        results_path = Path(results_path)
        if results_path.suffix == ".csv":
            df = pd.read_csv(results_path, index_col=0, parse_dates=True)
        else:
            return {"error": "Source format not supported"}

        # Filter variables if requested
        if variables:
            available = [v for v in variables if v in df.columns]
            df = df[available]

        # Export to requested format
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        if format == "csv":
            df.to_csv(output_path)
        elif format == "json":
            df.to_json(output_path, orient="records", date_format="iso")
        elif format == "netcdf":
            df.to_xarray().to_netcdf(output_path)
        else:
            return {"error": f"Output format not supported: {format}"}

        return {
            "success": True,
            "output_path": str(output_path),
            "format": format,
        }

    except Exception as e:
        return {"error": str(e)}
