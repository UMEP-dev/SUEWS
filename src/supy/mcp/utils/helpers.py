"""Helper utilities for MCP server operations."""

from pathlib import Path
from typing import Any, Dict
import json
import yaml


def load_yaml_file(file_path: str | Path) -> Dict[str, Any]:
    """Load and parse a YAML configuration file.

    Args:
        file_path: Path to YAML file

    Returns:
        Parsed YAML content as dictionary

    Raises:
        FileNotFoundError: If file doesn't exist
        yaml.YAMLError: If YAML parsing fails
    """
    file_path = Path(file_path)
    if not file_path.exists():
        raise FileNotFoundError(f"Configuration file not found: {file_path}")

    with open(file_path) as f:
        return yaml.safe_load(f)


def save_yaml_file(data: Dict[str, Any], file_path: str | Path) -> None:
    """Save dictionary as YAML file.

    Args:
        data: Dictionary to save
        file_path: Output path
    """
    file_path = Path(file_path)
    file_path.parent.mkdir(parents=True, exist_ok=True)

    with open(file_path, "w") as f:
        yaml.dump(data, f, default_flow_style=False, sort_keys=False)


def format_validation_error(error: Exception) -> str:
    """Format validation error for user-friendly display.

    Args:
        error: Pydantic ValidationError or other exception

    Returns:
        Formatted error message
    """
    from pydantic import ValidationError

    if isinstance(error, ValidationError):
        errors = []
        for err in error.errors():
            loc = " → ".join(str(l) for l in err["loc"])
            msg = err["msg"]
            errors.append(f"{loc}: {msg}")
        return "\n".join(errors)

    return str(error)


def format_results_summary(results) -> Dict[str, Any]:
    """Create summary of simulation results.

    Args:
        results: SuPy simulation results

    Returns:
        Dictionary with summary statistics
    """
    import pandas as pd

    summary = {}

    # Handle different result formats
    if hasattr(results, "to_dataframe"):
        df = results.to_dataframe()
    elif isinstance(results, pd.DataFrame):
        df = results
    else:
        return {"error": "Unknown result format"}

    # Basic statistics
    summary["shape"] = {"rows": len(df), "columns": len(df.columns)}
    summary["columns"] = list(df.columns)

    # Time range if available
    if hasattr(df.index, "min") and hasattr(df.index, "max"):
        summary["time_range"] = {
            "start": str(df.index.min()),
            "end": str(df.index.max()),
        }

    # Sample statistics for key variables
    numeric_cols = df.select_dtypes(include=["number"]).columns
    if len(numeric_cols) > 0:
        stats = df[numeric_cols].describe().to_dict()
        # Limit to first few columns to avoid overwhelming output
        summary["statistics"] = {col: stats[col] for col in list(numeric_cols)[:5]}

    return summary
