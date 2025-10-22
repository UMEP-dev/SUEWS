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


def load_results_file(file_path: str | Path) -> Any:
    """Load simulation results from file (common function for all formats).

    Args:
        file_path: Path to results file

    Returns:
        pandas.DataFrame with results (MultiIndex reset if present)

    Raises:
        FileNotFoundError: If file doesn't exist
        ValueError: If file format is not supported
    """
    import pandas as pd

    file_path = Path(file_path)

    if not file_path.exists():
        raise FileNotFoundError(f"Results file not found: {file_path}")

    # Load based on file extension
    if file_path.suffix == ".txt":
        # Text format from SUEWSSimulation.save()
        df = pd.read_csv(file_path, sep=r'\s+', index_col=0, parse_dates=True)
    elif file_path.suffix == ".csv":
        df = pd.read_csv(file_path, index_col=0, parse_dates=True)
    elif file_path.suffix == ".parquet":
        # Parquet format (default from SUEWSSimulation.save())
        df = pd.read_parquet(file_path)
    elif file_path.suffix == ".pkl":
        # Pickle file (legacy SuPy output format)
        df = pd.read_pickle(file_path)
    else:
        raise ValueError(f"Unsupported file format: {file_path.suffix}. Supported: .txt, .csv, .parquet, .pkl")

    # Handle MultiIndex: reset to make it easier to work with
    if isinstance(df.index, pd.MultiIndex):
        df = df.reset_index()

        # Set datetime column as index if it exists
        datetime_cols = [col for col in df.columns if 'datetime' in str(col).lower() or 'time' in str(col).lower()]
        if datetime_cols:
            df = df.set_index(datetime_cols[0])
        elif df.columns[0].dtype == 'datetime64[ns]':
            df = df.set_index(df.columns[0])

    # Handle MultiIndex columns: flatten to strings
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = ["_".join(map(str, col)).strip() for col in df.columns.values]

    return df


def format_results_summary(results) -> Dict[str, Any]:
    """Create summary of simulation results.

    Args:
        results: SuPy simulation results

    Returns:
        Dictionary with summary statistics (JSON-serializable)
    """
    import pandas as pd

    summary = {}

    # Handle different result formats
    if hasattr(results, "to_dataframe"):
        df = results.to_dataframe().copy()
    elif isinstance(results, pd.DataFrame):
        df = results.copy()
    else:
        return {"error": "Unknown result format"}

    # Handle MultiIndex columns (convert tuples to strings)
    if isinstance(df.columns, pd.MultiIndex):
        # Convert MultiIndex columns to string format: "level0_level1"
        df.columns = ["_".join(map(str, col)).strip() for col in df.columns.values]

    # Handle MultiIndex in index (for time_range extraction)
    index_for_stats = df.index
    if isinstance(df.index, pd.MultiIndex):
        # Get the datetime level if it exists
        if any(isinstance(level, pd.DatetimeIndex) for level in df.index.levels):
            # Find datetime level
            for i, level in enumerate(df.index.levels):
                if isinstance(level, pd.DatetimeIndex):
                    index_for_stats = df.index.get_level_values(i)
                    break

    # Basic statistics
    summary["shape"] = {"rows": len(df), "columns": len(df.columns)}
    # Convert column names to strings to ensure JSON serializability
    summary["columns"] = [str(col) for col in df.columns]

    # Time range if available
    if hasattr(index_for_stats, "min") and hasattr(index_for_stats, "max"):
        summary["time_range"] = {
            "start": str(index_for_stats.min()),
            "end": str(index_for_stats.max()),
        }

    # Sample statistics for key variables
    numeric_cols = df.select_dtypes(include=["number"]).columns
    if len(numeric_cols) > 0:
        stats = df[numeric_cols].describe().to_dict()
        # Limit to first few columns and convert all keys to strings
        summary["statistics"] = {
            str(col): {str(k): float(v) for k, v in stats[col].items()}
            for col in list(numeric_cols)[:5]
        }

    return summary
