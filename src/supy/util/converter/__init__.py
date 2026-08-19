"""SUEWS configuration converters."""

from pathlib import Path
from typing import Union

from .table import (
    convert_table,
    detect_table_version,
    list_ver_from,
    list_ver_to,
)
from .yaml import convert_to_yaml
from .df_state import (
    load_df_state_file,
    detect_df_state_version,
    convert_df_state_format,
    validate_converted_df_state,
)


def detect_input_type(input_file: Union[str, Path]) -> str:
    """Detect input type based on file.

    Args:
        input_file: Path to input file (must be a file, not directory)

    Returns:
        'nml' for RunControl.nml (table conversion)
        'df_state' for CSV/pickle files
        'yaml' for an existing YAML configuration (cross-release upgrade)

    Raises:
        ValueError: If input is not a file or has unknown extension
    """
    input_path = Path(input_file)

    if not input_path.exists():
        raise ValueError(f"Input file does not exist: {input_path}")

    if not input_path.is_file():
        raise ValueError(
            f"Input must be a file, not a directory. Got: {input_path}\n"
            f"For table conversion, specify: path/to/RunControl.nml\n"
            f"For df_state conversion, specify: path/to/df_state.csv or .pkl\n"
            f"For YAML upgrade, specify: path/to/config.yml"
        )

    # Check file type
    if input_path.name == "RunControl.nml" or input_path.suffix == ".nml":
        return "nml"
    elif input_path.suffix in [".csv", ".pkl", ".pickle"]:
        return "df_state"
    elif input_path.suffix in [".yml", ".yaml"]:
        return "yaml"
    else:
        raise ValueError(
            f"Unknown input file type: {input_path.suffix}\n"
            f"Supported: RunControl.nml for tables, .csv/.pkl for df_state, "
            f".yml/.yaml for cross-release YAML upgrade"
        )


__all__ = [
    "convert_table",
    "convert_to_yaml",
    "detect_table_version",
    "detect_input_type",
    "load_df_state_file",
    "detect_df_state_version",
    "convert_df_state_format",
    "validate_converted_df_state",
    "list_ver_from",
    "list_ver_to",
]
