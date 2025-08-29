"""df_state format converter for version migrations.

This module handles conversion of df_state between different SuPy versions,
particularly for migrating from pre-2025 format to current format.
"""

import logging
from pathlib import Path

import pandas as pd

logger = logging.getLogger(__name__)


def load_df_state_file(file_path: Path) -> pd.DataFrame:
    """Load df_state from a specific CSV or pickle file.

    Args:
        file_path: Path to df_state file

    Returns
    -------
        DataFrame with df_state data

    Raises
    ------
        ValueError: If file is not valid df_state format
    """
    logger.info(f"Loading df_state from: {file_path}")

    try:
        if file_path.suffix in {".pkl", ".pickle"}:
            df = pd.read_pickle(file_path)
        else:  # CSV
            # Try multi-index header format first (standard df_state)
            df = pd.read_csv(file_path, header=[0, 1], index_col=0)

        # Validate it's a df_state structure
        if not isinstance(df.columns, pd.MultiIndex):
            raise ValueError("Invalid df_state: expected MultiIndex columns")

        logger.info(f"  Loaded {df.shape[0]} grids with {len(df.columns)} columns")
        return df

    except Exception as e:
        raise ValueError(
            f"Failed to load df_state from {file_path}: {e}\n"
            f"Ensure file is a valid df_state CSV (with multi-level headers) or pickle"
        ) from e


def detect_df_state_version(df: pd.DataFrame) -> str:
    """Detect df_state format version by checking for known version indicators.

    Returns
    -------
        'current': Has expected current version columns
        'old': Has deprecated columns or missing new required columns
    """
    # Extract first-level column names
    col_names = {col[0] if isinstance(col, tuple) else col for col in df.columns}

    # Known deprecated columns from old versions
    old_indicators = {
        "age_0_4",
        "age_5_11",
        "age_12_18",
        "age_19_64",
        "age_65plus",
        "hhs0",
    }

    # Required new columns in current version
    new_required = {
        "buildingname",
        "buildingtype",
        "config",
        "description",
    }

    # Check for deprecated columns
    has_old = any(col in col_names for col in old_indicators)
    if has_old:
        logger.info("Detected old df_state format (has deprecated columns)")
        return "old"

    # Check for new required columns
    has_new = all(col in col_names for col in new_required)
    if has_new:
        logger.info("Detected current df_state format")
        return "current"

    # If missing new required columns, it's old
    missing_new = new_required - col_names
    if missing_new:
        logger.info(f"Detected old df_state format (missing columns: {missing_new})")
        return "old"

    # Default to current if unsure
    logger.info("Unable to determine version definitively - assuming current")
    return "current"


def convert_df_state_format(df_old: pd.DataFrame) -> pd.DataFrame:  # noqa: PLR0912, PLR0915
    """Convert old/different df_state format to current format.

    Approach:
    - Remove deprecated columns
    - Add new required columns with defaults
    - Keep all other existing columns

    Args:
        df_old: DataFrame in old/different df_state format

    Returns
    -------
        DataFrame in current df_state format
    """
    logger.info("Converting df_state to current format...")

    # Start with a copy of the old dataframe
    df_new = df_old.copy()

    # Extract first-level column names for checking
    col_names = {col[0] if isinstance(col, tuple) else col for col in df_new.columns}

    # Remove deprecated columns if they exist
    deprecated_cols = {
        "age_0_4",
        "age_5_11",
        "age_12_18",
        "age_19_64",
        "age_65plus",
        "hhs0",
    }
    cols_to_remove = []
    for col in df_new.columns:
        col_name = col[0] if isinstance(col, tuple) else col
        if col_name in deprecated_cols:
            cols_to_remove.append(col)

    if cols_to_remove:
        df_new = df_new.drop(columns=cols_to_remove)
        logger.info(
            f"Removed {len(cols_to_remove)} deprecated columns: {[c[0] if isinstance(c, tuple) else c for c in cols_to_remove[:5]]}"
        )

    # Add new required columns if missing
    new_columns_defaults = {
        ("buildingname", "0"): "building_1",
        ("buildingtype", "0"): "residential",
        ("config", "0"): "default",
        ("description", "0"): "Converted from previous df_state format",
        ("h_std", "0"): 1.0,
        ("lambda_c", "0"): 0.5,
        ("n_buildings", "0"): 1,
    }

    added_cols = []
    for col, default_val in new_columns_defaults.items():
        if col not in df_new.columns:
            df_new[col] = default_val
            added_cols.append(col)

    if added_cols:
        logger.info(f"Added {len(added_cols)} new columns with defaults")

    logger.info("Conversion complete")
    return df_new


def validate_converted_df_state(df_state: pd.DataFrame) -> tuple[bool, str]:
    """Validate converted df_state using SuPy's check_state.

    Args:
        df_state: DataFrame to validate

    Returns
    -------
        Tuple of (is_valid, message)
    """
    try:
        import supy as sp  # noqa: PLC0415 - Late import to avoid circular dependency

        sp.check_state(df_state)
        return True, "Validation passed"
    except Exception as e:
        return False, f"Validation warning: {e!s}"
