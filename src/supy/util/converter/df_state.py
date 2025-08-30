"""df_state format converter for version migrations.

This module handles conversion of df_state between different SuPy versions,
particularly for migrating from pre-2025 format to current format.
"""

import logging
from pathlib import Path
from functools import lru_cache

import pandas as pd

logger = logging.getLogger(__name__)


@lru_cache(maxsize=1)
def _get_current_df_state_template() -> pd.DataFrame:
    """Get the current df_state template structure.

    This is cached to avoid repeated loading of sample data.

    Returns
    -------
        DataFrame with current df_state structure
    """
    # We're inside supy, so we can import directly
    from ..._supy_module import load_sample_data

    logger.debug("Loading current df_state template from sample data")
    df_template, _ = load_sample_data()
    return df_template


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
    """Detect df_state format version by comparing with current template.

    Returns
    -------
        'current': Matches current SuPy version exactly
        'old': Different from current version (needs conversion)
    """
    # Get current template
    df_template = _get_current_df_state_template()

    # Compare column sets
    input_cols = set(df.columns)
    template_cols = set(df_template.columns)

    # If columns match exactly, it's current
    if input_cols == template_cols:
        logger.info("Detected current df_state format")
        return "current"

    # Otherwise it's old/different and needs conversion
    missing_cols = template_cols - input_cols
    extra_cols = input_cols - template_cols
    common_cols = input_cols & template_cols

    logger.info(f"Detected old/different df_state format:")
    logger.info(f"  - {len(common_cols)} common columns")
    logger.info(f"  - {len(missing_cols)} missing columns (will add defaults)")
    logger.info(f"  - {len(extra_cols)} extra columns (will be removed)")

    return "old"


def convert_df_state_format(df_old: pd.DataFrame) -> pd.DataFrame:
    """Convert old/different df_state format to current format.

    Approach:
    - Compare input with current template
    - Keep all common columns with their values
    - Add missing columns with sensible defaults
    - Remove extra columns not in current format

    Args:
        df_old: DataFrame in old/different df_state format

    Returns
    -------
        DataFrame in current df_state format
    """
    logger.info("Converting df_state to current format...")

    # Get template for current version
    df_template = _get_current_df_state_template()

    # Create new DataFrame with correct structure
    df_new = pd.DataFrame(index=df_old.index, columns=df_template.columns)
    if hasattr(df_template.index, "name"):
        df_new.index.name = df_template.index.name

    # Identify column differences
    common_cols = set(df_old.columns) & set(df_template.columns)
    missing_cols = set(df_template.columns) - set(df_old.columns)
    extra_cols = set(df_old.columns) - set(df_template.columns)

    # Copy all common columns - preserve existing data
    logger.info(f"Preserving {len(common_cols)} common columns...")
    for col in common_cols:
        try:
            # Direct assignment preserves data exactly
            df_new[col] = df_old[col].values
        except Exception as e:
            logger.warning(f"Failed to copy column {col}: {e}, using template default")
            # Fall back to template value if copy fails
            if len(df_old) == 1:
                df_new[col] = df_template[col].iloc[0]
            else:
                df_new[col] = [df_template[col].iloc[0]] * len(df_old)

    # Add missing columns with appropriate defaults
    logger.info(f"Adding {len(missing_cols)} missing columns with defaults...")
    for col in missing_cols:
        template_value = df_template[col].iloc[0]
        col_name = col[0] if isinstance(col, tuple) else col

        # Smart defaults based on column name
        if "buildingname" in str(col_name).lower():
            default_val = "building_1"
        elif "buildingtype" in str(col_name).lower():
            default_val = "residential"
        elif "description" in str(col_name).lower():
            default_val = "Converted from previous df_state format"
        elif "config" in str(col_name).lower():
            default_val = "default"
        else:
            # Use template default
            default_val = template_value

        # Apply to all rows
        if len(df_old) == 1:
            df_new[col] = default_val
        else:
            df_new[col] = [default_val] * len(df_old)

    # Ensure data types match template where possible
    logger.info("Aligning data types...")
    for col in df_new.columns:
        try:
            target_dtype = df_template[col].dtype
            if df_new[col].dtype != target_dtype:
                df_new[col] = df_new[col].astype(target_dtype)
        except (ValueError, TypeError):
            pass  # Keep original dtype if conversion fails

    # Log what was changed
    if extra_cols:
        # Extract column names for logging
        removed_names = []
        for col in list(extra_cols)[:10]:  # Show first 10
            if isinstance(col, tuple):
                removed_names.append(f"{col[0]}[{col[1]}]")
            else:
                removed_names.append(str(col))

        logger.info(
            f"Removed {len(extra_cols)} extra columns including: {', '.join(removed_names)}"
        )
        if len(extra_cols) > 10:
            logger.info(f"  ... and {len(extra_cols) - 10} more")

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
