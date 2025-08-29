"""df_state format converter for version migrations.

This module handles conversion of df_state between different SuPy versions,
particularly for migrating from pre-2025 format to current format.
"""

import pandas as pd
import numpy as np
from pathlib import Path
from typing import Optional, Union, Tuple
import logging

logger = logging.getLogger(__name__)


def detect_input_type(input_file: Union[str, Path]) -> str:
    """Detect input type based on file.
    
    Args:
        input_file: Path to input file (must be a file, not directory)
        
    Returns:
        'nml' for RunControl.nml (table conversion)
        'df_state' for CSV/pickle files
        
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
            f"For df_state conversion, specify: path/to/df_state.csv or .pkl"
        )
    
    # Check file type
    if input_path.name == 'RunControl.nml' or input_path.suffix == '.nml':
        return 'nml'
    elif input_path.suffix in ['.csv', '.pkl', '.pickle']:
        return 'df_state'
    else:
        raise ValueError(
            f"Unknown input file type: {input_path.suffix}\n"
            f"Supported: RunControl.nml for tables, .csv/.pkl for df_state"
        )


def load_df_state_file(file_path: Path) -> pd.DataFrame:
    """Load df_state from a specific CSV or pickle file.
    
    Args:
        file_path: Path to df_state file
        
    Returns:
        DataFrame with df_state data
        
    Raises:
        ValueError: If file is not valid df_state format
    """
    logger.info(f"Loading df_state from: {file_path}")
    
    try:
        if file_path.suffix in ['.pkl', '.pickle']:
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
        )


def detect_df_state_version(df: pd.DataFrame) -> str:
    """Detect df_state format version.
    
    Returns:
        'old': Has deprecated columns (age_0_4, etc.)
        'new': Has new columns (buildingname, etc.)
        'current': Matches current SuPy version
    """
    # Column sets for version detection
    old_indicators = {'age_0_4', 'age_5_11', 'age_12_18', 'age_19_64', 'age_65plus', 'hhs0'}
    new_indicators = {'buildingname', 'buildingtype', 'config', 'description', 'h_std', 'lambda_c', 'n_buildings'}
    
    # Extract first level column names
    col_names = {col[0] if isinstance(col, tuple) else col for col in df.columns}
    
    # Check version
    has_old = any(col in col_names for col in old_indicators)
    has_new = all(col in col_names for col in new_indicators)
    
    if has_old:
        logger.info("Detected old df_state format (pre-2025)")
        return 'old'
    elif has_new:
        logger.info("Detected new df_state format (2025+)")
        return 'new'
    else:
        # Check if it's partially new (has some but not all new columns)
        has_some_new = any(col in col_names for col in new_indicators)
        if has_some_new and not has_old:
            logger.info("Detected current df_state format")
            return 'current'
        
        logger.warning("Unknown df_state format - will attempt conversion")
        return 'unknown'


def convert_df_state_format(df_old: pd.DataFrame) -> pd.DataFrame:
    """Convert old df_state format to new format.
    
    Handles migration from pre-2025 format to current format by:
    - Removing deprecated columns (age_0_4, age_5_11, etc.)
    - Adding new required columns (buildingname, buildingtype, etc.)
    - Preserving all common columns
    - Using sensible defaults for new columns
    
    Args:
        df_old: DataFrame in old df_state format
        
    Returns:
        DataFrame in current df_state format
    """
    import supy as sp
    
    logger.info("Converting df_state to current format...")
    
    # Get template for current version
    logger.info("Loading current format template...")
    df_template, _ = sp.load_sample_data()
    
    # Create new DataFrame with correct structure
    df_new = pd.DataFrame(index=df_old.index, columns=df_template.columns)
    if hasattr(df_template.index, 'name'):
        df_new.index.name = df_template.index.name
    
    # Copy common columns
    common_cols = set(df_old.columns) & set(df_template.columns)
    logger.info(f"Copying {len(common_cols)} common columns...")
    
    for col in common_cols:
        try:
            # Handle both single and multi-row DataFrames
            if len(df_old) == 1:
                df_new[col] = df_old[col].values[0]
            else:
                df_new[col] = df_old[col].values
        except Exception as e:
            logger.warning(f"Failed to copy column {col}: {e}, using template default")
            df_new[col] = df_template[col].iloc[0]
    
    # Add new columns with appropriate defaults
    new_cols = set(df_template.columns) - set(df_old.columns)
    logger.info(f"Adding {len(new_cols)} new columns with defaults...")
    
    for col in new_cols:
        template_value = df_template[col].iloc[0]
        col_name = col[0] if isinstance(col, tuple) else col
        
        # Smart defaults based on column name
        if 'buildingname' in str(col_name).lower():
            df_new[col] = 'building_1'
        elif 'buildingtype' in str(col_name).lower():
            df_new[col] = 'residential'
        elif 'description' in str(col_name).lower():
            df_new[col] = 'Converted from previous df_state format'
        elif 'config' in str(col_name).lower():
            df_new[col] = 'Converted from previous df_state format'
        else:
            # Use template default
            df_new[col] = template_value
    
    # Ensure data types match template
    logger.info("Aligning data types...")
    for col in df_new.columns:
        try:
            target_dtype = df_template[col].dtype
            if df_new[col].dtype != target_dtype:
                df_new[col] = df_new[col].astype(target_dtype)
        except (ValueError, TypeError):
            pass  # Keep original dtype if conversion fails
    
    # Log what was changed
    removed_cols = set(df_old.columns) - set(df_template.columns)
    if removed_cols:
        # Extract column names for logging
        removed_names = []
        for col in list(removed_cols)[:10]:  # Show first 10
            if isinstance(col, tuple):
                removed_names.append(f"{col[0]}[{col[1]}]")
            else:
                removed_names.append(str(col))
        
        logger.info(f"Removed {len(removed_cols)} deprecated columns including: {', '.join(removed_names)}")
        if len(removed_cols) > 10:
            logger.info(f"  ... and {len(removed_cols) - 10} more")
    
    logger.info("Conversion complete")
    return df_new


def validate_converted_df_state(df_state: pd.DataFrame) -> Tuple[bool, str]:
    """Validate converted df_state using SuPy's check_state.
    
    Args:
        df_state: DataFrame to validate
        
    Returns:
        Tuple of (is_valid, message)
    """
    try:
        import supy as sp
        sp.check_state(df_state)
        return True, "Validation passed"
    except Exception as e:
        return False, f"Validation warning: {str(e)}"