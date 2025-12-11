"""
Helper functions for attribution analysis.

Provides utility functions for extracting and validating SUEWS output data.
"""

from typing import Optional

import pandas as pd


def extract_suews_group(
    df_output: pd.DataFrame,
    required_cols: Optional[set[str]] = None,
) -> pd.DataFrame:
    """
    Extract SUEWS output group from MultiIndex DataFrame.

    Handles both MultiIndex column structure and flat DataFrames.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame, possibly with MultiIndex columns
    required_cols : set of str, optional
        Columns that must be present after extraction. If None, no validation.

    Returns
    -------
    pd.DataFrame
        Extracted DataFrame with flat column structure

    Raises
    ------
    ValueError
        If required columns are missing from the extracted DataFrame
    """
    # Check if MultiIndex columns
    if isinstance(df_output.columns, pd.MultiIndex):
        # Try to get SUEWS group
        if "SUEWS" in df_output.columns.get_level_values(0):
            # Handle MultiIndex rows (grid, datetime)
            if isinstance(df_output.index, pd.MultiIndex):
                # Get first grid
                grid = df_output.index.get_level_values(0)[0]
                df = df_output.loc[grid, "SUEWS"]
            else:
                df = df_output["SUEWS"]
        else:
            # Return first level
            df = df_output.droplevel(0, axis=1)
    else:
        # Already flat DataFrame
        df = df_output

    # Validate required columns if specified
    if required_cols is not None:
        missing = required_cols - set(df.columns)
        if missing:
            available = list(df.columns)[:10]
            raise ValueError(
                f"Required columns missing from SUEWS output: {missing}. "
                f"Available columns (first 10): {available}"
            )

    return df
