"""
Helper functions for attribution analysis.

Provides utility functions for extracting and validating SUEWS output data.
"""

from typing import Literal, Optional
import warnings

import numpy as np
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
    # Unwrap SUEWSOutput to underlying DataFrame
    if hasattr(df_output, "_df_output"):
        df_output = df_output._df_output

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


def unwrap_forcing(df_forcing) -> pd.DataFrame:
    """Unwrap SUEWSForcing-like objects to underlying DataFrame if needed."""
    if isinstance(df_forcing, pd.DataFrame):
        return df_forcing

    data = getattr(df_forcing, "_data", None)
    if isinstance(data, pd.DataFrame):
        return data

    return df_forcing


def align_scenarios(
    df_A: pd.DataFrame,
    df_B: pd.DataFrame,
    stacklevel: int = 3,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.Index]:
    """
    Align two scenario DataFrames on their common index.

    Parameters
    ----------
    df_A : pd.DataFrame
        Scenario A DataFrame.
    df_B : pd.DataFrame
        Scenario B DataFrame.
    stacklevel : int, optional
        Warning stacklevel for alignment data-loss warning, by default 3.

    Returns
    -------
    tuple
        (df_A_aligned, df_B_aligned, common_idx)

    Raises
    ------
    ValueError
        If scenarios have no overlapping timestamps.
    """
    n_A_original = len(df_A.index)
    n_B_original = len(df_B.index)
    common_idx = df_A.index.intersection(df_B.index)

    if len(common_idx) == 0:
        raise ValueError("No overlapping timestamps between scenarios A and B")

    pct_A_kept = 100 * len(common_idx) / n_A_original
    pct_B_kept = 100 * len(common_idx) / n_B_original
    if pct_A_kept < 90 or pct_B_kept < 90:
        warnings.warn(
            f"Significant data loss during alignment: keeping {pct_A_kept:.1f}% of "
            f"scenario A ({len(common_idx)}/{n_A_original}), {pct_B_kept:.1f}% of "
            f"scenario B ({len(common_idx)}/{n_B_original}). "
            "Check that time indices match between scenarios.",
            UserWarning,
            stacklevel=stacklevel,
        )

    return df_A.loc[common_idx], df_B.loc[common_idx], common_idx


def _group_means(
    df_A: pd.DataFrame,
    df_B: pd.DataFrame,
    columns: list[str],
) -> dict[str, tuple[np.ndarray, np.ndarray]]:
    """
    Extract group means as 1-element arrays for Shapley inputs.

    Parameters
    ----------
    df_A, df_B : pd.DataFrame
        DataFrames representing state/group A and B.
    columns : list of str
        Column names to average in both groups.

    Returns
    -------
    dict
        Mapping of column name to (A_mean, B_mean) tuple, where each
        mean is a shape-(1,) numpy array.
    """
    return {
        col: (np.array([df_A[col].mean()]), np.array([df_B[col].mean()]))
        for col in columns
    }


def detect_anomalies(
    series: pd.Series,
    method: Literal["anomaly", "extreme", "diurnal"],
    threshold: float = 2.0,
    min_count: int = 10,
) -> tuple[pd.Series, pd.Series]:
    """
    Classify timesteps into anomaly and normal masks.

    Parameters
    ----------
    series : pd.Series
        Input variable time series.
    method : {"anomaly", "extreme", "diurnal"}
        Detection method.
    threshold : float, optional
        Standard deviation threshold for anomaly detection, by default 2.0.
    min_count : int, optional
        Minimum number of timesteps required in each class, by default 10.

    Returns
    -------
    tuple
        (anomaly_mask, normal_mask)
    """
    if method == "anomaly":
        daily_mean = series.resample("D").transform("mean")
        daily_std = series.resample("D").transform("std")
        daily_std = daily_std.replace(0, np.nan)
        z_score = (series - daily_mean) / daily_std

        anomaly_mask = abs(z_score) > threshold
        normal_mask = abs(z_score) <= 1.0
    elif method == "extreme":
        q05 = series.quantile(0.05)
        q25 = series.quantile(0.25)
        q75 = series.quantile(0.75)
        q95 = series.quantile(0.95)

        anomaly_mask = (series <= q05) | (series >= q95)
        normal_mask = (series >= q25) & (series <= q75)
    elif method == "diurnal":
        hour = pd.Series(series.index.hour, index=series.index)
        anomaly_mask = (hour >= 12) & (hour <= 15)
        normal_mask = (hour >= 6) & (hour <= 10)
    else:
        raise ValueError(
            f"Unknown method: {method}. Use 'anomaly', 'extreme', 'diurnal'"
        )

    n_anomaly = anomaly_mask.sum()
    n_normal = normal_mask.sum()

    if n_anomaly < min_count:
        raise ValueError(
            f"Only {n_anomaly} anomalous timesteps found. "
            f"Try lowering threshold (current: {threshold})"
        )
    if n_normal < min_count:
        raise ValueError(
            f"Only {n_normal} reference timesteps found. Cannot establish baseline."
        )

    return anomaly_mask, normal_mask
