"""Utilities for handling SUEWS missing values."""

import pandas as pd
import numpy as np

# SUEWS missing value indicator
SUEWS_MISSING = -999.0


def to_nan(data):
    """Convert SUEWS missing values to NaN.

    Parameters
    ----------
    data : pd.DataFrame or pd.Series
        Data containing SUEWS missing value indicators (-999)

    Returns
    -------
    pd.DataFrame or pd.Series
        Data with -999 replaced by NaN
    """
    return data.mask(data == SUEWS_MISSING, np.nan)


def from_nan(data):
    """Convert NaN to SUEWS missing values.

    Parameters
    ----------
    data : pd.DataFrame or pd.Series
        Data containing NaN values

    Returns
    -------
    pd.DataFrame or pd.Series
        Data with NaN replaced by -999
    """
    return data.fillna(SUEWS_MISSING)
