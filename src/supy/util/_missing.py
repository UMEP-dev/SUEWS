"""Utilities for handling SUEWS missing values."""

import pandas as pd
import numpy as np

# SUEWS missing value indicator emitted by SuPy / forcing IO.
SUEWS_MISSING = -999.0

# Defensive threshold matching the Fortran runtime convention (e.g. the
# observed-LAI override in ``update_GDDLAI``): any value at or below this
# sentinel is treated as "missing". This tolerates floating-point drift in
# user-supplied forcing files (-999.0001, -950, etc.) while still accepting
# genuine physical values such as Tair = -60 C or LAI = 0.
SUEWS_MISSING_THRESHOLD = -900.0


def to_nan(data):
    """Convert SUEWS missing values to NaN.

    Any value at or below ``SUEWS_MISSING_THRESHOLD`` (-900) is treated as
    missing, matching the Fortran runtime's defensive sentinel handling.

    Parameters
    ----------
    data : pd.DataFrame or pd.Series
        Data containing SUEWS missing value indicators.

    Returns
    -------
    pd.DataFrame or pd.Series
        Data with missing sentinels replaced by NaN.
    """
    return data.mask(data <= SUEWS_MISSING_THRESHOLD, np.nan)


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
