"""Lightweight metric helpers for SUEWS run / observation comparison.

These are NaN-aware wrappers around numpy / pandas primitives. Public API:

- ``rmse(y_true, y_pred) -> float``
- ``bias(y_true, y_pred) -> float`` (signed, ``mean(y_pred - y_true)``)
- ``pearson_r(y_true, y_pred) -> float``

Each function aligns the inputs by their pandas index when both are
``pd.Series`` (or coerces them to ``pd.Series``), drops pairs where either
member is NaN, and raises ``ValueError`` if fewer than 3 valid pairs remain.
The minimum-pair guard avoids silently returning a metric computed from
two-or-fewer points (Pearson r is undefined for n<2 and unreliable for n<3).

Pure dependency-free in the supy build (numpy / pandas already required).
"""

from __future__ import annotations

import numpy as np
import pandas as pd

__all__ = ["bias", "pearson_r", "rmse"]

_MIN_VALID_PAIRS = 3


def _coerce_pair(
    y_true: pd.Series | np.ndarray | list,
    y_pred: pd.Series | np.ndarray | list,
) -> tuple[np.ndarray, np.ndarray]:
    """Align inputs and drop pairs where either side is NaN.

    Parameters
    ----------
    y_true : pd.Series, np.ndarray, or list
        Reference / observed values.
    y_pred : pd.Series, np.ndarray, or list
        Modelled / scenario values.

    Returns
    -------
    tuple of np.ndarray
        ``(arr_true, arr_pred)`` with NaN pairs dropped.

    Raises
    ------
    ValueError
        If fewer than 3 finite pairs remain after alignment.
    """
    ser_true = y_true if isinstance(y_true, pd.Series) else pd.Series(y_true)
    ser_pred = y_pred if isinstance(y_pred, pd.Series) else pd.Series(y_pred)
    df_pair = pd.concat({"a": ser_true, "b": ser_pred}, axis=1, join="inner")
    df_pair = df_pair.replace([np.inf, -np.inf], np.nan).dropna()
    if len(df_pair) < _MIN_VALID_PAIRS:
        raise ValueError(
            f"Need at least {_MIN_VALID_PAIRS} non-NaN aligned pairs for a "
            f"metric; got {len(df_pair)}."
        )
    return df_pair["a"].to_numpy(dtype=float), df_pair["b"].to_numpy(dtype=float)


def rmse(
    y_true: pd.Series | np.ndarray | list,
    y_pred: pd.Series | np.ndarray | list,
) -> float:
    """Root mean square error between two aligned series.

    NaN pairs are dropped before computation.
    """
    arr_true, arr_pred = _coerce_pair(y_true, y_pred)
    return float(np.sqrt(np.mean((arr_pred - arr_true) ** 2)))


def bias(
    y_true: pd.Series | np.ndarray | list,
    y_pred: pd.Series | np.ndarray | list,
) -> float:
    """Signed mean bias (``mean(y_pred - y_true)``).

    NaN pairs are dropped before computation.
    """
    arr_true, arr_pred = _coerce_pair(y_true, y_pred)
    return float(np.mean(arr_pred - arr_true))


def pearson_r(
    y_true: pd.Series | np.ndarray | list,
    y_pred: pd.Series | np.ndarray | list,
) -> float:
    """Pearson correlation coefficient between two aligned series.

    NaN pairs are dropped before computation. Returns ``nan`` if either
    series has zero variance after pairing — undefined correlation rather
    than a misleading number.
    """
    arr_true, arr_pred = _coerce_pair(y_true, y_pred)
    # Exact-zero variance is the test we want here — corrcoef returns nan
    # in that case anyway and we want to short-circuit deterministically.
    if np.std(arr_true) == 0.0 or np.std(arr_pred) == 0.0:  # noqa: RUF069
        return float("nan")
    matrix = np.corrcoef(arr_true, arr_pred)
    return float(matrix[0, 1])
