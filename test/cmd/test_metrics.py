"""Unit tests for ``supy.metrics``."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from supy.metrics import bias, pearson_r, rmse

pytestmark = pytest.mark.api


def test_rmse_identity_zero() -> None:
    """RMSE of a series against itself is zero."""
    ser = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
    assert rmse(ser, ser) == pytest.approx(0.0, abs=1e-12)


def test_bias_identity_zero() -> None:
    """Mean bias of a series against itself is zero."""
    ser = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
    assert bias(ser, ser) == pytest.approx(0.0, abs=1e-12)


def test_pearson_r_identity_one() -> None:
    """Pearson r of a non-constant series against itself is 1."""
    ser = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
    assert pearson_r(ser, ser) == pytest.approx(1.0, abs=1e-12)


def test_rmse_known_value() -> None:
    """RMSE of a constant offset matches the offset magnitude."""
    arr_a = np.array([0.0, 0.0, 0.0, 0.0, 0.0])
    arr_b = np.array([2.0, 2.0, 2.0, 2.0, 2.0])
    assert rmse(arr_a, arr_b) == pytest.approx(2.0, abs=1e-12)


def test_bias_known_value() -> None:
    """Bias is signed; ``y_pred - y_true`` mean."""
    arr_a = np.array([0.0, 0.0, 0.0, 0.0])
    arr_b = np.array([1.0, 2.0, 3.0, 4.0])
    assert bias(arr_a, arr_b) == pytest.approx(2.5, abs=1e-12)


def test_nan_pairs_dropped() -> None:
    """NaN pairs are discarded before metric computation."""
    arr_a = np.array([1.0, np.nan, 3.0, 4.0, 5.0])
    arr_b = np.array([1.0, 2.0, 3.0, np.nan, 5.0])
    # Valid pairs: (1,1), (3,3), (5,5) -> rmse 0, bias 0.
    assert rmse(arr_a, arr_b) == pytest.approx(0.0, abs=1e-12)
    assert bias(arr_a, arr_b) == pytest.approx(0.0, abs=1e-12)


def test_too_few_pairs_raises() -> None:
    """Fewer than 3 valid pairs must raise ValueError."""
    arr_a = np.array([1.0, np.nan, np.nan, np.nan])
    arr_b = np.array([1.0, 2.0, 3.0, 4.0])
    with pytest.raises(ValueError, match="at least 3"):
        rmse(arr_a, arr_b)


def test_pearson_r_zero_variance_returns_nan() -> None:
    """Pearson r with a flat predictor is undefined; return nan."""
    arr_a = np.array([1.0, 2.0, 3.0, 4.0])
    arr_b = np.array([5.0, 5.0, 5.0, 5.0])
    result = pearson_r(arr_a, arr_b)
    assert np.isnan(result)


def test_index_alignment_for_pandas_series() -> None:
    """Pandas series are joined on the inner intersection of their indices."""
    ser_a = pd.Series([1.0, 2.0, 3.0], index=[0, 1, 2])
    ser_b = pd.Series([1.0, 2.0, 3.0], index=[1, 2, 3])
    # Inner join keeps indices 1 and 2 only -> 2 pairs, below the floor.
    with pytest.raises(ValueError, match="at least 3"):
        rmse(ser_a, ser_b)
