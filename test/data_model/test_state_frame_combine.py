"""State assembly preserves pandas precedence, missingness and dtypes."""

import numpy as np
import pandas as pd
import pytest

from supy.data_model.core.type import combine_df_states

pytestmark = [pytest.mark.api, pytest.mark.cfg]


@pytest.mark.parametrize("dtype", ["float64", "object", "Int64", "Float64"])
@pytest.mark.parametrize("overlap", [True, False])
@pytest.mark.parametrize("aligned", [True, False])
def test_combine_matches_pandas_for_nulls_and_conflicts(dtype, overlap, aligned):
    """Independent pandas operation defines fill precedence and dtype resolution."""
    left = pd.DataFrame({
        ("gridiv", "0"): [1, 2, 3],
        ("z", "0"): pd.array([3, None, 0], dtype=dtype),
        ("a", "0"): pd.array([None, None, None], dtype=dtype),
    })
    right = pd.DataFrame({
        ("gridiv", "0"): [7, 8, 9],
        ("z" if overlap else "b", "0"): pd.array([8, 5, None], dtype=dtype),
        ("c", "0"): [False, True, False],
        ("s", "0"): ["x", None, "y"],
    })
    left.index = pd.Index([11, 12, 13], name="grid")
    right.index = pd.Index([11, 12, 13] if aligned else [12, 13, 14], name="grid")
    left.columns.names = right.columns.names = ["var", "ind_dim"]
    before_left, before_right = left.copy(deep=True), right.copy(deep=True)
    expected = left.combine_first(right)
    actual = combine_df_states(left, right)
    pd.testing.assert_frame_equal(actual, expected)
    pd.testing.assert_frame_equal(left, before_left)
    pd.testing.assert_frame_equal(right, before_right)
    actual.iloc[0, 0] = 99
    pd.testing.assert_frame_equal(left, before_left)
    pd.testing.assert_frame_equal(right, before_right)


@pytest.mark.parametrize("empty_side", ["left", "right", "neither"])
def test_combine_literal_values_and_empty_frames(empty_side):
    """Zero stays zero, missing fills from the right, and left values win."""
    left = pd.DataFrame({("x", "0"): [0.0, np.nan, 3.0]})
    right = pd.DataFrame({("x", "0"): [8.0, 5.0, 9.0]})
    if empty_side == "left":
        left = left.iloc[:0]
    elif empty_side == "right":
        right = right.iloc[:0]
    actual = combine_df_states(left, right)
    pd.testing.assert_frame_equal(actual, left.combine_first(right))
    if empty_side == "neither":
        assert actual["x", "0"].tolist() == [0.0, 5.0, 3.0]
