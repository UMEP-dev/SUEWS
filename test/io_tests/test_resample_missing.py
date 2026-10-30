"""Missing accumulated output stays missing across resampling boundaries."""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSOutput
from supy.data_model.output import OUTPUT_REGISTRY

pytestmark = pytest.mark.api


def _output_frame(rain):
    """Build two grids with registered SUEWS columns and prescribed rainfall."""
    dates = pd.date_range("2024-01-01 00:05", periods=len(rain), freq="5min")
    index = pd.MultiIndex.from_product([[1, 2], dates], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_product(
        [["SUEWS"], OUTPUT_REGISTRY.get_aggregation_rules()["SUEWS"]],
        names=["group", "var"],
    )
    df_output = pd.DataFrame(1.0, index=index, columns=columns)
    df_output["SUEWS", "Rain"] = np.tile(rain, 2)
    return df_output


@pytest.mark.parametrize("tail_rows", [1, 2])
def test_short_chunks_preserve_missing_sums(tail_rows):
    """One- and two-row tails match whole output, including missing rainfall."""
    # ARRANGE: the final chunk contains only missing rainfall, but valid fluxes.
    df_output = _output_frame([0.0, 2.0, np.nan] + [np.nan] * tail_rows)
    dates = df_output.index.get_level_values("datetime")
    boundary = dates.unique()[2]
    df_state = pd.DataFrame()

    # ACT: resample the same data whole and in chunks through the public API.
    df_whole = SUEWSOutput(df_output, df_state).resample("5min").df
    df_chunked = pd.concat([
        SUEWSOutput(df_chunk, df_state).resample("5min").df
        for df_chunk in (
            df_output.loc[dates <= boundary],
            df_output.loc[dates > boundary],
        )
    ]).sort_index()

    # ASSERT: missing values, genuine zeroes and non-zero sums all survive.
    pd.testing.assert_frame_equal(df_whole, df_output)
    pd.testing.assert_frame_equal(df_chunked, df_whole)


def test_downsampling_preserves_missing_sums_and_valid_zeroes():
    """All-missing bins stay NaN; mixed bins still sum their observed values."""
    # ARRANGE: three bins hold only NaN, a real zero, and two observed values.
    df_output = _output_frame([np.nan] * 3 + [np.nan, 0.0, np.nan, 1.0, np.nan, 2.0])

    # ACT: use a coarser frequency so the engine must aggregate each bin.
    df_result = SUEWSOutput(df_output, pd.DataFrame()).resample("15min").df

    # ASSERT: both grids retain missingness without changing valid aggregation.
    for grid in (1, 2):
        df_grid = df_result.xs(grid, level="grid")
        np.testing.assert_allclose(df_grid["SUEWS", "Rain"], [np.nan, 0.0, 3.0])
        np.testing.assert_allclose(df_grid["SUEWS", "QH"], [1.0, 1.0, 1.0])
