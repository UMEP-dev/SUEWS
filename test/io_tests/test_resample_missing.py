"""Missing output values stay missing across sum, mean, last, and DailyState resampling."""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSOutput
from supy.data_model.output import OUTPUT_REGISTRY

pytestmark = [pytest.mark.api, pytest.mark.contract("C-RESAMPLE")]


def _output_frame(rain, *, qh=None, state=None):
    """Build two grids with varying registered SUEWS columns and prescribed test series.

    Grid 2 scales Grid 1 by 2.0 so genuine zeroes (0.0) and missing values
    (NaN) remain identical across grids while every non-zero value differs
    between grids and across timesteps.
    """
    n_rows = len(rain)
    dates = pd.date_range("2024-01-01 00:05", periods=n_rows, freq="5min")
    index = pd.MultiIndex.from_product([[1, 2], dates], names=["grid", "datetime"])
    suews_vars = list(OUTPUT_REGISTRY.get_aggregation_rules()["SUEWS"])
    columns = pd.MultiIndex.from_product(
        [["SUEWS"], suews_vars], names=["group", "var"]
    )

    # Fill background columns with non-constant grid- and time-varying values
    # so no variable can pass a resampling check merely by staying constant.
    base_ramp = np.arange(1.0, n_rows + 1.0, dtype=float)
    grid1_bg = np.column_stack([
        base_ramp + 0.1 * col_idx for col_idx in range(len(suews_vars))
    ])
    grid2_bg = grid1_bg * 2.0
    df_output = pd.DataFrame(
        np.vstack([grid1_bg, grid2_bg]),
        index=index,
        columns=columns,
    )

    arr_rain = np.asarray(rain, dtype=float)
    df_output["SUEWS", "Rain"] = np.concatenate([arr_rain, arr_rain * 2.0])

    if qh is not None:
        arr_qh = np.asarray(qh, dtype=float)
        df_output["SUEWS", "QH"] = np.concatenate([arr_qh, arr_qh * 2.0])

    if state is not None:
        arr_state = np.asarray(state, dtype=float)
        df_output["SUEWS", "State"] = np.concatenate([arr_state, arr_state * 2.0])

    return df_output


def _dailystate_frame(p_day_by_day, hdd_tmean_by_day, *, include_suews=True):
    """Build a multi-day hourly frame with sparse end-of-day DailyState entries.

    Intra-day rows hold NaN in DailyState; only the final step of each day
    (23:00) carries the prescribed daily summary values. Grid 2 scales Grid 1
    by 2.0 to keep genuine zeroes and NaNs aligned while separating grids.
    """
    n_days = len(p_day_by_day)
    assert len(hdd_tmean_by_day) == n_days
    dates = pd.date_range("2024-01-01 01:00", periods=n_days * 24, freq="1h")
    index = pd.MultiIndex.from_product([[1, 2], dates], names=["grid", "datetime"])

    cols = []
    if include_suews:
        cols.extend([("SUEWS", "Rain"), ("SUEWS", "QH"), ("SUEWS", "State")])
    cols.extend([("DailyState", "P_day"), ("DailyState", "HDD3_Tmean")])
    columns = pd.MultiIndex.from_tuples(cols, names=["group", "var"])

    n_per_grid = len(dates)
    df = pd.DataFrame(np.nan, index=index, columns=columns, dtype=float)

    if include_suews:
        ramp = np.arange(1.0, n_per_grid + 1.0, dtype=float)
        df["SUEWS", "Rain"] = np.concatenate([
            np.full(n_per_grid, 0.25),
            np.full(n_per_grid, 0.5),
        ])
        df["SUEWS", "QH"] = np.concatenate([ramp, ramp * 2.0])
        df["SUEWS", "State"] = np.concatenate([ramp * 0.5, ramp])

    # Place DailyState records at 23:00 of each simulated day (hours index 22, 46, ...).
    daily_positions = [day * 24 + 22 for day in range(n_days)]
    p_day_series = np.full(n_per_grid, np.nan, dtype=float)
    hdd_series = np.full(n_per_grid, np.nan, dtype=float)
    for pos, p_val, hdd_val in zip(daily_positions, p_day_by_day, hdd_tmean_by_day):
        p_day_series[pos] = p_val
        hdd_series[pos] = hdd_val

    df["DailyState", "P_day"] = np.concatenate([p_day_series, p_day_series * 2.0])
    df["DailyState", "HDD3_Tmean"] = np.concatenate([hdd_series, hdd_series * 2.0])
    return df


@pytest.mark.parametrize("tail_rows", [1, 2])
def test_short_chunks_preserve_missing_values_across_aggregation_rules(tail_rows):
    """One- and two-row tails match whole output for sum, mean, and last variables."""
    # ARRANGE: vary Rain (sum), QH (mean), and State (last) across rows and
    # grids, with missing values in both the prefix and the short tail chunk.
    prefix_rain = [0.0, 2.0, np.nan]
    prefix_qh = [12.0, np.nan, 34.0]
    prefix_state = [1.5, 4.0, np.nan]

    tail_rain = [np.nan] * tail_rows
    tail_qh = [np.nan] * tail_rows
    tail_state = [np.nan] * tail_rows

    df_output = _output_frame(
        prefix_rain + tail_rain,
        qh=prefix_qh + tail_qh,
        state=prefix_state + tail_state,
    )
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

    # ASSERT: missing values, genuine zeroes, and varying observations survive
    # identically across whole and chunked paths.
    pd.testing.assert_frame_equal(df_whole, df_output)
    pd.testing.assert_frame_equal(df_chunked, df_whole)


def test_downsampling_preserves_missing_sums_means_and_last_values():
    """Coarser resampling obeys missingness contracts for sum, mean, and last rules."""
    # ARRANGE: 15 five-minute steps -> 5 fifteen-minute bins (3 rows per bin).
    # Each rule exercises all-NaN, genuine-zero, partial-NaN, and non-constant bins.
    rain = [
        np.nan,
        np.nan,
        np.nan,  # bin 0: all-NaN -> NaN (never 0.0)
        np.nan,
        0.0,
        np.nan,  # bin 1: genuine zero survives -> 0.0
        1.0,
        np.nan,
        2.0,  # bin 2: partial-NaN sum -> 3.0
        1.5,
        2.5,
        4.0,  # bin 3: varying full bin -> 8.0
        0.0,
        0.0,
        np.nan,  # bin 4: multiple zeroes + NaN -> 0.0
    ]
    qh = [
        10.0,
        20.0,
        60.0,  # bin 0: mean=30.0 (distinct from first/last/median/sum)
        10.0,
        np.nan,
        20.0,  # bin 1: partial-NaN mean=15.0 (fails if NaN coerced to 0 -> 10.0)
        np.nan,
        np.nan,
        np.nan,  # bin 2: all-NaN -> NaN (never 0.0)
        np.nan,
        0.0,
        np.nan,  # bin 3: genuine zero survives -> 0.0
        -15.0,
        np.nan,
        45.0,  # bin 4: signed mean=15.0
    ]
    state = [
        1.0,
        5.0,
        9.0,  # bin 0: last=9.0 (distinct from first/mean/sum)
        1.0,
        5.0,
        np.nan,  # bin 1: missing endpoint stays NaN (no backfill of 5.0)
        np.nan,
        2.0,
        7.0,  # bin 2: leading NaN does not poison valid endpoint -> 7.0
        np.nan,
        np.nan,
        np.nan,  # bin 3: all-NaN -> NaN
        4.0,
        np.nan,
        0.0,  # bin 4: genuine zero endpoint survives -> 0.0
    ]
    df_output = _output_frame(rain, qh=qh, state=state)

    # ACT: downsample to 15-minute bins through the public API.
    df_result = SUEWSOutput(df_output, pd.DataFrame()).resample("15min").df

    # ASSERT: verify both grids with explicit NaN vs genuine-zero distinction.
    expected_g1 = {
        "Rain": np.array([np.nan, 0.0, 3.0, 8.0, 0.0]),
        "QH": np.array([30.0, 15.0, np.nan, 0.0, 15.0]),
        "State": np.array([9.0, np.nan, 7.0, np.nan, 0.0]),
    }
    for grid, scale in ((1, 1.0), (2, 2.0)):
        df_grid = df_result.xs(grid, level="grid")
        for var, exp in expected_g1.items():
            actual = df_grid["SUEWS", var].to_numpy()
            expected = exp * scale
            np.testing.assert_array_equal(np.isnan(actual), np.isnan(expected))
            np.testing.assert_allclose(actual, expected, equal_nan=True)


def test_dailystate_resampling_preserves_missing_sums_and_last_states():
    """DailyState keeps all-missing P_day as NaN and propagates partial-day LAST NaNs."""
    # ARRANGE: 10 days -> 5 two-day bins (2024-01-03, 01-05, 01-07, 01-09, 01-11).
    # - Bin 0 (Days 1-2): P_day all-NaN -> NaN (never 0.0); HDD3_Tmean -> last (14.0).
    # - Bin 1 (Days 3-4): Day 3 has genuine P_day=0.0; Day 4 is all-NaN and dropped
    #   by dropna(how="all"), leaving Day 3's genuine 0.0 and 16.0.
    # - Bin 2 (Days 5-6): Day 6 is partial-NaN (P_day=2.5, HDD3_Tmean=NaN), so it
    #   survives dropna(how="all"): P_day sums to 4.0 and LAST HDD3_Tmean is NaN
    #   (not backfilled with Day 5's 18.0).
    # - Bin 3 (Days 7-8): interior all-NaN DailyState gap -> NaN (never 0.0).
    # - Bin 4 (Days 9-10): mixed NaN + 3.5 -> P_day=3.5; HDD3_Tmean -> last (26.0).
    p_day = [
        np.nan,
        np.nan,
        0.0,
        np.nan,
        1.5,
        2.5,
        np.nan,
        np.nan,
        np.nan,
        3.5,
    ]
    hdd_tmean = [
        12.0,
        14.0,
        16.0,
        np.nan,
        18.0,
        np.nan,
        np.nan,
        np.nan,
        20.0,
        26.0,
    ]
    df_output = _dailystate_frame(p_day, hdd_tmean, include_suews=True)

    # ACT: resample through the public API to 1-day and 2-day cadences.
    df_daily = SUEWSOutput(df_output, pd.DataFrame()).resample("D").df
    df_2d = SUEWSOutput(df_output, pd.DataFrame()).resample("2D").df

    # ASSERT (1-day): partial-NaN days survive; all-missing P_day stays NaN.
    # Day D is labelled D+1 00:00 (closed="right", label="right").
    daily_dates = pd.date_range("2024-01-02", periods=10, freq="D")
    for grid, scale in ((1, 1.0), (2, 2.0)):
        ds_daily = df_daily.xs(grid, level="grid")["DailyState"].reindex(daily_dates)
        expected_p = np.asarray(p_day, dtype=float) * scale
        expected_hdd = np.asarray(hdd_tmean, dtype=float) * scale
        np.testing.assert_array_equal(
            np.isnan(ds_daily["P_day"].to_numpy()), np.isnan(expected_p)
        )
        np.testing.assert_allclose(
            ds_daily["P_day"].to_numpy(), expected_p, equal_nan=True
        )
        np.testing.assert_array_equal(
            np.isnan(ds_daily["HDD3_Tmean"].to_numpy()), np.isnan(expected_hdd)
        )
        np.testing.assert_allclose(
            ds_daily["HDD3_Tmean"].to_numpy(), expected_hdd, equal_nan=True
        )

    # ASSERT (2-day): all-missing sums, genuine zeroes, trailing LAST NaNs, and
    # interior empty gaps all match the expected missingness contract.
    bin_dates = pd.date_range("2024-01-03", periods=5, freq="2D")
    expected_2d_p = np.array([np.nan, 0.0, 4.0, np.nan, 3.5])
    expected_2d_hdd = np.array([14.0, 16.0, np.nan, np.nan, 26.0])
    for grid, scale in ((1, 1.0), (2, 2.0)):
        ds_2d = df_2d.xs(grid, level="grid")["DailyState"].reindex(bin_dates)
        exp_p = expected_2d_p * scale
        exp_hdd = expected_2d_hdd * scale
        np.testing.assert_array_equal(
            np.isnan(ds_2d["P_day"].to_numpy()), np.isnan(exp_p)
        )
        np.testing.assert_allclose(ds_2d["P_day"].to_numpy(), exp_p, equal_nan=True)
        np.testing.assert_array_equal(
            np.isnan(ds_2d["HDD3_Tmean"].to_numpy()), np.isnan(exp_hdd)
        )
        np.testing.assert_allclose(
            ds_2d["HDD3_Tmean"].to_numpy(), exp_hdd, equal_nan=True
        )


def test_all_missing_dailystate_never_fabricates_zeroes():
    """All-NaN DailyState produces empty DailyState output rather than 0.0 sums."""
    # Case 1: DailyState alone (all NaN) drops cleanly to an empty frame.
    df_only_ds = _dailystate_frame(
        [np.nan, np.nan], [np.nan, np.nan], include_suews=False
    )
    df_only_ds_resampled = SUEWSOutput(df_only_ds, pd.DataFrame()).resample("D").df
    assert df_only_ds_resampled.empty

    # Case 2: Combined with valid SUEWS data, DailyState columns remain all-NaN.
    df_combined = _dailystate_frame(
        [np.nan, np.nan], [np.nan, np.nan], include_suews=True
    )
    df_combined_resampled = SUEWSOutput(df_combined, pd.DataFrame()).resample("D").df
    assert not df_combined_resampled.empty
    assert df_combined_resampled["DailyState", "P_day"].isna().all()
    assert df_combined_resampled["DailyState", "HDD3_Tmean"].isna().all()


def test_resample_contract_diverges_from_naive_pandas_sum_and_last():
    """Public SUEWSOutput.resample avoids naive pandas sum-to-zero and last backfill."""
    # ARRANGE: one all-NaN bin and one partial-NaN bin with missing endpoint.
    df_output = _output_frame(
        [np.nan, np.nan, np.nan, 1.0, np.nan, 2.0],
        qh=[np.nan, np.nan, np.nan, 10.0, np.nan, 20.0],
        state=[np.nan, np.nan, np.nan, 4.0, 8.0, np.nan],
    )
    df_g1 = df_output.xs(1, level="grid")["SUEWS"]
    naive_resampler = df_g1.resample("15min", closed="right", label="right")

    # ACT: resample through SUEWS's public API.
    df_resampled = (
        SUEWSOutput(df_output, pd.DataFrame())
        .resample("15min")
        .df.xs(1, level="grid")["SUEWS"]
    )

    # ASSERT:
    # 1. Naive pandas .sum() collapses an all-NaN bin to 0.0; SUEWS keeps it NaN.
    assert naive_resampler["Rain"].sum().iloc[0] == pytest.approx(0.0, abs=0.0)
    assert np.isnan(df_resampled["Rain"].iloc[0])
    assert df_resampled["Rain"].iloc[1] == pytest.approx(3.0)

    # 2. Mean stays NaN on all-NaN and averages only observed values on partial-NaN.
    assert np.isnan(df_resampled["QH"].iloc[0])
    assert df_resampled["QH"].iloc[1] == pytest.approx(15.0)

    # 3. Naive pandas .last() backfills 8.0 when the endpoint is missing;
    #    SUEWS's LAST rule preserves the missing endpoint as NaN.
    assert naive_resampler["State"].last().iloc[1] == pytest.approx(8.0)
    assert np.isnan(df_resampled["State"].iloc[0])
    assert np.isnan(df_resampled["State"].iloc[1])
