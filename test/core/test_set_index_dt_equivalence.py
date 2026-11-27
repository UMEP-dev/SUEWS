"""Equivalence tests for vectorised ``set_index_dt`` timestamp parsing.

Compares the vectorised ``pd.to_datetime`` implementation of
:func:`supy._load.set_index_dt` against a private copy of the original per-row
``pd.to_datetime`` implementation across leap days, year crossings, NaN footer
rows, ordering/duplicate/gap failures, malformed rows and format edge cases.
"""

import numpy as np
import pandas as pd
import pytest

from supy._load import (
    BASELINE_DATETIME_FORCING_COLUMNS,
    BASELINE_DATETIME_FORCING_SET,
    set_index_dt,
)

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-FORCING", "C-CLOCK")]


def _reference_set_index_dt(df_raw: pd.DataFrame) -> pd.DataFrame:
    """Reference copy of the original per-row ``pd.to_datetime`` implementation."""
    df_raw = df_raw.dropna(subset=BASELINE_DATETIME_FORCING_SET)
    idx_dt = pd.date_range(
        *df_raw.iloc[[0, -1]][list(BASELINE_DATETIME_FORCING_COLUMNS)]
        .astype(int)
        .astype(str)
        .apply(lambda ser: ser.str.cat(sep=" "), axis=1)
        .map(lambda dt: pd.to_datetime(dt, format="%Y %j %H %M")),
        periods=df_raw.shape[0],
    )
    list_dt = [
        *df_raw[list(BASELINE_DATETIME_FORCING_COLUMNS)]
        .astype(int)
        .astype(str)
        .apply(lambda ser: ser.str.cat(sep=" "), axis=1)
        .map(lambda dt: pd.to_datetime(dt, format="%Y %j %H %M"))
    ]
    ser_dt = pd.DatetimeIndex(list_dt).to_series()
    ser_dt_diff = ser_dt.diff()
    ser_dt_diff_sec = ser_dt_diff.dt.total_seconds()
    dif_dt = ser_dt_diff_sec.abs().min()
    ser_test = ser_dt_diff_sec.eq(dif_dt)
    loc_issue = ser_dt_diff_sec.reset_index().index[~ser_test][1:] + 2
    if loc_issue.size == 0:
        freq = ser_dt_diff.iloc[1]
        df_datetime = df_raw.set_index(idx_dt).asfreq(freq)
    else:
        raise RuntimeError(
            f"Loaded forcing files have gaps/duplicates; "
            f"suspious lines include: {loc_issue.to_list()}. "
            f"NOTE: the reported line numbers are NOT completely accurate: only for your reference."
        )
    return df_datetime


def _frame_from_timestamps(timestamps: pd.DatetimeIndex) -> pd.DataFrame:
    """Build a minimal raw forcing table from *timestamps*."""
    return pd.DataFrame({
        "iy": timestamps.year,
        "id": timestamps.day_of_year,
        "it": timestamps.hour,
        "imin": timestamps.minute,
        "tair": np.linspace(5.0, 15.0, len(timestamps)),
    })


def _frame_from_rows(rows: list[tuple]) -> pd.DataFrame:
    """Build a minimal raw forcing table from explicit ``(iy, id, it, imin)`` tuples."""
    return pd.DataFrame(
        [(iy, doy, hr, mn, float(idx)) for idx, (iy, doy, hr, mn) in enumerate(rows)],
        columns=["iy", "id", "it", "imin", "tair"],
    )


def _capture_error(func, df_raw: pd.DataFrame) -> BaseException:
    """Call *func(df_raw)* and return the raised exception, failing if none raised."""
    # ZeroDivisionError: all-duplicate rows give a zero frequency, which
    # pandas rejects in ``asfreq`` on both the reference and the fast path.
    with pytest.raises(
        (ValueError, RuntimeError, IndexError, ZeroDivisionError)
    ) as exc_info:
        func(df_raw)
    return exc_info.value


def _assert_same_outcome(df_raw: pd.DataFrame) -> None:
    """Assert that ``set_index_dt`` and the reference return or raise identically."""
    try:
        expected = _reference_set_index_dt(df_raw)
    except Exception as exc_expected:
        with pytest.raises(type(exc_expected)) as exc_info:
            set_index_dt(df_raw)
        assert type(exc_info.value) is type(exc_expected)
        assert str(exc_info.value) == str(exc_expected)
    else:
        actual = set_index_dt(df_raw)
        pd.testing.assert_index_equal(actual.index, expected.index, exact=True)
        assert actual.index.freq == expected.index.freq
        assert actual.index.name == expected.index.name
        pd.testing.assert_frame_equal(actual, expected, check_exact=True)


@pytest.mark.parametrize(
    ("start", "end", "freq"),
    [
        ("2012-02-28 22:00", "2012-03-01 02:00", "5min"),
        ("2012-02-28 18:00", "2012-03-01 06:00", "h"),
        ("2011-12-31 20:00", "2012-01-01 04:00", "5min"),
        ("2012-12-31 20:00", "2013-01-01 04:00", "h"),
    ],
    ids=[
        "leap-day-5min",
        "leap-day-hourly",
        "year-crossing-nonleap-to-leap",
        "year-crossing-leap-doy366-to-nonleap",
    ],
)
def test_set_index_dt_matches_reference_on_valid_calendars(start, end, freq):
    """Vectorised index matches the per-row reference exactly on valid calendars."""
    df_raw = _frame_from_timestamps(pd.date_range(start, end, freq=freq))

    actual = set_index_dt(df_raw)
    expected = _reference_set_index_dt(df_raw)

    pd.testing.assert_index_equal(actual.index, expected.index, exact=True)
    assert actual.index.freq == expected.index.freq
    assert actual.index.name == expected.index.name
    pd.testing.assert_frame_equal(actual, expected, check_exact=True)


def test_set_index_dt_drops_nan_footer_rows_like_reference():
    """Rows with missing timestamp fields (float columns) match the reference."""
    df_base = _frame_from_timestamps(
        pd.date_range("2012-02-28 23:00", "2012-02-29 03:00", freq="h")
    )
    df_footer = pd.DataFrame({
        "iy": [np.nan],
        "id": [np.nan],
        "it": [np.nan],
        "imin": [np.nan],
        "tair": [-9.0],
    })
    df_raw = pd.concat([df_base, df_footer], ignore_index=True)

    actual = set_index_dt(df_raw)
    expected = _reference_set_index_dt(df_raw)

    pd.testing.assert_index_equal(actual.index, expected.index, exact=True)
    assert actual.index.freq == expected.index.freq
    pd.testing.assert_frame_equal(actual, expected, check_exact=True)


@pytest.mark.parametrize(
    "rows",
    [
        # Unsorted interior swap
        [(2012, 1, 0, 0), (2012, 1, 2, 0), (2012, 1, 1, 0), (2012, 1, 3, 0)],
        # Unsorted strictly descending 2-row input
        [(2012, 1, 1, 0), (2012, 1, 0, 0)],
        # Leading duplicate followed by regular step
        [(2012, 1, 0, 0), (2012, 1, 0, 0), (2012, 1, 1, 0)],
        # Interior duplicate
        [(2012, 1, 0, 0), (2012, 1, 1, 0), (2012, 1, 1, 0), (2012, 1, 2, 0)],
        # All duplicate rows
        [(2012, 1, 0, 0), (2012, 1, 0, 0)],
        # Gap in regular cadence
        [(2012, 1, 0, 0), (2012, 1, 1, 0), (2012, 1, 3, 0)],
    ],
    ids=[
        "unsorted-interior",
        "unsorted-descending",
        "duplicate-leading",
        "duplicate-interior",
        "duplicate-all",
        "gap",
    ],
)
def test_set_index_dt_matches_reference_on_order_gap_and_duplicate_errors(rows):
    """Unsorted, duplicate and gapped inputs raise the same exception class."""
    df_raw = _frame_from_rows(rows)

    exc_actual = _capture_error(set_index_dt, df_raw)
    exc_expected = _capture_error(_reference_set_index_dt, df_raw)

    assert type(exc_actual) is type(exc_expected)
    if isinstance(exc_expected, RuntimeError):
        assert str(exc_actual) == str(exc_expected)


@pytest.mark.parametrize(
    "rows",
    [
        # Hour 24
        [(2012, 1, 23, 0), (2012, 1, 24, 0)],
        # Minute 60
        [(2012, 1, 0, 55), (2012, 1, 0, 60)],
        # Legacy -9 footer not filtered to NaN
        [(2012, 1, 0, 0), (-9, -9, -9, -9)],
        # Non-numeric string in timestamp column
        [("bad", 1, 0, 0), (2012, 1, 1, 0)],
        # Interior bad value paired with invalid final endpoint (endpoint-first order)
        [(2012, 1, 0, 0), ("bad", 1, 1, 0), (2012, 1, 24, 0)],
    ],
    ids=[
        "hour-24",
        "minute-60",
        "sentinel-minus9",
        "non-numeric-year",
        "interior-bad-and-invalid-final",
    ],
)
def test_set_index_dt_matches_reference_on_malformed_rows(rows):
    """Malformed rows raise the exact same exception class and message as reference."""
    df_raw = _frame_from_rows(rows)

    exc_actual = _capture_error(set_index_dt, df_raw)
    exc_expected = _capture_error(_reference_set_index_dt, df_raw)

    assert type(exc_actual) is type(exc_expected)
    assert str(exc_actual) == str(exc_expected)


@pytest.mark.parametrize(
    "rows",
    [
        # Non-leap year with %j=366
        [(2011, 365, 23, 0), (2011, 366, 0, 0)],
        # 3-digit year
        [(999, 1, 0, 0), (999, 1, 1, 0)],
    ],
    ids=[
        "nonleap-doy366",
        "three-digit-year",
    ],
)
def test_set_index_dt_matches_reference_on_format_edge_cases(rows):
    """Non-leap %j=366 and 3-digit year match the reference result or error class."""
    _assert_same_outcome(_frame_from_rows(rows))
