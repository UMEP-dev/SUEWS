"""Calendar resampling bins and labels every output group alike.

A two-month sample run checks that weekly, monthly and yearly bins hold one
period in one row for SUEWS and DailyState, and that the step closing a period
counts towards it.
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._post import _resample_output

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def output():
    """Sample run from 1 January to 2 March 2012 01:15 (5-minute steps)."""
    sim = SUEWSSimulation.from_sample_data()
    return sim.run(start_date="2012-01-01", end_date="2012-03-02 01:15")


def _native_rain_by_period(output, freq):
    """Sum native Rain over right-closed periods using interval start times."""
    rain = output.df["SUEWS", "Rain"].droplevel("grid")
    starts = rain.index - pd.Timedelta(output._output_frequency())
    return rain.groupby(starts.to_period(freq)).sum()


PERIOD = {"W": "W", "MS": "M", "ME": "M", "YE": "Y"}


@pytest.mark.parametrize("freq", ["W", "MS", "ME", "YE"])
def test_calendar_dailystate_shares_the_period_row(output, freq):
    """Each period's P_day sits in the row of that period's Rain.

    Before the fix, ME put January's P_day in the 2011-12-31 row and January's
    Rain in the 2012-01-31 row. A DailyState day sums the steps stamped on
    that date (00:00 to 23:55), so P_day is compared with Rain summed the same
    way, period by period.
    """
    result = output.resample(freq).df.droplevel("grid")
    p_day = result["DailyState", "P_day"]
    rain = result["SUEWS", "Rain"]
    # No row holds only one of the two: no leading DailyState-only row.
    assert (p_day.notna() == rain.notna()).all()
    native = output.df["SUEWS", "Rain"].droplevel("grid")
    native = native.loc[: "2012-03-01 23:55"]  # days with a DailyState row
    by_date = native.groupby(native.index.to_period(PERIOD[freq])).sum()
    np.testing.assert_allclose(p_day.to_numpy()[: len(by_date)], by_date.to_numpy(), atol=1e-9)


def test_monthly_values_match_independent_sums(output):
    """ME labels January 2012-01-31; Rain includes the step closing January."""
    result = output.resample("ME").df.droplevel("grid")
    expected = _native_rain_by_period(output, "M")
    assert result.index[0] == pd.Timestamp("2012-01-31")
    np.testing.assert_allclose(
        result["SUEWS", "Rain"].to_numpy(), expected.to_numpy(), atol=1e-9
    )
    jan = output.df["DailyState", "P_day"].droplevel("grid").loc["2012-01"].sum()
    assert result.loc["2012-01-31", ("DailyState", "P_day")] == pytest.approx(jan)


def test_period_closing_step_counts_towards_its_period():
    """The 2012-02-01 00:00 step closes January under ME, as under MS."""
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-31 23:55", periods=3, freq="5min")],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_tuples([("SUEWS", "QH")], names=["group", "var"])
    frame = pd.DataFrame([[1.0], [2.0], [4.0]], index=index, columns=columns)
    monthly = _resample_output(frame, "ME", freq_source="5min")
    start = _resample_output(frame, "MS", freq_source="5min")
    assert monthly["SUEWS", "QH"].to_numpy() == pytest.approx([1.5, 4.0])
    assert start["SUEWS", "QH"].to_numpy() == pytest.approx([1.5, 4.0])


def test_daily_labels_are_unchanged(output):
    """At fixed frequencies DailyState keeps its start-of-day label."""
    result = output.resample("D").df.droplevel("grid")
    p_day = result["DailyState", "P_day"].dropna()
    rain = result["SUEWS", "Rain"].dropna()
    assert p_day.index[0] == pd.Timestamp("2012-01-01")
    assert rain.index[0] == pd.Timestamp("2012-01-02")
    assert p_day.iloc[0] == pytest.approx(rain.iloc[0])


@pytest.mark.parametrize("freq", ["W", "MS", "ME", "YE", "2D"])
def test_daily_then_coarser_matches_direct(output, freq):
    """Resampling daily output again assigns each day as the native rows do.

    A daily DailyState row is labelled by the start of its day, so binning it
    as if the label were the day's end put 1 January's P_day in December
    (``ME``, ``YE``, ``MS``) or into the wrong two-day bin (``2D``).
    """
    direct = output.resample(freq)
    chained = output.resample("D").resample(freq)
    for column in [("DailyState", "P_day"), ("SUEWS", "Rain")]:
        pd.testing.assert_series_equal(
            chained.df[column].dropna(), direct.df[column].dropna(), rtol=1e-9
        )
    coverage = "supy_output_daily_coverage_s"
    assert chained.df.attrs[coverage] == pytest.approx(direct.df.attrs[coverage])


@pytest.mark.parametrize("freq", ["W", "MS", "ME", "YE", "2D"])
def test_raw_daily_frame_then_coarser_matches_direct(output, freq):
    """A raw daily frame resampled again holds its interval in its attributes.

    ``_resample_output(output.resample("D").df, freq)`` has no wrapper to say
    the rows are daily; reading the frame's own ``supy_output_interval_s``
    keeps 1 January's P_day and coverage out of December.
    """
    daily = output.resample("D").df
    assert daily.attrs["supy_output_interval_s"] == 86400
    direct = output.resample(freq).df
    chained = _resample_output(daily, freq)
    for column in [("DailyState", "P_day"), ("SUEWS", "Rain")]:
        pd.testing.assert_series_equal(
            chained[column].dropna(), direct[column].dropna(), rtol=1e-9
        )
    coverage = "supy_output_daily_coverage_s"
    assert chained.attrs[coverage] == pytest.approx(direct.attrs[coverage])


def test_raw_native_frame_without_attributes_uses_index_cadence(output):
    """Without attributes or state, a regular index gives the held interval."""
    from supy._output_metadata import resolve_held_interval

    raw = output.df.copy()
    raw.attrs = {
        key: value for key, value in raw.attrs.items()
        if key not in ("supy_output_interval_s", "supy_output_daily_coverage_s")
    }
    held = resolve_held_interval(raw)
    assert (held.kind, held.seconds, held.source) == ("fixed", 300.0, "index")
    assert _resample_output(raw, "5min") is raw
    direct = output.resample("ME").df
    result = _resample_output(raw, "ME")
    pd.testing.assert_series_equal(
        result["DailyState", "P_day"].dropna(),
        direct["DailyState", "P_day"].dropna(),
        rtol=1e-9,
    )


def test_held_interval_is_unknown_or_calendar_honestly():
    """Irregular stamps are unknown; a calendar attribute or alias is calendar."""
    from supy._output_metadata import resolve_held_interval

    index = pd.MultiIndex.from_product(
        [[1], pd.to_datetime(["2012-01-01 00:05", "2012-01-01 00:10", "2012-01-01 00:20"])],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_tuples([("SUEWS", "QH")], names=["group", "var"])
    frame = pd.DataFrame(1.0, index=index, columns=columns)
    assert resolve_held_interval(frame).kind == "unknown"
    assert resolve_held_interval(frame, "ME").kind == "calendar"
    frame.attrs["supy_output_interval_s"] = None
    assert resolve_held_interval(frame).kind == "calendar"
    frame.attrs["supy_output_interval_s"] = 3600
    assert resolve_held_interval(frame).seconds == 3600


def test_held_interval_reads_an_unnamed_datetime_level():
    """A (grid, datetime) index whose names were lost is still read by position."""
    from supy._output_metadata import resolve_held_interval

    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01 00:05", periods=4, freq="5min")]
    )
    columns = pd.MultiIndex.from_tuples([("SUEWS", "QH")], names=["group", "var"])
    frame = pd.DataFrame(1.0, index=index, columns=columns)
    assert resolve_held_interval(frame).seconds == 300
