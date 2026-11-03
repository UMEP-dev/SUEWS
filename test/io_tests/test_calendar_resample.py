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
