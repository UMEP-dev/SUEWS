"""A run ending part-way through a day writes no DailyState row for that day."""

import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def output():
    """Sample run from 1 January to 2 March 2012 01:15 (5-minute steps)."""
    sim = SUEWSSimulation.from_sample_data()
    return sim.run(start_date="2012-01-01", end_date="2012-03-02 01:15")


def test_last_partial_day_has_no_dailystate_row(output):
    """A day's DailyState row is written at its last step (documented)."""
    stamps = output.df["DailyState", "P_day"].droplevel("grid").dropna().index
    assert stamps[-1] == pd.Timestamp("2012-03-01 23:55")
