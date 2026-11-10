"""An explicit ``groups`` argument to ``SUEWSOutput.save`` is honoured."""

import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def output():
    """Sample run from 1 January to 2 March 2012 01:15 (5-minute steps)."""
    sim = SUEWSSimulation.from_sample_data()
    return sim.run(start_date="2012-01-01", end_date="2012-03-02 01:15")


@pytest.mark.parametrize("groups", [["DailyState"], ["SUEWS", "DailyState"], "DailyState"])
def test_save_honours_groups(output, tmp_path, groups):
    """An explicit groups argument overrides the configured groups."""
    paths = output.save(tmp_path, groups=groups)
    written = {p.name.split("_")[2].split(".")[0] for p in paths if p.suffix == ".txt"}
    expected = {groups} if isinstance(groups, str) else set(groups)
    assert written == expected


def test_save_rejects_unknown_group(output, tmp_path):
    with pytest.raises(ValueError, match=r"Nope.*available groups"):
        output.save(tmp_path, groups=["Nope"])
