"""Saving resampled output: the held interval, or a refusal for calendar periods."""

import json

import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def output():
    """Sample run from 1 January to 2 March 2012 01:15 (5-minute steps)."""
    sim = SUEWSSimulation.from_sample_data()
    return sim.run(start_date="2012-01-01", end_date="2012-03-02 01:15")


@pytest.mark.parametrize("freq", ["W", "MS", "ME", "YE"])
@pytest.mark.parametrize("file_format", ["txt", "parquet"])
def test_calendar_save_is_refused(output, tmp_path, freq, file_format):
    """No crash, no stray files: calendar saves stop with the remedy."""
    with pytest.raises(ValueError, match=r"Calendar-resampled output.*cannot be saved.*native output"):
        output.resample(freq).save(tmp_path, format=file_format)
    assert not [p for p in tmp_path.iterdir() if p.suffix in {".txt", ".parquet"}]


def test_fixed_resample_saves_at_held_interval(output, tmp_path):
    """Daily output writes one file named, and described, at one day."""
    paths = output.resample("D").save(tmp_path)
    texts = sorted(p.name for p in paths if p.suffix == ".txt")
    assert texts == ["KCL1_2012_SUEWS_1440.txt"]
    meta = json.loads((tmp_path / "KCL1_2012_SUEWS_1440.txt.units.json").read_text(encoding="utf-8"))
    assert meta["columns"]["Rain"]["interval"] == "1 day"
    saved = pd.read_csv(tmp_path / texts[0], sep=r"\s+")
    expected = output.df["SUEWS", "Rain"].droplevel("grid").loc["2012-01-01 00:05":"2012-01-02 00:00"].sum()
    assert saved["Rain"].iloc[0] * 86400 == pytest.approx(expected, rel=1e-6)


def test_resampled_save_refuses_finer_frequency(output, tmp_path):
    with pytest.raises(ValueError, match="finer than the held output interval"):
        output.resample("D").save(tmp_path, freq_s=3600)


def test_explicit_freq_s_overrides_config(output, tmp_path):
    paths = output.save(tmp_path, freq_s=1800)
    assert any(p.name.endswith("_SUEWS_30.txt") for p in paths)
