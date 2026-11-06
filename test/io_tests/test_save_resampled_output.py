"""Saving resampled output: the held interval, or a refusal for calendar periods."""

import json

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._save import save_df_output, save_df_output_parquet
from supy.suews_output import SUEWSOutput

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


def _assert_daily_files(paths, tmp_path):
    texts = sorted(p.name for p in paths if p.suffix == ".txt")
    suews = [name for name in texts if "_SUEWS_" in name]
    assert suews and all(name.endswith("_SUEWS_1440.txt") for name in suews)
    meta = json.loads((tmp_path / f"{suews[0]}.units.json").read_text(encoding="utf-8"))
    assert meta["columns"]["Rain"]["interval"] == "1 day"


@pytest.mark.parametrize("with_config", [False, True])
def test_reconstructed_resampled_frame_saves_at_held_interval(output, tmp_path, with_config):
    """A wrapper rebuilt from resampled rows keeps their interval when saved."""
    daily = output.resample("D")
    rebuilt = SUEWSOutput(
        daily.df, output.state_final, config=output.config if with_config else None
    )
    _assert_daily_files(rebuilt.save(tmp_path), tmp_path)
    with pytest.raises(ValueError, match="finer than the held output interval"):
        rebuilt.save(tmp_path / "hourly", freq_s=3600)


def test_reloaded_parquet_frame_saves_at_held_interval(output, tmp_path):
    """Parquet keeps the held interval, so reloaded daily rows save as daily."""
    with pytest.warns(FutureWarning):
        paths = output.resample("D").save(tmp_path / "pq", format="parquet")
    path_output = next(p for p in paths if p.name.endswith("_output.parquet"))
    reloaded = SUEWSOutput(pd.read_parquet(path_output), output.state_final)
    _assert_daily_files(reloaded.save(tmp_path / "txt"), tmp_path / "txt")


@pytest.mark.parametrize("file_format", ["txt", "parquet"])
def test_writer_refuses_finer_frequency(output, tmp_path, file_format):
    """The low-level writers refuse to describe daily rows as hourly ones."""
    writer = save_df_output if file_format == "txt" else save_df_output_parquet
    with pytest.raises(ValueError, match="finer than the held output interval of 86400 s"):
        writer(output.resample("D").df, freq_s=3600, path_dir_save=tmp_path,
               df_state_final=output.state_final)
    assert not [p for p in tmp_path.iterdir() if p.suffix in {".txt", ".parquet"}]


def test_native_output_with_units_saves_at_configured_frequency(output, tmp_path):
    """Native rows converted to a rate unit still save at the configured hour."""
    paths = output.to_units({"Rain": "mm h-1"}).save(tmp_path)
    assert any(p.name.endswith("_SUEWS_60.txt") for p in paths)


@pytest.mark.parametrize(
    ("freq", "freq_s", "message"),
    [
        (None, 420, "whole multiple of the held output interval of 300 s"),
        (None, 0, "must be positive"),
        (None, -300, "must be positive"),
        ("2h", 10800, "whole multiple of the held output interval of 7200 s"),
    ],
)
def test_save_frequency_must_be_a_multiple_of_held_interval(output, tmp_path, freq, freq_s, message):
    """Seven-minute saving of five-minute rows mixed five- and ten-minute rows."""
    source = output if freq is None else output.resample(freq)
    with pytest.raises(ValueError, match=message):
        source.save(tmp_path, freq_s=freq_s)
    assert not [p for p in tmp_path.iterdir() if p.suffix == ".txt"]


@pytest.mark.parametrize(("freq", "freq_s", "suffix"), [(None, 600, "_10"), ("2h", 14400, "_240")])
def test_save_frequency_multiple_is_written(output, tmp_path, freq, freq_s, suffix):
    source = output if freq is None else output.resample(freq)
    paths = source.save(tmp_path, freq_s=freq_s)
    assert any(p.name.endswith(f"_SUEWS{suffix}.txt") for p in paths)


@pytest.mark.parametrize(
    ("groups", "file_format"),
    [(["DailyState"], "txt"), (["SUEWS", "DailyState"], "txt"), (None, "parquet")],
)
def test_multi_day_dailystate_save_is_refused(output, tmp_path, groups, file_format):
    """Two-day DailyState aggregates were written described as one day."""
    with pytest.raises(ValueError, match=r"DailyState rows resampled to more than one day \(172800 s\)"):
        output.resample("2D").save(tmp_path, groups=groups, format=file_format)
    assert not [p for p in tmp_path.iterdir() if p.suffix in {".txt", ".parquet"}]


def test_multi_day_output_without_dailystate_is_saved(output, tmp_path):
    paths = output.resample("2D").save(tmp_path, groups=["SUEWS"])
    assert [p.name for p in paths if p.suffix == ".txt"] == ["KCL1_2012_SUEWS_2880.txt"]


@pytest.mark.parametrize("freq", ["h", "D"])
def test_sub_daily_and_daily_dailystate_saves_one_row_per_day(output, tmp_path, freq):
    paths = output.resample(freq).save(tmp_path, groups=["DailyState"])
    path = next(p for p in paths if p.name.endswith("_DailyState.txt"))
    saved = pd.read_csv(path, sep=r"\s+")
    assert len(saved) == 61  # 1 January to 1 March: the days the run completes
    assert saved["DOY"].is_unique
    meta = json.loads(path.with_name(path.name + ".units.json").read_text(encoding="utf-8"))
    assert meta["columns"]["HDD1_h"]["interval"] == "1 day"


def test_parquet_save_coarser_than_a_day_refuses_dailystate(output, tmp_path):
    """Parquet resamples DailyState with the other groups: two days is refused."""
    with pytest.raises(ValueError, match=r"more than one day \(172800 s\)"):
        output.save(tmp_path, format="parquet", freq_s=172800)
    assert not list(tmp_path.glob("*.parquet"))


def _without_interval(frame):
    """Copy of *frame* without the declared interval or its derived coverage."""
    raw = frame.copy()
    raw.attrs = {
        key: value for key, value in raw.attrs.items()
        if key not in ("supy_output_interval_s", "supy_output_daily_coverage_s")
    }
    return raw


def test_writer_without_final_state_checks_index_cadence(output, tmp_path):
    """Without df_state_final the timestamps' regular cadence is the held interval.

    The check returned early, so five-minute rows saved at seven minutes held
    alternately five and ten minutes under a seven-minute label. (The Parquet
    writer requires final state, so it always has the model timestep.)
    """
    raw = _without_interval(output.df)
    with pytest.raises(ValueError, match="whole multiple of the held output interval of 300 s"):
        save_df_output(raw, freq_s=420, output_groups=["SUEWS"], path_dir_save=tmp_path)
    assert not [p for p in tmp_path.iterdir() if p.suffix == ".txt"]


def test_writer_without_final_state_saves_a_multiple(output, tmp_path):
    paths = save_df_output(
        _without_interval(output.df), freq_s=600, output_groups=["SUEWS"], path_dir_save=tmp_path
    )
    assert [p.name for p in paths if p.suffix == ".txt"] == ["1_2012_SUEWS_10.txt"]


def test_writer_with_irregular_rows_and_no_state_warns(output, tmp_path, caplog):
    """When no interval can be established the check is skipped, saying so."""
    raw = _without_interval(output.df)
    raw = raw.drop(raw.index[[1, 5, 6]])
    with caplog.at_level("WARNING", logger="SuPy"):
        save_df_output(raw, freq_s=3600, output_groups=["SUEWS"], path_dir_save=tmp_path)
    assert "cannot be checked" in caplog.text
