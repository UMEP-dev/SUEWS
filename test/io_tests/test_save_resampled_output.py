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


def _assert_daily_files(paths, tmp_path):
    texts = sorted(p.name for p in paths if p.suffix == ".txt")
    suews = [name for name in texts if "_SUEWS_" in name]
    assert suews and all(name.endswith("_SUEWS_1440.txt") for name in suews)
    meta = json.loads((tmp_path / f"{suews[0]}.units.json").read_text(encoding="utf-8"))
    assert meta["columns"]["Rain"]["interval"] == "1 day"


@pytest.mark.parametrize("with_config", [False, True])
def test_reconstructed_resampled_frame_saves_at_held_interval(output, tmp_path, with_config):
    """A wrapper rebuilt from resampled rows keeps their interval when saved."""
    from supy.suews_output import SUEWSOutput

    daily = output.resample("D")
    rebuilt = SUEWSOutput(
        daily.df, output.state_final, config=output.config if with_config else None
    )
    _assert_daily_files(rebuilt.save(tmp_path), tmp_path)
    with pytest.raises(ValueError, match="finer than the held output interval"):
        rebuilt.save(tmp_path / "hourly", freq_s=3600)


def test_reloaded_parquet_frame_saves_at_held_interval(output, tmp_path):
    """Parquet keeps the held interval, so reloaded daily rows save as daily."""
    from supy.suews_output import SUEWSOutput

    with pytest.warns(FutureWarning):
        paths = output.resample("D").save(tmp_path / "pq", format="parquet")
    path_output = next(p for p in paths if p.name.endswith("_output.parquet"))
    reloaded = SUEWSOutput(pd.read_parquet(path_output), output.state_final)
    _assert_daily_files(reloaded.save(tmp_path / "txt"), tmp_path / "txt")


@pytest.mark.parametrize("file_format", ["txt", "parquet"])
def test_writer_refuses_finer_frequency(output, tmp_path, file_format):
    """The low-level writers refuse to describe daily rows as hourly ones."""
    from supy._save import save_df_output, save_df_output_parquet

    writer = save_df_output if file_format == "txt" else save_df_output_parquet
    with pytest.raises(ValueError, match="finer than the held output interval of 86400 s"):
        writer(output.resample("D").df, freq_s=3600, path_dir_save=tmp_path,
               df_state_final=output.state_final)
    assert not [p for p in tmp_path.iterdir() if p.suffix in {".txt", ".parquet"}]


def test_native_output_with_units_saves_at_configured_frequency(output, tmp_path):
    """Native rows converted to a rate unit still save at the configured hour."""
    paths = output.to_units({"Rain": "mm h-1"}).save(tmp_path)
    assert any(p.name.endswith("_SUEWS_60.txt") for p in paths)
