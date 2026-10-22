"""Tests for the opt-in year-by-year streamed run, ``run(stream="year")``.

A streamed run writes each calendar year's files during the run and keeps no
output frame. Its files must match what ``run()`` followed by ``save()``
writes; the chunked state chaining is close to, but not bit-identical with,
an uninterrupted run (see ``test_checkpoint.py``), so the file contents are
compared numerically at the precision the text files are written with.
"""

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy._run_rust import forcing_year_keys

pytestmark = pytest.mark.api

# Text output carries four decimals; allow one unit in the last place plus
# the round-off of reading it back.
TXT_ATOL = 1.5e-4
# Chained state differs from an uninterrupted run only by round-off.
STATE_RTOL = 1e-9
STATE_ATOL = 1e-9
GROUPS = ["SUEWS", "DailyState", "RSL", "debug"]


def test_year_keys_follow_right_closed_years():
    """The row stamped 1 January 00:00 closes the previous year."""
    for tstep_s in (300, 3600):
        step = pd.Timedelta(seconds=tstep_s)
        index = pd.date_range(
            pd.Timestamp("2012-12-31 00:00") + step,
            "2013-01-02 00:00",
            freq=step,
        )
        keys = pd.Series(forcing_year_keys(index, tstep_s), index=index)

        assert (keys.loc[: pd.Timestamp("2013-01-01 00:00")] == 2012).all()
        assert (keys.loc[pd.Timestamp("2013-01-01 00:00") + step :] == 2013).all()


def test_year_keys_fold_a_leading_stub():
    """A run starting with the last interval of the previous year is one chunk."""
    index = pd.date_range("2012-01-01 00:00", "2012-01-03 00:00", freq="h")

    keys = forcing_year_keys(index, 3600)

    assert set(keys) == {2012}


def _year_crossing_sim(short_sample_sim, path_out, freq_s):
    """The January short sample relabelled to run across 1 January."""
    sim = short_sample_sim()
    forcing = sim.forcing.df.copy()
    forcing.index -= pd.Timedelta(days=16)
    forcing.index.freq = forcing.index[1] - forcing.index[0]
    for col, values in (
        ("iy", forcing.index.year),
        ("id", forcing.index.day_of_year),
        ("it", forcing.index.hour),
        ("imin", forcing.index.minute),
    ):
        if col in forcing.columns:
            forcing[col] = values
    sim.update_config(
        {
            "model": {
                "control": {
                    "start_time": str(forcing.index[0]),
                    "end_time": str(forcing.index[-1]),
                    "output": {
                        "format": "txt",
                        "freq": freq_s,
                        "dir": str(path_out),
                        "groups": GROUPS,
                    },
                }
            }
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(forcing)
    return sim


def _numbers(obj):
    """Flatten every number in a JSON document, in document order."""
    if isinstance(obj, dict):
        for key in sorted(obj):
            yield from _numbers(obj[key])
    elif isinstance(obj, list):
        for item in obj:
            yield from _numbers(item)
    elif isinstance(obj, (int, float)) and not isinstance(obj, bool):
        yield float(obj)


@pytest.mark.parametrize("freq_s", [3600, 300])
def test_streamed_files_match_run_then_save(short_sample_sim, tmp_path, freq_s):
    """Streamed year files match run() + save(), file for file."""
    dir_full = tmp_path / "full"
    dir_stream = tmp_path / "stream"
    sim_full = _year_crossing_sim(short_sample_sim, dir_full, freq_s)
    sim_full.run()
    sim_full.save()

    sim_stream = _year_crossing_sim(short_sample_sim, dir_stream, freq_s)
    output = sim_stream.run(stream="year")

    names_full = sorted(p.name for p in dir_full.iterdir())
    names_stream = sorted(p.name for p in dir_stream.iterdir())
    assert names_stream == names_full
    assert any("_2011_" in name for name in names_full)
    assert any("_2012_" in name for name in names_full)

    for name in names_full:
        if not name.endswith(".txt"):
            continue
        df_full = pd.read_csv(dir_full / name, sep=r"\s+")
        df_stream = pd.read_csv(dir_stream / name, sep=r"\s+")
        assert list(df_stream.columns) == list(df_full.columns), name
        assert df_stream.shape == df_full.shape, name
        np.testing.assert_allclose(
            df_stream.to_numpy(dtype=float),
            df_full.to_numpy(dtype=float),
            rtol=0,
            atol=TXT_ATOL,
            err_msg=name,
        )

    [name_ckpt] = [n for n in names_full if n.endswith("_checkpoint.json")]
    ckpt_full = json.loads((dir_full / name_ckpt).read_text(encoding="utf-8"))
    ckpt_stream = json.loads((dir_stream / name_ckpt).read_text(encoding="utf-8"))
    assert ckpt_stream["last_timestamp"] == ckpt_full["last_timestamp"]
    np.testing.assert_allclose(
        list(_numbers(ckpt_stream)),
        list(_numbers(ckpt_full)),
        rtol=STATE_RTOL,
        atol=STATE_ATOL,
    )

    # The returned output keeps the layout but holds no rows.
    assert output.df.empty
    assert output.df.columns.equals(sim_full.output.df.columns)
    assert sim_stream.checkpoint is not None


def test_save_after_stream_returns_the_streamed_files(short_sample_sim, tmp_path):
    """save() after a streamed run reports the files; it cannot rewrite them."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600)
    sim.run(stream="year")

    paths = sim.save()

    assert {Path(p).name for p in paths} == {
        p.name for p in (tmp_path / "out").iterdir()
    }
    with pytest.raises(ValueError, match="streamed its output"):
        sim.save(tmp_path / "elsewhere")
    with pytest.raises(ValueError, match="streamed its output"):
        sim.save(format="parquet")


def test_stream_rejects_unsupported_settings(short_sample_sim, tmp_path):
    """Settings a year-by-year stream cannot reproduce fail before running."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600)

    with pytest.raises(ValueError, match="stream must be"):
        sim.run(stream="month")

    sim.update_config(
        {"model": {"control": {"output": {"freq": 25200}}}}, auto_load_forcing=False
    )
    with pytest.raises(ValueError, match="divides one day"):
        sim.run(stream="year")

    sim.update_config(
        {"model": {"control": {"output": {"freq": 3600, "format": "parquet"}}}},
        auto_load_forcing=False,
    )
    with pytest.raises(ValueError, match="text output only"):
        sim.run(stream="year")
