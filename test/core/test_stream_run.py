"""Tests for the opt-in year-by-year streamed run, ``run(stream="year")``.

A streamed run writes each calendar year's files during the run and keeps no
output frame. Its files must match what ``run()`` followed by ``save()``
writes; the chunked state chaining is close to, but not bit-identical with,
an uninterrupted run (see ``test_checkpoint.py``), so the file contents are
compared numerically at the precision the text files are written with.

The returned output holds no rows, so its row accessors raise; the checkpoint
is written after every year, so a run that fails part-way resumes from the
last completed year.
"""

import json
from pathlib import Path
import re

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSCheckpoint, SUEWSSimulation
from supy._run_rust import forcing_year_keys
import supy.suews_sim as suews_sim_module

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


def _compare_dirs(dir_a, dir_b):
    """Same file list; text values and checkpoint numbers within tolerance."""
    names_a = sorted(p.name for p in dir_a.iterdir())
    names_b = sorted(p.name for p in dir_b.iterdir())
    assert names_a == names_b
    for name in names_a:
        if name.endswith(".txt"):
            df_a = pd.read_csv(dir_a / name, sep=r"\s+")
            df_b = pd.read_csv(dir_b / name, sep=r"\s+")
            assert list(df_a.columns) == list(df_b.columns), name
            assert df_a.shape == df_b.shape, name
            np.testing.assert_allclose(
                df_a.to_numpy(dtype=float),
                df_b.to_numpy(dtype=float),
                rtol=0,
                atol=TXT_ATOL,
                err_msg=name,
            )
        elif name.endswith("_checkpoint.json"):
            ckpt_a = json.loads((dir_a / name).read_text(encoding="utf-8"))
            ckpt_b = json.loads((dir_b / name).read_text(encoding="utf-8"))
            assert ckpt_a["last_timestamp"] == ckpt_b["last_timestamp"]
            np.testing.assert_allclose(
                list(_numbers(ckpt_a)),
                list(_numbers(ckpt_b)),
                rtol=STATE_RTOL,
                atol=STATE_ATOL,
            )


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

    names_full = [p.name for p in dir_full.iterdir()]
    assert any("_2011_" in name for name in names_full)
    assert any("_2012_" in name for name in names_full)
    assert sum(name.endswith("_checkpoint.json") for name in names_full) == 1
    _compare_dirs(dir_stream, dir_full)

    # The returned output keeps the layout but holds no rows.
    assert output.columns.equals(sim_full.output.df.columns)
    assert sim_stream.checkpoint is not None


def test_streamed_output_raises_on_row_access(short_sample_sim, tmp_path):
    """Row accessors of a streamed output raise and name the output folder."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600)
    output = sim.run(stream="year")
    folder = re.escape(str(dir_out.resolve()))

    accessors = [
        lambda o: o.df,
        lambda o: o.empty,
        lambda o: o.index,
        lambda o: o.loc,
        lambda o: o.iloc,
        lambda o: o.xs(1, level="grid"),
        lambda o: o.times,
        lambda o: o.grids,
        len,
        lambda o: o.get_group("SUEWS"),
        lambda o: o.get_variable("QH"),
        lambda o: o.QH,
        lambda o: o["SUEWS"],
        lambda o: o.resample("1D"),
        lambda o: o.save(tmp_path / "again"),
    ]
    for access in accessors:
        with pytest.raises(RuntimeError, match=folder):
            access(output)
        with pytest.raises(RuntimeError, match="streamed to disk"):
            access(sim.output)
    with pytest.raises(RuntimeError, match=folder):
        sim.get_variable("QH")
    assert not (tmp_path / "again").exists()

    # Layout, state and checkpoint stay available.
    assert "SUEWS" in output.groups
    assert "QH" in output.available_variables["SUEWS"]
    assert output.checkpoint is sim.checkpoint
    assert output.checkpoint is not None
    assert not output.state_final.empty
    assert isinstance(output.kernel_warnings, pd.DataFrame)
    assert set(output.streamed_paths) == {str(p) for p in sim.save()}
    assert "streamed to" in repr(output)
    assert "streamed to" in repr(sim)


def test_checkpoint_is_written_after_every_year(
    short_sample_sim, tmp_path, monkeypatch
):
    """A run failing in its second year resumes from the first year's checkpoint."""
    # Uninterrupted streamed run, recording each per-year checkpoint.
    dir_full = tmp_path / "full"
    sim_full = _year_crossing_sim(short_sample_sim, dir_full, 3600)
    last_forcing = sim_full.forcing.df.index[-1]
    boundary = pd.Timestamp("2012-01-01 00:00")
    written = []
    write_year_checkpoint = SUEWSSimulation._write_year_checkpoint

    def spy(dict_state_json, last_timestamp, settings):
        path = write_year_checkpoint(dict_state_json, last_timestamp, settings)
        written.append((pd.Timestamp(last_timestamp), path.name))
        return path

    monkeypatch.setattr(SUEWSSimulation, "_write_year_checkpoint", staticmethod(spy))
    sim_full.run(stream="year")
    monkeypatch.undo()
    [name_ckpt] = [
        p.name for p in dir_full.iterdir() if p.name.endswith("_checkpoint.json")
    ]
    assert written == [(boundary, name_ckpt), (last_forcing, name_ckpt)]

    # The same run failing once the first year is written.
    dir_resume = tmp_path / "resume"
    sim_fail = _year_crossing_sim(short_sample_sim, dir_resume, 3600)
    iter_by_year = suews_sim_module.iter_suews_rust_by_year

    def fail_in_second_year(*args, **kwargs):
        chunks = iter_by_year(*args, **kwargs)
        yield next(chunks)
        raise RuntimeError("simulated failure in year 2")

    monkeypatch.setattr(
        suews_sim_module, "iter_suews_rust_by_year", fail_in_second_year
    )
    with pytest.raises(RuntimeError, match="simulated failure"):
        sim_fail.run(stream="year")
    monkeypatch.undo()

    names = {p.name for p in dir_resume.iterdir()}
    assert name_ckpt in names
    assert "provenance.json" not in names
    assert not any(name.endswith(".tmp") for name in names)
    assert any("_2011_" in name for name in names)
    assert not any("_2012_" in name for name in names)
    ckpt = SUEWSCheckpoint.from_file(dir_resume / name_ckpt)
    assert pd.Timestamp(ckpt.last_timestamp) == boundary

    # Resume from the last completed year, as the run() docstring describes.
    tstep = sim_fail.config.model.control.tstep
    tstep = int(getattr(tstep, "value", tstep))
    sim_resume = SUEWSSimulation.from_checkpoint(sim_fail.config, ckpt)
    sim_resume.update_forcing(sim_fail.forcing.df)
    start = pd.Timestamp(ckpt.last_timestamp) + pd.Timedelta(seconds=tstep)
    sim_resume.run(start_date=start, stream="year")

    _compare_dirs(dir_resume, dir_full)


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
