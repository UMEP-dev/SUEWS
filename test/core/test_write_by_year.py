"""Tests for writing a run's output year by year, ``output.write_by_year``.

With the YAML switch on, ``run()`` goes one calendar year at a time, writes
each year's files and keeps no output frame. Its files must be byte-identical
to what ``run()`` followed by ``save()`` writes: state crosses each year
boundary exactly (gh#1912). While a year is held only the groups the text
output saves are kept (``_KeptOutputStore``), which must be lossless.

The returned output holds no rows, so its row accessors raise; the checkpoint
is written after every year, so a run that fails part-way resumes from the
last completed year.
"""

import json
import logging
from pathlib import Path
import re

from click.testing import CliRunner
import pandas as pd
from pydantic import ValidationError
import pytest

from supy import SUEWSCheckpoint, SUEWSSimulation
from supy._provenance import read_provenance
from supy._run_rust import forcing_year_keys, iter_suews_rust_by_year
from supy.cmd.SUEWS import SUEWS as run_cmd
from supy.data_model.core.model import ModelControl, OutputControl, OutputFormat
import supy.suews_sim as suews_sim_module

pytestmark = pytest.mark.api

GROUPS = ["SUEWS", "DailyState", "RSL", "debug"]
SAMPLE_FORCING_ROWS_PER_DAY = 24  # the sample forcing file is hourly


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


def _replicate_sites(sim, list_gridiv):
    """Replace the site by one copy per *list_gridiv* entry.

    Each copy is 10 degrees further north than the one before, so the grids'
    outputs differ and a block of rows carrying the wrong grid's label would
    not match.
    """
    config = sim.config.model_copy(deep=True)
    site0 = config.sites[0]
    list_site = []
    for pos, gridiv in enumerate(list_gridiv):
        site = site0.model_copy(deep=True)
        site.gridiv = gridiv
        site.name = f"grid{gridiv}"
        site.properties.lat.value += 10.0 * pos
        list_site.append(site)
    config.sites = list_site
    sim.update_config(config, auto_load_forcing=False)


def _year_crossing_forcing(sim):
    """The January short sample relabelled to run across 1 January."""
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
    return forcing


def _year_crossing_sim(
    short_sample_sim,
    path_out,
    freq_s,
    write_by_year,
    list_gridiv=None,
    groups=GROUPS,
):
    """A simulation across 1 January saving *groups* to *path_out*."""
    sim = short_sample_sim()
    forcing = _year_crossing_forcing(sim)
    if list_gridiv is not None:
        _replicate_sites(sim, list_gridiv)
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
                        "groups": groups,
                        "write_by_year": write_by_year,
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
    """Same file list, byte-identical text files and identical checkpoint numbers.

    State crosses each year boundary exactly (gh#1912), so a run written by
    year writes what an uninterrupted run writes, bit for bit; only the
    checkpoint's creation time and ``provenance.json`` record when they ran.
    """
    names_a = sorted(p.name for p in dir_a.iterdir())
    names_b = sorted(p.name for p in dir_b.iterdir())
    assert names_a == names_b
    for name in names_a:
        if name.endswith(".txt"):
            assert (dir_a / name).read_bytes() == (dir_b / name).read_bytes(), name
        elif name.endswith("_checkpoint.json"):
            ckpt_a = json.loads((dir_a / name).read_text(encoding="utf-8"))
            ckpt_b = json.loads((dir_b / name).read_text(encoding="utf-8"))
            assert ckpt_a["last_timestamp"] == ckpt_b["last_timestamp"]
            assert list(_numbers(ckpt_a)) == list(_numbers(ckpt_b)), name


@pytest.mark.parametrize("list_gridiv", [None, [2, 1]], ids=["1grid", "2grids"])
@pytest.mark.parametrize("freq_s", [3600, 300])
def test_files_match_run_then_save(short_sample_sim, tmp_path, freq_s, list_gridiv):
    """Year files match run() + save(), file for file, for one and two grids."""
    dir_full = tmp_path / "full"
    dir_year = tmp_path / "year"
    sim_full = _year_crossing_sim(
        short_sample_sim, dir_full, freq_s, False, list_gridiv=list_gridiv
    )
    sim_full.run()
    sim_full.save()

    sim_year = _year_crossing_sim(
        short_sample_sim, dir_year, freq_s, True, list_gridiv=list_gridiv
    )
    output = sim_year.run()

    names_full = [p.name for p in dir_full.iterdir()]
    assert any("_2011_" in name for name in names_full)
    assert any("_2012_" in name for name in names_full)
    assert sum(name.endswith("_checkpoint.json") for name in names_full) == 1
    if list_gridiv is not None:
        df_suews = sim_full.output.df["SUEWS"]
        assert not df_suews.loc[1].equals(df_suews.loc[2])
    _compare_dirs(dir_year, dir_full)

    # The returned output holds no rows; its layout is that of the saved groups.
    assert output.groups == [g for g in sim_full.output.groups if g in GROUPS]
    assert sim_year.checkpoint is not None
    prov = read_provenance(dir_year)
    assert prov["run"]["write_by_year"] is True
    assert "write_by_year" not in read_provenance(dir_full)["run"]


def test_each_year_keeps_only_the_saved_groups(short_sample_sim, tmp_path, monkeypatch):
    """The frame each year hands to the save code holds only the saved groups."""
    seen = []
    write_output_frame = SUEWSSimulation._write_output_frame

    def spy(df_output, df_state_final, settings):
        seen.append((
            df_output.columns.get_level_values("group").unique().tolist(),
            df_output.index.get_level_values("datetime").max(),
        ))
        return write_output_frame(df_output, df_state_final, settings)

    monkeypatch.setattr(SUEWSSimulation, "_write_output_frame", staticmethod(spy))
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, True, groups=["debug", "SUEWS"]
    )
    sim.run()

    assert [groups for groups, _ in seen] == [["SUEWS", "debug"]] * 2
    assert seen[0][1] == pd.Timestamp("2012-01-01 00:00")


@pytest.mark.parametrize("with_state", [False, True], ids=["fresh", "from-state"])
@pytest.mark.parametrize("list_gridiv", [None, [2, 1]], ids=["1grid", "2grids"])
def test_kept_groups_equal_the_full_year_frames(
    short_sample_sim, list_gridiv, with_state
):
    """Each year's kept frame equals the full frame restricted to the kept groups.

    Covers grids listed out of ID order, at different latitudes, and runs
    starting from saved states, so each grid's rows land in its own block of
    the kept store.
    """
    sim = short_sample_sim()
    forcing = _year_crossing_forcing(sim)
    if list_gridiv is not None:
        _replicate_sites(sim, list_gridiv)
    config = sim.config
    tstep = config.model.control.tstep
    tstep_s = int(getattr(tstep, "value", tstep))
    dict_state = None
    if with_state:
        sim.update_forcing(forcing.iloc[:12])
        sim.run(start_date=forcing.index[0], end_date=forcing.index[11])
        dict_state = sim.checkpoint.grid_states
        forcing = forcing.iloc[12:]

    list_keep = ["SUEWS", "DailyState", "RSL"]
    list_full = list(
        iter_suews_rust_by_year(
            config, forcing, tstep_s, initial_state_json_by_grid=dict_state
        )
    )
    list_kept = list(
        iter_suews_rust_by_year(
            config,
            forcing,
            tstep_s,
            initial_state_json_by_grid=dict_state,
            keep_groups=list_keep,
        )
    )

    assert len(list_full) == len(list_kept) == 2
    for full, kept in zip(list_full, list_kept):
        df_full, state_full, _, last_full, year_full, n_full = full
        df_kept, state_kept, _, last_kept, year_kept, n_kept = kept
        assert (last_full, year_full, n_full) == (last_kept, year_kept, n_kept)
        assert state_full == state_kept
        assert df_kept.columns.get_level_values("group").unique().tolist() == list_keep
        pd.testing.assert_frame_equal(
            df_kept,
            df_full.loc[:, df_full.columns.get_level_values("group").isin(list_keep)],
            check_exact=True,
        )
        if list_gridiv is not None:
            assert df_kept.index.get_level_values("grid").unique().tolist() == [1, 2]


def test_logs_the_folder_and_each_year(short_sample_sim, tmp_path, caplog, monkeypatch):
    """The run says once where the output goes, then reports each year written."""
    monkeypatch.setattr(logging.getLogger("SuPy"), "propagate", True)
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, True)

    with caplog.at_level(logging.INFO, logger="SuPy"):
        sim.run()

    list_msg = [
        r.getMessage()
        for r in caplog.records
        if r.getMessage().startswith("write_by_year:")
    ]
    folder = dir_out.resolve()
    assert list_msg == [
        f"write_by_year: writing the output to {folder} one calendar year at a "
        "time; it is not kept in memory, and chunk_day is ignored.",
        f"write_by_year: wrote 2011 (1 of 2) to {folder}",
        f"write_by_year: wrote 2012 (2 of 2) to {folder}",
    ]


def test_output_raises_on_row_access(short_sample_sim, tmp_path):
    """Row accessors of an output written by year raise and name the folder."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, True)
    output = sim.run()
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
        with pytest.raises(RuntimeError, match="write_by_year"):
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
    assert "written by year to" in repr(output)
    assert "written by year to" in repr(sim)


def test_checkpoint_is_written_after_every_year(
    short_sample_sim, tmp_path, monkeypatch
):
    """A run failing in its second year resumes from the first year's checkpoint."""
    # Uninterrupted run written by year, recording each per-year checkpoint.
    dir_full = tmp_path / "full"
    sim_full = _year_crossing_sim(short_sample_sim, dir_full, 3600, True)
    last_forcing = sim_full.forcing.df.index[-1]
    boundary = pd.Timestamp("2012-01-01 00:00")
    written = []
    write_year_checkpoint = SUEWSSimulation._write_year_checkpoint

    def spy(dict_state_json, last_timestamp, settings):
        path = write_year_checkpoint(dict_state_json, last_timestamp, settings)
        written.append((pd.Timestamp(last_timestamp), path.name))
        return path

    monkeypatch.setattr(SUEWSSimulation, "_write_year_checkpoint", staticmethod(spy))
    sim_full.run()
    monkeypatch.undo()
    [name_ckpt] = [
        p.name for p in dir_full.iterdir() if p.name.endswith("_checkpoint.json")
    ]
    assert written == [(boundary, name_ckpt), (last_forcing, name_ckpt)]

    # The same run failing once the first year is written.
    dir_resume = tmp_path / "resume"
    sim_fail = _year_crossing_sim(short_sample_sim, dir_resume, 3600, True)
    iter_by_year = suews_sim_module.iter_suews_rust_by_year

    def fail_in_second_year(*args, **kwargs):
        chunks = iter_by_year(*args, **kwargs)
        yield next(chunks)
        raise RuntimeError("simulated failure in year 2")

    monkeypatch.setattr(
        suews_sim_module, "iter_suews_rust_by_year", fail_in_second_year
    )
    with pytest.raises(RuntimeError, match="simulated failure"):
        sim_fail.run()
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
    sim_resume.run(start_date=start)

    _compare_dirs(dir_resume, dir_full)


def test_save_afterwards_returns_the_written_files(short_sample_sim, tmp_path):
    """save() after a run written by year reports the files; it cannot rewrite them."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, True)
    sim.run()
    dict_bytes = {p.name: p.read_bytes() for p in dir_out.iterdir()}

    paths = sim.save()

    assert {Path(p).name for p in paths} == set(dict_bytes)
    assert len(paths) == len(set(paths))
    # Nothing was written again.
    assert {p.name: p.read_bytes() for p in dir_out.iterdir()} == dict_bytes
    with pytest.raises(ValueError, match="year by year"):
        sim.save(tmp_path / "elsewhere")
    with pytest.raises(ValueError, match="year by year"):
        sim.save(format="parquet")

    # A command line given to save() reaches the provenance sidecar only.
    sim.save(command="suews run case.yml")
    assert read_provenance(dir_out)["run"]["command"] == "suews run case.yml"
    for name, data in dict_bytes.items():
        if name != "provenance.json":
            assert (dir_out / name).read_bytes() == data, name


def test_default_run_does_not_write_by_year(short_sample_sim, tmp_path, monkeypatch):
    """With the switch off, run() takes the in-memory path and writes nothing."""

    def no_year_loop(*args, **kwargs):
        raise AssertionError("the year loop must not run with the switch off")

    monkeypatch.setattr(suews_sim_module, "iter_suews_rust_by_year", no_year_loop)
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, False)
    output = sim.run()

    assert not (tmp_path / "out").exists()
    assert not output.df.empty


@pytest.mark.parametrize(
    ("output", "match"),
    [
        ({"format": "parquet"}, "text output only"),
        ({"freq": 25200}, "divides one day"),
        ({"timestamp_reference": "daylight"}, "forcing clock"),
    ],
)
def test_output_block_rejects_settings_it_cannot_reproduce(output, match):
    """The YAML validation rejects settings writing by year cannot reproduce."""
    OutputControl(**output)  # valid with the switch off
    with pytest.raises(ValidationError, match=match):
        OutputControl(**output, write_by_year=True)


def test_output_clock_must_follow_the_forcing():
    """Output timestamps must stay on the forcing clock."""
    ok = [
        ("follow", "local_standard_time"),
        ("local_standard_time", "local_standard_time"),
        ("utc", "utc"),
        ("follow", "utc"),
    ]
    for target, source in ok:
        ModelControl(
            output={"write_by_year": True, "timestamp_reference": target},
            forcing={"timestamp_reference": source},
        )
    with pytest.raises(ValidationError, match="forcing clock"):
        ModelControl(
            output={"write_by_year": True, "timestamp_reference": "utc"},
            forcing={"timestamp_reference": "local_standard_time"},
        )
    # The switch off leaves the relabelling allowed.
    ModelControl(
        output={"timestamp_reference": "utc"},
        forcing={"timestamp_reference": "local_standard_time"},
    )


def test_unsupported_settings_fail_before_running(short_sample_sim, tmp_path):
    """Updating a simulation's config to a setting it cannot write by year fails."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, True)

    with pytest.raises(ValueError, match="divides one day"):
        sim.update_config(
            {"model": {"control": {"output": {"freq": 25200}}}},
            auto_load_forcing=False,
        )
    with pytest.raises(ValueError, match="text output only"):
        sim.update_config(
            {"model": {"control": {"output": {"format": "parquet"}}}},
            auto_load_forcing=False,
        )


@pytest.mark.parametrize(
    ("write_by_year", "field", "value", "match"),
    [
        (True, "format", OutputFormat.PARQUET, "text output only"),
        (True, "freq", 25200, "divides one day"),
        (True, "timestamp_reference", "utc", "forcing clock"),
        (False, "write_by_year", True, "text output only"),
    ],
)
def test_settings_assigned_on_the_config_fail_before_running(
    short_sample_sim, tmp_path, write_by_year, field, value, match
):
    """A config changed by attribute assignment is checked again by run()."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, write_by_year)
    output = sim.config.model.control.output
    if field == "write_by_year":
        output.format = OutputFormat.PARQUET
    setattr(output, field, value)

    with pytest.raises(ValueError, match=match):
        sim.run()
    assert not (tmp_path / "out").exists()


def test_groups_the_model_does_not_write_are_rejected(short_sample_sim, tmp_path):
    """Saving only groups the kernel never writes would write no files."""
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path / "out", 3600, True, groups=["BL"]
    )

    with pytest.raises(ValueError, match="names no group the model writes"):
        sim.run()
    assert not (tmp_path / "out").exists()


def test_cli_run_writes_by_year_once(tmp_path, monkeypatch):
    """``suews run`` honours the switch, writes each file once and records itself."""
    sim = SUEWSSimulation.from_sample_data()
    src = sim._config_path.parent
    cfg_text = (src / "sample_config.yml").read_text(encoding="utf-8")
    forcing_name = sim.config.model.control.forcing.file.value
    lines = (src / forcing_name).read_text(encoding="utf-8").splitlines(keepends=True)
    # Two days of the hourly sample forcing, so the CLI run stays quick.
    (tmp_path / forcing_name).write_text(
        "".join(lines[: 2 * SAMPLE_FORCING_ROWS_PER_DAY + 1]), encoding="utf-8"
    )
    assert 'end_time: "2012-12-31"' in cfg_text
    cfg_text = cfg_text.replace('end_time: "2012-12-31"', 'end_time: "2012-01-02"')
    assert "      write_by_year: false\n" in cfg_text
    cfg_text = cfg_text.replace(
        "      write_by_year: false\n", "      write_by_year: true\n", 1
    )
    cfg_path = tmp_path / "case.yml"
    cfg_path.write_text(cfg_text, encoding="utf-8")

    calls = []
    write_output_frame = SUEWSSimulation._write_output_frame

    def spy(df_output, df_state_final, settings):
        paths = write_output_frame(df_output, df_state_final, settings)
        calls.append(paths)
        return paths

    monkeypatch.setattr(SUEWSSimulation, "_write_output_frame", staticmethod(spy))
    result = CliRunner().invoke(run_cmd, [str(cfg_path)])
    assert result.exit_code == 0, result.output

    # One year, written once, by the run itself rather than again by save().
    assert len(calls) == 1
    dir_out = tmp_path / "Output"
    listed = [
        line.strip()
        for line in result.output.split("have been written out:")[1].splitlines()
        if line.strip().startswith(str(dir_out))
    ]
    assert len(listed) == len(set(listed))
    assert {Path(p).name for p in listed} == {p.name for p in dir_out.iterdir()}
    prov = read_provenance(dir_out)
    assert prov["run"]["interface"] == "cli"
    assert prov["run"]["command"] == "suews run case.yml"
    assert prov["run"]["write_by_year"] is True
