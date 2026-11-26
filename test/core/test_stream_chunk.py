"""Tests for streaming a run's output to disk in chunks, ``output.stream_chunk``.

With ``output.stream_chunk`` set to N days, ``run()`` goes through each
calendar year in chunks of up to N days, writes or extends the text files
after each chunk and keeps no output frame. The files must be byte-identical
to what ``run()`` followed by ``save()`` writes, for any chunk length and in
every allowed clock mode: state crosses each chunk boundary exactly (gh#1912),
and a file is started the first time the run writes it and extended after,
so the period ending at 1 January 00:00, which an explicit output clock saves
into the new year's file, is kept. While a chunk is held only the groups the
text output saves are kept (``_KeptOutputStore``), which must be lossless.

Parquet output streams too, one file per chunk in an output folder. Read
back and sorted by grid and time, the folder must equal, bit for bit, the
chosen groups of the frame a whole run saves as Parquet, DailyState at its
own rows; a resumed run must give the same files.

The returned output holds no rows, so its row accessors raise. The checkpoint
is replaced at the end of each calendar year; a run that fails part-way
resumes from it, trims the files it writes back to what the run up to the
checkpoint produced, and ends with the files of an uninterrupted run.

Runs cross 1 January in a six-day window, three days of each year, so chunks
of 1, 2 and 7 days cut each year into days, unevenly (two days then one) and
not at all. A separate 31-day January run is what splits a single year with
30-day chunks. Where a test used to take every combination of its settings,
it now takes a set of cases covering every pair of values of any two settings
(``_parametrize_cases``), so each value and each pair is still exercised.
"""

import itertools
import json
import logging
import os
from pathlib import Path
import re

from click.testing import CliRunner
from conftest import build_year_crossing_sim
import numpy as np
import pandas as pd
from pydantic import ValidationError
import pytest

from supy import SUEWSCheckpoint, SUEWSSimulation
from supy._provenance import read_provenance
from supy._run_rust import (
    forcing_chunk_keys,
    forcing_year_keys,
    iter_suews_rust_streamed,
)
import supy._save as save_module
from supy._save import StreamedTextFiles, save_df_output
from supy.cmd.SUEWS import SUEWS as run_cmd
from supy.data_model.core.model import ModelControl, OutputControl, OutputFormat
import supy.suews_sim as suews_sim_module

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-RESTART", "C-SAVE", "C-RESAMPLE", "C-CLOCK", "C-VALID"),
]

GROUPS = ["SUEWS", "DailyState", "RSL", "debug"]
GROUPS_NO_DAILY = ["SUEWS", "RSL", "debug"]
SAMPLE_FORCING_ROWS_PER_DAY = 24  # the sample forcing file is hourly
TSTEP_S = 300  # the short sample's model timestep
STEPS_PER_HOUR = 3600 // TSTEP_S
STEPS_PER_DAY = 24 * STEPS_PER_HOUR
# Explicit output clocks: (forcing timestamp_reference, output timestamp_reference)
CLOCK_FOLLOW = ("local_standard_time", "follow")
CLOCK_UTC = ("utc", "utc")
CLOCK_LST = ("local_standard_time", "local_standard_time")

# The year-crossing window: 29 to 31 December 2011 and 1 to 3 January 2012.
# Every simulation built by _year_crossing_sim covers it unless a test asks
# for other first or last rows; reference folders are asked for with the same
# strings, so the session fixtures share one reference run per setting.
WINDOW_START = "2011-12-29 00:05"
WINDOW_END = "2012-01-04 00:00"
WINDOW = {"start": WINDOW_START, "end": WINDOW_END}
N_CHUNK_2011 = 3  # one-day chunks from 29 to 31 December 2011
N_CHUNK_2012 = 3  # one-day chunks from 1 to 3 January 2012
# One-day chunks written by a run that fails after the first chunk of 2012.
N_WRITTEN_MID_YEAR = N_CHUNK_2011 + 1
# Chunk lengths of the year-crossing runs: one day, two days (a two-day chunk
# then a one-day one in each year) and seven days (each year whole).
CHUNKS = {"1": 1, "2": 2, "7": 7}
GRIDS = {"1grid": None, "2grids": [2, 1]}

# Cost markers: the cases reading 10 CPU-s or more (`medium`) or 30 or more
# (`slow`) in the call phase, by the thresholds of .claude/rules/tests/patterns.md.
# Measure again and edit here when these tests get cheaper or dearer.
COST_MARKER = {}


def _uncovered_pairs(axes, cases):
    """Pairs of values of two axes that no case in *cases* takes together.

    *axes* is a sequence of mappings from case-id parts to values; each case
    is a tuple of case-id parts, one per axis.
    """
    list_missing = []
    for (pos_a, axis_a), (pos_b, axis_b) in itertools.combinations(enumerate(axes), 2):
        covered = {(case[pos_a], case[pos_b]) for case in cases}
        list_missing.extend(
            (key_a, key_b)
            for key_a, key_b in itertools.product(axis_a, axis_b)
            if (key_a, key_b) not in covered
        )
    return list_missing


def _parametrize_cases(argnames, *axes, cases=None, cover_pairs=True):
    """Parametrise over *cases*, marked from COST_MARKER.

    Each axis maps case ids to values; each case is a tuple of ids, one per
    axis, and its id joins them with "-". Without *cases*, every combination
    is taken. *cases* must name only values the axes hold and, unless
    *cover_pairs* is false (for a share of a set checked as a whole, such as
    TZ_CASES), cover every pair of values of any two axes, so a pairwise set
    cannot lose a value or a pair unnoticed. A case takes the marker
    COST_MARKER gives its id under the decorated test's name; a COST_MARKER
    id that names no case raises, so a changed axis cannot drop a marker
    unnoticed.
    """
    if cases is None:
        cases = list(itertools.product(*(list(axis) for axis in axes)))
    for case in cases:
        if len(case) != len(axes) or any(
            key not in axis for key, axis in zip(case, axes)
        ):
            raise ValueError(f"case {case} does not name one value per axis")
    missing = _uncovered_pairs(axes, cases) if cover_pairs else []
    if missing:
        raise ValueError(f"cases leave pairs uncovered: {missing}")

    def decorate(func):
        dict_marker = COST_MARKER.get(func.__name__, {})
        list_param = []
        for case in cases:
            case_id = "-".join(case)
            marker = dict_marker.get(case_id)
            list_param.append(
                pytest.param(
                    *(axis[key] for key, axis in zip(case, axes)),
                    marks=[getattr(pytest.mark, marker)] if marker else [],
                    id=case_id,
                )
            )
        unknown = set(dict_marker) - {param.id for param in list_param}
        if unknown:
            raise ValueError(f"{func.__name__}: no case {sorted(unknown)}")
        return pytest.mark.parametrize(argnames, list_param)(func)

    return decorate


# ---------------------------------------------------------------------------
# Chunk keys
# ---------------------------------------------------------------------------


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


@pytest.mark.parametrize("chunk_days", [1, 30, 366])
def test_chunks_start_at_the_first_step_of_a_day(chunk_days):
    """Chunks hold whole days from 00:00 + tstep and at most chunk_days days."""
    step = pd.Timedelta(seconds=TSTEP_S)
    index = pd.date_range(
        pd.Timestamp("2011-12-16 00:00") + step, "2012-03-15 00:00", freq=step
    )

    ar_year, ar_chunk = forcing_chunk_keys(index, TSTEP_S, chunk_days)

    df_key = pd.DataFrame({"year": ar_year, "chunk": ar_chunk}, index=index)
    for (_, _), df_chunk in df_key.groupby(["year", "chunk"]):
        first, last = df_chunk.index[0], df_chunk.index[-1]
        assert first.time() == (pd.Timestamp("2000-01-01") + step).time()
        assert last.time() == pd.Timestamp("2000-01-01").time()
        assert (last - first + step) <= pd.Timedelta(days=chunk_days)
        assert first.year == (last - step).year
    if chunk_days == 366:
        assert df_key.groupby("year")["chunk"].nunique().eq(1).all()


def test_chunk_keys_fold_a_leading_stub():
    """A leading stub joins the first chunk of the year it is folded into."""
    index = pd.date_range("2012-01-01 00:00", "2012-01-04 00:00", freq="h")

    ar_year, ar_chunk = forcing_chunk_keys(index, 3600, 1)

    assert set(ar_year) == {2012}
    assert ar_chunk[0] == ar_chunk[1] == 0
    assert list(np.unique(ar_chunk)) == [0, 1, 2]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _with_replicated_sites(config, list_gridiv):
    """A copy of *config* whose site is replaced by one per *list_gridiv* entry.

    Each copy is 10 degrees further north than the one before, so the grids'
    outputs differ and a block of rows carrying the wrong grid's label would
    not match.
    """
    config = config.model_copy(deep=True)
    site0 = config.sites[0]
    list_site = []
    for pos, gridiv in enumerate(list_gridiv):
        site = site0.model_copy(deep=True)
        site.gridiv = gridiv
        site.name = f"grid{gridiv}"
        site.properties.lat.value += 10.0 * pos
        list_site.append(site)
    config.sites = list_site
    return config


def _replicate_sites(sim, list_gridiv):
    """Replace the site of *sim* by one per *list_gridiv* entry (see above)."""
    config = _with_replicated_sites(sim.config, list_gridiv)
    sim.update_config(config, auto_load_forcing=False)


def _year_crossing_forcing(sim, end=WINDOW_END, start=WINDOW_START):
    """The January short sample relabelled to run across 1 January.

    The forcing starts and stops at the *start* and *end* rows, by default
    those of the six-day window.
    """
    forcing = sim.forcing.df.copy()
    forcing.index -= pd.Timedelta(days=16)
    forcing.index.freq = forcing.index[1] - forcing.index[0]
    if start is not None:
        forcing = forcing.loc[pd.Timestamp(start) :]
    if end is not None:
        forcing = forcing.loc[: pd.Timestamp(end)]
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
    stream_chunk,
    list_gridiv=None,
    groups=GROUPS,
    clock=CLOCK_FOLLOW,
    end=WINDOW_END,
    start=WINDOW_START,
    site_clock=None,
    fmt="txt",
):
    """A simulation across 1 January saving *groups* to *path_out*.

    Runs the six-day window unless *start* or *end* says otherwise.
    Delegates to ``conftest.build_year_crossing_sim`` so tests and the
    session-scoped reference fixtures construct identical simulations.
    """
    return build_year_crossing_sim(
        short_sample_sim,
        path_out,
        freq_s,
        stream_chunk,
        list_gridiv=list_gridiv,
        groups=groups,
        clock=clock,
        end=end,
        start=start,
        site_clock=site_clock,
        fmt=fmt,
    )


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

    Only the checkpoint's creation time and ``provenance.json`` record when
    they ran.
    """
    names_a = sorted(p.name for p in dir_a.iterdir())
    names_b = sorted(p.name for p in dir_b.iterdir())
    assert names_a == names_b
    for name in names_a:
        if name.endswith((".txt", ".units.json")):
            assert (dir_a / name).read_bytes() == (dir_b / name).read_bytes(), name
        elif name.endswith("_checkpoint.json"):
            ckpt_a = json.loads((dir_a / name).read_text(encoding="utf-8"))
            ckpt_b = json.loads((dir_b / name).read_text(encoding="utf-8"))
            assert ckpt_a["last_timestamp"] == ckpt_b["last_timestamp"]
            assert list(_numbers(ckpt_a)) == list(_numbers(ckpt_b)), name


# ---------------------------------------------------------------------------
# Byte identity with run() + save()
# ---------------------------------------------------------------------------


def test_pairwise_cases_must_cover_every_pair():
    """A case set leaving a pair of values untested, or naming no value, raises."""
    axes = ({"a": 1, "b": 2}, {"x": 1, "y": 2}, {"p": 1, "q": 2})
    pairwise = [("a", "x", "p"), ("a", "y", "q"), ("b", "x", "q"), ("b", "y", "p")]

    assert _uncovered_pairs(axes, pairwise) == []
    assert _uncovered_pairs(axes, pairwise[:3]) == [("b", "y"), ("b", "p"), ("y", "p")]
    with pytest.raises(ValueError, match="uncovered"):
        _parametrize_cases(("i", "j", "k"), *axes, cases=pairwise[:3])
    with pytest.raises(ValueError, match="one value per axis"):
        _parametrize_cases(("i", "j", "k"), *axes, cases=[*pairwise, ("a", "z", "p")])


# Chunk length, output frequency, grids and groups: each pair of values once.
@_parametrize_cases(
    ("stream_chunk", "freq_s", "list_gridiv", "groups"),
    CHUNKS,
    {"3600": 3600, "300": TSTEP_S},
    GRIDS,
    {"daily": GROUPS, "nodaily": GROUPS_NO_DAILY},
    cases=[
        ("1", "3600", "1grid", "daily"),
        ("1", "300", "2grids", "nodaily"),
        ("2", "3600", "2grids", "nodaily"),
        ("2", "300", "1grid", "daily"),
        ("7", "3600", "1grid", "nodaily"),
        ("7", "300", "2grids", "daily"),
    ],
)
def test_files_match_run_then_save(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    stream_chunk,
    freq_s,
    list_gridiv,
    groups,
):
    """Streamed files match run() + save(), file for file, in follow mode."""
    dir_whole = stream_chunk_reference_dir(
        freq_s, list_gridiv, groups, CLOCK_FOLLOW, **WINDOW
    )
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, freq_s, stream_chunk, list_gridiv, groups
    )
    output = sim.run()

    names = [p.name for p in dir_whole.iterdir()]
    assert any("_2011_" in name for name in names)
    assert any("_2012_" in name for name in names)
    _compare_dirs(tmp_path, dir_whole)
    assert output.groups == [g for g in GROUPS if g in groups]
    assert read_provenance(tmp_path)["run"]["stream_chunk"] == stream_chunk
    assert "stream_chunk" not in read_provenance(dir_whole)["run"]


# Chunk length, output clock and grids across text and Parquet: every pair of
# values of any two of the four axes appears at least once.
CLOCKS_EXPLICIT = {"utc": CLOCK_UTC, "lst": CLOCK_LST}
EXPLICIT_CLOCK_FORMATS = {"txt": "txt", "parquet": "parquet"}
EXPLICIT_CLOCK_CASES = [
    ("txt", "1", "utc", "1grid"),
    ("txt", "2", "lst", "1grid"),
    ("txt", "7", "utc", "2grids"),
    ("parquet", "1", "lst", "2grids"),
    ("parquet", "2", "utc", "2grids"),
    ("parquet", "7", "lst", "1grid"),
]
if _uncovered_pairs(
    (EXPLICIT_CLOCK_FORMATS, CHUNKS, CLOCKS_EXPLICIT, GRIDS), EXPLICIT_CLOCK_CASES
):
    raise ValueError(
        "EXPLICIT_CLOCK_CASES must cover every pair of format, chunk, clock and grids"
    )


def _explicit_clock_cases(fmt):
    """The (stream_chunk, clock, grids) cases of *fmt* in EXPLICIT_CLOCK_CASES."""
    return [case[1:] for case in EXPLICIT_CLOCK_CASES if case[0] == fmt]


@_parametrize_cases(
    ("stream_chunk", "clock", "list_gridiv"),
    CHUNKS,
    CLOCKS_EXPLICIT,
    GRIDS,
    cases=_explicit_clock_cases("txt"),
    cover_pairs=False,
)
def test_explicit_clock_files_match_run_then_save(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    stream_chunk,
    clock,
    list_gridiv,
):
    """With timestamps labelled by period end, the 1 January 00:00 row is kept.

    That row closes the previous year and is computed in its last chunk, but
    is saved into the new year's file.
    """
    dir_whole = stream_chunk_reference_dir(3600, list_gridiv, GROUPS, clock, **WINDOW)
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, stream_chunk, list_gridiv, clock=clock
    )
    sim.run()

    list_path_2012 = list(dir_whole.glob("*_2012_SUEWS_60_*.txt"))
    assert len(list_path_2012) == (1 if list_gridiv is None else 2)
    for path in list_path_2012:
        first_row = _data_lines(path)[0]
        assert first_row.split("\t")[:4] == ["2012", "  1", "   0", "  0"]
    _compare_dirs(tmp_path, dir_whole)


@pytest.mark.parametrize("clock", [CLOCK_FOLLOW, CLOCK_UTC], ids=["follow", "utc"])
def test_year_end_stub_of_one_row_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, clock
):
    """A run ending one hour into the new year saves that hour once, as save() does."""
    end = "2012-01-01 01:00"
    dir_whole = stream_chunk_reference_dir(
        3600, None, GROUPS, clock, end=end, start=WINDOW_START
    )
    sim = _year_crossing_sim(short_sample_sim, tmp_path, 3600, 7, clock=clock, end=end)
    sim.run()

    _compare_dirs(tmp_path, dir_whole)


def _january_sim(short_sample_sim, path_out, stream_chunk):
    """The 31-day January short sample, unshifted, saving to *path_out*.

    The output block is the one ``build_year_crossing_sim`` sets, for SUEWS
    and DailyState at hourly frequency.
    """
    sim = short_sample_sim()
    forcing = sim.forcing.df
    output = {
        "format": "txt",
        "freq": 3600,
        "dir": str(path_out),
        "groups": ["SUEWS", "DailyState"],
        "include_provisional": False,
        "timestamp_reference": CLOCK_FOLLOW[1],
    }
    if stream_chunk is not None:
        output["stream_chunk"] = stream_chunk
    sim.update_config(
        {
            "model": {
                "control": {
                    "start_time": str(forcing.index[0]),
                    "end_time": str(forcing.index[-1]),
                    "forcing": {"timestamp_reference": CLOCK_FOLLOW[0]},
                    "output": output,
                }
            }
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(forcing)
    return sim


def test_january_in_two_chunks_matches_run_then_save(short_sample_sim, tmp_path):
    """Thirty-day chunks cut 31 days of January into 30 days and one.

    In the year-crossing window every year fits in one 30-day chunk; this
    run is where stream_chunk 30 splits a year, so state crosses a chunk
    boundary inside it and the year's files are extended.
    """
    dir_whole, dir_streamed = tmp_path / "whole", tmp_path / "streamed"
    sim_whole = _january_sim(short_sample_sim, dir_whole, None)
    sim_whole.run()
    sim_whole.save()
    sim = _january_sim(short_sample_sim, dir_streamed, 30)
    index = sim.forcing.df.index
    assert (index[0], index[-1]) == (
        pd.Timestamp("2012-01-01 00:05"),
        pd.Timestamp("2012-02-01 00:00"),
    )

    ar_year, ar_chunk = forcing_chunk_keys(index, TSTEP_S, 30)
    sim.run()

    ser_size = pd.Series(ar_year).groupby([ar_year, ar_chunk]).size()
    assert ser_size.to_dict() == {
        (2012, 0): 30 * STEPS_PER_DAY,
        (2012, 1): STEPS_PER_DAY,
    }
    _compare_dirs(dir_streamed, dir_whole)
    [path_ckpt] = dir_streamed.glob("*_checkpoint.json")
    ckpt = SUEWSCheckpoint.from_file(path_ckpt)
    assert pd.Timestamp(ckpt.last_timestamp) == index[-1]


@pytest.mark.parametrize("tail_rows", [1, 2])
def test_text_short_last_chunk_at_the_model_timestep(
    two_day_output, tmp_path, tail_rows
):
    """Short text tails keep missing sums exactly as a whole run saves them."""
    # ARRANGE: real output for two grids, with missing rainfall in the tail.
    df_output, df_state = two_day_output
    dates = df_output.index.get_level_values("datetime")
    list_dates = dates.unique()[: 3 + tail_rows]
    df_output = df_output.loc[dates.isin(list_dates)].copy()
    tail = df_output.index.get_level_values("datetime") > list_dates[2]
    df_output.loc[tail, ("SUEWS", "Rain")] = np.nan
    streamed = StreamedTextFiles()

    # ACT: write the same frame whole and in two successive chunks.
    _save(df_output, df_state, tmp_path / "whole", freq_s=TSTEP_S)
    for df_chunk in (df_output.loc[~tail], df_output.loc[tail]):
        _save(
            df_chunk,
            df_state,
            tmp_path / "chunked",
            freq_s=TSTEP_S,
            streamed_files=streamed,
        )

    # ASSERT: the missing-value representation and every saved byte agree.
    _compare_dirs(tmp_path / "chunked", tmp_path / "whole")


def test_each_chunk_keeps_only_the_saved_groups(
    short_sample_sim, tmp_path, monkeypatch
):
    """The frame each chunk hands to the save code holds only the saved groups."""
    seen = []
    write_output_frame = SUEWSSimulation._write_output_frame

    def spy(df_output, df_state_final, settings, streamed_files=None):
        seen.append(df_output.columns.get_level_values("group").unique().tolist())
        return write_output_frame(df_output, df_state_final, settings, streamed_files)

    monkeypatch.setattr(SUEWSSimulation, "_write_output_frame", staticmethod(spy))
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, 30, groups=["debug", "SUEWS"]
    )
    sim.run()

    assert seen == [["SUEWS", "debug"]] * 2


@pytest.mark.parametrize("with_state", [False, True], ids=["fresh", "from-state"])
@pytest.mark.parametrize("list_gridiv", [None, [2, 1]], ids=["1grid", "2grids"])
def test_kept_groups_equal_the_full_chunk_frames(
    short_sample_sim, list_gridiv, with_state
):
    """Each chunk's kept frame equals the full frame restricted to the kept groups.

    Covers grids listed out of ID order, at different latitudes, and runs
    starting from saved states, so each grid's rows land in its own block of
    the kept store. One-day chunks across two days per year keep four chunks;
    starting from saved state begins a day earlier so the remaining 23 hours
    fold into 30 December and still split each year in two.
    """
    sim = short_sample_sim()
    start = "2011-12-29 00:05" if with_state else "2011-12-30 00:05"
    forcing = _year_crossing_forcing(sim, start=start, end="2012-01-03 00:00")
    if list_gridiv is not None:
        _replicate_sites(sim, list_gridiv)
    config = sim.config
    dict_state = None
    if with_state:
        sim.update_forcing(forcing.iloc[:12])
        sim.run(start_date=forcing.index[0], end_date=forcing.index[11])
        dict_state = sim.checkpoint.grid_states
        forcing = forcing.iloc[12:]

    list_keep = ["SUEWS", "DailyState", "RSL"]
    kw = {"initial_state_json_by_grid": dict_state}
    list_full = list(iter_suews_rust_streamed(config, forcing, TSTEP_S, 1, **kw))
    list_kept = list(
        iter_suews_rust_streamed(
            config, forcing, TSTEP_S, 1, keep_groups=list_keep, **kw
        )
    )

    assert [(c.year, c.chunk, c.n_chunk) for c in list_full] == [
        (2011, 1, 2),
        (2011, 2, 2),
        (2012, 1, 2),
        (2012, 2, 2),
    ]
    for full, kept in zip(list_full, list_kept):
        assert full.dict_state_json == kept.dict_state_json
        assert full[3:] == kept[3:]
        df_kept, df_full = kept.df_output, full.df_output
        assert df_kept.columns.get_level_values("group").unique().tolist() == list_keep
        pd.testing.assert_frame_equal(
            df_kept,
            df_full.loc[:, df_full.columns.get_level_values("group").isin(list_keep)],
            check_exact=True,
        )
        if list_gridiv is not None:
            assert df_kept.index.get_level_values("grid").unique().tolist() == [1, 2]


def test_one_chunk_per_year_at_366(short_sample_sim):
    """stream_chunk 366 runs each calendar year as one chunk."""
    sim = short_sample_sim()
    forcing = _year_crossing_forcing(sim)

    list_chunk = [
        (c.year, c.chunk, c.n_chunk, c.first_timestamp, c.last_timestamp)
        for c in iter_suews_rust_streamed(sim.config, forcing, TSTEP_S, 366)
    ]

    assert list_chunk == [
        (2011, 1, 1, forcing.index[0], pd.Timestamp("2012-01-01 00:00")),
        (2012, 1, 1, pd.Timestamp("2012-01-01 00:05"), forcing.index[-1]),
    ]


# ---------------------------------------------------------------------------
# Save helpers with a table of a single row
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def two_day_output(short_sample_sim):
    """Output and final state of two days of two asymmetric grids."""
    sim = short_sample_sim()
    _replicate_sites(sim, [2, 1])
    forcing = sim.forcing.df.iloc[: 2 * 24 * STEPS_PER_HOUR]
    sim.update_forcing(forcing)
    output = sim.run(start_date=forcing.index[0], end_date=forcing.index[-1])
    return output.df, sim.state_final


def _save(df_output, df_state_final, path, freq_s=3600, streamed_files=None, grid=None):
    """save_df_output of SUEWS and DailyState, optionally for one grid."""
    if grid is not None:
        df_output = df_output.loc[[grid]]
    Path(path).mkdir(parents=True, exist_ok=True)
    return save_df_output(
        df_output,
        freq_s=freq_s,
        site="t",
        path_dir_save=path,
        output_groups=["SUEWS", "DailyState"],
        df_state_final=df_state_final,
        streamed_files=streamed_files,
    )


def _data_lines(path):
    return Path(path).read_text(encoding="utf-8").splitlines()[1:]


@pytest.mark.parametrize("grid", [1, None], ids=["1grid", "2grids"])
def test_one_resampled_row_is_saved_like_the_first_row_of_a_longer_run(
    two_day_output, tmp_path, grid
):
    """One hour of output saves its single hourly row under the right label.

    Covers the row moved to the start of its period without a frequency
    (``_save_row_index``), the year list for one grid (``_save_groups``),
    the move back to its end (``_gen_df_year``) and the file name frequency
    (``save_df_grid_group``).
    """
    df_output, df_state_final = two_day_output
    df_hour = df_output.loc[
        df_output.index.get_level_values("datetime")
        <= df_output.index.get_level_values("datetime")[0] + pd.Timedelta(minutes=55)
    ]
    _save(df_output, df_state_final, tmp_path / "long", grid=grid)

    list_path = _save(df_hour, df_state_final, tmp_path / "one", grid=grid)

    list_suews = sorted(p for p in list_path if "SUEWS" in p.name)
    assert len(list_suews) == (1 if grid is not None else 2)
    for path in list_suews:
        assert path.name.endswith("_SUEWS_60.txt")
        assert _data_lines(path) == _data_lines(tmp_path / "long" / path.name)[:1]


def test_one_daily_row_is_named_dailystate_and_extended(two_day_output, tmp_path):
    """A one-day table writes DailyState.txt, and the next day extends it.

    A single DailyState row once fell back to a five-minute frequency, wrote
    ``DailyState_5.txt`` and renamed it over the year's file.
    """
    df_output, df_state_final = two_day_output
    ar_dt = df_output.index.get_level_values("datetime")
    day_end = ar_dt[0].normalize() + pd.Timedelta(days=1)
    df_day1, df_day2 = df_output.loc[ar_dt <= day_end], df_output.loc[ar_dt > day_end]
    _save(df_output, df_state_final, tmp_path / "whole")
    streamed = StreamedTextFiles()

    for df_day in (df_day1, df_day2):
        _save(df_day, df_state_final, tmp_path / "days", streamed_files=streamed)

    names = sorted(p.name for p in (tmp_path / "days").iterdir())
    assert names == sorted(p.name for p in (tmp_path / "whole").iterdir())
    assert not any("DailyState_5" in name for name in names)
    for name in names:
        assert (tmp_path / "days" / name).read_bytes() == (
            tmp_path / "whole" / name
        ).read_bytes(), name


def test_resume_trims_a_file_back_to_the_checkpoint(two_day_output, tmp_path):
    """On resume, rows after the checkpoint are dropped before appending, twice over."""
    df_output, df_state_final = two_day_output
    ar_dt = df_output.index.get_level_values("datetime")
    day_end = ar_dt[0].normalize() + pd.Timedelta(days=1)
    df_day2 = df_output.loc[ar_dt > day_end]
    _save(df_output, df_state_final, tmp_path / "whole")
    # A first attempt wrote day 1 and then half of day 2 before stopping.
    _save(
        df_output.loc[ar_dt <= day_end + pd.Timedelta(hours=12)],
        df_state_final,
        tmp_path / "out",
    )

    for _ in range(2):
        resumed = StreamedTextFiles(resume_after=day_end)
        _save(df_day2, df_state_final, tmp_path / "out", streamed_files=resumed)

    for path in (tmp_path / "whole").iterdir():
        assert (tmp_path / "out" / path.name).read_bytes() == path.read_bytes(), (
            path.name
        )


# ---------------------------------------------------------------------------
# Logging, row access, save afterwards, checkpoint and resume
# ---------------------------------------------------------------------------


def _collect_log(sim, prefix="stream_chunk:"):
    """Run *sim* and return the SuPy log messages starting with *prefix*."""
    list_msg = []

    class _Collect(logging.Handler):
        def emit(self, record):
            if record.getMessage().startswith(prefix):
                list_msg.append(record.getMessage())

    logger = logging.getLogger("SuPy")
    handler = _Collect(level=logging.INFO)
    level = logger.level
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    try:
        sim.run()
    finally:
        logger.removeHandler(handler)
        logger.setLevel(level)
    return list_msg


def test_logs_the_folder_and_each_chunk(short_sample_sim, tmp_path):
    """The run says once where the output goes, then reports each chunk written."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, 30)
    first, last = sim.forcing.df.index[0], sim.forcing.df.index[-1]

    list_msg = _collect_log(sim)

    folder = dir_out.resolve()
    n_primary_files = sum(not p.name.endswith(".units.json") for p in folder.iterdir())
    assert list_msg == [
        f"stream_chunk: writing the output to {folder} in chunks of up to 30 days; "
        "it is not kept in memory, and chunk_day is ignored.",
        f"stream_chunk: wrote 2011 chunk 1 of 1 ({first} to 2012-01-01 00:00:00) "
        f"to {folder}",
        f"stream_chunk: wrote 2012 chunk 1 of 1 (2012-01-01 00:05:00 to {last}) "
        f"to {folder}",
        f"stream_chunk: finished writing {n_primary_files} files to "
        f"{folder}; the returned output holds no rows.",
    ]
    assert all(msg.isascii() for msg in list_msg)


def test_long_in_memory_run_hints_at_stream_chunk(short_sample_sim, monkeypatch):
    """A run longer than two years without stream_chunk suggests it, once."""
    sim = short_sample_sim()
    df = sim.forcing.df
    step = df.index[1] - df.index[0]
    index = pd.date_range(
        df.index[0], periods=int(pd.Timedelta(days=740) / step), freq=step
    )
    forcing = pd.DataFrame(
        np.resize(df.to_numpy(), (len(index), df.shape[1])),
        index=index,
        columns=df.columns,
    )
    for col, values in (
        ("iy", index.year),
        ("id", index.day_of_year),
        ("it", index.hour),
        ("imin", index.minute),
    ):
        forcing[col] = values
    sim.update_config(
        {
            "model": {
                "control": {"start_time": str(index[0]), "end_time": str(index[-1])}
            }
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(forcing)

    class _Stop(Exception):
        pass

    def stop(**kwargs):
        raise _Stop

    monkeypatch.setattr(suews_sim_module, "run_suews_rust_chunked", stop)
    list_msg = []

    class _Collect(logging.Handler):
        def emit(self, record):
            if "output.stream_chunk" in record.getMessage():
                list_msg.append(record.getMessage())

    logger = logging.getLogger("SuPy")
    handler = _Collect(level=logging.INFO)
    level = logger.level
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    try:
        with pytest.raises(_Stop):
            sim.run(_validate_forcing=False)
    finally:
        logger.removeHandler(handler)
        logger.setLevel(level)

    assert len(list_msg) == 1
    assert "more than two years" in list_msg[0]


def test_output_raises_on_row_access(short_sample_sim, tmp_path):
    """Row accessors of a streamed output raise and name the folder."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, 30)
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
        with pytest.raises(RuntimeError, match="stream_chunk"):
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
    assert "streamed to" in repr(output)
    assert "streamed to" in repr(sim)


def test_checkpoint_is_written_only_at_year_ends(
    short_sample_sim, tmp_path, monkeypatch
):
    """With one-day chunks the checkpoint is replaced at each year end only."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path, 3600, 1)
    written = []
    write_year_checkpoint = SUEWSSimulation._write_year_checkpoint

    def spy(dict_state_json, last_timestamp, settings):
        path = write_year_checkpoint(dict_state_json, last_timestamp, settings)
        written.append(pd.Timestamp(last_timestamp))
        return path

    monkeypatch.setattr(SUEWSSimulation, "_write_year_checkpoint", staticmethod(spy))
    sim.run()

    assert written == [pd.Timestamp("2012-01-01 00:00"), sim.forcing.df.index[-1]]


def _run_failing_after(sim, n_chunk, monkeypatch):
    """Run *sim*, failing once *n_chunk* chunks have been written."""
    iter_streamed = suews_sim_module.iter_suews_rust_streamed

    def fail_after(*args, **kwargs):
        chunks = iter_streamed(*args, **kwargs)
        for _ in range(n_chunk):
            yield next(chunks)
        raise RuntimeError("simulated failure")

    monkeypatch.setattr(suews_sim_module, "iter_suews_rust_streamed", fail_after)
    with pytest.raises(RuntimeError, match="simulated failure"):
        sim.run()
    monkeypatch.undo()


def _resume(sim, dir_out, monkeypatch=None, fail_after=None):
    """Resume *sim*'s run from the checkpoint it left in *dir_out*."""
    [path_ckpt] = list(dir_out.glob("*_checkpoint.json"))
    ckpt = SUEWSCheckpoint.from_file(path_ckpt)
    sim_resume = SUEWSSimulation.from_checkpoint(sim.config, ckpt)
    sim_resume.update_forcing(sim.forcing.df)
    start = pd.Timestamp(ckpt.last_timestamp) + pd.Timedelta(seconds=TSTEP_S)
    if fail_after is None:
        sim_resume.run(start_date=start)
        return
    iter_streamed = suews_sim_module.iter_suews_rust_streamed

    def failing(*args, **kwargs):
        chunks = iter_streamed(*args, **kwargs)
        for _ in range(fail_after):
            yield next(chunks)
        raise RuntimeError("simulated failure")

    monkeypatch.setattr(suews_sim_module, "iter_suews_rust_streamed", failing)
    with pytest.raises(RuntimeError, match="simulated failure"):
        sim_resume.run(start_date=start)
    monkeypatch.undo()


@_parametrize_cases(
    ("n_written", "clock"),
    {"at-year-end": N_CHUNK_2011, "mid-year": N_WRITTEN_MID_YEAR},
    {"follow": CLOCK_FOLLOW, "utc": CLOCK_UTC, "lst": CLOCK_LST},
)
def test_resumed_run_matches_an_uninterrupted_run(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    monkeypatch,
    clock,
    n_written,
):
    """A run killed at or after a year end, then resumed, writes the same files.

    The mid-year case is resumed twice, the first resume itself failing part
    of the way, past the point the first attempt reached, so trimming back to
    the checkpoint must be repeatable.
    """
    # Text output is byte-identical between streamed and whole runs, so the
    # cached unstreamed reference dirs serve resume tests without rebuilding.
    dir_full = stream_chunk_reference_dir(3600, None, GROUPS, clock, **WINDOW)

    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, 1, clock=clock)
    _run_failing_after(sim, n_written, monkeypatch)
    names = {p.name for p in dir_out.iterdir()}
    assert "provenance.json" not in names
    assert not any(name.endswith(".tmp") for name in names)
    ckpt = SUEWSCheckpoint.from_file(next(dir_out.glob("*_checkpoint.json")))
    assert pd.Timestamp(ckpt.last_timestamp) == pd.Timestamp("2012-01-01 00:00")

    if n_written > N_CHUNK_2011:
        _resume(sim, dir_out, monkeypatch, fail_after=N_CHUNK_2012 - 1)
    _resume(sim, dir_out)

    _compare_dirs(dir_out, dir_full)


def test_save_afterwards_returns_the_written_files(short_sample_sim, tmp_path):
    """save() after a streamed run reports the files; it cannot rewrite them."""
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, 30)
    sim.run()
    dict_bytes = {p.name: p.read_bytes() for p in dir_out.iterdir()}

    paths = sim.save()

    assert {Path(p).name for p in paths} == {
        name for name in dict_bytes if not name.endswith(".units.json")
    }
    assert len(paths) == len(set(paths))
    # Nothing was written again.
    assert {p.name: p.read_bytes() for p in dir_out.iterdir()} == dict_bytes
    with pytest.raises(ValueError, match="streamed its output"):
        sim.save(tmp_path / "elsewhere")
    with pytest.raises(ValueError, match="streamed its output"):
        sim.save(format="parquet")

    # A command line given to save() reaches the provenance sidecar only.
    sim.save(command="suews run case.yml")
    assert read_provenance(dir_out)["run"]["command"] == "suews run case.yml"
    for name, data in dict_bytes.items():
        if name != "provenance.json":
            assert (dir_out / name).read_bytes() == data, name


def test_default_run_does_not_stream(short_sample_sim, tmp_path, monkeypatch):
    """With stream_chunk unset, run() takes the in-memory path and writes nothing."""

    def no_stream(*args, **kwargs):
        raise AssertionError("the streamed loop must not run when unset")

    monkeypatch.setattr(suews_sim_module, "iter_suews_rust_streamed", no_stream)
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, None)
    output = sim.run()

    assert not (tmp_path / "out").exists()
    assert not output.df.empty


# ---------------------------------------------------------------------------
# Review round 1: year of single-row files, stubs, sidecars, headers, clocks
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("stream_chunk", list(CHUNKS.values()), ids=list(CHUNKS))
def test_daily_output_across_new_year_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, stream_chunk
):
    """Daily output: 31 December's row stays in the old year's file.

    With one- and two-day chunks the last chunk of 2011 holds that single
    row, labelled 1 January 00:00; the file it goes to is the year the save
    selected it for, not the year of its label.
    """
    dir_whole = stream_chunk_reference_dir(86400, None, GROUPS, CLOCK_FOLLOW, **WINDOW)
    sim = _year_crossing_sim(short_sample_sim, tmp_path, 86400, stream_chunk)
    sim.run()

    _compare_dirs(tmp_path, dir_whole)
    path_2011 = next(dir_whole.glob("*_2011_SUEWS_1440.txt"))
    assert _data_lines(path_2011)[-1].split("\t")[:4] == ["2012", "  1", "   0", "  0"]


@pytest.mark.parametrize("clock", [CLOCK_FOLLOW, CLOCK_UTC], ids=["follow", "utc"])
@pytest.mark.parametrize(
    ("start", "end"),
    [
        ("2012-01-01 00:00", "2012-01-03 00:00"),
        ("2011-12-30 00:00", "2012-01-02 00:00"),
    ],
    ids=["new-year", "mid-year"],
)
def test_run_starting_at_midnight_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, start, end, clock
):
    """A run whose first row closes the previous day saves like run() + save()."""
    dir_whole = stream_chunk_reference_dir(
        3600, None, GROUPS, clock, end=end, start=start
    )
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, 1, clock=clock, start=start, end=end
    )
    sim.run()

    _compare_dirs(tmp_path, dir_whole)
    if start.startswith("2012-01-01") and clock == CLOCK_FOLLOW:
        # The first hour, (31 December 23:00, 1 January 00:00], is kept in its
        # own year's file rather than overwritten by the next year's.
        [path_2011] = list(dir_whole.glob("*_2011_SUEWS_60.txt"))
        assert len(_data_lines(path_2011)) == 1


def test_leading_midnight_stub_joins_the_first_chunk():
    """A mid-year run starting at 00:00 has no one-step first chunk."""
    index = pd.date_range("2012-06-01 00:00", "2012-06-04 00:00", freq="h")

    _, ar_chunk = forcing_chunk_keys(index, 3600, 1)

    assert list(ar_chunk[:26]) == [0] * 25 + [1]
    assert list(np.unique(ar_chunk)) == [0, 1, 2]


def test_checkpoint_file_is_replaced_atomically(tmp_path, monkeypatch):
    """An interrupted checkpoint write leaves the previous checkpoint intact."""
    path = tmp_path / "site_SUEWS_checkpoint.json"
    old = SUEWSCheckpoint.from_grid_states({1: "{}"}, last_timestamp="2012-01-01")
    old.to_file(path)
    text_old = path.read_text(encoding="utf-8")
    new = SUEWSCheckpoint.from_grid_states({1: "{}"}, last_timestamp="2013-01-01")

    def fail(src, dst):
        raise OSError("interrupted")

    monkeypatch.setattr("supy.suews_checkpoint.os.replace", fail)
    with pytest.raises(OSError, match="interrupted"):
        new.to_file(path)

    assert path.read_text(encoding="utf-8") == text_old


def test_final_checkpoint_is_written_once(short_sample_sim, tmp_path, monkeypatch):
    """A streamed run writes its checkpoint at each year end and not again."""
    calls = []
    to_file = SUEWSCheckpoint.to_file

    def spy(self, path):
        calls.append(Path(path).name)
        return to_file(self, path)

    monkeypatch.setattr(SUEWSCheckpoint, "to_file", spy)
    sim = _year_crossing_sim(short_sample_sim, tmp_path, 3600, 30)
    sim.run()

    assert len(calls) == 2
    assert {Path(p).name for p in sim.save()} >= set(calls)


def test_fresh_run_removes_a_previous_runs_sidecars(
    short_sample_sim, tmp_path, monkeypatch
):
    """Starting afresh in a used folder, an early failure leaves no stale checkpoint."""
    _year_crossing_sim(short_sample_sim, tmp_path, 3600, 30).run()
    assert list(tmp_path.glob("*_checkpoint.json"))
    assert (tmp_path / "provenance.json").exists()

    sim = _year_crossing_sim(short_sample_sim, tmp_path, 3600, 1)
    # Stopped inside 2011, before its year-end checkpoint.
    _run_failing_after(sim, N_CHUNK_2011 - 1, monkeypatch)

    assert not list(tmp_path.glob("*_checkpoint.json"))
    assert not (tmp_path / "provenance.json").exists()


@pytest.mark.parametrize("damage", ["empty", "partial-header"])
def test_resume_rewrites_a_file_without_a_whole_header(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, monkeypatch, damage
):
    """A file left with no header, or half of one, is written anew on resume."""
    dir_full = stream_chunk_reference_dir(3600, None, GROUPS, CLOCK_FOLLOW, **WINDOW)
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(short_sample_sim, dir_out, 3600, 1)
    _run_failing_after(sim, N_WRITTEN_MID_YEAR, monkeypatch)
    path = next(dir_out.glob("*_2012_SUEWS_60.txt"))
    text = path.read_text(encoding="utf-8")
    path.write_text(
        "" if damage == "empty" else text[: text.index("\n") // 2], encoding="utf-8"
    )

    _resume(sim, dir_out)

    _compare_dirs(dir_out, dir_full)


SITE_CLOCKS = {
    "tz+8": (8, 300, 80),
    "tz-5": (-5, 300, 80),
}
# The time-zone cases of the three tests that take them, as (test, output
# clock, site clock). Between them every pair of values of the three appears,
# so each test takes two of the four (clock, time zone) pairs and the tests
# together all four.
TZ_TESTS = {"txt": "txt", "txt-resume": "txt-resume", "parquet": "parquet"}
TZ_CASES = [
    ("txt", "utc", "tz+8"),
    ("txt", "lst", "tz-5"),
    ("txt-resume", "utc", "tz-5"),
    ("txt-resume", "lst", "tz+8"),
    ("parquet", "utc", "tz-5"),
    ("parquet", "lst", "tz+8"),
]
if _uncovered_pairs((TZ_TESTS, CLOCKS_EXPLICIT, SITE_CLOCKS), TZ_CASES):
    raise ValueError("TZ_CASES must cover every pair of test, clock and time zone")


def _tz_cases(test):
    """The (clock, site clock) cases of *test* in TZ_CASES."""
    return [case[1:] for case in TZ_CASES if case[0] == test]


@_parametrize_cases(
    ("clock", "site_clock"),
    CLOCKS_EXPLICIT,
    SITE_CLOCKS,
    cases=_tz_cases("txt"),
    cover_pairs=False,
)
def test_non_zero_time_zone_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, clock, site_clock
):
    """With a time zone and a daylight-saving window, streamed equals whole."""
    dir_whole = stream_chunk_reference_dir(
        3600, [2, 1], GROUPS, clock, **WINDOW, site_clock=site_clock
    )
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, 7, [2, 1], clock=clock, site_clock=site_clock
    )
    sim.run()

    _compare_dirs(tmp_path, dir_whole)


@_parametrize_cases(
    ("clock", "site_clock"),
    CLOCKS_EXPLICIT,
    SITE_CLOCKS,
    cases=_tz_cases("txt-resume"),
    cover_pairs=False,
)
def test_non_zero_time_zone_resume_matches_an_uninterrupted_run(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    monkeypatch,
    clock,
    site_clock,
):
    """Killed mid-year and resumed with a time zone, the files equal a whole run."""
    dir_full = stream_chunk_reference_dir(
        3600, None, GROUPS, clock, **WINDOW, site_clock=site_clock
    )
    dir_out = tmp_path / "out"
    sim = _year_crossing_sim(
        short_sample_sim, dir_out, 3600, 1, clock=clock, site_clock=site_clock
    )
    _run_failing_after(sim, N_WRITTEN_MID_YEAR, monkeypatch)

    _resume(sim, dir_out)

    _compare_dirs(dir_out, dir_full)


# ---------------------------------------------------------------------------
# Parquet output
# ---------------------------------------------------------------------------

GROUPS_PARQUET = ["SUEWS", "DailyState", "RSL"]


def _parquet_sim(short_sample_sim, path_out, freq_s, stream_chunk, **kwargs):
    """``_year_crossing_sim`` saving GROUPS_PARQUET as Parquet."""
    kwargs.setdefault("groups", GROUPS_PARQUET)
    return _year_crossing_sim(
        short_sample_sim, path_out, freq_s, stream_chunk, fmt="parquet", **kwargs
    )


def _output_path(path_dir):
    """The Parquet output (file or folder) in *path_dir*."""
    [path] = path_dir.glob("*SUEWS_output*.parquet")
    return path


def _part_names(path_dir):
    """Names of the part files in the streamed output folder in *path_dir*."""
    path = _output_path(path_dir)
    assert path.is_dir()
    return sorted(p.name for p in path.iterdir())


def _read_streamed(path_dir):
    """The streamed folder read back as one frame, sorted by grid and time."""
    return pd.read_parquet(_output_path(path_dir)).sort_index()


def _expected_streamed(df_whole, groups, freq_s, tstep_s=TSTEP_S):
    """What a streamed folder holds, built from a whole run's Parquet frame.

    Every group — DailyState included — uses the shared ``closed="right",
    label="right"`` rule in both streamed and whole-run Parquet output, so the
    streamed folder matches the selected groups of ``df_whole`` directly with
    no timestamp shifting or leading DailyState-only row. When ``DailyState``
    is requested alone, only rows carrying DailyState values are written (one
    per grid and day).
    """
    ser_group = df_whole.columns.get_level_values("group")
    df_sel = df_whole.loc[:, ser_group.isin(groups)]
    if list(groups) == ["DailyState"]:
        df_sel = df_sel.loc[df_sel.notna().any(axis=1).to_numpy()]
    return df_sel.sort_index()


def _assert_identical(df_streamed, df_expected):
    """Equal values, bit for bit, with the same index, columns and dtypes."""
    pd.testing.assert_frame_equal(df_streamed, df_expected, check_exact=True)
    assert df_streamed.index.equals(df_expected.index)
    assert df_streamed.columns.equals(df_expected.columns)
    assert (df_streamed.dtypes == df_expected.dtypes).all()
    ar_a, ar_b = df_streamed.to_numpy(), df_expected.to_numpy()
    assert np.array_equal(np.isnan(ar_a), np.isnan(ar_b))
    assert np.array_equal(
        np.nan_to_num(ar_a).view(np.uint64), np.nan_to_num(ar_b).view(np.uint64)
    )


def _compare_with_whole(dir_streamed, dir_whole, groups, freq_s):
    """The streamed folder against a whole run saved as Parquet."""
    assert sorted(p.name for p in dir_streamed.iterdir()) == sorted(
        p.name for p in dir_whole.iterdir()
    )
    assert _output_path(dir_whole).is_file()
    df_whole = pd.read_parquet(_output_path(dir_whole))
    _assert_identical(
        _read_streamed(dir_streamed), _expected_streamed(df_whole, groups, freq_s)
    )
    _compare_sidecars(dir_streamed, dir_whole)


def _compare_sidecars(dir_a, dir_b):
    """Same metadata (but its creation time) and checkpoint numbers."""
    [path_meta] = dir_a.glob("*SUEWS_metadata*.parquet")
    pd.testing.assert_frame_equal(
        pd.read_parquet(path_meta).drop(columns="creation_time"),
        pd.read_parquet(dir_b / path_meta.name).drop(columns="creation_time"),
    )
    [path_ckpt] = dir_a.glob("*_checkpoint.json")
    ckpt_a = json.loads(path_ckpt.read_text(encoding="utf-8"))
    ckpt_b = json.loads((dir_b / path_ckpt.name).read_text(encoding="utf-8"))
    assert ckpt_a["last_timestamp"] == ckpt_b["last_timestamp"]
    assert list(_numbers(ckpt_a)) == list(_numbers(ckpt_b))


def _compare_streamed_dirs(dir_a, dir_b):
    """Two streamed runs: the same files, part for part."""
    assert sorted(p.name for p in dir_a.iterdir()) == sorted(
        p.name for p in dir_b.iterdir()
    )
    assert _part_names(dir_a) == _part_names(dir_b)
    for name in _part_names(dir_a):
        _assert_identical(
            pd.read_parquet(_output_path(dir_a) / name),
            pd.read_parquet(_output_path(dir_b) / name),
        )
    _compare_sidecars(dir_a, dir_b)


# Chunk length, output frequency and grids: each pair of values once.
@_parametrize_cases(
    ("stream_chunk", "freq_s", "list_gridiv"),
    CHUNKS,
    {"300": TSTEP_S, "3600": 3600, "86400": 86400},
    GRIDS,
    cases=[
        ("1", "300", "1grid"),
        ("1", "3600", "2grids"),
        ("1", "86400", "1grid"),
        ("2", "300", "2grids"),
        ("2", "3600", "1grid"),
        ("2", "86400", "2grids"),
        ("7", "300", "2grids"),
        ("7", "3600", "1grid"),
        ("7", "86400", "2grids"),
    ],
)
def test_parquet_matches_run_then_save(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    stream_chunk,
    freq_s,
    list_gridiv,
):
    """Read back and sorted, the streamed folder is the whole run's frame.

    Restricted to output.groups, with DailyState at its own rows (see
    ``_expected_streamed``).
    """
    dir_whole = stream_chunk_reference_dir(
        freq_s, list_gridiv, GROUPS_PARQUET, CLOCK_FOLLOW, **WINDOW, fmt="parquet"
    )
    sim = _parquet_sim(
        short_sample_sim, tmp_path, freq_s, stream_chunk, list_gridiv=list_gridiv
    )
    output = sim.run()

    df_whole = pd.read_parquet(_output_path(dir_whole))
    assert set(df_whole.index.get_level_values("datetime").year) == {2011, 2012}
    # A whole run's Parquet holds every group; the streamed one the chosen ones.
    assert set(df_whole.columns.get_level_values("group")) > set(GROUPS_PARQUET)
    _compare_with_whole(tmp_path, dir_whole, GROUPS_PARQUET, freq_s)
    assert output.groups == GROUPS_PARQUET
    provenance = read_provenance(tmp_path)
    assert provenance["run"]["stream_chunk"] == stream_chunk
    assert provenance["output"]["format"] == "parquet"


@pytest.mark.parametrize("freq_s", [TSTEP_S, 3600, 86400])
def test_parquet_daily_state_only_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, freq_s
):
    """With DailyState alone, the folder holds one row per grid and day."""
    groups = ["DailyState"]
    # Whole-run Parquet always writes every group, so reuse the cached reference.
    dir_whole = stream_chunk_reference_dir(
        freq_s, [2, 1], GROUPS_PARQUET, CLOCK_FOLLOW, **WINDOW, fmt="parquet"
    )
    sim = _parquet_sim(
        short_sample_sim, tmp_path, freq_s, 2, list_gridiv=[2, 1], groups=groups
    )
    sim.run()

    _compare_with_whole(tmp_path, dir_whole, groups, freq_s)
    assert len(_read_streamed(tmp_path)) == 2 * 6  # two grids, six days


@pytest.mark.parametrize(
    ("stream_chunk", "names"),
    [
        (
            1,
            [f"part-2011-{k:03d}.parquet" for k in range(1, N_CHUNK_2011 + 1)]
            + [f"part-2012-{k:03d}.parquet" for k in range(1, N_CHUNK_2012 + 1)],
        ),
        (
            2,
            [
                "part-2011-001.parquet",
                "part-2011-002.parquet",
                "part-2012-001.parquet",
                "part-2012-002.parquet",
            ],
        ),
        (7, ["part-2011-001.parquet", "part-2012-001.parquet"]),
    ],
)
def test_parquet_folder_holds_one_file_per_chunk(
    short_sample_sim, tmp_path, stream_chunk, names
):
    """Part files are named by calendar year and chunk within it."""
    _parquet_sim(short_sample_sim, tmp_path, 3600, stream_chunk).run()

    assert _part_names(tmp_path) == names


@_parametrize_cases(
    ("stream_chunk", "clock", "list_gridiv"),
    CHUNKS,
    CLOCKS_EXPLICIT,
    GRIDS,
    cases=_explicit_clock_cases("parquet"),
    cover_pairs=False,
)
def test_parquet_explicit_clock_matches_run_then_save(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    stream_chunk,
    clock,
    list_gridiv,
):
    """Explicit output clocks on the forcing clock give the same frame."""
    dir_whole = stream_chunk_reference_dir(
        3600, list_gridiv, GROUPS_PARQUET, clock, **WINDOW, fmt="parquet"
    )
    sim = _parquet_sim(
        short_sample_sim,
        tmp_path,
        3600,
        stream_chunk,
        list_gridiv=list_gridiv,
        clock=clock,
    )
    sim.run()

    _compare_with_whole(tmp_path, dir_whole, GROUPS_PARQUET, 3600)


@_parametrize_cases(
    ("clock", "site_clock"),
    CLOCKS_EXPLICIT,
    SITE_CLOCKS,
    cases=_tz_cases("parquet"),
    cover_pairs=False,
)
def test_parquet_non_zero_time_zone_matches_run_then_save(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, clock, site_clock
):
    """With a time zone and a daylight-saving window, streamed equals whole."""
    dir_whole = stream_chunk_reference_dir(
        3600,
        [2, 1],
        GROUPS_PARQUET,
        clock,
        **WINDOW,
        site_clock=site_clock,
        fmt="parquet",
    )
    sim = _parquet_sim(
        short_sample_sim,
        tmp_path,
        3600,
        7,
        list_gridiv=[2, 1],
        clock=clock,
        site_clock=site_clock,
    )
    sim.run()

    _compare_with_whole(tmp_path, dir_whole, GROUPS_PARQUET, 3600)


def test_parquet_short_last_chunk_at_the_model_timestep(
    short_sample_sim, stream_chunk_reference_dir, tmp_path
):
    """A last chunk of two rows at the model timestep is not resampled alone."""
    end = "2012-01-04 00:10"
    dir_whole = stream_chunk_reference_dir(
        TSTEP_S,
        None,
        GROUPS_PARQUET,
        CLOCK_FOLLOW,
        end=end,
        start=WINDOW_START,
        fmt="parquet",
    )
    sim = _parquet_sim(short_sample_sim, tmp_path, TSTEP_S, 1, end=end)
    sim.run()

    assert _part_names(tmp_path)[-1] == "part-2012-004.parquet"
    assert len(pd.read_parquet(_output_path(tmp_path) / "part-2012-004.parquet")) == 2
    _compare_with_whole(tmp_path, dir_whole, GROUPS_PARQUET, TSTEP_S)


def test_parquet_later_chunk_partial_bin_keeps_rate_coverage(
    short_sample_sim, tmp_path
):
    """A completed streamed run retains the last chunk's fifteen-minute bin."""
    from supy.suews_output import SUEWSOutput

    sim = _parquet_sim(
        short_sample_sim,
        tmp_path,
        3600,
        1,
        end="2011-12-30 00:15",
        groups=["SUEWS", "DailyState"],
    )
    forcing = sim.forcing.df.copy()
    forcing["rain"] = 0.5
    sim.update_forcing(forcing)
    sim.run()

    path = _output_path(tmp_path)
    parts = sorted(path.glob("part-*.parquet"))
    assert len(parts) == 2
    key = "1|2011-12-30T01:00:00"
    for source in (path, parts[-1]):
        saved = pd.read_parquet(source)
        assert saved.attrs["supy_output_coverage_s"][key] == 900
        assert saved["SUEWS", "Rain"].dropna().to_numpy() == pytest.approx(
            0.5 / TSTEP_S
        )
        daily = SUEWSOutput(saved, None).resample("D")
        # DailyState-only bins have no main-group coverage or rain rate.
        rain = daily.df["SUEWS", "Rain"].dropna()
        assert len(rain) == (2 if source == path else 1)
        assert rain.to_numpy() == pytest.approx(0.5 / TSTEP_S)


@pytest.mark.parametrize("freq", ["MS", "ME", "YE"])
def test_parquet_reload_resamples_dailystate_like_the_run(
    stream_chunk_reference_output, stream_chunk_reference_dir, freq
):
    """Streamed hourly Parquet reloaded and resampled keeps each day's period.

    Streamed Parquet resamples DailyState onto the same ``D+1 00:00`` rows as
    ``SUEWS``, so reloading and resampling to calendar frequencies puts
    31 December's ``P_day`` and coverage in its own year (and each month's last
    day in its own month), matching the direct resample of the run.
    """
    from supy._post import _resample_output
    from supy.suews_output import SUEWSOutput

    groups = ["SUEWS", "DailyState"]
    output = stream_chunk_reference_output(3600, None, groups, CLOCK_FOLLOW, **WINDOW)
    dir_streamed = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, **WINDOW, fmt="parquet", stream_chunk=2
    )
    saved = pd.read_parquet(_output_path(dir_streamed))
    assert saved.attrs["supy_output_interval_s"] == 3600

    expected = output.resample(freq).df
    coverage = "supy_output_daily_coverage_s"
    for reloaded in (
        SUEWSOutput(saved, output.state_final).resample(freq).df,
        _resample_output(saved, freq),
    ):
        p_day = reloaded["DailyState", "P_day"].dropna()
        pd.testing.assert_series_equal(
            p_day, expected["DailyState", "P_day"].dropna(), rtol=1e-9
        )
        assert reloaded.attrs[coverage] == pytest.approx(expected.attrs[coverage])


def test_parquet_output_names_the_folder_and_save_returns_it(
    short_sample_sim, tmp_path
):
    """The row accessors name the folder; save() returns what was written."""
    dir_out = tmp_path / "out"
    sim = _parquet_sim(short_sample_sim, dir_out, 3600, 30)
    output = sim.run()
    dict_mtime = {p.name: p.stat().st_mtime_ns for p in dir_out.rglob("*")}

    with pytest.raises(RuntimeError, match=r"read_parquet\(folder\)"):
        _ = output.df
    assert "a Parquet folder" in repr(output)
    paths = sim.save()
    assert {Path(p).name for p in paths} == {p.name for p in dir_out.iterdir()}
    assert {p.name: p.stat().st_mtime_ns for p in dir_out.rglob("*")} == dict_mtime
    with pytest.raises(ValueError, match="as Parquet"):
        sim.save(format="txt")


@pytest.mark.parametrize("previous", ["streamed", "whole"])
def test_fresh_parquet_run_removes_a_previous_runs_files(
    short_sample_sim, tmp_path, monkeypatch, previous
):
    """A fresh run first removes the parts and sidecars a previous run left."""
    dir_out = tmp_path / "out"
    sim = _parquet_sim(
        short_sample_sim, dir_out, 3600, 30 if previous == "streamed" else None
    )
    sim.run()
    if previous == "whole":
        sim.save()
        assert _output_path(dir_out).is_file()
    path_output = _output_path(dir_out)
    [path_meta] = dir_out.glob("*SUEWS_metadata*.parquet")
    if previous == "streamed":
        (path_output / "part-2099-001.parquet").write_bytes(b"stale")
        (path_output / ".part-2099-002.parquet.tmp").write_bytes(b"stale")
        # Not the run's files: left alone.
        (path_output / "notes.txt").write_bytes(b"mine")
        (path_output / ".part-analysis.tmp").write_bytes(b"mine")
    (dir_out / "other.parquet").write_bytes(b"mine")
    assert path_meta.exists()

    # Stopped after its first chunk, the new run has already cleared the folder.
    _run_failing_after(
        _parquet_sim(short_sample_sim, dir_out, 3600, 30), 1, monkeypatch
    )

    assert path_output.is_dir()
    list_left = ["part-2011-001.parquet"]
    if previous == "streamed":
        list_left = [".part-analysis.tmp", "notes.txt", *list_left]
    assert _part_names(dir_out) == list_left
    assert not path_meta.exists()
    assert (dir_out / "other.parquet").read_bytes() == b"mine"


# Output frequency, grids and failure point: each pair of values once.
@_parametrize_cases(
    ("freq_s", "list_gridiv", "n_written"),
    {"3600": 3600, "86400": 86400},
    GRIDS,
    {"at-year-end": N_CHUNK_2011, "mid-year": N_WRITTEN_MID_YEAR},
    cases=[
        ("3600", "1grid", "at-year-end"),
        ("3600", "2grids", "mid-year"),
        ("86400", "1grid", "mid-year"),
        ("86400", "2grids", "at-year-end"),
    ],
)
def test_parquet_resumed_run_matches_an_uninterrupted_run(
    short_sample_sim,
    stream_chunk_reference_dir,
    tmp_path,
    monkeypatch,
    freq_s,
    list_gridiv,
    n_written,
):
    """Killed at or after a year end and resumed, the folder is the same.

    The mid-year case is resumed twice, the first resume itself failing part
    of the way, so deleting the parts after the checkpoint must be
    repeatable. Completed parts keep their values; only the first part's
    folder-wide coverage metadata is refreshed at completion.
    """
    dir_full = stream_chunk_reference_dir(
        freq_s,
        list_gridiv,
        GROUPS_PARQUET,
        CLOCK_FOLLOW,
        **WINDOW,
        fmt="parquet",
        stream_chunk=1,
    )

    dir_out = tmp_path / "out"
    sim = _parquet_sim(short_sample_sim, dir_out, freq_s, 1, list_gridiv=list_gridiv)
    _run_failing_after(sim, n_written, monkeypatch)
    names = {p.name for p in dir_out.iterdir()}
    assert "provenance.json" not in names
    assert not any("SUEWS_metadata" in name for name in names)
    assert len(_part_names(dir_out)) == n_written
    ckpt = SUEWSCheckpoint.from_file(next(dir_out.glob("*_checkpoint.json")))
    assert pd.Timestamp(ckpt.last_timestamp) == pd.Timestamp("2012-01-01 00:00")
    path_output = _output_path(dir_out)
    dict_2011 = {
        p.name: p.stat().st_mtime_ns for p in path_output.glob("part-2011-*.parquet")
    }

    if n_written > N_CHUNK_2011:
        _resume(sim, dir_out, monkeypatch, fail_after=N_CHUNK_2012 - 1)
        assert len(_part_names(dir_out)) == N_CHUNK_2011 + N_CHUNK_2012 - 1
    _resume(sim, dir_out)

    _compare_streamed_dirs(dir_out, dir_full)
    # Folder readers use the first part's schema. Completion atomically
    # refreshes that schema with coverage from all parts, including the
    # resumed year; _compare_streamed_dirs above still checks every value.
    first_part = min(dict_2011)
    assert {
        p.name: p.stat().st_mtime_ns
        for p in path_output.glob("part-2011-*.parquet")
        if p.name != first_part
    } == {name: mtime for name, mtime in dict_2011.items() if name != first_part}


def test_parquet_part_cut_short_is_never_left_half_written(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, monkeypatch
):
    """A part file is renamed into place whole; an interrupted write leaves
    only its temporary file, which the resumed run removes."""
    dir_full = stream_chunk_reference_dir(
        3600,
        None,
        GROUPS_PARQUET,
        CLOCK_FOLLOW,
        **WINDOW,
        fmt="parquet",
        stream_chunk=1,
    )
    dir_out = tmp_path / "out"
    sim = _parquet_sim(short_sample_sim, dir_out, 3600, 1)
    replace = os.replace

    def fail_on_part(src, dst):
        if Path(dst).name == "part-2012-002.parquet":
            raise OSError("simulated interruption")
        return replace(src, dst)

    monkeypatch.setattr(os, "replace", fail_on_part)
    with pytest.raises(OSError, match="simulated interruption"):
        sim.run()
    monkeypatch.undo()
    path_output = _output_path(dir_out)
    assert (path_output / ".part-2012-002.parquet.tmp").exists()
    assert "part-2012-002.parquet" not in _part_names(dir_out)
    # The temporary file is hidden from readers.
    assert len(pd.read_parquet(path_output)) > 0

    _resume(sim, dir_out)

    _compare_streamed_dirs(dir_out, dir_full)


def test_parquet_resume_refuses_a_checkpoint_inside_a_year_with_parts(
    short_sample_sim, tmp_path
):
    """Parts of the checkpoint's own year cannot be split by name: refused.

    So is continuing a completed run from its final checkpoint into the
    same folder; nothing is deleted.
    """
    dir_out = tmp_path / "out"
    sim_out = _parquet_sim(short_sample_sim, dir_out, 3600, 1)
    sim_out.run()
    sim_mid = _parquet_sim(short_sample_sim, tmp_path / "mid", 3600, None)
    sim_mid.run(end_date="2011-12-30 00:00")
    sim = SUEWSSimulation.from_checkpoint(sim_out.config, sim_mid.checkpoint)
    sim.update_forcing(sim_mid.forcing.df)
    n_part = len(_part_names(dir_out))

    with pytest.raises(ValueError, match="not at a year end"):
        sim.run(start_date=pd.Timestamp("2011-12-30 00:05"))
    assert len(_part_names(dir_out)) == n_part

    sim_done = _parquet_sim(short_sample_sim, tmp_path / "done", 3600, 1)
    sim_done.run(end_date="2012-01-03 00:00")
    sim_next = SUEWSSimulation.from_checkpoint(sim_done.config, sim_done.checkpoint)
    sim_next.update_forcing(sim_done.forcing.df)
    n_part = len(_part_names(tmp_path / "done"))
    with pytest.raises(ValueError, match="not at a year end"):
        sim_next.run(start_date=pd.Timestamp("2012-01-03 00:05"))
    assert len(_part_names(tmp_path / "done")) == n_part


@pytest.mark.parametrize("freq_s", [3600, 86400])
def test_parquet_group_without_values_keeps_every_interval(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, freq_s
):
    """A group the run leaves empty still has a row per output interval."""
    groups = ["EHC"]
    # Whole-run Parquet always writes every group, so reuse the cached reference.
    dir_whole = stream_chunk_reference_dir(
        freq_s, None, GROUPS_PARQUET, CLOCK_FOLLOW, **WINDOW, fmt="parquet"
    )
    sim = _parquet_sim(short_sample_sim, tmp_path, freq_s, 7, groups=groups)
    sim.run()

    df_streamed = _read_streamed(tmp_path)
    assert df_streamed.isna().all().all()
    _compare_with_whole(tmp_path, dir_whole, groups, freq_s)


@pytest.mark.parametrize("fmt", ["txt", "parquet"])
def test_streamed_provisional_group_needs_opt_in(short_sample_sim, tmp_path, fmt):
    """A streamed run refuses provisional groups before computing anything."""
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path, 3600, 30, groups=["EHC"], fmt=fmt
    )
    sim.config.model.control.output.include_provisional = False
    with pytest.raises(ValueError, match="EHC are provisional"):
        sim.run()
    assert not list(tmp_path.glob("*SUEWS_output*"))


@pytest.mark.parametrize("fmt", ["txt", "parquet"])
def test_streamed_provisional_warning_points_to_caller(short_sample_sim, tmp_path, fmt):
    sim = _year_crossing_sim(
        short_sample_sim,
        tmp_path,
        3600,
        1,
        groups=["BEERS"],
        fmt=fmt,
        start="2011-12-31 00:05",
        end="2012-01-01 00:00",
    )
    with pytest.warns(UserWarning, match="provisional output group") as caught:
        sim.run()
    provisional = [w for w in caught if "provisional output group" in str(w.message)]
    assert all(Path(w.filename) == Path(__file__) for w in provisional)
    paths = sim.save()
    assert any(
        Path(path).suffix == (".txt" if fmt == "txt" else ".parquet") for path in paths
    )


def test_parquet_failure_writing_metadata_resumes_from_the_year_before(
    short_sample_sim, stream_chunk_reference_dir, tmp_path, monkeypatch
):
    """The metadata file is written before the last checkpoint."""
    dir_full = stream_chunk_reference_dir(
        3600,
        None,
        GROUPS_PARQUET,
        CLOCK_FOLLOW,
        **WINDOW,
        fmt="parquet",
        stream_chunk=1,
    )
    dir_out = tmp_path / "out"
    sim = _parquet_sim(short_sample_sim, dir_out, 3600, 1)

    def fail(*args, **kwargs):
        raise OSError("simulated failure")

    monkeypatch.setattr(save_module, "_write_parquet_sidecars", fail)
    with pytest.raises(OSError, match="simulated failure"):
        sim.run()
    monkeypatch.undo()
    ckpt = SUEWSCheckpoint.from_file(next(dir_out.glob("*_checkpoint.json")))
    assert pd.Timestamp(ckpt.last_timestamp) == pd.Timestamp("2012-01-01 00:00")

    _resume(sim, dir_out)

    _compare_streamed_dirs(dir_out, dir_full)


def test_parquet_stream_chunk_is_accepted():
    """Parquet output with stream_chunk passes the YAML validation."""
    output = OutputControl(format=OutputFormat.PARQUET, stream_chunk=30)
    assert output.stream_chunk == 30


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("value", "match"),
    [
        (True, "number of days from 1 to 366, not true"),
        (False, "number of days from 1 to 366, not false"),
        (0, "from 1 to 366; got 0"),
        (367, "from 1 to 366; got 367"),
        (3600, "output.freq is in seconds, output.stream_chunk in days"),
        (30.5, "integer"),
    ],
)
def test_stream_chunk_must_be_a_number_of_days(value, match):
    """stream_chunk is an integer number of days from 1 to 366, never a bool."""
    with pytest.raises(ValidationError, match=match):
        OutputControl(stream_chunk=value)


def test_stream_chunk_unit_is_days():
    """The field carries its unit in the schema metadata, not in its name."""
    extra = OutputControl.model_fields["stream_chunk"].json_schema_extra
    assert extra == {"unit": "d"}
    assert OutputControl(stream_chunk=1).stream_chunk == 1
    assert OutputControl(stream_chunk=366).stream_chunk == 366
    assert OutputControl().stream_chunk is None


@pytest.mark.parametrize(
    ("output", "match"),
    [
        ({"freq": 25200}, "divides one day"),
        ({"timestamp_reference": "daylight"}, "forcing clock"),
    ],
)
def test_output_block_rejects_settings_it_cannot_reproduce(output, match):
    """The YAML validation rejects settings streaming cannot reproduce."""
    OutputControl(**output)  # valid when unset
    with pytest.raises(ValidationError, match=match):
        OutputControl(**output, stream_chunk=30)


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
            output={"stream_chunk": 30, "timestamp_reference": target},
            forcing={"timestamp_reference": source},
        )
    with pytest.raises(ValidationError, match="forcing clock"):
        ModelControl(
            output={"stream_chunk": 30, "timestamp_reference": "utc"},
            forcing={"timestamp_reference": "local_standard_time"},
        )
    # Unset leaves the relabelling allowed.
    ModelControl(
        output={"timestamp_reference": "utc"},
        forcing={"timestamp_reference": "local_standard_time"},
    )


def test_unsupported_settings_fail_before_running(short_sample_sim, tmp_path):
    """Updating a simulation's config to a setting it cannot stream fails."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, 30)

    with pytest.raises(ValueError, match="divides one day"):
        sim.update_config(
            {"model": {"control": {"output": {"freq": 25200}}}},
            auto_load_forcing=False,
        )
    # Parquet output streams too.
    sim.update_config(
        {"model": {"control": {"output": {"format": "parquet"}}}},
        auto_load_forcing=False,
    )


@pytest.mark.parametrize(
    ("stream_chunk", "field", "value", "match"),
    [
        (30, "freq", 25200, "divides one day"),
        (30, "timestamp_reference", "utc", "forcing clock"),
        (30, "stream_chunk", True, "not true"),
        (30, "stream_chunk", 30.5, "integer"),
        (30, "stream_chunk", 0, "from 1 to 366; got 0"),
        (30, "stream_chunk", 367, "from 1 to 366; got 367"),
    ],
)
def test_settings_assigned_on_the_config_fail_before_running(
    short_sample_sim, tmp_path, stream_chunk, field, value, match
):
    """A config changed by attribute assignment is checked again by run()."""
    sim = _year_crossing_sim(short_sample_sim, tmp_path / "out", 3600, stream_chunk)
    output = sim.config.model.control.output
    setattr(output, field, value)

    with pytest.raises(ValueError, match=match):
        sim.run()
    assert not (tmp_path / "out").exists()


def test_groups_the_model_does_not_write_are_rejected(short_sample_sim, tmp_path):
    """Saving only groups the kernel never writes would write no files."""
    sim = _year_crossing_sim(
        short_sample_sim, tmp_path / "out", 3600, 30, groups=["BL"]
    )

    with pytest.raises(ValueError, match="names no group the model writes"):
        sim.run()
    assert not (tmp_path / "out").exists()


def test_cli_run_streams_each_file_once(tmp_path, monkeypatch):
    """``suews run`` honours stream_chunk, writes each file once and records itself."""
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
    assert "      timestamp_reference: follow\n" in cfg_text
    cfg_text = cfg_text.replace(
        "      timestamp_reference: follow\n",
        "      timestamp_reference: follow\n      stream_chunk: 1\n",
        1,
    )
    cfg_path = tmp_path / "case.yml"
    cfg_path.write_text(cfg_text, encoding="utf-8")

    calls = []
    write_output_frame = SUEWSSimulation._write_output_frame

    def spy(df_output, df_state_final, settings, streamed_files=None):
        paths = write_output_frame(df_output, df_state_final, settings, streamed_files)
        calls.append(streamed_files)
        return paths

    monkeypatch.setattr(SUEWSSimulation, "_write_output_frame", staticmethod(spy))
    result = CliRunner().invoke(run_cmd, [str(cfg_path)])
    assert result.exit_code == 0, result.output

    # Two one-day chunks, written by the run itself rather than again by save().
    assert len(calls) == 2
    assert all(streamed is not None for streamed in calls)
    dir_out = tmp_path / "Output"
    listed = [
        line.strip()
        for line in result.output.split("have been written out:")[1].splitlines()
        if line.strip().startswith(str(dir_out))
    ]
    assert len(listed) == len(set(listed))
    assert {Path(p).name for p in listed} == {
        p.name for p in dir_out.iterdir() if not p.name.endswith(".units.json")
    }
    prov = read_provenance(dir_out)
    assert prov["run"]["interface"] == "cli"
    assert prov["run"]["command"] == "suews run case.yml"
    assert prov["run"]["stream_chunk"] == 1
