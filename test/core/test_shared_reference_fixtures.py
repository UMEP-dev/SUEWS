"""Contracts for shared session-scoped reference-run fixtures in ``test/conftest.py``.

Verifies that:
- ``sample_config_run_cached``, ``stream_chunk_reference_dir``, and
  ``stream_chunk_reference_output`` build each distinct key at most once per
  session;
- cached ``SUEWSOutput`` results are defensively deep-copied on every hand-out
  so mutations to frames, attributes, config, or metadata cannot poison later
  consumers;
- ``stream_chunk_reference_dir(..., copy_dir=True)`` isolates directory edits
  from the cached folder;
- ``SUEWS_LOG_REFERENCE_RUNS`` is silent when unset/disabled and emits ASCII
  stderr lines or JSONL records with worker metadata when enabled.
"""

from __future__ import annotations

import json
from pathlib import Path

from conftest import (
    REFERENCE_RUN_LOG_ENV,
    _normalise_period_key,
    _record_reference_run,
    get_reference_run_counts,
)
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING", "C-ISOLATION")]

CLOCK_FOLLOW = ("local_standard_time", "follow")
# Wall-clock timestamps in SUEWSOutput.metadata that naturally differ between runs.
WALL_CLOCK_METADATA_KEYS = frozenset({"started_at", "ended_at"})


def test_normalise_period_key_distinguishes_date_only_string_and_timestamp():
    """Date-only strings and Timestamps have distinct run-period semantics."""
    key_str = _normalise_period_key("2012-01-01")
    key_ts = _normalise_period_key(pd.Timestamp("2012-01-01"))

    assert key_str != key_ts
    assert _normalise_period_key(None) is None
    assert _normalise_period_key("2012-01-01") == key_str


def test_sample_config_run_cached_computes_once_and_matches_direct_run(
    sample_config_run_cached, sample_data_loaded
):
    """One 12-step sample-config run builds once and matches a direct run bit for bit."""
    _, df_forcing = sample_data_loaded
    end_date = df_forcing.index[11]
    key = ("sample_config_run_cached", f"start={None!r},end={end_date!r}")
    before = get_reference_run_counts().get(key, 0)

    out_first = sample_config_run_cached(end_date=end_date)
    after_first = get_reference_run_counts().get(key, 0)
    out_second = sample_config_run_cached(end_date=end_date)
    after_second = get_reference_run_counts().get(key, 0)

    assert after_first >= 1
    assert after_first - before <= 1
    assert after_second == after_first

    sim_direct = SUEWSSimulation.from_sample_data()
    out_direct = sim_direct.run(end_date=end_date)

    pd.testing.assert_frame_equal(out_first.df, out_direct.df, check_exact=True)
    pd.testing.assert_frame_equal(out_second.df, out_direct.df, check_exact=True)
    pd.testing.assert_frame_equal(
        out_first.state_final, out_direct.state_final, check_exact=True
    )
    assert out_first.config.sites[0].name == out_direct.config.sites[0].name
    assert out_first.checkpoint.last_timestamp == out_direct.checkpoint.last_timestamp

    meta_cached = {
        k: v for k, v in out_first.metadata.items() if k not in WALL_CLOCK_METADATA_KEYS
    }
    meta_direct = {
        k: v
        for k, v in out_direct.metadata.items()
        if k not in WALL_CLOCK_METADATA_KEYS
    }
    assert meta_cached == meta_direct
    assert out_first.metadata["inputs"]["config"]["source"] == "file"
    assert out_first.metadata["inputs"]["forcing"]["source"] == "files"


def test_sample_config_run_cached_isolates_mutations(
    sample_config_run_cached, sample_data_loaded
):
    """Mutating a returned ``SUEWSOutput`` cannot poison subsequent callers."""
    _, df_forcing = sample_data_loaded
    end_date = df_forcing.index[11]

    out_a = sample_config_run_cached(end_date=end_date)
    df_expected = out_a.df.copy(deep=True)
    state_expected = out_a.state_final.copy(deep=True)
    site_name_expected = out_a.config.sites[0].name

    # Mutate every mutable component on out_a in place.
    out_a._df_output.iloc[0, 0] = -99999.0
    out_a._df_output.attrs["poisoned"] = True
    out_a._df_state_final[("version", "0")] = "MUTATED"
    out_a.config.sites[0].name = "MUTATED"
    out_a._metadata["poisoned"] = True

    out_b = sample_config_run_cached(end_date=end_date)

    assert out_b is not out_a
    assert not np.shares_memory(
        out_a._df_output.to_numpy(), out_b._df_output.to_numpy()
    )
    pd.testing.assert_frame_equal(out_b.df, df_expected, check_exact=True)
    pd.testing.assert_frame_equal(out_b.state_final, state_expected, check_exact=True)
    assert "poisoned" not in out_b._df_output.attrs
    assert out_b.config.sites[0].name == site_name_expected
    assert "poisoned" not in out_b._metadata


def test_stream_chunk_reference_dir_caches_and_supports_copy_dir(
    stream_chunk_reference_dir,
):
    """Cached reference folders reuse builds per key and isolate ``copy_dir=True``."""
    groups = ("SUEWS", "DailyState")
    start = "2011-12-31 00:05"
    end = "2012-01-02 00:00"
    whole_key = (
        "stream_chunk_reference_dir",
        repr((3600, (), groups, CLOCK_FOLLOW, end, start, None, "txt", None)),
    )
    streamed_key = (
        "stream_chunk_reference_dir",
        repr((3600, (), groups, CLOCK_FOLLOW, end, start, None, "txt", 1)),
    )
    before_whole = get_reference_run_counts().get(whole_key, 0)
    before_streamed = get_reference_run_counts().get(streamed_key, 0)

    dir_whole_1 = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start
    )
    after_whole_1 = get_reference_run_counts().get(whole_key, 0)
    dir_whole_2 = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start
    )
    after_whole_2 = get_reference_run_counts().get(whole_key, 0)

    assert dir_whole_1 == dir_whole_2
    assert after_whole_1 >= 1
    assert after_whole_1 - before_whole <= 1
    assert after_whole_2 == after_whole_1

    dir_streamed_1 = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start, stream_chunk=1
    )
    after_streamed_1 = get_reference_run_counts().get(streamed_key, 0)
    dir_streamed_2 = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start, stream_chunk=1
    )
    after_streamed_2 = get_reference_run_counts().get(streamed_key, 0)

    assert dir_streamed_1 != dir_whole_1
    assert dir_streamed_1 == dir_streamed_2
    assert after_streamed_1 >= 1
    assert after_streamed_1 - before_streamed <= 1
    assert after_streamed_2 == after_streamed_1

    # copy_dir=True hands back a distinct writable copy without rebuilding.
    dir_copy = stream_chunk_reference_dir(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start, copy_dir=True
    )
    assert dir_copy != dir_whole_1
    assert get_reference_run_counts().get(whole_key, 0) == after_whole_2
    txt_files = sorted(p.name for p in dir_whole_1.glob("*.txt"))
    assert txt_files
    original_bytes = (dir_whole_1 / txt_files[0]).read_bytes()
    assert (dir_copy / txt_files[0]).read_bytes() == original_bytes
    (dir_copy / txt_files[0]).write_text("tampered\n", encoding="utf-8")
    assert (dir_whole_1 / txt_files[0]).read_bytes() == original_bytes


def test_stream_chunk_reference_output_caches_and_isolates_mutations(
    stream_chunk_reference_output,
):
    """Unstreamed year-crossing output builds once per key and copies on hand-out."""
    groups = ("SUEWS", "DailyState")
    start = "2011-12-31 00:05"
    end = "2012-01-02 00:00"
    key = (
        "stream_chunk_reference_output",
        repr((3600, (), groups, CLOCK_FOLLOW, end, start, None, "txt")),
    )
    before = get_reference_run_counts().get(key, 0)

    out_a = stream_chunk_reference_output(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start
    )
    after_a = get_reference_run_counts().get(key, 0)
    df_expected = out_a.df.copy(deep=True)
    out_a._df_output.iloc[0, 0] = -99999.0
    out_a.config.sites[0].name = "MUTATED"

    out_b = stream_chunk_reference_output(
        3600, None, groups, CLOCK_FOLLOW, end=end, start=start
    )
    after_b = get_reference_run_counts().get(key, 0)

    assert after_a >= 1
    assert after_a - before <= 1
    assert after_b == after_a
    pd.testing.assert_frame_equal(out_b.df, df_expected, check_exact=True)
    assert out_b.config.sites[0].name != "MUTATED"


def test_reference_run_counter_env_var_logging(monkeypatch, capsys, tmp_path: Path):
    """``SUEWS_LOG_REFERENCE_RUNS`` stays silent by default and logs when opted in."""
    monkeypatch.delenv(REFERENCE_RUN_LOG_ENV, raising=False)
    monkeypatch.delenv("PYTEST_XDIST_WORKER", raising=False)

    # Disabled when unset or "0": in-process count updates, nothing emitted.
    before = get_reference_run_counts().get(("test_fixture", "k0"), 0)
    _record_reference_run("test_fixture", "k0")
    monkeypatch.setenv(REFERENCE_RUN_LOG_ENV, "0")
    _record_reference_run("test_fixture", "k0")
    assert get_reference_run_counts()[("test_fixture", "k0")] == before + 2
    captured = capsys.readouterr()
    assert captured.err == ""

    # Enabled with "1": ASCII stderr log line with worker id.
    monkeypatch.setenv(REFERENCE_RUN_LOG_ENV, "1")
    monkeypatch.setenv("PYTEST_XDIST_WORKER", "gw2")
    _record_reference_run("test_fixture", "k1")
    captured = capsys.readouterr()
    assert captured.err.isascii()
    assert "[ref-run] worker=gw2" in captured.err
    assert "fixture=test_fixture key=k1" in captured.err

    # Enabled with a file path: appends UTF-8 ASCII JSONL records.
    log_file = tmp_path / "logs" / "ref_runs.jsonl"
    monkeypatch.setenv(REFERENCE_RUN_LOG_ENV, str(log_file))
    _record_reference_run("test_fixture", "k2")
    _record_reference_run("test_fixture", "k3")
    raw_text = log_file.read_text(encoding="utf-8")
    assert raw_text.isascii()
    records = [json.loads(line) for line in raw_text.splitlines()]
    assert len(records) == 2
    assert records[0]["worker"] == "gw2"
    assert records[0]["fixture"] == "test_fixture"
    assert records[0]["key"] == "k2"
    assert records[1]["key"] == "k3"
    assert isinstance(records[0]["pid"], int)
