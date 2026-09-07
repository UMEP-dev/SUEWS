"""Overlapping forcing files: deduplicate identical records, reject conflicts.

Regression coverage for the multi-file merge path. Before the fix every
loader kept the first record for a duplicated timestamp, so two valid
files that disagreed (10 C vs 20 C) produced opposite results when the
list order was reversed, with no warning.
"""

import logging
from importlib.resources import files
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy._load import (
    FORCING_OPTIONAL_FILL,
    ForcingConflictError,
    load_SUEWS_Forcing_met_df_yaml,
    merge_forcing_frames,
)
from supy.suews_forcing import SUEWSForcing
from supy.suews_sim import SUEWSSimulation
from supy.util._io import read_forcing

pytestmark = pytest.mark.api

SAMPLE = files("supy").joinpath("sample_data/Kc_2012_data_60.txt")


# ---------------------------------------------------------------------------
# Fixtures: two proper forcing files sharing timestamps
# ---------------------------------------------------------------------------


def _write_variant(tmp_path: Path, name: str, tair: float, n_rows: int = 4) -> Path:
    with SAMPLE.open("r", encoding="utf-8") as fh:
        raw = pd.read_csv(fh, sep=r"\s+").iloc[:n_rows].copy()
    raw["Tair"] = tair
    path = tmp_path / name
    raw.to_csv(path, sep="\t", index=False, encoding="utf-8")
    return path


@pytest.fixture
def conflicting_files(tmp_path):
    first = _write_variant(tmp_path, "first.txt", 10.0)
    second = _write_variant(tmp_path, "second.txt", 20.0)
    return first, second


@pytest.fixture
def identical_files(tmp_path):
    first = _write_variant(tmp_path, "a.txt", 10.0)
    second = _write_variant(tmp_path, "b.txt", 10.0)
    return first, second


def _frame(values, start="2012-01-01 01:00", periods=None, freq="h", **extra):
    values = list(values)
    idx = pd.date_range(start, periods=periods or len(values), freq=freq)
    data = {"Tair": values, "iy": np.full(len(values), 2012, dtype=int)}
    data.update(extra)
    return pd.DataFrame(data, index=idx)


# ---------------------------------------------------------------------------
# merge_forcing_frames unit behaviour
# ---------------------------------------------------------------------------


class TestMergeForcingFrames:
    def test_no_overlap_concatenates(self):
        a = _frame([1.0, 2.0])
        b = _frame([3.0, 4.0], start="2012-01-01 03:00")
        merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["Tair"].tolist() == [1.0, 2.0, 3.0, 4.0]
        assert merged.index.is_unique and merged.index.is_monotonic_increasing

    def test_identical_overlap_deduplicated_silently(self, caplog):
        a = _frame([1.0, 2.0, 3.0])
        b = _frame([2.0, 3.0, 4.0], start="2012-01-01 02:00")
        with caplog.at_level(logging.WARNING, logger="SuPy"):
            merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["Tair"].tolist() == [1.0, 2.0, 3.0, 4.0]
        assert merged.index.is_unique
        assert not caplog.records

    def test_conflict_raises_naming_source_timestamp_variable(self):
        a = _frame([10.0, 10.0])
        b = _frame([10.0, 20.0])
        with pytest.raises(ForcingConflictError) as excinfo:
            merge_forcing_frames([a, b], ["first.txt", "second.txt"])
        msg = str(excinfo.value)
        assert "first.txt" in msg and "second.txt" in msg
        assert "2012-01-01 02:00:00" in msg
        assert "Tair" in msg
        assert "10.0" in msg and "20.0" in msg
        assert "on_conflict" in msg

    def test_default_result_is_order_independent(self):
        a = _frame([1.0, 2.0, np.nan])
        b = _frame([np.nan, 2.0, 3.0])
        forward = merge_forcing_frames([a, b], ["a", "b"])
        reverse = merge_forcing_frames([b, a], ["b", "a"])
        pd.testing.assert_frame_equal(forward, reverse)
        assert forward["Tair"].tolist() == [1.0, 2.0, 3.0]

    def test_missing_nan_and_sentinel_fill_rather_than_conflict(self):
        a = _frame([np.nan, FORCING_OPTIONAL_FILL, 5.0])
        b = _frame([1.0, 2.0, 5.0])
        merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["Tair"].tolist() == [1.0, 2.0, 5.0]

    def test_all_missing_cell_keeps_sentinel(self):
        a = _frame([FORCING_OPTIONAL_FILL])
        b = _frame([np.nan])
        merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["Tair"].iloc[0] == FORCING_OPTIONAL_FILL

    def test_extension_column_in_one_file_is_filled(self):
        a = _frame([1.0, 2.0])
        b = _frame([1.0, 2.0], lai_grass=[3.0, 3.5])
        merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["lai_grass"].tolist() == [3.0, 3.5]
        assert merged["Tair"].tolist() == [1.0, 2.0]

    def test_integer_dtype_preserved(self):
        a = _frame([1.0, 2.0])
        b = _frame([2.0, 3.0], start="2012-01-01 02:00")
        merged = merge_forcing_frames([a, b], ["a", "b"])
        assert merged["iy"].dtype == a["iy"].dtype

    @pytest.mark.parametrize(
        "policy, expected",
        [("first", [10.0, 10.0]), ("last", [10.0, 20.0])],
    )
    def test_explicit_precedence_resolves_and_warns(self, policy, expected, caplog):
        a = _frame([10.0, 10.0])
        b = _frame([10.0, 20.0])
        with caplog.at_level(logging.WARNING, logger="SuPy"):
            merged = merge_forcing_frames([a, b], ["a", "b"], on_conflict=policy)
        assert merged["Tair"].tolist() == expected
        assert any(
            "Conflicting forcing observations" in r.message for r in caplog.records
        )
        assert any(policy in r.message for r in caplog.records)

    def test_precedence_still_fills_missing_from_other_source(self):
        a = _frame([np.nan, 10.0])
        b = _frame([1.0, 20.0])
        merged = merge_forcing_frames([a, b], ["a", "b"], on_conflict="first")
        assert merged["Tair"].tolist() == [1.0, 10.0]

    def test_conflict_within_a_single_source_is_detected(self):
        a = pd.concat([_frame([1.0]), _frame([2.0])])
        with pytest.raises(ForcingConflictError):
            merge_forcing_frames([a], ["a"])

    def test_unknown_policy_rejected(self):
        with pytest.raises(ValueError, match="on_conflict"):
            merge_forcing_frames([_frame([1.0])], ["a"], on_conflict="keep")

    def test_mismatched_sources_rejected(self):
        with pytest.raises(ValueError, match="sources"):
            merge_forcing_frames([_frame([1.0])], ["a", "b"])

    def test_many_conflicts_report_totals_but_cap_detail(self, caplog):
        """Detail collection is capped even when every cell conflicts."""
        n = 2000
        a = _frame(np.arange(n, dtype=float), kdown=np.zeros(n))
        b = _frame(np.arange(n, dtype=float) + 1000, kdown=np.ones(n))
        with pytest.raises(ForcingConflictError) as excinfo:
            merge_forcing_frames([a, b], ["a", "b"])
        msg = str(excinfo.value)
        assert f"{n} timestamp(s)" in msg
        assert f"({2 * n} variable cell(s))" in msg
        assert f"+{2 * n - 10} more" in msg
        # 10 detail lines only, in timestamp order, both variables of a row
        detail_lines = [ln for ln in msg.splitlines() if ln.startswith("  2012-")]
        assert len(detail_lines) == 10
        assert detail_lines[0].startswith(
            "  2012-01-01 01:00:00: Tair = 0.0 (a) vs 1000.0 (b)"
        )
        assert detail_lines[1].startswith(
            "  2012-01-01 01:00:00: kdown = 0.0 (a) vs 1.0 (b)"
        )
        # policies stay correct over the whole frame, not just the reported cells
        with caplog.at_level(logging.WARNING, logger="SuPy"):
            first = merge_forcing_frames([a, b], ["a", "b"], on_conflict="first")
            last = merge_forcing_frames([a, b], ["a", "b"], on_conflict="last")
        assert np.array_equal(first["Tair"].to_numpy(), np.arange(n, dtype=float))
        assert np.array_equal(last["Tair"].to_numpy(), np.arange(n, dtype=float) + 1000)
        assert (first["kdown"] == 0).all() and (last["kdown"] == 1).all()
        assert len(first) == n and first.index.is_unique

    def test_sources_per_cell_are_bounded(self):
        frames = [_frame([float(i)]) for i in range(8)]
        with pytest.raises(ForcingConflictError) as excinfo:
            merge_forcing_frames(frames, [f"f{i}" for i in range(8)])
        msg = str(excinfo.value)
        detail = [ln for ln in msg.splitlines() if ln.startswith("  2012-")]
        assert len(detail) == 1
        assert "(f4)" in detail[0] and "(f5)" not in detail[0]
        assert "+3 more source(s)" in detail[0]
        assert "across 8 overlapping source(s)" in msg and "(+3 more)" in msg
        assert "1 timestamp(s) (1 variable cell(s))" in msg

    def test_error_message_is_bounded(self):
        a = _frame(np.arange(50, dtype=float))
        b = _frame(np.arange(50, dtype=float) + 100)
        with pytest.raises(ForcingConflictError) as excinfo:
            merge_forcing_frames([a, b], ["a", "b"])
        msg = str(excinfo.value)
        assert "50 timestamp(s)" in msg
        assert "+40 more" in msg
        assert msg.count("Tair =") == 10


# ---------------------------------------------------------------------------
# Public loaders: the audit reproduction
# ---------------------------------------------------------------------------


class TestFromFileOverlap:
    def test_conflicting_files_rejected_in_both_orders(self, conflicting_files):
        first, second = conflicting_files
        for order in ([first, second], [second, first]):
            with pytest.raises(ForcingConflictError) as excinfo:
                SUEWSForcing.from_file(order, tstep_mod=None)
            assert "first.txt" in str(excinfo.value)
            assert "second.txt" in str(excinfo.value)
            assert "Tair" in str(excinfo.value)

    def test_identical_files_load_once(self, identical_files):
        a, b = identical_files
        merged = SUEWSForcing.from_file([a, b], tstep_mod=None)
        single = SUEWSForcing.from_file(a, tstep_mod=None)
        pd.testing.assert_frame_equal(merged.df, single.df)
        assert merged.df.index.is_unique

    def test_explicit_precedence_follows_list_order(self, conflicting_files):
        first, second = conflicting_files
        fwd_first = SUEWSForcing.from_file(
            [first, second], tstep_mod=None, on_conflict="first"
        )
        fwd_last = SUEWSForcing.from_file(
            [first, second], tstep_mod=None, on_conflict="last"
        )
        rev_last = SUEWSForcing.from_file(
            [second, first], tstep_mod=None, on_conflict="last"
        )
        assert set(fwd_first.df["Tair"]) == {10.0}
        assert set(fwd_last.df["Tair"]) == {20.0}
        assert set(rev_last.df["Tair"]) == {10.0}

    def test_error_is_a_value_error(self, conflicting_files):
        with pytest.raises(ValueError):
            SUEWSForcing.from_file(list(conflicting_files), tstep_mod=None)


class TestLowerLevelLoaders:
    def test_yaml_loader_rejects_conflicts(self, conflicting_files):
        with pytest.raises(ForcingConflictError):
            load_SUEWS_Forcing_met_df_yaml([str(p) for p in conflicting_files])

    def test_yaml_loader_dedups_identical(self, identical_files):
        df = load_SUEWS_Forcing_met_df_yaml([str(p) for p in identical_files])
        assert df.index.is_unique
        assert len(df) == 4

    def test_yaml_loader_directory_rejects_conflicts(self, conflicting_files):
        with pytest.raises(ForcingConflictError):
            load_SUEWS_Forcing_met_df_yaml(str(conflicting_files[0].parent))

    def test_wildcard_pattern_rejects_conflicts(self, conflicting_files):
        pattern = conflicting_files[0].parent / "*.txt"
        with pytest.raises(ForcingConflictError):
            read_forcing(str(pattern), tstep_mod=None)

    def test_wildcard_pattern_precedence_opt_in(self, conflicting_files):
        pattern = conflicting_files[0].parent / "*.txt"
        df = read_forcing(str(pattern), tstep_mod=None, on_conflict="last")
        # sorted glob order: first.txt, second.txt -> "last" keeps 20 C
        assert set(df["Tair"]) == {20.0}


class TestSimulationUpdateForcing:
    def test_list_rejects_conflicts(self, conflicting_files):
        sim = SUEWSSimulation()
        with pytest.raises(ForcingConflictError):
            sim.update_forcing(list(conflicting_files))

    def test_list_precedence_opt_in(self, conflicting_files):
        sim = SUEWSSimulation()
        sim.update_forcing(list(conflicting_files), on_conflict="last")
        assert set(sim.forcing["Tair"].round(6)) == {20.0}

    def test_list_identical_files_unique_index(self, identical_files):
        sim = SUEWSSimulation()
        sim.update_forcing(list(identical_files))
        assert sim.forcing.index.is_unique

    def test_directory_rejects_conflicts(self, conflicting_files):
        sim = SUEWSSimulation()
        with pytest.warns(DeprecationWarning), pytest.raises(ForcingConflictError):
            sim.update_forcing(conflicting_files[0].parent)
