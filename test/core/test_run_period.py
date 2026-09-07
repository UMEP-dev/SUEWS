"""Requested-period resolution and forcing coverage checks (gh#1268).

Forcing rows are stamped at the end of each interval, so a calendar day at a
5-minute timestep is ``D 00:05`` .. ``D+1 00:00``. These tests pin the
date-only and explicit-timestamp semantics of ``start_date`` / ``end_date``
and the rule that ``run()`` refuses a period the forcing does not cover
unless the caller opts in to clipping.
"""

import datetime as dt
import logging

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy._run_period import (
    is_date_only,
    required_rows,
    resolve_run_period,
    slice_forcing_to_period,
)

pytestmark = pytest.mark.api

STEP = pd.Timedelta(minutes=5)
STEPS_PER_DAY = 288


def _forcing(start="2012-01-01 00:05", periods=STEPS_PER_DAY * 3):
    """Regular 5-minute frame whose first row is the first interval of a day."""
    index = pd.date_range(start, periods=periods, freq="5min", name="datetime")
    return pd.DataFrame({"Tair": np.arange(periods, dtype=float)}, index=index)


def _period(start, end, df):
    return resolve_run_period(start, end, df.index)


class TestDateOnlyDetection:
    @pytest.mark.parametrize(
        ("value", "expected"),
        [
            ("2012-01-01", True),
            ("2012-1-1", True),
            (" 2012-01-01 ", True),
            (dt.date(2012, 1, 1), True),
            ("2012-01-01 00:00", False),
            ("2012-01-01T00:00:00", False),
            (pd.Timestamp("2012-01-01"), False),
            (dt.datetime(2012, 1, 1), False),
            (None, False),
        ],
    )
    def test_is_date_only(self, value, expected):
        assert is_date_only(value) is expected


class TestDateOnlySemantics:
    def test_date_only_end_includes_following_midnight_row(self):
        df = _forcing()
        df_slice, meta = slice_forcing_to_period(
            df, _period("2012-01-01", "2012-01-01", df)
        )
        assert df_slice.index[0] == pd.Timestamp("2012-01-01 00:05")
        assert df_slice.index[-1] == pd.Timestamp("2012-01-02 00:00")
        assert len(df_slice) == STEPS_PER_DAY
        assert meta["clipped"] is False
        assert meta["requested_end"] == pd.Timestamp("2012-01-02 00:00")

    def test_date_only_start_excludes_previous_day_interval_end(self):
        # Forcing that starts at midnight: that row is the last interval of
        # the previous day and must not be part of a run that starts on D.
        df = _forcing(start="2012-01-01 00:00", periods=STEPS_PER_DAY + 1)
        df_slice, _ = slice_forcing_to_period(df, _period("2012-01-01", None, df))
        assert df_slice.index[0] == pd.Timestamp("2012-01-01 00:05")
        assert len(df_slice) == STEPS_PER_DAY

    def test_full_day_request_on_first_of_month(self):
        df = _forcing()
        first, last = required_rows(_period("2012-01-02", "2012-01-03", df), df.index)
        assert first == pd.Timestamp("2012-01-02 00:05")
        assert last == pd.Timestamp("2012-01-04 00:00")

    def test_python_date_objects_are_date_only(self):
        df = _forcing()
        period = _period(dt.date(2012, 1, 2), dt.date(2012, 1, 2), df)
        df_slice, _ = slice_forcing_to_period(df, period)
        assert df_slice.index[0] == pd.Timestamp("2012-01-02 00:05")
        assert df_slice.index[-1] == pd.Timestamp("2012-01-03 00:00")


class TestExplicitTimestampSemantics:
    def test_explicit_bounds_are_row_inclusive(self):
        df = _forcing()
        period = _period(df.index[12], df.index[23], df)
        df_slice, meta = slice_forcing_to_period(df, period)
        assert df_slice.index[0] == df.index[12]
        assert df_slice.index[-1] == df.index[23]
        assert len(df_slice) == 12
        assert meta["clipped"] is False

    def test_explicit_end_inside_final_interval_is_covered(self):
        # An end that falls between two rows needs only the last row before it.
        df = _forcing(periods=STEPS_PER_DAY)  # ends 2012-01-02 00:00
        period = _period("2012-01-01", "2012-01-01 23:59:59", df)
        df_slice, meta = slice_forcing_to_period(df, period)
        assert df_slice.index[-1] == pd.Timestamp("2012-01-01 23:55")
        assert meta["clipped"] is False

    def test_explicit_start_between_rows_is_covered(self):
        df = _forcing()
        period = _period("2012-01-01 00:03", df.index[5], df)
        df_slice, meta = slice_forcing_to_period(df, period)
        assert df_slice.index[0] == pd.Timestamp("2012-01-01 00:05")
        assert meta["clipped"] is False

    def test_end_before_start_rejected(self):
        df = _forcing()
        with pytest.raises(ValueError, match="before start_date"):
            _period("2012-01-02", "2012-01-01", df)


class TestCoverage:
    def test_unbounded_request_uses_forcing_bounds(self):
        df = _forcing()
        df_slice, meta = slice_forcing_to_period(df, _period(None, None, df))
        assert len(df_slice) == len(df)
        assert meta["requested_start"] is None
        assert meta["requested_end"] is None
        assert meta["actual_start"] == df.index[0]
        assert meta["actual_end"] == df.index[-1]

    def test_request_ending_after_forcing_rejected(self):
        df = _forcing()  # 3 days
        with pytest.raises(ValueError, match="does not cover") as excinfo:
            slice_forcing_to_period(df, _period("2012-01-01", "2012-01-10", df))
        message = str(excinfo.value)
        assert "2012-01-11 00:00:00" in message  # last required row
        assert "clip_to_forcing=True" in message

    def test_request_starting_before_forcing_rejected(self):
        df = _forcing()
        with pytest.raises(ValueError, match="does not cover"):
            slice_forcing_to_period(df, _period("2011-12-31", "2012-01-01", df))

    def test_request_wholly_outside_forcing_rejected_even_with_clip(self):
        df = _forcing()
        with pytest.raises(ValueError, match="does not overlap"):
            slice_forcing_to_period(
                df, _period("2015-01-01", "2015-12-31", df), clip_to_forcing=True
            )

    def test_clip_runs_overlap_and_records_metadata(self, caplog):
        df = _forcing()
        with caplog.at_level(logging.WARNING, logger="SuPy"):
            df_slice, meta = slice_forcing_to_period(
                df, _period("2011-01-01", "2013-01-01", df), clip_to_forcing=True
            )
        assert len(df_slice) == len(df)
        assert meta["clipped"] is True
        assert meta["policy"] == "clip"
        assert meta["actual_start"] == df.index[0]
        assert meta["actual_end"] == df.index[-1]
        assert any("clip_to_forcing=True" in rec.getMessage() for rec in caplog.records)

    def test_covered_request_not_marked_clipped_under_clip_policy(self):
        df = _forcing()
        _, meta = slice_forcing_to_period(
            df, _period("2012-01-02", "2012-01-02", df), clip_to_forcing=True
        )
        assert meta["clipped"] is False
        assert meta["policy"] == "clip"

    def test_empty_forcing_rejected(self):
        df = _forcing().iloc[:0]
        with pytest.raises(ValueError, match="forcing data is empty"):
            slice_forcing_to_period(df, _period(None, None, df))


@pytest.mark.core
@pytest.mark.rust
class TestRunIntegration:
    """``SUEWSSimulation.run`` enforces the policy on the sample data."""

    def test_sample_config_period_matches_sample_forcing(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        control = sim.config.model.control
        forcing_index = sim.forcing.df.index
        period = resolve_run_period(control.start_time, control.end_time, forcing_index)
        df_slice, meta = slice_forcing_to_period(sim.forcing.df, period)
        assert meta["clipped"] is False
        assert df_slice.index[0] == forcing_index[0] == pd.Timestamp("2012-01-01 00:05")
        assert (
            df_slice.index[-1] == forcing_index[-1] == pd.Timestamp("2013-01-01 00:00")
        )
        assert len(df_slice) == len(forcing_index)

    def test_truncated_forcing_rejected_by_default(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        sim.update_forcing(sim.forcing.df.iloc[:24])
        with pytest.raises(ValueError, match="does not cover"):
            sim.run(n_jobs=1)
        assert sim._run_period is None

    def test_truncated_forcing_rejected_with_validation_off(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        sim.update_forcing(sim.forcing.df.iloc[:24])
        with pytest.raises(ValueError, match="does not cover"):
            sim.run(n_jobs=1, _validate_forcing=False)

    def test_explicit_request_beyond_forcing_rejected(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        sim.update_forcing(sim.forcing.df.iloc[:24])
        with pytest.raises(ValueError, match="does not cover"):
            sim.run(start_date="2011-01-01", end_date="2013-01-01", n_jobs=1)

    def test_clip_to_forcing_runs_overlap(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        sim.update_forcing(sim.forcing.df.iloc[:24])
        output = sim.run(
            start_date="2011-01-01",
            end_date="2013-01-01",
            n_jobs=1,
            clip_to_forcing=True,
        )
        assert len(output.df) == 24
        assert sim._run_period["clipped"] is True
        assert sim._run_period["requested_start_raw"] == "2011-01-01"
        assert sim._run_period["actual_end"] == pd.Timestamp("2012-01-01 02:00")

    def test_explicit_end_within_forcing_runs(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        forcing = sim.forcing.df.iloc[:24]
        sim.update_forcing(forcing)
        output = sim.run(end_date=forcing.index[11], n_jobs=1)
        assert len(output.df) == 12
        assert sim._run_period["clipped"] is False
        assert sim._run_period["policy"] == "strict"

    def test_date_only_continuation_after_checkpoint(self):
        # A run through 2012-01-01 leaves last_timestamp at 2012-01-02 00:00;
        # a date-only start of 2012-01-02 begins at 00:05, one timestep on,
        # so the continuation check accepts it.
        sim = sp.SUEWSSimulation.from_sample_data()
        forcing = sim.forcing.df.iloc[: STEPS_PER_DAY + 24]
        sim.update_forcing(forcing)
        first = sim.run(end_date="2012-01-01", n_jobs=1)
        assert len(first.df) == STEPS_PER_DAY
        assert pd.Timestamp(sim.checkpoint.last_timestamp) == pd.Timestamp(
            "2012-01-02 00:00"
        )
        second = sim.run(start_date="2012-01-02", end_date=forcing.index[-1], n_jobs=1)
        assert second.df.index.get_level_values("datetime")[0] == pd.Timestamp(
            "2012-01-02 00:05"
        )
        assert len(second.df) == 24

    def test_reset_clears_run_period(self):
        sim = sp.SUEWSSimulation.from_sample_data()
        forcing = sim.forcing.df.iloc[:24]
        sim.update_forcing(forcing)
        sim.run(end_date=forcing.index[11], n_jobs=1)
        assert sim._run_period is not None
        sim.reset()
        assert sim._run_period is None
