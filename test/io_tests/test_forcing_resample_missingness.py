"""Missingness and coverage semantics of ``SUEWSForcing.resample``.

Regression tests for the audit finding that coarsening forcing data
aggregated the ``-999`` sentinel as a number: an all-missing rain interval
became 0 mm, two missing ``Wuh`` readings summed to -1998 mm and the mean
of ``100`` and ``-999`` W m-2 came out as -449.5.
"""

import numpy as np
import pandas as pd
import pytest

from supy.suews_forcing import SUEWSForcing

pytestmark = [pytest.mark.api]

# SUEWS missing sentinel as written to forcing files and consumed by the kernel.
MISSING = -999.0


def _forcing(columns, start="2024-01-01 00:05", freq="5min", extras=None):
    n = len(next(iter(columns.values())))
    idx = pd.date_range(start, periods=n, freq=freq)
    data = {
        "iy": idx.year,
        "id": idx.dayofyear,
        "it": idx.hour,
        "imin": idx.minute,
        **{name: np.asarray(values, dtype=float) for name, values in columns.items()},
    }
    forcing = SUEWSForcing(pd.DataFrame(data, index=idx), source="test")
    if extras:
        forcing._extras = {k: np.asarray(v, dtype=float) for k, v in extras.items()}
    return forcing


class TestSentinelsAreNotAggregated:
    """The three failure modes from the audit probe."""

    def test_all_missing_rain_stays_missing(self):
        forcing = _forcing({"rain": [np.nan] * 4})
        rain = forcing.resample("10min").df["rain"]
        assert (rain == MISSING).all()

    def test_missing_wuh_sentinels_are_not_summed(self):
        forcing = _forcing({"Wuh": [MISSING] * 4})
        wuh = forcing.resample("10min").df["Wuh"]
        assert (wuh == MISSING).all()

    def test_mean_with_a_missing_row_is_missing(self):
        forcing = _forcing({"kdown": [100.0, MISSING, 100.0, MISSING]})
        kdown = forcing.resample("10min").df["kdown"]
        assert (kdown == MISSING).all()

    def test_defensive_sentinel_threshold_is_masked(self):
        """Values at or below -900 are missing, as in the Fortran runtime."""
        forcing = _forcing({"kdown": [100.0, -950.0, 100.0, 100.0]})
        kdown = forcing.resample("10min").df["kdown"]
        assert kdown.iloc[0] == MISSING
        assert np.isclose(kdown.iloc[1], 100.0)


class TestCoverageRules:
    def test_fully_covered_intervals_aggregate_by_type(self):
        forcing = _forcing({
            "rain": [0.1, 0.2, 0.3, 0.4],
            "kdown": [100.0, 200.0, 300.0, 400.0],
            "Tair": [10.0, 11.0, 12.0, 13.0],
        })
        out = forcing.resample("10min").df
        assert np.allclose(out["rain"], [0.3, 0.7])
        assert np.allclose(out["kdown"], [150.0, 350.0])
        assert np.allclose(out["Tair"], [11.0, 13.0])

    def test_instantaneous_endpoint_is_not_backfilled(self):
        """A missing endpoint is not replaced by an earlier valid reading."""
        forcing = _forcing({"Tair": [20.0, MISSING, 21.0, 22.0]})
        tair = forcing.resample("10min").df["Tair"]
        assert tair.iloc[0] == MISSING
        assert np.isclose(tair.iloc[1], 22.0)

    def test_partial_edge_interval_is_missing(self):
        """Rows starting on the hour leave the first interval half-covered."""
        forcing = _forcing(
            {"rain": [1.0] * 5, "Tair": [10.0] * 5}, start="2024-01-01 00:00"
        )
        out = forcing.resample("10min").df
        assert out.index[0] == pd.Timestamp("2024-01-01 00:00")
        assert out["rain"].iloc[0] == MISSING
        assert out["Tair"].iloc[0] == MISSING
        assert np.allclose(out["rain"].iloc[1:], 2.0)
        assert np.allclose(out["Tair"].iloc[1:], 10.0)

    def test_temporal_columns_track_output_index(self):
        forcing = _forcing({"Tair": np.arange(24.0)}, start="2024-12-31 23:05")
        out = forcing.resample("1h").df
        idx = out.index
        assert list(idx) == [
            pd.Timestamp("2025-01-01 00:00"),
            pd.Timestamp("2025-01-01 01:00"),
        ]
        assert (out["iy"] == idx.year).all()
        assert (out["id"] == idx.dayofyear).all()
        assert (out["it"] == idx.hour).all()
        assert (out["imin"] == idx.minute).all()
        assert out["iy"].dtype.kind == "i"
        assert list(out.columns) == list(forcing.df.columns)
        assert forcing.resample("1h").timestep == pd.Timedelta("1h")


class TestExtras:
    def test_per_surface_extras_follow_the_same_rules(self):
        forcing = _forcing(
            {"Tair": [10.0] * 4},
            extras={
                "wuh_grass": [1.0, MISSING, 2.0, 3.0],
                "lai_grass": [1.0, 2.0, 3.0, MISSING],
            },
        )
        out = forcing.resample("10min")
        assert np.array_equal(out.extras["wuh_grass"], [MISSING, 5.0])
        assert np.array_equal(out.extras["lai_grass"], [2.0, MISSING])
        assert "wuh_grass" not in out.df.columns


class TestFrequencyContract:
    def test_same_frequency_returns_data_unchanged(self):
        forcing = _forcing(
            {"Tair": [10.0, MISSING, 12.0, 13.0]}, extras={"lai_grass": [1.0] * 4}
        )
        same = forcing.resample("5min")
        pd.testing.assert_frame_equal(same.df, forcing.df)
        assert np.array_equal(same.extras["lai_grass"], forcing.extras["lai_grass"])

    def test_upsampling_is_rejected_with_the_disaggregation_path(self):
        forcing = _forcing({"Tair": [10.0] * 4})
        with pytest.raises(ValueError, match="tstep_mod"):
            forcing.resample("1min")

    def test_non_integer_multiple_is_rejected(self):
        forcing = _forcing({"Tair": [10.0] * 4})
        with pytest.raises(ValueError, match="integer multiple"):
            forcing.resample("7min")

    def test_calendar_frequency_is_rejected(self):
        forcing = _forcing({"Tair": [10.0] * 4})
        with pytest.raises(ValueError, match="fixed-length"):
            forcing.resample("ME")

    def test_offset_phase_timestamps_are_rejected(self):
        """Rows at 00:02 and 00:07 cannot tile (00:00, 00:10] and are not shifted."""
        forcing = _forcing(
            {"Tair": [10.0, 20.0], "rain": [1.0, 2.0]}, start="2024-01-01 00:02"
        )
        with pytest.raises(ValueError, match="source-step grid"):
            forcing.resample("10min")

    def test_aligned_phase_is_unchanged_by_the_grid_check(self):
        forcing = _forcing(
            {"Tair": [10.0, 20.0], "rain": [1.0, 2.0]}, start="2024-01-01 00:05"
        )
        out = forcing.resample("10min").df
        assert np.isclose(out["Tair"].iloc[0], 20.0)
        assert np.isclose(out["rain"].iloc[0], 3.0)

    def test_bin_whose_last_row_misses_the_label_is_missing(self):
        """Defence in depth inside the aggregation step, independent of the phase check."""
        idx = pd.date_range("2024-01-01 00:02", periods=2, freq="5min")
        masked = pd.DataFrame({"Tair": [10.0, 20.0], "rain": [1.0, 2.0]}, index=idx)
        out = SUEWSForcing._aggregate_bins(masked, "10min", 2)
        assert out.isna().all().all()

    def test_irregular_index_is_rejected(self):
        idx = pd.DatetimeIndex([
            "2024-01-01 00:05",
            "2024-01-01 00:10",
            "2024-01-01 00:25",
        ])
        forcing = SUEWSForcing(pd.DataFrame({"Tair": [1.0, 2.0, 3.0]}, index=idx))
        with pytest.raises(ValueError, match="regular DatetimeIndex"):
            forcing.resample("10min")
