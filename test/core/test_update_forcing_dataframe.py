"""Regression tests for in-memory forcing normalisation (gh#1537).

``SUEWSSimulation.update_forcing`` used to assign DataFrame / SUEWSForcing
inputs directly to ``self._df_forcing``, bypassing the timestep resampling,
temporal-column refresh, and unit contract that file loading applies. Hourly
DataFrame forcing then ran at the input timestep instead of the model
timestep, so a 300 s model never reached the ``23:55`` daily-write point and
``DailyState`` stopped being saved. These tests pin the normalised behaviour.
"""

import pandas as pd
import pytest

try:
    from importlib.resources import files
except ImportError:  # pragma: no cover - Python < 3.9 fallback
    from importlib_resources import files

from supy.suews_sim import SUEWSSimulation
from supy.util import read_forcing

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def sample_paths():
    """Resolve the bundled sample config and hourly forcing file.

    ``files(...)`` returns a meson-python editable traversable; ``str`` of a
    file resolves to the real path, but the directory itself is not glob-able,
    so each file is resolved individually.
    """
    return {
        "config": str(files("supy").joinpath("sample_data/sample_config.yml")),
        "forcing": str(files("supy").joinpath("sample_data/Kc_2012_data_60.txt")),
    }


@pytest.fixture(scope="module")
def hourly_forcing(sample_paths):
    """Canonical, hPa, NOT-yet-resampled hourly forcing DataFrame."""
    return read_forcing(str(sample_paths["forcing"]), tstep_mod=None)


class TestTimestepNormalisation:
    """AC1/AC2: DataFrame forcing is resampled like path/file forcing."""

    @pytest.mark.core
    def test_dataframe_matches_path_loading(self, sample_paths, hourly_forcing):
        """DataFrame and config/path loading yield the same forcing shape."""
        # ARRANGE: config/path loading (the reference behaviour)
        sim_path = SUEWSSimulation(str(sample_paths["config"]))

        # ACT: DataFrame loading of the same data via the in-memory path
        sim_df = SUEWSSimulation()
        sim_df.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim_df.update_forcing(hourly_forcing)

        # ASSERT: both resampled to the model timestep (5 min)
        assert pd.infer_freq(sim_path._df_forcing.index) == "5min"
        assert pd.infer_freq(sim_df._df_forcing.index) == "5min"
        assert len(sim_df._df_forcing) == len(sim_path._df_forcing)
        assert sim_df._df_forcing.index.equals(sim_path._df_forcing.index)

    @pytest.mark.core
    def test_hourly_input_is_upsampled(self, sample_paths, hourly_forcing):
        """An hourly DataFrame is upsampled to the 300 s model timestep."""
        assert pd.infer_freq(hourly_forcing.index) == "h"

        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)

        diffs = sim._df_forcing.index.to_series().diff().dropna()
        assert (diffs == pd.Timedelta(seconds=300)).all()
        # 8784 hourly rows -> 12 per hour after upsampling.
        assert len(sim._df_forcing) == len(hourly_forcing) * 12

    @pytest.mark.core
    def test_dailystate_written_for_dataframe_forcing(
        self, sample_paths, hourly_forcing
    ):
        """End-to-end: DailyState is saved once the forcing is resampled."""
        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)

        out = sim.run(
            start_date="2012-01-01", end_date="2012-01-03 23:59:59", n_jobs=1
        )
        df_daily = out.df.xs("DailyState", level="group", axis=1)
        # Three complete days -> three daily-state write points (the symptom
        # in gh#1537 was zero rows here).
        assert len(df_daily.dropna(how="all")) == 3


class TestPressureUnitContract:
    """AC3: pressure must be in hPa; kPa is rejected early and clearly."""

    @pytest.mark.core
    def test_kpa_pressure_rejected(self, hourly_forcing):
        """A DataFrame with kPa pressure raises a clear ValueError."""
        df_kpa = hourly_forcing.copy()
        df_kpa["pres"] /= 10.0  # hPa -> kPa

        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match="kPa"):
            sim.update_forcing(df_kpa)

    @pytest.mark.core
    def test_hpa_pressure_accepted(self, sample_paths, hourly_forcing):
        """Properly-converted hPa pressure passes the unit check."""
        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)  # already hPa
        assert sim._df_forcing["pres"].median() > 300


class TestTemporalColumns:
    """AC4: isec (and the other temporal columns) follow the index."""

    @pytest.mark.core
    def test_isec_derived_when_missing(self, sample_paths, hourly_forcing):
        """A DataFrame without isec gains a correct isec column."""
        df_no_isec = hourly_forcing.drop(columns=["isec"], errors="ignore")
        assert "isec" not in df_no_isec.columns

        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(df_no_isec)

        assert "isec" in sim._df_forcing.columns
        # 5-min boundaries have zero seconds.
        assert (sim._df_forcing["isec"] == 0).all()

    @pytest.mark.core
    def test_temporal_columns_track_index(self, sample_paths, hourly_forcing):
        """iy/id/it/imin/isec are reasserted from the resampled index."""
        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)

        idx = sim._df_forcing.index
        assert (sim._df_forcing["iy"] == idx.year).all()
        assert (sim._df_forcing["id"] == idx.dayofyear).all()
        assert (sim._df_forcing["it"] == idx.hour).all()
        assert (sim._df_forcing["imin"] == idx.minute).all()


class TestValidationFailFast:
    """Non-canonical DataFrames are rejected with actionable errors."""

    @pytest.mark.core
    def test_non_datetime_index_rejected(self, hourly_forcing):
        """A DataFrame without a DatetimeIndex raises ValueError."""
        df_bad = hourly_forcing.reset_index(drop=True)
        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match="DatetimeIndex"):
            sim.update_forcing(df_bad)

    @pytest.mark.core
    def test_missing_baseline_column_rejected(self, hourly_forcing):
        """Dropping a required meteorological column raises ValueError."""
        df_bad = hourly_forcing.drop(columns=["Tair"])
        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match="missing required columns"):
            sim.update_forcing(df_bad)

    @pytest.mark.core
    def test_single_row_rejected(self, hourly_forcing):
        """A single-timestamp DataFrame cannot infer a timestep."""
        df_bad = hourly_forcing.iloc[:1]
        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match="at least two timestamps"):
            sim.update_forcing(df_bad)
