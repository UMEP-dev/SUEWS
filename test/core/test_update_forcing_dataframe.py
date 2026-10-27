"""Regression tests for in-memory forcing normalisation (gh#1537).

``SUEWSSimulation.update_forcing`` used to assign DataFrame / SUEWSForcing
inputs directly to ``self._df_forcing``, bypassing the timestep resampling,
temporal-column refresh, and unit contract that file loading applies. Hourly
DataFrame forcing then ran at the input timestep instead of the model
timestep, so a 300 s model never reached the ``23:55`` daily-write point and
``DailyState`` stopped being saved. These tests pin the normalised behaviour.
"""

from importlib.resources import as_file, files

import numpy as np
import pandas as pd
from pandas.tseries.frequencies import to_offset
import pytest

# ruff: disable[import-private-name]
from supy._check import check_forcing
from supy._misc import infer_freq_offset
from supy.suews_forcing import SUEWSForcing
from supy.suews_sim import SUEWSSimulation
from supy.util import read_forcing

# ruff: enable[import-private-name]

pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def sample_paths():
    """Materialise the config and its sibling forcing for the fixture lifetime."""
    with as_file(files("supy") / "sample_data") as path_sample:
        yield {
            "config": path_sample / "sample_config.yml",
            "forcing": path_sample / "Kc_2012_data_60.txt",
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
        assert infer_freq_offset(sim_path.forcing.index) == to_offset("5min")
        assert infer_freq_offset(sim_df.forcing.index) == to_offset("5min")
        assert len(sim_df.forcing) == len(sim_path.forcing)
        assert sim_df.forcing.index.equals(sim_path.forcing.index)

    @pytest.mark.core
    def test_hourly_input_is_upsampled(self, sample_paths, hourly_forcing):
        """An hourly DataFrame is upsampled to the 300 s model timestep."""
        assert infer_freq_offset(hourly_forcing.index) == to_offset("h")

        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)

        diffs = sim.forcing.index.to_series().diff().dropna()
        assert (diffs == pd.Timedelta(seconds=300)).all()
        # 8784 hourly rows -> 12 per hour after upsampling.
        assert len(sim.forcing) == len(hourly_forcing) * 12

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
    @pytest.mark.parametrize("wrapped", [False, True], ids=["dataframe", "forcing"])
    def test_kpa_pressure_rejected(self, hourly_forcing, wrapped):
        """A DataFrame with kPa pressure raises a clear ValueError."""
        df_kpa = hourly_forcing.copy()
        df_kpa["pres"] /= 10.0  # hPa -> kPa

        sim = SUEWSSimulation()
        with pytest.raises(ValueError, match="kPa"):
            sim.update_forcing(SUEWSForcing(df_kpa) if wrapped else df_kpa)

    @pytest.mark.core
    @pytest.mark.parametrize("wrapped", [False, True], ids=["dataframe", "forcing"])
    def test_hpa_pressure_accepted(self, sample_paths, hourly_forcing, wrapped):
        """Properly-converted hPa pressure passes the unit check."""
        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(
            SUEWSForcing(hourly_forcing) if wrapped else hourly_forcing
        )  # already hPa
        assert sim.forcing["pres"].median() > 300

    @pytest.mark.core
    @pytest.mark.parametrize("pressure", [68.0, 101.325, 130.0])
    @pytest.mark.parametrize("loader", ["read_forcing", "from_file"])
    def test_kpa_file_loads_as_hpa(self, sample_paths, tmp_path, pressure, loader):
        """Both file APIs convert kPa to hPa once, including range endpoints."""
        df_file = pd.read_csv(sample_paths["forcing"], sep=r"\s+").iloc[:24].copy()
        df_file["pres"] = pressure
        path_forcing = tmp_path / "forcing.txt"
        df_file.to_csv(path_forcing, sep=" ", index=False)

        if loader == "read_forcing":
            df_loaded = read_forcing(str(path_forcing), tstep_mod=None)
        else:
            df_loaded = SUEWSForcing.from_file(path_forcing).df

        # Independent physical unit oracle: one kPa is ten hPa.
        np.testing.assert_allclose(df_loaded["pres"], pressure * 10.0, rtol=1e-12)

    def test_out_of_range_message_states_both_units(self, hourly_forcing):
        """A pressure outlier is reported in hPa (what the DataFrame holds)
        and in kPa (what a forcing file holds)."""
        df_bad = hourly_forcing.copy()
        df_bad.iloc[5, df_bad.columns.get_loc("pres")] = 500.0  # hPa, too low

        issues = check_forcing(df_bad, fix=False)

        message = next(issue for issue in issues if "`pres`" in issue)
        assert "[680, 1300] hPa in memory" in message
        assert "[68, 130] kPa in a forcing file" in message


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

        assert "isec" in sim.forcing.columns
        # 5-min boundaries have zero seconds.
        assert (sim.forcing["isec"] == 0).all()

    @pytest.mark.core
    def test_temporal_columns_track_index(self, sample_paths, hourly_forcing):
        """iy/id/it/imin/isec are reasserted from the resampled index."""
        sim = SUEWSSimulation()
        sim.update_config(str(sample_paths["config"]), auto_load_forcing=False)
        sim.update_forcing(hourly_forcing)

        idx = sim.forcing.index
        assert (sim.forcing["iy"] == idx.year).all()
        assert (sim.forcing["id"] == idx.day_of_year).all()
        assert (sim.forcing["it"] == idx.hour).all()
        assert (sim.forcing["imin"] == idx.minute).all()


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
