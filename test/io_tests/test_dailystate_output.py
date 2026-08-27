"""Test DailyState output functionality."""

import tempfile

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pytest

import supy as sp

from supy._supy_module import _save_supy
from supy.data_model.core.model import OutputControl

pytestmark = pytest.mark.physics


class TestDailyStateOutput:
    """Test suite for DailyState output handling."""

    def test_dailystate_no_resampling(self, sample_data_loaded, sample_run_cached):
        """Test that DailyState output is not resampled and preserves daily values."""
        # Run for multiple days to ensure we have DailyState data
        n_steps = TIMESTEPS_PER_DAY * 3  # 3 days of 5-min data
        _, df_forcing = sample_data_loaded
        df_forcing_multi_day = df_forcing.iloc[:n_steps]

        # Run simulation
        df_output, df_state_final = sample_run_cached(n_steps)

        # Check DailyState exists in output
        assert "DailyState" in df_output.columns.get_level_values("group").unique()

        # Get DailyState data
        df_dailystate = df_output.loc[:, "DailyState"]

        # Remove all-NaN rows (DailyState only has values at end of each day)
        df_dailystate_clean = df_dailystate.dropna(how="all")

        # Check we have the expected number of daily values (one per day)
        n_days = len(
            pd.date_range(
                df_forcing_multi_day.index[0], df_forcing_multi_day.index[-1], freq="D"
            )
        )
        assert len(df_dailystate_clean) <= n_days
        assert len(df_dailystate_clean) > 0  # Should have at least some data

        # Check that values are only at day boundaries (around 23:55 or similar)
        hours = df_dailystate_clean.index.get_level_values("datetime").hour
        assert all(h >= 23 for h in hours), (
            "DailyState should only have values at end of day"
        )

    def test_dailystate_save_output(self, sample_run_cached):
        """Test that DailyState data is correctly saved to file."""
        # Run for multiple days (3 days of 5-min data)
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 3)

        # Save output with default settings (should include DailyState)
        with tempfile.TemporaryDirectory() as dir_temp:
            list_files = _save_supy(df_output, df_state_final, path_dir_save=dir_temp)

            # Check that DailyState file was created
            dailystate_files = [f for f in list_files if "DailyState" in f.name]
            assert len(dailystate_files) > 0, "DailyState file should be created"

            # Read the DailyState file and check it's not empty
            for ds_file in dailystate_files:
                df_saved = pd.read_csv(ds_file, sep="\t")
                assert len(df_saved) > 0, "DailyState file should not be empty"

                # Check that we have actual data values (not all -999)
                data_cols = [
                    c
                    for c in df_saved.columns
                    if c not in ["Year", "DOY", "Hour", "Min", "Dectime"]
                ]
                assert len(data_cols) > 0, "Should have data columns"

                # Check that at least some values are not -999 (missing data marker)
                has_real_data = False
                for col in data_cols:
                    if any(df_saved[col] != -999):
                        has_real_data = True
                        break
                assert has_real_data, "DailyState should contain actual data values"

                # Check that all days are present (no missing first day)
                doy_values = df_saved["DOY"].values
                expected_doys = list(range(1, len(doy_values) + 1))
                assert list(doy_values) == expected_doys, (
                    f"Missing days in output. Got DOYs: {list(doy_values)}, expected: {expected_doys}"
                )

    def test_dailystate_different_output_frequencies(self, sample_run_cached):
        """Test DailyState output with different resampling frequencies."""
        # Run for multiple days (2 days of 5-min data)
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 2)

        # Test with different output frequencies
        for freq_s in [300, 1800, 3600]:  # 5min, 30min, 60min
            with tempfile.TemporaryDirectory() as dir_temp:
                list_files = _save_supy(
                    df_output, df_state_final, path_dir_save=dir_temp, freq_s=freq_s
                )

                # Check DailyState file exists and has same content regardless of freq
                dailystate_files = [f for f in list_files if "DailyState" in f.name]
                assert len(dailystate_files) > 0

                # DailyState should not be affected by output frequency
                df_ds = pd.read_csv(dailystate_files[0], sep="\t")
                assert len(df_ds) > 0, f"DailyState should have data at freq={freq_s}"

    def test_dailystate_only_output_config_saves_file(self, sample_run_cached):
        """Test that requesting only DailyState writes a populated output file."""
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY)

        with tempfile.TemporaryDirectory() as dir_temp:
            list_files = _save_supy(
                df_output,
                df_state_final,
                path_dir_save=dir_temp,
                output_config=OutputControl(groups=["DailyState"]),
            )

            assert [f.name for f in list_files if "SUEWS" in f.name] == []
            dailystate_files = [f for f in list_files if "DailyState" in f.name]
            assert len(dailystate_files) == 1

            df_saved = pd.read_csv(dailystate_files[0], sep="\t")
            data_cols = [
                c
                for c in df_saved.columns
                if c not in ["Year", "DOY", "Hour", "Min", "Dectime"]
            ]

            assert len(df_saved) == 1
            assert (df_saved[data_cols] != -999).any().any()


    def test_dailystate_lai_responds_to_phenology(
        self, sample_run_cached, sample_data_loaded
    ):
        """LAI increases during leaf growth and decreases during senescence."""

        _, df_forcing = sample_data_loaded

        # Run the full available forcing period.
        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        lai_change = lai.diff()

        # The simulation should contain both leaf growth and senescence.
        assert (lai_change > 0).any(), "LAI should increase during leaf growth"
        assert (lai_change < 0).any(), "LAI should decrease during senescence"

        # LAI should increase while GDD is accumulating.
        growth = (gdd > 0) & (lai_change > 0)
        assert growth.any(), "No LAI increase found during GDD accumulation"

        # LAI should decrease while SDD is driving senescence.
        senescence = (sdd < 0) & (lai_change < 0)
        assert senescence.any(), "No LAI decrease found during senescence"

        # LAI must remain physically/configurationally bounded
        assert (lai >= 0).all()

    def test_dailystate_gdd_sdd_progression(
        self, sample_run_cached, sample_data_loaded, sample_config_loaded
    ):
        """GDD increases and SDD decreases until they are reset."""

        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        gdd_full_dectr = config_lai_dectr.gdd_full.value
        sdd_full_dectr = config_lai_dectr.sdd_full.value

        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        # Remove any initial missing values.
        gdd = gdd.dropna()
        sdd = sdd.dropna()

        # GDD should never decrease, except when it is reset to zero.
        gdd_after_change = gdd.iloc[1:]

        gdd_change = gdd.diff().dropna()
        gdd_decreases = gdd_change < 0

        assert (gdd_after_change[gdd_decreases] == 0).all(), (
            "GDD should only decrease when it is reset to 0"
        )

        # SDD should never increase, except when it is reset to zero.
        sdd_after_change = sdd.iloc[1:]

        sdd_change = sdd.diff().dropna()
        sdd_increases = sdd_change > 0

        assert (sdd_after_change[sdd_increases] == 0).all(), (
            "SDD should only increase when it is reset to 0"
        )

        # GDD should remain within its expected range.
        assert (gdd >= 0).all()
        assert (gdd <= gdd_full_dectr).all()

        # SDD should remain within its expected range.
        assert (sdd <= 0).all()
        assert (sdd >= sdd_full_dectr).all()

    def test_dailystate_gdd_sdd_seasonal_resets(
        self, sample_run_cached, sample_data_loaded, sample_config_loaded
    ):
        """GDD and SDD are reset at the seasonal transition."""

        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        gdd = df_dailystate["GDD_DecTr"].dropna()
        sdd = df_dailystate["SDD_DecTr"].dropna()

        doy = df_dailystate.loc[gdd.index].index.get_level_values("datetime").dayofyear

        # Northern hemisphere seasonal transition day.
        transition_day = doy == 140

        assert transition_day.any(), (
            "The available simulation should contain the seasonal transition day"
        )

        # SDD is reset to zero at the seasonal transition.
        sdd_transition = sdd.loc[transition_day]

        assert (sdd_transition == 0).all(), (
            "SDD should be reset to 0 on the seasonal transition day"
        )

    def test_dailystate_gdd_sdd_threshold_resets(
        self, sample_run_cached, sample_data_loaded, sample_config_loaded
    ):
        """GDD and SDD are reset when their seasonal thresholds are exceeded."""

        crit_days = 50

        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        gdd = df_dailystate["GDD_DecTr"].dropna()
        sdd = df_dailystate["SDD_DecTr"].dropna()

        doy = df_dailystate.loc[gdd.index].index.get_level_values("datetime").dayofyear

        # ---------------------------------------------------------------
        # SDD reset during summer.
        #
        # Once GDD exceeds crit_days, SDD is reset while DOY < 170.
        # ---------------------------------------------------------------
        summer_reset = (doy < 170) & (gdd > crit_days)

        assert summer_reset.any(), (
            "The available simulation should contain a summer SDD reset condition"
        )

        assert (sdd.loc[summer_reset] == 0).all(), (
            "SDD should be reset to 0 when GDD > crit_days "
            "during the summer period"
        )

        # ---------------------------------------------------------------
        # GDD reset during winter.
        #
        # Once SDD falls below -crit_days, GDD is reset while DOY > 170.
        # ---------------------------------------------------------------
        winter_reset = (doy > 170) & (sdd < -crit_days)

        assert winter_reset.any(), (
            "The available simulation should contain a winter GDD reset condition"
        )

        assert (gdd.loc[winter_reset] == 0).all(), (
            "GDD should be reset to 0 when SDD < -crit_days "
            "during the winter period"
        )

    def test_dailystate_lai_gdd_growth_branch(
        self, sample_data_loaded, sample_run_cached, sample_config_loaded
    ):
        """LAI increases while GDD is between zero and GDDFull."""

        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        gdd_full = config_lai_dectr.gdd_full.value
        lai_max = config_lai_dectr.lai_max.value

        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]

        lai_change = lai.diff().iloc[1:]
        gdd = gdd.iloc[1:]
        lai = lai.iloc[1:]

        # calculate_lai() uses the GDD growth branch when:
        #
        #     GDD > 0 AND GDD < GDDFull
        #
        growth = (gdd > 0) & (gdd < gdd_full)

        valid_growth = (
            (lai_change > 0)
            | (lai >= lai_max)
        )

        assert growth.any(), (
            "The GDD growth condition did not occur in the available simulation"
        )

        assert valid_growth[growth].all(), (
            "LAI should increase when GDD > 0 and GDD < GDDFull"
        )


    def test_dailystate_lai_northern_lai_type_0_sdd_senescence(
        self, sample_data_loaded, sample_config_loaded, sample_yaml_path
    ):
        """Northern Hemisphere LAItype 0 uses SDD to drive senescence."""

        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        # Check that this is a Northern Hemisphere simulation.
        assert sim._config.sites[0].properties.lat.value > 0

        # Configure LAItype 0 on the simulation itself.
        lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
        lai_config.lai_type.value = 0

        gdd_full = lai_config.gdd_full.value
        sdd_full = lai_config.sdd_full.value
        lai_min = lai_config.lai_min.value

        # Start from a fully grown canopy so only the autumn window needs
        # simulating: growth is complete (GDD at full) and senescence has
        # not begun. Running spring/summer from a cold start adds ~10 s per
        # test on the slowest CI platform without touching the branch under
        # test.
        initial_states = sim._config.sites[0].initial_states.dectr
        initial_states.lai_id.value = lai_config.lai_max.value
        initial_states.gdd_id.value = gdd_full
        initial_states.sdd_id.value = 0.0

        _, df_forcing = sample_data_loaded
        df_forcing = df_forcing.copy()

        # Autumn window: SDD accumulates and the senescence branch fires.
        day_of_year = df_forcing.index.dayofyear
        df_forcing = df_forcing[(day_of_year >= 230) & (day_of_year <= 330)]

        sim.update_forcing(df_forcing)
        sim.run()

        df_output = sim.output
        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        lai_change = lai.diff().iloc[1:]
        sdd = sdd.iloc[1:]
        gdd = gdd.iloc[1:]
        lai = lai.iloc[1:]

        # calculate_lai() first checks the GDD growth branch.
        #
        # Therefore LAItype 0 can only reach the SDD branch when:
        #
        #   NOT (GDD > 0 AND GDD < GDDFull)
        #
        not_growing = ~((gdd > 0) & (gdd < gdd_full))

        # LAItype 0 calls calculate_sdd_type0 when:
        #
        #   SDD < 0 AND SDD > SDDFull
        #
        senescence = (
            not_growing
            & (sdd < 0)
            & (sdd > sdd_full)
        )

        valid_senescence = (
            (lai_change < 0)
            | (lai <= lai_min)
        )

        assert senescence.any(), (
            "Northern Hemisphere LAItype 0 did not encounter "
            "the SDD senescence condition"
        )

        assert valid_senescence[senescence].all(), (
            "Northern Hemisphere LAItype 0 should decrease LAI "
            "when SDD < 0 and SDD > SDDFull"
        )

    def test_dailystate_lai_northern_lai_type_1_daylength_senescence(
        self, sample_data_loaded, sample_yaml_path
    ):
        """Northern Hemisphere LAItype 1 uses day length to start senescence."""

        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        # Check that this is a Northern Hemisphere simulation.
        assert sim._config.sites[0].properties.lat.value > 0

        # Configure LAItype 1 on the simulation itself.
        lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
        lai_config.lai_type.value = 1

        gdd_full = lai_config.gdd_full.value
        sdd_full = lai_config.sdd_full.value
        lai_min = lai_config.lai_min.value

        # Start from a fully grown canopy and simulate only the autumn
        # window (see the LAItype 0 test above): day length drops below
        # 12 h from the September equinox, which is what this branch needs.
        initial_states = sim._config.sites[0].initial_states.dectr
        initial_states.lai_id.value = lai_config.lai_max.value
        initial_states.gdd_id.value = gdd_full
        initial_states.sdd_id.value = 0.0

        _, df_forcing = sample_data_loaded
        df_forcing = df_forcing.copy()

        day_of_year = df_forcing.index.dayofyear
        df_forcing = df_forcing[(day_of_year >= 230) & (day_of_year <= 330)]

        sim.update_forcing(df_forcing)
        sim.run()

        df_output = sim.output
        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        # Replace this with the actual DailyState day-length column name
        # if it differs in the model output.
        daylength = df_dailystate["DLHrs"].iloc[1:]

        lai_change = lai.diff().iloc[1:]
        gdd = gdd.iloc[1:]
        sdd = sdd.iloc[1:]
        lai = lai.iloc[1:]

        # calculate_lai() first checks the GDD growth branch.
        not_growing = ~((gdd > 0) & (gdd < gdd_full))

        # For Northern Hemisphere LAItype 1 using SEN_DAYLENGTH,
        # check_start_senescence() requires:
        #
        #   lenDay <= 12
        #   AND
        #   SDD > SDDFull
        #
        senescence = (
            not_growing
            & (daylength <= 12)
            & (sdd > sdd_full)
        )

        valid_senescence = (
            (lai_change < 0)
            | (lai <= lai_min)
        )

        assert senescence.any(), (
            "Northern Hemisphere LAItype 1 did not encounter "
            "the day-length senescence condition"
        )

        assert valid_senescence[senescence].all(), (
            "Northern Hemisphere LAItype 1 should decrease LAI "
            "when day length <= 12 and SDD > SDDFull"
        )

    def test_dailystate_lai_southern_hemisphere_sdd_reset(
        self, sample_data_loaded, sample_yaml_path
    ):
        """Southern Hemisphere resets SDD during summer."""

        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        # Check that this is a Southern Hemisphere simulation.
        sim._config.sites[0].properties.lat.value = -(
            abs(sim._config.sites[0].properties.lat.value)
        )

        lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
        lai_config.lai_type.value = 1

        crit_days = 50
        summer_day = 250
        sdd_reset_day = 300

        gdd_full = lai_config.gdd_full.value
        sdd_full = lai_config.sdd_full.value

        initial_states = sim._config.sites[0].initial_states.dectr
        initial_states.gdd_id.value = gdd_full
        initial_states.sdd_id.value = 1.0

        _, df_forcing = sample_data_loaded

        # Shift the forcing by six months so the existing seasonal cycle
        # occurs in the Southern Hemisphere calendar.
        df_forcing = df_forcing.copy()
        df_forcing.index = df_forcing.index + pd.Timedelta(days=182)

        # Only simulate Southern Hemisphere summer, before the generic
        # transition-day reset at sdd_reset_day.
        day_of_year = df_forcing.index.dayofyear
        df_forcing = df_forcing[
            (day_of_year >= summer_day)
            & (day_of_year < sdd_reset_day)
        ]

        sim.update_forcing(df_forcing)
        sim.run()

        df_dailystate = (
            sim.output.loc[:, "DailyState"]
            .dropna(how="all")
        )

        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        previous_sdd = sdd.shift(1)

        # Ignore the first timestep because the degree-day states are
        # evaluated using the previous timestep.
        gdd = gdd.iloc[1:]
        sdd = sdd.iloc[1:]
        previous_sdd = previous_sdd.iloc[1:]

        day_of_year = (
            df_dailystate.index.get_level_values("datetime").dayofyear[1:]
        )

        sdd_reset = (
            (day_of_year >= summer_day)
            & (gdd > crit_days)
            & (previous_sdd.notna())
            & (previous_sdd != 0)
            & (sdd == 0)
        )

        assert sdd_reset.any(), (
            "Southern Hemisphere did not reset SDD when "
            "GDD > critDays and day of year > summer_day"
        )

    def test_dailystate_lai_southern_hemisphere_gdd_reset(
        self, sample_data_loaded, sample_yaml_path
    ):
        """Southern Hemisphere resets GDD during winter."""

        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        # Check that this is a Southern Hemisphere simulation.
        sim._config.sites[0].properties.lat.value = -(
            abs(sim._config.sites[0].properties.lat.value)
        )

        lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
        lai_config.lai_type.value = 1

        crit_days = 50
        winter_day = 250

        base_t_gdd = lai_config.base_temperature.value

        gdd_full = lai_config.gdd_full.value
        sdd_full = lai_config.sdd_full.value

        initial_states = sim._config.sites[0].initial_states.dectr
        initial_states.gdd_id.value = 0
        initial_states.sdd_id.value = sdd_full

        _, df_forcing = sample_data_loaded

        # Shift the forcing by six months so the existing seasonal cycle
        # occurs in the Southern Hemisphere calendar.
        df_forcing = df_forcing.copy()
        df_forcing.index = df_forcing.index + pd.Timedelta(days=182)

        # The shifted forcing starts at day 183, and every qualifying reset
        # falls before winter_day (250), so the first ~67 days of the run
        # are all this test needs. Slice by elapsed time: a day-of-year
        # filter would also catch a non-contiguous wrapped segment from the
        # end of the shifted year.
        df_forcing = df_forcing[
            df_forcing.index < df_forcing.index[0] + pd.Timedelta(days=67)
        ]

        sim.update_forcing(df_forcing)
        sim.run()

        df_dailystate = (
            sim.output.loc[:, "DailyState"]
            .dropna(how="all")
        )

        day_of_year = (
            df_dailystate.index.get_level_values("datetime").dayofyear
        )

        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        # Ensure cold spring condition is not met (delta_gdd >= 0)
        delta_gdd = (
            (df_dailystate["Tmin"] + df_dailystate["Tmax"]) / 2
            - base_t_gdd
        )

        # State used by the model before applying today's delta.
        previous_gdd = gdd.shift(1)

        gdd_before_limit = previous_gdd + delta_gdd.clip(lower=0)

        # Ignore the first timestep, where no previous state exists.
        gdd = gdd.iloc[1:]
        sdd = sdd.iloc[1:]
        previous_gdd = previous_gdd.iloc[1:]
        delta_gdd = delta_gdd.iloc[1:]
        gdd_before_limit = gdd_before_limit.iloc[1:]
        day_of_year = day_of_year[1:]

        gdd_reset = (
            (day_of_year < winter_day)
            & (sdd < -crit_days)
            & (sdd <= sdd_full)
            # Exclude the cold-spring reset.
            & (delta_gdd >= 0)
            # Exclude the GDDFull limit reset.
            & (gdd_before_limit < gdd_full)
            # There must have been GDD available to reset.
            & (gdd_before_limit > 0)
            & (gdd == 0)
        )

        assert gdd_reset.any(), (
            "Southern Hemisphere did not reset GDD when "
            "SDD < -critDays, day of year < winter_day."
        )

    def test_dailystate_lai_observed_invalid_fortran_guard(
        self, sample_data_loaded, sample_yaml_path, monkeypatch
    ):
        """Invalid observed LAI reaching Fortran raises error 105."""

        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        sim._config.model.physics.laimethod.value = 0
        sim._df_state_init = sim._config.to_df_state()

        _, df_forcing = sample_data_loaded
        df_forcing = df_forcing.copy()

        first_day = df_forcing.index.normalize() == df_forcing.index[0].normalize()
        df_forcing.loc[first_day, "lai"] = -1.0

        sim.update_forcing(df_forcing)

        monkeypatch.setattr(
            sp._check,
            "_check_observed_lai_nonneg",
            lambda *a, **k: False,
        )

        with pytest.raises(RuntimeError, match="code 105"):
            sim.run(_validate_forcing=False)

    
    def test_dailystate_gdd_stagnates_during_cold_spring(
        self, sample_config_loaded, sample_run_cached,
    ):
        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai

        base_t_gdd = config_lai_dectr.base_temperature.value
        gdd_full = config_lai_dectr.gdd_full.value
        sdd_full = config_lai_dectr.sdd_full.value

        df_output, df_state_final = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]
        previous_gdd = gdd.shift(1)
        delta_gdd = (
            (df_dailystate["Tmin"] + df_dailystate["Tmax"]) / 2
            - base_t_gdd
        )

        # Ignore initial timestep, previous state required for test
        gdd = gdd.iloc[1:]
        sdd = sdd.iloc[1:]
        previous_gdd = previous_gdd.iloc[1:]
        delta_gdd_calculated = delta_gdd.iloc[1:]
        delta_gdd_model = gdd - previous_gdd

        cold_spring_condition = (
            (sdd <= sdd_full)
            & (delta_gdd_calculated < 0)
            & (previous_gdd != 0) # Must check a transition, not retaining zero state
        )

        cold_spring_transition = delta_gdd_model.loc[cold_spring_condition]

        assert((cold_spring_transition == 0).all())