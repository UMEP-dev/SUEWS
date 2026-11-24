"""Test DailyState output functionality."""

import tempfile

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pytest

import supy as sp

from supy._supy_module import _save_supy
from supy.data_model.core.model import OutputControl

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract(
        "C-CLOCK", "C-RESPONSE", "C-OUTPUT", "C-SAVE", "C-RESAMPLE", "C-NATIVE-ERROR"
    ),
]


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


    def test_dailystate_lai_responds_to_gdd_sdd(self, sample_dailystate_full_year, sample_config_loaded):
        """Test that LAI grows when accumulated GDD exceeds gdd_base (the minimum gdd required to initiate leaf growth)
        and declines when accumulated sdd surpasses sdd_base """

        # extract lai config parameters
        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        gdd_base = config_lai_dectr.gdd_base.value
        sdd_base = config_lai_dectr.sdd_base.value

        # The full available forcing period, DailyState only.
        df_dailystate = sample_dailystate_full_year

        # extract fields
        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        lai_change = lai.diff()

        if (gdd > gdd_base).any():
            assert (lai_change > 0).any(), "No LAI growth found despite accumulated gdd exceeding gdd_base"

        if (sdd < sdd_base).any():
            assert (lai_change < 0).any(), "No LAI decline found despite accumulated sdd surpassing sdd_base"


    def test_LAI_remains_bounded_and_physical(self, sample_dailystate_full_year, sample_config_loaded):
        """ Test that LAI does go out of presribed max and mininum bounds and that is a real numeric value above zero """

        # extract lai config parameters
        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        lai_max = config_lai_dectr.gdd_base.value
        lai_min = config_lai_dectr.sdd_base.value

        # The full available forcing period, DailyState only.
        df_dailystate = sample_dailystate_full_year

        # extract fields
        lai = df_dailystate["LAI_DecTr"]

        # LAI must remain physical
        assert (lai >= 0).all()

        # and within configured bounds
        assert (lai >= lai_min).all(), "LAI falls below configured minimum"
        assert (lai <= lai_max).all(), "LAI exceeds configured maximum "


    def test_LAI_only_grows_during_growth_season(self, sample_dailystate_full_year, sample_config_loaded):
        """ LAI should only grow in growth half of year (hemipshere dependant). Note omitting an equivalent test for sdd for now 
        because future PRs will introduce alternative means of LAI decline not dependant upon SDD """

        # extract latitude to determine GDD reset day
        latitude = sample_config_loaded.sites[0].properties.lat

        # reset days depend upon hemipshere. Could try and read from fortran file directly to prevent drift if this parameter changes?
        if latitude > 0:
            summer_reset_day = 196
            winter_reset_day = 15
        else:
            summer_reset_day = 15
            winter_reset_day = 196

        # get sample output
        df_dailystate = sample_dailystate_full_year

        # get LAI
        lai = df_dailystate["LAI_DecTr"]

        delta_lai = lai.diff()

        # definiton of growing season depends upon the hemipshere
        if latitude >= 0:
            assert (delta_lai[(df_dailystate.index.get_level_values('datetime').day_of_year > winter_reset_day) & (df_dailystate.index.get_level_values('datetime').day_of_year < summer_reset_day)] >= 0).all, "LAI decreases during growing season"
        else:
            assert (delta_lai[(df_dailystate.index.get_level_values('datetime').day_of_year > winter_reset_day) | (df_dailystate.index.get_level_values('datetime').day_of_year < summer_reset_day)] >= 0).all, "LAI decreases during growing season"


    def test_gdd_sdd_accumulates_in_correct_season_resetting_at_end(self, sample_dailystate_full_year, sample_config_loaded):
        """ tests that GDD and SDD are resets only at hemipshere dependant reset day, and that gdd and sdd accumulation occurs only in 
        the (hemisphere dependant) growth and senescence halves of the year
        """
        # extract latitude to determine GDD reset day
        latitude = sample_config_loaded.sites[0].properties.lat

        # NOTE: could try and read fromf ortran file directly to prevent dirft if this parameter changes?
        if latitude > 0:
            summer_reset_day = 196
            winter_reset_day = 15
        else:
            summer_reset_day = 15
            winter_reset_day = 196

        # get sample output
        df_dailystate = sample_dailystate_full_year
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]

        # get daily change in gdd and sdd
        delta_gdd = gdd.diff()
        delta_sdd = sdd.diff()

        # mask for points at reset day
        summer_reset_day_mask = (df_dailystate.index.get_level_values('datetime').day_of_year == summer_reset_day)
        winter_reset_day_mask = (df_dailystate.index.get_level_values('datetime').day_of_year == winter_reset_day)

        # check gdd and sdd are zero (and thus reset) on reset days
        assert (gdd[summer_reset_day_mask] == 0).all(), 'gdd is not reset to zero on summer reset day'
        assert (sdd[summer_reset_day_mask] == 0).all(), 'sdd is not zero on summer reset day'
        assert (gdd[winter_reset_day_mask] == 0).all(), 'gdd is not zero on winter reset day'
        assert (sdd[winter_reset_day_mask] == 0).all(), 'sdd is not reset to zero on winter reset day'

        # check change in gdd is non-negative (and sdd non-positive) for all other days
        # remove first timestep (because no delta to compute there)
        assert (delta_gdd[~summer_reset_day_mask].iloc[1:] >= 0).all(), 'accumulated gdd shrinks on days other than reset day'
        assert (delta_sdd[~winter_reset_day_mask].iloc[1:] <= 0).all(), 'accumulated sdd shrinks on days other than reset day'

        # check that accumulation of GDD does not occur in senescence season, and sdd does not accumulate in growth season
        # season definition (and thus indexing) depennds upon hemipshere
        if latitude >= 0:
            assert (delta_gdd[(df_dailystate.index.get_level_values('datetime').day_of_year < winter_reset_day) |
                              (df_dailystate.index.get_level_values('datetime').day_of_year > summer_reset_day)] == 0).all, \
                                "GDD accumulates during senescence season"
            assert (delta_sdd[(df_dailystate.index.get_level_values('datetime').day_of_year > winter_reset_day) &
                              (df_dailystate.index.get_level_values('datetime').day_of_year < summer_reset_day)] >= 0).all, \
                                "SDD accumulates during growing season"
        else:
            assert (delta_gdd[(df_dailystate.index.get_level_values('datetime').day_of_year < winter_reset_day) &
                              (df_dailystate.index.get_level_values('datetime').day_of_year > summer_reset_day)] == 0).all, \
                                "GDD accumulates during senescence season"
            assert (delta_sdd[(df_dailystate.index.get_level_values('datetime').day_of_year > winter_reset_day) |
                              (df_dailystate.index.get_level_values('datetime').day_of_year < summer_reset_day)] >= 0).all, \
                                "SDD accumulates during growing season"

    # def test_dailystate_gdd_sdd_threshold_resets(self, sample_dailystate_full_year):
    #     """GDD and SDD are reset when their seasonal thresholds are exceeded."""

    #     crit_days = 50

    #     df_dailystate = sample_dailystate_full_year

    #     gdd = df_dailystate["GDD_DecTr"].dropna()
    #     sdd = df_dailystate["SDD_DecTr"].dropna()

    #     doy = df_dailystate.loc[gdd.index].index.get_level_values("datetime").day_of_year

    #     # ---------------------------------------------------------------
    #     # SDD reset during summer.
    #     #
    #     # Once GDD exceeds crit_days, SDD is reset while DOY < 170.
    #     # ---------------------------------------------------------------
    #     summer_reset = (doy < 170) & (gdd > crit_days)

    #     assert summer_reset.any(), (
    #         "The available simulation should contain a summer SDD reset condition"
    #     )

    #     assert (sdd.loc[summer_reset] == 0).all(), (
    #         "SDD should be reset to 0 when GDD > crit_days "
    #         "during the summer period"
    #     )

    #     # ---------------------------------------------------------------
    #     # GDD reset during winter.
    #     #
    #     # Once SDD falls below -crit_days, GDD is reset while DOY > 170.
    #     # ---------------------------------------------------------------
    #     winter_reset = (doy > 170) & (sdd < -crit_days)

    #     assert winter_reset.any(), (
    #         "The available simulation should contain a winter GDD reset condition"
    #     )

    #     assert (gdd.loc[winter_reset] == 0).all(), (
    #         "GDD should be reset to 0 when SDD < -crit_days "
    #         "during the winter period"
    #     )



    # @pytest.mark.medium
    # def test_dailystate_lai_northern_lai_type_0_sdd_senescence(
    #     self, sample_data_loaded, sample_config_loaded, sample_yaml_path
    # ):
    #     """Northern Hemisphere LAItype 0 uses SDD to drive senescence."""

    #     sim = sp.SUEWSSimulation(str(sample_yaml_path))

    #     # Check that this is a Northern Hemisphere simulation.
    #     assert sim._config.sites[0].properties.lat.value > 0

    #     # Configure LAItype 0 on the simulation itself.
    #     lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
    #     lai_config.lai_type.value = 0

    #     gdd_full = lai_config.gdd_full.value
    #     sdd_full = lai_config.sdd_full.value
    #     lai_min = lai_config.lai_min.value

    #     # Start from a fully grown canopy so only the autumn window needs
    #     # simulating: growth is complete (GDD at full) and senescence has
    #     # not begun. Running spring/summer from a cold start adds ~10 s per
    #     # test on the slowest CI platform without touching the branch under
    #     # test.
    #     initial_states = sim._config.sites[0].initial_states.dectr
    #     initial_states.lai_id.value = lai_config.lai_max.value
    #     initial_states.gdd_id.value = gdd_full
    #     initial_states.sdd_id.value = 0.0

    #     _, df_forcing = sample_data_loaded
    #     df_forcing = df_forcing.copy()

    #     # Autumn window: SDD accumulates and the senescence branch fires.
    #     day_of_year = df_forcing.index.day_of_year
    #     df_forcing = df_forcing[(day_of_year >= 230) & (day_of_year <= 330)]

    #     sim.update_forcing(df_forcing)
    #     sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

    #     df_output = sim.output
    #     df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

    #     lai = df_dailystate["LAI_DecTr"]
    #     gdd = df_dailystate["GDD_DecTr"]
    #     sdd = df_dailystate["SDD_DecTr"]

    #     lai_change = lai.diff().iloc[1:]
    #     sdd = sdd.iloc[1:]
    #     gdd = gdd.iloc[1:]
    #     lai = lai.iloc[1:]

    #     # calculate_lai() first checks the GDD growth branch.
    #     #
    #     # Therefore LAItype 0 can only reach the SDD branch when:
    #     #
    #     #   NOT (GDD > 0 AND GDD < GDDFull)
    #     #
    #     not_growing = ~((gdd > 0) & (gdd < gdd_full))

    #     # LAItype 0 calls calculate_sdd_type0 when:
    #     #
    #     #   SDD < 0 AND SDD > SDDFull
    #     #
    #     senescence = (
    #         not_growing
    #         & (sdd < 0)
    #         & (sdd > sdd_full)
    #     )

    #     valid_senescence = (
    #         (lai_change < 0)
    #         | (lai <= lai_min)
    #     )

    #     assert senescence.any(), (
    #         "Northern Hemisphere LAItype 0 did not encounter "
    #         "the SDD senescence condition"
    #     )

    #     assert valid_senescence[senescence].all(), (
    #         "Northern Hemisphere LAItype 0 should decrease LAI "
    #         "when SDD < 0 and SDD > SDDFull"
    #     )

    # @pytest.mark.medium
    # def test_dailystate_lai_northern_lai_type_1_daylength_senescence(
    #     self, sample_data_loaded, sample_yaml_path
    # ):
    #     """Northern Hemisphere LAItype 1 uses day length to start senescence."""

    #     sim = sp.SUEWSSimulation(str(sample_yaml_path))

    #     # Check that this is a Northern Hemisphere simulation.
    #     assert sim._config.sites[0].properties.lat.value > 0

    #     # Configure LAItype 1 on the simulation itself.
    #     lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
    #     lai_config.lai_type.value = 1

    #     gdd_full = lai_config.gdd_full.value
    #     sdd_full = lai_config.sdd_full.value
    #     lai_min = lai_config.lai_min.value

    #     # Start from a fully grown canopy and simulate only the autumn
    #     # window (see the LAItype 0 test above): day length drops below
    #     # 12 h from the September equinox, which is what this branch needs.
    #     initial_states = sim._config.sites[0].initial_states.dectr
    #     initial_states.lai_id.value = lai_config.lai_max.value
    #     initial_states.gdd_id.value = gdd_full
    #     initial_states.sdd_id.value = 0.0

    #     _, df_forcing = sample_data_loaded
    #     df_forcing = df_forcing.copy()

    #     day_of_year = df_forcing.index.day_of_year
    #     df_forcing = df_forcing[(day_of_year >= 230) & (day_of_year <= 330)]

    #     sim.update_forcing(df_forcing)
    #     sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

    #     df_output = sim.output
    #     df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

    #     lai = df_dailystate["LAI_DecTr"]
    #     gdd = df_dailystate["GDD_DecTr"]
    #     sdd = df_dailystate["SDD_DecTr"]

    #     # Replace this with the actual DailyState day-length column name
    #     # if it differs in the model output.
    #     daylength = df_dailystate["DLHrs"].iloc[1:]

    #     lai_change = lai.diff().iloc[1:]
    #     gdd = gdd.iloc[1:]
    #     sdd = sdd.iloc[1:]
    #     lai = lai.iloc[1:]

    #     # calculate_lai() first checks the GDD growth branch.
    #     not_growing = ~((gdd > 0) & (gdd < gdd_full))

    #     # For Northern Hemisphere LAItype 1 using SEN_DAYLENGTH,
    #     # check_start_senescence() requires:
    #     #
    #     #   lenDay <= 12
    #     #   AND
    #     #   SDD > SDDFull
    #     #
    #     senescence = (
    #         not_growing
    #         & (daylength <= 12)
    #         & (sdd > sdd_full)
    #     )

    #     valid_senescence = (
    #         (lai_change < 0)
    #         | (lai <= lai_min)
    #     )

    #     assert senescence.any(), (
    #         "Northern Hemisphere LAItype 1 did not encounter "
    #         "the day-length senescence condition"
    #     )

    #     assert valid_senescence[senescence].all(), (
    #         "Northern Hemisphere LAItype 1 should decrease LAI "
    #         "when day length <= 12 and SDD > SDDFull"
    #     )

    # def test_dailystate_lai_southern_hemisphere_sdd_reset(
    #     self, sample_data_loaded, sample_yaml_path
    # ):
    #     """Southern Hemisphere resets SDD during summer."""

    #     sim = sp.SUEWSSimulation(str(sample_yaml_path))

    #     # Check that this is a Southern Hemisphere simulation.
    #     sim._config.sites[0].properties.lat.value = -(
    #         abs(sim._config.sites[0].properties.lat.value)
    #     )

    #     lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
    #     lai_config.lai_type.value = 1

    #     crit_days = 50
    #     summer_day = 250
    #     sdd_reset_day = 300

    #     gdd_full = lai_config.gdd_full.value
    #     sdd_full = lai_config.sdd_full.value

    #     initial_states = sim._config.sites[0].initial_states.dectr
    #     initial_states.gdd_id.value = gdd_full
    #     initial_states.sdd_id.value = 1.0

    #     _, df_forcing = sample_data_loaded

    #     # Shift the forcing by six months so the existing seasonal cycle
    #     # occurs in the Southern Hemisphere calendar.
    #     df_forcing = df_forcing.copy()
    #     df_forcing.index = df_forcing.index + pd.Timedelta(days=182)

    #     # Only simulate Southern Hemisphere summer, before the generic
    #     # transition-day reset at sdd_reset_day.
    #     day_of_year = df_forcing.index.day_of_year
    #     df_forcing = df_forcing[
    #         (day_of_year >= summer_day)
    #         & (day_of_year < sdd_reset_day)
    #     ]

    #     sim.update_forcing(df_forcing)
    #     sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

    #     df_dailystate = (
    #         sim.output.loc[:, "DailyState"]
    #         .dropna(how="all")
    #     )

    #     gdd = df_dailystate["GDD_DecTr"]
    #     sdd = df_dailystate["SDD_DecTr"]

    #     previous_sdd = sdd.shift(1)

    #     # Ignore the first timestep because the degree-day states are
    #     # evaluated using the previous timestep.
    #     gdd = gdd.iloc[1:]
    #     sdd = sdd.iloc[1:]
    #     previous_sdd = previous_sdd.iloc[1:]

    #     day_of_year = (
    #         df_dailystate.index.get_level_values("datetime").day_of_year[1:]
    #     )

    #     sdd_reset = (
    #         (day_of_year >= summer_day)
    #         & (gdd > crit_days)
    #         & (previous_sdd.notna())
    #         & (previous_sdd != 0)
    #         & (sdd == 0)
    #     )

    #     assert sdd_reset.any(), (
    #         "Southern Hemisphere did not reset SDD when "
    #         "GDD > critDays and day of year > summer_day"
    #     )

    # def test_dailystate_lai_southern_hemisphere_gdd_reset(
    #     self, sample_data_loaded, sample_yaml_path
    # ):
    #     """Southern Hemisphere resets GDD during winter."""

    #     sim = sp.SUEWSSimulation(str(sample_yaml_path))

    #     # Check that this is a Southern Hemisphere simulation.
    #     sim._config.sites[0].properties.lat.value = -(
    #         abs(sim._config.sites[0].properties.lat.value)
    #     )

    #     lai_config = sim._config.sites[0].properties.land_cover.dectr.lai
    #     lai_config.lai_type.value = 1

    #     crit_days = 50
    #     winter_day = 250

    #     base_t_gdd = lai_config.base_temperature.value

    #     gdd_full = lai_config.gdd_full.value
    #     sdd_full = lai_config.sdd_full.value

    #     initial_states = sim._config.sites[0].initial_states.dectr
    #     initial_states.gdd_id.value = 0
    #     initial_states.sdd_id.value = sdd_full

    #     _, df_forcing = sample_data_loaded

    #     # Shift the forcing by six months so the existing seasonal cycle
    #     # occurs in the Southern Hemisphere calendar.
    #     df_forcing = df_forcing.copy()
    #     df_forcing.index = df_forcing.index + pd.Timedelta(days=182)

    #     # The shifted forcing starts at day 183, and every qualifying reset
    #     # falls before winter_day (250), so the first ~67 days of the run
    #     # are all this test needs. Slice by elapsed time: a day-of-year
    #     # filter would also catch a non-contiguous wrapped segment from the
    #     # end of the shifted year.
    #     df_forcing = df_forcing[
    #         df_forcing.index < df_forcing.index[0] + pd.Timedelta(days=67)
    #     ]

    #     sim.update_forcing(df_forcing)
    #     sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

    #     df_dailystate = (
    #         sim.output.loc[:, "DailyState"]
    #         .dropna(how="all")
    #     )

    #     day_of_year = (
    #         df_dailystate.index.get_level_values("datetime").day_of_year
    #     )

    #     gdd = df_dailystate["GDD_DecTr"]
    #     sdd = df_dailystate["SDD_DecTr"]

    #     # Ensure cold spring condition is not met (delta_gdd >= 0)
    #     delta_gdd = (
    #         (df_dailystate["Tmin"] + df_dailystate["Tmax"]) / 2
    #         - base_t_gdd
    #     )

    #     # State used by the model before applying today's delta.
    #     previous_gdd = gdd.shift(1)

    #     gdd_before_limit = previous_gdd + delta_gdd.clip(lower=0)

    #     # Ignore the first timestep, where no previous state exists.
    #     gdd = gdd.iloc[1:]
    #     sdd = sdd.iloc[1:]
    #     previous_gdd = previous_gdd.iloc[1:]
    #     delta_gdd = delta_gdd.iloc[1:]
    #     gdd_before_limit = gdd_before_limit.iloc[1:]
    #     day_of_year = day_of_year[1:]

    #     gdd_reset = (
    #         (day_of_year < winter_day)
    #         & (sdd < -crit_days)
    #         & (sdd <= sdd_full)
    #         # Exclude the cold-spring reset.
    #         & (delta_gdd >= 0)
    #         # Exclude the GDDFull limit reset.
    #         & (gdd_before_limit < gdd_full)
    #         # There must have been GDD available to reset.
    #         & (gdd_before_limit > 0)
    #         & (gdd == 0)
    #     )

    #     assert gdd_reset.any(), (
    #         "Southern Hemisphere did not reset GDD when "
    #         "SDD < -critDays, day of year < winter_day."
    #     )

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
