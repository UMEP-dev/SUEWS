"""Test DailyState output functionality."""

import tempfile

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pytest

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


    def test_dailystate_gdd_sdd_reset_conditions(
        self, sample_run_cached, sample_data_loaded, sample_config_loaded
    ):
        """GDD and SDD are reset under their seasonal reset conditions."""

        crit_days = 50

        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        gdd_full_dectr = config_lai_dectr.gdd_full.value
        sdd_full_dectr = config_lai_dectr.sdd_full.value

        df_output, _ = sample_run_cached()

        df_dailystate = df_output.loc[:, "DailyState"].dropna(how="all")

        gdd = df_dailystate["GDD_DecTr"].dropna()
        sdd = df_dailystate["SDD_DecTr"].dropna()

        doy = df_dailystate.loc[gdd.index].index.get_level_values("datetime").dayofyear

        # ---------------------------------------------------------------
        # SDD reset on the transition day.
        #
        # Northern hemisphere reset day is hard-coded as DOY 140.
        # ---------------------------------------------------------------
        transition_day = doy == 140

        if transition_day.any():
            sdd_transition = sdd.loc[transition_day]

            assert (sdd_transition == 0).all(), (
                "SDD should be reset to 0 on the SDD transition day"
            )

        # ---------------------------------------------------------------
        # SDD reset during summer.
        #
        # Once GDD exceeds crit_days, SDD is reset while DOY < 170.
        # ---------------------------------------------------------------
        summer_reset = (doy < 170) & (gdd > crit_days)

        if summer_reset.any():
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

        if winter_reset.any():
            assert (gdd.loc[winter_reset] == 0).all(), (
                "GDD should be reset to 0 when SDD < -crit_days "
                "during the winter period"
            )

        # At least one of the reset conditions should occur in the
        # available simulation; otherwise the test could pass without
        # exercising any reset behaviour.
        assert (
            transition_day.any()
            or summer_reset.any()
            or winter_reset.any()
        ), "No GDD/SDD reset condition occurred in the available simulation"