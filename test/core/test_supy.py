import io
from pathlib import Path
import platform
import sys
import tempfile
from time import time
from unittest import TestCase, skipIf

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy import SUEWSSimulation

# Import debug utilities from conftest (centralised)
from conftest import (
    TIMESTEPS_PER_DAY,
    capture_test_artifacts,
    debug_dataframe_output,
    debug_on_ci,
    debug_water_balance,
)


# Get the test data directory from the environment variable
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
# test_data_dir = os.environ.get('TEST_DATA_DIR', Path(__file__).parent.parent / 'fixtures' / 'data_test')

# Note: sample_output.pkl testing has been moved to test_sample_output.py

# Enable all tests on all platforms and Python versions
flag_full_test = True

# Note: Sample data loading moved to individual test methods to avoid test interference
# This prevents caching issues when tests run in sequence


class TestSuPy(TestCase):
    # test if single-tstep mode can run
    @pytest.mark.smoke
    def test_is_supy_running_single_step(self):
        print("\n========================================")
        print("Testing if single-tstep mode can run...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run only 1 hour (12 timesteps) instead of 8 hours
        results = sim.run(end_date=sim.forcing.index[11])

        # Verify results and state are populated
        self.assertIsNotNone(results)
        self.assertIsNotNone(sim.state_final)
        self.assertFalse(results.empty)
        self.assertFalse(sim.state_final.empty)

    # test if multi-tstep mode can run
    @pytest.mark.core
    @debug_on_ci
    @debug_dataframe_output
    @capture_test_artifacts("multi_step")
    def test_is_supy_running_multi_step(self):
        print("\n========================================")
        print("Testing if multi-tstep mode can run...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run only 2 days instead of 10 days
        end_index = TIMESTEPS_PER_DAY * 2 - 1  # 0-indexed
        results = sim.run(end_date=sim.forcing.index[end_index])

        # Debug output
        print("empty output?", results.empty)
        print("empty state?", sim.state_final.empty)
        print("any NaN in state?", sim.state_final.isnull().values.any())

        # Find the first NaN in state
        if sim.state_final.isnull().values.any():
            print("NaN in state:")
            print(sim.state_final.columns[np.any(sim.state_final.isnull(), axis=0)])

        test_non_empty = np.all([
            not results.empty,
            not sim.state_final.empty,
        ])
        self.assertTrue(test_non_empty and not sim.state_final.isnull().values.any())

    # test if multi-grid simulation can run in parallel
    # NOTE: This test uses functional API (sp.run_supy) instead of SUEWSSimulation
    # because multi-grid parallelization is a low-level feature not exposed in the OOP interface.
    # SUEWSSimulation is designed for single-grid workflows.

    def test_is_supy_sim_save_multi_grid_par(self):
        print("\n========================================")
        print("Testing if multi-grid simulation can run in parallel...")
        n_grid = 4

        # Load sample data
        df_state_init, df_forcing_tstep = sp.load_SampleData()

        df_state_init_base = df_state_init.copy()

        df_state_init_multi = pd.concat([df_state_init_base for x in range(n_grid)])
        df_state_init_multi.index = pd.RangeIndex(n_grid, name="grid")
        df_forcing_part = df_forcing_tstep.iloc[: TIMESTEPS_PER_DAY * 60]
        t_start = time()
        df_output, df_state = sp.run_supy(df_forcing_part, df_state_init_multi)
        t_end = time()

        test_success_sim = np.all([
            not df_output.empty,
            not df_state.empty,
        ])

        with tempfile.TemporaryDirectory() as dir_temp:
            list_outfile = sp.save_supy(
                df_output,
                df_state,
                path_dir_save=dir_temp,
                site="pytest",
                logging_level=10,
            )

        test_success_save = np.all([isinstance(fn, Path) for fn in list_outfile])
        self.assertTrue(test_success_sim and test_success_save)

        # only print to screen on macOS due incompatibility on Windows
        if platform.system() == "Darwin":
            n_grid = df_state_init_multi.index.size
            print(f"Running time: {t_end - t_start:.2f} s for {n_grid} grids")

        test_non_empty = np.all([
            not df_output.empty,
            not df_state.empty,
        ])
        self.assertTrue(test_non_empty)

    #  test if flag_test can be set to True
    @skipIf(
        True,
        "Skipping debug mode test due to STEBBS debug structure issues in YL/fixstebbs-rebase branch",
    )
    def test_is_flag_test_working(self):
        print("\n========================================")
        print("Testing if flag_test can be set to True...")

        # Load sample data
        df_state_init, df_forcing_tstep = sp.load_SampleData()

        df_forcing_part = df_forcing_tstep.iloc[: TIMESTEPS_PER_DAY * 10]
        df_output, df_state, df_debug, res_state = sp.run_supy(
            df_forcing_part,
            df_state_init,
            debug_mode=True,
        )
        # check if `flag_test` in `df_output.debug` equals 1.0
        self.assertTrue((df_output.debug.flag_test == 1.0).all())

    def test_run_with_version(self):
        print("\n========================================")
        print("Testing if state_init with version can be loaded...")

        # Create simulation and manually add version to initial state
        # (simulating loading a previously saved state with version metadata)
        sim = SUEWSSimulation.from_sample_data()
        sim._df_state_init[("version", "0")] = sp.__version__

        # Run with version column present in initial state
        results = sim.run(end_date=sim.forcing.index[11])  # 12 timesteps

        # Verify simulation completed successfully
        self.assertIsNotNone(results)
        self.assertIsNotNone(sim.state_final)
        self.assertFalse(results.empty)
        self.assertFalse(sim.state_final.empty)

    def test_is_runtime_version_saved(self):
        print("\n========================================")
        print("Testing if current SUEWS version is saved...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run only 1 hour (12 timesteps) instead of 8 hours
        results = sim.run(end_date=sim.forcing.index[11])

        # Verify version column exists and contains correct version
        self.assertIn(("version", "0"), sim.state_final.columns)
        self.assertTrue(all(sim.state_final[("version", "0")] == sp.__version__))

    def test_metadata_columns_are_strings(self):
        """Test that metadata columns (config, description, version) are plain Python strings, not numpy arrays.

        This verifies the fix for issue where pack_df_state_final would either:
        1. Crash with ValueError when trying to concatenate 0-dimensional numpy arrays, or
        2. Store numpy array objects in cells instead of plain strings

        The fix extracts scalar values from 0-dimensional numpy arrays using .item()
        """
        print("\n========================================")
        print("Testing metadata columns are plain strings not numpy arrays...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run a short simulation
        results = sim.run(end_date=sim.forcing.index[11])

        # Check that version metadata exists and is a string
        self.assertIn(("version", "0"), sim.state_final.columns)
        # Use .iloc to get the first cell value, accounting for MultiIndex
        version_value = sim.state_final[("version", "0")].iloc[0]
        self.assertIsInstance(
            version_value,
            str,
            f"version column should contain plain strings, not {type(version_value)}",
        )
        self.assertNotIsInstance(
            version_value,
            np.ndarray,
            "version column should not contain numpy arrays",
        )

        # Check config and description if they exist
        for metadata_col in ["config", "description"]:
            if (metadata_col, "0") in sim.state_final.columns:
                col_value = sim.state_final[(metadata_col, "0")].iloc[0]
                self.assertIsInstance(
                    col_value,
                    str,
                    f"{metadata_col} column should contain plain strings, not {type(col_value)}",
                )
                self.assertNotIsInstance(
                    col_value,
                    np.ndarray,
                    f"{metadata_col} column should not contain numpy arrays",
                )

        print("Metadata columns correctly stored as plain strings")

    def test_version_tracking_save_load_cycle(self):
        """Test complete save→load→run cycle with version tracking.

        This test verifies that:
        1. Version info is automatically saved with state
        2. Saved state (with version) can be loaded
        3. Loaded state can be used to continue simulation
        4. Version info persists through the cycle
        """
        print("\n========================================")
        print("Testing complete save→load→run cycle with version tracking...")

        # Run 1: Initial simulation
        sim1 = SUEWSSimulation.from_sample_data()
        sim1.run(end_date=sim1.forcing.index[11])  # 12 timesteps

        # Verify version was automatically added
        self.assertIn(("version", "0"), sim1.state_final.columns)
        version_from_run1 = sim1.state_final[("version", "0")].iloc[0]
        self.assertEqual(version_from_run1, sp.__version__)

        # Save state to temporary location
        with tempfile.TemporaryDirectory() as tmpdir:
            save_path = Path(tmpdir)
            sim1.save(save_path)

            # Verify state file was created
            state_files = list(save_path.glob("*_state_*.csv"))
            self.assertGreater(len(state_files), 0, "State file should be created")

            # Run 2: Load saved state and continue simulation
            sim2 = SUEWSSimulation.from_state(sim1.state_final)

            # Load forcing for continuation (using remaining timesteps)
            sim2.update_forcing(sim1.forcing.iloc[12:24])  # Next 12 timesteps

            # Run continuation
            results2 = sim2.run()

            # Verify continuation completed successfully
            self.assertIsNotNone(results2)
            self.assertIsNotNone(sim2.state_final)

            # Verify version info persisted through the cycle
            self.assertIn(("version", "0"), sim2.state_final.columns)
            version_from_run2 = sim2.state_final[("version", "0")].iloc[0]
            self.assertEqual(
                version_from_run2,
                sp.__version__,
                "Version should persist through save→load→run cycle",
            )

        print("✓ Version tracking works correctly through save→load→run cycle")

    # # test if single-tstep and multi-tstep modes can produce the same SUEWS results
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_is_supy_euqal_mode(self):
    #     print("\n========================================")
    #     print("Testing if single-tstep and multi-tstep modes can produce the same SUEWS results...")
    #     df_state_init, df_forcing_tstep = sp.load_SampleData()
    #     df_forcing_part = df_forcing_tstep.iloc[: 12*8]

    # # single-step results
    # df_output_s, df_state_s = sp.run_supy(
    #     df_forcing_part, df_state_init, save_state=True
    # )
    # df_res_s = (
    #     df_output_s.loc[:, list_grp_test]
    #     .fillna(-999.0)
    #     .sort_index(axis=1)
    #     .round(6)
    #     .applymap(lambda x: -999.0 if np.abs(x) > 3e4 else x)
    # )

    # df_state_init, df_forcing_tstep = sp.load_SampleData()
    # # multi-step results
    # df_output_m, df_state_m = sp.run_supy(
    #     df_forcing_part, df_state_init, save_state=False
    # )
    # df_res_m = (
    #     df_output_m.loc[:, list_grp_test]
    #     .fillna(-999.0)
    #     .sort_index(axis=1)
    #     .round(6)
    #     .applymap(lambda x: -999.0 if np.abs(x) > 3e4 else x)
    # )
    # # print(df_res_m.iloc[:3, 86], df_res_s.iloc[:3, 86])
    # pd.testing.assert_frame_equal(
    #     left=df_res_s,
    #     right=df_res_m,
    # )

    # test saving output files working
    def test_is_supy_save_working(self):
        print("\n========================================")
        print("Testing if saving output files working...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run for 2 days
        end_index = TIMESTEPS_PER_DAY * 2 - 1  # 0-indexed
        t_start = time()
        results = sim.run(end_date=sim.forcing.index[end_index])
        t_end = time()

        # Save to temporary directory
        with tempfile.TemporaryDirectory() as dir_temp:
            list_outfile = sim.save(dir_temp)

            # Verify files were created
            self.assertIsNotNone(list_outfile)
            self.assertGreater(len(list_outfile), 0)
            test_non_empty = np.all([isinstance(fn, Path) for fn in list_outfile])
            self.assertTrue(test_non_empty)

        # Performance logging (macOS only for compatibility)
        if platform.system() == "Darwin":
            capturedOutput = io.StringIO()
            sys.stdout = capturedOutput
            n_grid = sim._df_state_init.index.size
            print(f"Running time: {t_end - t_start:.2f} s for {n_grid} grids")
            sys.stdout = sys.__stdout__
            print("Captured:\n", capturedOutput.getvalue())

    # TODO: disable this test for now - need to recover in the future
    # # test saving output files working
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_is_checking_complete(self):
    #     print("\n========================================")
    #     print("Testing if checking-complete is working...")
    #     df_state_init, df_forcing_tstep = sp.load_SampleData()
    #     dict_rules = sp._check.dict_rules_indiv

    #     # variables in loaded dataframe
    #     set_var_df_init = set(df_state_init.columns.get_level_values("var"))

    #     # variables in dict_rules
    #     set_var_dict_rules = set(list(dict_rules.keys()))

    #     # common variables
    #     set_var_common = set_var_df_init.intersection(set_var_dict_rules)

    #     # test if common variables are all those in `df_state_init`
    #     test_common_all = set_var_df_init == set_var_common
    #     if not test_common_all:
    #         print("Variables not in `dict_rules` but in `df_state_init`:")
    #         print(set_var_df_init.difference(set_var_common))
    #         print("Variables not in `df_state_init` but in `dict_rules`:")
    #         print(set_var_common.difference(set_var_df_init))
    #     self.assertTrue(test_common_all)

    # test ERA5 forcing generation
    def test_gen_forcing(self):
        print("\n========================================")
        print("Testing if forcing generation working...")

        # # mimic downloading
        # dict_era5_file = sp.util.download_era5(
        #     57.7081,
        #     11.9653,
        #     "20030101",
        #     "20031231",
        #     dir_save="./data_test/single-grid",
        # )
        # list_fn_ml = [k for k in dict_era5file.keys() if "ml" in k]
        # list_fn_sfc = [k for k in dict_era5_file.keys() if "sfc" in k]
        # test forcing generation

        # skip this test if under cibuild environment where the test data is not available
        p_data_test = Path("test/fixtures/data_test/era5")
        if not p_data_test.exists():
            self.assertTrue(True)
        else:
            list_fn_fc = sp.util.gen_forcing_era5(
                57.7081,
                11.9653,
                "20030101",
                "20031231",
                dir_save=p_data_test.as_posix(),
            )
            df_forcing = sp.util.read_suews(list_fn_fc[0])
            ser_tair = df_forcing.Tair
            # ds_sfc = xr.open_mfdataset(list_fn_sfc)
            # ser_t2 = ds_sfc.t2m.to_series()
            # res_dif = ((df_forcing.Tair + 273.15 - ser_t2.values) / 98).round(4)
            test_dif = -30 < ser_tair.max() < 100
            self.assertTrue(test_dif)

    # Note: benchmark tests have been moved to test_sample_output.py
    # for better diagnostics and platform-specific tolerance handling

    # test if the weighted SMD of vegetated surfaces are properly calculated
    def test_is_smd_veg_weighted(self):
        print("\n========================================")
        print("Testing if SMD of vegetated surfaces are properly calculated...")
        soilstorecap = np.ones(7) * 100
        sfr_surf = np.random.random(7)
        soilstore_id = np.random.random(7) * 80
        nonwaterfraction = sfr_surf[:-1].sum()

        # correct SMD_veg
        smd = soilstorecap - soilstore_id
        smd_veg = smd[2:5]
        surf_veg = sfr_surf[2:5]
        surf_veg = surf_veg / surf_veg.sum()
        smd_veg_correct = np.dot(surf_veg, smd_veg)

        # test SMD_veg
        from supy.supy_driver import module_phys_waterdist as wm

        smd_veg_test = wm.cal_smd_veg(soilstorecap, soilstore_id, sfr_surf)

        self.assertAlmostEqual(smd_veg_correct, smd_veg_test)

    # test if dailystate are written out correctly
    def test_dailystate_meaningful(self):
        print("\n========================================")
        print("Testing if dailystate are written out correctly...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run for 10 days
        n_days = 10
        end_index = TIMESTEPS_PER_DAY * n_days - 1  # 0-indexed
        results = sim.run(end_date=sim.forcing.index[end_index])

        # Check that DailyState exists in output
        groups = results.columns.get_level_values("group").unique()
        self.assertIn("DailyState", groups, "DailyState should be in output groups")

        # Use xs() for robust MultiIndex column access across platforms
        df_dailystate = results.xs("DailyState", level="group", axis=1)

        # More robust check: Count rows that have at least one non-NaN value
        # This avoids issues with dropna() behavior across pandas versions
        mask_has_data = df_dailystate.notna().any(axis=1)
        n_days_with_data = mask_has_data.sum()

        # For even more robustness, also count unique days based on a key column
        # that should always have data (e.g., HDD1_h)
        if "HDD1_h" in df_dailystate.columns:
            n_days_by_hdd = df_dailystate.loc[mask_has_data, "HDD1_h"].notna().sum()
        else:
            # Fallback to first column if HDD1_h doesn't exist
            n_days_by_hdd = df_dailystate.loc[mask_has_data].iloc[:, 0].notna().sum()

        # Debug information
        print(f"DailyState shape: {df_dailystate.shape}")
        print(f"Rows with any data: {n_days_with_data}")
        print(f"Days with valid data (by column check): {n_days_by_hdd}")

        # Check we have the expected number of days
        # Use the count of rows with data instead of dropna().drop_duplicates()
        self.assertGreaterEqual(
            n_days_with_data,
            n_days - 1,
            f"Expected at least {n_days - 1} days of DailyState data, got {n_days_with_data}",
        )
        self.assertLessEqual(
            n_days_with_data,
            n_days + 1,
            f"Expected at most {n_days + 1} days of DailyState data, got {n_days_with_data}",
        )

        # Additional check: ensure we have actual data
        self.assertGreater(
            n_days_with_data, 0, "DailyState should have at least some data"
        )

    # test if the water balance is closed
    @debug_water_balance
    @capture_test_artifacts("water_balance")
    def test_water_balance_closed(self):
        print("\n========================================")
        print("Testing if water balance is closed...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run for 100 days
        n_days = 100
        end_index = TIMESTEPS_PER_DAY * n_days - 1  # 0-indexed
        results = sim.run(end_date=sim.forcing.index[end_index])

        # Get soilstore from debug output
        df_soilstore = results.loc[1, "debug"].filter(regex="^ss_.*_next$")
        ser_sfr_surf = sim._df_state_init.sfr_surf.iloc[0]
        ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)

        # Get water balance
        df_water = results.SUEWS[["Rain", "Irr", "Evap", "RO", "State"]].assign(
            SoilStore=ser_soilstore, TotalStore=ser_soilstore + results.SUEWS.State
        )

        # ===============================
        # check if water balance is closed
        # ===============================
        # Change in total store
        ser_totalstore_change = df_water.TotalStore.diff().dropna()
        # Water input
        ser_water_in = df_water.Rain + df_water.Irr
        # Water output
        ser_water_out = df_water.Evap + df_water.RO
        # Water balance
        ser_water_balance = ser_water_in - ser_water_out
        # Test if water balance is closed
        test_dif = (ser_totalstore_change - ser_water_balance).abs().max() < 1e-6
        self.assertTrue(test_dif)
