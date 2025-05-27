import tempfile
from pathlib import Path
import io
import sys
import warnings
from time import time
from unittest import TestCase, skipIf, skipUnless

import numpy as np
import pandas as pd

import supy as sp
import platform

from pathlib import Path

# Get the test data directory from the environment variable
test_data_dir = Path(__file__).parent / "data_test"
# test_data_dir = os.environ.get('TEST_DATA_DIR', Path(__file__).parent / 'data_test')

# Construct the file path for the data file
p_df_sample = Path(test_data_dir) / "sample_output.pkl"

# if platform is macOS and python version is 3.12, set flag_full_test to True
flag_full_test = any(
    [
        all(
            [
                sys.version_info[0] == 3,
                sys.version_info[1] == 12,
                platform.system() == "Darwin",
                platform.machine() == "arm64",
            ]
        ),
        all(
            [
                sys.version_info[0] == 3,
                sys.version_info[1] == 13,
                platform.system() == "Linux",
                platform.machine() == "x86_64",
            ]
        ),
    ]
)

# Load sample data once, as it will be used frequently later to save time.
df_state_init, df_forcing_tstep = sp.load_SampleData()

class TestSuPy(TestCase):
    def setUp(self):
        warnings.simplefilter("ignore", category=ImportWarning)

    # test if supy_driver can be connected
    def test_is_driver_connected(self):
        print("\n========================================")
        print("Testing if supy_driver can be connected...")
        sd = sp.supy_driver.Suews_Driver()
        self.assertTrue(sd is not None)
        # self.assertTrue(isinstance(s[0], np.str_))

    # test if single-tstep mode can run
    # def test_is_supy_running_single_step(self):
    #     print("\n========================================")
    #     print("Testing if single-tstep mode can run...")

    #     df_forcing_part = df_forcing_tstep.iloc[: 12 * 8]
    #     df_output, df_state = sp.run_supy(
    #         df_forcing_part, df_state_init, save_state=True
    #     )
        
    #     # test_non_empty = np.all(
    #     #     [
    #     #         not df_output.empty,
    #     #         not df_state.empty,
    #     #     ]
    #     # )
    #     # self.assertTrue((test_non_empty and not df_state.isnull().values.any()))
    #     self.assertFalse(df_output.empty)
    #     self.assertFalse(df_state.empty)
    #     # self.assertFalse(df_state.isnull().values.any())

    # test if multi-tstep mode can run
    def test_is_supy_running_multi_step(self):
        print("\n========================================")
        print("Testing if multi-tstep mode can run...")

        df_forcing_part = df_forcing_tstep.iloc[: 288 * 10]
        df_output, df_state = sp.run_supy(
            df_forcing_part, df_state_init, check_input=True
        )

        # # only print to screen on macOS due incompatibility on Windows
        # if platform.system() == "Darwin":
        #     # capturedOutput = io.StringIO()  # Create StringIO object
        #     # sys.stdout = capturedOutput  # and redirect stdout.
        #     # Call function.
        #     print(f"Running time: {t_end-t_start:.2f} s")
        #     # sys.stdout = sys.__stdout__  # Reset redirect.
        #     # Now works as before.
        #     # print("Captured:\n", capturedOutput.getvalue())
        print(f"empty output?", df_output.empty)
        print(f"empty state?", df_state.empty)
        print(f"any NaN in state?", df_state.isnull().values.any())

        test_non_empty = np.all(
            [
                not df_output.empty,
                not df_state.empty,
            ]
        )
        self.assertTrue((test_non_empty and not df_state.isnull().values.any()))

    # test if multi-grid simulation can run in parallel
    # def test_is_supy_sim_save_multi_grid_par(self):
    #     print("\n========================================")
    #     print("Testing if multi-grid simulation can run in parallel...")
    #     n_grid = 4

    #     df_state_init_base = df_state_init.copy()
    #     df_state_init_multi = pd.concat([df_state_init_base for x in range(n_grid)])
    #     df_state_init_multi.index = pd.RangeIndex(n_grid, name="grid")
    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 60]
    #     t_start = time()
    #     df_output, df_state = sp.run_supy(df_forcing_part, df_state_init_multi)
    #     t_end = time()

    #     test_success_sim = np.all(
    #         [
    #             not df_output.empty,
    #             not df_state.empty,
    #         ]
    #     )

    #     with tempfile.TemporaryDirectory() as dir_temp:
    #         list_outfile = sp.save_supy(
    #             df_output,
    #             df_state,
    #             path_dir_save=dir_temp,
    #             site="pytest",
    #             logging_level=10,
    #         )

    #     test_success_save = np.all([isinstance(fn, Path) for fn in list_outfile])
    #     self.assertTrue(test_success_sim and test_success_save)

    #     # only print to screen on macOS due incompatibility on Windows
    #     if platform.system() == "Darwin":
    #         n_grid = df_state_init_multi.index.size
    #         print(f"Running time: {t_end-t_start:.2f} s for {n_grid} grids")

    #     test_non_empty = np.all(
    #         [
    #             not df_output.empty,
    #             not df_state.empty,
    #         ]
    #     )
    #     self.assertTrue(test_non_empty)

    # #  test if flag_test can be set to True
    # def test_is_flag_test_working(self):
    #     print("\n========================================")
    #     print("Testing if flag_test can be set to True...")
    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 10]
    #     df_output, df_state, df_debug, res_state = sp.run_supy(
    #         df_forcing_part,
    #         df_state_init,
    #         debug_mode=True,
    #     )
    #     # check if `flag_test` in `df_output.debug` equals 1.0
    #     self.assertTrue((df_output.debug.flag_test == 1.0).all())

    # # # test if single-tstep and multi-tstep modes can produce the same SUEWS results
    # # @skipUnless(flag_full_test, "Full test is not required.")
    # # def test_is_supy_euqal_mode(self):
    # #     print("\n========================================")
    # #     print("Testing if single-tstep and multi-tstep modes can produce the same SUEWS results...")
    # #     df_state_init, df_forcing_tstep = sp.load_SampleData()
    # #     df_forcing_part = df_forcing_tstep.iloc[: 12*8]

    # # # single-step results
    # # df_output_s, df_state_s = sp.run_supy(
    # #     df_forcing_part, df_state_init, save_state=True
    # # )
    # # df_res_s = (
    # #     df_output_s.loc[:, list_grp_test]
    # #     .fillna(-999.0)
    # #     .sort_index(axis=1)
    # #     .round(6)
    # #     .applymap(lambda x: -999.0 if np.abs(x) > 3e4 else x)
    # # )

    # # df_state_init, df_forcing_tstep = sp.load_SampleData()
    # # # multi-step results
    # # df_output_m, df_state_m = sp.run_supy(
    # #     df_forcing_part, df_state_init, save_state=False
    # # )
    # # df_res_m = (
    # #     df_output_m.loc[:, list_grp_test]
    # #     .fillna(-999.0)
    # #     .sort_index(axis=1)
    # #     .round(6)
    # #     .applymap(lambda x: -999.0 if np.abs(x) > 3e4 else x)
    # # )
    # # # print(df_res_m.iloc[:3, 86], df_res_s.iloc[:3, 86])
    # # pd.testing.assert_frame_equal(
    # #     left=df_res_s,
    # #     right=df_res_m,
    # # )

    # # test saving output files working
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_is_supy_save_working(self):
    #     print("\n========================================")
    #     print("Testing if saving output files working...")
    #     df_state_init_sample, df_forcing_tstep = sp.load_SampleData()
    #     # df_state_init = pd.concat([df_state_init for x in range(6)])
    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 2]
    #     t_start = time()
    #     df_output, df_state = sp.run_supy(df_forcing_part, df_state_init_sample)
    #     t_end = time()
    #     with tempfile.TemporaryDirectory() as dir_temp:
    #         list_outfile = sp.save_supy(df_output, df_state, path_dir_save=dir_temp)

    #     # only print to screen on macOS due incompatibility on Windows
    #     if platform.system() == "Darwin":
    #         capturedOutput = io.StringIO()  # Create StringIO object
    #         sys.stdout = capturedOutput  # and redirect stdout.
    #         # Call function.
    #         n_grid = df_state_init_sample.index.size
    #         print(f"Running time: {t_end-t_start:.2f} s for {n_grid} grids")
    #         sys.stdout = sys.__stdout__  # Reset redirect.
    #         # Now works as before.
    #         print("Captured:\n", capturedOutput.getvalue())

    #     test_non_empty = np.all([isinstance(fn, Path) for fn in list_outfile])
    #     self.assertTrue(test_non_empty)

    # # TODO: disable this test for now - need to recover in the future
    # # # test saving output files working
    # # @skipUnless(flag_full_test, "Full test is not required.")
    # # def test_is_checking_complete(self):
    # #     print("\n========================================")
    # #     print("Testing if checking-complete is working...")
    # #     df_state_init, df_forcing_tstep = sp.load_SampleData()
    # #     dict_rules = sp._check.dict_rules_indiv

    # #     # variables in loaded dataframe
    # #     set_var_df_init = set(df_state_init.columns.get_level_values("var"))

    # #     # variables in dict_rules
    # #     set_var_dict_rules = set(list(dict_rules.keys()))

    # #     # common variables
    # #     set_var_common = set_var_df_init.intersection(set_var_dict_rules)

    # #     # test if common variables are all those in `df_state_init`
    # #     test_common_all = set_var_df_init == set_var_common
    # #     if not test_common_all:
    # #         print("Variables not in `dict_rules` but in `df_state_init`:")
    # #         print(set_var_df_init.difference(set_var_common))
    # #         print("Variables not in `df_state_init` but in `dict_rules`:")
    # #         print(set_var_common.difference(set_var_df_init))
    # #     self.assertTrue(test_common_all)

    # # test ERA5 forcing generation
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_gen_forcing(self):
    #     print("\n========================================")
    #     print("Testing if forcing generation working...")
    #     import xarray as xr

    #     # # mimic downloading
    #     # dict_era5_file = sp.util.download_era5(
    #     #     57.7081,
    #     #     11.9653,
    #     #     "20030101",
    #     #     "20031231",
    #     #     dir_save="./data_test/single-grid",
    #     # )
    #     # list_fn_ml = [k for k in dict_era5file.keys() if "ml" in k]
    #     # list_fn_sfc = [k for k in dict_era5_file.keys() if "sfc" in k]
    #     # test forcing generation

    #     # skip this test if under cibuild environment where the test data is not available
    #     p_data_test = Path("./supy/test/data_test/multi-grid")
    #     if not p_data_test.exists():
    #         self.assertTrue(True)
    #     else:
    #         list_fn_fc = sp.util.gen_forcing_era5(
    #             57.7081,
    #             11.9653,
    #             "20030101",
    #             "20031231",
    #             dir_save=p_data_test.as_posix(),
    #             force_download=False,
    #         )
    #         df_forcing = sp.util.read_suews(list_fn_fc[0])
    #         ser_tair = df_forcing.Tair
    #         # ds_sfc = xr.open_mfdataset(list_fn_sfc)
    #         # ser_t2 = ds_sfc.t2m.to_series()
    #         # res_dif = ((df_forcing.Tair + 273.15 - ser_t2.values) / 98).round(4)
    #         test_dif = -30 < ser_tair.max() < 100
    #         self.assertTrue(test_dif)

    # # test if the sample output is the same as the one in the repo
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_benchmark1_same(self):
    #     print("\n========================================")
    #     print("Testing if benchmark1 output is the same...")
    #     path_to_bm1 = Path(__file__).parent / "benchmark1"
    #     path_to_bm1_yml = path_to_bm1 / "benchmark1.yml"
    #     p_df_bm1 = path_to_bm1 / "benchmark1.pkl"

    #     config = sp.data_model.init_config_from_yaml(path_to_bm1_yml)
    #     df_state_init_bm1 = config.to_df_state()
    #     grid = df_state_init_bm1.index[0]
    #     df_forcing_tstep = sp.load_forcing_grid(path_to_bm1_yml, grid=grid, df_state_init=df_state_init_bm1)
    #     # met_path = str(config.model.control.forcing_file)
    #     # df_forcing_tstep = sp._load.load_SUEWS_Forcing_met_df_yaml(met_path)

    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 365]

    #     # single-step results
    #     df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init_bm1)

    #     # only test chosen columns
    #     col_test = [
    #         "QN",
    #         "QF",
    #         "QS",
    #         "QE",
    #         "QH",
    #         "T2",
    #         "RH2",
    #         "U10",
    #     ]

    #     print(f"Columns to test: {col_test}")

    #     # load sample output
    #     df_res_bm1 = pd.read_pickle(p_df_bm1).loc[:, col_test]

    #     # choose the same columns as the testing group
    #     df_res_s = df_output_s.SUEWS.loc[df_res_bm1.index, df_res_bm1.columns]

    #     pd.testing.assert_frame_equal(
    #         left=df_res_s,
    #         right=df_res_bm1,
    #         rtol=8e-3,  # 0.8% tolerance - temporary fix to pass the CI test
    #     )
    
    # # @skipUnless(flag_full_test, "Full test is not required.")
    # # def test_benchmark1b_same(self):
    # #     print("\n========================================")
    # #     print("Testing if benchmark1 output is the same...")
    # #     path_to_bm1 = Path(__file__).parent / "benchmark1"
    # #     path_to_bm1_yml = path_to_bm1 / "benchmark1b.yml"
    # #     p_df_bm1 = path_to_bm1 / "benchmark1b.pkl"

    # #     config = sp.data_model.init_config_from_yaml(path_to_bm1_yml)
    # #     df_state_init = config.to_df_state()
    # #     grid = df_state_init.index[0]
    # #     df_forcing_tstep = sp.load_forcing_grid(path_to_bm1_yml, grid=grid, df_state_init=df_state_init)
    # #     # met_path = str(config.model.control.forcing_file)
    # #     # df_forcing_tstep = sp._load.load_SUEWS_Forcing_met_df_yaml(met_path)

    # #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 365]

    # #     # single-step results
    # #     df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

    # #     # only test chosen columns
    # #     col_test = [
    # #         "QN",
    # #         "QF",
    # #         "QS",
    # #         "QE",
    # #         "QH",
    # #         "T2",
    # #         "RH2",
    # #         "U10",
    # #     ]

    # #     print(f"Columns to test: {col_test}")

    # #     # load sample output
    # #     df_res_bm1 = pd.read_pickle(p_df_bm1).loc[:, col_test]

    # #     # choose the same columns as the testing group
    # #     df_res_s = df_output_s.SUEWS.loc[df_res_bm1.index, df_res_bm1.columns]

    # #     pd.testing.assert_frame_equal(
    # #         left=df_res_s,
    # #         right=df_res_bm1,
    # #         rtol=8e-3,  # 0.8% tolerance - temporary fix to pass the CI test
    # #     )

    # # # test if the sample output is the same as the one in the repo
    # # @skipUnless(flag_full_test, "Full test is not required.")
    # # def test_is_sample_output_same(self):
    # #     print("\n========================================")
    # #     print("Testing if sample output is the same...")
    # #     df_state_init, df_forcing_tstep = sp.load_SampleData()
    # #     df_forcing_part = df_forcing_tstep.iloc[: 288 * 365]

    # #     # single-step results
    # #     df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

    # #     # only test chosen columns
    # #     col_test = [
    # #         "QN",
    # #         "QF",
    # #         "QS",
    # #         "QE",
    # #         "QH",
    # #         "T2",
    # #         "RH2",
    # #         "U10",
    # #     ]

    # #     print(f"Columns to test: {col_test}")

    # #     # load sample output
    # #     df_res_sample = pd.read_pickle(p_df_sample).loc[:, col_test]

    # #     # choose the same columns as the testing group
    # #     df_res_s = df_output_s.SUEWS.loc[df_res_sample.index, df_res_sample.columns]
    # #     import pdb; pdb.set_trace()
    # #     pd.testing.assert_frame_equal(
    # #         left=df_res_s,
    # #         right=df_res_sample,
    # #         rtol=8e-3,  # 0.8% tolerance - temporary fix to pass the CI test
    # #     )

    # # test if the weighted SMD of vegetated surfaces are properly calculated
    # @skipUnless(flag_full_test, "Full test is not required.")
    # def test_is_smd_veg_weighted(self):
    #     print("\n========================================")
    #     print("Testing if SMD of vegetated surfaces are properly calculated...")
    #     soilstorecap = np.ones(7) * 100
    #     sfr_surf = np.random.random(7)
    #     soilstore_id = np.random.random(7) * 80
    #     nonwaterfraction = sfr_surf[:-1].sum()

    #     # correct SMD_veg
    #     smd = soilstorecap - soilstore_id
    #     smd_veg = smd[2:5]
    #     surf_veg = sfr_surf[2:5]
    #     surf_veg = surf_veg / surf_veg.sum()
    #     smd_veg_correct = np.dot(surf_veg, smd_veg)

    #     # test SMD_veg
    #     from supy.supy_driver import Waterdist_Module as wm

    #     smd_veg_test = wm.cal_smd_veg(soilstorecap, soilstore_id, sfr_surf)

    #     self.assertAlmostEqual(smd_veg_correct, smd_veg_test)

    # # test if dailystate are written out correctly
    # def test_dailystate_meaningful(self):
    #     print("\n========================================")
    #     print("Testing if dailystate are written out correctly...")
    #     # df_state_init, df_forcing_tstep = sp.load_SampleData()
    #     n_days = 10
    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * n_days]

    #     # single-step results
    #     df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)
    #     df_dailystate = df_output.DailyState
    #     n_days_test = df_dailystate.dropna().drop_duplicates().shape[0]
    #     self.assertEqual(n_days_test, n_days)

    # # test if the water balance is closed
    # def test_water_balance_closed(self):
    #     print("\n========================================")
    #     print("Testing if water balance is closed...")
    #     n_days = 100
    #     df_forcing_part = df_forcing_tstep.iloc[: 288 * n_days]
    #     df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

    #     # get soilstore
    #     df_soilstore = df_output.loc[1,"debug"].filter(regex="^ss_.*_next$")
    #     ser_sfr_surf = df_state_init.sfr_surf.iloc[0]
    #     ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)

    #     # get water balance
    #     df_water = df_output.SUEWS[['Rain','Irr','Evap','RO','State']].assign(
    #         SoilStore=ser_soilstore,
    #         TotalStore=ser_soilstore+df_output.SUEWS.State
    #     )
    #     # ===============================
    #     # check if water balance is closed
    #     # ===============================
    #     # change in total store
    #     ser_totalstore_change = df_water.TotalStore.diff().dropna()
    #     # water input
    #     ser_water_in = df_water.Rain+df_water.Irr
    #     # water output
    #     ser_water_out = df_water.Evap+df_water.RO
    #     # water balance
    #     ser_water_balance = ser_water_in-ser_water_out
    #     # test if water balance is closed
    #     test_dif = (ser_totalstore_change-ser_water_balance).abs().max() < 1e-6
    #     self.assertTrue(test_dif)
