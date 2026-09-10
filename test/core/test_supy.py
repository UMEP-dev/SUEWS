import io
from pathlib import Path
import platform
import sys
import tempfile
from time import time
from unittest import TestCase

# Import debug utilities from conftest (centralised)
from conftest import (
    SHORT_RUN_STEPS,
    TIMESTEPS_PER_DAY,
    run_simulation,
)
import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy import SUEWSSimulation

# Get the test data directory from the environment variable
test_data_dir = Path(__file__).parent.parent / "fixtures" / "data_test"
# test_data_dir = os.environ.get('TEST_DATA_DIR', Path(__file__).parent.parent / 'fixtures' / 'data_test')

# Note: sample_output.pkl testing has been moved to test_sample_output.py

# Note: Sample data loading moved to individual test methods to avoid test interference
# This prevents caching issues when tests run in sequence

pytestmark = pytest.mark.api


class TestSuPy(TestCase):
    @pytest.fixture(autouse=True, scope="class")
    @classmethod
    def _sample_fixtures(cls, request, sample_data_loaded, sample_run_cached):
        """Bridge session-scoped sample fixtures onto this unittest.TestCase.

        ``sample_data_loaded`` is the bundled ``(df_state_init, df_forcing)``
        tuple loaded once for the session; ``sample_run_cached`` is the
        matching OOP run factory (see conftest.py). Methods below
        MUST ``.copy()`` any frame they mutate.

        ``@classmethod``: a class-scoped fixture runs once per class, not
        once per test instance, so it must set attributes on ``cls`` rather
        than being bound as an instance method (pytest deprecates the
        instance-method form; see PytestRemovedIn10Warning).
        """
        request.cls._sample_data = sample_data_loaded
        request.cls._sample_run = staticmethod(sample_run_cached)

    # test if single-tstep mode can run
    @pytest.mark.physics
    @pytest.mark.smoke
    @pytest.mark.smoke_bridge
    def test_is_supy_running_single_step(self):
        print("\n========================================")
        print("Testing if single-tstep mode can run...")

        # Create simulation with sample data
        sim = SUEWSSimulation.from_sample_data()

        # Run only 1 hour (12 timesteps) instead of 8 hours
        results = sim.run(end_date=sim.forcing.index[11])

        # Verify results are populated
        self.assertIsNotNone(results)
        self.assertFalse(results.empty)

    # test if multi-grid simulation can run in parallel
    def test_is_supy_sim_save_multi_grid_par(self):
        print("\n========================================")
        print("Testing if multi-grid simulation can run in parallel...")
        n_grid = 4

        # Shared sample data (session-scoped); copy since we mutate below.
        df_state_init, df_forcing_tstep = self._sample_data

        df_state_init_base = df_state_init.copy()

        df_state_init_multi = pd.concat([df_state_init_base for x in range(n_grid)])
        df_state_init_multi.index = pd.RangeIndex(n_grid, name="grid")
        # The contract is multi-grid execution plus save output; two hours is
        # sufficient and avoids retaining 60 days of four-grid output.
        df_forcing_part = df_forcing_tstep.iloc[:SHORT_RUN_STEPS]
        t_start = time()
        simulation = SUEWSSimulation.from_state(df_state_init_multi)
        simulation.update_forcing(df_forcing_part)
        output = simulation.run()
        df_output = output.df
        df_state = output.state_final
        t_end = time()

        test_success_sim = np.all([
            not df_output.empty,
            not df_state.empty,
        ])

        with tempfile.TemporaryDirectory() as dir_temp:
            list_outfile = output.save(dir_temp)

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

    def test_debug_mode_exposes_debug_output_group(self):
        print("\n========================================")
        print("Testing if debug_mode exposes debug output...")

        df_state_init, df_forcing_tstep = self._sample_data
        df_forcing_part = df_forcing_tstep.iloc[:24]

        df_output, df_state = run_simulation(
            df_forcing_part,
            df_state_init.copy(),
        )

        self.assertFalse(df_output.empty)
        self.assertFalse(df_state.empty)
        self.assertIn("debug", df_output.columns.get_level_values("group"))
        self.assertIn("flag_test", df_output["debug"].columns)
        self.assertTrue(df_output["debug"]["flag_test"].notna().all())

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
        """Test complete checkpoint save/load/run cycle with version tracking.

        This test verifies that:
        1. Version info is automatically saved with state
        2. A typed checkpoint can be saved and loaded
        3. Loaded checkpoint can be used to continue simulation
        4. Version info persists through the cycle
        """
        print("\n========================================")
        print(
            "Testing complete checkpoint save/load/run cycle with version tracking..."
        )

        # Run 1: Initial simulation
        sim1 = SUEWSSimulation.from_sample_data()
        sim1.run(end_date=sim1.forcing.index[11])  # 12 timesteps

        # Verify version was automatically added
        self.assertIn(("version", "0"), sim1.state_final.columns)
        version_from_run1 = sim1.state_final[("version", "0")].iloc[0]
        self.assertEqual(version_from_run1, sp.__version__)

        # Save checkpoint to temporary location
        with tempfile.TemporaryDirectory() as tmpdir:
            save_path = Path(tmpdir)
            sim1.save(save_path)

            # Verify checkpoint file was created as the restart artifact
            checkpoint_files = list(save_path.glob("*_checkpoint.json"))
            self.assertGreater(
                len(checkpoint_files),
                0,
                "Checkpoint file should be created",
            )
            state_files = list(save_path.glob("*_state_*.csv"))
            self.assertEqual(
                state_files,
                [],
                "OOP save should not create legacy DFState CSV by default",
            )

            # Run 2: Load saved checkpoint and continue simulation
            checkpoint = sp.SUEWSCheckpoint.from_file(checkpoint_files[0])
            sim2 = SUEWSSimulation.from_checkpoint(sim1.config, checkpoint)

            # Load forcing for continuation (using remaining timesteps)
            sim2.update_forcing(sim1.forcing.iloc[12:24])  # Next 12 timesteps

            # Run continuation, bounded to the loaded window
            results2 = sim2.run(
                start_date=sim2.forcing.index[0], end_date=sim2.forcing.index[-1]
            )

            # Verify continuation completed successfully
            self.assertIsNotNone(results2)
            self.assertIsNotNone(sim2.state_final)

            # Verify version info persisted through the cycle
            self.assertIn(("version", "0"), sim2.state_final.columns)
            version_from_run2 = sim2.state_final[("version", "0")].iloc[0]
            self.assertEqual(
                version_from_run2,
                sp.__version__,
                "Version should persist through checkpoint save/load/run cycle",
            )
            self.assertEqual(sim2.checkpoint.supy_version, sp.__version__)

        print("Version tracking works correctly through checkpoint save/load/run cycle")

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

        # test SMD_veg via Python port of Fortran cal_smd_veg
        from supy.util import cal_smd_veg

        smd_veg_test = cal_smd_veg(soilstorecap, soilstore_id, sfr_surf)

        self.assertAlmostEqual(smd_veg_correct, smd_veg_test)
