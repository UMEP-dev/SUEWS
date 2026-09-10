"""Physics checks on the bundled sample run, moved out of ``test/core/test_supy.py``.

These three tests assert on what the compiled model produces (a multi-day run
that leaves no NaN in the Fortran state, DailyState accumulation, water-balance
closure), not on the Python wrapper surface, so they are ``physics``: CI runs
them once per (OS, arch) on the build Python instead of once per CPython in the
api matrix. The assertions are unchanged from the api-marked originals.
"""

from unittest import TestCase

from conftest import (
    TIMESTEPS_PER_DAY,
    capture_test_artifacts,
    debug_dataframe_output,
    debug_on_ci,
    debug_water_balance,
)
import numpy as np
import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.physics


class TestSampleRunPhysics(TestCase):
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

    # test if dailystate are written out correctly
    def test_dailystate_meaningful(self):
        print("\n========================================")
        print("Testing if dailystate are written out correctly...")

        # Run for 10 days via the shared cached functional-API run (verified
        # equivalent to SUEWSSimulation.from_sample_data().run() for this
        # window - see task-4-report.md).
        n_days = 10
        df_output, df_state_final = self._sample_run(TIMESTEPS_PER_DAY * n_days)

        # Check that DailyState exists in output
        groups = df_output.columns.get_level_values("group").unique()
        self.assertIn("DailyState", groups, "DailyState should be in output groups")

        # Use xs() for robust MultiIndex column access across platforms
        df_dailystate = df_output.xs("DailyState", level="group", axis=1)

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

        # Seven days include both wet and dry periods in the bundled forcing
        # and reuse the same cached window as the post-processing tests. The
        # per-timestep closure contract does not require a 100-day output.
        n_days = 7
        df_output, _ = self._sample_run(TIMESTEPS_PER_DAY * n_days)

        # Get soilstore from debug output
        df_soilstore = df_output.loc[1, "debug"].filter(regex="^ss_.*_next$")
        df_state_init, _ = self._sample_data
        ser_sfr_surf = df_state_init.sfr_surf.iloc[0]
        ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)

        # Get water balance
        df_water = df_output.SUEWS[["Rain", "Irr", "Evap", "RO", "State"]].assign(
            SoilStore=ser_soilstore, TotalStore=ser_soilstore + df_output.SUEWS.State
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
