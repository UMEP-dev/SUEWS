#!/usr/bin/env python3
"""
Test Fortran State Persistence Within Same Python Process

This test suite verifies that Fortran state does not persist between multiple
calls to sp.run_supy() within the same Python process. State persistence would
indicate uninitialized variables or improper state reset mechanisms.

Based on user correction: "you need to test running the fortran multiple times
within the same python script. Not calling the test 3 separate times. A new
python kernel will reset both python and fortran states."
"""

import logging
from pathlib import Path
from unittest import TestCase
import warnings

import supy as sp

# Suppress logging to get clean output
logging.getLogger("SuPy").setLevel(logging.CRITICAL)


class TestFortranStatePersistence(TestCase):
    """Test suite for Fortran state persistence issues."""

    def setUp(self):
        """Set up test environment."""
        warnings.simplefilter("ignore", category=ImportWarning)

        # Clear any cached data from previous tests
        import functools
        import gc

        # Clear all LRU caches in the supy module
        for obj in gc.get_objects():
            if isinstance(obj, functools._lru_cache_wrapper):
                try:
                    obj.cache_clear()
                except:
                    pass

        # More aggressive cache clearing for supy._load module
        try:
            import supy._load

            # Clear specific caches in _load module
            for attr_name in dir(supy._load):
                attr = getattr(supy._load, attr_name)
                if hasattr(attr, "cache_clear"):
                    attr.cache_clear()
        except:
            pass

    def run_sample_year(self):
        """Run the sample year simulation that shows pollution effects."""
        trv_sample_data = Path(sp.__file__).parent / "sample_data"
        path_config_default = trv_sample_data / "sample_config.yml"

        df_state_init = sp.init_supy(path_config_default, force_reload=True)
        df_forcing_tstep = sp.load_forcing_grid(
            path_config_default, df_state_init.index[0], df_state_init=df_state_init
        )

        # Run one year
        df_forcing_part = df_forcing_tstep.iloc[: 288 * 366]
        df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)

        return df_output.SUEWS["QE"].values[286]

    def run_benchmark_test(self):
        """Run the benchmark test that causes pollution."""
        p_config = Path("test/fixtures/benchmark1/benchmark1.yml")
        if p_config.exists():
            df_state_init = sp.init_supy(p_config, force_reload=True)
            df_forcing_tstep = sp.load_forcing_grid(
                p_config, df_state_init.index[0], df_state_init=df_state_init
            )
            df_output, df_state = sp.run_supy(df_forcing_tstep, df_state_init)
            return True
        return False

    def run_short_benchmark(self):
        """Run a short version of benchmark test to minimize execution time."""
        p_config = Path("test/fixtures/benchmark1/benchmark1.yml")
        if p_config.exists():
            df_state_init = sp.init_supy(p_config, force_reload=True)
            df_forcing_tstep = sp.load_forcing_grid(
                p_config, df_state_init.index[0], df_state_init=df_state_init
            )
            # Just run first day to minimize time
            df_forcing_part = df_forcing_tstep.iloc[:288]
            df_output, df_state = sp.run_supy(df_forcing_part, df_state_init)
            return True
        return False

    def test_fortran_state_persistence(self):
        """
        Test if Fortran state persists between multiple calls in the same Python process.

        This test passes if no difference is found between runs, fails if pollution is detected.
        """
        print("\n" + "=" * 70)
        print("FORTRAN STATE PERSISTENCE TEST")
        print("=" * 70)

        print("\n1. Clean baseline run...")
        clean_qe = self.run_sample_year()
        print(f"   Clean QE at timestep 286: {clean_qe:.6f}")

        print("\n2. Running benchmark test (potential polluter)...")
        benchmark_success = self.run_benchmark_test()
        print(f"   Benchmark completed: {benchmark_success}")

        print("\n3. Sample year run after benchmark (same Python process)...")
        polluted_qe = self.run_sample_year()
        print(f"   Polluted QE at timestep 286: {polluted_qe:.6f}")

        diff = abs(clean_qe - polluted_qe)
        print("\n*** RESULTS ***")
        print(f"Difference: {diff:.6f}")

        if diff > 1e-6:
            print("*** FORTRAN STATE PERSISTENCE DETECTED ***")
            print("The problem persists within the same Python process.")
            print(
                "This indicates Fortran module variables are not being reset between calls."
            )
            self.fail(f"Fortran state persistence detected: QE difference = {diff:.6f}")
        else:
            print("*** NO FORTRAN STATE PERSISTENCE ***")
            print("The problem does not occur within the same Python process.")
            print("Test PASSED: No state pollution detected.")

    def test_multiple_consecutive_runs(self):
        """Test multiple consecutive runs to see if pollution accumulates."""
        print("\n" + "=" * 70)
        print("MULTIPLE CONSECUTIVE RUNS TEST")
        print("=" * 70)

        print("\n1. Initial clean run...")
        baseline_qe = self.run_sample_year()
        print(f"   Baseline QE: {baseline_qe:.6f}")

        results = [baseline_qe]

        # Run multiple polluter-target cycles
        for i in range(3):
            print(f"\n{i + 2}. Cycle {i + 1}: Polluter + Target...")

            # Short polluter run
            self.run_short_benchmark()

            # Target run
            cycle_qe = self.run_sample_year()
            results.append(cycle_qe)

            diff_from_baseline = abs(baseline_qe - cycle_qe)
            print(
                f"   Cycle {i + 1} QE: {cycle_qe:.6f} (diff from baseline: {diff_from_baseline:.6f})"
            )

        print("\n*** CONSECUTIVE RUNS RESULTS ***")
        print(f"Baseline:  {results[0]:.6f}")
        for i, qe in enumerate(results[1:], 1):
            diff = abs(results[0] - qe)
            print(f"Cycle {i}:   {qe:.6f} (diff: {diff:.6f})")

        # Check if pollution is consistent or accumulating
        diffs = [abs(results[0] - qe) for qe in results[1:]]
        max_diff = max(diffs)
        min_diff = min(diffs)

        if max_diff > 1e-6:
            print("\n*** POLLUTION CONFIRMED ***")
            print(f"Max difference: {max_diff:.6f}")
            print(f"Min difference: {min_diff:.6f}")

            if max_diff - min_diff < 1e-8:
                print("*** CONSISTENT POLLUTION - Same difference each time ***")
                print("This suggests deterministic Fortran state pollution.")
            else:
                print("*** VARIABLE POLLUTION - Different differences ***")
                print("This suggests accumulating or variable state pollution.")

            self.fail(
                f"Multiple consecutive runs show pollution: max diff = {max_diff:.6f}"
            )
        else:
            print("*** NO POLLUTION DETECTED ***")
            print("Test PASSED: No pollution in consecutive runs.")

    def test_minimal_reproduction(self):
        """Create minimal reproduction case."""
        print("\n" + "=" * 70)
        print("MINIMAL REPRODUCTION TEST")
        print("=" * 70)

        print("\n1. Minimal clean run...")
        clean_qe = self.run_sample_year()
        print(f"   Clean QE: {clean_qe:.6f}")

        print("\n2. Minimal polluter (just first day of benchmark)...")
        success = self.run_short_benchmark()
        print(f"   Short benchmark success: {success}")

        print("\n3. Minimal target run...")
        polluted_qe = self.run_sample_year()
        print(f"   Polluted QE: {polluted_qe:.6f}")

        diff = abs(clean_qe - polluted_qe)
        print("\n*** MINIMAL REPRODUCTION RESULTS ***")
        print(f"Difference: {diff:.6f}")

        if diff > 1e-6:
            print("*** MINIMAL REPRODUCTION SUCCESSFUL ***")
            print("Even a short benchmark run causes pollution.")
            self.fail(
                f"Minimal reproduction shows pollution: QE difference = {diff:.6f}"
            )
        else:
            print("*** MINIMAL REPRODUCTION PASSED ***")
            print("Short benchmark run does not cause pollution.")
            print("Test PASSED: No state pollution detected in minimal case.")


if __name__ == "__main__":
    import unittest

    unittest.main()
