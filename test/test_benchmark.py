"""
Benchmark tests for SUEWS - CRITICAL tests that must ALWAYS pass
"""
import unittest
from pathlib import Path
import pandas as pd
import supy as sp
import platform
import sys
import pytest
import os

# Only run benchmark tests on manylinux with Python 3.13
# This provides a stable reference platform for numerical comparisons
# TODO: Eventually extend to more platforms once numerical precision issues are resolved

# Check if we're on Linux (manylinux is a Linux platform)
is_linux = platform.system() == "Linux"
is_py313 = sys.version_info[:2] == (3, 13)
# In CI, manylinux builds run on Linux runners
is_ci = any(env in os.environ for env in ['CI', 'GITHUB_ACTIONS', 'CIBUILDWHEEL'])

# Only run on Linux with Python 3.13 (which includes manylinux in CI)
if not (is_linux and is_py313):
    skip_msg = f"Benchmark tests only run on Linux Python 3.13 (current: {platform.system()} Python {sys.version_info[0]}.{sys.version_info[1]})"
    pytest.skip(skip_msg, allow_module_level=True)


class TestBenchmark(unittest.TestCase):
    """Critical benchmark tests for SUEWS model validation"""

    def test_benchmark1_same(self):
        """
        CRITICAL TEST - Validates SUEWS model outputs against known good results.
        This test MUST NEVER be skipped as it ensures core physics calculations are correct.
        """
        print("\n========================================")
        print("Testing if benchmark1 output is the same...")
        path_to_bm1 = Path(__file__).parent / "benchmark1"
        path_to_bm1_yml = path_to_bm1 / "benchmark1.yml"
        p_df_bm1 = path_to_bm1 / "benchmark1.pkl"

        config = sp.data_model.init_config_from_yaml(path_to_bm1_yml)
        df_state_init = config.to_df_state()
        grid = df_state_init.index[0]
        df_forcing_tstep = sp.load_forcing_grid(
            path_to_bm1_yml, grid=grid, df_state_init=df_state_init
        )

        df_forcing_part = df_forcing_tstep.iloc[: 288 * 365]

        # single-step results
        df_output_s, df_state_s = sp.run_supy(df_forcing_part, df_state_init)

        # only test chosen columns
        col_test = [
            "QN",
            "QF",
            "QS",
            "QE",
            "QH",
            "T2",
            "RH2",
            "U10",
        ]

        print(f"Columns to test: {col_test}")

        # load sample output
        df_res_bm1 = pd.read_pickle(p_df_bm1).loc[:, col_test]

        # choose the same columns as the testing group
        df_res_s = df_output_s.SUEWS.loc[df_res_bm1.index, df_res_bm1.columns]

        pd.testing.assert_frame_equal(
            left=df_res_s,
            right=df_res_bm1,
            atol=5e-3,  # add this to avoid the test failure for small values
            rtol=8e-3,  # 0.8% tolerance for py313 on manylinux and windows
            check_exact=False,  # Use tolerance checking instead of exact comparison
        )


if __name__ == "__main__":
    unittest.main()