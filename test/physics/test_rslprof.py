"""
Test suews_phys_rslprof - RSL/MOST wind profile calculations.

Regression tests for Issue #572 where the MIN constraint on height arrays
caused negative LOG arguments and negative wind speeds for tall buildings.
"""

import warnings

from conftest import load_sample_frames, run_simulation
import numpy as np
import pandas as pd
import pytest

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-RESPONSE")]


def _required_output(output, state, forcing, group, variables):
    """Require every simulated grid/timestep before checking profile physics."""
    expected = pd.MultiIndex.from_product(
        [state.index, forcing.index], names=output.index.names
    )
    assert output.index.is_unique and output.index.sort_values().equals(
        expected.sort_values()
    ), (
        "Incomplete wind output rows (grid, datetime): "
        f"missing={expected.difference(output.index).tolist()}, "
        f"unexpected={output.index.difference(expected).tolist()}, "
        f"duplicates={output.index[output.index.duplicated()].tolist()}"
    )
    columns = [(group, name) for name in variables]
    missing = [col for col in columns if col not in output.columns]
    assert not missing, (
        f"Missing wind output columns for grids={state.index.tolist()}: {missing}"
    )
    values = output.loc[:, columns].to_numpy(dtype=float)
    assert values.size, f"Empty wind output for {columns}"
    invalid = np.argwhere(~np.isfinite(values) | (values == -999))
    assert not invalid.size, "Invalid wind output: " + "; ".join(
        f"{columns[col]} at grid/time={output.index[row]}: {values[row, col]}"
        for row, col in invalid[:5]
    )
    return values


# suews_phys_rslprof.f95: nz=30; output names are numbered from 1.
PROFILE_LEVELS = range(1, 31)


class TestWindProfiles:
    """Test suite for wind profile calculations."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data for testing."""
        df_state_init, df_forcing = load_sample_frames()
        return df_state_init, df_forcing

    def test_most_height_array_monotonic(self, sample_data):
        """Test that MOST generates monotonic height arrays for various building heights."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        for bldgh in [2.0, 10.0, 50.0]:
            df_state_init.loc[:, "bldgh"] = bldgh
            df_state_init.loc[:, "rslmethod"] = 0  # MOST diagnostic

            df_output, _ = run_simulation(df_forcing_short, df_state_init)

            heights = _required_output(
                df_output,
                df_state_init,
                df_forcing_short,
                "RSL",
                [f"z_{i}" for i in PROFILE_LEVELS],
            )

            assert np.all(np.diff(heights, axis=1) > 0), (
                f"Height array not monotonic for {bldgh}m building"
            )

    def test_most_and_rsl_methods_run(self, sample_data):
        """Test that both MOST and RSL methods run without errors."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]
        df_state_init.loc[:, "bldgh"] = 10.0

        # Test MOST
        df_state_init.loc[:, "rslmethod"] = 0
        df_output_most, _ = run_simulation(df_forcing_short, df_state_init)
        _required_output(
            df_output_most,
            df_state_init,
            df_forcing_short,
            "RSL",
            [f"{field}_{i}" for field in ("z", "U") for i in PROFILE_LEVELS],
        )

        # Test RSL
        df_state_init.loc[:, "rslmethod"] = 1
        df_output_rsl, _ = run_simulation(df_forcing_short, df_state_init)
        _required_output(
            df_output_rsl,
            df_state_init,
            df_forcing_short,
            "RSL",
            [f"{field}_{i}" for field in ("z", "U") for i in PROFILE_LEVELS],
        )

    def test_rsl_grid_includes_exact_canopy_top(self, sample_data):
        """Regression test for GH#296: RSL level 20 is the canopy top."""
        df_state_init, df_forcing = sample_data
        df_state = df_state_init.iloc[[0]].copy()

        df_state.loc[:, ("rslmethod", "0")] = 1
        df_state.loc[:, ("bldgh", "0")] = 40.0

        df_output, _ = run_simulation(df_forcing.iloc[:2], df_state)
        values = _required_output(
            df_output,
            df_state,
            df_forcing.iloc[:2],
            "RSL",
            ["flag_RSL", "z_20", "zH_RSL"],
        )
        assert (values[:, 0] == 1).all()
        np.testing.assert_allclose(values[:, 1], values[:, 2], rtol=0, atol=1e-9)

    @pytest.mark.parametrize(
        "zh,z0m_in,zdm_in",
        [
            (2.0, 0.1, 0.1),  # Very low buildings
            (5.0, 0.5, 3.5),  # zdm > building height scenario
            (10.0, 1.0, 7.0),  # Standard case
            (1.0, 0.01, 0.01),  # Extremely low values
            (2.0, 0.01, 0.01),  # Original test case from issue
        ],
    )
    def test_wind_speeds_positive(self, sample_data, zh, z0m_in, zdm_in):
        """Test that wind speeds are always positive for various building configs."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:48]

        df_state_init.loc[:, "zh"] = zh
        df_state_init.loc[:, "z0m_in"] = z0m_in
        df_state_init.loc[:, "zdm_in"] = zdm_in
        df_state_init.loc[:, "rslmethod"] = 0
        df_state_init.loc[:, "stabilitymethod"] = 2

        df_output, _ = run_simulation(df_forcing_short, df_state_init)

        u10 = _required_output(
            df_output, df_state_init, df_forcing_short, "SUEWS", ["U10"]
        )
        assert (u10 >= 0).all(), f"U10 negative for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"

    def test_tall_building_no_negative_winds(self, sample_data):
        """Test that tall buildings don't cause negative wind speeds."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        # Tall building configuration
        df_state_init.loc[:, "bldgh"] = 50.0
        df_state_init.loc[:, "z0m_in"] = 5.0
        df_state_init.loc[:, "zdm_in"] = 35.0
        df_state_init.loc[:, "z"] = 60.0  # Above building height
        df_state_init.loc[:, "rslmethod"] = 0

        df_output, _ = run_simulation(df_forcing_short, df_state_init)

        wind_speeds = _required_output(
            df_output,
            df_state_init,
            df_forcing_short,
            "RSL",
            [f"U_{i}" for i in PROFILE_LEVELS],
        )
        assert (wind_speeds >= 0).all(), "Negative wind speeds detected"

    def test_full_building_auto_rsl_grid_runs_without_nan(self, sample_data):
        """Regression test for GH#1223 full-building grids in the auto MOST path."""
        df_state_init, df_forcing = sample_data
        df_state_issue = df_state_init.iloc[[0]].copy()
        df_forcing_short = df_forcing.iloc[:2]

        for idx in range(7):
            df_state_issue.loc[:, ("sfr_surf", f"({idx},)")] = 0.0
        df_state_issue.loc[:, ("sfr_surf", "(1,)")] = 1.0

        df_state_issue.loc[:, ("bldgh", "0")] = 17.464
        df_state_issue.loc[:, ("evetreeh", "0")] = 0.0
        df_state_issue.loc[:, ("dectreeh", "0")] = 0.0
        df_state_issue.loc[:, ("faibldg", "0")] = 0.011
        df_state_issue.loc[:, ("faievetree", "0")] = 0.0
        df_state_issue.loc[:, ("faidectree", "0")] = 0.0
        df_state_issue.loc[:, ("faimethod", "0")] = 0
        df_state_issue.loc[:, ("roughlenmommethod", "0")] = 2
        df_state_issue.loc[:, ("rslmethod", "0")] = 2
        df_state_issue.loc[:, ("stabilitymethod", "0")] = 3
        df_state_issue.loc[:, ("z", "0")] = 52.392
        df_state_issue.loc[:, ("z0m_in", "0")] = 0.03
        df_state_issue.loc[:, ("zdm_in", "0")] = 17.464

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_output, _ = run_simulation(
                df_forcing_short,
                df_state_issue,
            )

        u10 = _required_output(
            df_output, df_state_issue, df_forcing_short, "SUEWS", ["U10"]
        )
        assert (u10 >= 0).all(), "Negative full-building auto MOST wind output"
