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
            (2.0, 0.1, 1.4),  # Very low buildings (z0m < zdm)
            (5.0, 0.5, 3.5),  # Low-rise scenario (catches #572 profile cap)
            (10.0, 1.0, 7.0),  # Standard case
            (1.0, 0.01, 0.7),  # Extremely low roughness
            (2.0, 0.01, 1.4),  # Low roughness with 2 m canopy
        ],
    )
    def test_wind_speeds_positive(self, sample_data, zh, z0m_in, zdm_in):
        """Test that U10 and full RSL/MOST profiles stay finite, non-negative, and monotone."""
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

        # Verify the full 30-level diagnostic profile across all 48 timesteps
        # (catches #572 where U10 stayed positive while profile winds went negative/NaN).
        z_profile = _required_output(
            df_output,
            df_state_init,
            df_forcing_short,
            "RSL",
            [f"z_{level}" for level in PROFILE_LEVELS],
        )
        u_profile = _required_output(
            df_output,
            df_state_init,
            df_forcing_short,
            "RSL",
            [f"U_{level}" for level in PROFILE_LEVELS],
        )
        assert (np.diff(z_profile, axis=1) > 0).all(), (
            f"Profile heights not strictly monotone for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )
        assert (u_profile >= 0).all(), (
            f"Profile winds negative for zh={zh}, z0m={z0m_in}, zdm={zdm_in}"
        )

    @pytest.mark.parametrize(
        "zh,z0m_in,zdm_in",
        [
            (2.0, 0.1, 0.1),
            (1.0, 0.01, 0.01),
            (2.0, 0.01, 0.01),
        ],
    )
    def test_equal_roughness_and_displacement_refused(
        self, sample_data, zh, z0m_in, zdm_in
    ):
        """Equal roughness and displacement heights are rejected before execution."""
        df_state_init, df_forcing = sample_data
        df_forcing_short = df_forcing.iloc[:2]

        df_state_init.loc[:, "zh"] = zh
        df_state_init.loc[:, "z0m_in"] = z0m_in
        df_state_init.loc[:, "zdm_in"] = zdm_in
        df_state_init.loc[:, "rslmethod"] = 0
        df_state_init.loc[:, "stabilitymethod"] = 2

        with pytest.raises(ValueError, match=r"z0m_in .* must be less than zdm_in"):
            run_simulation(df_forcing_short, df_state_init)

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


@pytest.mark.core
@pytest.mark.parametrize("canopy_height", [0.5, 1.0, 1.5])
def test_forced_rsl_low_canopy_uses_consistent_zh_floor(canopy_height):
    """Regression test for GH#1883: RSL_cal_prms and RSLProfile share the 2 m zH floor."""
    df_state_init, df_forcing = load_sample_frames()
    df_state = df_state_init.iloc[[0]].copy()
    # Summer daytime slice so QH and QE are both active for scalar profile checks.
    df_forcing_short = df_forcing.loc["2012-07-01 09:05:00":"2012-07-01 11:00:00"].copy()

    df_state.loc[:, ("rslmethod", "0")] = 1
    df_state.loc[:, ("bldgh", "0")] = canopy_height
    df_state.loc[:, ("evetreeh", "0")] = canopy_height
    df_state.loc[:, ("dectreeh", "0")] = canopy_height

    df_output, _ = run_simulation(df_forcing_short, df_state)
    fluxes = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "SUEWS",
        ["QH", "QE"],
    )
    assert np.max(np.abs(fluxes[:, 0])) > 10.0
    assert np.max(np.abs(fluxes[:, 1])) > 1.0

    diag = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        ["flag_RSL", "z_10", "z_20", "zH_RSL"],
    )
    assert (diag[:, 0] == 1).all()
    np.testing.assert_allclose(diag[:, 3], 2.0, rtol=0, atol=1e-9)
    np.testing.assert_allclose(diag[:, 2], diag[:, 3], rtol=0, atol=1e-9)
    np.testing.assert_allclose(diag[:, 1], 0.5 * diag[:, 3], rtol=0, atol=1e-9)

    z_profile = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        [f"z_{i}" for i in PROFILE_LEVELS],
    )
    u_profile = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        [f"U_{i}" for i in PROFILE_LEVELS],
    )
    t_profile = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        [f"T_{i}" for i in PROFILE_LEVELS],
    )
    q_profile = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        [f"q_{i}" for i in PROFILE_LEVELS],
    )
    assert (np.diff(z_profile, axis=1) > 0).all()
    assert (u_profile >= 0).all()
    # Within the canopy (levels 1..20), wind must decay downward rather than
    # jumping above U_20 due to zarray(z) > zH_RSL.
    assert (np.diff(u_profile[:, :20], axis=1) >= 0).all()
    # Forcing-height boundary condition: level 30 equals forcing air temperature,
    # and specific humidity stays positive throughout the profile.
    np.testing.assert_allclose(
        t_profile[:, -1], df_forcing_short["Tair"].to_numpy(dtype=float), rtol=0, atol=1e-9
    )
    assert (q_profile > 0.0).all()
    # In-canopy scalar profiles must approach level 20 smoothly without a
    # positive-exponent canopy-top jump (|T_19 - T_20| << |T_1 - T_20|).
    t_can_delta = np.abs(t_profile[:, :20] - t_profile[:, 19:20])
    q_can_delta = np.abs(q_profile[:, :20] - q_profile[:, 19:20])
    assert (np.diff(t_can_delta, axis=1) <= 1e-12).all()
    assert (np.diff(q_can_delta, axis=1) <= 1e-12).all()


@pytest.mark.core
@pytest.mark.parametrize("forcing_height", [1.1, 2.0])
def test_forced_rsl_rejects_forcing_height_not_above_zh_floor(forcing_height):
    """Forced RSL must fail with code 119 when zMeas <= ZH_MIN (2 m) instead of NaN."""
    df_state_init, df_forcing = load_sample_frames()
    df_state = df_state_init.iloc[[0]].copy()
    df_forcing_short = df_forcing.iloc[:2]

    df_state.loc[:, ("rslmethod", "0")] = 1
    df_state.loc[:, ("bldgh", "0")] = 0.5
    df_state.loc[:, ("evetreeh", "0")] = 0.5
    df_state.loc[:, ("dectreeh", "0")] = 0.5
    df_state.loc[:, ("z", "0")] = forcing_height
    df_state.loc[:, ("zdm_in", "0")] = 0.3
    df_state.loc[:, ("z0m_in", "0")] = 0.05

    with pytest.raises(
        RuntimeError,
        match=r"code 119.*RSLProfile: forcing height zMeas must exceed effective RSL canopy height zH_RSL",
    ):
        run_simulation(df_forcing_short, df_state)


@pytest.mark.core
def test_forced_rsl_accepts_forcing_height_just_above_zh_floor():
    """Forced RSL succeeds cleanly when zMeas exceeds ZH_MIN (2 m) over a low canopy."""
    df_state_init, df_forcing = load_sample_frames()
    df_state = df_state_init.iloc[[0]].copy()
    df_forcing_short = df_forcing.iloc[:2]

    df_state.loc[:, ("rslmethod", "0")] = 1
    df_state.loc[:, ("bldgh", "0")] = 0.5
    df_state.loc[:, ("evetreeh", "0")] = 0.5
    df_state.loc[:, ("dectreeh", "0")] = 0.5
    df_state.loc[:, ("z", "0")] = 2.5
    df_state.loc[:, ("zdm_in", "0")] = 0.3
    df_state.loc[:, ("z0m_in", "0")] = 0.05

    df_output, _ = run_simulation(df_forcing_short, df_state)
    diag = _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        ["flag_RSL", "z_20", "z_30", "zH_RSL"],
    )
    assert (diag[:, 0] == 1).all()
    np.testing.assert_allclose(diag[:, 3], 2.0, rtol=0, atol=1e-9)
    np.testing.assert_allclose(diag[:, 1], 2.0, rtol=0, atol=1e-9)
    np.testing.assert_allclose(diag[:, 2], 2.5, rtol=0, atol=1e-9)
    _required_output(
        df_output,
        df_state,
        df_forcing_short,
        "RSL",
        [f"{var}_{i}" for var in ("U", "T", "q") for i in PROFILE_LEVELS],
    )
