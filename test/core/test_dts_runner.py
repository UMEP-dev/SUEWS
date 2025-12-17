"""Tests for DTS simulation runner.

This module tests the DTS-based simulation runner that bypasses the DataFrame
intermediate layer and uses f90wrap DTS objects directly.

Test Categories:
1. Basic runner functionality (single timestep)
2. Multi-timestep simulation loops
3. Output extraction and validation
"""

import numpy as np
import pandas as pd
import pytest

from supy.dts import (
    create_suews_config,
    create_suews_forcing,
    create_suews_state,
    create_suews_timer,
    create_suews_site,
    populate_timer_from_datetime,
    populate_forcing_from_row,
    populate_site_from_dict,
    run_supy_dts_tstep,
)
from supy import _state_accessors as acc


# =============================================================================
# Test Fixtures
# =============================================================================


@pytest.fixture
def dts_timer():
    """Create and populate a timer for testing."""
    timer = create_suews_timer()
    populate_timer_from_datetime(
        timer,
        pd.Timestamp("2012-01-01 01:00:00"),
        tstep_s=3600,
        dt_since_start=3600,
    )
    return timer


@pytest.fixture
def dts_config():
    """Create a basic config for testing."""
    config = create_suews_config()
    # Set basic method flags
    config.storageheatmethod = 1  # OHM
    config.netradiationmethod = 1  # Default
    config.stabilitymethod = 1  # Default
    # Disable RSL (requires z-height profile initialization)
    config.roughlenmommethod = 1  # Simple roughness
    config.diagmethod = 0  # No diagnostics
    return config


@pytest.fixture
def dts_site():
    """Create and populate a site for testing."""
    site = create_suews_site(nlayer=5)
    populate_site_from_dict(site, {
        "lat": 51.51,
        "lon": -0.12,
        "alt": 20.0,
        "timezone": 0,
        "z": 25.0,  # Measurement height [m] - must be above zdm+z0m+diagnostic heights
        "surfacearea": 1000000.0,
        "sfr_surf": [0.2, 0.3, 0.1, 0.1, 0.2, 0.05, 0.05],  # 7 surfaces
        # Roughness parameters (used when roughlenmommethod=1)
        "z0m_in": 1.0,  # Roughness length momentum [m]
        "zdm_in": 7.0,  # Zero-plane displacement [m]
    })
    # Set conductance parameters (required for resistance calculation)
    # gsModel=2 is Ward et al. 2016 parameterisation
    acc.set_site_conductance(
        site,
        gsmodel=2,      # Ward et al. 2016
        g_max=3.5,      # Max conductance [mm s-1]
        g_k=200.0,      # Half-saturation of gs-kdown response [W m-2]
        g_q_base=0.1,   # Base humidity response
        g_q_shape=1.0,  # Shape of humidity response
        g_t=0.1,        # Temperature response coefficient
        g_sm=0.05,      # Soil moisture response coefficient
        kmax=1200.0,    # Max solar radiation [W m-2]
        s1=0.1,         # Soil moisture parameter 1
        s2=200.0,       # Soil moisture parameter 2 [mm]
        th=55.0,        # Upper temperature limit [degC]
        tl=-10.0,       # Lower temperature limit [degC]
    )
    return site


@pytest.fixture
def dts_state():
    """Create and allocate a state for testing."""
    state = create_suews_state(nlayer=5, ndepth=5)
    # Initialize roughness state (required for RSL calculations)
    # Values typical for a low-density urban area
    acc.set_roughness_state(
        state,
        faibldg_use=0.3,   # Building frontal area index
        faievetree_use=0.1,  # Evergreen tree frontal area
        faidectree_use=0.1,  # Deciduous tree frontal area
        fai=0.5,           # Total frontal area index
        pai=0.3,           # Plan area index
        zh=10.0,           # Mean building height [m] - CRITICAL for RSL
        z0m=1.0,           # Roughness length momentum [m]
        z0v=0.1,           # Roughness length heat [m]
        zdm=7.0,           # Zero-plane displacement [m]
        zzd=18.0,          # zMeas - zdm [m] (25 - 7 = 18)
    )
    return state


@pytest.fixture
def dts_forcing():
    """Create and populate minimal forcing for testing."""
    forcing = create_suews_forcing()
    # Set minimal forcing values for a winter night
    forcing.kdown = 0.0  # No solar radiation
    forcing.ldown = 300.0  # Typical winter LW
    forcing.rain = 0.0
    forcing.pres = 1013.0
    forcing.rh = 80.0
    forcing.temp_c = 5.0
    forcing.fcld = 0.5
    forcing.qn1_obs = -999.0  # Missing value flag
    forcing.qs_obs = -999.0
    forcing.qf_obs = -999.0
    forcing.snowfrac = 0.0
    forcing.lai_obs = -999.0
    return forcing


# =============================================================================
# Test Basic Runner Functionality
# =============================================================================


class TestDTSRunnerBasic:
    """Test basic DTS runner functionality."""

    @pytest.mark.smoke
    def test_run_supy_dts_tstep_returns_tuple(
        self, dts_timer, dts_config, dts_site, dts_state, dts_forcing
    ):
        """Test that run_supy_dts_tstep returns expected tuple structure."""
        # ACT
        result = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing
        )

        # ASSERT
        assert result is not None
        assert isinstance(result, tuple)
        assert len(result) == 2

        state_out, output_dict = result
        assert state_out is not None
        assert isinstance(output_dict, dict)

    @pytest.mark.smoke
    def test_output_dict_has_expected_keys(
        self, dts_timer, dts_config, dts_site, dts_state, dts_forcing
    ):
        """Test that output dictionary contains expected keys."""
        # ACT
        _, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing
        )

        # ASSERT - should have heat state values from extract_heat_state
        # These are the primary outputs we care about
        heat_keys = ["qh", "qe", "qs", "qn", "qf", "tsurf"]
        for key in heat_keys:
            assert key in output_dict, f"Missing key: {key}"

    def test_state_is_modified_in_place(
        self, dts_timer, dts_config, dts_site, dts_state, dts_forcing
    ):
        """Test that state object is modified during timestep."""
        # ARRANGE - get initial values
        qh_before, _, _, _, _, _ = acc.get_heat_state_scalars(dts_state)

        # ACT
        state_out, _ = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing
        )

        # ASSERT - state should be the same object (modified in place)
        assert state_out is dts_state

    def test_forcing_values_affect_output(
        self, dts_timer, dts_config, dts_site, dts_state
    ):
        """Test that different forcing produces different outputs."""
        # ARRANGE - create two different forcing scenarios
        forcing_cold = create_suews_forcing()
        forcing_cold.kdown = 0.0
        forcing_cold.temp_c = -5.0
        forcing_cold.ldown = 280.0
        forcing_cold.pres = 1013.0
        forcing_cold.rh = 90.0
        forcing_cold.rain = 0.0
        forcing_cold.fcld = 0.8

        forcing_warm = create_suews_forcing()
        forcing_warm.kdown = 500.0
        forcing_warm.temp_c = 25.0
        forcing_warm.ldown = 350.0
        forcing_warm.pres = 1013.0
        forcing_warm.rh = 50.0
        forcing_warm.rain = 0.0
        forcing_warm.fcld = 0.2

        # ACT - run with cold forcing
        state1 = create_suews_state(nlayer=5, ndepth=5)
        _, output_cold = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, state1, forcing_cold
        )

        # Reset timer for second run
        timer2 = create_suews_timer()
        populate_timer_from_datetime(
            timer2, pd.Timestamp("2012-01-01 01:00:00"), tstep_s=3600
        )

        # ACT - run with warm forcing
        state2 = create_suews_state(nlayer=5, ndepth=5)
        _, output_warm = run_supy_dts_tstep(
            timer2, dts_config, dts_site, state2, forcing_warm
        )

        # ASSERT - outputs should differ
        # QN (net radiation) should be higher with more solar input
        assert output_cold["qn"] != output_warm["qn"]


# =============================================================================
# Test Multi-Timestep Simulation
# =============================================================================


class TestDTSMultiTimestep:
    """Test multi-timestep simulation loops."""

    def test_multi_timestep_state_evolution(self, dts_config, dts_site):
        """Test that state evolves correctly over multiple timesteps."""
        # ARRANGE
        state = create_suews_state(nlayer=5, ndepth=5)
        timer = create_suews_timer()
        forcing = create_suews_forcing()

        # Set up forcing for a typical day
        forcing.pres = 1013.0
        forcing.rh = 70.0
        forcing.rain = 0.0
        forcing.fcld = 0.3
        forcing.ldown = 320.0

        # Run 24 timesteps (one day at hourly resolution)
        outputs = []
        start_dt = pd.Timestamp("2012-06-21 00:00:00")

        for hour in range(24):
            dt = start_dt + pd.Timedelta(hours=hour)
            dt_since_start = hour * 3600

            # Populate timer
            populate_timer_from_datetime(
                timer, dt, tstep_s=3600, dt_since_start=dt_since_start
            )

            # Vary forcing with time of day (simple diurnal cycle)
            if 6 <= hour <= 18:
                # Daytime
                solar_angle = (hour - 6) / 6.0 if hour <= 12 else (18 - hour) / 6.0
                forcing.kdown = 800.0 * max(0, solar_angle)
                forcing.temp_c = 15.0 + 10.0 * solar_angle
            else:
                # Nighttime
                forcing.kdown = 0.0
                forcing.temp_c = 12.0

            # Run timestep
            state, output = run_supy_dts_tstep(
                timer, dts_config, dts_site, state, forcing
            )
            outputs.append(output.copy())

        # ASSERT - verify we got 24 outputs
        assert len(outputs) == 24

        # ASSERT - verify diurnal variation in outputs
        # QN should be higher during day than night
        daytime_qn = [outputs[h]["qn"] for h in range(8, 16)]
        nighttime_qn = [outputs[h]["qn"] for h in [0, 1, 2, 22, 23]]

        # Daytime average should be higher than nighttime
        # (at least some daytime values should exceed nighttime)
        assert max(daytime_qn) > max(nighttime_qn)

    def test_state_persistence_across_timesteps(self, dts_config, dts_site):
        """Test that state modifications persist across timesteps."""
        # ARRANGE
        state = create_suews_state(nlayer=5, ndepth=5)
        timer = create_suews_timer()
        forcing = create_suews_forcing()

        # Set forcing
        forcing.kdown = 400.0
        forcing.ldown = 320.0
        forcing.temp_c = 20.0
        forcing.pres = 1013.0
        forcing.rh = 60.0
        forcing.rain = 0.0
        forcing.fcld = 0.3

        # Get initial state
        initial_qh, _, _, _, _, initial_tsurf = acc.get_heat_state_scalars(state)

        # Run a few timesteps
        start_dt = pd.Timestamp("2012-06-21 12:00:00")
        for i in range(5):
            dt = start_dt + pd.Timedelta(hours=i)
            populate_timer_from_datetime(
                timer, dt, tstep_s=3600, dt_since_start=i * 3600
            )
            state, _ = run_supy_dts_tstep(
                timer, dts_config, dts_site, state, forcing
            )

        # Get final state
        final_qh, _, _, _, _, final_tsurf = acc.get_heat_state_scalars(state)

        # ASSERT - state should have evolved
        # (exact values depend on physics, but should differ from initial zeros)
        # At least one of the values should have changed
        state_changed = (
            initial_qh != final_qh or
            initial_tsurf != final_tsurf
        )
        assert state_changed, "State did not evolve over timesteps"


# =============================================================================
# Test Output Extraction
# =============================================================================


class TestDTSOutputExtraction:
    """Test output extraction from DTS runner."""

    def test_dataoutlinesuews_extraction(
        self, dts_timer, dts_config, dts_site, dts_state, dts_forcing
    ):
        """Test extraction of dataoutlinesuews array."""
        # ACT
        _, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing
        )

        # ASSERT - check for raw output array if present
        if "dataoutlinesuews" in output_dict:
            data = output_dict["dataoutlinesuews"]
            assert isinstance(data, np.ndarray)
            assert len(data) > 0

    def test_heat_state_values_in_output(
        self, dts_timer, dts_config, dts_site, dts_state, dts_forcing
    ):
        """Test that heat state values are correctly extracted."""
        # ACT
        state_out, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing
        )

        # Get values directly from state for comparison
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(state_out)

        # ASSERT - output dict should match state values
        assert output_dict["qh"] == pytest.approx(qh)
        assert output_dict["qe"] == pytest.approx(qe)
        assert output_dict["qs"] == pytest.approx(qs)
        assert output_dict["qn"] == pytest.approx(qn)
        assert output_dict["qf"] == pytest.approx(qf)
        assert output_dict["tsurf"] == pytest.approx(tsurf)


# =============================================================================
# Test Edge Cases
# =============================================================================


class TestDTSRunnerEdgeCases:
    """Test edge cases and error handling."""

    def test_zero_forcing_values(self, dts_timer, dts_config, dts_site, dts_state):
        """Test runner handles zero forcing values."""
        forcing = create_suews_forcing()
        forcing.kdown = 0.0
        forcing.ldown = 0.0
        forcing.temp_c = 0.0
        forcing.pres = 1013.0
        forcing.rh = 50.0
        forcing.rain = 0.0
        forcing.fcld = 0.0

        # Should not raise
        state_out, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, forcing
        )

        assert state_out is not None
        assert output_dict is not None

    def test_extreme_forcing_values(self, dts_timer, dts_config, dts_site, dts_state):
        """Test runner handles extreme but valid forcing values."""
        forcing = create_suews_forcing()
        forcing.kdown = 1200.0  # High solar (desert noon)
        forcing.ldown = 450.0  # High LW (hot humid)
        forcing.temp_c = 45.0  # Very hot
        forcing.pres = 1030.0  # High pressure
        forcing.rh = 95.0  # High humidity
        forcing.rain = 0.0
        forcing.fcld = 1.0  # Overcast

        # Should not raise
        state_out, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, forcing
        )

        assert state_out is not None
        assert output_dict is not None

    def test_debug_mode(self, dts_timer, dts_config, dts_site, dts_state, dts_forcing):
        """Test runner with debug mode enabled."""
        # ACT
        state_out, output_dict = run_supy_dts_tstep(
            dts_timer, dts_config, dts_site, dts_state, dts_forcing,
            debug=True
        )

        # ASSERT - should work with debug enabled
        assert state_out is not None
        assert output_dict is not None
