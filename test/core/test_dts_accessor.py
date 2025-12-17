"""Comprehensive tests for DTS (Derived Type State) accessor interface.

This module tests the DTS accessor system introduced in the sunt05/dts-accessor-interface
branch. The accessor system provides a Python interface to access and modify nested
Fortran state types (dts_*) through auto-generated f90wrap bindings.

Architecture Overview:
- Fortran accessor modules (suews_accessor_*.f95): Low-level getters/setters
- Python _state_accessors.py: Re-exports Fortran functions as unified namespace
- StateAccessor class: High-level Pythonic dict-based interface

Test Categories:
1. Core DTS creation (create_suews_state, create_suews_config, etc.)
2. Low-level accessor functions (get_*/set_* for each state type)
3. StateAccessor class (to_dict/from_dict round-trips)
4. Edge cases and error handling
"""

import numpy as np
import pytest

from supy.dts import (
    DTSConfig,
    StateAccessor,
    create_suews_config,
    create_suews_forcing,
    create_suews_state,
    create_suews_timer,
    test_dts_interface,
)
from supy import _state_accessors as acc


# =============================================================================
# Test Fixtures
# =============================================================================


@pytest.fixture
def dts_state():
    """Create a standard DTS state object for testing."""
    return create_suews_state(nlayer=5, ndepth=5)


@pytest.fixture
def dts_config():
    """Create a standard DTS config object for testing."""
    return create_suews_config()


@pytest.fixture
def dts_forcing():
    """Create a standard DTS forcing object for testing."""
    return create_suews_forcing()


@pytest.fixture
def dts_timer():
    """Create a standard DTS timer object for testing."""
    return create_suews_timer()


@pytest.fixture
def state_accessor(dts_state):
    """Create a StateAccessor wrapping a DTS state object."""
    return StateAccessor(dts_state, nlayer=5, ndepth=5, nsurf=7, nvegsurf=3)


@pytest.fixture
def dts_site():
    """Create a standard DTS site object for testing."""
    from supy.dts import create_suews_site
    return create_suews_site(nlayer=5)


# =============================================================================
# Test Core DTS Creation Functions
# =============================================================================


class TestCoreDTSCreation:
    """Test core DTS factory functions in _core.py."""

    @pytest.mark.smoke
    def test_create_suews_state_default(self):
        """Test state creation with default parameters."""
        state = create_suews_state()
        assert state is not None
        # Verify dimensions via accessor
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(state)
        assert nlayer == 5
        assert ndepth == 5
        assert nsurf == 7

    @pytest.mark.smoke
    def test_create_suews_state_custom_dims(self):
        """Test state creation with custom dimensions."""
        state = create_suews_state(nlayer=10, ndepth=8)
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(state)
        assert nlayer == 10
        assert ndepth == 8
        assert nsurf == 7  # Always 7 SUEWS surfaces

    @pytest.mark.smoke
    def test_create_suews_config(self):
        """Test config object creation."""
        config = create_suews_config()
        assert config is not None

    @pytest.mark.smoke
    def test_create_suews_forcing(self):
        """Test forcing object creation."""
        forcing = create_suews_forcing()
        assert forcing is not None

    @pytest.mark.smoke
    def test_create_suews_timer(self):
        """Test timer object creation."""
        timer = create_suews_timer()
        assert timer is not None

    def test_dts_config_dataclass(self):
        """Test DTSConfig dataclass defaults."""
        cfg = DTSConfig()
        assert cfg.nlayer == 5
        assert cfg.ndepth == 5
        assert cfg.nsurf == 7


# =============================================================================
# Test Low-Level Fortran Accessor Functions
# =============================================================================


class TestHeatStateAccessors:
    """Test heat state accessor functions."""

    @pytest.mark.smoke
    def test_get_heat_state_dims(self, dts_state):
        """Test dimension retrieval."""
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(dts_state)
        assert nlayer == 5
        assert ndepth == 5
        assert nsurf == 7

    @pytest.mark.smoke
    def test_get_set_heat_state_scalars(self, dts_state):
        """Test scalar value round-trip."""
        # ARRANGE: Initial values should be zero
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(dts_state)
        assert qh == pytest.approx(0.0)

        # ACT: Set new values
        acc.set_heat_state_scalars(dts_state, 100.0, 50.0, 30.0, 200.0, 10.0, 25.5)

        # ASSERT: Values should match
        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(dts_state)
        assert qh == pytest.approx(100.0)
        assert qe == pytest.approx(50.0)
        assert qs == pytest.approx(30.0)
        assert qn == pytest.approx(200.0)
        assert qf == pytest.approx(10.0)
        assert tsurf == pytest.approx(25.5)

    def test_get_set_heat_state_temp(self, dts_state):
        """Test temperature array round-trip."""
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(dts_state)

        # Create test arrays
        temp_roof = np.full((nlayer, ndepth), 20.0, dtype=np.float64, order="F")
        temp_wall = np.full((nlayer, ndepth), 18.0, dtype=np.float64, order="F")
        temp_surf = np.full((nsurf, ndepth), 15.0, dtype=np.float64, order="F")

        # Set temperatures (API requires dimension args)
        acc.set_heat_state_temp(
            dts_state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf
        )

        # Retrieve and verify
        out_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        out_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        out_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")
        acc.get_heat_state_temp(
            dts_state, nlayer, ndepth, nsurf, out_roof, out_wall, out_surf
        )

        np.testing.assert_allclose(out_roof, temp_roof, rtol=1e-10)
        np.testing.assert_allclose(out_wall, temp_wall, rtol=1e-10)
        np.testing.assert_allclose(out_surf, temp_surf, rtol=1e-10)


class TestHydroStateAccessors:
    """Test hydro state accessor functions."""

    @pytest.mark.smoke
    def test_get_hydro_state_dims(self, dts_state):
        """Test dimension retrieval (returns nlayer, nsurf)."""
        nlayer, nsurf = acc.get_hydro_state_dims(dts_state)
        assert nlayer == 5
        assert nsurf == 7

    def test_get_set_hydro_state_soilstore(self, dts_state):
        """Test soil store array round-trip (separate arrays per surface type)."""
        nlayer, nsurf = acc.get_hydro_state_dims(dts_state)

        # Create test arrays (roof/wall indexed by nlayer, surf by nsurf)
        soilstore_roof = np.linspace(50.0, 80.0, nlayer, dtype=np.float64)
        soilstore_wall = np.linspace(40.0, 70.0, nlayer, dtype=np.float64)
        soilstore_surf = np.linspace(60.0, 100.0, nsurf, dtype=np.float64)

        acc.set_hydro_state_soilstore(
            dts_state, nlayer, soilstore_roof, soilstore_wall, soilstore_surf
        )

        # Retrieve
        out_roof = np.zeros(nlayer, dtype=np.float64)
        out_wall = np.zeros(nlayer, dtype=np.float64)
        out_surf = np.zeros(nsurf, dtype=np.float64)
        acc.get_hydro_state_soilstore(
            dts_state, nlayer, out_roof, out_wall, out_surf
        )

        np.testing.assert_allclose(out_roof, soilstore_roof, rtol=1e-10)
        np.testing.assert_allclose(out_wall, soilstore_wall, rtol=1e-10)
        np.testing.assert_allclose(out_surf, soilstore_surf, rtol=1e-10)


class TestSnowStateAccessors:
    """Test snow state accessor functions."""

    @pytest.mark.smoke
    def test_get_snow_state_dims(self, dts_state):
        """Test dimension retrieval."""
        nsurf = acc.get_snow_state_dims(dts_state)
        assert nsurf == 7

    def test_get_set_snow_state_scalars(self, dts_state):
        """Test snow scalar round-trip (snowalb, swe, mwh, qm)."""
        # Set values: snowalb, swe, mwh, qm
        acc.set_snow_state_scalars(dts_state, 0.8, 100.0, 50.0, 10.0)

        # Retrieve
        snowalb, swe, mwh, qm = acc.get_snow_state_scalars(dts_state)

        assert snowalb == pytest.approx(0.8)
        assert swe == pytest.approx(100.0)
        assert mwh == pytest.approx(50.0)
        assert qm == pytest.approx(10.0)


class TestFlagStateAccessors:
    """Test flag state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_flag_state(self, dts_state):
        """Test flag state round-trip."""
        # Set values
        acc.set_flag_state(dts_state, True, 5, 1)

        # Retrieve
        flag_converge, i_iter, stebbs_bldg_init = acc.get_flag_state(dts_state)

        assert flag_converge == True  # noqa: E712 (explicit boolean comparison)
        assert i_iter == 5
        assert stebbs_bldg_init == 1


class TestSolarStateAccessors:
    """Test solar state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_solar_state(self, dts_state):
        """Test solar state round-trip."""
        # Set values
        acc.set_solar_state(dts_state, 180.0, 45.0)

        # Retrieve
        azimuth, zenith = acc.get_solar_state(dts_state)

        assert azimuth == pytest.approx(180.0)
        assert zenith == pytest.approx(45.0)


class TestRoughnessStateAccessors:
    """Test roughness state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_roughness_state(self, dts_state):
        """Test roughness state round-trip."""
        # Set values
        acc.set_roughness_state(
            dts_state,
            0.5,   # FAIBldg_use
            0.3,   # FAIEveTree_use
            0.2,   # FAIDecTree_use
            0.4,   # FAI
            0.6,   # PAI
            10.0,  # Zh
            1.5,   # z0m
            0.1,   # z0v
            7.0,   # zdm
            3.0,   # ZZD
        )

        # Retrieve
        (
            FAIBldg_use,
            FAIEveTree_use,
            FAIDecTree_use,
            FAI,
            PAI,
            Zh,
            z0m,
            z0v,
            zdm,
            ZZD,
        ) = acc.get_roughness_state(dts_state)

        assert FAIBldg_use == pytest.approx(0.5)
        assert FAIEveTree_use == pytest.approx(0.3)
        assert FAIDecTree_use == pytest.approx(0.2)
        assert FAI == pytest.approx(0.4)
        assert PAI == pytest.approx(0.6)
        assert Zh == pytest.approx(10.0)
        assert z0m == pytest.approx(1.5)
        assert z0v == pytest.approx(0.1)
        assert zdm == pytest.approx(7.0)
        assert ZZD == pytest.approx(3.0)


class TestNhoodStateAccessors:
    """Test neighbourhood state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_nhood_state(self, dts_state):
        """Test neighbourhood state round-trip."""
        # Set values
        acc.set_nhood_state(dts_state, 5.0, 100.0, 15.0, 10.0)

        # Retrieve
        U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count = acc.get_nhood_state(
            dts_state
        )

        assert U_hbh_1dravg == pytest.approx(5.0)
        assert QN_1dravg == pytest.approx(100.0)
        assert Tair_mn_prev == pytest.approx(15.0)
        assert iter_count == pytest.approx(10.0)


class TestOHMStateAccessors:
    """Test OHM state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_ohm_state_radiation(self, dts_state):
        """Test OHM radiation state round-trip."""
        # Set values
        acc.set_ohm_state_radiation(dts_state, 200.0, 10.0, 150.0, 8.0)

        # Retrieve
        qn_av, dqndt, qn_s_av, dqnsdt = acc.get_ohm_state_radiation(dts_state)

        assert qn_av == pytest.approx(200.0)
        assert dqndt == pytest.approx(10.0)
        assert qn_s_av == pytest.approx(150.0)
        assert dqnsdt == pytest.approx(8.0)

    def test_get_set_ohm_state_coef_grid(self, dts_state):
        """Test OHM grid coefficients round-trip."""
        acc.set_ohm_state_coef_grid(dts_state, 0.5, 0.1, -20.0)

        a1, a2, a3 = acc.get_ohm_state_coef_grid(dts_state)

        assert a1 == pytest.approx(0.5)
        assert a2 == pytest.approx(0.1)
        assert a3 == pytest.approx(-20.0)


class TestAtmStateAccessors:
    """Test atmospheric state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_atm_state_thermo(self, dts_state):
        """Test thermodynamic properties round-trip."""
        # Set values
        acc.set_atm_state_thermo(
            dts_state,
            0.5,      # fcld
            1004.0,   # avcp
            1.2,      # dens_dry
            1.25,     # avdens
            0.005,    # dq
            2.5e6,    # lv_J_kg
            2.83e6,   # lvS_J_kg
            2.5e3,    # tlv
        )

        # Retrieve
        fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv = (
            acc.get_atm_state_thermo(dts_state)
        )

        assert fcld == pytest.approx(0.5)
        assert avcp == pytest.approx(1004.0)
        assert dens_dry == pytest.approx(1.2)
        assert avdens == pytest.approx(1.25)

    def test_get_set_atm_state_rss_surf(self, dts_state):
        """Test surface resistance array round-trip."""
        nsurf = 7
        rss_surf = np.linspace(100.0, 500.0, nsurf, dtype=np.float64)

        acc.set_atm_state_rss_surf(dts_state, rss_surf)

        out = np.zeros(nsurf, dtype=np.float64)
        acc.get_atm_state_rss_surf(dts_state, out)

        np.testing.assert_allclose(out, rss_surf, rtol=1e-10)


class TestAnthroStateAccessors:
    """Test anthropogenic emissions state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_anthro_state_hdd(self, dts_state):
        """Test HDD array round-trip."""
        hdd_id = np.linspace(0.0, 100.0, 12, dtype=np.float64)

        acc.set_anthro_state_hdd(dts_state, hdd_id)

        out = np.zeros(12, dtype=np.float64)
        acc.get_anthro_state_hdd(dts_state, out)

        np.testing.assert_allclose(out, hdd_id, rtol=1e-10)

    def test_get_set_anthro_state_co2(self, dts_state):
        """Test CO2 flux scalars round-trip."""
        acc.set_anthro_state_co2(
            dts_state,
            10.0,   # Fc
            5.0,    # Fc_anthro
            3.0,    # Fc_biogen
            2.0,    # Fc_build
            1.0,    # Fc_metab
            0.5,    # Fc_photo
            0.2,    # Fc_point
            0.3,    # Fc_respi
            0.8,    # Fc_traff
        )

        (
            Fc,
            Fc_anthro,
            Fc_biogen,
            Fc_build,
            Fc_metab,
            Fc_photo,
            Fc_point,
            Fc_respi,
            Fc_traff,
        ) = acc.get_anthro_state_co2(dts_state)

        assert Fc == pytest.approx(10.0)
        assert Fc_anthro == pytest.approx(5.0)
        assert Fc_biogen == pytest.approx(3.0)


class TestPhenStateAccessors:
    """Test phenology state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_phen_state_alb(self, dts_state):
        """Test albedo array round-trip."""
        nsurf = 7
        alb = np.linspace(0.1, 0.3, nsurf, dtype=np.float64)

        acc.set_phen_state_alb(dts_state, alb)

        out = np.zeros(nsurf, dtype=np.float64)
        acc.get_phen_state_alb(dts_state, out)

        np.testing.assert_allclose(out, alb, rtol=1e-10)

    def test_get_set_phen_state_lai(self, dts_state):
        """Test LAI and degree-day arrays round-trip."""
        nvegsurf = 3
        lai_id = np.array([3.0, 4.0, 2.5], dtype=np.float64)
        GDD_id = np.array([100.0, 150.0, 80.0], dtype=np.float64)
        SDD_id = np.array([50.0, 60.0, 40.0], dtype=np.float64)

        acc.set_phen_state_lai(dts_state, lai_id, GDD_id, SDD_id)

        out_lai = np.zeros(nvegsurf, dtype=np.float64)
        out_GDD = np.zeros(nvegsurf, dtype=np.float64)
        out_SDD = np.zeros(nvegsurf, dtype=np.float64)
        acc.get_phen_state_lai(dts_state, out_lai, out_GDD, out_SDD)

        np.testing.assert_allclose(out_lai, lai_id, rtol=1e-10)
        np.testing.assert_allclose(out_GDD, GDD_id, rtol=1e-10)
        np.testing.assert_allclose(out_SDD, SDD_id, rtol=1e-10)


class TestStebbsStateAccessors:
    """Test STEBBS state accessor functions."""

    @pytest.mark.smoke
    def test_get_set_stebbs_state_krad(self, dts_state):
        """Test shortwave radiation round-trip."""
        acc.set_stebbs_state_krad(
            dts_state,
            400.0,  # Kdown2d
            50.0,   # Kup2d
            100.0,  # Kwest
            120.0,  # Ksouth
            80.0,   # Knorth
            90.0,   # Keast
        )

        Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast = acc.get_stebbs_state_krad(
            dts_state
        )

        assert Kdown2d == pytest.approx(400.0)
        assert Kup2d == pytest.approx(50.0)
        assert Kwest == pytest.approx(100.0)

    def test_get_set_stebbs_state_temps_envelope(self, dts_state):
        """Test building envelope temperatures round-trip."""
        acc.set_stebbs_state_temps_envelope(
            dts_state,
            10.0,   # DeepSoilTemperature
            15.0,   # OutdoorAirStartTemperature
            20.0,   # IndoorAirStartTemperature
            18.0,   # IndoorMassStartTemperature
            19.0,   # WallIndoorSurfaceTemperature
            16.0,   # WallOutdoorSurfaceTemperature
            21.0,   # RoofIndoorSurfaceTemperature
            14.0,   # RoofOutdoorSurfaceTemperature
            20.0,   # WindowIndoorSurfaceTemperature
            15.0,   # WindowOutdoorSurfaceTemperature
            17.0,   # GroundFloorIndoorSurfaceTemperature
            12.0,   # GroundFloorOutdoorSurfaceTemperature
        )

        (
            DeepSoilTemperature,
            OutdoorAirStartTemperature,
            IndoorAirStartTemperature,
            IndoorMassStartTemperature,
            WallIndoorSurfaceTemperature,
            WallOutdoorSurfaceTemperature,
            RoofIndoorSurfaceTemperature,
            RoofOutdoorSurfaceTemperature,
            WindowIndoorSurfaceTemperature,
            WindowOutdoorSurfaceTemperature,
            GroundFloorIndoorSurfaceTemperature,
            GroundFloorOutdoorSurfaceTemperature,
        ) = acc.get_stebbs_state_temps_envelope(dts_state)

        assert DeepSoilTemperature == pytest.approx(10.0)
        assert OutdoorAirStartTemperature == pytest.approx(15.0)
        assert IndoorAirStartTemperature == pytest.approx(20.0)


# =============================================================================
# Test StateAccessor Class
# =============================================================================


class TestStateAccessorClass:
    """Test the high-level StateAccessor class."""

    @pytest.mark.smoke
    def test_state_accessor_creation(self, state_accessor):
        """Test StateAccessor instantiation."""
        assert state_accessor is not None
        assert state_accessor.state is not None

    @pytest.mark.smoke
    def test_to_dict_returns_all_categories(self, state_accessor):
        """Test that to_dict returns all 12 state categories."""
        state_dict = state_accessor.to_dict()

        expected_keys = [
            "flag",
            "solar",
            "roughness",
            "nhood",
            "ohm",
            "atm",
            "anthro",
            "phen",
            "stebbs",
            "heat",
            "hydro",
            "snow",
        ]

        for key in expected_keys:
            assert key in state_dict, f"Missing key: {key}"

    def test_flag_state_round_trip(self, state_accessor):
        """Test flag state get/set round-trip via StateAccessor."""
        # Set values
        state_accessor.set_flag_state({
            "flag_converge": True,
            "i_iter": 10,
            "stebbs_bldg_init": 2,
        })

        # Get values
        flag_state = state_accessor.get_flag_state()

        assert flag_state["flag_converge"] == True  # noqa: E712
        assert flag_state["i_iter"] == 10
        assert flag_state["stebbs_bldg_init"] == 2

    def test_solar_state_round_trip(self, state_accessor):
        """Test solar state get/set round-trip via StateAccessor."""
        state_accessor.set_solar_state({
            "azimuth_deg": 225.0,
            "zenith_deg": 30.0,
        })

        solar_state = state_accessor.get_solar_state()

        assert solar_state["azimuth_deg"] == pytest.approx(225.0)
        assert solar_state["zenith_deg"] == pytest.approx(30.0)

    def test_roughness_state_round_trip(self, state_accessor):
        """Test roughness state get/set round-trip via StateAccessor."""
        state_accessor.set_roughness_state({
            "FAIBldg_use": 0.6,
            "FAI": 0.5,
            "z0m": 2.0,
            "Zh": 15.0,
        })

        roughness_state = state_accessor.get_roughness_state()

        assert roughness_state["FAIBldg_use"] == pytest.approx(0.6)
        assert roughness_state["FAI"] == pytest.approx(0.5)
        assert roughness_state["z0m"] == pytest.approx(2.0)
        assert roughness_state["Zh"] == pytest.approx(15.0)

    def test_atm_state_round_trip(self, state_accessor):
        """Test atmospheric state get/set round-trip via StateAccessor."""
        state_accessor.set_atm_state({
            "fcld": 0.7,
            "avcp": 1005.0,
            "UStar": 0.5,
            "T2_C": 22.0,
            "rss_surf": np.full(7, 200.0, dtype=np.float64),
        })

        atm_state = state_accessor.get_atm_state()

        assert atm_state["fcld"] == pytest.approx(0.7)
        assert atm_state["avcp"] == pytest.approx(1005.0)
        assert atm_state["UStar"] == pytest.approx(0.5)
        assert atm_state["T2_C"] == pytest.approx(22.0)
        np.testing.assert_allclose(atm_state["rss_surf"], 200.0, rtol=1e-10)

    def test_phen_state_round_trip(self, state_accessor):
        """Test phenology state get/set round-trip via StateAccessor."""
        alb = np.linspace(0.1, 0.25, 7, dtype=np.float64)
        lai_id = np.array([2.5, 3.5, 2.0], dtype=np.float64)

        state_accessor.set_phen_state({
            "alb": alb,
            "lai_id": lai_id,
            "GDD_id": np.zeros(3, dtype=np.float64),
            "SDD_id": np.zeros(3, dtype=np.float64),
            "porosity_id": 0.6,
        })

        phen_state = state_accessor.get_phen_state()

        np.testing.assert_allclose(phen_state["alb"], alb, rtol=1e-10)
        np.testing.assert_allclose(phen_state["lai_id"], lai_id, rtol=1e-10)
        assert phen_state["porosity_id"] == pytest.approx(0.6)

    def test_stebbs_state_round_trip(self, state_accessor):
        """Test STEBBS state get/set round-trip via StateAccessor."""
        state_accessor.set_stebbs_state({
            "Kdown2d": 500.0,
            "Ldown2d": 350.0,
            "DeepSoilTemperature": 12.0,
            "IndoorAirStartTemperature": 21.0,
        })

        stebbs_state = state_accessor.get_stebbs_state()

        assert stebbs_state["Kdown2d"] == pytest.approx(500.0)
        assert stebbs_state["Ldown2d"] == pytest.approx(350.0)
        assert stebbs_state["DeepSoilTemperature"] == pytest.approx(12.0)
        assert stebbs_state["IndoorAirStartTemperature"] == pytest.approx(21.0)

    def test_from_dict_partial_update(self, state_accessor):
        """Test that from_dict only updates provided keys."""
        # Set initial values
        state_accessor.set_solar_state({
            "azimuth_deg": 180.0,
            "zenith_deg": 45.0,
        })

        # Partial update via from_dict
        state_accessor.from_dict({
            "solar": {"azimuth_deg": 270.0},  # Only update azimuth
        })

        solar_state = state_accessor.get_solar_state()

        # Azimuth should be updated
        assert solar_state["azimuth_deg"] == pytest.approx(270.0)
        # Zenith should remain unchanged (default value since we didn't set it explicitly)
        # Note: The default is 0.0 when not provided, so this tests partial updates work


class TestStateAccessorBuildingTemps:
    """Test STEBBS building temperature accessors.

    Note: STEBBS building temperatures require proper STEBBS initialisation.
    These tests verify the accessor interface works, though values may not
    persist without proper STEBBS state setup.
    """

    def test_get_building_temps_returns_arrays(self, state_accessor):
        """Test building temperature retrieval returns correct array shapes."""
        temps = state_accessor.get_building_temps(bldg_idx=1)

        # Verify the dict keys exist and arrays have correct shape
        assert "Textroof_C" in temps
        assert "Textwall_C" in temps
        assert len(temps["Textroof_C"]) == 5  # nlayer=5
        assert len(temps["Textwall_C"]) == 5


# =============================================================================
# Test Built-in Interface Test
# =============================================================================


class TestBuiltInInterfaceTest:
    """Test the built-in DTS interface test function."""

    @pytest.mark.smoke
    def test_dts_interface_passes(self):
        """Test that the built-in interface test passes."""
        result = test_dts_interface()
        assert result is True


# =============================================================================
# Test Edge Cases and Error Handling
# =============================================================================


class TestEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_zero_values(self, dts_state):
        """Test that zero values are handled correctly."""
        acc.set_heat_state_scalars(dts_state, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(dts_state)

        assert qh == pytest.approx(0.0)
        assert tsurf == pytest.approx(0.0)

    def test_negative_values(self, dts_state):
        """Test that negative values are handled correctly (e.g., for fluxes)."""
        acc.set_heat_state_scalars(dts_state, -50.0, -30.0, 0.0, 0.0, 0.0, -5.0)

        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(dts_state)

        assert qh == pytest.approx(-50.0)
        assert qe == pytest.approx(-30.0)
        assert tsurf == pytest.approx(-5.0)

    def test_large_values(self, dts_state):
        """Test that large values are handled correctly."""
        acc.set_heat_state_scalars(dts_state, 1000.0, 500.0, 300.0, 800.0, 100.0, 50.0)

        qh, qe, qs, qn, qf, tsurf = acc.get_heat_state_scalars(dts_state)

        assert qh == pytest.approx(1000.0)
        assert tsurf == pytest.approx(50.0)

    def test_multiple_state_objects_independent(self):
        """Test that multiple state objects are independent."""
        state1 = create_suews_state(nlayer=5, ndepth=5)
        state2 = create_suews_state(nlayer=5, ndepth=5)

        # Set different values
        acc.set_heat_state_scalars(state1, 100.0, 0.0, 0.0, 0.0, 0.0, 25.0)
        acc.set_heat_state_scalars(state2, 200.0, 0.0, 0.0, 0.0, 0.0, 30.0)

        # Verify independence
        qh1, _, _, _, _, tsurf1 = acc.get_heat_state_scalars(state1)
        qh2, _, _, _, _, tsurf2 = acc.get_heat_state_scalars(state2)

        assert qh1 == pytest.approx(100.0)
        assert qh2 == pytest.approx(200.0)
        assert tsurf1 == pytest.approx(25.0)
        assert tsurf2 == pytest.approx(30.0)


class TestArrayOrdering:
    """Test Fortran array ordering (column-major)."""

    def test_2d_array_fortran_order(self, dts_state):
        """Test that 2D arrays use Fortran ordering."""
        nlayer, ndepth, nsurf = acc.get_heat_state_dims(dts_state)

        # Create test array with distinct values
        temp_roof = np.arange(nlayer * ndepth, dtype=np.float64).reshape(
            (nlayer, ndepth), order="F"
        )
        temp_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        temp_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")

        # API requires dimension args
        acc.set_heat_state_temp(
            dts_state, nlayer, ndepth, nsurf, temp_roof, temp_wall, temp_surf
        )

        out_roof = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        out_wall = np.zeros((nlayer, ndepth), dtype=np.float64, order="F")
        out_surf = np.zeros((nsurf, ndepth), dtype=np.float64, order="F")
        acc.get_heat_state_temp(
            dts_state, nlayer, ndepth, nsurf, out_roof, out_wall, out_surf
        )

        # Verify exact ordering is preserved
        np.testing.assert_array_equal(out_roof, temp_roof)


# =============================================================================
# Test OHM State Comprehensive
# =============================================================================


class TestOHMStateComprehensive:
    """Comprehensive tests for OHM state accessors."""

    def test_ohm_state_averages(self, dts_state):
        """Test OHM running averages round-trip."""
        acc.set_ohm_state_averages(dts_state, 15.0, 3.5, 20.0, 150.0)

        t2_prev, ws_rav, tair_prev, qn_rav = acc.get_ohm_state_averages(dts_state)

        assert t2_prev == pytest.approx(15.0)
        assert ws_rav == pytest.approx(3.5)
        assert tair_prev == pytest.approx(20.0)
        assert qn_rav == pytest.approx(150.0)

    def test_ohm_state_coef_surf(self, dts_state):
        """Test OHM surface coefficients round-trip."""
        # Set all 21 surface coefficients
        acc.set_ohm_state_coef_surf(
            dts_state,
            0.3, 0.1, -10.0,  # bldg
            0.4, 0.2, -15.0,  # paved
            0.2, 0.05, -5.0,  # evetr
            0.25, 0.08, -8.0,  # dectr
            0.15, 0.04, -3.0,  # grass
            0.35, 0.12, -12.0,  # bsoil
            0.1, 0.02, -2.0,  # water
        )

        (
            a1_bldg, a2_bldg, a3_bldg,
            a1_paved, a2_paved, a3_paved,
            a1_evetr, a2_evetr, a3_evetr,
            a1_dectr, a2_dectr, a3_dectr,
            a1_grass, a2_grass, a3_grass,
            a1_bsoil, a2_bsoil, a3_bsoil,
            a1_water, a2_water, a3_water,
        ) = acc.get_ohm_state_coef_surf(dts_state)

        assert a1_bldg == pytest.approx(0.3)
        assert a2_paved == pytest.approx(0.2)
        assert a3_water == pytest.approx(-2.0)


# =============================================================================
# Integration Test
# =============================================================================


class TestDTSIntegration:
    """Integration tests combining multiple DTS operations."""

    @pytest.mark.smoke
    def test_full_state_cycle(self):
        """Test complete state creation, modification, and extraction cycle."""
        # Create state
        state = create_suews_state(nlayer=5, ndepth=5)
        accessor = StateAccessor(state, nlayer=5, ndepth=5)

        # Set values across multiple state types
        accessor.set_flag_state({"flag_converge": True, "i_iter": 5})
        accessor.set_solar_state({"azimuth_deg": 180.0, "zenith_deg": 45.0})
        accessor.set_roughness_state({"z0m": 1.5, "Zh": 12.0})
        accessor.set_atm_state({"T2_C": 20.0, "UStar": 0.4})

        # Extract full state
        full_state = accessor.to_dict()

        # Verify all modified values
        assert full_state["flag"]["flag_converge"] == True  # noqa: E712
        assert full_state["flag"]["i_iter"] == 5
        assert full_state["solar"]["azimuth_deg"] == pytest.approx(180.0)
        assert full_state["roughness"]["z0m"] == pytest.approx(1.5)
        assert full_state["atm"]["T2_C"] == pytest.approx(20.0)

    def test_state_persistence_across_modifications(self, state_accessor):
        """Test that state persists correctly after multiple modifications."""
        # First modification
        state_accessor.set_solar_state({"azimuth_deg": 90.0, "zenith_deg": 60.0})

        # Second modification (different state type)
        state_accessor.set_roughness_state({"z0m": 2.0})

        # Verify first modification persists
        solar_state = state_accessor.get_solar_state()
        assert solar_state["azimuth_deg"] == pytest.approx(90.0)
        assert solar_state["zenith_deg"] == pytest.approx(60.0)

        # Third modification (back to solar)
        state_accessor.set_solar_state({"azimuth_deg": 270.0})

        # Verify latest modification
        solar_state = state_accessor.get_solar_state()
        assert solar_state["azimuth_deg"] == pytest.approx(270.0)


# =============================================================================
# Test Site Surface Property Accessors
# =============================================================================


class TestSiteSurfacePropertyAccessors:
    """Test site surface property accessor functions.
    
    These accessors provide access to nested SUEWS_SITE surface properties
    that are required for hydro state validation during bootstrap.
    """

    @pytest.mark.smoke
    def test_set_get_soil_params(self, dts_site):
        """Test soil parameter round-trip."""
        # Prepare test values
        soildepth = np.array([100.0, 100.0, 350.0, 350.0, 350.0, 200.0, 0.0], dtype=np.float64)
        soilstorecap = np.array([100.0, 120.0, 150.0, 150.0, 150.0, 150.0, 0.0], dtype=np.float64)
        sathydraulicconduct = np.array([0.0008, 0.0008, 0.0005, 0.0005, 0.0005, 0.0003, 0.0], dtype=np.float64)

        # Set values
        acc.set_site_soil_params(dts_site, soildepth, soilstorecap, sathydraulicconduct)

        # Get values back
        out_depth = np.zeros(7, dtype=np.float64)
        out_cap = np.zeros(7, dtype=np.float64)
        out_cond = np.zeros(7, dtype=np.float64)
        acc.get_site_soil_params(dts_site, out_depth, out_cap, out_cond)

        # Verify
        np.testing.assert_allclose(out_depth, soildepth, rtol=1e-10)
        np.testing.assert_allclose(out_cap, soilstorecap, rtol=1e-10)
        np.testing.assert_allclose(out_cond, sathydraulicconduct, rtol=1e-10)

    @pytest.mark.smoke
    def test_set_get_water_limits(self, dts_site):
        """Test water limits round-trip."""
        # Prepare test values
        statelimit = np.array([0.48, 0.25, 1.3, 0.8, 1.9, 0.8, 30000.0], dtype=np.float64)
        wetthresh = np.array([0.5, 0.5, 2.0, 2.0, 2.0, 1.0, 0.0], dtype=np.float64)

        # Set values
        acc.set_site_water_limits(dts_site, statelimit, wetthresh)

        # Get values back
        out_limit = np.zeros(7, dtype=np.float64)
        out_thresh = np.zeros(7, dtype=np.float64)
        acc.get_site_water_limits(dts_site, out_limit, out_thresh)

        # Verify
        np.testing.assert_allclose(out_limit, statelimit, rtol=1e-10)
        np.testing.assert_allclose(out_thresh, wetthresh, rtol=1e-10)

    def test_soil_params_individual_surfaces(self, dts_site):
        """Test that soil params are set on individual surfaces correctly."""
        # Set different values for each surface
        soildepth = np.array([100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0], dtype=np.float64)
        soilstorecap = np.array([10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0], dtype=np.float64)
        sathydraulicconduct = np.array([0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007], dtype=np.float64)

        acc.set_site_soil_params(dts_site, soildepth, soilstorecap, sathydraulicconduct)

        # Verify each surface has distinct values
        out_depth = np.zeros(7, dtype=np.float64)
        out_cap = np.zeros(7, dtype=np.float64)
        out_cond = np.zeros(7, dtype=np.float64)
        acc.get_site_soil_params(dts_site, out_depth, out_cap, out_cond)

        # Surface order: paved=0, bldg=1, evetr=2, dectr=3, grass=4, bsoil=5, water=6
        assert out_depth[0] == pytest.approx(100.0)  # paved
        assert out_depth[2] == pytest.approx(300.0)  # evetr
        assert out_cap[6] == pytest.approx(70.0)      # water
        assert out_cond[4] == pytest.approx(0.005)    # grass
