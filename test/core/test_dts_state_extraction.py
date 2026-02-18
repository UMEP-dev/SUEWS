"""Tests for DTS state extraction functionality.

This module tests the extraction of final state from Fortran DTS objects
to Pydantic InitialStates models, enabling continuation runs with the
DTS backend.

Note: These tests require a full build with DTS type wrappers (make dev-dts).
      They are automatically skipped when running with a fast build (make dev).
"""

import pytest

from supy import SUEWSSimulation, load_SampleData
from supy.data_model import SUEWSConfig
from supy.data_model.core.state import InitialStates
from supy.dts import _DTS_AVAILABLE, run_dts

# Skip all tests in this module if DTS is not available (fast build)
pytestmark = pytest.mark.skipif(
    not _DTS_AVAILABLE, reason="DTS not available (fast build without type wrappers)"
)

# Tolerance constants for numerical comparisons
FLUX_TOLERANCE_W_M2 = 0.1  # Acceptable difference for energy fluxes [W/m²]
EXACT_MATCH_TOLERANCE = 1e-6  # Tolerance for values that should match exactly


def _get_value(obj):
    """Unwrap RefValue wrapper to get raw value.

    Parameters
    ----------
    obj : Any
        Object that may be wrapped in RefValue.

    Returns
    -------
    Any
        The unwrapped value.
    """
    return obj.value if hasattr(obj, "value") else obj


class TestDTSStateExtraction:
    """Tests for DTS state extraction functionality."""

    @pytest.fixture
    def sample_data(self):
        """Load sample data using SuPy's built-in sample loader."""
        df_state_init, df_forcing = load_SampleData()
        grid_id = df_state_init.index.get_level_values("grid")[0]
        config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])
        # Use first 48 timesteps (4 hours) for quick tests
        df_forcing_subset = df_forcing.iloc[:48]
        return config, df_forcing_subset, df_state_init

    @pytest.mark.core
    def test_initial_states_available_after_dts_run(self, sample_data):
        """Test that initial_states is returned after DTS run."""
        config, df_forcing, _ = sample_data
        df_output, final_state = run_dts(
            df_forcing=df_forcing,
            config=config,
        )

        # Check that initial_states is in the final_state dict
        assert "initial_states" in final_state
        assert final_state["initial_states"] is not None

        # Check that it's an InitialStates instance
        initial_states = final_state["initial_states"]
        assert isinstance(initial_states, InitialStates)

    @pytest.mark.core
    def test_simulation_property_available(self, sample_data):
        """Test that SUEWSSimulation.initial_states_final property works."""
        config, df_forcing, _ = sample_data

        # Create simulation from config
        sim = SUEWSSimulation(config)
        sim.update_forcing(df_forcing)

        # Run with DTS backend
        sim.run(backend="dts")

        # Check property is available
        assert sim.initial_states_final is not None
        assert isinstance(sim.initial_states_final, InitialStates)

    @pytest.mark.core
    def test_state_extraction_values_physically_reasonable(self, sample_data):
        """Test that extracted state values are physically reasonable."""
        config, df_forcing, _ = sample_data
        df_output, final_state = run_dts(
            df_forcing=df_forcing,
            config=config,
        )

        initial_states = final_state["initial_states"]

        # Check surface states are non-negative (water state)
        for surface_name in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"]:
            surface = getattr(initial_states, surface_name)
            state_val = _get_value(surface.state)
            assert state_val >= 0, (
                f"{surface_name} state should be non-negative, got {state_val}"
            )

        # Check soil moisture is positive
        for surface_name in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"]:
            surface = getattr(initial_states, surface_name)
            soilstore_val = _get_value(surface.soilstore)
            assert soilstore_val >= 0, (
                f"{surface_name} soilstore should be positive, got {soilstore_val}"
            )

        # Check LAI is reasonable (0-10 m²/m²)
        for veg_name in ["evetr", "dectr", "grass"]:
            veg = getattr(initial_states, veg_name)
            lai_val = _get_value(veg.lai_id)
            assert 0 <= lai_val <= 15, f"{veg_name} LAI should be 0-15"

        # Check snow albedo is valid (0-1)
        snowalb_val = _get_value(initial_states.snowalb)
        assert 0 <= snowalb_val <= 1, "Snow albedo should be 0-1"

        # Check HDD_ID exists and has reasonable structure
        assert initial_states.hdd_id is not None

    @pytest.mark.core
    def test_state_extraction_temperatures_reasonable(self, sample_data):
        """Test that extracted temperature profiles are physically reasonable."""
        config, df_forcing, _ = sample_data
        df_output, final_state = run_dts(
            df_forcing=df_forcing,
            config=config,
        )

        initial_states = final_state["initial_states"]

        # Check surface temperatures are reasonable (-50 to +70 °C)
        for surface_name in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"]:
            surface = getattr(initial_states, surface_name)

            # Surface temperature
            if surface.tsfc is not None:
                tsfc_val = _get_value(surface.tsfc)
                assert -50 <= tsfc_val <= 70, f"{surface_name} tsfc out of range"

            # Temperature profile
            temp_vals = _get_value(surface.temperature)
            for i, t in enumerate(temp_vals):
                assert -50 <= t <= 70, f"{surface_name} temp[{i}] out of range"

    @pytest.mark.core
    def test_roof_wall_states_extracted(self, sample_data):
        """Test that roof and wall states are extracted."""
        config, df_forcing, _ = sample_data
        df_output, final_state = run_dts(
            df_forcing=df_forcing,
            config=config,
        )

        initial_states = final_state["initial_states"]

        # Check roofs and walls exist
        assert initial_states.roofs is not None
        assert initial_states.walls is not None
        assert len(initial_states.roofs) > 0
        assert len(initial_states.walls) > 0

        # Check roof/wall temperatures are reasonable
        for i, roof in enumerate(initial_states.roofs):
            if roof.tsfc is not None:
                tsfc_val = _get_value(roof.tsfc)
                assert -50 <= tsfc_val <= 70, f"roof[{i}] tsfc out of range"

    @pytest.mark.core
    def test_hdd_id_extraction(self, sample_data):
        """Test that HDD_ID (heating degree days) is properly extracted."""
        config, df_forcing, _ = sample_data
        df_output, final_state = run_dts(
            df_forcing=df_forcing,
            config=config,
        )

        initial_states = final_state["initial_states"]
        hdd = initial_states.hdd_id

        # Check HDD_ID has expected fields
        assert hasattr(hdd, "hdd_accum")
        assert hasattr(hdd, "cdd_accum")
        assert hasattr(hdd, "temp_accum")
        assert hasattr(hdd, "days_since_rain")

        # Check to_list method works (used for Fortran population)
        hdd_list = hdd.to_list()
        assert len(hdd_list) == 12
        assert all(isinstance(x, (int, float)) for x in hdd_list)

    @pytest.mark.core
    def test_traditional_backend_has_no_initial_states(self, sample_data):
        """Test that traditional backend doesn't set initial_states_final."""
        config, df_forcing, df_state_init = sample_data

        sim = SUEWSSimulation(config)
        sim.update_forcing(df_forcing)

        # Run with traditional backend
        sim.run(backend="traditional")

        # Check property is None for traditional backend
        assert sim.initial_states_final is None

        # But state_final (DataFrame) should be available
        assert sim.state_final is not None

    @pytest.mark.core
    def test_continuation_run_parity(self):
        """Test that continuation runs produce results within acceptable tolerance.

        This test verifies that running 96 timesteps in one continuous run
        produces nearly identical results to running 48 + 48 timesteps with
        state extraction and continuation.

        Acceptable tolerance: 0.1 W/m² for energy fluxes (QH, QE, QS).
        This is well below typical measurement uncertainty (5-10 W/m²).
        """
        import numpy as np

        # Load full forcing data (not the truncated fixture)
        df_state_init, df_forcing_full = load_SampleData()
        grid_id = df_state_init.index.get_level_values("grid")[0]
        config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])

        # Use 96 timesteps (8 hours at 5-min resolution)
        df_forcing_96 = df_forcing_full.iloc[:96]

        # Full continuous run
        df_output_full, _ = run_dts(
            df_forcing=df_forcing_96,
            config=config,
        )

        # Split run: first 48 timesteps
        df_forcing_48a = df_forcing_full.iloc[:48]
        df_forcing_48b = df_forcing_full.iloc[48:96]

        df_output_a, final_state_a = run_dts(
            df_forcing=df_forcing_48a,
            config=config,
        )

        # Use extracted state for continuation
        initial_states_cont = final_state_a["initial_states"]
        site = config.sites[0] if hasattr(config, "sites") else config.site
        site.initial_states = initial_states_cont

        # Run continuation
        df_output_b, _ = run_dts(
            df_forcing=df_forcing_48b,
            config=config,
        )

        # Compare second half of full run with continuation run
        df_full_second_half = df_output_full.iloc[48:]

        # Check energy balance variables are within tolerance
        for var in ["QN", "QF", "QH", "QE", "QS"]:
            full_vals = df_full_second_half[("SUEWS", var)].values.flatten()
            cont_vals = df_output_b[("SUEWS", var)].values.flatten()
            max_diff = np.max(np.abs(full_vals - cont_vals))

            assert max_diff < FLUX_TOLERANCE_W_M2, (
                f"{var} max difference {max_diff:.4f} W/m² exceeds tolerance {FLUX_TOLERANCE_W_M2}"
            )

        # Verify QN and QF match exactly (they depend only on forcing/params)
        qn_diff = np.max(
            np.abs(
                df_full_second_half[("SUEWS", "QN")].values
                - df_output_b[("SUEWS", "QN")].values
            )
        )
        assert qn_diff < EXACT_MATCH_TOLERANCE, (
            f"QN should match exactly but differs by {qn_diff}"
        )

        qf_diff = np.max(
            np.abs(
                df_full_second_half[("SUEWS", "QF")].values
                - df_output_b[("SUEWS", "QF")].values
            )
        )
        assert qf_diff < EXACT_MATCH_TOLERANCE, (
            f"QF should match exactly but differs by {qf_diff}"
        )


class TestChunkedDTS:
    """Tests for chunked DTS simulation."""

    @pytest.mark.core
    def test_chunked_dts_parity(self):
        """Test that chunked run matches non-chunked run within tolerance.

        Runs 96 timesteps both non-chunked and with chunk_day=1, then
        asserts that QN, QF, QH, QE, QS are within FLUX_TOLERANCE_W_M2.
        """
        import numpy as np

        df_state_init, df_forcing_full = load_SampleData()
        grid_id = df_state_init.index.get_level_values("grid")[0]
        config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])

        df_forcing_96 = df_forcing_full.iloc[:96]

        # Non-chunked (default chunk_day is large enough for 96 steps)
        df_output_nochunk, _ = run_dts(
            df_forcing=df_forcing_96,
            config=config,
        )

        # Chunked: chunk_day=1 forces splitting at daily boundaries
        df_output_chunked, _ = run_dts(
            df_forcing=df_forcing_96,
            config=config,
            chunk_day=1,
        )

        assert len(df_output_nochunk) == len(df_output_chunked), (
            "Chunked and non-chunked outputs should have same length"
        )

        for var in ["QN", "QF", "QH", "QE", "QS"]:
            nochunk_vals = df_output_nochunk[("SUEWS", var)].values.flatten()
            chunked_vals = df_output_chunked[("SUEWS", var)].values.flatten()
            max_diff = np.max(np.abs(nochunk_vals - chunked_vals))
            assert max_diff < FLUX_TOLERANCE_W_M2, (
                f"{var} max difference {max_diff:.4f} W/m2 exceeds "
                f"tolerance {FLUX_TOLERANCE_W_M2}"
            )

    @pytest.mark.core
    def test_simulation_run_accepts_chunk_day(self):
        """Test that SUEWSSimulation.run(backend='dts', chunk_day=1) works."""
        df_state_init, df_forcing_full = load_SampleData()
        grid_id = df_state_init.index.get_level_values("grid")[0]
        config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])

        sim = SUEWSSimulation(config)
        sim.update_forcing(df_forcing_full.iloc[:96])

        output = sim.run(backend="dts", chunk_day=1)
        assert output is not None
        assert len(output.df) == 96
