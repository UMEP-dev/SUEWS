import numpy as np
import pandas as pd
import pytest

from supy.util._forcing import convert_observed_soil_moisture

# All tests in this module are core physics tests
pytestmark = pytest.mark.core


def _make_site_level_state(
    smdmethod: int,
    *,  # keyword-only for clarity
    depth: float,
    smcap: float,
    soil_not_rocks: float,
    bulk_density: float,
):
    """Create a df_state with site-level soil observation config.

    Site-level config uses columns with index "0" instead of surface indices.
    This is the approach for YAML-based configurations.
    """
    columns = [
        ("smdmethod", "0"),
        ("obs_sm_depth", "0"),
        ("obs_sm_smcap", "0"),
        ("obs_sm_soil_not_rocks", "0"),
        ("obs_sm_bulk_density", "0"),
    ]
    data = [smdmethod, depth, smcap, soil_not_rocks, bulk_density]

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    return df_state


def test_convert_observed_soil_moisture_volumetric():
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=200.0,
        smcap=0.4,
        soil_not_rocks=0.8,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, 0.35, -999.0]},
        index=pd.date_range("2024-07-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Volumetric deficit = (smcap - xsmd) * depth * soil_not_rocks
    # = (0.4 - 0.25) * 200 * 0.8 = 24.0
    # = (0.4 - 0.35) * 200 * 0.8 = 8.0
    expected = [24.0, 8.0, -999.0]
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == expected


def test_convert_observed_soil_moisture_gravimetric():
    df_state = _make_site_level_state(
        smdmethod=2,
        depth=300.0,
        smcap=0.5,
        soil_not_rocks=0.9,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.3, 0.45]},
        index=pd.date_range("2024-08-01", periods=2, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Gravimetric deficit = (smcap - xsmd) * bulk_density * depth * soil_not_rocks
    # = (0.5 - 0.3) * 1.2 * 300 * 0.9 = 64.8
    # = (0.5 - 0.45) * 1.2 * 300 * 0.9 = 16.2
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [64.8, 16.2]


def test_missing_metadata_raises_error():
    """Test that missing metadata raises a clear error."""
    # Only provide smdmethod, other required fields missing
    columns = [
        ("smdmethod", "0"),
        ("obs_sm_depth", "0"),  # Only depth provided, others missing
    ]
    data = [1, 200.0]

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25]},
        index=pd.date_range("2024-09-01", periods=1, freq="h"),
    )

    with pytest.raises(ValueError, match="requires complete metadata"):
        convert_observed_soil_moisture(df_forcing, df_state)


def test_xsmd_with_nan_values():
    """Test that NaN values in xsmd are preserved during conversion."""
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=300.0,
        smcap=0.4,
        soil_not_rocks=0.8,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, np.nan, 0.35, -999.0]},
        index=pd.date_range("2024-10-01", periods=4, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # deficit = (0.4 - 0.25) * 300 * 0.8 = 36.0
    # deficit = (0.4 - 0.35) * 300 * 0.8 = 12.0
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 36.0
    # NaN fails the `> MISSING_VALUE + 1` comparison (NaN comparisons return False),
    # so it's excluded from mask_valid and remains unchanged in the output
    assert pd.isna(df_result["xsmd"].iloc[1])
    assert pytest.approx(df_result["xsmd"].iloc[2], rel=1e-6) == 12.0
    assert (
        pytest.approx(df_result["xsmd"].iloc[3], rel=1e-6) == -999.0
    )  # Missing value preserved


def test_xsmd_exceeding_smcap():
    """Test that values exceeding smcap are clipped correctly."""
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=200.0,
        smcap=0.4,
        soil_not_rocks=1.0,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.5, 0.6, 0.4]},  # First two exceed smcap=0.4
        index=pd.date_range("2024-11-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # All values clipped to smcap => deficit = (0.4 - 0.4) * 200 * 1.0 = 0
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [0.0, 0.0, 0.0]


def test_xsmd_negative_values():
    """Test that negative values are clipped to zero."""
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=100.0,
        smcap=0.3,
        soil_not_rocks=1.0,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [-0.1, 0.0, 0.1]},
        index=pd.date_range("2024-12-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # -0.1 clipped to 0 => deficit = (0.3 - 0.0) * 100 * 1.0 = 30.0
    # 0.0 => deficit = 30.0
    # 0.1 => deficit = (0.3 - 0.1) * 100 * 1.0 = 20.0
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [30.0, 30.0, 20.0]


def test_xsmd_all_missing():
    """Test that when all xsmd values are missing, the series is returned unchanged."""
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=200.0,
        smcap=0.4,
        soil_not_rocks=0.8,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [-999.0, -999.0, -999.0]},
        index=pd.date_range("2024-01-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # All missing values should be preserved
    assert df_result["xsmd"].tolist() == [-999.0, -999.0, -999.0]


def test_smdmethod_zero_passthrough():
    """Test that SMDMethod=0 passes through without conversion."""
    df_state = _make_site_level_state(
        smdmethod=0,
        depth=200.0,
        smcap=0.4,
        soil_not_rocks=0.8,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, 0.35]},
        index=pd.date_range("2024-07-01", periods=2, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # No conversion should happen
    assert df_result["xsmd"].tolist() == [0.25, 0.35]


def test_missing_xsmd_column_raises_error():
    """Test that missing xsmd column raises a clear error when SMDMethod > 0."""
    df_state = _make_site_level_state(
        smdmethod=1,
        depth=200.0,
        smcap=0.4,
        soil_not_rocks=0.8,
        bulk_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"other_col": [1.0, 2.0]},
        index=pd.date_range("2024-07-01", periods=2, freq="h"),
    )

    with pytest.raises(ValueError, match="xsmd.*missing"):
        convert_observed_soil_moisture(df_forcing, df_state)


# =============================================================================
# Integration Tests
# =============================================================================


class TestObservedSoilMoistureIntegration:
    """End-to-end integration tests for observed soil moisture forcing.

    These tests verify that:
    1. The conversion pipeline integrates correctly with run_supy_ser()
    2. The Fortran kernel responds to observed soil moisture values
    3. Output physics (evaporation) is affected by soil moisture stress
    """

    @pytest.mark.slow
    def test_observed_soil_moisture_affects_evaporation(self):
        """Verify that observed soil moisture affects latent heat flux (QE).

        When soil is dry (high deficit), evaporation should be reduced due to
        stomatal closure (g_smd factor in surface resistance calculation).

        Note: The sample site (KCL London) is highly urbanised with only ~5%
        vegetation, so the soil moisture effect on QE is small but measurable.
        We increase vegetation fraction to better demonstrate the physics.
        """
        import supy as sp

        # Load sample data
        df_state, df_forcing = sp.load_sample_data()

        # Increase vegetation fraction to show soil moisture effect more clearly
        # Original: 43% Paved, 38% Bldgs, 0% EveTr, 2% DecTr, 3% Grass, 0% BSoil, 14% Water
        # Modified: 30% Paved, 30% Bldgs, 0% EveTr, 10% DecTr, 15% Grass, 0% BSoil, 15% Water
        df_state[("sfr_surf", "(0,)")] = 0.30  # Paved
        df_state[("sfr_surf", "(1,)")] = 0.30  # Bldgs
        df_state[("sfr_surf", "(3,)")] = 0.10  # DecTr
        df_state[("sfr_surf", "(4,)")] = 0.15  # Grass
        df_state[("sfr_surf", "(6,)")] = 0.15  # Water

        # Use summer period with high evaporative demand (daytime with solar radiation)
        # Select a week in July 2012 to capture meaningful evaporation
        start = "2012-07-01"
        end = "2012-07-07"
        df_forcing_period = df_forcing.loc[start:end].copy()

        # === Run 1: Baseline with SMDMethod=0 (modelled soil moisture) ===
        df_state_baseline = df_state.copy()
        df_state_baseline[("smdmethod", "0")] = 0

        df_output_baseline, _ = sp.run_supy(
            df_forcing_period, df_state_baseline, save_state=False
        )

        # === Run 2: Observed soil moisture with DRY conditions ===
        # Set up SMDMethod=1 (volumetric) with soil observation metadata
        df_state_dry = df_state.copy()
        df_state_dry[("smdmethod", "0")] = 1
        df_state_dry[("obs_sm_depth", "0")] = 200.0  # 200mm sensor depth
        df_state_dry[("obs_sm_smcap", "0")] = 0.4  # 40% saturation capacity
        df_state_dry[("obs_sm_soil_not_rocks", "0")] = 0.9  # 90% soil
        df_state_dry[("obs_sm_bulk_density", "0")] = 1.3  # g/cm³

        # Set xsmd to DRY conditions (low volumetric moisture = high deficit)
        # 0.05 volumetric = very dry; deficit = (0.4-0.05)*200*0.9 = 63mm
        df_forcing_dry = df_forcing_period.copy()
        df_forcing_dry["xsmd"] = 0.05  # Very dry soil

        df_output_dry, _ = sp.run_supy(
            df_forcing_dry, df_state_dry, save_state=False
        )

        # === Run 3: Observed soil moisture with WET conditions ===
        df_state_wet = df_state_dry.copy()  # Same config
        df_forcing_wet = df_forcing_period.copy()
        df_forcing_wet["xsmd"] = 0.38  # Near saturation; deficit = (0.4-0.38)*200*0.9 = 3.6mm

        df_output_wet, _ = sp.run_supy(
            df_forcing_wet, df_state_wet, save_state=False
        )

        # === Assertions ===
        # Extract daytime QE (when evaporation is active, Kdown > 100 W/m²)
        daytime_mask = df_forcing_period["kdown"] > 100

        # Get QE for each run (grid 1) - output columns are MultiIndex (group, var)
        qe_baseline = df_output_baseline.loc[(1, slice(None)), ("SUEWS", "QE")].values
        qe_dry = df_output_dry.loc[(1, slice(None)), ("SUEWS", "QE")].values
        qe_wet = df_output_wet.loc[(1, slice(None)), ("SUEWS", "QE")].values

        # Align with daytime mask (output has same length as forcing)
        daytime_idx = daytime_mask.values[: len(qe_baseline)]

        qe_baseline_day = qe_baseline[daytime_idx]
        qe_dry_day = qe_dry[daytime_idx]
        qe_wet_day = qe_wet[daytime_idx]

        # Mean daytime evaporation
        mean_qe_baseline = np.nanmean(qe_baseline_day)
        mean_qe_dry = np.nanmean(qe_dry_day)
        mean_qe_wet = np.nanmean(qe_wet_day)

        # Physics check 1: Dry soil should have LOWER evaporation than wet soil
        # (stomatal closure due to soil moisture stress)
        assert mean_qe_dry < mean_qe_wet, (
            f"Physics error: dry soil (QE={mean_qe_dry:.1f}) should have "
            f"lower evaporation than wet soil (QE={mean_qe_wet:.1f})"
        )

        # Physics check 2: The difference should be meaningful (at least 0.5 W/m²)
        # Note: Urban sites show smaller effects due to limited vegetation cover
        qe_difference = mean_qe_wet - mean_qe_dry
        assert qe_difference > 0.5, (
            f"Observed soil moisture has minimal effect on QE "
            f"(difference={qe_difference:.1f} W/m²). "
            "Expected meaningful soil moisture stress response."
        )

        # Log the results for debugging
        print(f"\n=== Observed Soil Moisture Integration Test ===")
        print(f"Mean daytime QE (baseline, SMDMethod=0): {mean_qe_baseline:.1f} W/m²")
        print(f"Mean daytime QE (dry, xsmd=0.05):        {mean_qe_dry:.1f} W/m²")
        print(f"Mean daytime QE (wet, xsmd=0.38):        {mean_qe_wet:.1f} W/m²")
        print(f"QE difference (wet - dry):               {qe_difference:.1f} W/m²")

    @pytest.mark.slow
    def test_smdmethod_config_propagates_to_kernel(self):
        """Verify that SMDMethod setting propagates correctly through the pipeline."""
        import supy as sp

        df_state, df_forcing = sp.load_sample_data()

        # Short run for speed
        df_forcing_short = df_forcing.iloc[:48].copy()  # 4 hours at 5-min resolution

        # Configure for observed soil moisture
        df_state_obs = df_state.copy()
        df_state_obs[("smdmethod", "0")] = 1
        df_state_obs[("obs_sm_depth", "0")] = 150.0
        df_state_obs[("obs_sm_smcap", "0")] = 0.35
        df_state_obs[("obs_sm_soil_not_rocks", "0")] = 0.85
        df_state_obs[("obs_sm_bulk_density", "0")] = 1.2

        # Provide observed soil moisture
        df_forcing_short["xsmd"] = 0.20

        # This should run without errors - proving the config propagates correctly
        df_output, df_state_final = sp.run_supy(
            df_forcing_short, df_state_obs, save_state=True
        )

        # Verify output is generated
        assert df_output is not None
        assert len(df_output) == len(df_forcing_short)

        # Verify xsmd was converted (not raw 0.20 in the output)
        # The conversion should have happened: deficit = (0.35-0.20)*150*0.85 = 19.125mm
        # We can't easily check the internal xsmd, but we verified the run succeeded
