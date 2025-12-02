import pandas as pd
import pytest

from supy.util._forcing import convert_observed_soil_moisture


def _make_site_level_state(
    smdmethod: int,
    *,  # keyword-only for clarity
    depth: float,
    smcap: float,
    soil_not_rocks: float,
    bulk_density: float,
):
    """Create a df_state with site-level soil observation config (new approach).

    Site-level config uses columns with index "0" instead of surface indices.
    This is the preferred approach for YAML-based configurations.
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


def _make_state(
    smdmethod: int,
    *,  # keyword-only for clarity
    obs_depth: float,
    obs_cap: float,
    obs_soil_not_rocks: float,
    soil_density: float,
    surface_index: int = 0,
):
    """Create a minimal df_state with soil observation metadata on a single surface.

    The implementation searches through surfaces 0-5 and uses the first one
    with complete metadata. By default we set it on surface 0.
    """
    surf = surface_index

    columns = [
        ("smdmethod", "0"),
        ("obs_sm_depth", f"({surf},)"),
        ("obs_sm_cap", f"({surf},)"),
        ("obs_soil_not_rocks", f"({surf},)"),
        ("soildensity", f"({surf},)"),
    ]
    data = [smdmethod, obs_depth, obs_cap, obs_soil_not_rocks, soil_density]

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    return df_state


def test_convert_observed_soil_moisture_volumetric():
    df_state = _make_state(
        smdmethod=1,
        obs_depth=200.0,
        obs_cap=0.4,
        obs_soil_not_rocks=0.8,
        soil_density=1.2,
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
    df_state = _make_state(
        smdmethod=2,
        obs_depth=300.0,
        obs_cap=0.5,
        obs_soil_not_rocks=0.9,
        soil_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.3, 0.45]},
        index=pd.date_range("2024-08-01", periods=2, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Gravimetric deficit = (smcap - xsmd) * soil_density * depth * soil_not_rocks
    # = (0.5 - 0.3) * 1.2 * 300 * 0.9 = 64.8
    # = (0.5 - 0.45) * 1.2 * 300 * 0.9 = 16.2
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [64.8, 16.2]


def test_missing_metadata_raises_error():
    """Test that missing metadata raises a clear error."""
    # Only provide depth on surface 0, other required fields missing
    columns = [
        ("smdmethod", "0"),
        ("obs_sm_depth", "(0,)"),  # Only depth provided, others missing
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

    with pytest.raises(ValueError, match="obs_sm_cap"):
        convert_observed_soil_moisture(df_forcing, df_state)


def test_xsmd_with_nan_values():
    """Test that NaN values in xsmd are preserved during conversion."""
    import numpy as np

    df_state = _make_state(
        smdmethod=1,
        obs_depth=300.0,
        obs_cap=0.4,
        obs_soil_not_rocks=0.8,
        soil_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, np.nan, 0.35, -999.0]},
        index=pd.date_range("2024-10-01", periods=4, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # deficit = (0.4 - 0.25) * 300 * 0.8 = 36.0
    # deficit = (0.4 - 0.35) * 300 * 0.8 = 12.0
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 36.0
    assert pd.isna(df_result["xsmd"].iloc[1])  # NaN should remain NaN
    assert pytest.approx(df_result["xsmd"].iloc[2], rel=1e-6) == 12.0
    assert (
        pytest.approx(df_result["xsmd"].iloc[3], rel=1e-6) == -999.0
    )  # Missing value preserved


def test_xsmd_exceeding_smcap():
    """Test that values exceeding smcap are clipped correctly."""
    df_state = _make_state(
        smdmethod=1,
        obs_depth=200.0,
        obs_cap=0.4,
        obs_soil_not_rocks=1.0,
        soil_density=1.2,
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
    df_state = _make_state(
        smdmethod=1,
        obs_depth=100.0,
        obs_cap=0.3,
        obs_soil_not_rocks=1.0,
        soil_density=1.2,
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
    df_state = _make_state(
        smdmethod=1,
        obs_depth=200.0,
        obs_cap=0.4,
        obs_soil_not_rocks=0.8,
        soil_density=1.2,
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [-999.0, -999.0, -999.0]},
        index=pd.date_range("2024-01-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # All missing values should be preserved
    assert df_result["xsmd"].tolist() == [-999.0, -999.0, -999.0]


def test_metadata_from_non_zero_surface():
    """Test that metadata can be set on any surface (e.g., grass=4, bare soil=5)."""
    # Set metadata on surface 4 (grass) instead of default surface 0
    df_state = _make_state(
        smdmethod=1,
        obs_depth=200.0,
        obs_cap=0.4,
        obs_soil_not_rocks=0.8,
        soil_density=1.2,
        surface_index=4,  # Grass surface
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25]},
        index=pd.date_range("2024-07-01", periods=1, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Same formula as volumetric: (0.4 - 0.25) * 200 * 0.8 = 24.0
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 24.0


def test_metadata_from_bare_soil_surface():
    """Test that metadata set on bare soil (surface 5) works correctly."""
    df_state = _make_state(
        smdmethod=2,
        obs_depth=300.0,
        obs_cap=0.5,
        obs_soil_not_rocks=0.9,
        soil_density=1.2,
        surface_index=5,  # Bare soil surface
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.3]},
        index=pd.date_range("2024-08-01", periods=1, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Gravimetric: (0.5 - 0.3) * 1.2 * 300 * 0.9 = 64.8
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 64.8


# =============================================================================
# Site-level configuration tests (new approach)
# =============================================================================


def test_site_level_config_volumetric():
    """Test conversion using site-level config (new YAML approach)."""
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


def test_site_level_config_gravimetric():
    """Test gravimetric conversion using site-level config."""
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


def test_site_level_takes_precedence_over_per_surface():
    """Test that site-level config takes precedence over per-surface config."""
    # Create a state with BOTH site-level and per-surface config
    # Site-level should be used
    columns = [
        ("smdmethod", "0"),
        # Site-level config (should be used)
        ("obs_sm_depth", "0"),
        ("obs_sm_smcap", "0"),
        ("obs_sm_soil_not_rocks", "0"),
        ("obs_sm_bulk_density", "0"),
        # Per-surface config (should be ignored)
        ("obs_sm_depth", "(0,)"),
        ("obs_sm_cap", "(0,)"),
        ("obs_soil_not_rocks", "(0,)"),
        ("soildensity", "(0,)"),
    ]
    # Site-level: depth=200, smcap=0.4, soil_not_rocks=0.8, bulk_density=1.0
    # Per-surface: depth=100, smcap=0.2, soil_not_rocks=1.0, bulk_density=2.0
    data = [1, 200.0, 0.4, 0.8, 1.0, 100.0, 0.2, 1.0, 2.0]

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25]},
        index=pd.date_range("2024-07-01", periods=1, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Should use site-level: (0.4 - 0.25) * 200 * 0.8 = 24.0
    # If per-surface were used: (0.2 - 0.2) * 100 * 1.0 = 0.0 (clipped to smcap)
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 24.0


def test_fallback_to_per_surface_when_site_level_incomplete():
    """Test fallback to per-surface when site-level config is incomplete."""
    # Create state with incomplete site-level but complete per-surface
    columns = [
        ("smdmethod", "0"),
        # Incomplete site-level (missing bulk_density)
        ("obs_sm_depth", "0"),
        ("obs_sm_smcap", "0"),
        ("obs_sm_soil_not_rocks", "0"),
        # obs_sm_bulk_density intentionally missing
        # Complete per-surface config on surface 0
        ("obs_sm_depth", "(0,)"),
        ("obs_sm_cap", "(0,)"),
        ("obs_soil_not_rocks", "(0,)"),
        ("soildensity", "(0,)"),
    ]
    data = [1, 200.0, 0.4, 0.8, 100.0, 0.3, 1.0, 1.5]

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.2]},
        index=pd.date_range("2024-07-01", periods=1, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Should fall back to per-surface: (0.3 - 0.2) * 100 * 1.0 = 10.0
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 10.0
