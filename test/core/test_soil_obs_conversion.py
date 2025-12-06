import numpy as np
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
    assert pd.isna(df_result["xsmd"].iloc[1])  # NaN should remain NaN
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
