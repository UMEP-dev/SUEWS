import sys
from pathlib import Path
import importlib.util

import pandas as pd
import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = REPO_ROOT / "src" / "supy" / "_soil_obs.py"
spec = importlib.util.spec_from_file_location("_soil_obs", MODULE_PATH)
soil_obs = importlib.util.module_from_spec(spec)
assert spec.loader is not None
sys.modules[spec.name] = soil_obs
spec.loader.exec_module(soil_obs)
convert_observed_soil_moisture = soil_obs.convert_observed_soil_moisture


def _make_state(
    smdmethod: int,
    *,  # keyword-only for clarity
    obs_depths,
    obs_caps,
    obs_soil_not_rocks,
    soil_densities,
    sfr=None,
):
    surfaces = range(2)  # keep tests compact by using two surfaces
    if sfr is None:
        sfr = {0: 0.5, 1: 0.5}

    columns = [
        ("smdmethod", "0"),
    ]
    data = [smdmethod]

    for surf in surfaces:
        columns.append(("sfr_surf", f"({surf},)"))
        data.append(sfr.get(surf, 0.0))

        columns.append(("obs_sm_depth", f"({surf},)"))
        data.append(obs_depths.get(surf))

        columns.append(("obs_sm_cap", f"({surf},)"))
        data.append(obs_caps.get(surf))

        columns.append(("obs_soil_not_rocks", f"({surf},)"))
        data.append(obs_soil_not_rocks.get(surf))

        columns.append(("soildensity", f"({surf},)"))
        data.append(soil_densities.get(surf))

    multi_cols = pd.MultiIndex.from_tuples(columns)
    df_state = pd.DataFrame(
        [data], columns=multi_cols, index=pd.Index([1], name="grid")
    )
    return df_state


def test_convert_observed_soil_moisture_volumetric():
    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: 200.0, 1: 400.0},
        obs_caps={0: 0.4, 1: 0.4},
        obs_soil_not_rocks={0: 0.8, 1: 0.8},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, 0.35, -999.0]},
        index=pd.date_range("2024-07-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Weighted depth = (0.5*200 + 0.5*400) = 300 mm
    # Soil fraction (no rocks) = 0.8
    # => factor = 240
    expected = [36.0, 12.0, -999.0]
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == expected


def test_convert_observed_soil_moisture_gravimetric():
    df_state = _make_state(
        smdmethod=2,
        obs_depths={0: 300.0, 1: 300.0},
        obs_caps={0: 0.5, 1: 0.5},
        obs_soil_not_rocks={0: 0.9, 1: 0.9},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.3, 0.45]},
        index=pd.date_range("2024-08-01", periods=2, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # deficit = (smcap - xsmd) * soil_density * depth * soil_not_rocks
    # = (0.5 - 0.3) * 1.2 * 300 * 0.9 = 64.8
    # second value: (0.5 - 0.45) * 1.2 * 300 * 0.9 = 16.2
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [64.8, 16.2]


def test_missing_metadata_raises_error():
    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: None, 1: None},
        obs_caps={0: 0.4, 1: 0.4},
        obs_soil_not_rocks={0: 0.8, 1: 0.8},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25]},
        index=pd.date_range("2024-09-01", periods=1, freq="h"),
    )

    with pytest.raises(ValueError, match="obs_sm_depth"):
        convert_observed_soil_moisture(df_forcing, df_state)


def test_xsmd_with_nan_values():
    """Test that NaN values in xsmd are preserved during conversion."""
    import numpy as np

    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: 300.0, 1: 300.0},
        obs_caps={0: 0.4, 1: 0.4},
        obs_soil_not_rocks={0: 0.8, 1: 0.8},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.25, np.nan, 0.35, -999.0]},
        index=pd.date_range("2024-10-01", periods=4, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # Expected: (0.4 - 0.25) * 300 * 0.8 = 36.0
    # Expected: (0.4 - 0.35) * 300 * 0.8 = 12.0
    assert pytest.approx(df_result["xsmd"].iloc[0], rel=1e-6) == 36.0
    assert pd.isna(df_result["xsmd"].iloc[1])  # NaN should remain NaN
    assert pytest.approx(df_result["xsmd"].iloc[2], rel=1e-6) == 12.0
    assert pytest.approx(df_result["xsmd"].iloc[3], rel=1e-6) == -999.0  # Missing value preserved


def test_xsmd_exceeding_smcap():
    """Test that values exceeding smcap are clipped correctly."""
    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: 200.0, 1: 200.0},
        obs_caps={0: 0.4, 1: 0.4},
        obs_soil_not_rocks={0: 1.0, 1: 1.0},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [0.5, 0.6, 0.4]},  # First two exceed smcap=0.4
        index=pd.date_range("2024-11-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # All values should be clipped to smcap before conversion
    # deficit = (0.4 - clipped_value) * 200 * 1.0
    # For 0.5 and 0.6: clipped to 0.4 => deficit = 0
    # For 0.4: deficit = 0
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [0.0, 0.0, 0.0]


def test_xsmd_negative_values():
    """Test that negative values are clipped to zero."""
    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: 100.0, 1: 100.0},
        obs_caps={0: 0.3, 1: 0.3},
        obs_soil_not_rocks={0: 1.0, 1: 1.0},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [-0.1, 0.0, 0.1]},
        index=pd.date_range("2024-12-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # -0.1 clipped to 0 => deficit = (0.3 - 0.0) * 100 * 1.0 = 30.0
    # 0.0 => deficit = (0.3 - 0.0) * 100 * 1.0 = 30.0
    # 0.1 => deficit = (0.3 - 0.1) * 100 * 1.0 = 20.0
    assert pytest.approx(df_result["xsmd"].tolist(), rel=1e-6) == [30.0, 30.0, 20.0]


def test_xsmd_all_missing():
    """Test that when all xsmd values are missing, the series is returned unchanged."""
    df_state = _make_state(
        smdmethod=1,
        obs_depths={0: 200.0, 1: 200.0},
        obs_caps={0: 0.4, 1: 0.4},
        obs_soil_not_rocks={0: 0.8, 1: 0.8},
        soil_densities={0: 1.2, 1: 1.2},
    )
    df_forcing = pd.DataFrame(
        {"xsmd": [-999.0, -999.0, -999.0]},
        index=pd.date_range("2024-01-01", periods=3, freq="h"),
    )

    df_result = convert_observed_soil_moisture(df_forcing, df_state)

    # All missing values should be preserved
    assert df_result["xsmd"].tolist() == [-999.0, -999.0, -999.0]
