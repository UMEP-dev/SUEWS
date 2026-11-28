"""End-to-end native engine witness for scheme-gated STEBBS validation (#2363, #2348).

Verifies that when STEBBS is disabled (``stebbs.enabled == False`` or legacy
``stebbsmethod == 0``), relaxed, null, or placeholder STEBBS subordinate
switches, ``properties.stebbs``, and ``properties.building_archetype`` blocks
load, pass ``validate_for_engine()``, and execute through ``SUEWSSimulation.run()``
with bit-identical output to the unmodified baseline configuration.
"""

from __future__ import annotations

import copy
import warnings

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model.core import SUEWSConfig

pytestmark = [
    pytest.mark.native,
    pytest.mark.cfg,
    pytest.mark.core,
    pytest.mark.contract("C-VALID", "C-EMIT", "C-RESPONSE"),
]


def _run_variant(raw_config: dict, df_forcing: pd.DataFrame) -> pd.DataFrame:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        cfg = SUEWSConfig.from_dict(raw_config)
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    return sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df


def test_inactive_stebbs_relaxed_config_matches_baseline_bit_identically(
    short_sample_sim,
) -> None:
    """Relaxed/null/placeholder inactive STEBBS configs run bit-identically to baseline."""
    control_sim = short_sample_sim()
    df_forcing = control_sim.forcing.df.iloc[:12].copy()
    control_sim.update_forcing(df_forcing)
    baseline_out = control_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    base_dict = control_sim.config.model_dump(mode="json", round_trip=True)

    # Case 1: Nested disabled STEBBS with placeholder switches & relaxed properties/archetype.
    # Keep temperature_air_annual_mean unchanged here because yaml_config.rs still reads it
    # to initialise deep_soil_temperature for the diagnostic Ts_*_dyohm columns even when
    # STEBBS is off; all pure-STEBBS parameters and building_archetype fields are relaxed.
    relaxed_nested = copy.deepcopy(base_dict)
    relaxed_nested["model"]["physics"]["stebbs"] = {
        "enabled": False,
        "capacitance": {"value": "TBD"},
        "setpoint": None,
        "same_albedo_wall": "enabled",
        "same_albedo_roof": -999,
        "same_emissivity_wall": {"value": None},
        "same_emissivity_roof": "TBD",
    }
    relaxed_nested["sites"][0]["properties"]["stebbs"].update(
        depth_ground=-999.0,
        internal_shading=3,
        destination_waste_heat=-1,
        fraction_convective_heating=-0.5,
        convection_coefficient_wall_internal="TBD",
        volume_hot_water={"value": None},
    )
    relaxed_nested["sites"][0]["properties"]["building_archetype"].update(
        thickness_wall=-1.0,
        ratio_window_to_wall=2.0,
        occupants="TBD",
        hhs0={"value": None},
    )
    nested_out = _run_variant(relaxed_nested, df_forcing)
    pd.testing.assert_frame_equal(nested_out, baseline_out, check_exact=True)

    # Case 2: Null STEBBS & building_archetype blocks under flat legacy stebbsmethod=0.
    # When properties.stebbs is absent (or temperature_air_annual_mean is non-numeric),
    # deep_soil_temperature falls back to the Fortran/Rust default (0.0 degC), which affects
    # only the diagnostic Ts_*_dyohm columns; every other column remains bit-identical.
    relaxed_flat = copy.deepcopy(base_dict)
    relaxed_flat["model"]["physics"].pop("stebbs", None)
    relaxed_flat["model"]["physics"].update(
        stebbsmethod=0,
        rcmethod="TBD",
        setpointmethod=None,
    )
    relaxed_flat["sites"][0]["properties"]["stebbs"] = None
    relaxed_flat["sites"][0]["properties"]["building_archetype"] = None
    flat_out = _run_variant(relaxed_flat, df_forcing)
    non_dyohm_cols = [
        col for col in baseline_out.columns if not str(col[1]).endswith("_dyohm")
    ]
    pd.testing.assert_frame_equal(
        flat_out[non_dyohm_cols],
        baseline_out[non_dyohm_cols],
        check_exact=True,
    )

    # Also verify placeholder temperature_air_annual_mean runs cleanly and matches the
    # zero-deep-soil-temperature run (flat_out) bit-identically across all columns.
    relaxed_placeholder_tair = copy.deepcopy(relaxed_nested)
    relaxed_placeholder_tair["sites"][0]["properties"]["stebbs"][
        "temperature_air_annual_mean"
    ] = "TBD"
    placeholder_tair_out = _run_variant(relaxed_placeholder_tair, df_forcing)
    pd.testing.assert_frame_equal(placeholder_tair_out, flat_out, check_exact=True)
