"""Regression tests for SPARTACUS roof/wall facet parameter isolation (gh#2085).

``BuildingLayer`` defines ``roof_albedo_dir_mult_fact`` as roof-only and
``wall_specular_frac`` as wall-only, but both ``RoofLayer`` and ``WallLayer``
inherit and serialise both fields into engine YAML. These tests verify that
the Rust YAML bridge routes each field exclusively to its owning facet so
``walls[i]`` cannot clobber ``roofs[i].roof_albedo_dir_mult_fact`` and
``roofs[i]`` cannot populate ``wall_specular_frac``.
"""

from __future__ import annotations

import copy
from importlib.resources import as_file, files

import numpy as np
import pytest
import yaml

import supy
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-BIND", "C-RESPONSE", "C-STRICT"),
]


@pytest.fixture(scope="module")
def spartacus_summer_day():
    """Load the bundled sample config and one summer day of 5-min forcing."""
    with as_file(files("supy") / "sample_data") as sample_dir:
        with (sample_dir / "sample_config.yml").open(encoding="utf-8") as handle:
            base_dict = yaml.safe_load(handle)
        base_sim = supy.SUEWSSimulation(sample_dir / "sample_config.yml")
        day_forcing = base_sim.forcing.df.loc[
            "2012-06-13 00:05:00":"2012-06-14 00:00:00"
        ].copy()

    base_dict["model"]["control"]["start_time"] = "2012-06-13"
    base_dict["model"]["control"]["end_time"] = "2012-06-13"
    base_dict["model"]["physics"]["net_radiation"] = {"value": 1003}
    base_dict["model"]["physics"]["storage_heat"] = {"value": 5}
    site_props = base_dict["sites"][0]["properties"]
    site_props["spartacus"]["use_sw_direct_albedo"] = {"value": 1.0}
    # The sample's top canopy layer (15-22 m) sits above both tree crowns (13.1 m).
    site_props["vertical_layers"]["veg_frac"]["value"][2] = 0.0
    site_props["vertical_layers"]["veg_scale"]["value"][2] = 0.0
    return base_dict, day_forcing


def _run_with_facet_params(
    base_dict: dict,
    day_forcing,
    *,
    roof_mults: tuple[float, float, float] = (0.1, 0.1, 0.1),
    wall_mults: tuple[float, float, float] = (0.1, 0.1, 0.1),
    roof_spec: tuple[float, float, float] = (0.1, 0.1, 0.1),
    wall_spec: tuple[float | None, float | None, float | None] = (0.0, 0.0, 0.0),
):
    cfg_dict = copy.deepcopy(base_dict)
    vlayers = cfg_dict["sites"][0]["properties"]["vertical_layers"]
    for idx, (roof, wall) in enumerate(zip(vlayers["roofs"], vlayers["walls"])):
        roof["roof_albedo_dir_mult_fact"] = {"value": roof_mults[idx]}
        roof["wall_specular_frac"] = {"value": roof_spec[idx]}
        wall["roof_albedo_dir_mult_fact"] = {"value": wall_mults[idx]}
        if wall_spec[idx] is None:
            wall["wall_specular_frac"] = None
        else:
            wall["wall_specular_frac"] = {"value": wall_spec[idx]}

    config = SUEWSConfig.from_dict(cfg_dict)
    sim = supy.SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(day_forcing)
    return sim.run(n_jobs=1).df


@pytest.fixture(scope="module")
def spartacus_baseline_output(spartacus_summer_day):
    """Shared baseline SPARTACUS run for 2012-06-13."""
    base_dict, day_forcing = spartacus_summer_day
    return _run_with_facet_params(base_dict, day_forcing)


def test_roof_direct_albedo_multiplier_reaches_spartacus_and_ignores_wall_copy(
    spartacus_summer_day, spartacus_baseline_output
):
    """Changing roofs[].roof_albedo_dir_mult_fact moves Kup/QN; walls[] copy is ignored."""
    base_dict, day_forcing = spartacus_summer_day
    df_low = spartacus_baseline_output

    df_high_roof = _run_with_facet_params(
        base_dict,
        day_forcing,
        roof_mults=(1.8, 1.8, 1.8),
        wall_mults=(0.1, 0.1, 0.1),
    )
    df_high_wall_copy = _run_with_facet_params(
        base_dict,
        day_forcing,
        roof_mults=(0.1, 0.1, 0.1),
        wall_mults=(1.8, 1.8, 1.8),
    )

    kup_shift = (df_high_roof["SUEWS", "Kup"] - df_low["SUEWS", "Kup"]).abs().max()
    qn_shift = (df_high_roof["SUEWS", "QN"] - df_low["SUEWS", "QN"]).abs().max()
    assert kup_shift > 50.0, (
        f"Expected substantial Kup shift from roof multiplier, got {kup_shift}"
    )
    assert qn_shift > 50.0, (
        f"Expected substantial QN shift from roof multiplier, got {qn_shift}"
    )

    np.testing.assert_allclose(
        df_high_wall_copy["SUEWS", "Kup"].to_numpy(),
        df_low["SUEWS", "Kup"].to_numpy(),
        rtol=0.0,
        atol=0.0,
        err_msg="walls[].roof_albedo_dir_mult_fact must not alter SPARTACUS Kup",
    )
    np.testing.assert_allclose(
        df_high_wall_copy["SUEWS", "QN"].to_numpy(),
        df_low["SUEWS", "QN"].to_numpy(),
        rtol=0.0,
        atol=0.0,
        err_msg="walls[].roof_albedo_dir_mult_fact must not alter SPARTACUS QN",
    )


def test_wall_specular_frac_reaches_spartacus_and_ignores_roof_copy(
    spartacus_summer_day, spartacus_baseline_output
):
    """Changing walls[].wall_specular_frac moves Kup; roofs[] copy is ignored and cannot satisfy a missing wall value."""
    base_dict, day_forcing = spartacus_summer_day
    df_base = spartacus_baseline_output

    df_high_wall_spec = _run_with_facet_params(
        base_dict,
        day_forcing,
        roof_spec=(0.1, 0.1, 0.1),
        wall_spec=(0.8, 0.8, 0.8),
    )
    df_high_roof_copy = _run_with_facet_params(
        base_dict,
        day_forcing,
        roof_spec=(0.8, 0.8, 0.8),
        wall_spec=(0.0, 0.0, 0.0),
    )

    kup_shift = (
        (df_high_wall_spec["SUEWS", "Kup"] - df_base["SUEWS", "Kup"]).abs().max()
    )
    assert kup_shift > 1e-3, (
        f"Expected measurable Kup shift from wall_specular_frac, got {kup_shift}"
    )

    np.testing.assert_allclose(
        df_high_roof_copy["SUEWS", "Kup"].to_numpy(),
        df_base["SUEWS", "Kup"].to_numpy(),
        rtol=0.0,
        atol=0.0,
        err_msg="roofs[].wall_specular_frac must not alter SPARTACUS Kup",
    )

    # Under active SPARTACUS (BR-03), a null walls[].wall_specular_frac must be
    # rejected rather than silently filled from roofs[].wall_specular_frac.
    with pytest.raises((ValueError, RuntimeError), match="wall_specular_frac"):
        _run_with_facet_params(
            base_dict,
            day_forcing,
            roof_spec=(0.8, 0.8, 0.8),
            wall_spec=(None, None, None),
        )


def test_invalid_roof_direct_albedo_multiplier_is_caught_not_clobbered(
    spartacus_summer_day,
):
    """roofs[0].roof_albedo_dir_mult_fact=2.5 (alb*mult=1.25>1) must fail rather than be clobbered."""
    base_dict, day_forcing = spartacus_summer_day

    with pytest.raises(RuntimeError, match="effective direct roof albedo"):
        _run_with_facet_params(
            base_dict,
            day_forcing,
            roof_mults=(2.5, 0.1, 0.1),
            wall_mults=(0.1, 0.1, 0.1),
        )
