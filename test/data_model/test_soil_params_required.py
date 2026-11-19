"""Soil parameters are required on active non-water surfaces.

gh#2077 / gh#2076 / gh#2001 / gh#2002 (part of gh#1991): omitting or nulling
``soil_depth``, ``soil_store_capacity`` or ``saturated_hydraulic_conductivity``
on an active (``sfr > 0``) non-water surface used to pass validation and reach
the water-balance kernel as ``0.0`` via the Rust bridge. Active non-water
surfaces must now supply all three parameters; inactive surfaces (``sfr == 0``)
and ``water`` remain exempt. The 2026.6.dev11 migration writes the former
effective ``0.0`` explicitly on active non-water surfaces when upgrading older
configurations.
"""

from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.core import SUEWSConfig
from supy.data_model.validation.required_fields import (
    NON_WATER_SURFACES,
    SOIL_REQUIRED,
    required_when,
)
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]

SOIL_FIELDS: tuple[str, ...] = tuple(SOIL_REQUIRED)


@pytest.fixture(scope="module")
def sample_dict() -> dict:
    """Return a parsed copy of the shipped sample configuration."""
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _land_cover(data: dict) -> dict:
    return data["sites"][0]["properties"]["land_cover"]


def _with_soil_override(
    sample_dict: dict, surface: str, field: str, value: object
) -> dict:
    """Return a copy of the sample with one active surface soil field changed.

    If *surface* has ``sfr == 0`` in the shipped sample (``evetr``, ``bsoil``),
    0.05 is shifted from ``paved`` so the surface is genuinely active while
    preserving ``sum(sfr) == 1.0``. ``value`` is a scalar to wrap as
    ``{value: scalar}``, ``None`` for ``{value: null}``, or ``...`` to omit
    the key completely.
    """
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    surf = lc[surface]
    if surf["sfr"]["value"] <= 0.0:
        lc["paved"]["sfr"]["value"] = round(lc["paved"]["sfr"]["value"] - 0.05, 6)
        surf["sfr"]["value"] = 0.05
    surf.pop(field, None)
    if value is not ...:
        surf[field] = {"value": value}
    return data


@pytest.mark.parametrize("surface", NON_WATER_SURFACES)
@pytest.mark.parametrize("field", SOIL_FIELDS)
@pytest.mark.parametrize("value", [..., None])
def test_active_non_water_surface_requires_soil_params(
    sample_dict: dict, surface: str, field: str, value: object
) -> None:
    """Active non-water surfaces reject omitted or null soil parameters."""
    # ARRANGE
    data = _with_soil_override(sample_dict, surface, field, value)

    # ACT / ASSERT
    with pytest.raises(
        ValueError,
        match=rf"land_cover\.{surface}: {field} is None but {surface}\.sfr=",
    ):
        SUEWSConfig.from_dict(data)


def test_from_yaml_rejects_missing_soil_param_and_annotates(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Loading from YAML rejects omitted soil parameters and annotates them."""
    # ARRANGE
    data = _with_soil_override(sample_dict, "paved", "soil_depth", ...)
    config_path = tmp_path / "missing_soil.yml"
    config_path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match=r"land_cover\.paved: soil_depth is None"):
        SUEWSConfig.from_yaml(str(config_path))


def test_inactive_non_water_surface_allows_omitted_soil_params(
    sample_dict: dict,
) -> None:
    """An inactive (sfr == 0) non-water surface may omit all soil parameters."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    grass_sfr = lc["grass"]["sfr"]["value"]
    lc["paved"]["sfr"]["value"] = round(lc["paved"]["sfr"]["value"] + grass_sfr, 6)
    lc["grass"]["sfr"]["value"] = 0.0
    for surface in ("grass", "bsoil"):
        for field in SOIL_FIELDS:
            lc[surface].pop(field, None)

    # ACT
    config = SUEWSConfig.from_dict(data)

    # ASSERT
    for surface in ("grass", "bsoil"):
        surf = getattr(config.sites[0].properties.land_cover, surface)
        assert surf.sfr.value == 0.0
        for field in SOIL_FIELDS:
            assert getattr(surf, field) is None


def test_water_surface_allows_omitted_soil_params(sample_dict: dict) -> None:
    """Water has no soil column and remains exempt even when sfr == 1.0."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    for surface in NON_WATER_SURFACES:
        lc[surface]["sfr"] = {"value": 0.0}
        for field in SOIL_FIELDS:
            lc[surface].pop(field, None)
    lc["water"]["sfr"] = {"value": 1.0}
    for field in SOIL_FIELDS:
        lc["water"].pop(field, None)

    # ACT
    config = SUEWSConfig.from_dict(data)

    # ASSERT
    assert config.sites[0].properties.land_cover.water.sfr.value == 1.0
    for field in SOIL_FIELDS:
        assert getattr(config.sites[0].properties.land_cover.water, field) is None


def test_water_docs_do_not_claim_soil_requirements() -> None:
    """WaterProperties does not advertise soil-parameter requirements."""
    for field in (*SOIL_FIELDS, "soil_field_capacity"):
        assert not required_when("WaterProperties", field)


@pytest.mark.parametrize(
    "source_schema", ["2026.6.dev10", "2026.6.dev9", "2026.6.dev8"]
)
def test_upgrade_writes_former_zero_for_missing_and_null_soil_params(
    sample_dict: dict, tmp_path: Path, source_schema: str
) -> None:
    """Upgrading to dev11 writes 0.0 on active non-water surfaces and preserves refs."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["schema_version"] = source_schema
    if source_schema == "2026.6.dev8":
        physics = data["model"]["physics"]
        physics.pop("soil_moisture", None)
        physics["soil_moisture_deficit"] = {"value": 0}
    lc = _land_cover(data)
    # Omitted on active paved -> 0.0
    lc["paved"].pop("soil_depth", None)
    # Null with reference metadata on active paved -> 0.0 preserving ref
    lc["paved"]["soil_store_capacity"] = {
        "value": None,
        "ref": {"desc": "Legacy null placeholder"},
    }
    # Explicit 0.0 on active paved -> preserved
    lc["paved"]["saturated_hydraulic_conductivity"] = {"value": 0.0}
    # Inactive bsoil missing all three -> stays omitted
    bsoil_sfr = lc["bsoil"]["sfr"]["value"]
    lc["bldgs"]["sfr"]["value"] += bsoil_sfr
    lc["bsoil"]["sfr"]["value"] = 0.0
    for field in SOIL_FIELDS:
        lc["bsoil"].pop(field, None)
    # Water missing all three -> stays omitted
    for field in SOIL_FIELDS:
        lc["water"].pop(field, None)

    source = tmp_path / f"{source_schema}.yml"
    output = tmp_path / "upgraded.yml"
    source.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    # ACT
    upgrade_yaml(input_path=source, output_path=output)

    # ASSERT
    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    migrated_lc = _land_cover(migrated)
    assert migrated_lc["paved"]["soil_depth"] == {"value": 0.0}
    assert migrated_lc["paved"]["soil_store_capacity"] == {
        "value": 0.0,
        "ref": {"desc": "Legacy null placeholder"},
    }
    assert migrated_lc["paved"]["saturated_hydraulic_conductivity"] == {"value": 0.0}
    for field in SOIL_FIELDS:
        assert field not in migrated_lc["bsoil"]
        assert field not in migrated_lc["water"]

    loaded = SUEWSConfig.from_yaml(str(output))
    paved = loaded.sites[0].properties.land_cover.paved
    assert paved.soil_depth.value == 0.0
    assert paved.soil_store_capacity.value == 0.0
    assert paved.saturated_hydraulic_conductivity.value == 0.0


def test_rust_bridge_rejects_missing_soil_param_on_programmatic_config() -> None:
    """Even if a programmatic config drops a soil field, the Rust bridge refuses it."""
    from supy import SUEWSSimulation
    from supy._run_rust import run_suews_rust

    # ARRANGE
    sim = SUEWSSimulation.from_sample_data()
    cfg = sim.config.model_copy(deep=True)
    cfg.sites[0].properties.land_cover.paved.soil_depth = None
    forcing_slice = sim.forcing.df.iloc[:2]

    # ACT / ASSERT
    with pytest.raises(
        RuntimeError,
        match=r"properties\.land_cover\.paved\.soil_depth.*is missing",
    ):
        run_suews_rust(cfg, forcing_slice, grid_id=cfg.sites[0].gridiv)
