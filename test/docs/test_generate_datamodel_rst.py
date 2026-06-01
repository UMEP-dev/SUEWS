"""Tests for generated config-reference documentation helpers."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import re

import pytest

from supy.data_model.core.physics_families import PHYSICS_FAMILIES, resolve_scalar_name
from supy.data_model.core.physics_orthogonal import (
    coerce_orthogonal_to_flat,
    fold_storage_heat_ohm_inc_qf,
)


pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def _load_generator_module():
    module_path = PROJECT_ROOT / "docs" / "generate_datamodel_rst.py"
    spec = importlib.util.spec_from_file_location(
        "generate_datamodel_rst_for_test", module_path
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_modelphysics_selector_guide_uses_rubric_not_section() -> None:
    module = _load_generator_module()

    lines = module.RSTGenerator._format_modelphysics_selector_guide()

    assert lines[0] == ".. rubric:: Public selector forms"
    assert "---------------------" not in lines[:3]


def test_modelphysics_selector_guide_lists_registered_edge_tokens() -> None:
    module = _load_generator_module()

    guide = "\n".join(module.RSTGenerator._format_modelphysics_selector_guide())

    assert "``not_used2``" in guide
    assert "``rst`` / ``T19``" in guide
    assert "nested ``narp``; nested ``spartacus``" in guide
    assert resolve_scalar_name("stability", "not_used2") == 1
    assert resolve_scalar_name("roughness_sublayer", "rst") == 1


def _documented_choice_rows(lines: list[str]) -> list[tuple[str, str]]:
    rows: list[tuple[str, str]] = []
    for index, line in enumerate(lines[:-1]):
        if not line.startswith("   * - ``"):
            continue
        paths = re.findall(r"``([^`]+)``", line)
        choices = re.findall(r"``([^`]+)``", lines[index + 1])
        rows.extend((path, choice) for path in paths for choice in choices)
    return rows


def _resolve_emissions_anthropogenic(choice: str) -> None:
    payload = {"heat": "J11", "co2": {"anthropogenic": choice, "biogenic": "none"}}
    if choice.lower() != "none":
        payload["co2"]["biogenic"] = "rectangular"
    coerce_orthogonal_to_flat("emissions", payload)


def _resolve_emissions_biogenic(choice: str) -> None:
    payload = {"heat": "J11", "co2": {"anthropogenic": "none", "biogenic": choice}}
    if choice.lower() != "none":
        payload["co2"]["anthropogenic"] = "qf_linked"
    coerce_orthogonal_to_flat("emissions", payload)


def _resolve_storage_heat_family(choice: str) -> None:
    if choice.lower() in PHYSICS_FAMILIES["storage_heat"]:
        return
    resolve_scalar_name("storage_heat", choice)


def _resolve_storage_heat_include_qf(choice: str) -> None:
    values = {"storage_heat": {"ohm": {"include_qf": choice}}}
    fold_storage_heat_ohm_inc_qf(values, "ModelPhysics")
    assert set(values) == {"storage_heat", "ohm_inc_qf"}


CHOICE_RESOLVERS = {
    "net_radiation": lambda choice: (
        None
        if choice.lower() in PHYSICS_FAMILIES["net_radiation"]
        else resolve_scalar_name("net_radiation", choice)
    ),
    "net_radiation.narp.ldown": lambda choice: coerce_orthogonal_to_flat(
        "net_radiation", {"narp": {"ldown": choice}}
    ),
    "net_radiation.narp.variant": lambda choice: coerce_orthogonal_to_flat(
        "net_radiation", {"narp": {"ldown": "air", "variant": choice}}
    ),
    "net_radiation.spartacus.ldown": lambda choice: coerce_orthogonal_to_flat(
        "net_radiation", {"spartacus": {"ldown": choice}}
    ),
    "emissions.heat": lambda choice: coerce_orthogonal_to_flat(
        "emissions", {"heat": choice}
    ),
    "emissions.co2.anthropogenic": _resolve_emissions_anthropogenic,
    "emissions.co2.biogenic": _resolve_emissions_biogenic,
    "storage_heat": _resolve_storage_heat_family,
    "storage_heat.ohm.include_qf": _resolve_storage_heat_include_qf,
    "soil_moisture_deficit": lambda choice: resolve_scalar_name(
        "soil_moisture_deficit", choice
    ),
    "water_use": lambda choice: resolve_scalar_name("water_use", choice),
    "leaf_area_index": lambda choice: resolve_scalar_name("laimethod", choice),
    "frontal_area_index": lambda choice: resolve_scalar_name(
        "frontal_area_index", choice
    ),
    "snow": lambda choice: resolve_scalar_name("snow_use", choice),
    "roughness_length_momentum": lambda choice: resolve_scalar_name(
        "roughness_length_momentum", choice
    ),
    "roughness_length_heat": lambda choice: resolve_scalar_name(
        "roughness_length_heat", choice
    ),
    "stability": lambda choice: resolve_scalar_name("stability", choice),
    "roughness_sublayer": lambda choice: resolve_scalar_name(
        "roughness_sublayer", choice
    ),
    "roughness_sublayer_level": lambda choice: resolve_scalar_name(
        "roughness_sublayer_level", choice
    ),
    "surface_conductance": lambda choice: resolve_scalar_name(
        "surface_conductance", choice
    ),
    "stebbs.enabled": lambda choice: choice.lower() in {"false", "true"} or None,
    "stebbs.parameter_source": lambda choice: resolve_scalar_name(
        "parameters", choice
    ),
    "stebbs.capacitance": lambda choice: resolve_scalar_name(
        "capacitance", choice
    ),
    "stebbs.setpoint": lambda choice: resolve_scalar_name("setpoint", choice),
    "stebbs.same_albedo_wall": lambda choice: resolve_scalar_name(
        "same_albedo_wall", choice
    ),
    "stebbs.same_albedo_roof": lambda choice: resolve_scalar_name(
        "same_albedo_roof", choice
    ),
    "stebbs.same_emissivity_wall": lambda choice: resolve_scalar_name(
        "same_emissivity_wall", choice
    ),
    "stebbs.same_emissivity_roof": lambda choice: resolve_scalar_name(
        "same_emissivity_roof", choice
    ),
}


def test_modelphysics_selector_guide_choices_resolve() -> None:
    module = _load_generator_module()

    rows = _documented_choice_rows(
        module.RSTGenerator._format_modelphysics_selector_guide()
    )

    assert rows
    assert {path for path, _ in rows} == set(CHOICE_RESOLVERS)
    for path, choice in rows:
        CHOICE_RESOLVERS[path](choice)
