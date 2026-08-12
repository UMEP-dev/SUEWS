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


def test_modelphysics_selector_guide_includes_dependency_graph() -> None:
    module = _load_generator_module()

    guide = "\n".join(module.RSTGenerator._format_modelphysics_selector_guide())

    assert ".. rubric:: Method dependency graph" in guide
    assert "``emissions`` -> ``ohm_inc_qf`` -> ``storage_heat``" in guide
    assert "EHC and STEBBS storage heat require SPARTACUS net radiation" in guide


@pytest.mark.parametrize(
    "model_name",
    ["ThermalLayers", "VerticalLayers", "RoofLayer", "WallLayer", "ModelPhysics"],
)
def test_layer_related_models_link_to_layer_conventions(model_name: str) -> None:
    module = _load_generator_module()
    generator = module.RSTGenerator({})

    rendered = generator._format_model(
        model_name,
        {"title": model_name, "description": "Test model.", "fields": []},
    )

    assert "See :ref:`layer_conventions`" in rendered


def test_relationship_targets_ref_only_documented_fields() -> None:
    module = _load_generator_module()

    rendered = module.RSTGenerator._format_relationship_targets(
        ["storage_heat", "energy_balance", "stebbs.capacitance"]
    )

    assert ":ref:`storage_heat <storage_heat>`" in rendered
    assert "``energy_balance``" in rendered
    assert "``stebbs.capacitance``" in rendered


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


def _resolve_boolean_choice(choice: str) -> None:
    assert choice.lower() in {"false", "true"}


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
    "stebbs.enabled": _resolve_boolean_choice,
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


def test_field_without_default_is_not_described_as_optional() -> None:
    """A parameter carrying no default must not be labelled optional.

    Most such parameters are declared ``Optional[...] = None`` only so a partial
    configuration still loads; the science usually requires a value. Claiming
    they are optional is the defect reported for the config reference.
    """
    # ARRANGE
    module = _load_generator_module()
    field_doc = {
        "name": "store_cap",
        "type": "Optional[FlexibleRefValue(float)]",
        "default": None,
    }

    # ACT
    label, value = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert "optional" not in value.lower()
    assert (label, value) == (module.NO_DEFAULT_NOTE_LABEL, module.NO_DEFAULT_NOTE)
    # "Status" stays reserved for the short state token, not a sentence of advice.
    assert label != "Status"


def test_required_field_still_reports_required() -> None:
    """The unconditionally required rendering must be left intact."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    label, value = module.RSTGenerator._format_default({"default": "PydanticUndefined"})

    # ASSERT
    assert (label, value) == ("Status", "Required")


@pytest.mark.parametrize(
    ("default", "expected"),
    [
        (0.5, "``0.5``"),
        (0, "``0``"),
        (False, "``False``"),
        ("", "``''`` (empty string)"),
    ],
)
def test_real_default_still_reported_under_default_label(default, expected) -> None:
    """A genuine default keeps the ``Default`` label, including falsy values.

    ``0``, ``False`` and ``''`` are real defaults and must not be mistaken for
    an absent one.
    """
    # ARRANGE
    module = _load_generator_module()

    # ACT
    label, value = module.RSTGenerator._format_default({"default": default})

    # ASSERT
    assert label == "Default"
    assert value == expected


def test_nested_model_still_skips_the_default_line() -> None:
    """Nested models carry a structure link instead of a default."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    result = module.RSTGenerator._format_default({
        "default": None,
        "nested_model": "LAIParams",
    })

    # ASSERT
    assert result == (None, None)


def test_generated_config_reference_never_claims_optional(tmp_path) -> None:
    """End-to-end gate over every rendered page.

    The generated RST is not tracked in git, so this is the only place the
    claim can be caught before it reaches readers.
    """
    # ARRANGE
    module = _load_generator_module()
    doc_data = module.ModelDocExtractor().extract_all_models()

    # ACT - "hybrid" is the style main() ships; the parameter default is stale.
    module.RSTGenerator(doc_data).generate_all_rst(tmp_path, style="hybrid")

    # ASSERT
    pages = sorted(tmp_path.glob("*.rst"))
    assert pages, "no RST pages were generated"
    offenders = [
        page.name
        for page in pages
        if "None (optional)" in page.read_text(encoding="utf-8")
    ]
    assert not offenders
