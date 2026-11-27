"""Tests for generated config-reference documentation helpers."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import re

import pytest

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.human_activity import CO2Params
from supy.data_model.core.hydro import StorageDrainParams, WaterDistribution
from supy.data_model.core.model import OutputControl
from supy.data_model.core.physics_families import PHYSICS_FAMILIES, resolve_scalar_name
from supy.data_model.core.physics_orthogonal import (
    coerce_orthogonal_to_flat,
    fold_storage_heat_ohm_inc_qf,
)
from supy.data_model.core.site import (
    Conductance,
    DectrProperties,
    EvetrProperties,
    GrassProperties,
)
from supy.data_model.core.state import (
    InitialStateDectr,
    InitialStateEvetr,
    InitialStateGrass,
    InitialStateVeg,
)
from supy.data_model.core.surface import (
    BldgsProperties,
    BsoilProperties,
    PavedProperties,
    WaterProperties,
)
from supy.data_model.parameter_examples import (
    get_all_parameter_examples,
    get_parameter_examples,
)
from supy.data_model.validation.derived_fields import (
    DOC_DERIVED_DEFAULT,
    derived_default,
)
from supy.data_model.validation.required_fields import DOC_REQUIRED_WHEN

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SCHEMA", "C-TOOLING")]

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


def test_generated_modelphysics_explains_modelled_building_fai_limit(
    tmp_path,
) -> None:
    module = _load_generator_module()
    doc_data = module.ModelDocExtractor().extract_all_models()

    module.RSTGenerator(doc_data).generate_all_rst(tmp_path, style="hybrid")
    guide = (tmp_path / "modelphysics.rst").read_text(encoding="utf-8")

    assert ".. warning::" in guide
    assert "uses grid-cell area as a horizontal length scale" in guide
    assert ":math:`\\lambda_f = \\lambda_p H / b`" in guide
    assert "does not assume a universal building width" in guide
    assert ":cite:t:`GO99UrbanForm`" in guide


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


def test_model_page_does_not_add_redundant_parameters_label() -> None:
    """Option directives already make the page contents clear to readers."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    rendered = module.RSTGenerator({})._format_model(
        "ExampleModel",
        {
            "title": "Example model",
            "description": "Test model.",
            "fields": [{"name": "example_field", "type_info": {}}],
        },
    )

    # ASSERT
    assert "**Parameters:**" not in rendered
    assert ".. input:option:: example_field" in rendered


def test_relationship_targets_ref_only_documented_fields() -> None:
    module = _load_generator_module()

    rendered = module.RSTGenerator._format_relationship_targets([
        "storage_heat",
        "energy_balance",
        "stebbs.capacitance",
    ])

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
    "soil_moisture": lambda choice: resolve_scalar_name(
        "soil_moisture", choice
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
    "stebbs.parameter_source": lambda choice: resolve_scalar_name("parameters", choice),
    "stebbs.capacitance": lambda choice: resolve_scalar_name("capacitance", choice),
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


def test_field_without_default_or_rule_is_not_called_optional(monkeypatch) -> None:
    """A parameter with no default and no rule asks for a value, not "optional".

    Such parameters are declared ``Optional[...] = None`` only so a partial
    configuration still loads; labelling them optional was the defect reported
    for the configuration reference.
    """
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(module, "required_when", lambda model, field: "")
    monkeypatch.setattr(module, "derived_default", lambda model, field: "")
    field_doc = {
        "name": "drain_coef_1",
        "type": "Optional[FlexibleRefValue(float)]",
        "default": None,
    }

    # ACT
    status = module.RSTGenerator._format_status(field_doc, "StorageDrainParams")
    default_line = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert status == module.STATUS_NO_DEFAULT
    assert "optional" not in status.lower()
    assert status.isascii()
    assert default_line == (None, None)


def test_derived_default_field_reports_its_source(monkeypatch) -> None:
    """A runtime-derived field reports its derivation rather than no default."""
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(module, "required_when", lambda model, field: "")
    monkeypatch.setattr(
        module,
        "derived_default",
        lambda model, field: (
            "``store_max`` when omitted"
            if (model, field) == ("StorageDrainParams", "store_cap")
            else ""
        ),
    )
    field_doc = {"name": "store_cap", "default": None}

    # ACT
    status = module.RSTGenerator._format_status(field_doc, "StorageDrainParams")
    default_line = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert status == "Optional; derived from ``store_max`` when omitted"
    assert status.isascii()
    assert default_line == (None, None)


def test_conditionally_required_field_reports_its_condition(monkeypatch) -> None:
    """A recorded condition becomes the status of a field without a default.

    The lookup is injected rather than relied upon, so this holds whether or
    not the installed supy carries the registry.
    """
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(
        module,
        "required_when",
        lambda model, field: "buildings are present" if field == "bldgh" else "",
    )
    field_doc = {"name": "bldgh", "default": None}

    # ACT
    status = module.RSTGenerator._format_status(field_doc, "BldgsProperties")
    default_line = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert status == "Required when buildings are present"
    assert default_line == (None, None)


def test_condition_is_reported_beside_a_real_default(monkeypatch) -> None:
    """A field that must be given explicitly keeps both its condition and default."""
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(
        module, "required_when", lambda model, field: "``internal_shading`` is ``2``"
    )
    field_doc = {"name": "temperature_threshold_shading", "default": 0.0}

    # ACT
    status = module.RSTGenerator._format_status(field_doc, "StebbsProperties")
    default_line = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert status == "Required when ``internal_shading`` is ``2``"
    assert default_line == ("Default", "``0.0``")


def test_field_with_default_is_optional(monkeypatch) -> None:
    """A default means the parameter never has to be supplied."""
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(module, "required_when", lambda model, field: "")

    # ACT
    status = module.RSTGenerator._format_status(
        {"name": "emis", "default": 0.95}, "BldgsProperties"
    )

    # ASSERT
    assert status == module.STATUS_OPTIONAL


def test_required_field_still_reports_required() -> None:
    """A field with no default at all is required, with no default line."""
    # ARRANGE
    module = _load_generator_module()
    field_doc = {"default": "PydanticUndefined"}

    # ACT
    status = module.RSTGenerator._format_status(field_doc)
    default_line = module.RSTGenerator._format_default(field_doc)

    # ASSERT
    assert status == module.STATUS_REQUIRED
    assert default_line == (None, None)


def test_nested_model_has_no_status() -> None:
    """A nested structure is described by its own page, not a status line."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    status = module.RSTGenerator._format_status(
        {"name": "lai", "default": None, "nested_model": "LAIParams"}
    )

    # ASSERT
    assert status is None


@pytest.fixture(scope="module")
def generated_reference(tmp_path_factory) -> tuple[Path, dict]:
    """Generate the configuration reference once from the real data models."""
    module = _load_generator_module()
    output_dir = tmp_path_factory.mktemp("config-reference")
    doc_data = module.ModelDocExtractor().extract_all_models()
    module.RSTGenerator(doc_data).generate_all_rst(output_dir, style="hybrid")
    return output_dir, doc_data


def _option_blocks(page: str) -> dict[str, str]:
    """Split a generated page into its ``input:option`` blocks, keyed by name."""
    parts = re.split(r"^\.\. input:option:: (\S+)\n", page, flags=re.MULTILINE)
    return {
        name: re.split(r"^\S", body, maxsplit=1, flags=re.MULTILINE)[0]
        for name, body in zip(parts[1::2], parts[2::2])
    }


def test_every_parameter_entry_states_its_status(generated_reference) -> None:
    """Each parameter carries exactly one status; nested groups link instead."""
    # ARRANGE
    output_dir, doc_data = generated_reference
    nested = {
        (model_name.lower(), field["name"])
        for model_name, model_doc in doc_data["models"].items()
        for field in model_doc.get("fields", [])
        if field.get("nested_model")
    }
    pages = sorted(output_dir.glob("*.rst"))

    # ACT
    problems = [
        f"{path.name}:{name} has {block.count(':Status:')} status lines"
        for path in pages
        for name, block in _option_blocks(path.read_text(encoding="utf-8")).items()
        if block.count(":Status:") != (0 if (path.stem, name) in nested else 1)
    ]

    # ASSERT
    assert pages
    assert not problems


def test_every_recorded_requirement_reaches_the_reference(generated_reference) -> None:
    """A required-field rule the validation layer records must be documented.

    Adding a field to the registry without it appearing, with its condition,
    on its model's page fails here.
    """
    # ARRANGE
    output_dir, _ = generated_reference

    # ACT
    missing = []
    for model_name, fields in DOC_REQUIRED_WHEN.items():
        page = output_dir / f"{model_name.lower()}.rst"
        blocks = (
            _option_blocks(page.read_text(encoding="utf-8")) if page.exists() else {}
        )
        for field, condition in fields.items():
            expected = f":Status: Required when {condition}"
            if expected not in " ".join(blocks.get(field, "").split()):
                missing.append(f"{model_name}.{field}")

    # ASSERT
    assert not missing


def test_every_recorded_derived_default_reaches_the_reference(
    generated_reference,
) -> None:
    """Every runtime-derived field must report its derivation on its doc page."""
    # ARRANGE
    output_dir, _ = generated_reference

    # ACT
    missing = []
    for model_name, fields in DOC_DERIVED_DEFAULT.items():
        page = output_dir / f"{model_name.lower()}.rst"
        blocks = (
            _option_blocks(page.read_text(encoding="utf-8")) if page.exists() else {}
        )
        for field, source in fields.items():
            expected = f":Status: Optional; derived from {source}"
            normalised = " ".join(blocks.get(field, "").split())
            if expected not in normalised:
                missing.append(f"{model_name}.{field}")

    # ASSERT
    assert not missing


def test_recorded_derived_defaults_exist_and_resolve_at_runtime() -> None:
    """Every derived-default entry names a real field with runtime resolution."""
    # ARRANGE
    models = {
        "StorageDrainParams": StorageDrainParams,
        "InitialStateVeg": InitialStateVeg,
        "InitialStateEvetr": InitialStateEvetr,
        "InitialStateDectr": InitialStateDectr,
        "InitialStateGrass": InitialStateGrass,
        "OutputControl": OutputControl,
    }

    # ACT / ASSERT - every recorded field exists and lookup works
    assert set(DOC_DERIVED_DEFAULT) == set(models)
    for model_name, fields in DOC_DERIVED_DEFAULT.items():
        for field in fields:
            assert field in models[model_name].model_fields
            assert derived_default(model_name, field) == fields[field]
    assert not derived_default("StorageDrainParams", "not_a_field")

    # Runtime check 1: StorageDrainParams.store_cap resolves to store_max in engine dump
    drain = StorageDrainParams(store_max=0.73)
    dumped = drain.model_dump(exclude_none=True, mode="json")
    assert dumped["store_cap"]["value"] == pytest.approx(0.73)

    # Runtime check 2: InitialState*.alb_id follows opposite LAI directions for
    # trees (increasing with LAI) and grass (decreasing with LAI).
    cfg = SUEWSConfig(
        sites=[
            {
                "properties": {
                    "land_cover": {
                        "evetr": {
                            "alb_min": 0.10,
                            "alb_max": 0.30,
                            "lai": {"lai_min": 0.5, "lai_max": 5.5},
                        },
                        "grass": {
                            "alb_min": 0.10,
                            "alb_max": 0.30,
                            "lai": {"lai_min": 0.5, "lai_max": 5.5},
                        },
                    }
                },
                "initial_states": {
                    "evetr": {"lai_id": 4.0, "alb_id": None},
                    "grass": {"lai_id": 4.0, "alb_id": None},
                },
            }
        ]
    )
    evetr_alb = cfg.sites[0].initial_states.evetr.alb_id
    grass_alb = cfg.sites[0].initial_states.grass.alb_id
    evetr_val = evetr_alb.value if hasattr(evetr_alb, "value") else evetr_alb
    grass_val = grass_alb.value if hasattr(grass_alb, "value") else grass_alb
    # ratio = (4.0 - 0.5) / (5.5 - 0.5) = 0.7 -> tree: 0.10 + 0.20*0.7 = 0.24, grass: 0.30 - 0.20*0.7 = 0.16
    assert evetr_val == pytest.approx(0.24)
    assert grass_val == pytest.approx(0.16)

    # Runtime check 3: OutputControl.freq stays None on the model so SUEWSOutput.save
    # can apply the 3600 s native fallback or preserve a fixed resampled interval.
    assert OutputControl().freq is None


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


def test_site_specific_default_is_not_relabelled_as_an_example() -> None:
    """A model default and a literature-backed example are different concepts."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    label, value = module.RSTGenerator._format_default({
        "default": 0.1,
        "is_site_specific": True,
    })

    # ASSERT
    assert (label, value) == ("Default", "``0.1``")


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


def test_extractor_attaches_legacy_name_from_canonical_registry() -> None:
    """Current fields expose aliases already accepted by the YAML loader."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    doc_data = module.ModelDocExtractor().extract_all_models()
    lai_fields = {
        field["name"]: field for field in doc_data["models"]["LAIParams"]["fields"]
    }

    # ASSERT
    assert lai_fields["base_temperature_senescence"]["legacy_name"] == "basete"


def test_extractor_uses_only_the_immediately_preceding_migration_name() -> None:
    """Multi-stage renames show the last hop rather than the oldest alias."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    models = module.ModelDocExtractor().extract_all_models()["models"]
    fields = {
        model_name: {field["name"]: field for field in models[model_name]["fields"]}
        for model_name in (
            "ModelPhysics",
            "SnowParams",
            "ArchetypeProperties",
            "StebbsProperties",
        )
    }

    # ASSERT
    assert fields["ModelPhysics"]["net_radiation"]["legacy_name"] == (
        "net_radiation_method"
    )
    assert fields["SnowParams"]["radiation_melt_factor"]["legacy_name"] == (
        "rad_melt_factor"
    )
    assert (
        fields["ArchetypeProperties"]["max_power_heating_system_air"]["legacy_name"]
        == "power_air_heating_max"
    )
    assert fields["StebbsProperties"]["depth_ground"]["legacy_name"] == ("ground_depth")


def test_extractor_inherits_legacy_names_and_parameter_examples() -> None:
    """Concrete surface pages include metadata accepted by base validators."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    models = module.ModelDocExtractor().extract_all_models()["models"]
    paved_fields = {
        field["name"]: field for field in models["PavedProperties"]["fields"]
    }
    evetr_fields = {
        field["name"]: field for field in models["EvetrProperties"]["fields"]
    }

    # ASSERT
    assert paved_fields["soil_depth"]["legacy_name"] == "soildepth"
    assert len(paved_fields["soil_depth"]["examples"]) == 7
    assert evetr_fields["max_conductance"]["legacy_name"] == "maxconductance"
    assert evetr_fields["max_conductance"]["examples"]


def test_extractor_attaches_cited_lai_parameter_examples() -> None:
    """The database catalogue joins source columns to current data-model fields."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    doc_data = module.ModelDocExtractor().extract_all_models()
    lai_fields = {
        field["name"]: field for field in doc_data["models"]["LAIParams"]["fields"]
    }
    examples = lai_fields["base_temperature_senescence"]["examples"]

    # ASSERT
    assert [example["value"] for example in examples] == [10, 11]
    assert [example["origin"] for example in examples] == ["Helsinki", "SE England"]
    assert examples[0]["reference"]["doi"] == (
        "https://doi.org/10.5194/gmd-7-1691-2014"
    )
    assert examples[0]["reference"]["docs_citation_key"] == "J14"
    assert examples[1]["reference"]["doi"] == (
        "https://doi.org/10.1016/j.uclim.2016.05.001"
    )
    assert examples[1]["reference"]["docs_citation_key"] == "W16"


def test_parameter_example_catalogue_rejects_missing_value_sentinels() -> None:
    """Packaged examples must contain real values, not database sentinels."""
    # ACT
    examples = [
        example
        for field_examples in get_all_parameter_examples().values()
        for example in field_examples
    ]

    # ASSERT
    assert examples
    assert all(example["value"] != -999 for example in examples)


def test_parameter_example_internal_citations_exist_in_docs_bibliography() -> None:
    """Every example source must resolve on the documentation references page."""
    # ARRANGE
    references_dir = PROJECT_ROOT / "docs" / "source" / "assets" / "refs"
    bibliography = "\n".join(
        (references_dir / filename).read_text(encoding="utf-8")
        for filename in (
            "refs-SUEWS.bib",
            "refs-others.bib",
            "refs-community.bib",
        )
    )
    examples = [
        example
        for field_examples in get_all_parameter_examples().values()
        for example in field_examples
    ]

    # ACT
    citation_keys = {example["reference"]["docs_citation_key"] for example in examples}

    # ASSERT
    assert all(
        isinstance(citation_key, str) and citation_key for citation_key in citation_keys
    )
    assert citation_keys == {
        "A16",
        "B03",
        "D23",
        "H24",
        "H24data",
        "J11",
        "J14",
        "Kotthaus2014Aug",
        "O02",
        "R95",
        "S00",
        "S25",
        "S88",
        "V13",
        "W16",
        "W18",
        "X24",
        "Z23",
    }
    for citation_key in citation_keys:
        assert re.search(rf"@\w+\{{{re.escape(citation_key)},", bibliography)


def test_parameter_example_catalogue_covers_reliable_scalar_tables() -> None:
    """The expanded catalogue covers each explicitly mapped workbook domain."""
    # ACT
    index = get_all_parameter_examples()
    examples = [
        example for field_examples in index.values() for example in field_examples
    ]
    sheets = {example["source"]["sheet"] for example in examples}

    # ASSERT
    assert len(index) == 87
    assert len(examples) == 481
    assert sheets == {
        "Albedo",
        "Biogen CO2",
        "Conductance",
        "Drainage",
        "Emissivity",
        "Leaf Area Index",
        "Leaf Growth Power",
        "Max Vegetation Conductance",
        "OHM",
        "SnowLimPatch",
        "Soil",
        "Vegetation Growth",
        "Water Storage",
    }
    assert len(get_parameter_examples("Conductance", "g_max")) == 3
    assert get_parameter_examples("StorageDrainParams", "drain_eq") == []
    assert get_parameter_examples("SnowParams", "radiation_melt_factor") == []
    assert len(get_parameter_examples("SurfaceProperties", "soil_depth")) == 7
    assert len(get_parameter_examples("PavedProperties", "snowpack_limit")) == 1
    assert get_parameter_examples("WaterProperties", "state_limit") == []
    assert get_parameter_examples("DectrProperties", "porosity_min_deciduous") == []
    assert len(get_parameter_examples("EvetrProperties", "alpha_bio_co2")) == 8


def test_parameter_example_catalogue_keys_are_documented_fields() -> None:
    """A misspelt model or field mapping must not become an orphaned example."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    models = module.ModelDocExtractor().extract_all_models()["models"]
    documented_fields = {
        (model_name, field["name"])
        for model_name, model_doc in models.items()
        for field in model_doc["fields"]
    }

    # ASSERT
    assert set(get_all_parameter_examples()) <= documented_fields


def test_parameter_example_catalogue_excludes_unreliable_references() -> None:
    """Known missing, placeholder and incorrect references never reach docs."""
    # ACT
    reference_ids = {
        example["reference"]["id"]
        for field_examples in get_all_parameter_examples().values()
        for example in field_examples
    }

    # ASSERT
    assert not reference_ids & {
        "90240000",
        "90240027",
        "90240064",
        "90240991",
        "90241000",
        "99240099",
    }


def test_parameter_example_catalogue_excludes_uncited_state_limits() -> None:
    """Legacy state limits absent from the cited papers never reach docs."""
    # ACT
    record_ids = {
        record_id
        for field_examples in get_all_parameter_examples().values()
        for example in field_examples
        for record_id in example["source"]["record_ids"]
    }

    # ASSERT
    assert not record_ids & {
        42240000,
        42240041,
        42240042,
        42240043,
        42240044,
        42240045,
        42240046,
        42240057,
    }


def test_parameter_example_catalogue_excludes_unreferenced_database_records() -> None:
    """Database-unreferenced, unreviewed, and input-table-only rows stay unpublished."""
    # ACT
    index = get_all_parameter_examples()
    record_ids = {
        record_id
        for field_examples in index.values()
        for example in field_examples
        for record_id in example["source"]["record_ids"]
    }

    # ASSERT
    assert not record_ids & {
        22240064,
        22240065,
        23240006,
        30240009,
        31240021,
        31240025,
        31240026,
        34240002,
        40241247,
        40241248,
        50240002,
        50240044,
        50240045,
    }

    def _by_record_id(model_name: str, field_name: str, rid: int) -> dict:
        return next(
            example
            for example in get_parameter_examples(model_name, field_name)
            if rid in example["source"]["record_ids"]
        )

    assert _by_record_id("PavedProperties", "emis", 40240001)["value"] == pytest.approx(
        0.91
    )
    assert (
        _by_record_id("PavedProperties", "emis", 40240001)["reference"][
            "docs_citation_key"
        ]
        == "J11"
    )
    assert _by_record_id("BldgsProperties", "emis", 40240005)["value"] == pytest.approx(
        0.95
    )
    assert (
        _by_record_id("BldgsProperties", "emis", 40240005)["reference"][
            "docs_citation_key"
        ]
        == "J11"
    )
    assert (
        _by_record_id("PavedProperties", "emis", 40241259)["reference"][
            "docs_citation_key"
        ]
        == "O02"
    )
    assert _by_record_id("WaterProperties", "alb", 41240013)["value"] == pytest.approx(
        0.08
    )
    assert (
        _by_record_id("WaterProperties", "alb", 41240013)["reference"][
            "docs_citation_key"
        ]
        == "V13"
    )
    assert (
        _by_record_id("StorageDrainParams", "store_max", 43240041)["reference"][
            "docs_citation_key"
        ]
        == "W18"
    )
    assert _by_record_id("StorageDrainParams", "drain_coef_1", 45240004)["value"] == 10
    assert _by_record_id("StorageDrainParams", "drain_coef_2", 45240004)["value"] == 3
    assert (
        _by_record_id("StorageDrainParams", "drain_coef_1", 45240004)["reference"][
            "docs_citation_key"
        ]
        == "W18"
    )
    assert _by_record_id("StorageDrainParams", "drain_coef_2", 45240015)[
        "value"
    ] == pytest.approx(0.013)
    assert (
        _by_record_id("StorageDrainParams", "drain_coef_2", 45240015)["reference"][
            "docs_citation_key"
        ]
        == "J11"
    )
    assert _by_record_id("StorageDrainParams", "drain_coef_2", 45240015)[
        "selector"
    ] == {"name": "drain_eq", "value": 3}
    assert (
        _by_record_id("StorageDrainParams", "drain_coef_2", 45240012)["reference"][
            "docs_citation_key"
        ]
        == "J11"
    )
    assert _by_record_id("StorageDrainParams", "drain_coef_2", 45240012)[
        "selector"
    ] == {"name": "drain_eq", "value": 2}
    assert get_parameter_examples("StorageDrainParams", "drain_eq") == []
    assert _by_record_id("GrassProperties", "wet_threshold", 45240063)[
        "value"
    ] == pytest.approx(1.9)
    assert (
        _by_record_id("GrassProperties", "wet_threshold", 45240063)["reference"][
            "docs_citation_key"
        ]
        == "J11"
    )
    for rid in (45240061, 45240062, 45240063):
        coef1 = _by_record_id("StorageDrainParams", "drain_coef_1", rid)
        coef2 = _by_record_id("StorageDrainParams", "drain_coef_2", rid)
        assert coef1["reference"]["docs_citation_key"] == "J11"
        assert coef2["reference"]["docs_citation_key"] == "J11"
        assert coef1["selector"] == {"name": "drain_eq", "value": 2}
        assert coef2["selector"] == {"name": "drain_eq", "value": 2}

    module = _load_generator_module()
    drain_fields = {
        field["name"]: field
        for field in module.ModelDocExtractor().extract_all_models()["models"][
            "StorageDrainParams"
        ]["fields"]
    }
    drain_coef_2_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(drain_fields["drain_coef_2"])
    )
    for surface, desc in (
        ("evergreen tree", "EvergreenTree - Helsinki, Zurich, Beijing"),
        ("deciduous tree", "Deciduous Tree - Helsinki, Zurich, Beijing"),
        ("grass", "Grass - Helsinki, Zurich, Beijing"),
    ):
        expected_row = (
            "      * - ``1.71`` |m^2| |kg^-1|\n"
            f"        - London; {surface}; {desc}; drain_eq=2\n"
            "        - :cite:t:`J11`"
        )
        assert expected_row in drain_coef_2_rendered
        assert f"{desc}; drain_eq=3" not in drain_coef_2_rendered

    lai_type_rids = {
        rid
        for example in get_parameter_examples("LAIParams", "lai_type")
        for rid in example["source"]["record_ids"]
    }
    assert not lai_type_rids & {31240044, 31240045}
    for rid, expected_min, expected_max in (
        (31240044, 4, 5.1),
        (31240045, 1, 5.5),
    ):
        lai_min = _by_record_id("LAIParams", "lai_min", rid)
        lai_max = _by_record_id("LAIParams", "lai_max", rid)
        assert lai_min["value"] == pytest.approx(expected_min)
        assert lai_max["value"] == pytest.approx(expected_max)
        assert lai_min["reference"]["docs_citation_key"] == "J11"
        assert lai_max["reference"]["docs_citation_key"] == "J11"
    assert (
        _by_record_id("GrassProperties", "max_conductance", 33240002)["reference"][
            "docs_citation_key"
        ]
        == "B03"
    )
    assert (
        _by_record_id("SurfaceProperties", "soil_depth", 22240059)["reference"][
            "docs_citation_key"
        ]
        == "H24data"
    )
    assert (
        _by_record_id("LAIPowerCoefficients", "growth_gdd", 32240035)["reference"][
            "docs_citation_key"
        ]
        == "H24data"
    )


def test_parameter_example_catalogue_invalidates_generated_rst_stamp() -> None:
    """Incremental documentation builds regenerate RST after catalogue edits."""
    # ACT
    makefile = (PROJECT_ROOT / "docs" / "Makefile").read_text(encoding="utf-8")

    # ASSERT
    assert "DM_DOC_SRC" in makefile
    assert "../src/supy/data_model/parameter_examples.json" in makefile
    assert "$(RST_STAMP): generate_datamodel_rst.py $(DM_DOC_SRC)" in makefile


def test_extractor_does_not_attach_same_named_alias_to_another_model() -> None:
    """An alias stays with the model whose validator accepts it."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    doc_data = module.ModelDocExtractor().extract_all_models()
    model_physics_fields = {
        field["name"]: field for field in doc_data["models"]["ModelPhysics"]["fields"]
    }
    site_properties_fields = {
        field["name"]: field for field in doc_data["models"]["SiteProperties"]["fields"]
    }

    # ASSERT
    assert model_physics_fields["stebbs"]["legacy_name"] == "stebbs_method"
    assert "legacy_name" not in site_properties_fields["stebbs"]


def test_legacy_name_is_rendered_and_indexed() -> None:
    """A legacy-name search can find the maintained current-name entry."""
    # ARRANGE
    module = _load_generator_module()
    field_doc = {
        "name": "base_temperature_senescence",
        "type": "Optional[FlexibleRefValue(float)]",
        "type_info": {},
        "legacy_name": "basete",
    }

    # ACT
    rendered = "\n".join(module.RSTGenerator({})._format_field(field_doc, "LAIParams"))

    # ASSERT
    assert "single: basete (legacy YAML parameter)" in rendered
    assert ":Legacy name: ``basete``" in rendered


def test_cited_parameter_examples_are_rendered_separately_from_defaults() -> None:
    """The generated field shows contextual examples with original DOI links."""
    # ARRANGE
    module = _load_generator_module()
    doc_data = module.ModelDocExtractor().extract_all_models()
    lai_fields = {
        field["name"]: field for field in doc_data["models"]["LAIParams"]["fields"]
    }

    # ACT
    rendered = "\n".join(
        module.RSTGenerator({})._format_field(
            lai_fields["base_temperature_senescence"], "LAIParams"
        )
    )

    # ASSERT
    assert ".. rubric:: Example values" in rendered
    assert "``10`` degC" in rendered
    assert "``11`` degC" in rendered
    assert "Helsinki; evergreen tree, deciduous tree and grass" in rendered
    assert ":cite:t:`J14`" in rendered
    assert ":cite:t:`W16`" in rendered
    assert "https://doi.org/10.5194/gmd-7-1691-2014" not in rendered
    assert ":Default:" not in rendered


def test_parameter_example_rejects_source_outside_docs_bibliography() -> None:
    """A DOI alone cannot publish an example before its source is approved."""
    # ARRANGE
    module = _load_generator_module()
    field_doc = {
        "unit": "degC",
        "examples": [
            {
                "value": 7,
                "origin": "Example site",
                "surfaces": ["grass"],
                "description": "Irrigated lawn",
                "season": "summer",
                "reference": {
                    "citation": "Example et al. (2026)",
                    "doi": "https://doi.org/10.0000/example",
                },
            }
        ],
    }

    # ACT AND ASSERT
    with pytest.raises(ValueError, match="added to the documentation bibliography"):
        module.RSTGenerator({})._format_parameter_examples(field_doc)


def test_parameter_examples_limit_display_precision_without_changing_raw_data() -> None:
    """High-precision floats remain exact in data but concise in the public table."""
    # ARRANGE
    module = _load_generator_module()
    raw_value = 0.9366666666666666
    examples = [
        {
            "value": raw_value,
            "origin": "Example site",
            "surfaces": ["paved"],
            "description": "Repeating-decimal test value",
            "reference": {
                "citation": "Kotthaus et al. (2014)",
                "doi": "https://doi.org/10.1016/j.isprsjprs.2014.05.005",
                "docs_citation_key": "Kotthaus2014Aug",
            },
        }
    ]

    # ACT
    rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples({
            "unit": "dimensionless",
            "examples": examples,
        })
    )

    # ASSERT
    assert examples[0]["value"] == pytest.approx(0.9366666666666666)
    assert "``0.9366667`` dimensionless" in rendered
    assert "0.9366666666666666" not in rendered


def test_selector_dependent_examples_keep_their_method_context() -> None:
    """Flattened rows retain selector context and resolve fixed CF branch units."""
    # ARRANGE
    module = _load_generator_module()
    models = module.ModelDocExtractor().extract_all_models()["models"]
    conductance_fields = {
        field["name"]: field for field in models["Conductance"]["fields"]
    }
    drainage_fields = {
        field["name"]: field for field in models["StorageDrainParams"]["fields"]
    }
    conductance_examples = get_parameter_examples("Conductance", "g_q_base")

    # ACT
    g_q_base_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            conductance_fields["g_q_base"]
        )
    )
    g_q_shape_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            conductance_fields["g_q_shape"]
        )
    )
    drain_coef_1_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            drainage_fields["drain_coef_1"]
        )
    )
    drain_coef_2_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            drainage_fields["drain_coef_2"]
        )
    )

    # ASSERT
    assert [example["selector"]["value"] for example in conductance_examples] == [
        1,
        2,
        1,
    ]
    assert "gs_model=1" in g_q_base_rendered
    assert "gs_model=2" in g_q_base_rendered
    assert "``0.2163`` kg |g^-1|" in g_q_base_rendered
    assert "``0.13`` (dimensionless)" in g_q_base_rendered
    assert "model-dependent" not in g_q_base_rendered

    assert "``3.3649`` g |kg^-1|" in g_q_shape_rendered
    assert "* - ``0.7``\n" in g_q_shape_rendered
    assert "``0.7`` (dimensionless)" not in g_q_shape_rendered

    assert "``0.013`` kg |m^-2| |h^-1|" in drain_coef_1_rendered
    assert "* - ``10``\n" in drain_coef_1_rendered
    assert "equation-dependent" not in drain_coef_1_rendered

    assert "drain_eq=2" in drain_coef_2_rendered
    assert "drain_eq=3" in drain_coef_2_rendered
    assert "``1.71`` |m^2| |kg^-1|" in drain_coef_2_rendered
    assert "``3`` (dimensionless)" in drain_coef_2_rendered
    assert "equation-dependent" not in drain_coef_2_rendered


def test_published_example_fields_use_physically_consistent_units() -> None:
    """Metadata units agree with equations for newly published examples."""
    # ARRANGE
    module = _load_generator_module()

    # ACT
    models = module.ModelDocExtractor().extract_all_models()["models"]
    fields = {
        model_name: {field["name"]: field for field in models[model_name]["fields"]}
        for model_name in (
            "Conductance",
            "LAIPowerCoefficients",
            "PavedProperties",
            "StorageDrainParams",
            "CO2Params",
        )
    }

    # ASSERT
    assert fields["Conductance"]["g_max"]["unit"] == "1"
    assert fields["Conductance"]["g_k"]["unit"] == "W m-2"
    assert fields["Conductance"]["g_sm"]["unit"] == "m2 kg-1"
    assert "unit" not in fields["Conductance"]["g_q_base"]
    assert fields["Conductance"]["g_q_base"]["conditional_unit"]["selector"] == (
        "model.physics.surface_conductance"
    )
    assert (
        fields["Conductance"]["g_q_base"]["conditional_unit"]["cases"]["1"]["unit"]
        == "kg g-1"
    )
    assert (
        fields["Conductance"]["g_q_base"]["conditional_unit"]["cases"]["2"]["unit"]
        == "1"
    )
    assert "unit" not in fields["Conductance"]["g_q_shape"]
    assert (
        fields["Conductance"]["g_q_shape"]["conditional_unit"]["cases"]["1"]["unit"]
        == "g kg-1"
    )
    assert (
        fields["Conductance"]["g_q_shape"]["conditional_unit"]["cases"]["2"]["kind"]
        == "empirical_base"
    )
    assert fields["LAIPowerCoefficients"]["growth_gdd"]["unit"] == "K-1 d-1"
    assert fields["LAIPowerCoefficients"]["senescence_sdd"]["unit"] == ("K-1 d-1")
    assert fields["PavedProperties"]["state_limit"]["description"] == (
        "Upper limit to the surface water state"
    )
    assert fields["PavedProperties"]["wet_threshold"]["unit"] == "kg m-2"
    assert (
        "surface resistance becomes zero"
        in fields["PavedProperties"]["wet_threshold"]["description"]
    )
    assert "unit" not in fields["StorageDrainParams"]["drain_coef_1"]
    assert (
        fields["StorageDrainParams"]["drain_coef_1"]["conditional_unit"]["selector"]
        == "drain_eq"
    )
    assert (
        fields["StorageDrainParams"]["drain_coef_1"]["conditional_unit"]["cases"]["1"][
            "kind"
        ]
        == "formula"
    )
    assert (
        fields["StorageDrainParams"]["drain_coef_1"]["conditional_unit"]["cases"]["2"][
            "unit"
        ]
        == "kg m-2 h-1"
    )
    assert "unit" not in fields["StorageDrainParams"]["drain_coef_2"]
    assert (
        fields["StorageDrainParams"]["drain_coef_2"]["conditional_unit"]["cases"]["2"][
            "unit"
        ]
        == "m2 kg-1"
    )
    assert "modified Rutter" in fields["StorageDrainParams"]["drain_eq"]["description"]
    assert "unit" not in fields["CO2Params"]["traffic_rate"]
    assert (
        fields["CO2Params"]["traffic_rate"]["conditional_unit"]["cases"]["1"]["unit"]
        == "km m-2 d-1"
    )
    assert (
        fields["CO2Params"]["traffic_rate"]["conditional_unit"]["cases"]["2"]["unit"]
        == "km d-1"
    )

    wet_threshold_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            fields["PavedProperties"]["wet_threshold"]
        )
    )
    conductance_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(fields["Conductance"]["g_k"])
    )
    assert "``0.48`` kg |m^-2|" in wet_threshold_rendered
    assert "``566.0923`` W |m^-2|" in conductance_rendered


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


def test_generated_waterdistribution_rst_matches_validation_contract(tmp_path) -> None:
    """Generated waterdistribution.rst states sfr>0, zero-normalisation, and post-load rules."""
    # ARRANGE
    module = _load_generator_module()
    doc_data = module.ModelDocExtractor().extract_all_models()

    # ACT
    module.RSTGenerator(doc_data).generate_all_rst(tmp_path, style="hybrid")
    page = (tmp_path / "waterdistribution.rst").read_text(encoding="utf-8")

    # ASSERT
    assert ":ref:`waterdist_yaml_rules`" in page
    assert "``sfr > 0``" in page
    assert "``sfr == 0``" in page
    assert "normalised to absent" in page
    assert "post-load" in page
    assert "to_runoff:" in page
    assert "value: 0.98" in page
    assert "to_grass:" in page
    assert "value: 0.02" in page
    for field in (
        "to_paved",
        "to_bldgs",
        "to_dectr",
        "to_evetr",
        "to_grass",
        "to_bsoil",
        "to_water",
        "to_runoff",
        "to_soilstore",
    ):
        assert f".. input:option:: {field}" in page
    assert ":Status: Required when" in page


def test_waterdist_user_docs_and_committed_schema_match_implementation() -> None:
    """Maintained user docs and committed schema.json agree with WaterDistribution rules."""
    # ARRANGE / ACT
    transition_guide = (
        PROJECT_ROOT / "docs" / "source" / "inputs" / "transition_guide.rst"
    ).read_text(encoding="utf-8")
    yaml_index = (
        PROJECT_ROOT / "docs" / "source" / "inputs" / "yaml" / "index.rst"
    ).read_text(encoding="utf-8")
    yaml_validation = (
        PROJECT_ROOT / "docs" / "source" / "inputs" / "yaml" / "validation.rst"
    ).read_text(encoding="utf-8")
    legacy_table = (
        PROJECT_ROOT
        / "docs"
        / "source"
        / "inputs"
        / "tables"
        / "SUEWS_SiteInfo"
        / "SUEWS_WithinGridWaterDist.rst"
    ).read_text(encoding="utf-8")
    committed_schema = json.loads(
        (
            PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
        ).read_text(encoding="utf-8")
    )
    live_wd_schema = WaterDistribution.model_json_schema()

    # ASSERT: transition_guide carries anchor, worked paved example, and all three rules
    assert ".. _waterdist_yaml_rules:" in transition_guide
    assert "value: 0.98" in transition_guide
    assert "value: 0.02" in transition_guide
    assert "``sfr > 0``" in transition_guide
    assert "``sfr == 0``" in transition_guide
    assert "normalised to absent (``None``)" in transition_guide
    assert "Post-load live mutations" in transition_guide

    # ASSERT: yaml/index.rst qualifies missing-parameter autofill for waterdist
    assert ":ref:`waterdist_yaml_rules`" in yaml_index
    assert "mandatory active-surface routing fields" in yaml_index.lower()
    assert "not silently filled" in yaml_index

    # ASSERT: yaml/validation.rst documents the waterdist Phase C & post-load rules
    assert ":ref:`waterdist_yaml_rules`" in yaml_validation
    assert "``sfr > 0``" in yaml_validation
    assert "normalised to absent at YAML load" in yaml_validation

    # ASSERT: legacy SUEWS_WithinGridWaterDist.txt docs stay intact and cross-link YAML
    assert "diagonal elements should be zero" in legacy_table
    assert ":ref:`waterdist_yaml_rules`" in legacy_table

    # ASSERT: committed schema.json WaterDistribution metadata stays synced with live model
    committed_wd = committed_schema["$defs"]["WaterDistribution"]
    assert committed_wd["description"] == live_wd_schema["description"]
    assert committed_wd["title"] == live_wd_schema["title"]
    for field_name in WaterDistribution.model_fields:
        if not field_name.startswith("to_"):
            continue
        assert (
            committed_wd["properties"][field_name]["description"]
            == live_wd_schema["properties"][field_name]["description"]
        )
    for surface_cls in (
        PavedProperties,
        BldgsProperties,
        EvetrProperties,
        DectrProperties,
        GrassProperties,
        BsoilProperties,
        WaterProperties,
    ):
        assert (
            committed_schema["$defs"][surface_cls.__name__]["properties"]["waterdist"][
                "description"
            ]
            == surface_cls.model_fields["waterdist"].description
        )


def test_generated_conditional_unit_rst_and_committed_schema_match(tmp_path) -> None:
    """Generated reference pages and committed schema.json render all five conditional fields."""
    # ARRANGE
    module = _load_generator_module()
    doc_data = module.ModelDocExtractor().extract_all_models()
    committed_schema = json.loads(
        (
            PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
        ).read_text(encoding="utf-8")
    )
    live_schema = SUEWSConfig.model_json_schema()

    # ACT
    module.RSTGenerator(doc_data).generate_all_rst(tmp_path, style="hybrid")
    drain_page = (tmp_path / "storagedrainparams.rst").read_text(encoding="utf-8")
    cond_page = (tmp_path / "conductance.rst").read_text(encoding="utf-8")
    co2_page = (tmp_path / "co2params.rst").read_text(encoding="utf-8")

    # ASSERT: generated RST pages render selector paths, branch units/formulas, and no legacy prose
    assert ":Unit: Depends on ``drain_eq``:" in drain_page
    assert "``(kg m-2)^(1 - drain_coef_2) h-1``" in drain_page
    assert "kg |m^-2| |h^-1|" in drain_page
    assert "|m^2| |kg^-1|" in drain_page
    assert "``1`` (dimensionless)" in drain_page
    assert "equation-dependent" not in drain_page

    assert ":Unit: Depends on ``model.physics.surface_conductance``:" in cond_page
    assert "kg |g^-1|" in cond_page
    assert "g |kg^-1|" in cond_page
    assert (
        "empirical power base for exponent in g |kg^-1|; log unit: kg |g^-1|"
        in cond_page
    )
    assert "model-dependent" not in cond_page

    assert ":Unit: Depends on ``type_traffic_rate``:" in co2_page
    assert "km |m^-2| |d^-1|" in co2_page
    assert "km |d^-1|" in co2_page
    assert "vehicle km m-2 day-1" not in co2_page

    # ASSERT: committed schema.json stays synced with live model conditional_unit metadata
    for model_cls, field_name in (
        (StorageDrainParams, "drain_coef_1"),
        (StorageDrainParams, "drain_coef_2"),
        (Conductance, "g_q_base"),
        (Conductance, "g_q_shape"),
        (CO2Params, "traffic_rate"),
    ):
        model_name = model_cls.__name__
        live_prop = live_schema["$defs"][model_name]["properties"][field_name]
        committed_prop = committed_schema["$defs"][model_name]["properties"][field_name]
        assert "unit" not in live_prop
        assert "unit" not in committed_prop
        assert committed_prop["conditional_unit"] == live_prop["conditional_unit"]
        assert committed_prop["description"] == live_prop["description"]


def test_stebbs_volume_hot_water_metadata_has_no_placeholders_and_matches_schema(
    generated_reference,
) -> None:
    """STEBBS volume_hot_water carries real descriptions synced with schema.json."""
    # ARRANGE
    output_dir, _ = generated_reference
    committed_schema = json.loads(
        (
            PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
        ).read_text(encoding="utf-8")
    )
    live_schema = SUEWSConfig.model_json_schema()

    # ACT
    live_prop = live_schema["$defs"]["StebbsProperties"]["properties"][
        "volume_hot_water"
    ]
    committed_prop = committed_schema["$defs"]["StebbsProperties"]["properties"][
        "volume_hot_water"
    ]
    stebbs_page = (output_dir / "stebbsproperties.rst").read_text(encoding="utf-8")
    volume_block = _option_blocks(stebbs_page)["volume_hot_water"]

    # ASSERT: no placeholder explanations remain in live schema or generated RST
    live_schema_text = json.dumps(live_schema)
    assert "Missing default explanation." not in live_schema_text
    assert "Missing range explanation." not in live_schema_text
    for page in sorted(output_dir.glob("*.rst")):
        text = page.read_text(encoding="utf-8")
        assert "Missing default explanation." not in text
        assert "Missing range explanation." not in text

    # ASSERT: volume_hot_water descriptions are present, ASCII, rendered, and synced
    for key, label in (
        ("default_description", ":Default Description:"),
        ("range_description", ":Range Description:"),
    ):
        value = live_prop[key]
        assert isinstance(value, str) and value.strip()
        assert value.isascii()
        assert committed_prop[key] == value
        assert f"{label} {value}" in volume_block
