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
from supy.data_model.parameter_examples import (
    get_all_parameter_examples,
    get_parameter_examples,
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


def test_conditionally_required_field_reports_its_condition(monkeypatch) -> None:
    """A recorded condition is preferred over the generic note.

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

    # ACT
    label, value = module.RSTGenerator._format_default(
        {"name": "bldgh", "default": None}, "BldgsProperties"
    )

    # ASSERT
    assert (label, value) == ("Status", "Required when buildings are present")


def test_field_without_recorded_condition_keeps_the_note(monkeypatch) -> None:
    """Where nothing records a condition, no requirement may be claimed."""
    # ARRANGE
    module = _load_generator_module()
    monkeypatch.setattr(module, "required_when", lambda model, field: "")

    # ACT
    label, value = module.RSTGenerator._format_default(
        {"name": "store_cap", "default": None}, "StorageDrainParams"
    )

    # ASSERT
    assert (label, value) == (module.NO_DEFAULT_NOTE_LABEL, module.NO_DEFAULT_NOTE)
    assert "required" not in value.lower().split("may be required")[0]


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
    assert len(paved_fields["soil_depth"]["examples"]) == 9
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
        "D23",
        "F02",
        "H24",
        "J11",
        "J14",
        "Kotthaus2014Aug",
        "R95",
        "S00",
        "S25",
        "S88",
        "V13",
        "W13",
        "W16",
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
    assert len(index) == 108
    assert len(examples) == 554
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
        "Porosity",
        "Snow",
        "SnowLimPatch",
        "Soil",
        "Vegetation Growth",
        "Water State",
        "Water Storage",
    }
    assert len(get_parameter_examples("Conductance", "g_max")) == 3
    assert len(get_parameter_examples("SnowParams", "radiation_melt_factor")) == 1
    assert len(get_parameter_examples("SurfaceProperties", "soil_depth")) == 9
    assert len(get_parameter_examples("PavedProperties", "snowpack_limit")) == 1
    assert len(get_parameter_examples("WaterProperties", "state_limit")) == 2
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
    """Workbook averages remain exact in data but concise in the public table."""
    # ARRANGE
    module = _load_generator_module()
    examples = get_parameter_examples("PavedProperties", "emis")
    raw_value = next(
        example["value"]
        for example in examples
        if example["source"]["record_ids"] == [40241247]
    )

    # ACT
    rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples({
            "unit": "dimensionless",
            "examples": examples,
        })
    )

    # ASSERT
    assert raw_value == pytest.approx(0.9366666666666666)
    assert "``0.9366667`` dimensionless" in rendered
    assert "0.9366666666666666" not in rendered


def test_selector_dependent_examples_keep_their_method_context() -> None:
    """Flattened rows retain the selector needed to interpret their values."""
    # ARRANGE
    module = _load_generator_module()
    conductance_examples = get_parameter_examples("Conductance", "g_q_base")
    drainage_examples = get_parameter_examples("StorageDrainParams", "drain_coef_2")

    # ACT
    conductance_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples({
            "unit": "model-dependent",
            "examples": conductance_examples,
        })
    )
    drainage_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples({
            "unit": "equation-dependent",
            "examples": drainage_examples,
        })
    )

    # ASSERT
    assert [example["selector"]["value"] for example in conductance_examples] == [
        1,
        2,
        1,
    ]
    assert "gs_model=1" in conductance_rendered
    assert "gs_model=2" in conductance_rendered
    assert "drain_eq=2" in drainage_rendered
    assert "drain_eq=3" in drainage_rendered
    assert "``0.2163`` model-dependent" not in conductance_rendered


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
        )
    }

    # ASSERT
    assert fields["Conductance"]["g_k"]["unit"] == "W m^-2"
    assert fields["Conductance"]["g_sm"]["unit"] == "mm^-1"
    assert fields["Conductance"]["g_q_base"]["unit"] == "model-dependent"
    assert fields["Conductance"]["g_q_shape"]["unit"] == "model-dependent"
    assert fields["LAIPowerCoefficients"]["growth_gdd"]["unit"] == "K^-1 d^-1"
    assert fields["LAIPowerCoefficients"]["senescence_sdd"]["unit"] == ("K^-1 d^-1")
    assert fields["PavedProperties"]["state_limit"]["description"] == (
        "Upper limit to the surface water state"
    )
    assert fields["PavedProperties"]["wet_threshold"]["unit"] == "mm"
    assert (
        "surface resistance becomes zero"
        in fields["PavedProperties"]["wet_threshold"]["description"]
    )
    assert fields["StorageDrainParams"]["drain_coef_1"]["unit"] == (
        "equation-dependent"
    )
    assert "modified Rutter" in fields["StorageDrainParams"]["drain_eq"]["description"]

    wet_threshold_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(
            fields["PavedProperties"]["wet_threshold"]
        )
    )
    conductance_rendered = "\n".join(
        module.RSTGenerator({})._format_parameter_examples(fields["Conductance"]["g_k"])
    )
    assert "``0.48`` mm" in wet_threshold_rendered
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
