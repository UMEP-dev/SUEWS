"""Systematic ``{value: null}`` rejection on non-optional RefValue fields and Phase A hygiene.

gh#2087 (part of gh#2083): ``RefValue.value`` stays ``Optional[T]`` so
genuinely ``Optional[FlexibleRefValue(T)]`` fields can carry ``{value: null}``
when their governing physics block is inactive. Non-optional
``FlexibleRefValue(T)`` fields inherit ``DataModelBase`` and reject both bare
``null`` and ``{value: null}`` automatically; active biogenic CO2 methods also
reject explicit null on ``min_res_bioco2``; and ``suews validate`` Phase A
skips defaulted null-rejecting fields instead of writing null placeholders over
them.
"""

from __future__ import annotations

import copy
import importlib
from importlib.resources import as_file, files

from pydantic import BaseModel
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml  # noqa: PLC2701
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.surface import PavedProperties
from supy.data_model.core.type import (
    NULL_REJECTED_FIELDS,
    DataModelBase,
    Reference,
    RefValue,
)
from supy.data_model.validation.pipeline.phase_a import (
    annotate_missing_parameters,
    create_clean_missing_param_annotation,
    find_missing_parameters,
)
from supy.suews_sim import SUEWSSimulation

pytestmark = [
    pytest.mark.native,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-EMIT"),
]


@pytest.fixture(scope="module")
def sample_dict():
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def _is_core_config_model(attr: object, module_name: str) -> bool:
    if not isinstance(attr, type) or not issubclass(attr, BaseModel):
        return False
    if attr in {BaseModel, DataModelBase, Reference, RefValue}:
        return False
    return not issubclass(attr, RefValue) and attr.__module__ == module_name


def _block(data: dict, path: tuple[str, ...]) -> dict:
    block = data["sites"][0]
    for key in path:
        block = block[key]
    return block


def _unwrap(value):
    if isinstance(value, RefValue):
        return value.value
    return value["value"] if isinstance(value, dict) else value


def test_all_core_models_inherit_data_model_base():
    """Every config BaseModel in ``supy.data_model.core`` inherits ``DataModelBase``."""
    # ARRANGE
    core_modules = (
        "config",
        "human_activity",
        "hydro",
        "model",
        "ohm",
        "profile",
        "site",
        "state",
        "surface",
    )

    # ACT
    non_conforming = []
    for mod_name in core_modules:
        module = importlib.import_module(f"supy.data_model.core.{mod_name}")
        for attr_name in dir(module):
            attr = getattr(module, attr_name)
            if _is_core_config_model(attr, module.__name__) and not issubclass(
                attr, DataModelBase
            ):
                non_conforming.append(f"{mod_name}.{attr_name}")

    # ASSERT
    assert non_conforming == []


# (path inside sites[0], field_name, documented_default)
_NON_OPTIONAL_CASES = [
    (("properties", "land_cover", "paved"), "emis", 0.95),
    (("properties", "land_cover", "paved"), "alb", 0.1),
    (("properties", "land_cover", "paved"), "wet_threshold", 0.5),
    (("properties", "land_cover", "paved"), "state_limit", 10.0),
    (("properties", "land_cover", "grass"), "max_conductance", 0.5),
    (("properties", "land_cover", "water"), "flowchange", 0.0),
    (("initial_states", "grass"), "sdd_id", 0.0),
    (("initial_states",), "snowalb", 0.5),
]


@pytest.mark.parametrize(("path", "field", "default"), _NON_OPTIONAL_CASES)
@pytest.mark.parametrize("null_payload", [{"value": None}, None])
def test_non_optional_ref_fields_reject_wrapped_and_bare_null(
    sample_dict, path, field, default, null_payload
):
    """Non-optional FlexibleRefValue fields reject both ``{value: null}`` and ``null``."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    _block(data, path)[field] = null_payload

    # ACT / ASSERT
    with pytest.raises(ValueError):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(("path", "field", "default"), _NON_OPTIONAL_CASES)
def test_non_optional_ref_fields_emit_default_when_omitted(
    sample_dict, path, field, default
):
    """Omitting a defaulted non-optional field emits its model default and registers it."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    del _block(data, path)[field]

    # ACT
    dump = SUEWSConfig.from_dict(data).model_dump(exclude_none=True, mode="json")

    # ASSERT
    assert field in NULL_REJECTED_FIELDS
    assert _unwrap(_block(dump, path)[field]) == pytest.approx(default)


def test_min_res_bioco2_rejects_explicit_null_under_active_biogenic_co2(sample_dict):
    """Under emissions method 22, omitting ``min_res_bioco2`` gives 0.1 while null fails."""
    # ARRANGE: omit gives 0.1 default
    omitted = copy.deepcopy(sample_dict)
    omitted["model"]["physics"]["emissions"] = {"value": 22}
    del omitted["sites"][0]["properties"]["land_cover"]["grass"]["min_res_bioco2"]

    # ACT
    cfg = SUEWSConfig.from_dict(omitted)
    dump = cfg.model_dump(exclude_none=True, mode="json")

    # ASSERT
    assert _unwrap(
        dump["sites"][0]["properties"]["land_cover"]["grass"]["min_res_bioco2"]
    ) == pytest.approx(0.1)

    # ARRANGE / ACT / ASSERT: explicit null or {value: null} rejected when active
    for bad_val in (None, {"value": None}):
        bad = copy.deepcopy(sample_dict)
        bad["model"]["physics"]["emissions"] = {"value": 22}
        bad["sites"][0]["properties"]["land_cover"]["grass"]["min_res_bioco2"] = bad_val
        with pytest.raises(ValueError, match="min_res_bioco2"):
            SUEWSConfig.from_dict(bad)


def test_min_res_bioco2_allows_null_when_biogenic_co2_is_disabled(sample_dict):
    """Under non-biogenic emissions (method 2), inactive ``min_res_bioco2`` stays nullable."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["emissions"] = {"value": 2}
    data["sites"][0]["properties"]["land_cover"]["grass"]["min_res_bioco2"] = {
        "value": None
    }

    # ACT
    cfg = SUEWSConfig.from_dict(data)

    # ASSERT
    assert cfg.sites[0].properties.land_cover.grass.min_res_bioco2.value is None


def test_phase_a_skips_defaulted_non_optional_fields_and_empty_blocks(
    sample_dict, tmp_path
):
    """Phase A does not inject ``{value: null}`` over omitted defaulted core fields."""
    # ARRANGE: remove core defaulted fields and entire LUMPS block (all defaulted)
    sparse = copy.deepcopy(sample_dict)
    paved = sparse["sites"][0]["properties"]["land_cover"]["paved"]
    for key in ("emis", "alb", "wet_threshold", "state_limit"):
        del paved[key]
    del sparse["sites"][0]["properties"]["lumps"]

    user_yaml = tmp_path / "sparse.yml"
    uptodate_yaml = tmp_path / "updated_sparse.yml"
    report_txt = tmp_path / "report_sparse.txt"
    user_yaml.write_text(yaml.safe_dump(sparse, sort_keys=False), encoding="utf-8")

    # ACT
    with as_file(
        files("supy").joinpath("sample_data", "sample_config.yml")
    ) as sample_path:
        annotate_missing_parameters(
            user_file=str(user_yaml),
            standard_file=str(sample_path),
            uptodate_file=str(uptodate_yaml),
            report_file=str(report_txt),
            forcing="off",
        )
    updated_data = yaml.safe_load(uptodate_yaml.read_text(encoding="utf-8"))
    reloaded = SUEWSConfig.from_yaml(str(uptodate_yaml))

    # ASSERT: omitted defaulted fields were NOT overwritten with null placeholders
    updated_paved = updated_data["sites"][0]["properties"]["land_cover"]["paved"]
    for key in ("emis", "alb", "wet_threshold", "state_limit"):
        assert key not in updated_paved
    assert "lumps" not in updated_data["sites"][0]["properties"]
    assert _unwrap(reloaded.sites[0].properties.land_cover.paved.emis) == pytest.approx(
        0.95
    )
    assert _unwrap(reloaded.sites[0].properties.land_cover.paved.alb) == pytest.approx(
        0.1
    )


def test_phase_a_reports_public_names_for_missing_snow_and_ohm_inc_qf(
    sample_dict, tmp_path
):
    """Phase A diagnostics report ``snow`` and ``storage_heat.ohm.include_qf``."""
    # ARRANGE
    sparse = copy.deepcopy(sample_dict)
    sparse["model"]["physics"].pop("snow", None)
    sparse["model"]["physics"].pop("snow_use", None)
    sparse["model"]["physics"]["storage_heat"] = {"value": 1}
    sparse["model"]["physics"].pop("ohm_inc_qf", None)

    user_yaml = tmp_path / "missing_phys.yml"
    uptodate_yaml = tmp_path / "updated_missing_phys.yml"
    report_txt = tmp_path / "report_missing_phys.txt"
    user_yaml.write_text(yaml.safe_dump(sparse, sort_keys=False), encoding="utf-8")

    # ACT
    missing = find_missing_parameters(copy.deepcopy(sparse), copy.deepcopy(sample_dict))
    with as_file(
        files("supy").joinpath("sample_data", "sample_config.yml")
    ) as sample_path:
        annotate_missing_parameters(
            user_file=str(user_yaml),
            standard_file=str(sample_path),
            uptodate_file=str(uptodate_yaml),
            report_file=str(report_txt),
            forcing="off",
        )
    report = report_txt.read_text(encoding="utf-8")

    # ASSERT
    critical_paths = {p for p, _, is_phys in missing if is_phys}
    assert "model.physics.snow_use" in critical_paths
    assert "model.physics.ohm_inc_qf" in critical_paths
    assert "-- snow has been added" in report
    assert "-- storage_heat.ohm.include_qf has been added" in report
    assert "-- snow_use has been added" not in report
    assert "-- ohm_inc_qf has been added" not in report


def test_create_clean_missing_param_annotation_skips_all_null_rejected_block():
    """A missing block whose fields are all null-rejected emits no empty YAML key."""
    # ARRANGE / ACT
    lines = create_clean_missing_param_annotation(
        "lumps",
        {
            "raincover": {"value": 1.0},
            "rainmaxres": {"value": 10.0},
            "drainrt": {"value": 0.25},
            "veg_type": {"value": 1},
        },
    )

    # ASSERT
    assert lines == []


def test_from_yaml_and_direct_model_reject_null_ref_value(sample_dict, tmp_path):
    """Both YAML loading and direct model instantiation reject ``{value: null}``."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["properties"]["land_cover"]["paved"]["emis"] = {"value": None}
    bad_yaml = tmp_path / "bad_emis.yml"
    bad_yaml.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match="emis"):
        SUEWSConfig.from_yaml(str(bad_yaml))
    with pytest.raises(ValueError, match="emis"):
        PavedProperties(emis=RefValue(None))
    with pytest.raises(ValueError, match="emis"):
        PavedProperties(emis={"value": None})


def test_create_clean_missing_param_annotation_keeps_critical_physics_block():
    """Critical physics blocks still emit placeholders when ``skip_null_rejected=False``."""
    # ARRANGE / ACT
    lines = create_clean_missing_param_annotation(
        "stebbs",
        {"capacitance": {"value": 0}, "setpoint": {"value": 0}},
        skip_null_rejected=False,
    )

    # ASSERT
    assert lines[0] == "stebbs:"
    assert any("capacitance:" in line for line in lines)
    assert any("setpoint:" in line for line in lines)


def test_mutable_live_model_rejects_in_place_null_on_non_optional_ref(sample_dict):
    """Mutating ``.value = None`` or assigning null on a live model fails immediately."""
    # ARRANGE
    cfg = SUEWSConfig.from_dict(copy.deepcopy(sample_dict))
    paved = cfg.sites[0].properties.land_cover.paved
    original_emis = _unwrap(paved.emis)

    # ACT / ASSERT: in-place RefValue.value = None fails and leaves original value intact
    with pytest.raises(ValueError, match="emis needs a number"):
        paved.emis.value = None
    assert _unwrap(paved.emis) == pytest.approx(original_emis)

    # Direct attribute null assignments also fail immediately
    for bad_val in (None, RefValue(None), {"value": None}):
        with pytest.raises(ValueError, match="emis needs a number"):
            paved.emis = bad_val
    assert _unwrap(paved.emis) == pytest.approx(original_emis)

    # Deepcopy preserves the non-optional binding
    cloned = copy.deepcopy(cfg)
    with pytest.raises(ValueError, match="emis needs a number"):
        cloned.sites[0].properties.land_cover.paved.emis.value = None

    # Assigning a new RefValue binds it so a later .value = None is also rejected
    paved.emis = RefValue(0.90)
    assert _unwrap(paved.emis) == pytest.approx(0.90)
    with pytest.raises(ValueError, match="emis needs a number"):
        paved.emis.value = None


def test_mutable_model_positive_controls_and_optional_ref_allowed(sample_dict):
    """Valid typed edits succeed and genuinely Optional RefValue(None) stays allowed."""
    # ARRANGE
    cfg = SUEWSConfig.from_dict(copy.deepcopy(sample_dict))

    # ACT: valid live edit before SUEWSSimulation and after SUEWSSimulation
    cfg.sites[0].properties.land_cover.paved.emis.value = 0.90
    sim = SUEWSSimulation(cfg)
    engine_before = yaml.safe_load(config_to_engine_yaml(sim.config))
    sim.config.sites[0].properties.land_cover.paved.emis.value = 0.88
    engine_after = yaml.safe_load(config_to_engine_yaml(sim.config))

    # Genuinely optional field and standalone RefValue(None) remain nullable
    sim.config.sites[0].initial_states.evetr.alb_id.value = None
    standalone = RefValue(None)

    # ASSERT
    assert _unwrap(
        engine_before["sites"][0]["properties"]["land_cover"]["paved"]["emis"]
    ) == pytest.approx(0.90)
    assert _unwrap(
        engine_after["sites"][0]["properties"]["land_cover"]["paved"]["emis"]
    ) == pytest.approx(0.88)
    assert sim.config.sites[0].initial_states.evetr.alb_id.value is None
    assert standalone.value is None


def test_simulation_routes_reject_mutated_null_before_and_after_construction(
    sample_dict,
):
    """Both before- and after-construction ``SUEWSSimulation`` routes reject null edits."""
    # Route 1: mutate config before SUEWSSimulation(cfg)
    cfg_before = SUEWSConfig.from_dict(copy.deepcopy(sample_dict))
    with pytest.raises(ValueError, match="emis needs a number"):
        cfg_before.sites[0].properties.land_cover.paved.emis.value = None

    # Route 2: construct SUEWSSimulation, then mutate sim.config before run/export
    sim = SUEWSSimulation(SUEWSConfig.from_dict(copy.deepcopy(sample_dict)))
    with pytest.raises(ValueError, match="emis needs a number"):
        sim.config.sites[0].properties.land_cover.paved.emis.value = None
    with pytest.raises(ValueError, match="emis needs a number"):
        sim.config.sites[0].properties.land_cover.paved.emis = None
    with pytest.raises(ValueError, match="emis needs a number"):
        sim.config.sites[0].properties.land_cover.paved.emis = RefValue(None)


def test_public_handoffs_reject_mutated_null_min_res_bioco2_when_active(
    sample_dict, sample_forcing_parsed
):
    """Mutating ``min_res_bioco2.value = None`` under active biogenic CO2 fails handoff."""
    # ARRANGE: Route 1 -- mutate before SUEWSSimulation(cfg)
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["emissions"] = {"value": 22}
    cfg = SUEWSConfig.from_dict(data)
    cfg.sites[0].properties.land_cover.grass.min_res_bioco2.value = None

    # ACT / ASSERT
    with pytest.raises(ValueError, match="min_res_bioco2"):
        SUEWSSimulation(cfg)
    with pytest.raises(ValueError, match="min_res_bioco2"):
        config_to_engine_yaml(cfg)

    # ARRANGE: Route 2 -- construct valid SUEWSSimulation, then mutate sim.config
    _, df_short = sample_forcing_parsed(12)
    sim = SUEWSSimulation(SUEWSConfig.from_dict(copy.deepcopy(data)))
    sim.update_forcing(df_short)
    sim.config.sites[0].properties.land_cover.grass.min_res_bioco2.value = None

    # ACT / ASSERT: rejected before Rust bridge serialization / execution
    with pytest.raises(ValueError, match="min_res_bioco2"):
        config_to_engine_yaml(sim.config)
    with pytest.raises(ValueError, match="min_res_bioco2"):
        sim.run(end_date=df_short.index[-1])
