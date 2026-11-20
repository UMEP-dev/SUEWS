"""Layer 1 schema, validator AST, DOC_REQUIRED_WHEN, and doc-limbo contracts.

gh#2097 (part of gh#2083; Refs #1991, #2076, #2077): pins four Layer 1
invariants in the fast PR ``api`` tier:

1. End-to-end ``DOC_REQUIRED_WHEN`` enforcement through
   ``SUEWSConfig.from_yaml()`` (active missing raises ``ValueError`` and never
   downgrades to ``UserWarning``; inactive missing succeeds; explicit valid
   value succeeds), with explicit tracking of pending rules in
   ``schema_contract_baseline.json``.
2. Static AST lint over ``supy.data_model`` forbidding Pydantic
   ``@model_validator`` / ``@field_validator`` exception handlers that swallow
   validation errors into ``warnings.warn(...)`` without re-raising, backed by
   synthetic unit tests.
3. Classified field-contract ratchets for ``STATUS_NO_DEFAULT`` and
   ``Optional``-with-default fields so legitimate nullable and runtime-derived
   fields are distinguished from shrink-only debt and cannot be gamed by
   inventing static defaults.
4. Runtime-derived field contracts for vegetated ``alb_id`` (non-circular
   numerical checks verifying opposite tree vs grass LAI directions),
   ``LAIParams.lai_max`` fallback behaviour, ``StorageDrainParams.store_cap``,
   and ``OutputControl.freq`` (native hourly fallback vs fixed-frequency
   resampled held interval, matching
   ``test/io_tests/test_save_resampled_output.py::test_fixed_resample_saves_at_held_interval``).
"""

from __future__ import annotations

import ast
from collections.abc import Callable
import copy
from dataclasses import dataclass
from enum import IntFlag
import importlib
from importlib.resources import files
import importlib.util
import json
from pathlib import Path
import re
from typing import Any
import warnings

from pydantic_core import PydanticUndefined
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.hydro import StorageDrainParams, WaterDistribution
from supy.data_model.core.model import ForcingControl
from supy.data_model.core.site import GrassProperties, LAIParams
from supy.data_model.core.state import SurfaceInitialState
from supy.data_model.core.type import RefValue, SurfaceType
from supy.data_model.validation.derived_fields import DOC_DERIVED_DEFAULT
from supy.data_model.validation.pipeline.phase_b_rules.other_rules import (
    validate_irrigation_doy,
)
from supy.data_model.validation.required_fields import (
    CONDUCTANCE_REQUIRED,
    DOC_REQUIRED_WHEN,
    DYOHM_BUILDING_LAYERS_REQUIRED,
    LAI_CALCULATED_ONLY_REQUIRED,
    WATER_DISTRIBUTION_REQUIRED,
)

pytestmark = [pytest.mark.api, pytest.mark.cfg, pytest.mark.core]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
BASELINE_PATH = Path(__file__).resolve().parent / "schema_contract_baseline.json"
VALIDATOR_DECORATORS = frozenset({
    "model_validator",
    "field_validator",
    "validator",
    "root_validator",
})
WARN_CALL_NAMES = frozenset({"warn", "warn_missing_params"})


# =============================================================================
# Shared helpers
# =============================================================================


def _load_baseline() -> dict[str, Any]:
    """Load the committed schema contract ledger JSON."""
    return json.loads(BASELINE_PATH.read_text(encoding="utf-8"))


def _load_generator_module():
    """Load ``docs/generate_datamodel_rst.py`` without mutating ``sys.path``."""
    module_path = PROJECT_ROOT / "docs" / "generate_datamodel_rst.py"
    spec = importlib.util.spec_from_file_location(
        "generate_datamodel_rst_layer1_contracts", module_path
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _unwrap(value: Any) -> Any:
    """Unwrap one or two ``RefValue`` / Enum layers to a plain scalar."""
    if isinstance(value, RefValue):
        value = value.value
    if hasattr(value, "value"):
        value = value.value
    return value


def _load_from_yaml(
    data: dict[str, Any], tmp_path: Path, name: str = "case.yml"
) -> SUEWSConfig:
    """Write *data* to a temporary YAML file and load via ``SUEWSConfig.from_yaml``."""
    path_yaml = tmp_path / name
    path_yaml.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return SUEWSConfig.from_yaml(str(path_yaml))


def _set_surface_fractions(data: dict[str, Any], **fractions: float) -> None:
    """Set all seven land-cover fractions explicitly, summing to 1.0."""
    lc = data["sites"][0]["properties"]["land_cover"]
    for surf in ("paved", "bldgs", "dectr", "evetr", "grass", "bsoil", "water"):
        lc[surf]["sfr"] = {"value": float(fractions.get(surf, 0.0))}


def _option_blocks(page_text: str) -> dict[str, str]:
    """Split a generated RST page into ``input:option`` blocks keyed by name."""
    parts = re.split(r"^\.\. input:option:: (\S+)\n", page_text, flags=re.MULTILINE)
    return {
        name: re.split(r"^\S", body, maxsplit=1, flags=re.MULTILINE)[0]
        for name, body in zip(parts[1::2], parts[2::2], strict=True)
    }


def _status_line(block: str) -> str:
    """Extract the single ``:Status:`` line from a rendered RST field block."""
    matches = re.findall(r"^\s*:Status:\s*(.+)$", block, flags=re.MULTILINE)
    assert len(matches) == 1, f"Expected 1 :Status: line, got {matches!r}"
    return matches[0].strip()


@pytest.fixture(scope="module")
def compact_sample_dict() -> dict[str, Any]:
    """Compact valid configuration dict derived from ``sample_config.yml``.

    Drops bulky inactive sub-blocks and 24-hour/10-minute profiles so loading
    all ``DOC_REQUIRED_WHEN`` YAML cases through ``SUEWSConfig.from_yaml``
    stays fast while still exercising the real full schema.
    """
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    raw = yaml.safe_load(text)
    props = raw["sites"][0]["properties"]
    for key in ("building_archetype", "vertical_layers", "snow"):
        props.pop(key, None)
    stebbs = props.get("stebbs")
    if isinstance(stebbs, dict):
        stebbs.pop("profile_appliance", None)
        stebbs.pop("profile_flow_hot_water", None)
    irrigation = props.get("irrigation")
    if isinstance(irrigation, dict):
        irrigation.pop("wuprofa_24hr", None)
        irrigation.pop("wuprofm_24hr", None)
    anthro = props.get("anthropogenic_emissions")
    if isinstance(anthro, dict):
        anthro.pop("heat", None)
        co2 = anthro.get("co2")
        if isinstance(co2, dict):
            co2.pop("profile_traffic_24hr", None)
            co2.pop("profile_human_activity_24hr", None)
    lc = props.get("land_cover")
    if isinstance(lc, dict):
        for surf in ("paved", "bldgs", "dectr", "evetr", "grass", "bsoil", "water"):
            surf_block = lc.get(surf)
            if isinstance(surf_block, dict):
                surf_block.pop("ohm_coef", None)
                if surf != "bldgs":
                    surf_block.pop("thermal_layers", None)
    _set_surface_fractions(
        raw, paved=0.40, bldgs=0.20, dectr=0.10, evetr=0.10, grass=0.10, water=0.10
    )
    return raw


@pytest.fixture(scope="module")
def one_day_short_forcing(short_sample_sim):
    """One day (288 five-minute steps) of parsed forcing from the short sample."""
    return short_sample_sim().forcing.df.iloc[:288].copy()


@pytest.fixture(scope="module")
def doc_snapshot() -> tuple[Any, dict[str, Any], dict[str, type]]:
    """Extract model metadata and load the RST generator once per module."""
    gen = _load_generator_module()
    extractor = gen.ModelDocExtractor()
    doc_data = extractor.extract_all_models()
    return gen, doc_data, extractor.all_models


@pytest.fixture(scope="module")
def generated_rst_dir(doc_snapshot, tmp_path_factory: pytest.TempPathFactory) -> Path:
    """Render the configuration reference RST pages once per module."""
    gen, doc_data, _ = doc_snapshot
    rst_dir = tmp_path_factory.mktemp("layer1-rst")
    gen.RSTGenerator(doc_data).generate_all_rst(rst_dir, style="hybrid")
    return rst_dir


# =============================================================================
# 1. End-to-end DOC_REQUIRED_WHEN contracts through SUEWSConfig.from_yaml()
# =============================================================================


@dataclass(frozen=True)
class RequiredRuleSpec:
    """End-to-end contract specification for one ``DOC_REQUIRED_WHEN`` rule."""

    case_id: str
    model_name: str
    field_name: str
    active_method: str
    error_pattern: str
    build_active_missing: Callable[[dict[str, Any]], dict[str, Any]]
    build_inactive_missing: Callable[[dict[str, Any]], dict[str, Any]]
    build_active_valid: Callable[[dict[str, Any]], dict[str, Any]]


def _make_conductance_spec(field: str) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for a ``Conductance`` field."""

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, grass=0.25)
        d["sites"][0]["properties"]["conductance"].pop(field, None)
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=1.0)
        d["sites"][0]["properties"]["conductance"].pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, grass=0.25)
        return d

    return RequiredRuleSpec(
        case_id=f"Conductance.{field}",
        model_name="Conductance",
        field_name=field,
        active_method="grass.sfr > 0",
        error_pattern=rf"conductance\.{re.escape(field)} is None",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_lai_spec(field: str, *, modelled_only: bool) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for an ``LAIParams`` field."""

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, grass=0.25)
        d["model"]["physics"]["leaf_area_index"] = "modelled"
        d["sites"][0]["properties"]["land_cover"]["grass"]["lai"].pop(field, None)
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        if modelled_only:
            # Active vegetation under OBSERVED LAI does not require GDD/SDD fields.
            _set_surface_fractions(d, paved=0.75, grass=0.25)
            d["model"]["physics"]["leaf_area_index"] = "observed"
        else:
            # Inactive vegetation does not require lai_max.
            _set_surface_fractions(d, paved=1.0)
        d["sites"][0]["properties"]["land_cover"]["grass"]["lai"].pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, grass=0.25)
        d["model"]["physics"]["leaf_area_index"] = "modelled"
        return d

    method = (
        "grass.sfr > 0 and laimethod=1 (MODELLED)" if modelled_only else "grass.sfr > 0"
    )
    return RequiredRuleSpec(
        case_id=f"LAIParams.{field}",
        model_name="LAIParams",
        field_name=field,
        active_method=method,
        error_pattern=rf"land_cover\.grass\.lai: {re.escape(field)} is None",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_surface_geom_spec(
    model_name: str,
    surface: str,
    field: str,
    *,
    case_suffix: str = "",
    fai_mode: str | None = None,
    rsl_mode: str | None = None,
    error_pattern: str,
) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for building/tree height or FAI."""

    def _apply_modes(d: dict[str, Any], *, active: bool) -> None:
        physics = d["model"]["physics"]
        if fai_mode is not None:
            physics["frontal_area_index"] = fai_mode if active else "modelled"
        if rsl_mode is not None:
            physics["roughness_sublayer"] = rsl_mode if active else "most"
        elif fai_mode is not None and surface == "bldgs":
            # Prevent sample's explicit roughness_sublayer=variable from masking
            # the frontal_area_index=modelled inactive branch.
            physics["roughness_sublayer"] = "most"

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, **{surface: 0.25})
        _apply_modes(d, active=True)
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        if fai_mode is not None or rsl_mode is not None:
            # Keep surface active while switching the governing physics option off.
            _set_surface_fractions(d, paved=0.75, **{surface: 0.25})
            _apply_modes(d, active=False)
        else:
            _set_surface_fractions(d, paved=1.0)
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, paved=0.75, **{surface: 0.25})
        _apply_modes(d, active=True)
        return d

    label = f"{surface}.sfr > 0"
    if fai_mode is not None:
        label += f" + frontal_area_index={fai_mode}"
    if rsl_mode is not None:
        label += f" + roughness_sublayer={rsl_mode}"

    return RequiredRuleSpec(
        case_id=f"{model_name}.{field}{case_suffix}",
        model_name=model_name,
        field_name=field,
        active_method=label,
        error_pattern=error_pattern,
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_thermal_layer_spec(field: str) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for ``ThermalLayers`` under DyOHM."""

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["storage_heat"] = {"value": 6}
        d["sites"][0]["properties"]["land_cover"]["bldgs"]["thermal_layers"].pop(
            field, None
        )
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["storage_heat"] = {"ohm": {"include_qf": False}}
        d["sites"][0]["properties"]["land_cover"]["bldgs"]["thermal_layers"].pop(
            field, None
        )
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["storage_heat"] = {"value": 6}
        return d

    return RequiredRuleSpec(
        case_id=f"ThermalLayers.{field}",
        model_name="ThermalLayers",
        field_name=field,
        active_method="storage_heat=6 (DyOHM) on bldgs.thermal_layers",
        error_pattern=rf"bldgs\.thermal_layers\.{re.escape(field)}",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_shading_spec(
    field: str, active_mode: int, inactive_mode: int
) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for ``StebbsProperties`` shading."""

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        stebbs = d["sites"][0]["properties"]["stebbs"]
        stebbs["internal_shading"] = {"value": active_mode}
        stebbs.pop(field, None)
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        stebbs = d["sites"][0]["properties"]["stebbs"]
        stebbs["internal_shading"] = {"value": inactive_mode}
        stebbs.pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        stebbs = d["sites"][0]["properties"]["stebbs"]
        stebbs["internal_shading"] = {"value": active_mode}
        return d

    return RequiredRuleSpec(
        case_id=f"StebbsProperties.{field}",
        model_name="StebbsProperties",
        field_name=field,
        active_method=f"stebbs.internal_shading={active_mode}",
        error_pattern=rf"{re.escape(field)} must be provided when internal_shading",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_co2_point_source_spec() -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for ``CO2Params.emission_co2_point_source``."""

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["emissions"] = {"value": 12}
        d["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
            "emission_co2_point_source", None
        )
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["emissions"] = {"value": 2}
        d["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
            "emission_co2_point_source", None
        )
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["physics"]["emissions"] = {"value": 12}
        d["sites"][0]["properties"]["anthropogenic_emissions"]["co2"][
            "emission_co2_point_source"
        ] = {"value": 2.75}
        return d

    return RequiredRuleSpec(
        case_id="CO2Params.emission_co2_point_source",
        model_name="CO2Params",
        field_name="emission_co2_point_source",
        active_method="model.physics.emissions=12 (biogenic CO2)",
        error_pattern=r"emission_co2_point_source is None but emissions method 12",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_dls_spec(
    missing: str, given: str, *, daylight_clock: bool
) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for ``AnthropogenicEmissions`` DLS."""
    window = {"startdls": {"value": 86.0}, "enddls": {"value": 303.0}}

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        # Non-zero timezone avoids the unrelated zero-UTC-offset advisory warning.
        d["sites"][0]["properties"]["timezone"] = {"value": 1}
        d["model"]["control"]["output"]["timestamp_reference"] = (
            "daylight" if daylight_clock else "follow"
        )
        anthro = d["sites"][0]["properties"]["anthropogenic_emissions"]
        anthro.pop(missing, None)
        if daylight_clock:
            anthro.pop(given, None)
        else:
            anthro[given] = window[given]
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["model"]["control"]["output"]["timestamp_reference"] = "follow"
        anthro = d["sites"][0]["properties"]["anthropogenic_emissions"]
        anthro.pop("startdls", None)
        anthro.pop("enddls", None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        d["sites"][0]["properties"]["timezone"] = {"value": 1}
        d["model"]["control"]["output"]["timestamp_reference"] = (
            "daylight" if daylight_clock else "follow"
        )
        anthro = d["sites"][0]["properties"]["anthropogenic_emissions"]
        anthro["startdls"] = window["startdls"]
        anthro["enddls"] = window["enddls"]
        return d

    if daylight_clock:
        suffix = "[daylight]"
        method = "output.timestamp_reference='daylight'"
        pattern = r"timestamp_reference='daylight' requires"
    else:
        suffix = f"[paired_with_{given}]"
        method = f"{given} is given"
        pattern = r"must both be set or both be None"

    return RequiredRuleSpec(
        case_id=f"AnthropogenicEmissions.{missing}{suffix}",
        model_name="AnthropogenicEmissions",
        field_name=missing,
        active_method=method,
        error_pattern=pattern,
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


ACTIVE_FROM_YAML_SPECS: tuple[RequiredRuleSpec, ...] = (
    *(_make_conductance_spec(field) for field in CONDUCTANCE_REQUIRED),
    _make_lai_spec("lai_max", modelled_only=False),
    *(
        _make_lai_spec(field, modelled_only=True)
        for field in LAI_CALCULATED_ONLY_REQUIRED
    ),
    _make_surface_geom_spec(
        "BldgsProperties",
        "bldgs",
        "bldgh",
        error_pattern=r"land_cover\.bldgs: bldgh is None",
    ),
    _make_surface_geom_spec(
        "BldgsProperties",
        "bldgs",
        "faibldg",
        case_suffix="[fai_observed]",
        fai_mode="observed",
        error_pattern=r"land_cover\.bldgs: faibldg is None",
    ),
    _make_surface_geom_spec(
        "BldgsProperties",
        "bldgs",
        "faibldg",
        case_suffix="[rsl_variable]",
        fai_mode="modelled",
        rsl_mode="variable",
        error_pattern=r"bldgs\.faibldg must be set",
    ),
    _make_surface_geom_spec(
        "EvetrProperties",
        "evetr",
        "height_evergreen_tree",
        error_pattern=r"land_cover\.evetr: height_evergreen_tree is None",
    ),
    _make_surface_geom_spec(
        "EvetrProperties",
        "evetr",
        "fai_evergreen_tree",
        fai_mode="observed",
        error_pattern=r"land_cover\.evetr: fai_evergreen_tree is None",
    ),
    _make_surface_geom_spec(
        "DectrProperties",
        "dectr",
        "height_deciduous_tree",
        error_pattern=r"land_cover\.dectr: height_deciduous_tree is None",
    ),
    _make_surface_geom_spec(
        "DectrProperties",
        "dectr",
        "fai_deciduous_tree",
        fai_mode="observed",
        error_pattern=r"land_cover\.dectr: fai_deciduous_tree is None",
    ),
    *(_make_thermal_layer_spec(field) for field in DYOHM_BUILDING_LAYERS_REQUIRED),
    _make_shading_spec("reduction_factor_shading", active_mode=1, inactive_mode=0),
    _make_shading_spec("temperature_threshold_shading", active_mode=2, inactive_mode=1),
    _make_shading_spec("radiation_threshold_shading", active_mode=2, inactive_mode=1),
    _make_co2_point_source_spec(),
    _make_dls_spec("startdls", "enddls", daylight_clock=True),
    _make_dls_spec("enddls", "startdls", daylight_clock=True),
    _make_dls_spec("startdls", "enddls", daylight_clock=False),
    _make_dls_spec("enddls", "startdls", daylight_clock=False),
)


def test_doc_required_when_spec_table_covers_entire_registry() -> None:
    """Every ``DOC_REQUIRED_WHEN`` field is covered either directly or in the pending ledger."""
    # ARRANGE
    baseline = _load_baseline()
    registry_pairs = {
        f"{model_name}.{field}"
        for model_name, fields in DOC_REQUIRED_WHEN.items()
        for field in fields
    }
    direct_pairs = {
        f"{spec.model_name}.{spec.field_name}" for spec in ACTIVE_FROM_YAML_SPECS
    }
    pending_pairs = set(baseline["doc_required_when_pending_from_yaml"])

    # ACT / ASSERT
    assert not (direct_pairs & pending_pairs), (
        "Fields covered directly via SUEWSConfig.from_yaml must be removed from "
        f"doc_required_when_pending_from_yaml: {sorted(direct_pairs & pending_pairs)}"
    )
    uncovered = sorted(registry_pairs - direct_pairs - pending_pairs)
    stale = sorted((direct_pairs | pending_pairs) - registry_pairs)
    assert not uncovered, (
        "New DOC_REQUIRED_WHEN entries lack end-to-end specs in "
        f"test_schema_contracts.py: {uncovered}"
    )
    assert not stale, f"Stale specs no longer present in DOC_REQUIRED_WHEN: {stale}"


@pytest.mark.parametrize("spec", ACTIVE_FROM_YAML_SPECS, ids=lambda s: s.case_id)
def test_doc_required_when_end_to_end_from_yaml(
    spec: RequiredRuleSpec, compact_sample_dict: dict[str, Any], tmp_path: Path
) -> None:
    """Active missing raises ``ValueError`` (never warns); inactive & valid succeed."""
    # ACT / ASSERT 1: active condition + missing value -> ValueError, zero UserWarnings
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(ValueError, match=spec.error_pattern):
            _load_from_yaml(
                spec.build_active_missing(compact_sample_dict),
                tmp_path,
                "active_missing.yml",
            )
    user_warnings = [
        str(w.message) for w in caught if issubclass(w.category, UserWarning)
    ]
    assert not user_warnings, (
        f"{spec.case_id} ({spec.active_method}) emitted UserWarning instead of "
        f"clean ValueError: {user_warnings}"
    )

    # ACT / ASSERT 2: inactive condition + missing value -> succeeds cleanly
    cfg_inactive = _load_from_yaml(
        spec.build_inactive_missing(compact_sample_dict),
        tmp_path,
        "inactive_missing.yml",
    )
    assert isinstance(cfg_inactive, SUEWSConfig)

    # ACT / ASSERT 3: active condition + explicit valid value -> succeeds cleanly
    cfg_valid = _load_from_yaml(
        spec.build_active_valid(compact_sample_dict),
        tmp_path,
        "active_valid.yml",
    )
    assert isinstance(cfg_valid, SUEWSConfig)


@pytest.mark.parametrize(
    ("surface_name", "surface_type", "missing_field"),
    [
        ("paved", SurfaceType.PAVED, "to_bldgs"),
        ("paved", SurfaceType.PAVED, "to_dectr"),
        ("paved", SurfaceType.PAVED, "to_evetr"),
        ("paved", SurfaceType.PAVED, "to_grass"),
        ("paved", SurfaceType.PAVED, "to_bsoil"),
        ("paved", SurfaceType.PAVED, "to_water"),
        ("paved", SurfaceType.PAVED, "to_runoff"),
        ("bldgs", SurfaceType.BLDGS, "to_paved"),
        ("grass", SurfaceType.GRASS, "to_soilstore"),
    ],
)
def test_water_distribution_required_fields_contract_and_pr2117_ratchet(
    surface_name: str,
    surface_type: SurfaceType,
    missing_field: str,
    compact_sample_dict: dict[str, Any],
    tmp_path: Path,
) -> None:
    """Exercise ``WaterDistribution`` valid/omitted YAML and ratchet PR #2117 handoff.

    Every ``WaterDistribution`` destination in ``DOC_REQUIRED_WHEN`` is tested
    for: (a) direct ``validate_distribution`` rejection when missing,
    (b) omitted ``waterdist`` block taking surface-specific defaults via
    ``SUEWSConfig.from_yaml``, (c) explicit valid ``waterdist`` succeeding via
    ``SUEWSConfig.from_yaml``, and (d) explicit check against the pending
    PR #2117 ledger entry so once #2117 lands the pending entry must be removed
    and promoted into ``ACTIVE_FROM_YAML_SPECS``.
    """
    # ARRANGE: direct WaterDistribution model rejects any missing destination
    required = WATER_DISTRIBUTION_REQUIRED[surface_name]
    complete = dict.fromkeys(required, 0.0)
    complete[required[-1]] = 1.0
    incomplete = {k: v for k, v in complete.items() if k != missing_field}

    wd = WaterDistribution(**incomplete)
    with pytest.raises(ValueError, match=rf"Missing required field {missing_field}"):
        wd.validate_distribution(surface_type)

    # Inactive: omitted waterdist block takes surface-specific defaults cleanly
    d_omitted = copy.deepcopy(compact_sample_dict)
    d_omitted["sites"][0]["properties"]["land_cover"][surface_name].pop(
        "waterdist", None
    )
    cfg_omitted = _load_from_yaml(d_omitted, tmp_path, "wd_omitted.yml")
    wd_loaded = getattr(
        cfg_omitted.sites[0].properties.land_cover, surface_name
    ).waterdist
    assert getattr(wd_loaded, missing_field) is not None

    # Explicit valid waterdist succeeds via SUEWSConfig.from_yaml
    d_valid = copy.deepcopy(compact_sample_dict)
    d_valid["sites"][0]["properties"]["land_cover"][surface_name]["waterdist"] = {
        k: {"value": v} for k, v in complete.items()
    }
    cfg_valid = _load_from_yaml(d_valid, tmp_path, "wd_valid.yml")
    assert isinstance(cfg_valid, SUEWSConfig)

    # Pending PR #2117 ratchet check: once from_yaml propagates ValueError,
    # remove WaterDistribution.* from doc_required_when_pending_from_yaml.
    baseline = _load_baseline()
    key = f"WaterDistribution.{missing_field}"
    assert key in baseline["doc_required_when_pending_from_yaml"]
    assert baseline["doc_required_when_pending_from_yaml"][key]["owner_pr"] == "#2117"
    d_missing = copy.deepcopy(compact_sample_dict)
    d_missing["sites"][0]["properties"]["land_cover"][surface_name]["waterdist"] = {
        k: {"value": v} for k, v in incomplete.items()
    }
    try:
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            _load_from_yaml(d_missing, tmp_path, "wd_missing.yml")
    except ValueError as exc:
        pytest.fail(
            f"{key} now raises ValueError through SUEWSConfig.from_yaml ({exc}); "
            "remove it from doc_required_when_pending_from_yaml and promote it "
            "into ACTIVE_FROM_YAML_SPECS."
        )
    assert any(missing_field in str(w.message) for w in caught)


@pytest.mark.parametrize(
    ("ie_start", "ie_end"),
    [(120.0, None), (None, 240.0), (120.0, 0.0), (0.0, 240.0)],
)
def test_irrigation_window_phase_b_contract_and_from_yaml_controls(
    ie_start: float | None,
    ie_end: float | None,
    compact_sample_dict: dict[str, Any],
    tmp_path: Path,
) -> None:
    """``IrrigationParams`` window is enforced in Phase B; valid & omitted load cleanly."""
    # Active one-sided window -> Phase B ERROR
    results = validate_irrigation_doy(ie_start, ie_end, 51.5, 2012, "KCL")
    assert any(
        r.status == "ERROR"
        and "Both ie_start and ie_end must be specified together" in r.message
        for r in results
    )

    # Inactive (both None or both 0) -> no Phase B ERROR and from_yaml succeeds
    assert not any(
        r.status == "ERROR"
        for r in validate_irrigation_doy(None, None, 51.5, 2012, "KCL")
    )
    d_omitted = copy.deepcopy(compact_sample_dict)
    irr_omitted = d_omitted["sites"][0]["properties"]["irrigation"]
    irr_omitted.pop("ie_start", None)
    irr_omitted.pop("ie_end", None)
    assert isinstance(
        _load_from_yaml(d_omitted, tmp_path, "irr_omitted.yml"), SUEWSConfig
    )

    # Explicit valid window -> no Phase B ERROR and from_yaml succeeds
    assert not any(
        r.status == "ERROR"
        for r in validate_irrigation_doy(140.0, 240.0, 51.5, 2012, "KCL")
    )
    d_valid = copy.deepcopy(compact_sample_dict)
    irr_valid = d_valid["sites"][0]["properties"]["irrigation"]
    irr_valid["ie_start"] = {"value": 140.0}
    irr_valid["ie_end"] = {"value": 240.0}
    assert isinstance(_load_from_yaml(d_valid, tmp_path, "irr_valid.yml"), SUEWSConfig)


# =============================================================================
# 2. Validator warning-swallow AST lint + synthetic unit tests
# =============================================================================


@dataclass(frozen=True)
class WarningSwallowFinding:
    """One Pydantic validator ``except`` handler swallowing errors into a warning."""

    rel_path: str
    qualname: str
    lineno: int
    except_type: str

    @property
    def site_id(self) -> str:
        """Canonical ``path::Class.method`` identifier used in the ledger."""
        return f"{self.rel_path}::{self.qualname}"


def _decorator_name(decorator: ast.expr) -> str:
    """Return the terminal attribute/name of a decorator expression."""
    target = decorator.func if isinstance(decorator, ast.Call) else decorator
    if isinstance(target, ast.Name):
        return target.id
    if isinstance(target, ast.Attribute):
        return target.attr
    return ""


def _is_warn_call(node: ast.AST) -> bool:
    """True when *node* calls ``warnings.warn(...)`` or ``warn(...)``."""
    if not isinstance(node, ast.Call):
        return False
    func = node.func
    if isinstance(func, ast.Attribute):
        return func.attr in WARN_CALL_NAMES
    if isinstance(func, ast.Name):
        return func.id in WARN_CALL_NAMES
    return False


class _Exit(IntFlag):
    """Non-raising control-flow exits reachable from an AST statement block."""

    NONE = 0
    FALL = 1
    RETURN = 2
    BREAK = 4
    CONTINUE = 8


_TRY_NODE_TYPES: tuple[type[ast.AST], ...] = (
    (ast.Try, ast.TryStar) if hasattr(ast, "TryStar") else (ast.Try,)
)


def _expr_has_warn(node: ast.AST | None) -> bool:
    """True when *node* contains a warning call expression."""
    if node is None:
        return False
    return any(_is_warn_call(child) for child in ast.walk(node))


def _stmt_contains_warn(stmt: ast.stmt) -> bool:
    """True when *stmt* contains a warning call outside nested definitions."""
    if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        return any(_expr_has_warn(d) for d in stmt.decorator_list)
    stack: list[ast.AST] = [stmt]
    while stack:
        node = stack.pop()
        if _is_warn_call(node):
            return True
        for child in ast.iter_child_nodes(node):
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                if any(_expr_has_warn(d) for d in child.decorator_list):
                    return True
                continue
            stack.append(child)
    return False


def _seq_flow(
    first: tuple[_Exit, _Exit], second: tuple[_Exit, _Exit]
) -> tuple[_Exit, _Exit]:
    """Compose ``(all_exits, warn_exits)`` for sequential statements."""
    a1, w1 = first
    if _Exit.FALL not in a1:
        return a1, w1
    a2, w2 = second
    all_exits = (a1 & ~_Exit.FALL) | a2
    warn_exits = (w1 & ~_Exit.FALL) | w2 | (a2 if _Exit.FALL in w1 else _Exit.NONE)
    return all_exits, warn_exits


def _is_irrefutable_pattern(pattern: ast.pattern) -> bool:
    """True when a ``match`` pattern unconditionally matches every value."""
    if isinstance(pattern, ast.MatchAs):
        return pattern.pattern is None
    if isinstance(pattern, ast.MatchOr):
        return any(_is_irrefutable_pattern(p) for p in pattern.patterns)
    return False


def _is_constant_true(expr: ast.expr) -> bool:
    """True when *expr* is a literal truthy constant (e.g. ``while True:``)."""
    return isinstance(expr, ast.Constant) and bool(expr.value)


def _analyse_loop(stmt: ast.For | ast.AsyncFor | ast.While) -> tuple[_Exit, _Exit]:
    """Compute non-raising exits for a ``for`` or ``while`` loop."""
    header = stmt.test if isinstance(stmt, ast.While) else stmt.iter
    hdr_flow = (_Exit.FALL, _Exit.FALL if _expr_has_warn(header) else _Exit.NONE)
    ab, wb = _analyse_block(stmt.body)
    ao, wo = _analyse_block(stmt.orelse)
    wb_star = wb | (ab if (wb & (_Exit.FALL | _Exit.CONTINUE)) else _Exit.NONE)
    break_a = _Exit.FALL if _Exit.BREAK in ab else _Exit.NONE
    break_w = _Exit.FALL if _Exit.BREAK in wb_star else _Exit.NONE
    if isinstance(stmt, ast.While) and _is_constant_true(stmt.test):
        loop_a = (ab & _Exit.RETURN) | break_a
        loop_w = (wb_star & _Exit.RETURN) | break_w
    else:
        loop_a = (ab & _Exit.RETURN) | break_a | ao
        loop_w = (
            (wb_star & _Exit.RETURN)
            | break_w
            | wo
            | (ao if (wb_star & (_Exit.FALL | _Exit.CONTINUE)) else _Exit.NONE)
        )
    return _seq_flow(hdr_flow, (loop_a, loop_w))


def _apply_finally_flow(
    try_flow: tuple[_Exit, _Exit],
    finalbody: list[ast.stmt],
    try_has_warn: bool,
) -> tuple[_Exit, _Exit]:
    """Combine exits from a ``try``/``except``/``else`` with its ``finally`` block."""
    at, wt = try_flow
    af, wf = _analyse_block(finalbody)
    non_fall_af = af & ~_Exit.FALL
    non_fall_wf = wf & ~_Exit.FALL
    if _Exit.FALL in af:
        a_out = at | non_fall_af
        w_out = (
            wt
            | non_fall_wf
            | (at if _Exit.FALL in wf else _Exit.NONE)
            | (non_fall_af if (wt != _Exit.NONE or try_has_warn) else _Exit.NONE)
        )
        return a_out, w_out
    return non_fall_af, non_fall_wf | (non_fall_af if try_has_warn else _Exit.NONE)


def _analyse_try(stmt: ast.Try | Any) -> tuple[_Exit, _Exit]:
    """Compute non-raising exits for a nested ``try`` / ``except`` / ``finally``."""
    at, wt = _seq_flow(_analyse_block(stmt.body), _analyse_block(stmt.orelse))
    body_has_warn = any(_stmt_contains_warn(s) for s in stmt.body)
    for handler in stmt.handlers:
        ah, wh = _analyse_block(handler.body)
        at |= ah
        wt |= wh | (ah if body_has_warn else _Exit.NONE)
    if not stmt.finalbody:
        return at, wt
    try_has_warn = (
        body_has_warn
        or any(_stmt_contains_warn(s) for s in stmt.orelse)
        or any(any(_stmt_contains_warn(s) for s in h.body) for h in stmt.handlers)
    )
    return _apply_finally_flow((at, wt), stmt.finalbody, try_has_warn)


def _analyse_match(stmt: ast.Match) -> tuple[_Exit, _Exit]:
    """Compute non-raising exits for a ``match`` / ``case`` statement."""
    hdr = (_Exit.FALL, _Exit.FALL if _expr_has_warn(stmt.subject) else _Exit.NONE)
    all_cases = _Exit.NONE
    warn_cases = _Exit.NONE
    exhaustive = False
    for case in stmt.cases:
        if case.guard is None and _is_irrefutable_pattern(case.pattern):
            exhaustive = True
        guard_hdr = (
            _Exit.FALL,
            _Exit.FALL if _expr_has_warn(case.guard) else _Exit.NONE,
        )
        ac, wc = _seq_flow(guard_hdr, _analyse_block(case.body))
        all_cases |= ac
        warn_cases |= wc
    if not exhaustive:
        all_cases |= _Exit.FALL
    return _seq_flow(hdr, (all_cases, warn_cases))


def _analyse_stmt(stmt: ast.stmt) -> tuple[_Exit, _Exit]:
    """Compute ``(all_exits, warn_exits)`` reachable from one AST statement."""
    if isinstance(stmt, ast.Raise):
        result = (_Exit.NONE, _Exit.NONE)
    elif isinstance(stmt, ast.Return):
        result = (
            _Exit.RETURN,
            _Exit.RETURN if _expr_has_warn(stmt.value) else _Exit.NONE,
        )
    elif isinstance(stmt, ast.Break):
        result = (_Exit.BREAK, _Exit.NONE)
    elif isinstance(stmt, ast.Continue):
        result = (_Exit.CONTINUE, _Exit.NONE)
    elif isinstance(stmt, ast.If):
        hdr = (_Exit.FALL, _Exit.FALL if _expr_has_warn(stmt.test) else _Exit.NONE)
        ab, wb = _analyse_block(stmt.body)
        ao, wo = _analyse_block(stmt.orelse)
        result = _seq_flow(hdr, (ab | ao, wb | wo))
    elif isinstance(stmt, (ast.For, ast.AsyncFor, ast.While)):
        result = _analyse_loop(stmt)
    elif isinstance(stmt, (ast.With, ast.AsyncWith)):
        hdr_warn = any(_expr_has_warn(item.context_expr) for item in stmt.items)
        hdr = (_Exit.FALL, _Exit.FALL if hdr_warn else _Exit.NONE)
        result = _seq_flow(hdr, _analyse_block(stmt.body))
    elif isinstance(stmt, _TRY_NODE_TYPES):
        result = _analyse_try(stmt)
    elif isinstance(stmt, ast.Match):
        result = _analyse_match(stmt)
    else:
        result = (
            _Exit.FALL,
            _Exit.FALL if _stmt_contains_warn(stmt) else _Exit.NONE,
        )
    return result


def _analyse_block(stmts: list[ast.stmt]) -> tuple[_Exit, _Exit]:
    """Compute ``(all_exits, warn_exits)`` reachable from a statement sequence."""
    flow = (_Exit.FALL, _Exit.NONE)
    for stmt in stmts:
        flow = _seq_flow(flow, _analyse_stmt(stmt))
    return flow


def _handler_swallows_warning(handler: ast.ExceptHandler) -> bool:
    """True when *handler* can execute a warning call without raising."""
    _, warn_exits = _analyse_block(handler.body)
    return warn_exits != _Exit.NONE


def find_validator_warning_swallows(
    source: str, rel_path: str
) -> list[WarningSwallowFinding]:
    """Find Pydantic validator ``except`` blocks that downgrade errors to warnings.

    Parameters
    ----------
    source : str
        Python source code to parse.
    rel_path : str
        POSIX path label used in returned findings.

    Returns
    -------
    list of WarningSwallowFinding
        All validator ``except`` handlers with a non-raising path after a
        warning call (including conditional ``warn`` + ``return``/``continue``
        before a trailing ``raise``).
    """
    tree = ast.parse(source, filename=rel_path)
    findings: list[WarningSwallowFinding] = []

    def _visit_scope(stmts: list[ast.stmt], prefix: tuple[str, ...]) -> None:
        for stmt in stmts:
            if isinstance(stmt, ast.ClassDef):
                _visit_scope(stmt.body, (*prefix, stmt.name))
            elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
                dec_names = {_decorator_name(d) for d in stmt.decorator_list}
                qualname = ".".join((*prefix, stmt.name))
                if dec_names & VALIDATOR_DECORATORS:
                    for sub in ast.walk(stmt):
                        if not isinstance(sub, ast.ExceptHandler):
                            continue
                        if _handler_swallows_warning(sub):
                            exc_type = (
                                ast.unparse(sub.type)
                                if sub.type is not None
                                else "bare"
                            )
                            findings.append(
                                WarningSwallowFinding(
                                    rel_path=rel_path,
                                    qualname=qualname,
                                    lineno=sub.lineno,
                                    except_type=exc_type,
                                )
                            )

    _visit_scope(tree.body, ())
    return findings


def _iter_data_model_python_sources() -> list[tuple[str, str]]:
    """Read every ``.py`` file under packaged ``supy.data_model`` via Traversable."""
    root = files("supy").joinpath("data_model")
    collected: list[tuple[str, str]] = []

    def _walk(node: Any, rel_parts: tuple[str, ...]) -> None:
        entries = sorted(node.iterdir(), key=lambda p: p.name)
        for entry in entries:
            if entry.name == "__pycache__":
                continue
            if entry.is_dir():
                _walk(entry, (*rel_parts, entry.name))
            elif entry.is_file() and entry.name.endswith(".py"):
                rel_posix = "/".join((
                    "src",
                    "supy",
                    "data_model",
                    *rel_parts,
                    entry.name,
                ))
                collected.append((rel_posix, entry.read_text(encoding="utf-8")))

    _walk(root, ())
    return collected


def test_no_unreviewed_validator_warning_swallows() -> None:
    """Every ``except: warnings.warn`` in a Pydantic validator must be zero or allowlisted."""
    # ARRANGE
    baseline = _load_baseline()
    allowlist = baseline["validator_warning_swallows"]
    sources = _iter_data_model_python_sources()
    assert sources, "No Python sources found under supy.data_model"

    # ACT
    findings: list[WarningSwallowFinding] = []
    for rel_path, source in sources:
        findings.extend(find_validator_warning_swallows(source, rel_path))

    found_by_site = {f.site_id: f for f in findings}
    expected_sites = set(allowlist)
    actual_sites = set(found_by_site)

    # ASSERT
    unreviewed = sorted(actual_sites - expected_sites)
    stale = sorted(expected_sites - actual_sites)
    assert not unreviewed, (
        "Unreviewed Pydantic validator exception-to-warning swallow site(s) detected: "
        + ", ".join(
            f"{s} (line {found_by_site[s].lineno}, except {found_by_site[s].except_type})"
            for s in unreviewed
        )
    )
    assert not stale, (
        "Legacy validator warning-swallow site(s) were fixed; remove stale entry "
        f"from schema_contract_baseline.json: {stale}"
    )


def test_validator_warning_swallow_ast_lint_synthetic_cases() -> None:
    """Prove the AST lint catches warning swallows and ignores valid patterns."""
    # ARRANGE: offending @model_validator, @field_validator, bare except, and
    # control-flow bypasses (conditional warn+return/continue/fallthrough/finally)
    bad_source = """
import warnings
from warnings import warn
from pydantic import BaseModel, field_validator, model_validator

class BadModel(BaseModel):
    x: float = 1.0

    @model_validator(mode="after")
    def swallow_exception(self):
        try:
            raise ValueError("broken")
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
        return self

    @field_validator("x", mode="after")
    @classmethod
    def swallow_field(cls, v):
        try:
            return float(v)
        except ValueError:
            warn("bad float", UserWarning)
            return v

    @model_validator(mode="before")
    @classmethod
    def swallow_bare(cls, data):
        try:
            return data
        except:
            warnings.warn("bare swallow", UserWarning)
            return data

    @model_validator(mode="after")
    def swallow_conditional_return_before_raise(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            if self.x > 0:
                warnings.warn(str(exc), UserWarning)
                return self
            raise

    @model_validator(mode="after")
    def swallow_warn_then_conditional_return(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            warnings.warn(str(exc), UserWarning)
            if self.x > 0:
                return self
            raise

    @model_validator(mode="after")
    def swallow_conditional_fallthrough(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            if self.x > 0:
                warnings.warn(str(exc), UserWarning)
            else:
                raise
        return self

    @model_validator(mode="after")
    def swallow_loop_continue_before_raise(self):
        for _ in range(1):
            try:
                raise ValueError("broken")
            except ValueError as exc:
                if self.x > 0:
                    warnings.warn(str(exc), UserWarning)
                    continue
                raise
        return self

    @model_validator(mode="after")
    def swallow_finally_return_overrides_raise(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            try:
                warnings.warn(str(exc), UserWarning)
                raise
            finally:
                return self
"""
    # ARRANGE: allowed patterns (unconditional re-raise, conditional warn before
    # unconditional raise, advisory warn outside except, non-validator helper)
    good_source = """
import warnings
from pydantic import BaseModel, model_validator

class GoodModel(BaseModel):
    x: float = 1.0

    @model_validator(mode="after")
    def warn_and_reraise(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            warnings.warn(f"about to raise: {exc}", UserWarning)
            raise

    @model_validator(mode="after")
    def conditional_warn_then_reraise(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            if self.x > 0:
                warnings.warn(f"about to raise: {exc}", UserWarning)
            raise

    @model_validator(mode="after")
    def branch_warn_and_reraise(self):
        try:
            raise ValueError("broken")
        except ValueError as exc:
            if self.x > 0:
                warnings.warn(f"about to raise: {exc}", UserWarning)
                raise
            raise

    @model_validator(mode="before")
    @classmethod
    def advisory_deprecation(cls, data):
        warnings.warn("legacy alias deprecated", DeprecationWarning)
        return data

    def regular_helper(self):
        try:
            return 1 / 0
        except Exception:
            warnings.warn("helper warning", UserWarning)
"""

    # ACT
    bad_findings = find_validator_warning_swallows(bad_source, "synthetic/bad.py")
    good_findings = find_validator_warning_swallows(good_source, "synthetic/good.py")

    # ASSERT
    assert {(f.qualname, f.except_type) for f in bad_findings} == {
        ("BadModel.swallow_exception", "Exception"),
        ("BadModel.swallow_field", "ValueError"),
        ("BadModel.swallow_bare", "bare"),
        ("BadModel.swallow_conditional_return_before_raise", "ValueError"),
        ("BadModel.swallow_warn_then_conditional_return", "ValueError"),
        ("BadModel.swallow_conditional_fallthrough", "ValueError"),
        ("BadModel.swallow_loop_continue_before_raise", "ValueError"),
        ("BadModel.swallow_finally_return_overrides_raise", "ValueError"),
    }
    assert good_findings == []


# =============================================================================
# 3. Classified field-contract ratchets + anti-gaming unit tests
# =============================================================================


def _evaluate_classified_ratchet(
    live_fields: set[str],
    ledger_section: dict[str, Any],
    category_names: tuple[str, ...],
) -> tuple[list[str], list[str]]:
    """Return ``(unexpected_new_fields, stale_removed_fields)`` for a ledger section."""
    recorded: set[str] = set()
    for category in category_names:
        recorded.update(ledger_section[category])
    unexpected = sorted(live_fields - recorded)
    stale = sorted(recorded - live_fields)
    return unexpected, stale


def _check_nullable_preservation(
    protected_keys: set[str],
    field_defaults: dict[str, tuple[bool, bool]],
) -> list[str]:
    """Verify protected nullable/derived fields were not given an invented default.

    ``field_defaults`` maps ``Model.field`` -> ``(is_optional, has_none_default)``.
    """
    violations: list[str] = []
    for key in sorted(protected_keys):
        if key not in field_defaults:
            violations.append(f"{key}: field missing from extracted models")
            continue
        is_opt, has_none_default = field_defaults[key]
        if not is_opt or not has_none_default:
            violations.append(
                f"{key}: expected Optional with default=None, got "
                f"is_optional={is_opt}, has_none_default={has_none_default}"
            )
    return violations


def _extract_field_sets(
    gen: Any, doc_data: dict[str, Any], all_models: dict[str, type]
) -> tuple[set[str], set[str], set[str], dict[str, tuple[bool, bool]]]:
    """Compute live ``STATUS_NO_DEFAULT`` and ``Optional``-with-default field sets."""
    status_no_default: set[str] = set()
    opt_ref_with_default: set[str] = set()
    opt_plain_with_default: set[str] = set()
    nullability_map: dict[str, tuple[bool, bool]] = {}

    for model_name, model_doc in sorted(doc_data["models"].items()):
        model_cls = all_models[model_name]
        for fdoc in model_doc["fields"]:
            fname = fdoc["name"]
            key = f"{model_name}.{fname}"
            finfo = model_cls.model_fields[fname]
            is_opt = bool(fdoc["type_info"]["is_optional"])
            has_none_default = finfo.default is None and finfo.default_factory is None
            nullability_map[key] = (is_opt, has_none_default)

            status = gen.RSTGenerator._format_status(fdoc, model_name)
            if status == gen.STATUS_NO_DEFAULT:
                status_no_default.add(key)

            has_non_none_default = (
                finfo.default is not None and finfo.default is not PydanticUndefined
            ) or finfo.default_factory is not None
            if is_opt and has_non_none_default:
                if "RefValue" in fdoc["type"]:
                    opt_ref_with_default.add(key)
                else:
                    opt_plain_with_default.add(key)

    return (
        status_no_default,
        opt_ref_with_default,
        opt_plain_with_default,
        nullability_map,
    )


def test_schema_contract_ledger_integrity() -> None:
    """Every ledger category count, group tag, and conditional pointer is consistent."""
    # ARRANGE
    baseline = _load_baseline()
    groups = baseline["groups"]
    doc_required_keys = {f"{m}.{f}" for m, fs in DOC_REQUIRED_WHEN.items() for f in fs}

    sections = (
        (
            "status_no_default",
            ("derived_runtime", "legitimately_optional_nullable", "unclassified_debt"),
        ),
        (
            "optional_ref_with_default",
            (
                "documented_conditional",
                "legitimately_optional_nullable",
                "unclassified_debt",
            ),
        ),
        (
            "optional_plain_with_default",
            ("legitimately_optional_nullable", "unclassified_debt"),
        ),
    )

    # ACT / ASSERT
    for section_name, categories in sections:
        section = baseline[section_name]
        counts = section["summary_counts"]
        seen: set[str] = set()
        for category in categories:
            entries = section[category]
            assert counts[category] == len(entries), (
                f"{section_name}.summary_counts.{category} drifted: "
                f"{counts[category]} != {len(entries)}"
            )
            overlap = seen & set(entries)
            assert not overlap, (
                f"Duplicate entries across {section_name}: {sorted(overlap)}"
            )
            seen.update(entries)
            for field_key, group_tag in entries.items():
                assert group_tag in groups, (
                    f"{section_name}.{category}.{field_key} cites unknown group {group_tag!r}"
                )
                group_meta = groups[group_tag]
                assert group_meta.get("reason"), f"Group {group_tag!r} missing reason"
                if category == "unclassified_debt":
                    assert group_meta.get("owner_issue"), (
                        f"Debt group {group_tag!r} must record owner_issue"
                    )
        assert counts["total"] == len(seen), (
            f"{section_name}.summary_counts.total drifted: {counts['total']} != {len(seen)}"
        )

    for cond_key in baseline["optional_ref_with_default"]["documented_conditional"]:
        assert cond_key in doc_required_keys, (
            f"documented_conditional field {cond_key} is not in DOC_REQUIRED_WHEN"
        )


def test_status_no_default_classified_ratchet(doc_snapshot) -> None:
    """Ratchet ``STATUS_NO_DEFAULT`` strictly downward without inventing static defaults."""
    # ARRANGE
    gen, doc_data, all_models = doc_snapshot
    baseline = _load_baseline()
    section = baseline["status_no_default"]
    categories = (
        "derived_runtime",
        "legitimately_optional_nullable",
        "unclassified_debt",
    )

    # ACT
    live_no_default, _, _, nullability_map = _extract_field_sets(
        gen, doc_data, all_models
    )
    unexpected, stale = _evaluate_classified_ratchet(
        live_no_default, section, categories
    )
    protected = set(section["derived_runtime"]) | set(
        section["legitimately_optional_nullable"]
    )
    invented_defaults = _check_nullable_preservation(protected, nullability_map)

    # ASSERT
    assert not unexpected, (
        "New unclassified STATUS_NO_DEFAULT field(s) added; document a validator "
        "rule (DOC_REQUIRED_WHEN), runtime derivation (DOC_DERIVED_DEFAULT), or "
        f"explicitly classify in schema_contract_baseline.json: {unexpected}"
    )
    assert not stale, (
        "STATUS_NO_DEFAULT count shrank -- thank you! Lower "
        f"schema_contract_baseline.json by removing graduated field(s): {stale}"
    )
    assert not invented_defaults, (
        "Protected derived_runtime / legitimately_optional_nullable fields must "
        "not be replaced with invented static defaults: "
        f"{invented_defaults}"
    )


def test_optional_with_default_classified_ratchets(doc_snapshot) -> None:
    """Classify all ``Optional``-with-default fields and ratchet debt downward."""
    # ARRANGE
    gen, doc_data, all_models = doc_snapshot
    baseline = _load_baseline()

    # ACT
    _, live_opt_ref, live_opt_plain, _ = _extract_field_sets(gen, doc_data, all_models)
    ref_unexpected, ref_stale = _evaluate_classified_ratchet(
        live_opt_ref,
        baseline["optional_ref_with_default"],
        (
            "documented_conditional",
            "legitimately_optional_nullable",
            "unclassified_debt",
        ),
    )
    plain_unexpected, plain_stale = _evaluate_classified_ratchet(
        live_opt_plain,
        baseline["optional_plain_with_default"],
        ("legitimately_optional_nullable", "unclassified_debt"),
    )

    # ASSERT
    assert not ref_unexpected, (
        "New unclassified Optional[FlexibleRefValue]-with-default field(s): "
        f"{ref_unexpected}"
    )
    assert not ref_stale, (
        "Optional[FlexibleRefValue]-with-default count shrank; lower "
        f"schema_contract_baseline.json by removing: {ref_stale}"
    )
    assert not plain_unexpected, (
        f"New unclassified plain Optional-with-default field(s): {plain_unexpected}"
    )
    assert not plain_stale, (
        "Plain Optional-with-default count shrank; lower "
        f"schema_contract_baseline.json by removing: {plain_stale}"
    )

    # Behavioural witness that legitimately_optional_nullable fields still accept None
    assert ForcingControl(file=None).file is None
    roof_state = SurfaceInitialState(
        snowfrac=None, snowpack=None, icefrac=None, snowwater=None, snowdens=None
    )
    assert roof_state.snowfrac is None
    assert GrassProperties(min_res_bioco2=None).min_res_bioco2 is None


def test_classified_ratchet_rejects_new_debt_and_invented_defaults() -> None:
    """Synthetic anti-gaming check: new debt and invented defaults both fail."""
    # ARRANGE
    ledger_section = {
        "derived_runtime": {"OutputControl.dir": "derived_output_defaults"},
        "legitimately_optional_nullable": {
            "OutputControl.stream_chunk": "optional_stream_chunk"
        },
        "unclassified_debt": {"OHMCoefficients.a1": "debt_ohm_coefficients"},
    }
    categories = (
        "derived_runtime",
        "legitimately_optional_nullable",
        "unclassified_debt",
    )

    # ACT / ASSERT 1: adding a new unclassified field fails
    unexpected, stale = _evaluate_classified_ratchet(
        {
            "OutputControl.dir",
            "OutputControl.stream_chunk",
            "OHMCoefficients.a1",
            "NewModel.unreviewed_field",
        },
        ledger_section,
        categories,
    )
    assert unexpected == ["NewModel.unreviewed_field"]
    assert stale == []

    # ACT / ASSERT 2: removing debt surfaces stale entry so baseline must shrink
    unexpected, stale = _evaluate_classified_ratchet(
        {"OutputControl.dir", "OutputControl.stream_chunk"},
        ledger_section,
        categories,
    )
    assert unexpected == []
    assert stale == ["OHMCoefficients.a1"]

    # ACT / ASSERT 3: stuffing a literal default onto a protected nullable field
    # fails the nullability preservation check rather than passing as "progress"
    violations = _check_nullable_preservation(
        {"OutputControl.stream_chunk"},
        {"OutputControl.stream_chunk": (True, False)},
    )
    assert violations == [
        "OutputControl.stream_chunk: expected Optional with default=None, "
        "got is_optional=True, has_none_default=False"
    ]


# =============================================================================
# 4. Runtime-derived field contracts (alb_id, lai_max, store_cap, output.freq)
# =============================================================================


def test_derived_vegetation_alb_id_numerical_direction_and_doc_parity(
    compact_sample_dict: dict[str, Any],
    generated_rst_dir: Path,
    tmp_path: Path,
) -> None:
    """Verify non-circular tree vs grass ``alb_id`` derivation and RST status parity."""

    # ARRANGE: explicit bounds so ratio is exact:
    # lai_min=0.5, lai_max=5.5 -> span=5.0; alb_min=0.10, alb_max=0.30 -> span=0.20
    # At lai_id=1.0: ratio=0.1 -> tree=0.12, grass=0.28
    # At lai_id=4.0: ratio=0.7 -> tree=0.24, grass=0.16
    def _run_case(lai_id: float, explicit_alb: float | None = None) -> SUEWSConfig:
        d = copy.deepcopy(compact_sample_dict)
        for surf in ("evetr", "dectr", "grass"):
            lc_surf = d["sites"][0]["properties"]["land_cover"][surf]
            lc_surf["alb_min"] = {"value": 0.10}
            lc_surf["alb_max"] = {"value": 0.30}
            lc_surf["lai"]["lai_min"] = {"value": 0.5}
            lc_surf["lai"]["lai_max"] = {"value": 5.5}
            state = d["sites"][0]["initial_states"][surf]
            state["lai_id"] = {"value": lai_id}
            if explicit_alb is None:
                state.pop("alb_id", None)
            else:
                state["alb_id"] = {"value": explicit_alb}
        return _load_from_yaml(d, tmp_path, f"alb_{lai_id}_{explicit_alb}.yml")

    # ACT
    cfg_low = _run_case(lai_id=1.0)
    cfg_high = _run_case(lai_id=4.0)
    cfg_explicit = _run_case(lai_id=4.0, explicit_alb=0.19)

    evetr_low = _unwrap(cfg_low.sites[0].initial_states.evetr.alb_id)
    evetr_high = _unwrap(cfg_high.sites[0].initial_states.evetr.alb_id)
    dectr_low = _unwrap(cfg_low.sites[0].initial_states.dectr.alb_id)
    dectr_high = _unwrap(cfg_high.sites[0].initial_states.dectr.alb_id)
    grass_low = _unwrap(cfg_low.sites[0].initial_states.grass.alb_id)
    grass_high = _unwrap(cfg_high.sites[0].initial_states.grass.alb_id)

    # ASSERT: exact numerical formula and opposite tree vs grass directions
    assert evetr_low == pytest.approx(0.12)
    assert evetr_high == pytest.approx(0.24)
    assert dectr_low == pytest.approx(0.12)
    assert dectr_high == pytest.approx(0.24)
    assert grass_low == pytest.approx(0.28)
    assert grass_high == pytest.approx(0.16)
    assert evetr_high > evetr_low
    assert dectr_high > dectr_low
    assert grass_high < grass_low

    # Explicit alb_id is preserved unchanged
    assert _unwrap(cfg_explicit.sites[0].initial_states.evetr.alb_id) == pytest.approx(
        0.19
    )
    assert _unwrap(cfg_explicit.sites[0].initial_states.grass.alb_id) == pytest.approx(
        0.19
    )

    # Doc parity: rendered RST status names lai_min/lai_max and direction
    for model_name, direction_token in (
        ("InitialStateEvetr", "increasing with LAI"),
        ("InitialStateDectr", "increasing with LAI"),
        ("InitialStateGrass", "decreasing with LAI"),
        ("InitialStateVeg", "trees increase with LAI"),
    ):
        block = _option_blocks(
            (generated_rst_dir / f"{model_name.lower()}.rst").read_text(
                encoding="utf-8"
            )
        )["alb_id"]
        status = _status_line(block)
        assert "lai_min" in status and "lai_max" in status, status
        assert "alb_min" in status and "alb_max" in status, status
        assert direction_token in status, status
        assert "not checked by the validator" not in status.lower(), status


def test_lai_max_fallback_contract_active_vs_inactive(
    compact_sample_dict: dict[str, Any], tmp_path: Path
) -> None:
    """Active vegetation rejects omitted ``lai_max``; inactive surfaces use ``LAIMAX_DF_DEFAULT``.

    Pins the known split between Python's ``LAIParams.LAIMAX_DF_DEFAULT``
    (``10.0``, used by ``set_default_vegetation_albedo`` and ``to_df_state``)
    and ``config_to_engine_yaml`` omitting ``lai_max`` (leaving
    ``yaml_config.rs`` at its codec default when inactive; Refs #1991, #2002).
    """
    # ARRANGE 1: active grass with omitted lai_max -> rejected at load time
    d_active = copy.deepcopy(compact_sample_dict)
    _set_surface_fractions(d_active, paved=0.75, grass=0.25)
    d_active["sites"][0]["properties"]["land_cover"]["grass"]["lai"].pop(
        "lai_max", None
    )
    with pytest.raises(ValueError, match=r"land_cover\.grass\.lai: lai_max is None"):
        _load_from_yaml(d_active, tmp_path, "lai_max_active.yml")

    # ARRANGE 2: inactive grass & evetr (sfr=0) with omitted lai_max & alb_id
    d_inactive = copy.deepcopy(compact_sample_dict)
    _set_surface_fractions(d_inactive, paved=1.0)
    for surf in ("evetr", "grass"):
        lc_surf = d_inactive["sites"][0]["properties"]["land_cover"][surf]
        lc_surf["alb_min"] = {"value": 0.10}
        lc_surf["alb_max"] = {"value": 0.30}
        lc_surf["lai"]["lai_min"] = {"value": 0.0}
        lc_surf["lai"].pop("lai_max", None)
        state = d_inactive["sites"][0]["initial_states"][surf]
        state["lai_id"] = {"value": 2.5}
        state.pop("alb_id", None)

    # ACT
    cfg = _load_from_yaml(d_inactive, tmp_path, "lai_max_inactive.yml")
    df_state = cfg.to_df_state()
    config_to_engine_yaml = importlib.import_module(
        "supy._run_rust"
    ).config_to_engine_yaml
    engine_yaml = yaml.safe_load(config_to_engine_yaml(cfg))

    # ASSERT: ratio = 2.5 / LAIMAX_DF_DEFAULT(10.0) = 0.25
    assert float(LAIParams.LAIMAX_DF_DEFAULT) == pytest.approx(10.0)
    assert _unwrap(cfg.sites[0].initial_states.evetr.alb_id) == pytest.approx(0.15)
    assert _unwrap(cfg.sites[0].initial_states.grass.alb_id) == pytest.approx(0.25)
    # to_df_state materialises LAIMAX_DF_DEFAULT (veg_idx: 0=evetr, 2=grass)
    assert float(df_state.iloc[0]["laimax", "(0,)"]) == pytest.approx(10.0)
    assert float(df_state.iloc[0]["laimax", "(2,)"]) == pytest.approx(10.0)
    # config_to_engine_yaml omits None lai_max (so yaml_config.rs uses its default)
    lc_engine = engine_yaml["sites"][0]["properties"]["land_cover"]
    assert "lai_max" not in lc_engine["grass"]["lai"]
    assert "laimax" not in lc_engine["grass"]["lai"]


def test_derived_store_cap_resolves_from_store_max_and_doc_parity(
    generated_rst_dir: Path,
) -> None:
    """Omitted ``store_cap`` resolves to ``store_max``; explicit values are preserved."""
    # ARRANGE / ACT
    derived = StorageDrainParams(store_max=0.73)
    explicit_scalar = StorageDrainParams(store_max=0.73, store_cap=0.41)
    explicit_ref = StorageDrainParams(store_max=0.73, store_cap=RefValue(0.41))

    def _dumped_store_cap(model: StorageDrainParams) -> float:
        raw_val = model.model_dump(exclude_none=True, mode="json")["store_cap"]
        return float(raw_val["value"] if isinstance(raw_val, dict) else raw_val)

    # ASSERT: runtime resolution
    assert _dumped_store_cap(derived) == pytest.approx(0.73)
    assert _dumped_store_cap(explicit_scalar) == pytest.approx(0.41)
    assert _dumped_store_cap(explicit_ref) == pytest.approx(0.41)

    # ASSERT: doc status matches
    block = _option_blocks(
        (generated_rst_dir / "storagedrainparams.rst").read_text(encoding="utf-8")
    )["store_cap"]
    status = _status_line(block)
    assert "store_max" in status, status
    assert "not checked by the validator" not in status.lower(), status


def _run_window_with_output_freq(
    raw_base: dict[str, Any], weather: Any, freq: int | None, case_dir: Path
) -> tuple[Any, bytes]:
    """Run *weather* with *freq* configured and return ``(output, saved_bytes)``."""
    raw = copy.deepcopy(raw_base)
    output_block = raw["model"]["control"]["output"]
    if freq is None:
        output_block.pop("freq", None)
    else:
        output_block["freq"] = freq
    case_dir.mkdir()
    cfg_path = case_dir / "input.yml"
    cfg_path.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    sim = SUEWSSimulation()
    sim.update_config(cfg_path, auto_load_forcing=False)
    sim.update_forcing(weather)
    out = sim.run(start_date=weather.index[0], end_date=weather.index[-1], n_jobs=1)
    out.save(case_dir, format="txt", groups=["SUEWS"])
    (saved_file,) = case_dir.glob("*_SUEWS_*.txt")
    return out, saved_file.read_bytes()


def test_output_freq_native_hourly_fallback(
    short_sample_yaml_path: Path,
    one_day_short_forcing,
    tmp_path: Path,
) -> None:
    """Native output with omitted ``output.freq`` falls back to 3600 s (hourly)."""
    # ARRANGE: two-hour slice (24 five-minute steps) -> 2 hourly / 4 half-hourly rows
    weather = one_day_short_forcing.iloc[:24].copy()
    raw_base = yaml.safe_load(short_sample_yaml_path.read_text(encoding="utf-8"))

    # ACT
    saved_native: dict[int | None, bytes] = {}
    for freq, expected_rows in ((None, 2), (1800, 4), (3600, 2)):
        _, data = _run_window_with_output_freq(
            raw_base, weather, freq, tmp_path / f"native_{freq}"
        )
        assert len(data.splitlines()) == expected_rows + 1
        saved_native[freq] = data

    # ASSERT: omitted freq equals explicit 3600 s and differs from 1800 s
    assert saved_native[None] == saved_native[3600]
    assert saved_native[None] != saved_native[1800]


def test_output_freq_resampled_preserves_held_interval_and_doc_parity(
    short_sample_yaml_path: Path,
    one_day_short_forcing,
    generated_rst_dir: Path,
    tmp_path: Path,
) -> None:
    """Fixed-frequency resampled output keeps its held interval and matches docs.

    Complements ``test_output_freq_native_hourly_fallback`` and mirrors
    ``test/io_tests/test_save_resampled_output.py::test_fixed_resample_saves_at_held_interval``.
    """
    # ARRANGE: one full day (288 five-minute steps)
    weather = one_day_short_forcing.copy()
    raw_base = yaml.safe_load(short_sample_yaml_path.read_text(encoding="utf-8"))

    # ACT / ASSERT 1: fixed-frequency resampled output preserves 1-day interval
    for freq in (None, 3600):
        out, _ = _run_window_with_output_freq(
            raw_base, weather, freq, tmp_path / f"day_{freq}"
        )
        resamp_dir = tmp_path / f"resampled_{freq}"
        paths = out.resample("D").save(resamp_dir, format="txt", groups=["SUEWS"])
        texts = sorted(p.name for p in paths if p.suffix == ".txt")
        assert texts == ["KCL1_2012_SUEWS_1440.txt"]
        assert len((resamp_dir / texts[0]).read_bytes().splitlines()) == 2

    # ACT / ASSERT 2: rendered RST status agrees with both runtime branches
    assert "freq" in DOC_DERIVED_DEFAULT["OutputControl"]
    block = _option_blocks(
        (generated_rst_dir / "outputcontrol.rst").read_text(encoding="utf-8")
    )["freq"]
    status = _status_line(block)
    assert re.search(r"3,?600|hourly", status, re.IGNORECASE), status
    assert "held interval" in status.lower(), status
    assert "not checked by the validator" not in status.lower(), status
