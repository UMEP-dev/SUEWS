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
from collections import Counter
from collections.abc import Callable
import copy
from dataclasses import dataclass
from enum import IntFlag
import hashlib
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
    SOIL_OBSERVED_REQUIRED,
    SOIL_REQUIRED,
    WATER_DISTRIBUTION_REQUIRED,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.core,
    pytest.mark.contract("C-SCHEMA", "C-VALID", "C-TOOLING"),
]

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
    build_alt_inactive_missing: Callable[[dict[str, Any]], dict[str, Any]] | None = None


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


NON_WATER_SOIL_MODELS: tuple[tuple[str, str], ...] = (
    # SurfaceProperties is the shared base model in DOC_REQUIRED_WHEN; map it
    # to a concrete non-water surface (bsoil) alongside all six concrete subclasses.
    ("SurfaceProperties", "bsoil"),
    ("PavedProperties", "paved"),
    ("BldgsProperties", "bldgs"),
    ("EvetrProperties", "evetr"),
    ("DectrProperties", "dectr"),
    ("GrassProperties", "grass"),
    ("BsoilProperties", "bsoil"),
)


def _make_soil_required_spec(
    model_name: str, surface: str, field: str
) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for a ``SOIL_REQUIRED`` field."""
    other = "grass" if surface == "paved" else "paved"

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 0.75, surface: 0.25})
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def inactive_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 1.0})
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 0.75, surface: 0.25})
        return d

    return RequiredRuleSpec(
        case_id=f"{model_name}.{field}",
        model_name=model_name,
        field_name=field,
        active_method=f"{surface}.sfr > 0",
        error_pattern=rf"land_cover\.{re.escape(surface)}: {re.escape(field)} is None",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_missing,
        build_active_valid=active_valid,
    )


def _make_soil_field_capacity_spec(
    model_name: str, surface: str, field: str
) -> RequiredRuleSpec:
    """Build active/inactive/valid YAML cases for ``soil_field_capacity``.

    Required when ``surface.sfr > 0`` and ``model.physics.soil_moisture`` is
    observed (``1``); lawfully omitted under default modelled soil moisture
    (``0``) or when ``surface.sfr == 0``.
    """
    other = "grass" if surface == "paved" else "paved"

    def _enable_observed_sm(d: dict[str, Any]) -> None:
        d["model"]["physics"]["soil_moisture"] = {"value": 1}
        props = d["sites"][0]["properties"]
        props["soil_observation"] = {
            "layer_depth": {"value": 0.35},
            "layer_top": {"value": 0.0},
        }
        props["land_cover"][other][field] = {"value": 0.45}

    def active_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 0.75, surface: 0.25})
        _enable_observed_sm(d)
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def inactive_modelled_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 0.75, surface: 0.25})
        d["model"]["physics"]["soil_moisture"] = "modelled"
        d["sites"][0]["properties"].pop("soil_observation", None)
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def inactive_zero_sfr_missing(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 1.0})
        _enable_observed_sm(d)
        d["sites"][0]["properties"]["land_cover"][surface].pop(field, None)
        return d

    def active_valid(base: dict[str, Any]) -> dict[str, Any]:
        d = copy.deepcopy(base)
        _set_surface_fractions(d, **{other: 0.75, surface: 0.25})
        _enable_observed_sm(d)
        d["sites"][0]["properties"]["land_cover"][surface][field] = {"value": 0.45}
        return d

    return RequiredRuleSpec(
        case_id=f"{model_name}.{field}",
        model_name=model_name,
        field_name=field,
        active_method=f"{surface}.sfr > 0 + soil_moisture=1 (OBSERVED)",
        error_pattern=rf"soil_field_capacity.*{re.escape(surface)} \(missing;",
        build_active_missing=active_missing,
        build_inactive_missing=inactive_modelled_missing,
        build_active_valid=active_valid,
        build_alt_inactive_missing=inactive_zero_sfr_missing,
    )


ROUTING_RULES = (
    *(("paved", k) for k in WATER_DISTRIBUTION_REQUIRED["paved"]),
    ("bldgs", "to_paved"),
    ("grass", "to_soilstore"),
)


def _routing_spec(surface: str, field: str) -> RequiredRuleSpec:
    def prepare(base: dict[str, Any], active: bool, missing: bool) -> dict[str, Any]:
        data = copy.deepcopy(base)
        other = "grass" if surface == "paved" else "paved"
        _set_surface_fractions(
            data, **{surface: 0.5 if active else 0.0, other: 0.5 if active else 1.0}
        )
        required = WATER_DISTRIBUTION_REQUIRED[surface]
        values = dict.fromkeys(required, 0.0)
        values[next(k for k in required if k != field)] = 1.0
        if missing:
            del values[field]
        data["sites"][0]["properties"]["land_cover"][surface]["waterdist"] = values
        return data

    return RequiredRuleSpec(
        case_id=f"WaterDistribution.{field}[{surface}]",
        model_name="WaterDistribution",
        field_name=field,
        active_method=f"{surface}.sfr > 0",
        error_pattern=rf"Missing required field {field} for {surface}",
        build_active_missing=lambda base: prepare(base, True, True),
        build_inactive_missing=lambda base: prepare(base, False, True),
        build_active_valid=lambda base: prepare(base, True, False),
    )


ACTIVE_FROM_YAML_SPECS: tuple[RequiredRuleSpec, ...] = (
    *(_routing_spec(surface, field) for surface, field in ROUTING_RULES),
    *(_make_conductance_spec(field) for field in CONDUCTANCE_REQUIRED),
    _make_lai_spec("lai_max", modelled_only=False),
    *(
        _make_lai_spec(field, modelled_only=True)
        for field in LAI_CALCULATED_ONLY_REQUIRED
    ),
    *(
        _make_soil_required_spec(model_name, surface, field)
        for model_name, surface in NON_WATER_SOIL_MODELS
        for field in SOIL_REQUIRED
    ),
    *(
        _make_soil_field_capacity_spec(model_name, surface, field)
        for model_name, surface in NON_WATER_SOIL_MODELS
        for field in SOIL_OBSERVED_REQUIRED
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

    if spec.build_alt_inactive_missing is not None:
        cfg_alt_inactive = _load_from_yaml(
            spec.build_alt_inactive_missing(compact_sample_dict),
            tmp_path,
            "alt_inactive_missing.yml",
        )
        assert isinstance(cfg_alt_inactive, SUEWSConfig)

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
def test_water_distribution_direct_required_fields_and_whole_block_defaults(
    surface_name: str,
    surface_type: SurfaceType,
    missing_field: str,
    compact_sample_dict: dict[str, Any],
    tmp_path: Path,
) -> None:
    """Check direct field validation, defaults for an omitted block and a complete YAML block."""
    # ARRANGE: direct WaterDistribution model rejects any missing destination
    required = WATER_DISTRIBUTION_REQUIRED[surface_name]
    complete = dict.fromkeys(required, 0.0)
    complete[required[-1]] = 1.0
    incomplete = {k: v for k, v in complete.items() if k != missing_field}

    wd = WaterDistribution(**incomplete)
    with pytest.raises(ValueError, match=rf"Missing required field {missing_field}"):
        wd.validate_distribution(surface_type)

    # Omitting the whole block applies the defaults for the selected surface.
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
    try_body_sha256: str

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


def _canonical_ast_repr(node: ast.AST) -> str:
    """Return a Python-version-stable structural AST representation.

    Walks ``ast.iter_fields`` (which excludes line and column attributes) and
    omits ``None`` and empty lists so the result is identical across Python
    3.10-3.14 regardless of ``ast.dump``'s ``show_empty`` default.
    """
    fields: list[str] = []
    for field_name, value in ast.iter_fields(node):
        if value is None or value == []:
            continue
        if isinstance(value, ast.AST):
            rendered = _canonical_ast_repr(value)
        elif isinstance(value, list):
            items = ", ".join(
                _canonical_ast_repr(item) if isinstance(item, ast.AST) else repr(item)
                for item in value
            )
            rendered = f"[{items}]"
        else:
            rendered = repr(value)
        fields.append(f"{field_name}={rendered}")
    joined = ", ".join(fields)
    return f"{type(node).__name__}({joined})"


def _try_body_signature(try_node: ast.Try | Any) -> str:
    """Return a SHA-256 fingerprint of ``try_node.body`` independent of line numbers."""
    canonical = "\n".join(_canonical_ast_repr(stmt) for stmt in try_node.body)
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def _handler_swallows_warning(handler: ast.ExceptHandler) -> bool:
    """True when *handler* can execute a warning call without raising."""
    _, warn_exits = _analyse_block(handler.body)
    return warn_exits != _Exit.NONE


def _collect_validator_swallows(
    fn_node: ast.FunctionDef | ast.AsyncFunctionDef,
    rel_path: str,
    qualname: str,
) -> list[WarningSwallowFinding]:
    """Collect warning-swallowing ``except`` handlers inside one validator."""
    found: list[WarningSwallowFinding] = []
    for sub in ast.walk(fn_node):
        if not isinstance(sub, _TRY_NODE_TYPES):
            continue
        try_sig = _try_body_signature(sub)
        for handler in sub.handlers:
            if not _handler_swallows_warning(handler):
                continue
            exc_type = ast.unparse(handler.type) if handler.type is not None else "bare"
            found.append(
                WarningSwallowFinding(
                    rel_path=rel_path,
                    qualname=qualname,
                    lineno=handler.lineno,
                    except_type=exc_type,
                    try_body_sha256=try_sig,
                )
            )
    return found


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
        before a trailing ``raise``), tagged with the normalised SHA-256
        signature of the enclosing ``try`` body.
    """
    tree = ast.parse(source, filename=rel_path)
    findings: list[WarningSwallowFinding] = []

    def _visit_scope(stmts: list[ast.stmt], prefix: tuple[str, ...]) -> None:
        for stmt in stmts:
            if isinstance(stmt, ast.ClassDef):
                _visit_scope(stmt.body, (*prefix, stmt.name))
            elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
                dec_names = {_decorator_name(d) for d in stmt.decorator_list}
                if dec_names & VALIDATOR_DECORATORS:
                    qualname = ".".join((*prefix, stmt.name))
                    findings.extend(
                        _collect_validator_swallows(stmt, rel_path, qualname)
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


_SHA256_HEX_RE = re.compile(r"^[0-9a-f]{64}$")


def _parse_allowlist_handlers(
    allowlist: dict[str, dict[str, Any]],
) -> Counter[tuple[str, str, str]]:
    """Validate and expand allowlist entries into ``(site_id, exc_type, try_sha256)`` counts."""
    expected: Counter[tuple[str, str, str]] = Counter()
    for site_id, meta in allowlist.items():
        handlers = meta["handlers"]
        expected_count = meta["count"]
        if (
            isinstance(expected_count, bool)
            or not isinstance(expected_count, int)
            or expected_count < 1
        ):
            msg = (
                f"{site_id}: allowlist count must be a positive int, "
                f"got {expected_count!r}"
            )
            raise ValueError(msg)
        if not isinstance(handlers, list) or len(handlers) != expected_count:
            msg = (
                f"{site_id}: allowlist handlers length ({len(handlers)}) "
                f"must equal count ({expected_count})"
            )
            raise ValueError(msg)
        for entry in handlers:
            exc_type = entry["except_type"]
            try_sig = entry["try_body_sha256"]
            if not isinstance(exc_type, str) or not exc_type:
                msg = f"{site_id}: allowlist handler missing non-empty except_type"
                raise ValueError(msg)
            if not isinstance(try_sig, str) or not _SHA256_HEX_RE.match(try_sig):
                msg = (
                    f"{site_id}: allowlist handler try_body_sha256 must be 64 "
                    f"lowercase hex chars, got {try_sig!r}"
                )
                raise ValueError(msg)
            expected[site_id, exc_type, try_sig] += 1
    return expected


def _evaluate_validator_warning_swallows(
    findings: list[WarningSwallowFinding],
    allowlist: dict[str, dict[str, Any]],
) -> tuple[list[str], list[str]]:
    """Compare AST findings against exact ``(site_id, except_type, try_body_sha256)`` counts.

    Pinning the normalised ``try``-body AST signature prevents swapping a
    reviewed handler for a new warning swallow on a different protected
    operation inside the same method, while remaining invariant to comments,
    formatting, and line-number shifts.
    """
    expected = _parse_allowlist_handlers(allowlist)
    actual: Counter[tuple[str, str, str]] = Counter()
    lines_by_key: dict[tuple[str, str, str], list[int]] = {}
    for finding in findings:
        key = (finding.site_id, finding.except_type, finding.try_body_sha256)
        actual[key] += 1
        lines_by_key.setdefault(key, []).append(finding.lineno)

    unreviewed: list[str] = []
    stale: list[str] = []
    for key in sorted(set(actual) | set(expected)):
        site_id, exc_type, try_sig = key
        obs = actual[key]
        exp = expected[key]
        short_sig = try_sig[:12]
        if obs > exp:
            lines = ", ".join(str(ln) for ln in sorted(lines_by_key[key]))
            unreviewed.append(
                f"{site_id} (except {exc_type}, try={short_sig}: observed {obs} "
                f"handler(s) at line(s) {lines}, allowlisted {exp})"
            )
        elif obs < exp:
            stale.append(
                f"{site_id} (except {exc_type}, try={short_sig}: observed {obs} "
                f"handler(s), allowlisted {exp})"
            )
    return unreviewed, stale


def test_no_unreviewed_validator_warning_swallows() -> None:
    """Every ``except: warnings.warn`` in a Pydantic validator must match exact allowlist signatures."""
    # ARRANGE
    baseline = _load_baseline()
    allowlist = baseline["validator_warning_swallows"]
    sources = _iter_data_model_python_sources()
    assert sources, "No Python sources found under supy.data_model"

    # ACT
    findings: list[WarningSwallowFinding] = []
    for rel_path, source in sources:
        findings.extend(find_validator_warning_swallows(source, rel_path))

    unreviewed, stale = _evaluate_validator_warning_swallows(findings, allowlist)

    # ASSERT
    assert not unreviewed, (
        "Unreviewed Pydantic validator exception-to-warning swallow handler(s) "
        f"detected: {'; '.join(unreviewed)}"
    )
    assert not stale, (
        "Legacy validator warning-swallow handler(s) were fixed; remove or "
        f"shrink stale entry in schema_contract_baseline.json: {'; '.join(stale)}"
    )


def test_validator_warning_swallow_allowlist_enforces_exact_handler_count() -> None:
    """Adding a second swallow in an allowlisted method or fixing one must fail."""
    # ARRANGE
    site_path = "src/supy/data_model/core/config.py"
    site_id = f"{site_path}::SUEWSConfig.set_surface_types_validation"
    legacy_sig = _try_body_signature(
        ast.parse(
            "try:\n    surface_prop.set_surface_type(surface_type)\nexcept Exception:\n    pass"
        ).body[0]
    )
    allowlist = {
        site_id: {
            "owner_issue": "#2086",
            "owner_pr": "#2117",
            "handlers": [
                {
                    "except_type": "Exception",
                    "try_body_sha256": legacy_sig,
                    "protected_op": "surface_prop.set_surface_type(surface_type)",
                }
            ],
            "count": 1,
            "reason": "Synthetic 1-handler allowlist matching live legacy entry.",
        }
    }

    def _eval(body: str) -> tuple[int, list[str], list[str]]:
        src = (
            "import warnings\n"
            "from pydantic import BaseModel, model_validator\n"
            "class SUEWSConfig(BaseModel):\n"
            '    @model_validator(mode="after")\n'
            "    def set_surface_types_validation(self):\n"
            f"{body}"
            "        return self\n"
        )
        found = find_validator_warning_swallows(src, site_path)
        unreviewed, stale = _evaluate_validator_warning_swallows(found, allowlist)
        return len(found), unreviewed, stale

    one_exc = (
        "        try:\n"
        "            surface_prop.set_surface_type(surface_type)\n"
        "        except Exception as exc:\n"
        "            warnings.warn(str(exc), UserWarning)\n"
    )
    second_val = (
        "        try:\n"
        "            surface_prop.set_surface_type(surface_type)\n"
        "        except ValueError as exc:\n"
        "            warnings.warn(str(exc), UserWarning)\n"
    )

    # ACT / ASSERT: exact 1-handler match passes
    assert _eval(one_exc) == (1, [], [])

    # ACT / ASSERT: second handler of same exception type on same op fails
    n_double, unrev_double, stale_double = _eval(one_exc + one_exc)
    assert n_double == 2
    assert len(unrev_double) == 1
    assert "observed 2 handler(s)" in unrev_double[0]
    assert "allowlisted 1" in unrev_double[0]
    assert stale_double == []

    # ACT / ASSERT: additional handler with different exception type fails
    n_extra, unrev_extra, stale_extra = _eval(one_exc + second_val)
    assert n_extra == 2
    assert len(unrev_extra) == 1
    assert "except ValueError" in unrev_extra[0]
    assert stale_extra == []

    # ACT / ASSERT: removing the legacy swallow fails as stale (0 observed vs 1 allowed)
    n_clean, unrev_clean, stale_clean = _eval("")
    assert n_clean == 0
    assert unrev_clean == []
    assert len(stale_clean) == 1
    assert "observed 0 handler(s)" in stale_clean[0]
    assert "allowlisted 1" in stale_clean[0]


def test_validator_warning_swallow_try_body_signature_controls() -> None:
    """Try-body AST signatures tolerate source movement but catch swapped ops."""
    # ARRANGE
    site_path = "src/supy/data_model/core/config.py"
    site_id = f"{site_path}::SUEWSConfig.set_surface_types_validation"
    # This synthetic control must keep running after the real exception is retired.
    allowlist = {
        site_id: {
            "count": 1,
            "handlers": [
                {
                    "except_type": "Exception",
                    "protected_op": "surface_prop.set_surface_type(surface_type)",
                    "try_body_sha256": "a2d4e623ee7f24d975f4764bce3d1a6daf89d3704e0e04af642ed205188d22b2",
                }
            ],
        }
    }

    base_source = """
import warnings
from pydantic import BaseModel, model_validator

class SUEWSConfig(BaseModel):
    @model_validator(mode="after")
    def set_surface_types_validation(self):
        try:
            surface_prop.set_surface_type(surface_type)
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
        return self
"""
    # Positive control: shift lines, insert preceding helper, comments, blank
    # lines, and multi-line call formatting around the exact same try operation.
    moved_source = """
import warnings
from pydantic import BaseModel, model_validator

# Unrelated header comment shifting line numbers.


class SUEWSConfig(BaseModel):
    def unrelated_helper(self):
        # Extra lines before the allowlisted validator method.
        return 42

    @model_validator(mode="after")
    def set_surface_types_validation(self):
        # Harmless comment inside the validator.
        try:
            # Comment inside the protected try block.
            surface_prop.set_surface_type(
                surface_type,
            )
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
        return self
"""
    # Negative control 1: fix the allowlisted handler to re-raise while adding
    # a new except Exception warning swallow on a different operation in the
    # same method (count stays 1, same method and Exception type!).
    swapped_op_source = """
import warnings
from pydantic import BaseModel, model_validator

class SUEWSConfig(BaseModel):
    @model_validator(mode="after")
    def set_surface_types_validation(self):
        try:
            surface_prop.set_surface_type(surface_type)
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
            raise
        try:
            surface_prop.validate_water_distribution()
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
        return self
"""
    # Negative control 2: smuggle a second call inside the allowlisted try block.
    expanded_try_source = """
import warnings
from pydantic import BaseModel, model_validator

class SUEWSConfig(BaseModel):
    @model_validator(mode="after")
    def set_surface_types_validation(self):
        try:
            surface_prop.set_surface_type(surface_type)
            surface_prop.validate_water_distribution()
        except Exception as exc:
            warnings.warn(str(exc), UserWarning)
        return self
"""

    # ACT
    base_found = find_validator_warning_swallows(base_source, site_path)
    moved_found = find_validator_warning_swallows(moved_source, site_path)
    swapped_found = find_validator_warning_swallows(swapped_op_source, site_path)
    expanded_found = find_validator_warning_swallows(expanded_try_source, site_path)

    # ASSERT: positive control passes despite shifted line numbers & formatting
    assert len(base_found) == 1
    assert len(moved_found) == 1
    assert moved_found[0].lineno > base_found[0].lineno
    assert moved_found[0].try_body_sha256 == base_found[0].try_body_sha256
    assert _evaluate_validator_warning_swallows(moved_found, allowlist) == ([], [])

    # ASSERT: swapping to a new operation in the same method fails loudly
    assert len(swapped_found) == 1
    assert swapped_found[0].except_type == "Exception"
    assert swapped_found[0].try_body_sha256 != base_found[0].try_body_sha256
    swap_unrev, swap_stale = _evaluate_validator_warning_swallows(
        swapped_found, allowlist
    )
    assert len(swap_unrev) == 1
    assert swapped_found[0].try_body_sha256[:12] in swap_unrev[0]
    assert len(swap_stale) == 1
    assert base_found[0].try_body_sha256[:12] in swap_stale[0]

    # ASSERT: adding a second call into the allowlisted try block also fails
    assert len(expanded_found) == 1
    exp_unrev, exp_stale = _evaluate_validator_warning_swallows(
        expanded_found, allowlist
    )
    assert len(exp_unrev) == 1
    assert len(exp_stale) == 1


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

    for site_id, meta in baseline["validator_warning_swallows"].items():
        assert meta.get("owner_issue"), f"{site_id} allowlist entry missing owner_issue"
        assert meta.get("reason"), f"{site_id} allowlist entry missing reason"
        count = meta.get("count")
        handlers = meta.get("handlers")
        assert isinstance(count, int) and not isinstance(count, bool) and count >= 1, (
            f"{site_id} allowlist count must be a positive int, got {count!r}"
        )
        assert isinstance(handlers, list) and len(handlers) == count, (
            f"{site_id} allowlist handlers must be a list of length {count}"
        )
        for handler_meta in handlers:
            assert handler_meta.get("except_type"), (
                f"{site_id} allowlist handler missing except_type"
            )
            assert _SHA256_HEX_RE.match(handler_meta.get("try_body_sha256", "")), (
                f"{site_id} allowlist handler has invalid try_body_sha256"
            )
            assert handler_meta.get("protected_op"), (
                f"{site_id} allowlist handler missing protected_op description"
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
