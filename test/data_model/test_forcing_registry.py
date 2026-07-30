"""Contract tests for the canonical forcing-variable registry."""

# ruff: file-ignore[import-private-name]

from __future__ import annotations

from pathlib import Path
import re

import pytest

from supy._check import FORCING_REQUIREMENTS, dict_rules_indiv
from supy._load import (
    BASELINE_DATETIME_FORCING_COLUMNS,
    BASELINE_FORCING_COLUMNS,
    CANONICAL_FORCING_COLUMNS,
    FORCING_OPTIONAL_FILL,
    OPTIONAL_FORCING_COLUMNS,
    PER_LANDCOVER_ALLOWED_SUFFIXES,
    dict_var_type_forcing,
)
from supy.data_model.core.forcing_validation import (
    _PHYSICS_REQUIRED_FORCING,
)
from supy.data_model.forcing import FORCING_REGISTRY

pytestmark = pytest.mark.api


def test_registry_preserves_legacy_column_order_and_temporal_types() -> None:
    assert FORCING_REGISTRY.canonical_names == (
        "iy",
        "id",
        "it",
        "imin",
        "qn",
        "qh",
        "qe",
        "qs",
        "qf",
        "U",
        "RH",
        "Tair",
        "pres",
        "rain",
        "kdown",
        "snow",
        "ldown",
        "fcld",
        "Wuh",
        "xsmd",
        "lai",
        "kdiff",
        "kdir",
        "wdir",
    )
    assert FORCING_REGISTRY.temporal_types == dict_var_type_forcing
    assert FORCING_REGISTRY.by_name("isec").input_column is False


def test_registry_projects_existing_loader_contract() -> None:
    assert FORCING_REGISTRY.datetime_names == BASELINE_DATETIME_FORCING_COLUMNS
    assert FORCING_REGISTRY.baseline_names == BASELINE_FORCING_COLUMNS
    assert list(FORCING_REGISTRY.optional_names) == OPTIONAL_FORCING_COLUMNS
    assert frozenset(FORCING_REGISTRY.canonical_names) == CANONICAL_FORCING_COLUMNS
    assert FORCING_REGISTRY.per_landcover_suffixes == (PER_LANDCOVER_ALLOWED_SUFFIXES)
    assert {
        variable.missing_value for variable in FORCING_REGISTRY.input_variables
    } <= {
        None,
        FORCING_OPTIONAL_FILL,
    }


def test_registry_projects_legacy_and_current_physics_requirements() -> None:
    legacy = {
        key: sorted(columns)
        for key, columns in FORCING_REGISTRY.physics_requirements(legacy=True).items()
    }
    assert legacy == FORCING_REQUIREMENTS
    assert FORCING_REGISTRY.physics_requirements() == _PHYSICS_REQUIRED_FORCING


def test_registry_projects_legacy_range_checker_rules() -> None:
    registry_rules = FORCING_REGISTRY.checker_rules

    assert {name for name in registry_rules if name in dict_rules_indiv} == set(
        registry_rules
    )
    assert {name: dict_rules_indiv[name] for name in registry_rules} == registry_rules
    assert registry_rules["pres"]["param"] == {"min": 680.0, "max": 1300.0}
    assert FORCING_REGISTRY.by_name("pres").valid_range.model_dump() == {
        "minimum": 68.0,
        "maximum": 130.0,
    }


def test_alias_lookup_is_case_insensitive() -> None:
    assert FORCING_REGISTRY.by_name("TAIR").name == "Tair"
    assert FORCING_REGISTRY.by_name("air_temperature").name == "Tair"
    assert FORCING_REGISTRY.by_name("unknown") is None


def test_rust_descriptors_match_registry_with_linked_wuh_exception() -> None:
    rust_path = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "suews_bridge"
        / "src"
        / "forcing.rs"
    )
    rust_source = rust_path.read_text(encoding="utf-8")
    descriptors: dict[str, tuple[str, bool]] = {}
    pattern = re.compile(
        r"\(\w+,\s*\d+,\s*\[(.*?)\],\s*"
        r"InterpKind::(\w+),\s*(true|false)\)",
    )
    for csv_names, interpolation, required in pattern.findall(rust_source):
        for name in re.findall(r'"([^"]+)"', csv_names):
            descriptors[name.casefold()] = (interpolation, required == "true")

    interpolation_names = {
        "avg": "Average",
        "inst": "Instantaneous",
        "sum": "Sum",
    }
    for variable in FORCING_REGISTRY.input_variables:
        name = variable.name.casefold()
        if variable.temporal_semantics.value == "time" or name in {"lai", "wdir"}:
            continue
        assert name in descriptors, f"{name} is absent from Rust forcing descriptors"
        rust_interpolation, rust_required = descriptors[name]
        expected_interpolation = interpolation_names[variable.temporal_semantics.value]
        if name == "wuh":
            # Tracked by gh#1447. The infrastructure migration must expose,
            # rather than silently resolve, this numerical-semantics disagreement.
            assert (expected_interpolation, rust_interpolation) == (
                "Sum",
                "Instantaneous",
            )
        else:
            assert rust_interpolation == expected_interpolation
        assert rust_required is (variable.requirement.value == "baseline")

    for suffix in FORCING_REGISTRY.per_landcover_suffixes["lai"]:
        assert descriptors[f"lai_{suffix}"] == ("Instantaneous", False)
