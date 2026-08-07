"""Focused invariants for the unpublished forcing-variable registry."""

# This parity test intentionally inspects the current private forcing constants.

from pathlib import Path
import re
import subprocess
import sys

from pydantic import ValidationError
import pytest

# ruff: disable[import-private-name]
from supy._check import FORCING_REQUIREMENTS
from supy._load import (
    BASELINE_DATETIME_FORCING_COLUMNS,
    BASELINE_FORCING_COLUMNS,
    CANONICAL_FORCING_COLUMNS,
    LAI_LANDCOVER_SUFFIXES,
    WUH_LANDCOVER_SUFFIXES,
    dict_var_type_forcing,
)
from supy.data_model.core.forcing_validation import _PHYSICS_REQUIRED_FORCING
from supy.data_model.forcing import FORCING_REGISTRY, ForcingRegistry, ForcingVariable
from supy.suews_forcing import FORCING_ALIASES

# ruff: enable[import-private-name]

pytestmark = pytest.mark.api


def test_version_import_does_not_construct_unpublished_registry() -> None:
    """Keep lightweight version imports independent of registry construction."""
    data_model_dir = Path(__file__).resolve().parents[2] / "src" / "supy" / "data_model"
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            (
                "import sys; import forcing.version; "
                "assert 'forcing.registry' not in sys.modules; "
                "assert 'forcing.variables' not in sys.modules"
            ),
        ],
        check=False,
        cwd=data_model_dir,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr


def test_registry_matches_current_python_forcing_inventory() -> None:
    """Keep the unpublished inventory aligned with current Python behaviour."""
    variables = FORCING_REGISTRY.variables
    legacy = tuple(
        variable for variable in variables if variable.legacy_position is not None
    )
    surfaces = {
        variable.name for variable in variables if variable.legacy_position is None
    }

    assert len(variables) == 34
    assert tuple(variable.name for variable in legacy) == tuple(
        name for name in dict_var_type_forcing if name != "isec"
    )
    assert {variable.name for variable in legacy} == CANONICAL_FORCING_COLUMNS
    assert tuple(variable.legacy_position for variable in legacy) == tuple(range(1, 25))
    assert surfaces == {
        *(f"lai_{suffix}" for suffix in LAI_LANDCOVER_SUFFIXES),
        *(f"wuh_{suffix}" for suffix in WUH_LANDCOVER_SUFFIXES),
    }
    assert "isec" not in {variable.name for variable in variables}


def test_registry_matches_current_python_requiredness_and_temporal_types() -> None:
    """Record current baseline and resampling semantics without changing them."""
    variables = FORCING_REGISTRY.variables
    dict_variables = {variable.name: variable for variable in variables}
    baseline = {
        variable.name
        for variable in FORCING_REGISTRY.variables
        if variable.requiredness == "baseline"
    }

    assert baseline == {
        *BASELINE_DATETIME_FORCING_COLUMNS,
        *BASELINE_FORCING_COLUMNS,
    }
    assert {
        variable.name: variable.temporal
        for variable in FORCING_REGISTRY.variables
        if variable.legacy_position is not None
    } == {
        name: temporal
        for name, temporal in dict_var_type_forcing.items()
        if name != "isec"
    }
    assert all(
        dict_variables[f"lai_{suffix}"].temporal == "inst"
        for suffix in LAI_LANDCOVER_SUFFIXES
    )
    assert all(
        dict_variables[f"wuh_{suffix}"].temporal == "sum"
        for suffix in WUH_LANDCOVER_SUFFIXES
    )

    assert {variable.name: variable.role for variable in variables} == {
        **dict.fromkeys(BASELINE_DATETIME_FORCING_COLUMNS, "coordinate"),
        "qn": "observation",
        "qh": "reserved",
        "qe": "reserved",
        "qs": "observation",
        "qf": "observation",
        **dict.fromkeys(
            (
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
                "kdiff",
                "kdir",
                *(f"wuh_{suffix}" for suffix in WUH_LANDCOVER_SUFFIXES),
            ),
            "driver",
        ),
        "xsmd": "observation",
        "lai": "observation",
        **{f"lai_{suffix}": "observation" for suffix in LAI_LANDCOVER_SUFFIXES},
        "wdir": "reserved",
    }


def test_registry_projects_python_forcing_metadata() -> None:
    """Keep Python consumers on the forcing-owned registry projection."""
    legacy = FORCING_REGISTRY.legacy_variables

    assert FORCING_REGISTRY.canonical_file_columns == tuple(
        variable.name for variable in legacy
    )
    assert FORCING_REGISTRY.baseline_datetime_columns == (
        "iy",
        "id",
        "it",
        "imin",
    )
    assert FORCING_REGISTRY.baseline_driver_columns == (
        "U",
        "RH",
        "Tair",
        "pres",
        "rain",
        "kdown",
    )
    assert FORCING_REGISTRY.baseline_file_columns == (
        *FORCING_REGISTRY.baseline_datetime_columns,
        *FORCING_REGISTRY.baseline_driver_columns,
    )
    assert FORCING_REGISTRY.optional_canonical_columns == tuple(
        variable.name for variable in legacy if variable.requiredness != "baseline"
    )
    assert FORCING_REGISTRY.temporal_types == {
        variable.name: variable.temporal for variable in legacy
    }
    assert FORCING_REGISTRY.runtime_validation_ranges["pres"] == (
        680.0,
        1300.0,
        "hPa",
    )
    assert FORCING_REGISTRY.runtime_validation_ranges["Tair"] == (
        -60.0,
        90.0,
        "degC",
    )
    assert FORCING_REGISTRY.accessor_aliases == FORCING_ALIASES


def test_registry_projects_current_and_legacy_requirements() -> None:
    """Keep selector namespaces separate while sharing registry rules."""
    assert FORCING_REGISTRY.current_requirements == _PHYSICS_REQUIRED_FORCING
    assert FORCING_REGISTRY.legacy_requirements == FORCING_REQUIREMENTS


def test_file_and_accessor_aliases_are_separate_namespaces() -> None:
    """Do not silently treat human-readable accessor aliases as file headers."""
    assert FORCING_REGISTRY.by_file_name("%TAIR").name == "Tair"
    assert FORCING_REGISTRY.by_file_name("qn1_obs").name == "qn"
    assert FORCING_REGISTRY.by_accessor_name("qn1_obs") is None
    assert FORCING_REGISTRY.by_accessor_name("qstar").name == "qn"
    assert FORCING_REGISTRY.by_file_name("qstar") is None

    dict_accessor_aliases = {
        variable.name: list(variable.accessor_aliases)
        for variable in FORCING_REGISTRY.variables
        if variable.accessor_aliases
    }
    assert dict_accessor_aliases == FORCING_ALIASES


def test_rust_file_aliases_exclude_internal_positions() -> None:
    """Keep external Rust spellings separate from its private flat-buffer ABI."""
    expected = {
        "qn": ("qn1_obs",),
        "qs": ("qs_obs",),
        "qf": ("qf_obs",),
        "Tair": ("temp_c",),
        "snow": ("snowfrac",),
        "Wuh": ("wu_mm",),
        **{f"wuh_{suffix}": (f"wu_mm_{suffix}",) for suffix in WUH_LANDCOVER_SUFFIXES},
    }

    registry_aliases = {
        variable.name: variable.file_aliases
        for variable in FORCING_REGISTRY.variables
        if variable.file_aliases
    }
    assert registry_aliases == expected

    rust_source = (
        Path(__file__).resolve().parents[2] / "src/suews_bridge/src/forcing.rs"
    ).read_text(encoding="utf-8")
    csv_name_groups = [
        tuple(re.findall(r'"([^"]+)"', names))
        for names in re.findall(
            r"\(\w+,\s*\d+,\s*\[([^\]]+)\],\s*InterpKind::\w+",
            rust_source,
        )
    ]
    canonical_names = {
        variable.name.casefold(): variable.name
        for variable in FORCING_REGISTRY.variables
    }
    rust_aliases: dict[str, tuple[str, ...]] = {}
    for group in csv_name_groups:
        canonical_name = next(
            (canonical_names[name] for name in group if name in canonical_names),
            None,
        )
        if canonical_name is None:
            continue
        aliases = tuple(name for name in group if name != canonical_name.casefold())
        if aliases:
            rust_aliases[canonical_name] = aliases

    assert rust_aliases == registry_aliases

    bulk_wuh = next(
        variable for variable in FORCING_REGISTRY.variables if variable.name == "Wuh"
    )
    assert re.search(
        r'\(wu_mm,\s*18,\s*\["wu_mm",\s*"wuh"\],\s*'
        r"InterpKind::Instantaneous",
        rust_source,
    )
    assert "Rust reader currently treats it as instantaneous" in (
        bulk_wuh.metadata_note or ""
    )
    assert all(
        variable.legacy_position is None
        for variable in FORCING_REGISTRY.variables
        if variable.name.startswith(("lai_", "wuh_"))
    )


def test_requirement_rules_project_to_current_bulk_requirements() -> None:
    """Keep the first alternative aligned with current preflight validation."""
    projected: dict[tuple[str, int], frozenset[str]] = {}
    for rule in FORCING_REGISTRY.requirement_rules:
        for value in rule.values:
            projected[rule.selector, value] = frozenset(
                name.casefold() for name in rule.alternatives[0]
            )

    assert projected == _PHYSICS_REQUIRED_FORCING

    legacy_projected: dict[tuple[str, int], list[str]] = {}
    for rule in FORCING_REGISTRY.requirement_rules:
        if rule.legacy_selector is None:
            continue
        for value in rule.legacy_values:
            legacy_projected[rule.legacy_selector, value] = list(rule.alternatives[0])
    assert legacy_projected == FORCING_REQUIREMENTS

    lai_rule = next(
        rule
        for rule in FORCING_REGISTRY.requirement_rules
        if rule.selector == "laimethod"
    )
    water_use_rule = next(
        rule
        for rule in FORCING_REGISTRY.requirement_rules
        if rule.selector == "water_use"
    )
    assert lai_rule.alternatives == (
        ("lai",),
        tuple(f"lai_{suffix}" for suffix in LAI_LANDCOVER_SUFFIXES),
    )
    assert water_use_rule.alternatives == (
        ("Wuh",),
        tuple(f"wuh_{suffix}" for suffix in WUH_LANDCOVER_SUFFIXES),
    )


def test_registry_rejects_ambiguous_aliases_and_unknown_requirements() -> None:
    """Reject ambiguity at construction rather than in later consumers."""
    first = ForcingVariable(
        name="first",
        data_type="number",
        role="driver",
        unit="1",
        description="First test variable",
        temporal="inst",
        requiredness="optional",
        missing_policy="sentinel",
        file_aliases=("shared",),
        legacy_position=1,
    )
    second = ForcingVariable(
        name="second",
        data_type="number",
        role="driver",
        unit="1",
        description="Second test variable",
        temporal="inst",
        requiredness="optional",
        missing_policy="sentinel",
        file_aliases=("SHARED",),
        legacy_position=2,
    )

    with pytest.raises(ValidationError, match="file_aliases name"):
        ForcingRegistry(variables=(first, second), requirement_rules=())

    bad_rule = FORCING_REGISTRY.requirement_rules[0].model_copy(
        update={"alternatives": (("unknown",),)}
    )
    with pytest.raises(ValidationError, match="unknown columns"):
        ForcingRegistry(variables=(first,), requirement_rules=(bad_rule,))


def test_unresolved_scientific_metadata_stays_explicit_and_unpublished() -> None:
    """Do not turn conflicting sources into a published scientific claim."""
    dict_variables = {
        variable.name: variable for variable in FORCING_REGISTRY.variables
    }

    assert dict_variables["Wuh"].unit is None
    assert dict_variables["Wuh"].validation_range is None
    assert dict_variables["xsmd"].unit is None
    assert dict_variables["snow"].metadata_note is not None
    assert all(
        dict_variables[f"wuh_{suffix}"].unit == "mm"
        for suffix in WUH_LANDCOVER_SUFFIXES
    )
