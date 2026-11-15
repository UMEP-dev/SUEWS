"""All live unit metadata must use the shared CF vocabulary.

Input fields are inspected through Pydantic, including nested annotations and
inherited fields. Forcing selectors and output factories are expanded at runtime.
"""

import importlib
import inspect
import pkgutil
from typing import get_args

from pydantic import BaseModel
import pytest

from supy.data_model import configuration, core
from supy.data_model.forcing import FORCING_REGISTRY
from supy.data_model.output import get_output_contract_catalogue
from supy.units import UnitError, parse_unit

pytestmark = pytest.mark.api

# Capped, field-specific debt: these need structural metadata in a later change.
CONDITIONAL_UNIT_EXCEPTIONS = {
    "StorageDrainParams.drain_coef_1": "Its dimensions depend on the drainage equation and fitted exponent.",
    "StorageDrainParams.drain_coef_2": "It is an exponent or an inverse storage scale depending on the equation.",
    "Conductance.g_q_base": "The conductance selector changes its dimensional interpretation.",
    "Conductance.g_q_shape": "The conductance selector changes its dimensional interpretation.",
    "CO2Params.traffic_rate": "Traffic activity is per area or per person depending on its selector.",
}
EXCEPTION_LIMIT = 5


def _annotation_models(annotation):
    """Walk models nested in unions, containers and reference wrappers."""
    if inspect.isclass(annotation) and issubclass(annotation, BaseModel):
        yield annotation
    for argument in get_args(annotation):
        yield from _annotation_models(argument)


def _input_units():
    """Inspect all input model classes, then recursively visit their fields."""
    pending = [core.SUEWSConfig]
    for package in (core, configuration):
        modules = [package]
        modules.extend(
            importlib.import_module(info.name)
            for info in pkgutil.walk_packages(package.__path__, package.__name__ + ".")
        )
        for module in modules:
            pending.extend(
                model for _, model in inspect.getmembers(module, inspect.isclass)
                if issubclass(model, BaseModel)
                and model.__module__.startswith("supy.data_model.")
            )
    visited = set()
    units = {}
    while pending:
        model = pending.pop()
        if model in visited:
            continue
        visited.add(model)
        for name, field in model.model_fields.items():
            pending.extend(_annotation_models(field.annotation))
            extra = field.json_schema_extra
            if isinstance(extra, dict) and "unit" in extra:
                # Report a shared inherited field once under its declaring class.
                owner = next(
                    cls for cls in model.__mro__
                    if name in inspect.get_annotations(cls)
                )
                path = f"{owner.__name__}.{name}"
                if path in units:
                    assert units[path] == extra["unit"], path
                units[path] = extra["unit"]
    return units


def _forcing_units():
    """Include both file and runtime units, and every selector branch."""
    units = {}
    for variable in FORCING_REGISTRY.variables:
        if variable.unit is not None:
            units[f"{variable.name}.unit"] = variable.unit
        if variable.runtime_unit is not None:
            units[f"{variable.name}.runtime_unit"] = variable.runtime_unit
        for selector, unit in variable.units_by_value.items():
            units[f"{variable.name}.units_by_value[{selector}]"] = unit
    return units


def _output_units():
    """Use the expanded catalogue, including every surface and profile entry."""
    catalogue = get_output_contract_catalogue()
    return {f"{variable.group}.{variable.name}": variable.unit for variable in catalogue.variables}


@pytest.fixture(scope="module")
def unit_metadata():
    """Collect the three live metadata surfaces without source parsing."""
    return {"input": _input_units(), "forcing": _forcing_units(), "output": _output_units()}


@pytest.mark.parametrize("surface", ["input", "forcing", "output"])
def test_unit_metadata_parses(unit_metadata, surface):
    """Every unit parses, apart from exactly five named input-field exceptions."""
    assert len(CONDITIONAL_UNIT_EXCEPTIONS) == EXCEPTION_LIMIT
    units = unit_metadata[surface]
    assert units, f"No {surface} units were traversed"
    exceptions = CONDITIONAL_UNIT_EXCEPTIONS if surface == "input" else {}
    assert exceptions.keys() <= units.keys(), "An exception no longer names an input field"
    errors = []
    for path, unit in units.items():
        if path in exceptions:
            # Prevent resolved exceptions from silently becoming permanent skips.
            with pytest.raises(UnitError, match="not in the SUEWS unit vocabulary"):
                parse_unit(unit)
            continue
        try:
            parse_unit(unit)
        except UnitError as error:
            errors.append(f"{surface}.{path}: {error}")
    assert not errors, "\n".join(errors)


def test_unit_metadata_cf_units_when_available(unit_metadata):
    """Cross-check accepted strings with UDUNITS when its optional binding exists."""
    cf_units = pytest.importorskip(
        "cf_units", reason="Optional UDUNITS cross-check requires cf_units; no runtime dependency"
    )
    for surface, units in unit_metadata.items():
        for path, text in units.items():
            if surface == "input" and path in CONDITIONAL_UNIT_EXCEPTIONS:
                continue
            unit = parse_unit(text)
            if not unit.is_no_unit:
                parsed = cf_units.Unit(text)
                assert not parsed.is_unknown(), f"{surface}.{path}: {text}"
                assert not parsed.is_no_unit(), f"{surface}.{path}: {text}"
