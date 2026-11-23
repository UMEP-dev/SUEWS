"""All live unit metadata must use the shared CF vocabulary or structural conditional specs.

Input fields are inspected through Pydantic, including nested annotations and
inherited fields. Forcing selectors and output factories are expanded at runtime.
Selector-dependent input fields must declare validated ``conditional_unit``
metadata rather than placeholder prose strings or exemptions.
"""

import copy
import importlib
import inspect
import pkgutil
from typing import Any, get_args

from pydantic import BaseModel
import pytest

from supy.data_model import configuration, core
from supy.data_model.core.human_activity import CO2Params
from supy.data_model.core.hydro import StorageDrainParams
from supy.data_model.core.site import Conductance
from supy.data_model.core.unit_metadata import (
    CONDITIONAL_UNIT_KEY,
    ConditionalUnitCase,
    ConditionalUnitSpec,
    iter_cf_unit_strings,
    resolve_conditional_unit_case,
    validate_conditional_unit_binding,
)
from supy.data_model.forcing import FORCING_REGISTRY
from supy.data_model.output import get_output_contract_catalogue
from supy.units import UnitError, parse_unit

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SCHEMA")]


def _annotation_models(annotation):
    """Walk models nested in unions, containers and reference wrappers."""
    if inspect.isclass(annotation) and issubclass(annotation, BaseModel):
        yield annotation
    for argument in get_args(annotation):
        yield from _annotation_models(argument)


def _input_unit_surfaces():
    """Inspect all input model classes and collect unit + conditional_unit metadata."""
    pending = [core.SUEWSConfig]
    for package in (core, configuration):
        modules = [package]
        modules.extend(
            importlib.import_module(info.name)
            for info in pkgutil.walk_packages(package.__path__, package.__name__ + ".")
        )
        for module in modules:
            pending.extend(
                model
                for _, model in inspect.getmembers(module, inspect.isclass)
                if issubclass(model, BaseModel)
                and model.__module__.startswith("supy.data_model.")
            )
    visited = set()
    units: dict[str, Any] = {}
    conditional_units: dict[str, tuple[type[BaseModel], str, dict[str, Any]]] = {}
    duplicates: list[str] = []
    while pending:
        model = pending.pop()
        if model in visited:
            continue
        visited.add(model)
        for name, field in model.model_fields.items():
            pending.extend(_annotation_models(field.annotation))
            extra = field.json_schema_extra
            if not isinstance(extra, dict):
                continue
            has_unit = "unit" in extra
            has_cond = CONDITIONAL_UNIT_KEY in extra
            if not (has_unit or has_cond):
                continue
            # Report a shared inherited field once under its declaring class.
            owner = next(
                cls for cls in model.__mro__ if name in inspect.get_annotations(cls)
            )
            path = f"{owner.__name__}.{name}"
            if has_unit and has_cond:
                duplicates.append(path)
                continue
            if has_unit:
                if path in units:
                    assert units[path] == extra["unit"], path
                units[path] = extra["unit"]
            if has_cond:
                if path in conditional_units:
                    assert conditional_units[path][2] == extra, path
                conditional_units[path] = (owner, name, extra)
    return units, conditional_units, sorted(set(duplicates))


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
    return {
        f"{variable.group}.{variable.name}": variable.unit
        for variable in catalogue.variables
    }


@pytest.fixture(scope="module")
def unit_surfaces():
    """Collect the three live metadata surfaces without source parsing."""
    input_units, input_conditional, duplicates = _input_unit_surfaces()
    return {
        "units": {
            "input": input_units,
            "forcing": _forcing_units(),
            "output": _output_units(),
        },
        "input_conditional": input_conditional,
        "duplicates": duplicates,
    }


@pytest.mark.parametrize("surface", ["input", "forcing", "output"])
def test_unit_metadata_parses(unit_surfaces, surface):
    """Every unconditional unit parses with zero field exceptions."""
    units = unit_surfaces["units"][surface]
    assert units, f"No {surface} units were traversed"
    errors = []
    for path, unit in units.items():
        try:
            parse_unit(unit)
        except UnitError as error:
            errors.append(f"{surface}.{path}: {error}")
    assert not errors, "\n".join(errors)


def test_conditional_unit_metadata_is_valid_and_complete(unit_surfaces):
    """Every conditional_unit binding resolves cleanly with valid CF branch units."""
    assert not unit_surfaces["duplicates"], (
        "Fields must not declare both 'unit' and 'conditional_unit': "
        f"{unit_surfaces['duplicates']}"
    )
    conditional = unit_surfaces["input_conditional"]
    assert conditional, "No conditional_unit input fields were traversed"

    expected_fields = {
        "StorageDrainParams.drain_coef_1",
        "StorageDrainParams.drain_coef_2",
        "Conductance.g_q_base",
        "Conductance.g_q_shape",
        "CO2Params.traffic_rate",
    }
    assert expected_fields <= set(conditional.keys()), (
        f"Missing expected conditional-unit fields: "
        f"{sorted(expected_fields - set(conditional.keys()))}"
    )

    specs: dict[str, ConditionalUnitSpec] = {}
    for path, (owner, field_name, extra) in conditional.items():
        spec = validate_conditional_unit_binding(
            owner, field_name, extra, root_model=core.SUEWSConfig
        )
        specs[path] = spec
        for subpath, unit_text in iter_cf_unit_strings(spec):
            parsed = parse_unit(unit_text)
            assert not parsed.is_no_unit, f"{path}.{subpath}: unexpected NO_UNIT"

    # Every non-reference field on the three models carrying conditional units
    # must carry either 'unit' or 'conditional_unit', so deleting conditional_unit
    # from any of the five fields cannot pass silently.
    for model_cls in (StorageDrainParams, Conductance, CO2Params):
        for field_name, field_info in model_cls.model_fields.items():
            if field_name == "ref":
                continue
            extra = field_info.json_schema_extra
            assert isinstance(extra, dict), f"{model_cls.__name__}.{field_name}"
            has_unit = "unit" in extra
            has_cond = CONDITIONAL_UNIT_KEY in extra
            assert has_unit ^ has_cond, (
                f"{model_cls.__name__}.{field_name} must declare exactly one of "
                "'unit' or 'conditional_unit'"
            )

    # Pin key dimensional distinctions for the five fields.
    dc1 = specs["StorageDrainParams.drain_coef_1"]
    assert dc1.selector == "drain_eq"
    assert dc1.cases["1"].kind == "formula"
    assert dc1.cases["1"].unit is None
    assert dc1.cases["1"].depends_on == ("drain_coef_2",)
    assert dc1.cases["2"].kind == "cf"
    assert dc1.cases["2"].unit == "kg m-2 h-1"
    assert dc1.cases["3"].kind == "formula"
    assert dc1.cases["3"].unit is None

    dc2 = specs["StorageDrainParams.drain_coef_2"]
    assert dc2.selector == "drain_eq"
    assert dc2.cases["1"].unit == "1"
    assert dc2.cases["2"].unit == "m2 kg-1"
    assert dc2.cases["3"].unit == "1"

    gqb = specs["Conductance.g_q_base"]
    assert gqb.selector == "model.physics.surface_conductance"
    assert gqb.cases["1"].unit == "kg g-1"
    assert gqb.cases["2"].unit == "1"

    gqs = specs["Conductance.g_q_shape"]
    assert gqs.selector == "model.physics.surface_conductance"
    assert gqs.cases["1"].unit == "g kg-1"
    assert gqs.cases["2"].kind == "empirical_base"
    assert gqs.cases["2"].unit is None
    assert gqs.cases["2"].basis_unit == "g kg-1"
    assert gqs.cases["2"].log_unit == "kg g-1"

    tr = specs["CO2Params.traffic_rate"]
    assert tr.selector == "type_traffic_rate"
    assert tr.cases["1"].unit == "km m-2 d-1"
    assert tr.cases["2"].unit == "km d-1"

    # Branch resolution unwraps scalars, integral floats, strings, and enums.
    assert resolve_conditional_unit_case(tr.to_schema_dict(), 1.0) == tr.cases["1"]
    assert resolve_conditional_unit_case(tr.to_schema_dict(), "2") == tr.cases["2"]
    assert resolve_conditional_unit_case(dc1.to_schema_dict(), 0) is None


def test_unit_metadata_cf_units_when_available(unit_surfaces):
    """Cross-check all CF strings (static and conditional) with UDUNITS when available."""
    cf_units = pytest.importorskip(
        "cf_units",
        reason="Optional UDUNITS cross-check requires cf_units; no runtime dependency",
    )
    for surface, units in unit_surfaces["units"].items():
        for path, text in units.items():
            unit = parse_unit(text)
            if not unit.is_no_unit:
                parsed = cf_units.Unit(text)
                assert not parsed.is_unknown(), f"{surface}.{path}: {text}"
                assert not parsed.is_no_unit(), f"{surface}.{path}: {text}"

    for path, (owner, field_name, extra) in unit_surfaces["input_conditional"].items():
        spec = validate_conditional_unit_binding(
            owner, field_name, extra, root_model=core.SUEWSConfig
        )
        for subpath, text in iter_cf_unit_strings(spec):
            parsed = cf_units.Unit(text)
            assert not parsed.is_unknown(), f"input.{path}.{subpath}: {text}"
            assert not parsed.is_no_unit(), f"input.{path}.{subpath}: {text}"


class TestConditionalUnitNegativeControls:
    """Negative controls: removing or swapping selector metadata must fail."""

    @staticmethod
    def _cloned_extra(model_cls: type[BaseModel], field_name: str) -> dict[str, Any]:
        extra = model_cls.model_fields[field_name].json_schema_extra
        assert isinstance(extra, dict)
        return copy.deepcopy(extra)

    def test_removing_conditional_unit_is_rejected(self):
        extra = self._cloned_extra(StorageDrainParams, "drain_coef_1")
        extra.pop(CONDITIONAL_UNIT_KEY)
        with pytest.raises(ValueError, match="missing 'conditional_unit'"):
            validate_conditional_unit_binding(
                StorageDrainParams, "drain_coef_1", extra, root_model=core.SUEWSConfig
            )

    @pytest.mark.parametrize("unit_value", ["1", None, "equation-dependent"])
    def test_declaring_unit_alongside_conditional_unit_is_rejected(self, unit_value):
        extra = self._cloned_extra(StorageDrainParams, "drain_coef_2")
        extra["unit"] = unit_value
        with pytest.raises(ValueError, match="must not declare both 'unit'"):
            validate_conditional_unit_binding(
                StorageDrainParams, "drain_coef_2", extra, root_model=core.SUEWSConfig
            )

    @pytest.mark.parametrize(
        "removed_key",
        [
            "selector",
            "selector_scope",
            "selector_model",
            "selector_field",
            "cases",
            "inactive_note",
        ],
    )
    def test_removing_required_selector_key_is_rejected(self, removed_key):
        extra = self._cloned_extra(Conductance, "g_q_base")
        extra[CONDITIONAL_UNIT_KEY].pop(removed_key)
        with pytest.raises(ValueError, match="missing required keys"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", extra, root_model=core.SUEWSConfig
            )

    def test_swapping_sibling_selector_to_physical_field_is_rejected(self):
        extra = self._cloned_extra(StorageDrainParams, "drain_coef_1")
        extra[CONDITIONAL_UNIT_KEY]["selector"] = "store_cap"
        extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "store_cap"
        with pytest.raises(ValueError, match="must be a unitless selector"):
            validate_conditional_unit_binding(
                StorageDrainParams, "drain_coef_1", extra, root_model=core.SUEWSConfig
            )

    def test_swapping_sibling_selector_to_self_or_conditional_peer_is_rejected(self):
        self_extra = self._cloned_extra(StorageDrainParams, "drain_coef_2")
        self_extra[CONDITIONAL_UNIT_KEY]["selector"] = "drain_coef_2"
        self_extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "drain_coef_2"
        with pytest.raises(ValueError, match="cannot select its own unit"):
            validate_conditional_unit_binding(
                StorageDrainParams,
                "drain_coef_2",
                self_extra,
                root_model=core.SUEWSConfig,
            )

        peer_extra = self._cloned_extra(StorageDrainParams, "drain_coef_2")
        peer_extra[CONDITIONAL_UNIT_KEY]["selector"] = "drain_coef_1"
        peer_extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "drain_coef_1"
        with pytest.raises(ValueError, match="must be a unitless selector"):
            validate_conditional_unit_binding(
                StorageDrainParams,
                "drain_coef_2",
                peer_extra,
                root_model=core.SUEWSConfig,
            )

    def test_swapping_sibling_selector_to_legacy_alias_is_rejected(self):
        extra = self._cloned_extra(CO2Params, "traffic_rate")
        extra[CONDITIONAL_UNIT_KEY]["selector"] = "trafficunits"
        extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "trafficunits"
        with pytest.raises(ValueError, match="not found on CO2Params"):
            validate_conditional_unit_binding(
                CO2Params, "traffic_rate", extra, root_model=core.SUEWSConfig
            )

    def test_swapping_config_selector_to_legacy_alias_is_rejected(self):
        extra = self._cloned_extra(Conductance, "g_q_base")
        extra[CONDITIONAL_UNIT_KEY]["selector"] = "model.physics.gsmodel"
        extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "gsmodel"
        with pytest.raises(ValueError, match="has no field 'gsmodel'"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", extra, root_model=core.SUEWSConfig
            )

    @pytest.mark.parametrize(
        "wrong_field",
        ["snow_use", "kdown_split_method", "emissions"],
    )
    def test_swapping_config_selector_to_wrong_enum_is_rejected(self, wrong_field):
        extra = self._cloned_extra(Conductance, "g_q_base")
        extra[CONDITIONAL_UNIT_KEY]["selector"] = f"model.physics.{wrong_field}"
        extra[CONDITIONAL_UNIT_KEY]["selector_field"] = wrong_field
        with pytest.raises(ValueError, match="do not match"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", extra, root_model=core.SUEWSConfig
            )

    def test_swapping_config_selector_to_physical_path_is_rejected(self):
        extra = self._cloned_extra(Conductance, "g_q_base")
        extra[CONDITIONAL_UNIT_KEY]["selector"] = "sites.properties.conductance.g_k"
        extra[CONDITIONAL_UNIT_KEY]["selector_model"] = "Conductance"
        extra[CONDITIONAL_UNIT_KEY]["selector_field"] = "g_k"
        with pytest.raises(ValueError, match="must be a unitless selector"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", extra, root_model=core.SUEWSConfig
            )

    def test_missing_or_extra_enum_case_is_rejected(self):
        missing_extra = self._cloned_extra(Conductance, "g_q_base")
        missing_extra[CONDITIONAL_UNIT_KEY]["cases"].pop("2")
        with pytest.raises(ValueError, match="do not match GSModel values"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", missing_extra, root_model=core.SUEWSConfig
            )

        extra_case = self._cloned_extra(Conductance, "g_q_base")
        extra_case[CONDITIONAL_UNIT_KEY]["cases"]["3"] = extra_case[
            CONDITIONAL_UNIT_KEY
        ]["cases"]["1"]
        with pytest.raises(ValueError, match="do not match GSModel values"):
            validate_conditional_unit_binding(
                Conductance, "g_q_base", extra_case, root_model=core.SUEWSConfig
            )

    def test_formula_case_rejects_fixed_unit_or_dimensional_dependency(self):
        with pytest.raises(ValueError, match="must not invent a fixed CF unit"):
            ConditionalUnitCase(
                label="Bad formula",
                kind="formula",
                unit="kg m-2 h-1",
                expression="(kg m-2)^(1 - drain_coef_2) h-1",
                base_units=("kg m-2", "h"),
                depends_on=("drain_coef_2",),
                formula="drain = drain_coef_1 * state**drain_coef_2 / nsh_real",
            )

        bad_dep_extra = self._cloned_extra(StorageDrainParams, "drain_coef_1")
        bad_dep_extra[CONDITIONAL_UNIT_KEY]["cases"]["1"]["depends_on"] = ["store_cap"]
        with pytest.raises(ValueError, match="must be dimensionless"):
            validate_conditional_unit_binding(
                StorageDrainParams,
                "drain_coef_1",
                bad_dep_extra,
                root_model=core.SUEWSConfig,
            )

    @staticmethod
    def test_empirical_base_rejects_fixed_unit_or_non_reciprocal_log_unit():
        with pytest.raises(ValueError, match="must not invent a fixed CF unit"):
            ConditionalUnitCase(
                label="Bad empirical base",
                kind="empirical_base",
                unit="1",
                expression="empirical power base",
                basis_unit="g kg-1",
                log_unit="kg g-1",
                formula="g_dq = g_q_base + (1 - g_q_base) * (g_q_shape**dq)",
            )

        with pytest.raises(ValueError, match="exact reciprocal"):
            ConditionalUnitCase(
                label="Mismatched scale",
                kind="empirical_base",
                expression="empirical power base",
                basis_unit="g kg-1",
                log_unit="kg kg-1",
                formula="g_dq = g_q_base + (1 - g_q_base) * (g_q_shape**dq)",
            )

    @staticmethod
    def test_cf_case_rejects_prose_placeholder_unit():
        with pytest.raises(UnitError, match="not in the SUEWS unit vocabulary"):
            ConditionalUnitCase(
                label="Bad CF",
                kind="cf",
                unit="equation-dependent",
                formula="drain = ...",
            )
