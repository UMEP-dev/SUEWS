"""CF metadata contracts across the complete output registry."""

import importlib.resources
import json

from pydantic import ValidationError
import pytest

from supy.data_model.output import OUTPUT_REGISTRY, get_output_contract_catalogue
from supy.data_model.output.variables import OutputVariable
from supy.units import parse_unit

pytestmark = pytest.mark.api


@pytest.mark.parametrize(
    "variable",
    OUTPUT_REGISTRY.variables,
    ids=lambda variable: f"{variable.group}.{variable.name}",
)
def test_registry_cf_metadata(variable):
    """Every output has a recognised time method and appropriate temperature kind."""
    assert variable.cell_methods in {
        "",
        "time: point",
        "time: mean",
        "time: sum",
        "time: minimum",
        "time: maximum",
    }
    assert (not variable.cell_methods) == (variable.aggregation == "T")
    unit = parse_unit(variable.unit)
    if unit.dimension[3]:
        assert variable.units_metadata in {
            "temperature: on_scale",
            "temperature: difference",
        }
    else:
        assert variable.units_metadata is None


def test_only_water_flux_fields_change_from_previous_contract():
    """Keep stores, names and layouts unchanged across the rate migration."""
    previous = json.loads(
        importlib.resources.files("supy.data_model.output")
        .joinpath("artefacts/1.1.9/catalogue.json").read_text(encoding="utf-8")
    )
    current = get_output_contract_catalogue().model_dump(mode="json")
    for before, after in zip(previous["variables"], current["variables"]):
        if before["unit"] == "mm":
            assert after["unit"] == "kg m-2 s-1"
            assert after["cell_methods"] == "time: mean"
            expected = dict(before, unit="kg m-2 s-1", cell_methods="time: mean", interval_water_flux=True)
            if expected["aggregation"] == "A":
                expected["aggregation"] = "S"
            assert after == expected
        else:
            assert after == dict(before, interval_water_flux=False)
    assert len(current["variables"]) == len(previous["variables"])


@pytest.mark.parametrize("name", ["Tg", "Tw"])
def test_beers_surface_temperature_excess_is_a_difference(name):
    """BEERS reports surface excess above air temperature, not surface temperature."""
    variable = next(v for v in OUTPUT_REGISTRY.variables
                    if v.group == "BEERS" and v.name == name)
    assert variable.units_metadata == "temperature: difference"


@pytest.mark.parametrize(
    ("aggregation", "expected"),
    [("A", "time: mean"), ("S", "time: sum"), ("L", "time: point"), ("T", "")],
)
def test_metadata_defaults_and_explicit_overrides(aggregation, expected):
    """The public model derives defaults and preserves supplied producer metadata."""
    data = {
        "name": "example",
        "unit": "K",
        "description": "Example",
        "aggregation": aggregation,
        "group": "SUEWS",
        "level": 0,
    }
    variable = OutputVariable(**data)
    assert variable.cell_methods == expected
    assert variable.units_metadata == "temperature: on_scale"
    override = OutputVariable(
        **data, cell_methods="time: sum", units_metadata="temperature: difference"
    )
    assert override.cell_methods == "time: sum"
    assert override.units_metadata == "temperature: difference"


@pytest.mark.parametrize(
    "invalid",
    [
        {"cell_methods": "time: last"},
        {"units_metadata": "temperature: unknown"},
    ],
)
def test_unknown_cf_metadata_is_rejected(invalid):
    """Reject unsupported metadata instead of publishing unrecognised strings."""
    with pytest.raises(ValidationError):
        OutputVariable(
            name="example",
            unit="K",
            description="Example",
            aggregation="A",
            group="SUEWS",
            level=0,
            **invalid,
        )


@pytest.mark.parametrize(
    ("name", "method", "temperature"),
    [
        ("HDD1_h", "time: mean", "temperature: difference"),
        ("HDD2_c", "time: mean", "temperature: difference"),
        ("HDD3_Tmean", "time: mean", "temperature: on_scale"),
        ("P_day", "time: mean", None),
        ("DLHrs", "time: sum", None),
        ("Tmin", "time: minimum", "temperature: on_scale"),
        ("Tmax", "time: maximum", "temperature: on_scale"),
    ],
)
def test_daily_statistics_describe_the_producer(name, method, temperature):
    """Daily statistics retain their kernel meaning despite last-row selection."""
    variable = next(
        v
        for v in OUTPUT_REGISTRY.variables
        if v.group == "DailyState" and v.name == name
    )
    assert variable.aggregation == "L"
    assert variable.cell_methods == method
    assert variable.units_metadata == temperature


@pytest.mark.parametrize("declared,expected", [(False, "time: sum"), (True, "time: mean")])
def test_written_rate_metadata_requires_explicit_declaration(declared, expected):
    """The unit spelling alone does not change an amount's time operation."""
    variable = OutputVariable(
        name="example", unit="kg m-2 s-1", description="Example",
        aggregation="S", group="SUEWS", level=0, interval_water_flux=declared,
    )
    assert variable.cell_methods == expected
