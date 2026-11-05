"""The CO2 point source is required when the emissions method uses it.

gh#2001 (part of gh#1991): a missing ``emission_co2_point_source`` used to
reach the kernel as the Rust bridge's own 0. The biogenic CO2 methods (11-16,
21-26, 31-36, 41-46) add the point source to the CO2 flux, so a YAML using one
of them must now give it; methods 0-6 do not use it (the driver discards their
CO2 flux), so it stays optional there. The 2026.6.dev7 migration writes the 0.0
the model used into older configurations.
"""

import copy

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.core import SUEWSConfig
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]

FIELD = "emission_co2_point_source"
BIOGENIC_J11 = 12  # BIOGEN_RECT_J11
NON_BIOGENIC_J11 = 2  # J11: CO2 flux discarded by the driver


@pytest.fixture(scope="module")
def sample_dict():
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _co2(data):
    return data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]


def _config(sample_dict, method, point_source):
    """Return a copy of the sample with an emissions method and point source.

    ``point_source`` is a value to write, ``None`` for ``{value: null}``, or
    ``...`` to leave the field out.
    """
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["emissions"] = {"value": method}
    co2 = _co2(data)
    co2.pop(FIELD, None)
    if point_source is not ...:
        co2[FIELD] = {"value": point_source}
    return data


@pytest.mark.parametrize("point_source", [..., None])
def test_biogenic_method_without_point_source_is_rejected(sample_dict, point_source):
    data = _config(sample_dict, BIOGENIC_J11, point_source)
    with pytest.raises(ValueError, match=f"{FIELD} is None but emissions method"):
        SUEWSConfig.from_dict(data)


def test_nested_biogenic_selector_is_checked(sample_dict):
    """The user-facing nested selector reaches the same check (12 = j11 + rectangular)."""
    data = _config(sample_dict, BIOGENIC_J11, ...)
    data["model"]["physics"]["emissions"] = {
        "heat": "j11",
        "co2": {"anthropogenic": "qf_linked", "biogenic": "rectangular"},
    }
    with pytest.raises(ValueError, match=f"{FIELD} is None but emissions method 12"):
        SUEWSConfig.from_dict(data)


def test_biogenic_method_with_point_source_loads(sample_dict):
    config = SUEWSConfig.from_dict(_config(sample_dict, BIOGENIC_J11, 0.0))
    co2 = config.sites[0].properties.anthropogenic_emissions.co2
    assert co2.emission_co2_point_source.value == 0.0


def test_non_biogenic_method_does_not_need_point_source(sample_dict):
    config = SUEWSConfig.from_dict(_config(sample_dict, NON_BIOGENIC_J11, ...))
    co2 = config.sites[0].properties.anthropogenic_emissions.co2
    assert co2.emission_co2_point_source is None


def test_programmatic_construction_is_not_checked(sample_dict):
    """Only user-shaped input (from_yaml / from_dict) is checked, as for gh#1333."""
    config = SUEWSConfig(**_config(sample_dict, BIOGENIC_J11, ...))
    co2 = config.sites[0].properties.anthropogenic_emissions.co2
    assert co2.emission_co2_point_source is None


@pytest.mark.parametrize(
    ("given", "expected"),
    [(..., 0.0), (None, 0.0), (4.0, 4.0)],
)
def test_dev6_upgrade_writes_the_point_source_the_model_used(
    sample_dict, tmp_path, given, expected
):
    data = _config(sample_dict, BIOGENIC_J11, given)
    data["schema_version"] = "2026.6.dev6"
    source = tmp_path / "dev6.yml"
    output = tmp_path / "dev7.yml"
    source.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    upgrade_yaml(input_path=source, output_path=output)

    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    assert _co2(migrated)[FIELD] == {"value": expected}
    config = SUEWSConfig.from_yaml(str(output))
    co2 = config.sites[0].properties.anthropogenic_emissions.co2
    assert co2.emission_co2_point_source.value == expected
