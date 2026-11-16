"""Numeric fields the Rust bridge needs reject ``{value: null}``.

gh#2001 (part of gh#1991): a ``null`` on a required numeric field is dropped
from the supy dump, so the bridge used to replace it with its own fallback.
These fields now refuse ``null`` at the data model; omitting the field still
gives its documented default.
"""

import copy

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.core import SUEWSConfig

pytestmark = [pytest.mark.api, pytest.mark.cfg]

# (path inside sites[0], field, default when omitted)
CASES = [
    (("properties", "land_cover", "water"), "flowchange", 0.0),
    (("initial_states", "evetr"), "sdd_id", 0),
    (("initial_states", "dectr"), "sdd_id", 0),
    (("initial_states", "grass"), "sdd_id", 0),
    (("initial_states",), "snowalb", 0.5),
]


@pytest.fixture(scope="module")
def sample_dict():
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _block(data, path):
    block = data["sites"][0]
    for key in path:
        block = block[key]
    return block


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


@pytest.mark.parametrize(("path", "field", "default"), CASES)
def test_null_rejected(sample_dict, path, field, default):
    data = copy.deepcopy(sample_dict)
    _block(data, path)[field] = {"value": None}
    with pytest.raises(ValueError, match=f"{field} needs a number"):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(("path", "field", "default"), CASES)
def test_omitted_emits_default(sample_dict, path, field, default):
    data = copy.deepcopy(sample_dict)
    del _block(data, path)[field]
    dump = SUEWSConfig.from_dict(data).model_dump(exclude_none=True, mode="json")
    assert _unwrap(_block(dump, path)[field]) == default


def test_snow_free_season_adjustment_output_loads(sample_dict):
    """Phase B drops ``snowalb`` for a snow-free start, so its output loads.

    It used to write ``snowalb: null``; with null rejected that would make
    ``suews validate`` fail on the YAML it had just written (gh#2001).
    """
    from supy.data_model.validation.pipeline.phase_b import (
        adjust_seasonal_parameters,
    )

    data = copy.deepcopy(sample_dict)
    data["sites"][0]["initial_states"]["snowalb"] = {"value": None}
    # Northern-hemisphere site starting in July: a summer start.
    assert _unwrap(data["sites"][0]["properties"]["lat"]) > 23.5
    updated, adjustments = adjust_seasonal_parameters(
        data, start_date="2012-07-01", model_year=2012
    )
    assert "snowalb" not in updated["sites"][0]["initial_states"]
    assert any(a.parameter == "snowalb" for a in adjustments)
    config = SUEWSConfig.from_dict(updated)
    assert _unwrap(config.sites[0].initial_states.snowalb) == 0.5
