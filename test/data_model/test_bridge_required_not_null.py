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
