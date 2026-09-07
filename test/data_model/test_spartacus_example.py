"""Contract tests for the packaged SPARTACUS-Surface example (gh#1699).

The default ``sample_config.yml`` is a NARP configuration whose top vertical
layer (15-22 m) carries vegetation although the tallest tree is 13.1 m. That is
harmless under NARP but rejected by the SPARTACUS vegetation-layer check.
``sample_config_spartacus.yml`` is the reviewed SPARTACUS variant of the same
KCL site. These tests pin down what the example guarantees:

- it loads through the public validated path with SPARTACUS selected and
  without any test-side data repair;
- its vertical-layer geometry is internally consistent with the tree heights;
- it differs from the default NARP sample only in the declared, documented
  fields, so the two files cannot drift apart silently.

They do not validate the example against observations: the layer fractions
and scales are the benchmark 1a estimates carried over from the default
sample, not measured values.
"""

from importlib.resources import files

import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import NetRadiationMethod

pytestmark = pytest.mark.api

_SAMPLE_DIR = files("supy").joinpath("sample_data")


def _load(name):
    return yaml.safe_load(_SAMPLE_DIR.joinpath(name).read_text(encoding="utf-8"))


def _unwrap(node):
    return node["value"] if isinstance(node, dict) and "value" in node else node


def _vertical_layers(data):
    return data["sites"][0]["properties"]["vertical_layers"]


def _max_tree_height(data):
    land_cover = data["sites"][0]["properties"]["land_cover"]
    return max(
        _unwrap(land_cover["dectr"]["height_deciduous_tree"]),
        _unwrap(land_cover["evetr"]["height_evergreen_tree"]),
    )


def _flatten(node, prefix=()):
    """Yield (path, leaf) pairs for a nested YAML mapping/list structure."""
    if isinstance(node, dict):
        for key, value in node.items():
            yield from _flatten(value, (*prefix, str(key)))
    elif isinstance(node, list):
        for idx, value in enumerate(node):
            yield from _flatten(value, (*prefix, str(idx)))
    else:
        yield prefix, node


def test_spartacus_example_loads_via_public_path_without_repair():
    data = _load("sample_config_spartacus.yml")

    config = SUEWSConfig.from_dict(data)

    physics = config.model.physics
    assert physics.net_radiation.value == NetRadiationMethod.LDOWN_SS_AIR


def test_spartacus_example_top_layer_is_above_tallest_tree():
    data = _load("sample_config_spartacus.yml")
    layers = _vertical_layers(data)
    heights = _unwrap(layers["height"])
    veg_frac = _unwrap(layers["veg_frac"])
    veg_scale = _unwrap(layers["veg_scale"])
    max_tree = _max_tree_height(data)

    # The first interface at or above the tallest tree bounds the tree layer
    # from above; every layer starting at or above that interface is tree-free.
    tree_top_interface = next(
        i for i in range(1, len(heights)) if max_tree <= heights[i]
    )
    assert heights[tree_top_interface - 1] < max_tree <= heights[tree_top_interface]
    for layer in range(tree_top_interface, len(heights) - 1):
        assert veg_frac[layer] == 0.0
        assert veg_scale[layer] == 0.0
    # Layers that do reach the canopy keep their vegetation.
    assert all(v > 0.0 for v in veg_frac[:tree_top_interface])
    assert all(s > 0.0 for s in veg_scale[:tree_top_interface])


def test_default_sample_still_needs_repair_under_spartacus():
    """Documents why the dedicated example exists: the NARP sample is not a
    SPARTACUS starting point, and this must stay a deliberate choice."""
    data = _load("sample_config.yml")
    data["model"]["physics"]["net_radiation"] = {
        "value": NetRadiationMethod.LDOWN_SS_AIR.value
    }

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    message = str(excinfo.value)
    assert "veg_frac[2] should be zero" in message
    assert "veg_scale[2] should be zero" in message


def test_spartacus_example_differs_from_default_only_in_declared_fields():
    """The example is the default sample plus a documented delta, nothing else."""
    default = dict(_flatten(_load("sample_config.yml")))
    spartacus = dict(_flatten(_load("sample_config_spartacus.yml")))

    changed = {
        path
        for path in default.keys() | spartacus.keys()
        if default.get(path) != spartacus.get(path)
    }
    layers = ("sites", "0", "properties", "vertical_layers")
    expected = {
        ("name",),
        ("description",),
        ("model", "physics", "net_radiation", "narp", "ldown"),
        ("model", "physics", "net_radiation", "spartacus", "ldown"),
        (*layers, "veg_frac", "value", "2"),
        (*layers, "veg_scale", "value", "2"),
    }
    assert changed == expected

    assert (
        spartacus["model", "physics", "net_radiation", "spartacus", "ldown"] == "air"
    )
    assert spartacus[*layers, "veg_frac", "value", "2"] == 0.0
    assert spartacus[*layers, "veg_scale", "value", "2"] == 0.0
    assert spartacus["schema_version",] == default["schema_version",]
