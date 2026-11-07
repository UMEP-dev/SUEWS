"""Resolve omitted self-transfer before serialising kernel inputs (gh#2001)."""

import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig

pytestmark = pytest.mark.api

SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("form", ["omitted", "null", "wrapped-null"])
def test_self_transfer_resolves_to_zero(short_sample_yaml_path, surface, form):
    dict_config = yaml.safe_load(short_sample_yaml_path.read_text(encoding="utf-8"))
    dict_distribution = dict_config["sites"][0]["properties"]["land_cover"][surface][
        "waterdist"
    ]
    field = f"to_{surface}"
    if form == "omitted":
        dict_distribution.pop(field, None)
    else:
        dict_distribution[field] = None if form == "null" else {"value": None}

    config = SUEWSConfig.from_dict(dict_config)
    dict_resolved = config.model_dump(exclude_none=True)
    resolved = dict_resolved["sites"][0]["properties"]["land_cover"][surface][
        "waterdist"
    ]

    assert resolved[field]["value"] == pytest.approx(0.0, rel=0, abs=0)
    for destination, supplied in dict_distribution.items():
        if destination != field and supplied is not None:
            assert resolved[destination] == supplied


@pytest.mark.parametrize("surface", SURFACES)
def test_explicit_self_transfer_survives_yaml_reload(
    short_sample_yaml_path, tmp_path, surface
):
    dict_config = yaml.safe_load(short_sample_yaml_path.read_text(encoding="utf-8"))
    field = f"to_{surface}"
    dict_config["sites"][0]["properties"]["land_cover"][surface]["waterdist"][field] = {
        "value": 0.25
    }
    path_config = tmp_path / "explicit-self-transfer.yml"
    path_config.write_text(yaml.safe_dump(dict_config), encoding="utf-8")

    config = SUEWSConfig.from_yaml(str(path_config))
    dict_resolved = config.model_dump(exclude_none=True)
    resolved = dict_resolved["sites"][0]["properties"]["land_cover"][surface][
        "waterdist"
    ]

    assert resolved[field]["value"] == pytest.approx(0.25, rel=0, abs=0)
    reloaded = SUEWSConfig.from_dict(dict_resolved)
    assert reloaded.model_dump(exclude_none=True)["sites"] == dict_resolved["sites"]
