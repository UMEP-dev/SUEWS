"""The 2026.6.dev8 migration drops a null ``initial_states.snowalb`` (gh#2001).

Earlier ``suews validate --science-fixes apply`` runs wrote ``snowalb: null``
for a snow-free start. From 2026.6.dev8 ``snowalb`` rejects null, so the
``(2026.6.dev7 -> 2026.6.dev8)`` handler removes it and the default 0.5
applies; a number the user gave is kept. Older schemas chain through it.
"""

import copy
from pathlib import Path
import shutil

from conftest import TIMESTEPS_PER_DAY
import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.core import SUEWSConfig
from supy.suews_sim import SUEWSSimulation
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]

SHORT_SAMPLE = (
    Path(__file__).parents[1] / "fixtures" / "sample_short" / "sample_config_short.yml"
)


@pytest.fixture(scope="module")
def sample_dict():
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _snowalb(config):
    value = config.sites[0].initial_states.snowalb
    return getattr(value, "value", value)


def _upgrade(data, schema, tmp_path):
    data["schema_version"] = schema
    source = tmp_path / "old.yml"
    output = tmp_path / "new.yml"
    source.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    upgrade_yaml(input_path=source, output_path=output)
    return output


@pytest.mark.parametrize(
    ("given", "expected"),
    [(None, 0.5), ({"value": None}, 0.5), ({"value": 0.3}, 0.3)],
)
def test_dev7_upgrade_drops_null_snowalb(sample_dict, tmp_path, given, expected):
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["initial_states"]["snowalb"] = given

    output = _upgrade(data, "2026.6.dev7", tmp_path)

    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION
    states = migrated["sites"][0]["initial_states"]
    if expected == 0.5:
        assert "snowalb" not in states
    else:
        assert states["snowalb"] == given
    config = SUEWSConfig.from_yaml(str(output))
    assert _snowalb(config) == expected


def test_null_snowalb_without_upgrade_is_rejected(sample_dict):
    data = copy.deepcopy(sample_dict)
    data["schema_version"] = "2026.6.dev7"
    data["sites"][0]["initial_states"]["snowalb"] = {"value": None}
    with pytest.raises(ValueError, match="snowalb needs a number"):
        SUEWSConfig.from_dict(data)


def test_dev6_upgrade_chains_through_dev8(sample_dict, tmp_path):
    """A dev6 file gets both the dev7 and the dev8 deltas."""
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["initial_states"]["snowalb"] = {"value": None}
    co2 = data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
    co2.pop("emission_co2_point_source", None)

    output = _upgrade(data, "2026.6.dev6", tmp_path)

    migrated = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert "snowalb" not in migrated["sites"][0]["initial_states"]
    co2 = migrated["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
    assert co2["emission_co2_point_source"] == {"value": 0.0}
    config = SUEWSConfig.from_yaml(str(output))
    assert _snowalb(config) == 0.5


def _run_one_day(path):
    sim = SUEWSSimulation(str(path))
    df_forcing = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    sim.update_forcing(df_forcing)
    return sim.run(end_date=df_forcing.index[-1]).df


def _short_sample_with_snowalb(tmp_path, name, snowalb, schema):
    data = yaml.safe_load(SHORT_SAMPLE.read_text(encoding="utf-8"))
    forcing_name = data["model"]["control"]["forcing"]["file"]["value"]
    if not (tmp_path / forcing_name).exists():
        shutil.copy(SHORT_SAMPLE.parent / forcing_name, tmp_path / forcing_name)
    data["schema_version"] = schema
    for site in data["sites"]:
        site["initial_states"]["snowalb"] = snowalb
    path = tmp_path / name
    path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return path


def test_upgraded_null_snowalb_runs_unchanged(tmp_path):
    """With snow off, the upgraded file (0.5) runs as the null (0.0) did.

    The Rust bridge replaced a null ``snowalb`` with 0.0; with the snow
    module off the model resets the snow albedo every timestep, so the
    starting value never reaches the output.
    """
    data = yaml.safe_load(SHORT_SAMPLE.read_text(encoding="utf-8"))
    assert data["model"]["physics"]["snow"] == "disabled"

    old = _short_sample_with_snowalb(
        tmp_path, "dev7.yml", {"value": None}, "2026.6.dev7"
    )
    upgraded = tmp_path / "dev8.yml"
    upgrade_yaml(input_path=old, output_path=upgraded)
    as_bridge_used = _short_sample_with_snowalb(
        tmp_path, "zero.yml", {"value": 0.0}, CURRENT_SCHEMA_VERSION
    )

    df_upgraded = _run_one_day(upgraded)
    df_zero = _run_one_day(as_bridge_used)

    assert len(df_upgraded) == TIMESTEPS_PER_DAY
    assert df_upgraded.equals(df_zero)
