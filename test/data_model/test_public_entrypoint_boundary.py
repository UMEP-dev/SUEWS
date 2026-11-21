"""Public Phase A publication and five-route engine-boundary parity (gh#2099).

This complements the full DataFrame/default/receiver checks in
``test_entrypoint_parity.py``; it does not replace or narrow their selection.
"""

import copy
from importlib.resources import files
import json
from pathlib import Path

from click.testing import CliRunner
import pytest
import yaml

# Exercise the real export boundary in its owning module.
from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.cmd.suews_cli import cli as suews_cli
from supy.data_model.core import SUEWSConfig
from supy.suews_sim import SUEWSSimulation

pytestmark = [pytest.mark.api, pytest.mark.cfg]

# Only loader provenance differs between input routes; retain every engine value.
_LOADER_KEYS = {"_yaml_path", "_auto_generate_annotated", "_yaml_raw"}
_SAFE_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_SAFE_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


@pytest.fixture(scope="module")
def sample_yaml_bytes():
    """Share immutable input bytes, never decoded product configurations."""
    return (files("supy") / "sample_data" / "sample_config.yml").read_bytes()


def _canonical_engine(config):
    resolved = yaml.load(config_to_engine_yaml(config), Loader=_SAFE_LOADER)
    return {key: value for key, value in resolved.items() if key not in _LOADER_KEYS}


@pytest.mark.medium
@pytest.mark.parametrize(
    ("grid_id", "height", "emissivity"),
    [(None, None, None), (17, 18.5, 0.88), (29, 23.5, 0.91)],
    ids=["unchanged", "custom_17", "custom_29"],
)
def test_public_phase_a_preserves_five_route_engine_values(
    sample_yaml_bytes, tmp_path, monkeypatch, grid_id, height, emissivity
):
    """Reload the actual CLI-published file and compare complete resolved values."""
    payload = yaml.load(sample_yaml_bytes, Loader=_SAFE_LOADER)
    if grid_id is not None:
        payload["sites"][0]["gridiv"] = grid_id
        cover = payload["sites"][0]["properties"]["land_cover"]
        cover["bldgs"]["bldgh"] = {"value": height}
        cover["paved"]["emis"] = {"value": emissivity}
    expected_ids = [site["gridiv"] for site in payload["sites"]]

    runner = CliRunner()
    monkeypatch.chdir(tmp_path)
    source = Path("input.yml").resolve()
    source.write_text(
        yaml.dump(payload, Dumper=_SAFE_DUMPER, sort_keys=False), encoding="utf-8"
    )
    result = runner.invoke(
        suews_cli,
        ["validate", "--pipeline", "A", "--forcing", "off", "--format", "json", str(source)],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}, envelope
    assert envelope["data"]["phases_run"] == ["A"]
    published = Path(envelope["data"]["updated_yaml"]).resolve()
    assert published != source
    assert published.is_relative_to(source.parent)
    assert published.is_file(), envelope

    # Only update_config inherently materialises a DataFrame. Do not request
    # another frame here: the original full-parity suite covers that contract.
    configurations = {
        "from_yaml": SUEWSConfig.from_yaml(str(source)),
        "from_dict": SUEWSConfig.from_dict(copy.deepcopy(payload)),
        "programmatic": SUEWSConfig(**copy.deepcopy(payload)),
        "update_config": SUEWSSimulation().update_config(
            copy.deepcopy(payload), auto_load_forcing=False
        ).config,
        "public_phase_a": SUEWSConfig.from_yaml(str(published)),
    }
    reference = _canonical_engine(configurations["from_yaml"])
    for route, config in configurations.items():
        resolved = _canonical_engine(config) if route != "from_yaml" else reference
        assert resolved == reference, route
        ids = [site.gridiv for site in config.sites]
        assert ids == expected_ids, route
        assert [site["gridiv"] for site in resolved["sites"]] == expected_ids, route
        if grid_id is not None:
            cover = resolved["sites"][0]["properties"]["land_cover"]
            assert cover["bldgs"]["bldgh"]["value"] == height, route
            assert cover["paved"]["emis"]["value"] == emissivity, route
