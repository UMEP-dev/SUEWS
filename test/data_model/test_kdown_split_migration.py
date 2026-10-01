"""Preserve older shortwave selections through the dev5 schema migration."""

from copy import deepcopy
from pathlib import Path

from click.testing import CliRunner
import pytest
import yaml

from supy.cmd.table_converter import convert_table_cmd
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION, SchemaMigrator
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.model import ModelPhysics
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]

LEGACY_SELECTOR = (
    Path(__file__).resolve().parents[1]
    / "fixtures/release_configs/schema_deltas/2026.6.dev4.yml"
)


@pytest.fixture
def legacy_config(sample_yaml_path):
    """Overlay the outgoing selector fixture on a complete configuration."""
    payload = yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))
    legacy = yaml.safe_load(LEGACY_SELECTOR.read_text(encoding="utf-8"))
    payload["schema_version"] = legacy["schema_version"]
    payload["model"]["physics"].update(legacy["model"]["physics"])
    return payload


@pytest.mark.parametrize(
    "source_version", ["2026.6.dev1", "2026.6.dev2", "2026.6.dev3", "2026.6.dev4"]
)
@pytest.mark.parametrize("wrapped", [False, True])
def test_legacy_shortwave_selector_migrates_to_perez(
    legacy_config, source_version, wrapped, tmp_path, capsys
):
    """Both migration APIs rename the selector and preserve all other content."""
    legacy_config["schema_version"] = source_version
    if not wrapped:
        legacy_config["model"]["physics"]["kdown_split_method"] = " EPW "
    original = deepcopy(legacy_config)
    expected = deepcopy(legacy_config)
    expected["schema_version"] = CURRENT_SCHEMA_VERSION
    if wrapped:
        expected["model"]["physics"]["kdown_split_method"]["value"] = "perez"
    else:
        expected["model"]["physics"]["kdown_split_method"] = "perez"
    source = tmp_path / "legacy.yml"
    output = tmp_path / "current.yml"
    source.write_text(yaml.safe_dump(legacy_config), encoding="utf-8")

    migrated = SchemaMigrator().migrate(legacy_config)
    upgrade_yaml(source, output)

    assert migrated == expected
    assert legacy_config == original
    assert yaml.safe_load(output.read_text(encoding="utf-8")) == expected
    assert "kdown_split_method" in capsys.readouterr().err
    for config in (SUEWSConfig.from_dict(migrated), SUEWSConfig.from_yaml(str(output))):
        assert config.model.physics.kdown_split_method.value.value == 3
    assert SchemaMigrator().migrate(migrated) == migrated


@pytest.mark.parametrize(
    "selector", [3, {"value": 3}, "perez", "reindl", "forcing",
                 {"constant": {"sw_dn_direct_frac": 0.42}}]
)
def test_shortwave_migration_preserves_other_selections(selector):
    """A version bump must not change numeric or already-current selections."""
    config = {"schema_version": "2026.6.dev4",
              "model": {"physics": {"kdown_split_method": selector}}}
    expected = deepcopy(config)
    expected["schema_version"] = CURRENT_SCHEMA_VERSION

    assert SchemaMigrator().migrate(config) == expected


def test_shortwave_migration_preserves_an_omitted_selector():
    """Defaults remain implicit when an older configuration has no selector."""
    config = {"schema_version": "2026.6.dev4", "model": {"physics": {}}}
    migrated = SchemaMigrator().migrate(config)

    assert migrated["model"]["physics"] == {}
    assert migrated["schema_version"] == CURRENT_SCHEMA_VERSION


def test_shortwave_selector_upgrade_cli(legacy_config, tmp_path):
    """The documented converter produces a loadable dev5 configuration."""
    source = tmp_path / "legacy.yml"
    output = tmp_path / "current.yml"
    source.write_text(yaml.safe_dump(legacy_config), encoding="utf-8")

    result = CliRunner().invoke(
        convert_table_cmd, ["-i", str(source), "-o", str(output)]
    )

    assert result.exit_code == 0, result.output
    config = SUEWSConfig.from_yaml(str(output))
    assert config.schema_version == "2026.6.dev5"
    assert config.model.physics.kdown_split_method.value.value == 3


@pytest.mark.parametrize("selector", ["epw", {"value": "epw"}])
def test_current_schema_has_no_legacy_shortwave_alias(selector):
    """Only the migration accepts the retired selector."""
    with pytest.raises(ValueError, match="unknown scheme name"):
        ModelPhysics(kdown_split_method=selector)
