"""Scenario: release-yaml-upgrade."""

from helpers import FIXTURES
import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.schema import CURRENT_SCHEMA_VERSION
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.e2e]


def test_release_yaml_upgrade_preserves_migration_intent(tmp_path):
    """Scenario: release-yaml-upgrade.

    Persona: maintainer guarding release fixture compatibility.
    Starting condition: a vendored release YAML predates current schema cleanup.
    User action: upgrade it with its release tag and parse it under the current schema.
    Expected warning/error: none.
    Expected result: current schema stamp and explicit field migration.
    """
    source = FIXTURES / "release_configs" / "2025.10.15.yml"
    upgraded = tmp_path / "upgraded.yml"

    upgrade_yaml(input_path=source, output_path=upgraded, from_ver="2025.10.15")
    SUEWSConfig.from_yaml(str(upgraded))

    payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
    assert payload["schema_version"] == CURRENT_SCHEMA_VERSION

    building_archetype = payload["sites"][0]["properties"]["building_archetype"]
    stebbs = payload["sites"][0]["properties"]["stebbs"]
    assert "Wallx1" not in building_archetype
    assert "wall_outer_heat_capacity_fraction" not in building_archetype
    assert "fraction_heat_capacity_wall_external" in building_archetype
    assert "DHWVesselEmissivity" not in stebbs
    assert "MinimumVolumeOfDHWinUse" not in stebbs
