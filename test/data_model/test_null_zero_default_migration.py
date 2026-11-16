"""Outgoing null initial counters and water flow load with their effective zero."""

from pathlib import Path

import pytest

from supy.data_model.core import SUEWSConfig
from supy.util.converter.yaml_upgrade import upgrade_yaml

pytestmark = [pytest.mark.api, pytest.mark.cfg]


@pytest.mark.parametrize("fixture", [
    "dev6-vegetation-free-null.yml", "dev7-water-flowchange-null.yml",
])
def test_outgoing_null_zero_defaults_load_after_upgrade(tmp_path, fixture):
    source = Path(__file__).parents[1] / "fixtures" / "release_configs" / "schema_deltas" / fixture
    output = tmp_path / "upgraded.yml"

    upgrade_yaml(input_path=source, output_path=output)

    config = SUEWSConfig.from_yaml(str(output))
    site = config.sites[0]
    if "vegetation" in fixture:
        for surface in ("evetr", "dectr", "grass"):
            assert getattr(site.initial_states, surface).sdd_id.value == 0.0
            assert getattr(site.properties.land_cover, surface).sfr.value == 0.0
    else:
        assert site.properties.land_cover.water.flowchange.value == 0.0
