"""Validator and loader guidance for observed soil moisture (current and legacy YAML)."""

from copy import deepcopy
from importlib.resources import as_file
from pathlib import Path
import warnings

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation.pipeline.phase_a import (
    annotate_missing_parameters,
    is_declared_schema_field,
)

pytestmark = [pytest.mark.api, pytest.mark.cfg]
SAMPLE = trv_supy_module / "sample_data" / "sample_config.yml"
FIXTURES = Path(__file__).parents[1] / "fixtures" / "soil_observation"


def _phase_a(user_file, tmp_path, mode):
    report = tmp_path / f"report_{mode}.txt"
    updated = tmp_path / f"updated_{mode}.yml"
    with as_file(SAMPLE) as standard:
        annotate_missing_parameters(
            user_file=str(user_file),
            standard_file=str(standard),
            uptodate_file=str(updated),
            report_file=str(report),
            mode=mode,
            forcing="off",
        )
    return report.read_text(encoding="utf-8"), updated.read_text(encoding="utf-8")


def test_declared_optional_fields_are_schema_fields():
    assert is_declared_schema_field("sites[0].properties.soil_observation")
    assert is_declared_schema_field(
        "sites[0].properties.land_cover.paved.soil_field_capacity"
    )
    assert not is_declared_schema_field("sites[0].properties.not_a_field")
    assert not is_declared_schema_field("sites[0].properties.soil_observation.smcap")


@pytest.mark.parametrize("mode", ["public", "dev"])
def test_current_observed_yaml_is_not_flagged(tmp_path, mode):
    report, _ = _phase_a(FIXTURES / "canonical.yml", tmp_path, mode)
    assert "soil_observation at level" not in report
    assert "soil_field_capacity at level" not in report
    assert "forbidden locations" not in report


def test_legacy_observed_yaml_gets_migration_not_rename(tmp_path):
    report, updated = _phase_a(FIXTURES / "legacy_method1.yml", tmp_path, "public")
    assert "Legacy observed soil moisture" in report
    assert "--forcing forcing.txt" in report
    assert "soil_moisture_deficit changed to soil_moisture" not in report
    assert "soil_observation at level" not in report
    assert "SMDMethod=2" not in report
    cfg = yaml.safe_load(updated.split("\n", 1)[1] if updated.startswith("#") else updated)
    physics = cfg["model"]["physics"]
    assert "soil_moisture_deficit" in physics and "soil_moisture" not in physics
    assert "depth" in cfg["sites"][0]["properties"]["soil_observation"]


def test_drift_hint_for_current_schema_does_not_suggest_conversion():
    cfg = yaml.safe_load(SAMPLE.read_text(encoding="utf-8"))
    cfg["sites"][0]["properties"]["land_cover"]["paved"]["not_a_field"] = 1
    with pytest.raises(ValueError) as caught:
        SUEWSConfig.from_dict(cfg)
    message = str(caught.value)
    assert "not_a_field" in message
    assert "already declares the current schema version" in message
    assert "suews-convert" not in message


def test_drift_hint_for_legacy_observed_yaml_gives_coordinated_command():
    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    # The legacy selector name also raises its own deprecation warning.
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as caught:
            SUEWSConfig.from_dict(deepcopy(cfg))
    message = str(caught.value)
    assert "legacy observed soil moisture" in message
    assert "--forcing forcing.txt --forcing-output new/forcing.txt" in message
    assert "suews-convert -i <old.yml> -o <new.yml>" not in message


def test_modelled_yaml_with_stale_sensor_block_is_a_plain_upgrade(tmp_path):
    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    cfg["model"]["physics"]["soil_moisture_deficit"] = 0
    source = tmp_path / "modelled.yml"
    source.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    report, _ = _phase_a(source, tmp_path, "public")
    assert "Legacy observed soil moisture" not in report
    assert "soil_moisture_deficit changed to soil_moisture" in report
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as caught:
            SUEWSConfig.from_dict(cfg)
    message = str(caught.value)
    assert "legacy observed soil moisture" not in message
    assert "suews-convert -i <old.yml> -o <new.yml>" in message


@pytest.mark.parametrize(
    "selector,expected",
    [
        (1, True),
        (2, True),
        (1.0, True),
        (2.0, True),
        ({"value": 2.0}, True),
        ("2.0", True),
        ("observed_volumetric", True),
        ("SMDMethod.OBSERVED", True),
        (0, False),
        (0.0, False),
        ({"value": 0.0}, False),
        ("modelled", False),
    ],
)
def test_legacy_selector_values_resolve_numerically(selector, expected):
    from supy.data_model.core.config import _uses_legacy_observed_soil

    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    cfg["model"]["physics"]["soil_moisture_deficit"] = selector
    assert _uses_legacy_observed_soil(cfg) is expected


@pytest.mark.parametrize("alias", ["smdmethod", "soil_moisture_deficit"])
def test_current_yaml_with_selector_alias_is_renamed_not_refused(tmp_path, alias):
    from supy.data_model.core.config import _uses_legacy_observed_soil

    cfg = yaml.safe_load((FIXTURES / "canonical.yml").read_text(encoding="utf-8"))
    physics = cfg["model"]["physics"]
    physics[alias] = physics.pop("soil_moisture")
    assert not _uses_legacy_observed_soil(cfg)
    source = tmp_path / "alias.yml"
    source.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    report, _ = _phase_a(source, tmp_path, "public")
    assert "Legacy observed soil moisture" not in report
    assert f"{alias} changed to soil_moisture" in report


@pytest.mark.parametrize(
    "schema_version,expected",
    [(None, True), ("2026.6.dev5", True), ("2026.6.dev6", False), ("2026.7", False)],
)
def test_alias_without_observation_block_follows_schema_version(
    schema_version, expected
):
    from supy.data_model.core.config import _uses_legacy_observed_soil

    cfg = {"model": {"physics": {"smdmethod": 1}}, "sites": [{"properties": {}}]}
    if schema_version is not None:
        cfg["schema_version"] = schema_version
    assert _uses_legacy_observed_soil(cfg) is expected
