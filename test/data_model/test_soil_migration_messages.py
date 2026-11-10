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
    "key,schema_version,expected",
    [
        ("smdmethod", None, True),
        ("smdmethod", "2026.6.dev5", True),
        ("smdmethod", "2026.6.dev6", False),
        ("smdmethod", "2026.7", False),
        # A declared older schema needs the xsmd conversion whatever the spelling;
        # an undeclared one with the canonical name is taken as current.
        ("soil_moisture", None, False),
        ("soil_moisture", "2026.6.dev5", True),
        ("soil_moisture", "2026.6.dev6", False),
    ],
)
def test_selector_without_observation_block_follows_schema_version(
    key, schema_version, expected
):
    from supy.data_model.core.config import _uses_legacy_observed_soil

    cfg = {"model": {"physics": {key: 1}}, "sites": [{"properties": {}}]}
    if schema_version is not None:
        cfg["schema_version"] = schema_version
    assert _uses_legacy_observed_soil(cfg) is expected


def test_misspelt_child_of_unsampled_optional_block_is_reported(tmp_path):
    from supy.data_model.validation.pipeline.phase_a import find_extra_parameters

    standard = yaml.safe_load(SAMPLE.read_text(encoding="utf-8"))
    cfg = yaml.safe_load((FIXTURES / "canonical.yml").read_text(encoding="utf-8"))
    assert find_extra_parameters(deepcopy(cfg), deepcopy(standard)) == []
    cfg["sites"][0]["properties"]["soil_observation"]["bogus"] = 1
    extras = find_extra_parameters(cfg, standard)
    assert extras == ["sites[0].properties.soil_observation.bogus"]
    source = tmp_path / "bogus.yml"
    source.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    report, _ = _phase_a(source, tmp_path, "public")
    assert "bogus" in report


def _partially_migrated():
    """legacy_method1.yml with one canonical key added, all else legacy."""
    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    cfg["sites"][0]["properties"]["soil_observation"]["layer_top"] = 0
    return cfg


def _mixed_sites():
    """A legacy site alongside a site that already carries canonical bounds."""
    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    canonical = yaml.safe_load((FIXTURES / "canonical.yml").read_text(encoding="utf-8"))
    second = deepcopy(cfg["sites"][0])
    second["name"], second["gridiv"] = "S2", 2
    second["properties"]["soil_observation"] = deepcopy(
        canonical["sites"][0]["properties"]["soil_observation"]
    )
    cfg["sites"].append(second)
    return cfg


def _method2_with_canonical_bounds():
    cfg = yaml.safe_load((FIXTURES / "canonical.yml").read_text(encoding="utf-8"))
    cfg["model"]["physics"]["soil_moisture"] = 2
    return cfg


MIXED_CASES = {
    "partially_migrated": (
        _partially_migrated,
        ["sites[0].properties.soil_observation: legacy depth, smcap; canonical layer_top"],
    ),
    "mixed_sites": (
        _mixed_sites,
        [
            "sites[0].properties.soil_observation: legacy depth, smcap",
            "sites[1].properties.soil_observation: canonical layer_top, layer_depth",
        ],
    ),
    "method2_with_canonical_bounds": (
        _method2_with_canonical_bounds,
        [
            "model.physics.soil_moisture: legacy method 2 (gravimetric)",
            "sites[0].properties.soil_observation: canonical layer_top, layer_depth",
        ],
    ),
}


@pytest.mark.parametrize("case", sorted(MIXED_CASES))
def test_mixed_soil_contracts_are_legacy_and_named(case):
    from supy.data_model.core.config import (
        _soil_contract_conflicts,
        _uses_legacy_observed_soil,
    )

    build, expected = MIXED_CASES[case]
    cfg = build()
    assert _uses_legacy_observed_soil(cfg)
    assert _soil_contract_conflicts(cfg) == expected


@pytest.mark.parametrize("fixture", ["legacy_method1.yml", "canonical.yml"])
def test_single_soil_contract_is_not_a_conflict(fixture):
    from supy.data_model.core.config import _soil_contract_conflicts

    cfg = yaml.safe_load((FIXTURES / fixture).read_text(encoding="utf-8"))
    assert _soil_contract_conflicts(cfg) == []


@pytest.mark.parametrize("case", sorted(MIXED_CASES))
def test_phase_a_keeps_and_reports_mixed_soil_contracts(tmp_path, case):
    build, expected = MIXED_CASES[case]
    cfg = build()
    source = tmp_path / "mixed.yml"
    source.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    report, updated = _phase_a(source, tmp_path, "public")
    assert "Both legacy and canonical soil-moisture inputs were supplied" in report
    for entry in expected:
        assert entry in report
    assert "changed to soil_moisture" not in report
    out = yaml.safe_load(updated.split("\n", 1)[1] if updated.startswith("#") else updated)
    # The user's selector and both key families are left as given.
    assert out["model"]["physics"].keys() & {"soil_moisture_deficit", "soil_moisture"} == (
        cfg["model"]["physics"].keys() & {"soil_moisture_deficit", "soil_moisture"}
    )
    for index, site in enumerate(cfg["sites"]):
        given = site["properties"]["soil_observation"]
        kept = out["sites"][index]["properties"]["soil_observation"]
        assert set(given) <= set(kept)


@pytest.mark.parametrize("case", ["partially_migrated", "mixed_sites"])
def test_drift_hint_for_mixed_soil_contracts_names_the_conflict(case):
    build, expected = MIXED_CASES[case]
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        with pytest.raises(ValueError) as caught:
            SUEWSConfig.from_dict(build())
    message = str(caught.value)
    assert "Both legacy and canonical soil-moisture inputs were supplied" in message
    for entry in expected:
        assert entry in message
    assert "suews-convert -i <old.yml> -o <new.yml>" not in message
    assert "this YAML uses legacy observed soil moisture" not in message


def _phase_a_with_forcing(tmp_path, yaml_name):
    """Run Phase A with forcing checks on a fixture YAML and the legacy xsmd file."""
    import re
    import shutil

    for item in FIXTURES.iterdir():
        shutil.copy(item, tmp_path / item.name)
    shutil.copy(tmp_path / "legacy_method1.txt", tmp_path / "forcing.txt")
    user = tmp_path / yaml_name
    user.write_text(
        re.sub(r"value: \S+\.txt", "value: forcing.txt", user.read_text(encoding="utf-8"), count=1),
        encoding="utf-8",
    )
    report = tmp_path / "report.txt"
    with as_file(SAMPLE) as standard, warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        annotate_missing_parameters(
            user_file=str(user),
            standard_file=str(standard),
            uptodate_file=str(tmp_path / "updated.yml"),
            report_file=str(report),
            mode="public",
            forcing="on",
        )
    return report.read_text(encoding="utf-8")


def test_legacy_yaml_and_xsmd_forcing_report_the_convert_hint_once(tmp_path):
    from supy.util._forcing import SOIL_CONVERT_HINT, UNITS_MIGRATION_GUIDE

    report = _phase_a_with_forcing(tmp_path, "legacy_method1.yml")
    assert "Legacy observed soil moisture" in report
    assert "legacy xsmd forcing and the legacy observed soil-moisture methods" in report
    assert report.count(SOIL_CONVERT_HINT) == 1
    assert report.count(UNITS_MIGRATION_GUIDE) == 1


def test_xsmd_forcing_alone_keeps_its_convert_hint(tmp_path):
    from supy.util._forcing import SOIL_CONVERT_HINT

    report = _phase_a_with_forcing(tmp_path, "canonical.yml")
    assert "Legacy observed soil moisture" not in report
    assert report.count(SOIL_CONVERT_HINT) == 1
