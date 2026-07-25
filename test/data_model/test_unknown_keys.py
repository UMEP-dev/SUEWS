"""Tests for unrecognised-YAML-key detection (gh#1647).

Pydantic's default ``extra="ignore"`` silently drops a key the model does
not declare, so a mistyped or outdated field name leaves the user with the
default value and no signal. These tests pin the detector's two obligations:
it must find every dropped key, and it must stay silent on configurations
the model genuinely honours.
"""

import copy
import logging
from pathlib import Path

import pytest
import yaml

import supy
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation.core.unknown_keys import (
    collect_unknown_keys,
    format_unknown_keys_report,
)

# Exercises the pydantic/YAML config surface, not numerical behaviour.
pytestmark = pytest.mark.api


@pytest.fixture(scope="module")
def sample_config_data() -> dict:
    """Raw mapping of the shipped sample configuration."""
    path = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _paths(data: dict) -> set:
    return {item.path for item in collect_unknown_keys(data, SUEWSConfig)}


class TestNoFalsePositives:
    """The detector must not flag configurations the model honours.

    A warning that fires on a valid config is worse than no warning at all:
    users learn to ignore it, and the real hits go unread with it.
    """

    def test_shipped_sample_config_is_clean(self, sample_config_data):
        assert collect_unknown_keys(sample_config_data, SUEWSConfig) == []

    @pytest.mark.parametrize(
        "path",
        [
            "model.physics.net_radiation",
            "model.physics.storage_heat",
            "model.physics.emissions",
        ],
    )
    def test_nested_physics_family_blocks_are_not_flagged(
        self, sample_config_data, path
    ):
        """Scheme mappings such as ``net_radiation: {narp: {...}}`` are folded
        by a before-validator, so their inner keys are not stray fields."""
        flagged = _paths(sample_config_data)
        assert not any(item.startswith(f"{path}.") for item in flagged)

    @pytest.mark.parametrize(
        ("section", "key", "value"),
        [
            # Public YAML spellings that differ from the field name.
            ("physics", "leaf_area_index", {"value": 0}),
            ("physics", "snow", {"value": 0}),
            # Flat STEBBS switches, folded into model.physics.stebbs.
            ("physics", "same_albedo_roof", True),
            ("physics", "rcmethod", 1),
        ],
    )
    def test_validator_consumed_keys_are_not_flagged(
        self, sample_config_data, section, key, value
    ):
        data = copy.deepcopy(sample_config_data)
        data["model"][section].pop(key, None)
        data["model"][section][key] = value
        assert f"model.{section}.{key}" not in _paths(data)

    @pytest.mark.parametrize("key", ["forcing_file", "output_file"])
    def test_legacy_control_keys_are_not_flagged(self, sample_config_data, key):
        """These are lifted into nested objects and warn on their own."""
        data = copy.deepcopy(sample_config_data)
        data["model"]["control"][key] = "legacy.txt"
        assert f"model.control.{key}" not in _paths(data)

    def test_reserved_bookkeeping_keys_are_ignored(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["_yaml_path"] = "somewhere.yml"
        data["_validation_summary"] = {"anything": 1}
        assert collect_unknown_keys(data, SUEWSConfig) == []


class TestDetection:
    """Every key the model would drop must be reported, at any depth."""

    @pytest.mark.parametrize(
        ("keys", "expected_path"),
        [
            (["model", "physics"], "model.physics.totally_bogus_key"),
            (["model", "control"], "model.control.totally_bogus_key"),
        ],
    )
    def test_unknown_key_is_reported(self, sample_config_data, keys, expected_path):
        data = copy.deepcopy(sample_config_data)
        target = data
        for key in keys:
            target = target[key]
        target["totally_bogus_key"] = 42
        assert expected_path in _paths(data)

    def test_unknown_key_reported_at_site_level(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["sites"][0]["bogus_site_level"] = 3
        assert "sites[0].bogus_site_level" in _paths(data)

    def test_unknown_key_reported_deep_in_land_cover(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["sites"][0]["properties"]["land_cover"]["paved"]["bogus"] = 1
        assert "sites[0].properties.land_cover.paved.bogus" in _paths(data)

    def test_unknown_key_reported_in_initial_states(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["sites"][0]["initial_states"]["paved"]["bogus_state"] = 1
        assert "sites[0].initial_states.paved.bogus_state" in _paths(data)


class TestSuggestions:
    """Sue Grimmond's requirement on gh#1647: say it is unknown, and say
    what was probably meant."""

    def test_camel_case_legacy_name_resolves_to_current_field(self, sample_config_data):
        """``WaterUseMethod`` is the spelling every historical RunControl.nml
        used; the rename registry holds only the lowercase form, so the
        lookup must be case-insensitive."""
        data = copy.deepcopy(sample_config_data)
        data["model"]["physics"]["WaterUseMethod"] = 1

        (found,) = [
            item
            for item in collect_unknown_keys(data, SUEWSConfig)
            if item.key == "WaterUseMethod"
        ]
        assert found.suggestion == "water_use"
        assert found.reason == "legacy"
        assert "did you mean 'water_use'?" in found.describe()

    def test_plain_typo_matches_sibling_field(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["model"]["physics"]["stroage_heat"] = "ohm"

        (found,) = [
            item
            for item in collect_unknown_keys(data, SUEWSConfig)
            if item.key == "stroage_heat"
        ]
        assert found.suggestion == "storage_heat"
        assert found.reason == "typo"

    def test_unrelated_key_gets_no_suggestion(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["model"]["physics"]["zzzz_nothing_like_a_field"] = 1

        (found,) = [
            item
            for item in collect_unknown_keys(data, SUEWSConfig)
            if item.key == "zzzz_nothing_like_a_field"
        ]
        assert found.suggestion is None
        assert "will be ignored" in found.describe()


class TestReport:
    def test_empty_report_is_falsy(self):
        assert format_unknown_keys_report([]) == ""

    def test_report_names_every_key(self, sample_config_data):
        data = copy.deepcopy(sample_config_data)
        data["model"]["physics"]["WaterUseMethod"] = 1
        data["model"]["control"]["BogusControl"] = "x"

        report = format_unknown_keys_report(collect_unknown_keys(data, SUEWSConfig))
        assert "2 unrecognised configuration keys" in report
        assert "model.physics.WaterUseMethod" in report
        assert "model.control.BogusControl" in report


class TestLoadPathIntegration:
    """The detection is worthless if it only runs in the opt-in validator:
    the silent drop happens on the ordinary load path (gh#1647)."""

    def test_from_dict_warns_about_unknown_key(self, sample_config_data, caplog):
        data = copy.deepcopy(sample_config_data)
        data["model"]["physics"]["WaterUseMethod"] = 1

        with caplog.at_level(logging.WARNING, logger="SuPy"):
            config = SUEWSConfig.from_dict(data, strict=False)

        assert "model.physics.WaterUseMethod" in caplog.text
        assert "water_use" in caplog.text
        # The config still loads: this is advisory, not a rejection.
        assert config is not None

    def test_clean_config_produces_no_unknown_key_warning(
        self, sample_config_data, caplog
    ):
        with caplog.at_level(logging.WARNING, logger="SuPy"):
            SUEWSConfig.from_dict(copy.deepcopy(sample_config_data), strict=False)

        assert "unrecognised configuration" not in caplog.text
