"""
Tests for SUEWSConfig.from_yaml error handling.

Covers the schema-drift hints added in #1303:
  * raw TypeError / AttributeError from Pydantic union validation gets
    wrapped into a ValueError that names detected/current schema versions
    and the unified `suews-convert` command
  * ValidationError carrying `extra_forbidden` on a top-level key gains
    the same drift hint
  * a valid YAML still parses without raising
"""

from pathlib import Path

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.schema import CURRENT_SCHEMA_VERSION

pytestmark = pytest.mark.api


@pytest.fixture
def sample_config_dict() -> dict:
    """Load the packaged sample configuration as a mutable dict."""
    path = trv_supy_module / "sample_data" / "sample_config.yml"
    with path.open() as f:
        return yaml.safe_load(f)


def _write_yaml(tmp_path: Path, data: dict) -> Path:
    """Serialise `data` to a YAML file under `tmp_path` and return the path."""
    target = tmp_path / "drifted.yml"
    with target.open("w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False)
    return target


@pytest.mark.cfg
class TestFromYamlDriftHints:
    """Parse failures surface actionable schema-drift hints."""

    def test_valid_yaml_still_parses(self, sample_config_dict, tmp_path):
        """Positive control: the packaged sample still loads cleanly."""
        # ARRANGE
        path = _write_yaml(tmp_path, sample_config_dict)

        # ACT
        config = SUEWSConfig.from_yaml(str(path))

        # ASSERT
        assert config.name == "sample_config"

    def test_extra_forbidden_gets_drift_hint(self, sample_config_dict, tmp_path):
        """A stray field on a nested `extra=forbid` model triggers the hint.

        `SUEWSConfig` itself uses `extra="allow"`, but nested models
        (e.g. `SiteProperties`) forbid extras, so we trigger the drift smell
        there.
        """
        # ARRANGE
        drifted = dict(sample_config_dict)
        drifted["sites"][0]["properties"][
            "removed_field_from_old_release"
        ] = 42
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "suews-convert -i <old.yml>" in message
        assert f"Current schema version:  {CURRENT_SCHEMA_VERSION}" in message
        assert "Detected schema version:" in message

    def test_typeerror_path_gets_drift_hint(self, sample_config_dict, tmp_path):
        """A raw TypeError from validation is wrapped with the drift hint."""
        # ARRANGE: force a TypeError through from_yaml. We monkeypatch the
        # config-construction step to mimic what union dispatch does when
        # drifted YAML reaches a model with a kwarg-strict __init__.
        path = _write_yaml(tmp_path, sample_config_dict)

        original_init = SUEWSConfig.__init__

        def raise_type_error(self, **_):
            raise TypeError(
                "RefValue.__init__() got an unexpected keyword argument 'working_day'"
            )

        SUEWSConfig.__init__ = raise_type_error
        try:
            with pytest.raises(ValueError) as excinfo:
                SUEWSConfig.from_yaml(str(path))
        finally:
            SUEWSConfig.__init__ = original_init

        # ASSERT
        message = str(excinfo.value)
        assert "suspected schema drift" in message
        assert "suews-convert -i <old.yml>" in message
        assert "RefValue.__init__()" in message
        assert f"Current schema version:  {CURRENT_SCHEMA_VERSION}" in message

    def test_drift_hint_names_detected_schema_version(
        self, sample_config_dict, tmp_path
    ):
        """When the YAML declares schema_version, it shows up as 'Detected'."""
        # ARRANGE
        drifted = dict(sample_config_dict)
        drifted["schema_version"] = "2025.12"
        drifted["sites"][0]["properties"]["from_an_old_release"] = True
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "Detected schema version: 2025.12" in message

    def test_unsigned_yaml_hint_requests_from_ver(
        self, sample_config_dict, tmp_path
    ):
        """Unsigned YAMLs must not be told to run the bare suews-convert command.

        The CLI rejects unsigned YAMLs unless `-f/--from` is supplied, so
        the hint must advertise that flag and must not claim a spurious
        "Detected schema version: <current>" that was only there because
        from_yaml stamped CURRENT_SCHEMA_VERSION as a default.
        """
        # ARRANGE - strip the signature to mimic a pre-schema-version release
        drifted = dict(sample_config_dict)
        drifted.pop("schema_version", None)
        drifted["sites"][0]["properties"]["from_an_old_release"] = True
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "No schema_version field in YAML" in message
        assert "-f <release-tag>" in message
        assert f"Detected schema version: {CURRENT_SCHEMA_VERSION}" not in message
