"""
Tests for `supy.util.converter.yaml_upgrade` and the unified
`suews-convert` CLI (#1304).

Covers:
  * auto-detect path: no `-f/--from`, source schema read from the file
  * explicit-override path: `-f/--from` supplied, disagreement warning
  * missing-signature path: no schema_version in file, no `-f/--from` -> error
  * round-trip: upgrade then parse under the current validator
  * CLI wiring: `suews-convert` auto-dispatches YAML input to the upgrader
    via the shared `-i/-o/-f` flag set.
"""

from pathlib import Path

import pytest
import yaml
from click.testing import CliRunner
from supy.cmd.table_converter import convert_table_cmd
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.schema import CURRENT_SCHEMA_VERSION
from supy.util.converter.yaml_upgrade import (
    YamlUpgradeError,
    upgrade_yaml,
)

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
RELEASE_FIXTURE = (
    REPO_ROOT / "test" / "fixtures" / "release_configs" / "2026.4.3.yml"
)
PRE_DRIFT_FIXTURE = (
    REPO_ROOT / "test" / "fixtures" / "release_configs" / "2026.1.28.yml"
)


@pytest.fixture
def release_yaml(tmp_path: Path) -> Path:
    """Copy the vendored 2026.4.3 fixture into a writable scratch location."""
    destination = tmp_path / "source.yml"
    destination.write_text(RELEASE_FIXTURE.read_text(encoding="utf-8"), encoding="utf-8")
    return destination


@pytest.fixture
def signed_yaml(tmp_path: Path, release_yaml: Path) -> Path:
    """A YAML with an explicit `schema_version` at the current value."""
    payload = yaml.safe_load(release_yaml.read_text(encoding="utf-8"))
    payload["schema_version"] = CURRENT_SCHEMA_VERSION
    target = tmp_path / "signed.yml"
    target.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
    return target


@pytest.mark.cfg
class TestYamlUpgradeModule:
    """Behaviour of `upgrade_yaml` at the Python-API level."""

    def test_auto_detect_uses_schema_version_field(
        self, signed_yaml: Path, tmp_path: Path, capsys
    ):
        """With a signed YAML, auto-detect should read the field, not prompt."""
        # ARRANGE
        out = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(input_path=signed_yaml, output_path=out)

        # ASSERT
        captured = capsys.readouterr().err
        assert f"Detected schema version from file: {CURRENT_SCHEMA_VERSION}" in captured
        assert out.exists()
        reloaded = yaml.safe_load(out.read_text(encoding="utf-8"))
        assert reloaded["schema_version"] == CURRENT_SCHEMA_VERSION

    def test_upgraded_yaml_parses_under_current_validator(
        self, signed_yaml: Path, tmp_path: Path
    ):
        """Upgraded output must survive SUEWSConfig.from_yaml."""
        # ARRANGE
        out = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(input_path=signed_yaml, output_path=out)

        # ASSERT
        reloaded = SUEWSConfig.from_yaml(str(out))
        assert reloaded.name is not None

    def test_explicit_override_respected(
        self, signed_yaml: Path, tmp_path: Path, capsys
    ):
        """When `from_ver` is supplied, the CLI should log it was used."""
        # ARRANGE
        out = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=signed_yaml,
            output_path=out,
            from_ver=CURRENT_SCHEMA_VERSION,
        )

        # ASSERT
        captured = capsys.readouterr().err
        assert f"Using user-supplied --from={CURRENT_SCHEMA_VERSION}" in captured

    def test_explicit_override_warns_on_disagreement(
        self, signed_yaml: Path, tmp_path: Path, capsys
    ):
        """Disagreement between `--from-ver` and file signature -> warn."""
        # ARRANGE
        out = tmp_path / "upgraded.yml"

        # ACT
        # `2026.4.3` resolves to schema 2025.12 via _PACKAGE_TO_SCHEMA but
        # we'll mutate the signature to '9.9' to force disagreement.
        cfg = yaml.safe_load(signed_yaml.read_text(encoding="utf-8"))
        cfg["schema_version"] = "9.9"
        signed_yaml.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

        # The override (CURRENT) will clash with '9.9' in the file.
        with pytest.raises(YamlUpgradeError):
            # source schema stays at CURRENT, target is CURRENT -> no handler
            # needed; but disagreement warning should still fire first.
            upgrade_yaml(
                input_path=signed_yaml,
                output_path=out,
                from_ver="unknown-release",
            )

        captured = capsys.readouterr().err
        assert "WARNING" in captured
        assert "disagrees with file signature 9.9" in captured


    def test_missing_signature_raises_error_message_points_at_from_flag(
        self, release_yaml: Path, tmp_path: Path
    ):
        """The error when no signature is present must point at the `-f` flag."""
        # ARRANGE
        cfg = yaml.safe_load(release_yaml.read_text(encoding="utf-8"))
        for key in ("schema_version", "version", "config_version"):
            cfg.pop(key, None)
        release_yaml.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
        out = tmp_path / "upgraded.yml"

        # ACT / ASSERT
        with pytest.raises(YamlUpgradeError) as excinfo:
            upgrade_yaml(input_path=release_yaml, output_path=out)
        assert "-f/--from" in str(excinfo.value)

    def test_missing_signature_raises(
        self, release_yaml: Path, tmp_path: Path
    ):
        """YAML with no signature and no `--from-ver` -> YamlUpgradeError."""
        # ARRANGE: strip any version hints from the fixture
        cfg = yaml.safe_load(release_yaml.read_text(encoding="utf-8"))
        for key in ("schema_version", "version", "config_version"):
            cfg.pop(key, None)
        release_yaml.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
        out = tmp_path / "upgraded.yml"

        # ACT / ASSERT
        with pytest.raises(YamlUpgradeError) as excinfo:
            upgrade_yaml(input_path=release_yaml, output_path=out)
        assert "No schema_version field found" in str(excinfo.value)


@pytest.mark.cfg
class TestSuewsConvertYamlPath:
    """Unified `suews-convert` CLI routing for YAML inputs."""

    def test_help_describes_yaml_input(self):
        """Root `suews-convert --help` must advertise the YAML upgrade path."""
        # ARRANGE / ACT
        runner = CliRunner()
        result = runner.invoke(convert_table_cmd, ["--help"])

        # ASSERT
        assert result.exit_code == 0
        assert ".yml" in result.output
        assert "YAML" in result.output or "cross-release" in result.output

    def test_yaml_input_end_to_end(self, signed_yaml: Path, tmp_path: Path):
        """`suews-convert -i old.yml -o new.yml` routes via `upgrade_yaml`."""
        # ARRANGE
        out = tmp_path / "upgraded.yml"
        runner = CliRunner()

        # ACT
        result = runner.invoke(
            convert_table_cmd,
            [
                "--input",
                str(signed_yaml),
                "--output",
                str(out),
            ],
        )

        # ASSERT
        assert result.exit_code == 0, result.output
        assert out.exists()
        SUEWSConfig.from_yaml(str(out))  # must still parse

    def test_yaml_input_with_explicit_from(self, tmp_path: Path):
        """`suews-convert -f <release-tag>` drives the upgrade path for YAML."""
        # ARRANGE
        assert PRE_DRIFT_FIXTURE.exists()
        out = tmp_path / "upgraded.yml"
        runner = CliRunner()

        # ACT
        result = runner.invoke(
            convert_table_cmd,
            [
                "--input",
                str(PRE_DRIFT_FIXTURE),
                "--output",
                str(out),
                "--from",
                "2026.1.28",
            ],
        )

        # ASSERT
        assert result.exit_code == 0, result.output
        SUEWSConfig.from_yaml(str(out))

    def test_bare_invocation_shows_help(self):
        """`suews-convert` with no flags should exit cleanly showing help."""
        # ARRANGE / ACT
        runner = CliRunner()
        result = runner.invoke(convert_table_cmd, [])

        # ASSERT: the command prints help and exits 0 when no flags are given.
        assert result.exit_code == 0
        assert "-i" in result.output
        assert ".yml" in result.output


@pytest.mark.cfg
class TestPreSetpointSplitMigration:
    """Handler behaviour for the gh#1261 STEBBS setpoint split (release 2026.1.28)."""

    def test_pre_drift_fixture_upgrades_to_current_schema(self, tmp_path: Path):
        """The vendored 2026.1.28 fixture must upgrade cleanly under the handler."""
        # ARRANGE
        assert PRE_DRIFT_FIXTURE.exists(), PRE_DRIFT_FIXTURE
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=PRE_DRIFT_FIXTURE,
            output_path=upgraded,
            from_ver="2026.1.28",
        )

        # ASSERT: stamped schema version + renamed profile fields present
        payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
        assert payload["schema_version"] == CURRENT_SCHEMA_VERSION
        arch = payload["sites"][0]["properties"]["building_archetype"]
        assert "HeatingSetpointTemperatureProfile" in arch
        assert "CoolingSetpointTemperatureProfile" in arch
        assert isinstance(arch["HeatingSetpointTemperature"], dict)
        assert "value" in arch["HeatingSetpointTemperature"]
        # Profile intent is preserved by defaulting setpointmethod to SCHEDULED=2
        assert payload["model"]["physics"]["setpointmethod"]["value"] == 2

    def test_upgraded_pre_drift_fixture_validates(self, tmp_path: Path):
        """Running the handler's output through SUEWSConfig.from_yaml must succeed."""
        # ARRANGE
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=PRE_DRIFT_FIXTURE,
            output_path=upgraded,
            from_ver="2026.1.28",
        )

        # ASSERT
        SUEWSConfig.from_yaml(str(upgraded))
