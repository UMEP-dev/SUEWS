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

from click.testing import CliRunner
import pytest
import yaml

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
        # `2026.4.3` resolves to the current schema via _PACKAGE_TO_SCHEMA;
        # mutate the signature to '9.9' to force disagreement.
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

    def test_raises_on_old_and_new_key_conflict(self, tmp_path: Path):
        """Upgrade should not silently drop one side of an old/new key clash."""
        source = tmp_path / "conflict.yml"
        target = tmp_path / "upgraded.yml"
        payload = {
            "schema_version": "2026.4",
            "model": {
                "physics": {
                    "snowuse": {"value": 0},
                    "snow_use": {"value": 1},
                }
            },
        }
        source.write_text(
            yaml.safe_dump(payload, sort_keys=False),
            encoding="utf-8",
        )

        with pytest.raises(YamlUpgradeError, match=r"Both 'snowuse'.*'snow_use'"):
            upgrade_yaml(input_path=source, output_path=target)

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
class TestIdentityPathReleaseTags:
    """`-f <release-tag>` must resolve for every post-split identity-path fixture.

    Regression for the bug where release tag `2026.4.3` was missing from
    `_PACKAGE_TO_SCHEMA`: passing it through `-f` bypasses auto-detect, so
    the resolver returned the raw tag and the registry lookup failed
    with a "no handler registered" error. Older release tags
    (`2025.10.15`, `2025.11.20`) are covered by
    `TestPre2026_1ReleaseTagMigration` because they need real cleanup.
    """

    @pytest.mark.parametrize(
        "release_tag",
        ["2026.4.3"],
    )
    def test_identity_release_tag_upgrades_cleanly(
        self, release_tag: str, tmp_path: Path
    ):
        """Each identity-path release tag must map to the current schema."""
        # ARRANGE
        fixture = (
            REPO_ROOT
            / "test"
            / "fixtures"
            / "release_configs"
            / f"{release_tag}.yml"
        )
        assert fixture.exists(), fixture
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=fixture,
            output_path=upgraded,
            from_ver=release_tag,
        )

        # ASSERT
        payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
        assert payload["schema_version"] == CURRENT_SCHEMA_VERSION
        SUEWSConfig.from_yaml(str(upgraded))


@pytest.mark.cfg
class TestPre2026_1ReleaseTagMigration:
    """Handler behaviour for 2025.10.15 / 2025.11.20 release YAMLs.

    Both releases predate the Nov 2025 STEBBS clean-up that renamed
    `Wallx1`/`Roofx1` to `*OuterCapFrac` and removed
    `DHWVesselEmissivity`, as well as the later #1240 / #1242 / #1261
    schema deltas. Before this was wired up, those tags were mapped
    straight to `CURRENT_SCHEMA_VERSION` and the deprecated fields were
    silently dropped by Pydantic `extra="ignore"` on `StebbsProperties`.
    """

    @pytest.mark.parametrize(
        "release_tag", ["2025.10.15", "2025.11.20"]
    )
    def test_pre_2026_1_release_migrates_stebbs_renames(
        self, release_tag: str, tmp_path: Path
    ):
        """Wallx1/Roofx1 -> *OuterCapFrac rename must survive the upgrade."""
        # ARRANGE
        fixture = (
            REPO_ROOT
            / "test"
            / "fixtures"
            / "release_configs"
            / f"{release_tag}.yml"
        )
        assert fixture.exists(), fixture
        source_text = fixture.read_text(encoding="utf-8")
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=fixture,
            output_path=upgraded,
            from_ver=release_tag,
        )

        # ASSERT: stamped version and rename propagated, with the stale
        # source key gone.
        payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
        assert payload["schema_version"] == CURRENT_SCHEMA_VERSION
        arch = payload["sites"][0]["properties"]["building_archetype"]
        stebbs = payload["sites"][0]["properties"]["stebbs"]
        if "Wallx1" in source_text:
            assert "Wallx1" not in arch
            assert "wall_outer_heat_capacity_fraction" in arch
        if "Roofx1" in source_text:
            assert "Roofx1" not in arch
            assert "roof_outer_heat_capacity_fraction" in arch
        # DHWVesselEmissivity was removed during the Nov 2025 clean-up.
        assert "DHWVesselEmissivity" not in stebbs
        # The volume bounds were removed in #1242.
        assert "MinimumVolumeOfDHWinUse" not in stebbs
        assert "MaximumVolumeOfDHWinUse" not in stebbs

    @pytest.mark.parametrize(
        "release_tag", ["2025.10.15", "2025.11.20"]
    )
    def test_pre_2026_1_release_upgrades_and_validates(
        self, release_tag: str, tmp_path: Path
    ):
        """Upgrader output for older releases must parse under the current validator."""
        # ARRANGE
        fixture = (
            REPO_ROOT
            / "test"
            / "fixtures"
            / "release_configs"
            / f"{release_tag}.yml"
        )
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=fixture,
            output_path=upgraded,
            from_ver=release_tag,
        )

        # ASSERT
        SUEWSConfig.from_yaml(str(upgraded))

    @pytest.mark.parametrize(
        "release_tag", ["2025.10.15", "2025.11.20"]
    )
    def test_scalar_only_release_keeps_setpoint_constant(
        self, release_tag: str, tmp_path: Path
    ):
        """Scalar-only YAMLs (2025.10.15 / 2025.11.20, no *Profile siblings)
        must not be flipped to setpointmethod=SCHEDULED; otherwise Phase B
        validation nulls the scalars and heating/cooling fall back to off
        defaults (gh#1304).
        """
        # ARRANGE
        fixture = (
            REPO_ROOT
            / "test"
            / "fixtures"
            / "release_configs"
            / f"{release_tag}.yml"
        )
        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=fixture,
            output_path=upgraded,
            from_ver=release_tag,
        )

        # ASSERT: scalar setpoints preserved, no *Profile synthesised,
        # setpointmethod NOT forced to SCHEDULED (stays at its default of
        # CONSTANT=0 so the user's scalars drive the run).
        payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
        arch = payload["sites"][0]["properties"]["building_archetype"]
        assert isinstance(arch.get("heating_setpoint_temperature"), dict)
        assert arch["heating_setpoint_temperature"].get("value") is not None
        assert isinstance(arch.get("cooling_setpoint_temperature"), dict)
        assert arch["cooling_setpoint_temperature"].get("value") is not None
        assert "heating_setpoint_temperature_profile" not in arch
        assert "cooling_setpoint_temperature_profile" not in arch
        physics = payload.get("model", {}).get("physics", {})
        setpointmethod = physics.get("setpointmethod")
        # Either absent (use enum default CONSTANT=0) or explicitly 0/1.
        if setpointmethod is not None:
            value = (
                setpointmethod.get("value")
                if isinstance(setpointmethod, dict)
                else setpointmethod
            )
            assert value in (0, 1), (
                f"setpointmethod must not be flipped to SCHEDULED=2 for "
                f"scalar-only {release_tag} YAMLs; got {value!r}"
            )


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
        stebbs = payload["sites"][0]["properties"]["stebbs"]
        assert "heating_setpoint_temperature_profile" in arch
        assert "cooling_setpoint_temperature_profile" in arch
        assert isinstance(arch["heating_setpoint_temperature"], dict)
        assert "value" in arch["heating_setpoint_temperature"]
        # Profile intent is preserved by defaulting setpoint to SCHEDULED=2
        # (key was renamed from fused `setpointmethod` to `setpoint` in #1321)
        assert payload["model"]["physics"]["setpoint"]["value"] == 2
        # #1240 rename lives under the stebbs sub-tree, not building_archetype.
        assert "DeepSoilTemperature" not in stebbs
        assert "annual_mean_air_temperature" in stebbs
        # #1242 drop: DHW volume bounds removed from the schema entirely.
        assert "MinimumVolumeOfDHWinUse" not in stebbs
        assert "MaximumVolumeOfDHWinUse" not in stebbs

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


@pytest.mark.cfg
class TestNoSilentFieldDrops:
    """Every STEBBS field in a source release fixture must be accounted for.

    `StebbsProperties` sets no `extra=` on its Pydantic config, so the
    default `extra="ignore"` policy silently swallows unknown fields.
    Without this guard, a new schema rename/removal on master would slip
    through the upgrade path and users would see their values vanish with
    no log trace. For each vendored pre-drift fixture, assert every
    stebbs-scoped key from the source either survives to the upgraded
    output or is explicitly logged as renamed/dropped.
    """

    @pytest.mark.parametrize(
        "release_tag", ["2025.10.15", "2025.11.20", "2026.1.28"]
    )
    def test_every_stebbs_key_is_either_preserved_renamed_or_logged(
        self,
        release_tag: str,
        tmp_path: Path,
        capsys: pytest.CaptureFixture[str],
    ):
        # ARRANGE
        fixture = (
            REPO_ROOT
            / "test"
            / "fixtures"
            / "release_configs"
            / f"{release_tag}.yml"
        )
        source = yaml.safe_load(fixture.read_text(encoding="utf-8"))
        source_stebbs_keys: set[str] = set()
        for site in source.get("sites", []):
            for section in ("building_archetype", "stebbs"):
                container = site.get("properties", {}).get(section, {}) or {}
                source_stebbs_keys.update(container.keys())

        upgraded = tmp_path / "upgraded.yml"

        # ACT
        upgrade_yaml(
            input_path=fixture,
            output_path=upgraded,
            from_ver=release_tag,
        )
        captured = capsys.readouterr()
        log = captured.err

        upgraded_payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
        upgraded_keys: set[str] = set()
        for site in upgraded_payload.get("sites", []):
            for section in ("building_archetype", "stebbs"):
                container = site.get("properties", {}).get(section, {}) or {}
                upgraded_keys.update(container.keys())

        # ASSERT: each source key is preserved, appears in a rename log,
        # or appears in a drop log. Anything else indicates a silent drop
        # via Pydantic extra="ignore" and needs a new handler entry.
        missing_without_log: list[str] = []
        for key in source_stebbs_keys:
            if key in upgraded_keys:
                continue
            if f"renamed {key!r}" in log:
                continue
            if f"dropped {key!r}" in log:
                continue
            if f"split {key!r}" in log:
                continue
            missing_without_log.append(key)

        assert not missing_without_log, (
            f"Fields removed from upgraded YAML without a migration log entry: "
            f"{sorted(missing_without_log)}. Add a handler delta in "
            f"src/supy/util/converter/yaml_upgrade.py."
        )


def test_forcing_file_string_migrates_to_forcing_subobject():
    """gh#1372: forcing_file: 'x.txt' -> forcing: {file: 'x.txt'}."""
    from supy.util.converter.yaml_upgrade import _apply_forcing_subobject_restructure

    cfg = {
        "schema_version": "2026.5.dev6",
        "model": {"control": {"forcing_file": "forcing.txt"}},
    }
    out = _apply_forcing_subobject_restructure(cfg)
    assert "forcing_file" not in out["model"]["control"]
    assert out["model"]["control"]["forcing"] == {"file": "forcing.txt"}


def test_forcing_file_refvalue_migrates_to_forcing_subobject():
    """RefValue-wrapped forcing_file ({value: ...}) is preserved under .file."""
    from supy.util.converter.yaml_upgrade import _apply_forcing_subobject_restructure

    cfg = {
        "schema_version": "2026.5.dev6",
        "model": {"control": {"forcing_file": {"value": "forcing.txt"}}},
    }
    out = _apply_forcing_subobject_restructure(cfg)
    assert out["model"]["control"]["forcing"] == {"file": {"value": "forcing.txt"}}


def test_forcing_file_list_migrates_to_forcing_subobject():
    """List-of-paths forcing_file is preserved under .file."""
    from supy.util.converter.yaml_upgrade import _apply_forcing_subobject_restructure

    cfg = {
        "schema_version": "2026.5.dev6",
        "model": {"control": {"forcing_file": ["a.txt", "b.txt"]}},
    }
    out = _apply_forcing_subobject_restructure(cfg)
    assert out["model"]["control"]["forcing"] == {"file": ["a.txt", "b.txt"]}


def test_dev6_to_current_handler_registered():
    """Compatibility from 2026.5.dev6 must be granted via _HANDLERS."""
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
    from supy.util.converter.yaml_upgrade import _HANDLERS

    assert ("2026.5.dev6", CURRENT_SCHEMA_VERSION) in _HANDLERS


class TestOutputSubobjectRestructure:
    """gh#1372 follow-up: dev7 -> dev8 lifts output_file -> output."""

    def _migrate(self, cfg: dict) -> dict:
        from supy.util.converter.yaml_upgrade import (
            _migrate_2026_5_dev7_to_current,
        )
        return _migrate_2026_5_dev7_to_current(cfg)

    def test_dict_form_lifts_and_renames_path(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output_file": {
                        "format": "txt",
                        "freq": 3600,
                        "groups": ["SUEWS"],
                        "path": "./legacy_out",
                    },
                },
            },
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        result = out["model"]["control"]["output"]
        assert result["format"] == "txt"
        assert result["freq"] == 3600
        assert result["groups"] == ["SUEWS"]
        assert result["dir"] == "./legacy_out"
        assert "path" not in result

    def test_dict_form_already_uses_dir(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output_file": {
                        "format": "parquet",
                        "freq": 1800,
                        "dir": "./already_dir",
                    },
                },
            },
        }
        out = self._migrate(cfg)
        assert out["model"]["control"]["output"]["dir"] == "./already_dir"
        assert "path" not in out["model"]["control"]["output"]

    def test_string_form_dropped(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {"control": {"output_file": "output.txt"}},
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        # No `output` synthesised - the migrator drops the string form;
        # the data-model default_factory re-installs OutputControl() at load.
        assert "output" not in out["model"]["control"]

    def test_already_migrated_is_idempotent(self):
        cfg = {
            "schema_version": "2026.5.dev8",
            "model": {
                "control": {
                    "output": {"format": "parquet", "dir": "./out"},
                },
            },
        }
        out = self._migrate(cfg)
        assert out["model"]["control"]["output"] == {
            "format": "parquet",
            "dir": "./out",
        }

    def test_both_keys_output_wins(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output": {"format": "parquet", "dir": "./new"},
                    "output_file": {"format": "txt", "path": "./old"},
                },
            },
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        assert out["model"]["control"]["output"]["dir"] == "./new"
        assert out["model"]["control"]["output"]["format"] == "parquet"

    def test_dev7_to_current_handler_registered(self):
        """Compatibility from 2026.5.dev7 must be granted via _HANDLERS."""
        from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
        from supy.util.converter.yaml_upgrade import _HANDLERS

        assert ("2026.5.dev7", CURRENT_SCHEMA_VERSION) in _HANDLERS
