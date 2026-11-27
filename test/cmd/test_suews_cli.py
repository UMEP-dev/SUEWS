"""Tests for the unified ``suews`` CLI dispatcher.

Validates:
- `suews --help` lists the public subcommands
  (validate/schema/convert/init/run/knowledge).
- Each subcommand resolves to its existing Click implementation.
- The legacy hyphenated entry points (`suews-run`, `-convert`, `-validate`,
  `-schema`) print a deprecation notice and forward to the underlying command.
"""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
import json
from pathlib import Path
import re
import sys

import click
from click.testing import CliRunner
import pytest
import yaml

from supy.cmd.suews_cli import cli as suews_cli_root

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API", "C-ERROR")]


# ---------------------------------------------------------------------------
# Top-level group
# ---------------------------------------------------------------------------


def test_top_level_help_lists_subcommands() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0, result.output
    for sub in ("validate", "schema", "convert", "init", "run", "knowledge"):
        assert sub in result.output, f"subcommand {sub!r} missing from help output"
    assert "rust" not in result.output


def test_validate_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["validate", "--help"])
    assert result.exit_code == 0, result.output
    assert "validate" in result.output.lower()


def test_schema_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["schema", "--help"])
    assert result.exit_code == 0, result.output


def test_convert_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["convert", "--help"])
    assert result.exit_code == 0, result.output


def test_run_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["run", "--help"])
    assert result.exit_code == 0, result.output


def test_knowledge_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["knowledge", "--help"])
    assert result.exit_code == 0, result.output


def test_init_subcommand_help() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["init", "--help"])
    assert result.exit_code == 0, result.output
    assert "template" in result.output.lower()


def test_rust_subcommand_is_not_registered() -> None:
    from supy.cmd.suews_cli import cli

    assert "rust" not in cli.commands


# ---------------------------------------------------------------------------
# Subcommand registry preserves the underlying Click commands
# ---------------------------------------------------------------------------


def test_validate_subcommand_is_validate_cli() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.validate_config import cli as validate_cli

    assert cli.commands["validate"] is validate_cli


def test_schema_subcommand_is_schema_cli() -> None:
    from supy.cmd.schema_cli import cli as schema_cli
    from supy.cmd.suews_cli import cli

    assert cli.commands["schema"] is schema_cli


def test_convert_subcommand_is_convert_cmd() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.table_converter import convert_table_cmd

    assert cli.commands["convert"] is convert_table_cmd


def test_run_subcommand_is_suews_cmd() -> None:
    from supy.cmd.SUEWS import SUEWS as suews_run
    from supy.cmd.suews_cli import cli

    assert cli.commands["run"] is suews_run


def test_knowledge_subcommand_is_knowledge_group() -> None:
    from supy.cmd.knowledge_cli import knowledge_group
    from supy.cmd.suews_cli import cli

    assert cli.commands["knowledge"] is knowledge_group


def test_init_subcommand_is_init_case_cmd() -> None:
    from supy.cmd.init_case import init_case_cmd
    from supy.cmd.suews_cli import cli

    assert cli.commands["init"] is init_case_cmd


@pytest.mark.parametrize("subcommand", ["info", "version", "migrate", "export"])
def test_schema_public_subcommands_have_help(subcommand: str) -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["schema", subcommand, "--help"])
    assert result.exit_code == 0, result.output


def test_schema_validate_is_not_public() -> None:
    from supy.cmd.suews_cli import cli

    assert "validate" not in cli.commands["schema"].commands


def test_validate_help_keeps_schema_lifecycle_hidden() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["validate", "--help"])
    assert result.exit_code == 0, result.output
    for hidden_subcommand in ("migrate", "export"):
        assert hidden_subcommand not in result.output


def test_validate_is_a_plain_command() -> None:
    """``suews validate`` takes FILES directly and declares no subcommands.

    Its variadic FILES argument would swallow any subcommand name, so
    subcommands registered on it could never be reached; schema lifecycle
    operations live under ``suews schema`` instead.
    """
    import click

    from supy.cmd.validate_config import cli as validate_cli

    assert not isinstance(validate_cli, click.Group)


# ---------------------------------------------------------------------------
# suews schema version / migrate: backup handling
# ---------------------------------------------------------------------------


def _write_old_config(path: Path) -> None:
    path.write_text("name: test\nschema_version: '0.1'\n", encoding="utf-8")


def test_schema_version_update_keeps_backup_by_default(tmp_path) -> None:
    import yaml as _yaml
    from supy.cmd.suews_cli import cli

    config = tmp_path / "config.yml"
    _write_old_config(config)

    result = CliRunner().invoke(cli, ["schema", "version", "--update", str(config)])
    assert result.exit_code == 0, result.output

    backups = list(tmp_path.glob("config.backup-*.yml"))
    assert len(backups) == 1
    assert _yaml.safe_load(backups[0].read_text(encoding="utf-8"))["schema_version"] == "0.1"
    assert _yaml.safe_load(config.read_text(encoding="utf-8"))["schema_version"] != "0.1"


def test_schema_version_update_no_backup(tmp_path) -> None:
    import yaml as _yaml
    from supy.cmd.suews_cli import cli

    config = tmp_path / "config.yml"
    _write_old_config(config)

    result = CliRunner().invoke(
        cli, ["schema", "version", "--update", "--no-backup", str(config)]
    )
    assert result.exit_code == 0, result.output

    assert list(tmp_path.glob("config.backup-*.yml")) == []
    assert _yaml.safe_load(config.read_text(encoding="utf-8"))["schema_version"] != "0.1"


@pytest.mark.parametrize("flag", [[], ["-b"], ["--no-backup"]])
def test_schema_migrate_leaves_input_unchanged(
    tmp_path: Path, flag: list[str], monkeypatch: pytest.MonkeyPatch
) -> None:
    """``migrate`` writes a migrated copy and leaves its input untouched."""
    import yaml as _yaml
    from supy.cmd import schema_cli
    from supy.cmd.suews_cli import cli
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION

    # ARRANGE: skip only the heavy post-write JSON Schema generation so the
    # real SchemaMigrator and file I/O execute in milliseconds.
    monkeypatch.setattr(
        schema_cli, "validate_file_against_schema", lambda *a, **kw: (True, [])
    )
    config = tmp_path / "config.yml"
    config.write_text("name: test\nschema_version: '2026.6.dev9'\n", encoding="utf-8")
    original = config.read_text(encoding="utf-8")

    # ACT
    result = CliRunner().invoke(cli, ["schema", "migrate", *flag, str(config)])

    # ASSERT
    assert result.exit_code == 0, result.output
    assert config.read_text(encoding="utf-8") == original
    migrated = tmp_path / "config.migrated.yml"
    assert migrated.exists()
    migrated_doc = _yaml.safe_load(migrated.read_text(encoding="utf-8"))
    assert migrated_doc["schema_version"] == CURRENT_SCHEMA_VERSION


def _stderr_or_output(result) -> str:
    """Return captured stderr when Click separates streams, else combined output."""
    try:
        return result.stderr
    except ValueError:  # Click < 8.2 without mix_stderr=False
        return result.output


@pytest.mark.parametrize("quiet_flag", [[], ["--quiet"]])
@pytest.mark.parametrize("subcmd", ["version", "migrate"])
def test_schema_subcommands_exit_nonzero_on_malformed_yaml(
    tmp_path: Path, subcmd: str, quiet_flag: list[str]
) -> None:
    from supy.cmd.suews_cli import cli

    # ARRANGE
    bad = tmp_path / "bad.yml"
    bad.write_text("name: [unterminated\n", encoding="utf-8")

    # ACT
    result = CliRunner().invoke(cli, ["schema", *quiet_flag, subcmd, str(bad)])

    # ASSERT
    assert result.exit_code == 1
    if quiet_flag:
        err = _stderr_or_output(result)
        assert "bad.yml:" in err
        assert "YAML" in err
    else:
        assert "bad.yml" in result.output
        assert "YAML" in result.output


@pytest.mark.parametrize("quiet_flag", [[], ["--quiet"]])
@pytest.mark.parametrize("subcmd", ["version", "migrate"])
@pytest.mark.parametrize("content", ["- item1\n- item2\n", ""])
def test_schema_subcommands_exit_nonzero_on_non_mapping_yaml(
    tmp_path: Path, subcmd: str, quiet_flag: list[str], content: str
) -> None:
    from supy.cmd.suews_cli import cli

    # ARRANGE
    non_mapping = tmp_path / "non_mapping.yml"
    non_mapping.write_text(content, encoding="utf-8")

    # ACT
    result = CliRunner().invoke(cli, ["schema", *quiet_flag, subcmd, str(non_mapping)])

    # ASSERT
    assert result.exit_code == 1
    if quiet_flag:
        err = _stderr_or_output(result)
        assert "non_mapping.yml:" in err
        assert "mapping" in err
    else:
        assert "mapping" in result.output


@pytest.mark.parametrize("quiet_flag", [[], ["--quiet"]])
@pytest.mark.parametrize("dry_run_flag", [[], ["--dry-run"]])
def test_schema_migrate_exits_nonzero_on_unsupported_migration_path(
    tmp_path: Path, quiet_flag: list[str], dry_run_flag: list[str]
) -> None:
    from supy.cmd.suews_cli import cli

    # ARRANGE
    config = tmp_path / "unsupported.yml"
    _write_old_config(config)

    # ACT
    result = CliRunner().invoke(
        cli, ["schema", *quiet_flag, "migrate", *dry_run_flag, str(config)]
    )

    # ASSERT
    assert result.exit_code == 1
    assert not (tmp_path / "unsupported.migrated.yml").exists()
    if quiet_flag:
        err = _stderr_or_output(result)
        assert "unsupported.yml:" in err
        assert "No migration path available" in err
    else:
        assert "No migration path available" in result.output


@pytest.mark.parametrize("quiet_flag", [[], ["--quiet"]])
def test_schema_migrate_dry_run_valid_config_writes_nothing(
    tmp_path: Path, quiet_flag: list[str]
) -> None:
    """``migrate --dry-run`` validates migration in memory without writing to disk."""
    from supy.cmd.suews_cli import cli

    # ARRANGE
    config = tmp_path / "config.yml"
    config.write_text("name: test\nschema_version: '2026.6.dev9'\n", encoding="utf-8")
    original = config.read_text(encoding="utf-8")
    out_dir = tmp_path / "should_not_be_created"

    # ACT
    result = CliRunner().invoke(
        cli,
        [
            "schema",
            *quiet_flag,
            "migrate",
            "--dry-run",
            "--output-dir",
            str(out_dir),
            str(config),
        ],
    )

    # ASSERT
    assert result.exit_code == 0, result.output
    assert config.read_text(encoding="utf-8") == original
    assert not out_dir.exists()
    assert not (tmp_path / "config.migrated.yml").exists()
    if quiet_flag:
        assert not result.output
    else:
        assert "Would migrate" in result.output


def test_schema_version_batch_quiet_reports_only_failing_file(tmp_path: Path) -> None:
    from supy.cmd.suews_cli import cli

    # ARRANGE
    good = tmp_path / "good.yml"
    good.write_text("name: good\nschema_version: '2026.6.dev9'\n", encoding="utf-8")
    listfile = tmp_path / "listfile.yml"
    listfile.write_text("- item1\n- item2\n", encoding="utf-8")

    # ACT
    result = CliRunner().invoke(
        cli, ["schema", "--quiet", "version", str(good), str(listfile)]
    )

    # ASSERT
    assert result.exit_code == 1
    err = _stderr_or_output(result)
    assert "listfile.yml:" in err
    assert "mapping" in err
    assert "good.yml" not in result.output


@pytest.mark.parametrize("quiet_flag", [[], ["--quiet"]])
@pytest.mark.parametrize("dry_run_flag", [[], ["--dry-run"]])
def test_schema_migrate_batch_exits_nonzero_when_any_file_fails(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    quiet_flag: list[str],
    dry_run_flag: list[str],
) -> None:
    from supy.cmd import schema_cli
    from supy.cmd.suews_cli import cli

    # ARRANGE
    monkeypatch.setattr(
        schema_cli, "validate_file_against_schema", lambda *a, **kw: (True, [])
    )
    good = tmp_path / "good.yml"
    good.write_text("name: good\nschema_version: '2026.6.dev9'\n", encoding="utf-8")
    bad = tmp_path / "bad.yml"
    _write_old_config(bad)

    # ACT
    result = CliRunner().invoke(
        cli, ["schema", *quiet_flag, "migrate", *dry_run_flag, str(good), str(bad)]
    )

    # ASSERT
    assert result.exit_code == 1
    if dry_run_flag:
        assert not (tmp_path / "good.migrated.yml").exists()
    else:
        assert (tmp_path / "good.migrated.yml").exists()
    assert not (tmp_path / "bad.migrated.yml").exists()
    if quiet_flag:
        err = _stderr_or_output(result)
        assert "bad.yml:" in err
        assert "good.yml" not in result.output


def test_schema_migrate_warns_once_for_older_target_version(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """CLI surfaces the older-target warning once on stderr without raw UserWarning spam."""
    from supy.cmd import schema_cli
    from supy.cmd.suews_cli import cli
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION

    # ARRANGE
    monkeypatch.setattr(
        schema_cli, "validate_file_against_schema", lambda *a, **kw: (True, [])
    )
    first = tmp_path / "site_a.yml"
    second = tmp_path / "site_b.yml"
    for path in (first, second):
        path.write_text("name: test\nschema_version: '2026.6.dev17'\n", encoding="utf-8")

    # ACT: multi-file migration to an older historical schema
    result = CliRunner().invoke(
        cli,
        [
            "schema",
            "migrate",
            "--target-version",
            "2026.6.dev18",
            str(first),
            str(second),
        ],
    )

    # ASSERT: warning appears once cleanly on stderr (no double-spam or raw traceback)
    assert result.exit_code == 0, result.output
    err = _stderr_or_output(result)
    assert err.count("older than the current installed schema") == 1
    assert "Warning:" in err
    assert "2026.6.dev18" in err
    assert CURRENT_SCHEMA_VERSION in err
    assert "UserWarning:" not in result.output

    # ACT: --dry-run also surfaces the older-target warning cleanly once
    dry_result = CliRunner().invoke(
        cli,
        [
            "schema",
            "migrate",
            "--dry-run",
            "--target-version",
            "2026.6.dev18",
            str(first),
        ],
    )
    assert dry_result.exit_code == 0, dry_result.output
    dry_err = _stderr_or_output(dry_result)
    assert dry_err.count("older than the current installed schema") == 1
    assert "UserWarning:" not in dry_result.output

    # ACT: --quiet suppresses non-error warning output without leaking raw UserWarning
    quiet_result = CliRunner().invoke(
        cli,
        [
            "schema",
            "--quiet",
            "migrate",
            "--dry-run",
            "--target-version",
            "2026.6.dev18",
            str(first),
        ],
    )
    assert quiet_result.exit_code == 0, quiet_result.output
    assert not quiet_result.output


@pytest.mark.parametrize("pass_current", [False, True])
def test_schema_migrate_no_older_target_warning_for_default_or_current(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, pass_current: bool
) -> None:
    """Omitting --target-version or passing CURRENT_SCHEMA_VERSION emits no historical warning."""
    from supy.cmd import schema_cli
    from supy.cmd.suews_cli import cli
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION

    # ARRANGE
    monkeypatch.setattr(
        schema_cli, "validate_file_against_schema", lambda *a, **kw: (True, [])
    )
    config = tmp_path / "site.yml"
    config.write_text("name: test\nschema_version: '2026.6.dev18'\n", encoding="utf-8")
    args = ["schema", "migrate"]
    if pass_current:
        args.extend(["--target-version", CURRENT_SCHEMA_VERSION])
    args.append(str(config))

    # ACT
    result = CliRunner().invoke(cli, args)

    # ASSERT
    assert result.exit_code == 0, result.output
    assert "older than the current installed schema" not in result.output
    assert "UserWarning:" not in result.output


# ---------------------------------------------------------------------------
# Back-compat aliases
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "alias_func_name, expected_legacy_name, expected_replacement",
    [
        ("run_alias", "suews-run", "suews run"),
        ("convert_alias", "suews-convert", "suews convert"),
        ("validate_alias", "suews-validate", "suews validate"),
        ("schema_alias", "suews-schema", "suews schema"),
    ],
)
def test_back_compat_alias_emits_deprecation(
    alias_func_name: str,
    expected_legacy_name: str,
    expected_replacement: str,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    from supy.cmd import suews_cli

    invoked = {"called": False}

    def fake_command(*args, **kwargs):
        invoked["called"] = True

    # Force the underlying Click command to be a no-op so we only test the shim.
    if alias_func_name == "run_alias":
        monkeypatch.setattr(suews_cli, "_run_cmd", fake_command)
    elif alias_func_name == "convert_alias":
        monkeypatch.setattr(suews_cli, "_convert_cmd", fake_command)
    elif alias_func_name == "validate_alias":
        monkeypatch.setattr(suews_cli, "_validate_cli", fake_command)
    elif alias_func_name == "schema_alias":
        monkeypatch.setattr(suews_cli, "_schema_cli", fake_command)
    else:  # pragma: no cover
        pytest.fail(f"unknown alias {alias_func_name!r}")

    monkeypatch.setattr(sys, "argv", [expected_legacy_name])

    alias = getattr(suews_cli, alias_func_name)
    alias()

    captured = capsys.readouterr()
    assert "DEPRECATED" in captured.err
    assert expected_legacy_name in captured.err
    assert expected_replacement in captured.err
    assert invoked["called"], "alias did not forward to underlying command"


def test_convert_dispatcher_matches_legacy(tmp_path) -> None:
    """``suews convert`` and the legacy ``suews-convert`` produce equivalent YAML.

    Acceptance criterion (gh#1362): the dispatcher's ``convert`` route must
    match ``suews-convert`` on the bundled fixtures. The strict identity
    ``cli.commands["convert"] is convert_table_cmd`` (above) already proves
    both routes invoke the same Click command; this integration test runs
    each route end-to-end on a real fixture and asserts the produced YAML
    is structurally equivalent. (One field, ``model.control.output.dir``,
    embeds the converter's per-invocation temp directory, so a byte-for-byte
    comparison is impossible across separate runs -- we strip it before
    diffing.)
    """
    import yaml as _yaml
    from supy.cmd.suews_cli import cli
    from supy.cmd.table_converter import convert_table_cmd

    fixture_nml = (
        Path(__file__).resolve().parents[1]
        / "fixtures"
        / "data_test"
        / "AVL_1_LDN1"
        / "RunControl.nml"
    )
    if not fixture_nml.exists():
        pytest.skip(f"Fixture not available at {fixture_nml}")

    out_dispatcher = tmp_path / "via_dispatcher.yml"
    out_legacy = tmp_path / "via_legacy.yml"

    runner = CliRunner()
    result_dispatcher = runner.invoke(
        cli,
        ["convert", "-i", str(fixture_nml), "-o", str(out_dispatcher)],
    )
    assert result_dispatcher.exit_code == 0, result_dispatcher.output

    result_legacy = runner.invoke(
        convert_table_cmd,
        ["-i", str(fixture_nml), "-o", str(out_legacy)],
    )
    assert result_legacy.exit_code == 0, result_legacy.output

    def _strip_nondeterministic(path):
        doc = _yaml.safe_load(path.read_text(encoding="utf-8"))
        # Per-invocation temp path leaks into output.dir; null it out
        # before comparing so the rest of the dict can be diffed cleanly.
        try:
            doc["model"]["control"]["output"]["dir"] = None
        except (KeyError, TypeError):
            pass
        return doc

    assert _strip_nondeterministic(out_dispatcher) == _strip_nondeterministic(
        out_legacy
    ), (
        "suews convert (dispatcher) and legacy suews-convert produced "
        "structurally different YAML on the AVL_1_LDN1 fixture."
    )


def test_rust_bridge_main_accepts_explicit_argv(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from supy.cmd import rust_bridge

    captured: dict[str, object] = {}

    class Result:
        returncode = 7

    def fake_run(cmd, check):
        captured["cmd"] = cmd
        captured["check"] = check
        return Result()

    fake_binary = Path("/tmp/suews-engine")
    monkeypatch.setattr(rust_bridge, "_bridge_binary", lambda: fake_binary)
    monkeypatch.setattr(rust_bridge.subprocess, "run", fake_run)

    with pytest.raises(SystemExit) as excinfo:
        rust_bridge.main(argv=["--version"])

    assert excinfo.value.code == 7
    # Compare against the platform-appropriate string form so Windows (where
    # ``str(Path("/tmp/suews-engine"))`` becomes ``\\tmp\\suews-engine``) and
    # POSIX both pass; ``rust_bridge.main`` stringifies the Path before exec.
    assert captured["cmd"] == [str(fake_binary), "--version"]
    assert captured["check"] is False


# ---------------------------------------------------------------------------
# Onboarding consistency
#
# What the README, the ``suews run`` help text and the setup messages teach
# must match the canonical ``suews <subcmd>`` interface and the Python floor
# declared in ``pyproject.toml``. These guard the drift that let the README
# and ``suews run --help`` keep recommending the deprecated hyphenated aliases.
# ---------------------------------------------------------------------------

_REPO_ROOT = Path(__file__).resolve().parents[2]
_LEGACY_ALIAS_PATTERN = re.compile(r"\bsuews-(run|validate|convert|schema|inspect)\b")
_PYTHON_FLOOR_CLAIM_PATTERN = re.compile(
    r"Python (\d+)\.(\d+)\s*(?:\+|or newer|and newer|or later)"
    r"|version_info < \((\d+), (\d+)\)"
)
_ONBOARDING_FILES = (
    "README.md",
    "Makefile",
    "docs/README.md",
    "dev-ref/building-locally.md",
    "dev-ref/onboarding-guide.md",
)


def _read_repo_file(relative_path: str) -> str:
    path = _REPO_ROOT / relative_path
    if not path.is_file():
        pytest.skip(f"{relative_path} not present; not running from a source checkout")
    return path.read_text(encoding="utf-8")


def _required_python_floor() -> tuple[int, int]:
    import tomllib

    project = tomllib.loads(_read_repo_file("pyproject.toml"))["project"]
    spec = project["requires-python"].strip()
    match = re.fullmatch(r">=\s*(\d+)\.(\d+)", spec)
    assert match, f"unexpected requires-python spec: {spec!r}"
    return int(match.group(1)), int(match.group(2))


def test_run_help_teaches_canonical_commands() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["run", "--help"])
    assert result.exit_code == 0, result.output
    assert "suews run config.yml" in result.output
    assert "suews convert -i RunControl.nml" in result.output
    assert not _LEGACY_ALIAS_PATTERN.search(result.output), result.output


def test_readme_teaches_only_resolvable_subcommands() -> None:
    from supy.cmd.suews_cli import cli

    readme = _read_repo_file("README.md")
    legacy = sorted(set(_LEGACY_ALIAS_PATTERN.findall(readme)))
    assert not legacy, f"README still teaches deprecated aliases: {legacy}"
    taught = set(re.findall(r"\bsuews ([a-z]+)\b", readme))
    assert taught, "README no longer documents any suews subcommand"
    missing = sorted(taught - set(cli.commands))
    assert not missing, f"README teaches unknown subcommands: {missing}"


@pytest.mark.parametrize("relative_path", _ONBOARDING_FILES)
def test_onboarding_files_do_not_understate_python_floor(relative_path: str) -> None:
    floor = _required_python_floor()
    text = _read_repo_file(relative_path)
    claims = [
        (int(major or check_major), int(minor or check_minor))
        for major, minor, check_major, check_minor in _PYTHON_FLOOR_CLAIM_PATTERN.findall(
            text
        )
    ]
    understated = sorted({claim for claim in claims if claim < floor})
    assert not understated, (
        f"{relative_path} claims a Python floor below requires-python "
        f"{floor[0]}.{floor[1]}: {understated}"
    )


def test_run_explains_null_forcing_file(tmp_path: Path) -> None:
    """``suews run`` on a ``forcing.file: null`` YAML points at the Python API."""
    from supy._env import trv_supy_module
    from supy.cmd.suews_cli import cli

    sample = trv_supy_module / "sample_data" / "sample_config.yml"
    entry = "      file:\n        value: Kc_2012_data_60.txt\n"
    text = sample.read_text(encoding="utf-8")
    assert text.count(entry) == 1, "fixture forcing entry moved"
    config_path = tmp_path / "config.yml"
    config_path.write_text(text.replace(entry, "      file: null\n"), encoding="utf-8")

    result = CliRunner().invoke(cli, ["run", str(config_path)])

    assert result.exit_code == 1, result.output
    assert "'forcing.file' is null" in result.output
    assert "update_forcing()" in result.output


@pytest.mark.parametrize("entry", ["command", "entrypoint"])
@pytest.mark.parametrize("spelling", ["canonical", "alias", "invalid-both"])
def test_run_site_longitude_source_boundary(
    tmp_path: Path, monkeypatch, capsys, entry, spelling
) -> None:
    """Exercise normal run loading; stop only at simulation and saving.

    The canonical input reaches the real resolved engine export. An unknown
    site longitude key must fail even when a valid canonical key is present.
    Native numerical execution is covered separately, not repeated here.
    """
    import yaml
    from supy._env import trv_supy_module
    from supy._run_rust import config_to_engine_yaml
    from supy.cmd.suews_cli import cli, main
    from supy.suews_sim import SUEWSSimulation

    sample_dir = trv_supy_module / "sample_data"
    forcing = tmp_path / "Kc_2012_data_60.txt"
    forcing.write_bytes((sample_dir / forcing.name).read_bytes())
    data = yaml.safe_load((sample_dir / "sample_config.yml").read_text(encoding="utf-8"))
    props = data["sites"][0]["properties"]
    props["lng"] = {"value": 0.0}
    if spelling == "alias":
        props["lon"] = props.pop("lng")
    elif spelling == "invalid-both":
        props["lon"] = {"value": 0.0}
    path = tmp_path / "config.yml"
    path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    original = path.read_bytes()
    original_forcing = forcing.read_bytes()
    engine_inputs = []

    def capture_engine_input(simulation):
        engine_inputs.append(yaml.safe_load(config_to_engine_yaml(simulation.config)))

    monkeypatch.setattr(SUEWSSimulation, "run", capture_engine_input)
    monkeypatch.setattr(SUEWSSimulation, "save", lambda self, **kwargs: [])
    if entry == "command":
        result = CliRunner().invoke(cli, ["run", str(path)])
        exit_code = result.exit_code
        diagnostics = result.output
    else:
        # pyproject.toml registers suews = supy.cmd.suews_cli:main.
        monkeypatch.setattr(sys, "argv", ["suews", "run", str(path)])
        with pytest.raises(SystemExit) as exc:
            main()
        exit_code = exc.value.code
        captured = capsys.readouterr()
        diagnostics = captured.out + captured.err

    assert path.read_bytes() == original
    assert forcing.read_bytes() == original_forcing
    if spelling in {"alias", "invalid-both"}:
        assert exit_code == 1, diagnostics
        assert "lon" in diagnostics
        assert "extra" in diagnostics.lower()
        assert engine_inputs == []
    else:
        assert exit_code == 0, diagnostics
        assert len(engine_inputs) == 1
        resolved = engine_inputs[0]["sites"][0]["properties"]
        assert "lon" not in resolved
        longitude = resolved["lng"]
        assert (longitude["value"] if isinstance(longitude, dict) else longitude) == 0.0


def _assert_waterdist_schema_rules(schema_dict: dict) -> None:
    wd = schema_dict["$defs"]["WaterDistribution"]
    assert "sfr > 0" in wd["description"]
    assert "sfr == 0" in wd["description"]
    assert "0 normalises to absent at load" in wd["description"]
    assert "non-zero or post-load edits fail" in wd["description"]

    for field_name in ("to_runoff", "to_soilstore", "to_paved"):
        desc = wd["properties"][field_name]["description"]
        assert "sfr > 0" in desc
        assert "0 normalises to absent at load" in desc
        assert "non-zero or post-load edits fail" in desc
        # Keep descriptions within MCP search_schema's 200-char display budget.
        assert len(desc) <= 200, (field_name, len(desc))


def _write_waterdist_cli_cases(tmp_path: Path) -> tuple[Path, Path, Path]:
    sample_raw = yaml.safe_load(
        (files("supy") / "sample_data" / "sample_config.yml").read_text(
            encoding="utf-8"
        )
    )
    props = sample_raw["sites"][0]["properties"]
    props.pop("stebbs", None)
    props.pop("building_archetype", None)

    raw_zero = deepcopy(sample_raw)
    raw_zero["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"][
        "to_soilstore"
    ] = {"value": 0.0}
    path_zero = tmp_path / "paved_zero_soilstore.yml"
    path_zero.write_text(yaml.safe_dump(raw_zero), encoding="utf-8")

    raw_inactive = deepcopy(sample_raw)
    del raw_inactive["sites"][0]["properties"]["land_cover"]["evetr"]["waterdist"][
        "to_soilstore"
    ]
    path_inactive = tmp_path / "inactive_evetr_omitted.yml"
    path_inactive.write_text(yaml.safe_dump(raw_inactive), encoding="utf-8")

    raw_nonzero = deepcopy(sample_raw)
    raw_nonzero["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"][
        "to_soilstore"
    ] = {"value": 0.5}
    path_nonzero = tmp_path / "paved_nonzero_soilstore.yml"
    path_nonzero.write_text(yaml.safe_dump(raw_nonzero), encoding="utf-8")
    return path_zero, path_inactive, path_nonzero


def test_schema_export_cli_displays_waterdistribution_rules(tmp_path: Path) -> None:
    """``suews schema export`` exposes the synced WaterDistribution rules."""
    runner = CliRunner()
    out_json = tmp_path / "exported_schema.json"

    export_res = runner.invoke(
        suews_cli_root, ["schema", "export", "-o", str(out_json)]
    )

    assert export_res.exit_code == 0, export_res.output
    exported = json.loads(out_json.read_text(encoding="utf-8"))
    _assert_waterdist_schema_rules(exported)


def test_schema_envelope_cli_displays_waterdistribution_rules() -> None:
    """``suews schema --format json`` exposes the synced WaterDistribution rules."""
    runner = CliRunner()

    envelope_res = runner.invoke(suews_cli_root, ["schema", "--format", "json"])

    assert envelope_res.exit_code == 0, envelope_res.output
    envelope = json.loads(envelope_res.output)
    assert envelope["status"] == "success"
    _assert_waterdist_schema_rules(envelope["data"])


def test_inspect_cli_waterdist_rules(tmp_path: Path) -> None:
    """``suews inspect --format json`` enforces load-time waterdist rules."""
    path_zero, path_inactive, path_nonzero = _write_waterdist_cli_cases(tmp_path)
    runner = CliRunner()

    for ok_path in (path_zero, path_inactive):
        inspect_ok = runner.invoke(
            suews_cli_root, ["inspect", "--format", "json", str(ok_path)]
        )
        assert inspect_ok.exit_code == 0, inspect_ok.output
        assert json.loads(inspect_ok.output)["status"] == "success"

    inspect_bad = runner.invoke(
        suews_cli_root, ["inspect", "--format", "json", str(path_nonzero)]
    )
    assert inspect_bad.exit_code != 0, inspect_bad.output
    bad_env = json.loads(inspect_bad.output)
    assert bad_env["status"] == "error"
    assert any("to_soilstore" in str(err) for err in bad_env["errors"]), bad_env


def test_validate_cli_waterdist_rules(tmp_path: Path) -> None:
    """``suews validate -p C --dry-run --format json`` enforces load-time waterdist rules."""
    path_zero, path_inactive, path_nonzero = _write_waterdist_cli_cases(tmp_path)
    runner = CliRunner()

    val_res = runner.invoke(
        suews_cli_root,
        [
            "validate",
            "-p",
            "C",
            "--dry-run",
            "--format",
            "json",
            str(path_zero),
            str(path_inactive),
            str(path_nonzero),
        ],
    )
    assert val_res.exit_code == 1, val_res.output
    val_env = json.loads(val_res.output)
    assert val_env["status"] == "error"
    files_by_name = {
        Path(entry["file"]).name: entry for entry in val_env["data"]["files"]
    }
    assert files_by_name["paved_zero_soilstore.yml"]["valid"] is True
    assert files_by_name["inactive_evetr_omitted.yml"]["valid"] is True
    assert files_by_name["paved_nonzero_soilstore.yml"]["valid"] is False
    assert any(
        "to_soilstore" in str(err.get("message", "")) for err in val_env["errors"]
    ), val_env["errors"]


# ---------------------------------------------------------------------------
# CLI documentation & show_default contracts (gh#2165)
# ---------------------------------------------------------------------------


def _walk_click_tree(command, path: tuple[str, ...] = ()):
    """Yield ``(path, command)`` for every registered Click command and group."""
    yield path, command
    if isinstance(command, click.Group):
        with click.Context(command) as ctx:
            for name in command.list_commands(ctx):
                sub = command.get_command(ctx, name)
                if sub is not None:
                    yield from _walk_click_tree(sub, (*path, name))


def test_registered_command_names_match_click_name() -> None:
    """Every registered command exposes its CLI name so sphinx-click renders true headings."""
    assert suews_cli_root.name == "suews"
    mismatches = [
        (path, cmd.name)
        for path, cmd in _walk_click_tree(suews_cli_root)
        if path and cmd.name != path[-1]
    ]
    assert not mismatches, f"Command .name differs from registered CLI name: {mismatches}"


def test_cli_options_use_show_default_instead_of_handwritten_default_prose() -> None:
    """Non-flag options with defaults enable Click show_default and avoid '(default: ...)' prose."""
    unset = getattr(click.core, "UNSET", None)
    handwritten_default = re.compile(r"\(\s*default\s*:", re.IGNORECASE)
    missing_show_default: list[str] = []
    duplicated_prose: list[str] = []

    for path, cmd in _walk_click_tree(suews_cli_root):
        cmd_label = " ".join(("suews", *path))
        cmd_show_default = bool((cmd.context_settings or {}).get("show_default"))
        for param in cmd.params:
            if not isinstance(param, click.Option) or param.hidden:
                continue
            help_text = param.help or ""
            if handwritten_default.search(help_text):
                duplicated_prose.append(f"{cmd_label} {param.opts[0]}: {help_text!r}")
            has_default = param.default is not None and param.default is not unset
            if not param.is_flag and has_default:
                effective_show = (
                    param.show_default
                    if param.show_default is not None
                    else cmd_show_default
                )
                if not effective_show:
                    missing_show_default.append(
                        f"{cmd_label} {param.opts[0]} (default={param.default!r})"
                    )

    assert not duplicated_prose, f"Hand-written default prose in help=: {duplicated_prose}"
    assert not missing_show_default, (
        f"Options with defaults missing show_default: {missing_show_default}"
    )


def test_validate_help_renders_click_defaults_from_code() -> None:
    """``suews validate --help`` renders defaults via Click ``show_default``."""
    runner = CliRunner()
    result = runner.invoke(suews_cli_root, ["validate", "--help"], terminal_width=240)
    assert result.exit_code == 0, result.output
    assert "(default: on)" not in result.output
    for expected in (
        "[default: ABC]",
        "[default: public]",
        "[default: table]",
        "[default: on]",
        "[default: suggest]",
    ):
        assert expected in result.output, result.output


def test_command_line_rst_uses_sphinx_click() -> None:
    """``docs/source/api/command-line.rst`` generates CLI reference via ``sphinx_click.ext``."""
    text = _read_repo_file("docs/source/api/command-line.rst")
    assert text.startswith(":doc-source: hand-written\n")
    assert ".. click:: supy.cmd.suews_cli:cli" in text
    assert ":prog: suews" in text
    assert ":nested: full" in text
