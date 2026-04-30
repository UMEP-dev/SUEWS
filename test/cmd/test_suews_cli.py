"""Tests for the unified ``suews`` CLI dispatcher.

Validates:
- `suews --help` lists the 5 expected subcommands (validate/schema/convert/run/rust).
- Each subcommand resolves to its existing Click implementation.
- The legacy hyphenated entry points (`suews-run`, `-convert`, `-validate`,
  `-schema`) print a deprecation notice and forward to the underlying command.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


# ---------------------------------------------------------------------------
# Top-level group
# ---------------------------------------------------------------------------


def test_top_level_help_lists_subcommands() -> None:
    from supy.cmd.suews_cli import cli

    runner = CliRunner()
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0, result.output
    for sub in ("validate", "schema", "convert", "run", "rust"):
        assert sub in result.output, f"subcommand {sub!r} missing from help output"


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


def test_rust_subcommand_registered() -> None:
    from supy.cmd.suews_cli import cli

    assert "rust" in cli.commands


# ---------------------------------------------------------------------------
# Subcommand registry preserves the underlying Click commands
# ---------------------------------------------------------------------------


def test_validate_subcommand_is_validate_cli() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.validate_config import cli as validate_cli

    assert cli.commands["validate"] is validate_cli


def test_schema_subcommand_is_schema_cli() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.schema_cli import cli as schema_cli

    assert cli.commands["schema"] is schema_cli


def test_convert_subcommand_is_convert_cmd() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.table_converter import convert_table_cmd

    assert cli.commands["convert"] is convert_table_cmd


def test_run_subcommand_is_suews_cmd() -> None:
    from supy.cmd.suews_cli import cli
    from supy.cmd.SUEWS import SUEWS as suews_run

    assert cli.commands["run"] is suews_run


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


# ---------------------------------------------------------------------------
# Rust subcommand passthrough
# ---------------------------------------------------------------------------


def test_rust_subcommand_passes_argv_through(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from supy.cmd import rust_bridge, suews_cli

    captured: dict[str, list[str] | None] = {"argv": None}

    def fake_rust_main(argv: list[str] | None = None) -> None:
        captured["argv"] = argv

    monkeypatch.setattr(rust_bridge, "main", fake_rust_main)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli.cli,
        ["rust", "--input", "config.yml", "--verbose"],
    )
    assert result.exit_code == 0, result.output
    assert captured["argv"] == ["--input", "config.yml", "--verbose"]


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
