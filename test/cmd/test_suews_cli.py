"""Tests for the unified ``suews`` CLI dispatcher.

Validates:
- `suews --help` lists the public subcommands
  (validate/schema/convert/init/run/knowledge).
- Each subcommand resolves to its existing Click implementation.
- The legacy hyphenated entry points (`suews-run`, `-convert`, `-validate`,
  `-schema`) print a deprecation notice and forward to the underlying command.
"""

from __future__ import annotations

from pathlib import Path
import re
import sys

from click.testing import CliRunner
import pytest

pytestmark = pytest.mark.api


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
