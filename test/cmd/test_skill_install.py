"""Tests for ``suews skill install``."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def test_install_local_copies_skill(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    from supy.cmd.suews_cli import cli

    monkeypatch.chdir(tmp_path)
    runner = CliRunner()
    result = runner.invoke(cli, ["skill", "install", "--format", "json"])

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"
    assert envelope["data"]["method"] == "copy"

    target = tmp_path / ".claude" / "skills" / "suews"
    assert target.exists()
    assert (target / "SKILL.md").exists()
    assert (target / "references" / "common-errors.md").exists()


def test_install_refuses_to_overwrite(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    from supy.cmd.suews_cli import cli

    monkeypatch.chdir(tmp_path)
    runner = CliRunner()

    first = runner.invoke(cli, ["skill", "install", "--format", "json"])
    assert first.exit_code == 0, first.output

    second = runner.invoke(cli, ["skill", "install", "--format", "json"])
    assert second.exit_code != 0
    envelope = json.loads(second.stdout)
    assert envelope["status"] == "error"
    assert any("already installed" in str(e).lower() for e in envelope["errors"])


def test_install_force_overwrite(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    from supy.cmd.suews_cli import cli

    monkeypatch.chdir(tmp_path)
    runner = CliRunner()

    first = runner.invoke(cli, ["skill", "install", "--format", "json"])
    assert first.exit_code == 0
    second = runner.invoke(cli, ["skill", "install", "--force", "--format", "json"])
    assert second.exit_code == 0


def test_install_symlink(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    from supy.cmd.suews_cli import cli

    monkeypatch.chdir(tmp_path)
    runner = CliRunner()
    result = runner.invoke(
        cli, ["skill", "install", "--symlink", "--format", "json"]
    )
    assert result.exit_code == 0, result.output
    target = tmp_path / ".claude" / "skills" / "suews"
    assert target.is_symlink()
