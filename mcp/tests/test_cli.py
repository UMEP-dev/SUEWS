from __future__ import annotations

from pathlib import Path

import pytest
from suews_mcp import cli
import tomllib

pytestmark = pytest.mark.api


def test_main_requires_root(capsys):
    code = cli.main([])

    captured = capsys.readouterr()

    assert code == 2
    assert captured.err == "suews-mcp: --root is required\n"


def test_missing_mcp_dependency_returns_usage(tmp_path, capsys, monkeypatch):
    monkeypatch.setattr(
        cli,
        "_dependency_available",
        lambda name: name != "mcp",
    )

    code = cli.main(["--root", str(tmp_path)])

    captured = capsys.readouterr()

    assert code == 2
    assert "missing dependency `mcp`" in captured.err
    assert captured.err.count("\n") == 1


def test_missing_executables_returns_usage(tmp_path, capsys, monkeypatch):
    monkeypatch.setattr(cli, "_dependency_available", lambda name: True)
    monkeypatch.setattr(cli, "_missing_executables", lambda: ["suews-validate"])

    code = cli.main(["--root", str(tmp_path)])

    captured = capsys.readouterr()

    assert code == 2
    assert captured.err == (
        "suews-mcp: missing SUEWS CLI executable(s): suews-validate\n"
    )


def test_pyproject_registers_entrypoint():
    package_root = Path(__file__).resolve().parents[1]
    data = tomllib.loads((package_root / "pyproject.toml").read_text())

    assert data["project"]["scripts"]["suews-mcp"] == "suews_mcp.cli:main"
    assert data["project"]["requires-python"] == ">=3.10"
    assert "mcp[cli]>=1.27,<2" in data["project"]["dependencies"]
