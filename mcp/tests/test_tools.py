from __future__ import annotations

import json
import subprocess

import pytest
from suews_mcp import tools

pytestmark = pytest.mark.api


def test_resolve_under_root_accepts_relative_path(tmp_path):
    root = tmp_path / "case"
    root.mkdir()
    config = root / "config.yml"
    config.write_text("name: test\n", encoding="utf-8")

    resolved = tools.resolve_under_root(root, "config.yml")

    assert resolved == config.resolve()


def test_resolve_under_root_rejects_symlink_escape(tmp_path):
    root = tmp_path / "case"
    root.mkdir()
    outside = tmp_path / "outside.yml"
    outside.write_text("name: outside\n", encoding="utf-8")
    link = root / "link.yml"
    link.symlink_to(outside)

    with pytest.raises(tools.PathOutsideRootError):
        tools.resolve_under_root(root, "link.yml")


def test_resolve_under_root_rejects_missing_parent_escape(tmp_path):
    root = tmp_path / "case"
    root.mkdir()

    with pytest.raises(tools.PathOutsideRootError):
        tools.resolve_under_root(root, "../outside.yml")


def test_ensure_command_available_rejects_non_allowlisted_command():
    with pytest.raises(tools.CommandNotAllowedError):
        tools.ensure_command_available("python")


def test_run_allowlisted_command_uses_argument_array_without_shell(tmp_path, monkeypatch):
    captured = {}

    def fake_run(cmd, **kwargs):
        captured["cmd"] = cmd
        captured["kwargs"] = kwargs
        return subprocess.CompletedProcess(cmd, 0, "{}", "")

    monkeypatch.setattr(tools, "ensure_command_available", lambda command: command)
    monkeypatch.setattr(subprocess, "run", fake_run)

    proc = tools.run_allowlisted_command(
        "suews-validate",
        ["validate", "config.yml"],
        cwd=tmp_path,
        timeout_s=10,
    )

    assert proc.returncode == 0
    assert captured["cmd"] == ["suews-validate", "validate", "config.yml"]
    assert "shell" not in captured["kwargs"]
    assert captured["kwargs"]["check"] is False


def test_validate_config_success(tmp_path, monkeypatch):
    config = tmp_path / "config.yml"
    config.write_text("name: test\n", encoding="utf-8")
    payload = {"status": "success", "summary": {"total_errors": 0}}

    def fake_run(command, args, *, cwd, timeout_s):
        assert command == "suews-validate"
        assert cwd == tmp_path.resolve()
        assert timeout_s == 30
        assert args == ["--dry-run", "--format", "json", str(config.resolve())]
        return subprocess.CompletedProcess(args, 0, json.dumps(payload), "")

    monkeypatch.setattr(tools, "run_allowlisted_command", fake_run)

    result = tools.validate_config(tmp_path, "config.yml")

    assert result["status"] == "success"
    assert result["isError"] is False
    assert result["result"] == payload


def test_validate_config_failure_is_structured_tool_error(tmp_path, monkeypatch):
    config = tmp_path / "config.yml"
    config.write_text("name: test\n", encoding="utf-8")
    payload = {"status": "failure", "summary": {"total_errors": 1}}

    def fake_run(command, args, *, cwd, timeout_s):
        return subprocess.CompletedProcess(args, 1, json.dumps(payload), "bad config")

    monkeypatch.setattr(tools, "run_allowlisted_command", fake_run)

    result = tools.validate_config(
        tmp_path,
        str(config),
        schema_version="2026.5",
        timeout_s=999,
    )

    assert result["status"] == "error"
    assert result["isError"] is True
    assert result["schema_version"] == "2026.5"
    assert result["command"]["timeout_s"] == 120
    assert result["stderr"] == "bad config"
    assert "--schema-version" in result["command"]["args"]


def test_validate_config_timeout(tmp_path, monkeypatch):
    config = tmp_path / "config.yml"
    config.write_text("name: test\n", encoding="utf-8")

    def fake_run(command, args, *, cwd, timeout_s):
        raise subprocess.TimeoutExpired(args, timeout_s, output="partial", stderr="err")

    monkeypatch.setattr(tools, "run_allowlisted_command", fake_run)

    result = tools.validate_config(tmp_path, "config.yml", timeout_s=1)

    assert result["status"] == "error"
    assert result["isError"] is True
    assert result["error_code"] == "TIMEOUT"
    assert result["stdout"] == "partial"
    assert result["stderr"] == "err"


def test_validate_config_invalid_json(tmp_path, monkeypatch):
    config = tmp_path / "config.yml"
    config.write_text("name: test\n", encoding="utf-8")

    def fake_run(command, args, *, cwd, timeout_s):
        return subprocess.CompletedProcess(args, 0, "not json", "")

    monkeypatch.setattr(tools, "run_allowlisted_command", fake_run)

    result = tools.validate_config(tmp_path, "config.yml")

    assert result["status"] == "error"
    assert result["isError"] is True
    assert result["error_code"] == "INVALID_JSON"
