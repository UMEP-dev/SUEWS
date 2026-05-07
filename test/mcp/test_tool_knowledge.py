"""Unit tests for ``query_knowledge`` and ``read_knowledge_manifest``."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def _stub_envelope(captured: dict, payload: dict):
    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        envelope = {
            "status": "success",
            "data": payload,
            "errors": [],
            "warnings": [],
            "meta": {"command": " ".join(cmd), "git_commit": "abc1234"},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    return fake_run


def test_query_knowledge_passes_question_and_limit(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from suews_mcp.tools import query_knowledge

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": [], "manifest": {}}),
    )

    query_knowledge("STEBBS", limit=3)
    assert captured["cmd"][:3] == ["suews", "knowledge", "query"]
    assert "STEBBS" in captured["cmd"]
    assert "--limit" in captured["cmd"]
    assert "3" in captured["cmd"]


def test_query_knowledge_forwards_scope(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from suews_mcp.tools import query_knowledge

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": [], "manifest": {}}),
    )

    query_knowledge("OHM", scope="fortran")
    assert "--scope" in captured["cmd"]
    assert "fortran" in captured["cmd"]


def test_query_knowledge_sandbox_violation_returns_error(tmp_path: Path) -> None:
    from suews_mcp.tools import query_knowledge

    (tmp_path / "case").mkdir()
    result = query_knowledge(
        "anything",
        project_root=str(tmp_path / "case"),
        pack="/etc/pack",
    )
    assert result["status"] == "error"


def test_query_knowledge_rejects_empty_question() -> None:
    from suews_mcp.tools import query_knowledge

    result = query_knowledge("   ")
    assert result["status"] == "error"


def test_read_knowledge_manifest_calls_subcommand(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from suews_mcp.tools import read_knowledge_manifest

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(
            captured,
            {
                "manifest": {
                    "git_sha": "deadbeef",
                    "schema_version": "2026.5",
                    "pack_version": "1",
                }
            },
        ),
    )

    result = read_knowledge_manifest()
    assert result["status"] == "success"
    assert captured["cmd"][:3] == ["suews", "knowledge", "manifest"]
    assert result["data"]["manifest"]["git_sha"] == "deadbeef"


def test_read_knowledge_manifest_with_pack(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import read_knowledge_manifest

    pack_dir = tmp_path / "pack"
    pack_dir.mkdir()

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"manifest": {}}),
    )

    read_knowledge_manifest(project_root=str(tmp_path), pack=str(pack_dir))
    assert "--pack" in captured["cmd"]
    assert str(pack_dir) in captured["cmd"]
