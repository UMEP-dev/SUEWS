"""Unit tests for the ``suews://knowledge/...`` resource family."""

from __future__ import annotations

import json
import subprocess

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
            "meta": {"command": " ".join(cmd)},
        }
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    return fake_run


def test_manifest_resource_passes_through(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.resources import read_knowledge_manifest_resource

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(
            captured,
            {"manifest": {"git_sha": "cafe", "pack_version": "1"}},
        ),
    )

    result = read_knowledge_manifest_resource()
    assert result["status"] == "success"
    assert result["data"]["manifest"]["git_sha"] == "cafe"
    assert captured["cmd"][:3] == ["suews", "knowledge", "manifest"]


def test_query_resource_passes_through(monkeypatch: pytest.MonkeyPatch) -> None:
    from suews_mcp.resources import read_knowledge_query_resource

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": [], "manifest": {}}),
    )

    result = read_knowledge_query_resource("OHM", limit=2)
    assert result["status"] == "success"
    assert "OHM" in captured["cmd"]
    assert "--limit" in captured["cmd"]
    assert "2" in captured["cmd"]
