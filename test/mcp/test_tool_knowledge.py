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
    assert captured["cmd"][0].endswith("suews")
    assert captured["cmd"][1:3] == ["knowledge", "query"]
    assert "STEBBS" in captured["cmd"]
    assert "--limit" in captured["cmd"]
    assert "3" in captured["cmd"]


def test_query_knowledge_default_limit_is_three(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Default ``limit`` lowered from 5 to 3 so the bounded snippet
    response stays well under any host's per-tool token cap (gh#1403).
    """
    from suews_mcp.tools import query_knowledge

    captured: dict = {}
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": [], "manifest": {}}),
    )

    query_knowledge("anything")
    # --limit value follows the flag in the argv list.
    limit_index = captured["cmd"].index("--limit")
    assert captured["cmd"][limit_index + 1] == "3"


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
    assert captured["cmd"][0].endswith("suews")
    assert captured["cmd"][1:3] == ["knowledge", "manifest"]
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


# ---------------------------------------------------------------------------
# Envelope size policy — gh#1403
# ---------------------------------------------------------------------------


def _bulky_match(text_size: int, idx: int = 0) -> dict:
    return {
        "id": f"chunk-{idx}",
        "content_type": "fortran",
        "git_sha": "deadbeef",
        "github_url": f"https://example/blob/deadbeef/file.f95#L1-L100",
        "repo_path": "src/file.f95",
        "line_start": 1,
        "line_end": 100,
        "score": 10,
        "text": "X" * text_size,
    }


def test_query_knowledge_default_mode_caps_match_text(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Default ``mode="snippet"`` caps each match's ``text`` so a single
    bulky chunk does not blow the agent's token budget (gh#1403).
    """
    from suews_mcp.tools import query_knowledge
    from suews_mcp.tools.knowledge import _SNIPPET_BYTES_CAP

    captured: dict = {}
    bulky = [_bulky_match(text_size=10_000, idx=i) for i in range(2)]
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": bulky, "manifest": {}}),
    )

    result = query_knowledge("anything")
    assert result["status"] == "success"
    assert result["data"]["mode"] == "snippet"
    for match in result["data"]["matches"]:
        assert len(match["text"].encode("utf-8")) <= _SNIPPET_BYTES_CAP
        assert match["text_truncated"] is True
        assert match["text_full_bytes"] == 10_000


def test_query_knowledge_summary_mode_drops_text(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """``mode="summary"`` drops the chunk text entirely while preserving
    citation metadata so the agent can still link to the source
    (gh#1403).
    """
    from suews_mcp.tools import query_knowledge

    captured: dict = {}
    bulky = [_bulky_match(text_size=10_000, idx=i) for i in range(2)]
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": bulky, "manifest": {}}),
    )

    result = query_knowledge("anything", mode="summary")
    assert result["status"] == "success"
    assert result["data"]["mode"] == "summary"
    for match in result["data"]["matches"]:
        assert match["text"] is None
        assert match["text_truncated"] is True
        # Citation fields preserved.
        for key in ("git_sha", "github_url", "repo_path", "line_start", "line_end"):
            assert key in match


def test_query_knowledge_full_mode_passes_through(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """``mode="full"`` is the explicit opt-in for the original
    unbounded envelope; no trimming applied (gh#1403).
    """
    from suews_mcp.tools import query_knowledge

    captured: dict = {}
    bulky = [_bulky_match(text_size=10_000, idx=0)]
    monkeypatch.setattr(
        subprocess,
        "run",
        _stub_envelope(captured, {"matches": bulky, "manifest": {}}),
    )

    result = query_knowledge("anything", mode="full")
    assert result["status"] == "success"
    assert result["data"]["mode"] == "full"
    assert len(result["data"]["matches"][0]["text"]) == 10_000
    assert "text_truncated" not in result["data"]["matches"][0]


def test_query_knowledge_unknown_mode_returns_error() -> None:
    from suews_mcp.tools import query_knowledge

    result = query_knowledge("anything", mode="not-a-mode")
    assert result["status"] == "error"
    assert any("Unknown mode" in str(e) for e in result["errors"])
