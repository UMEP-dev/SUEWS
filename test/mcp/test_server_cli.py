"""Tests for ``suews-mcp`` console-script CLI argument handling."""

from __future__ import annotations

import os
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def test_parse_args_accepts_root_flag(tmp_path: Path) -> None:
    """The ``--root`` flag must accept a path string and surface it on
    the parsed namespace (gh#1405).
    """
    from suews_mcp.server import _parse_args

    ns = _parse_args(["--root", str(tmp_path)])
    assert ns.root == str(tmp_path)


def test_parse_args_default_root_is_none() -> None:
    """When ``--root`` is omitted the namespace value is ``None`` so
    ``main()`` knows to defer to the env-var / cwd fallback chain in
    ``ProjectRoot.__init__`` (gh#1405).
    """
    from suews_mcp.server import _parse_args

    ns = _parse_args([])
    assert ns.root is None


def test_main_root_flag_sets_env_var_before_server_build(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """``main(--root <path>)`` must set ``SUEWS_MCP_PROJECT_ROOT`` before
    ``_build_server()`` is called so per-tool ``ProjectRoot`` instances
    pick it up. Conductor-isolated launches without this flag inherit a
    temp cwd as the project root, leading to confusing path-rejection
    errors (gh#1405).
    """
    from suews_mcp import server as server_mod
    from suews_mcp.constants import ENV_PROJECT_ROOT

    captured: dict[str, str] = {}

    def fake_build_server():
        captured["env_value"] = os.environ.get(ENV_PROJECT_ROOT, "<unset>")

        class _Stub:
            def run(self):
                captured["ran"] = "yes"

        return _Stub()

    monkeypatch.delenv(ENV_PROJECT_ROOT, raising=False)
    monkeypatch.setattr(server_mod, "_build_server", fake_build_server)

    server_mod.main(["--root", str(tmp_path)])

    assert captured["ran"] == "yes"
    assert captured["env_value"] == str(tmp_path.resolve())


def test_check_knowledge_pack_freshness_returns_none_for_matching_versions(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """When the pack manifest's ``suews_version`` matches the running
    supy, the staleness probe returns ``None`` (no startup warning).
    gh#1406.
    """
    from suews_mcp import server as server_mod
    import supy

    def fake_load_manifest():
        return {
            "suews_version": supy.__version__,
            "git_sha": "deadbeef",
        }

    monkeypatch.setattr(
        "supy.knowledge.load_manifest", fake_load_manifest, raising=False
    )
    assert server_mod._check_knowledge_pack_freshness() is None


def test_check_knowledge_pack_freshness_warns_on_drift(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """When the pack was built against a different supy version, the
    probe returns a warning string for the server to log to stderr at
    startup. gh#1406.
    """
    from suews_mcp import server as server_mod

    def fake_load_manifest():
        return {
            "suews_version": "0.0.0-stale",
            "git_sha": "deadbeefcafebabe1234567890abcdef00000000",
        }

    monkeypatch.setattr(
        "supy.knowledge.load_manifest", fake_load_manifest, raising=False
    )
    warning = server_mod._check_knowledge_pack_freshness()
    assert warning is not None
    assert "0.0.0-stale" in warning
    assert "knowledge pack staleness" in warning
    assert "suews knowledge build" in warning


def test_check_knowledge_pack_freshness_handles_unreadable_manifest(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """An unreadable / missing manifest must not crash server
    startup; the probe degrades to ``None`` (gh#1406).
    """
    from suews_mcp import server as server_mod

    def fake_load_manifest():
        raise FileNotFoundError("no pack")

    monkeypatch.setattr(
        "supy.knowledge.load_manifest", fake_load_manifest, raising=False
    )
    assert server_mod._check_knowledge_pack_freshness() is None


def test_main_without_root_does_not_override_env(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """When ``--root`` is omitted the env var must be left alone so the
    user's existing ``SUEWS_MCP_PROJECT_ROOT`` continues to take effect
    (gh#1405). Defaulting silently to ``cwd`` here would mask
    misconfiguration.
    """
    from suews_mcp import server as server_mod
    from suews_mcp.constants import ENV_PROJECT_ROOT

    monkeypatch.setenv(ENV_PROJECT_ROOT, str(tmp_path))

    captured: dict[str, str] = {}

    def fake_build_server():
        captured["env_value"] = os.environ.get(ENV_PROJECT_ROOT, "<unset>")

        class _Stub:
            def run(self):
                pass

        return _Stub()

    monkeypatch.setattr(server_mod, "_build_server", fake_build_server)

    server_mod.main([])

    # Env var preserved unchanged.
    assert captured["env_value"] == str(tmp_path)
