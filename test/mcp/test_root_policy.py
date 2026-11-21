"""Trusted launcher root policy and temporary-host compatibility."""

import json
import os
from pathlib import Path
import subprocess

import pytest

from suews_mcp.backend import ProjectRoot, SUEWSMCPSandboxError
from suews_mcp.constants import ENV_ALLOWED_ROOTS, ENV_PROJECT_ROOT

pytestmark = pytest.mark.api


@pytest.fixture
def policy(monkeypatch, tmp_path):
    home = tmp_path / "home"
    home.mkdir()
    host = tmp_path / "host-cwd"
    host.mkdir()
    monkeypatch.setattr(Path, "home", classmethod(lambda cls: home))
    monkeypatch.chdir(host)
    monkeypatch.delenv(ENV_PROJECT_ROOT, raising=False)
    monkeypatch.delenv(ENV_ALLOWED_ROOTS, raising=False)
    return home


def test_temp_host_accepts_explicit_home_project(policy):
    project = policy / "project"
    project.mkdir()
    assert ProjectRoot(project).root == project
    with pytest.raises(SUEWSMCPSandboxError, match="not authorised"):
        ProjectRoot()


def test_home_cwd_default_and_home_itself(policy, monkeypatch):
    monkeypatch.chdir(policy)
    assert ProjectRoot().root == policy
    assert ProjectRoot(policy).root == policy


@pytest.mark.parametrize("root", ["/", "/etc", "/usr", "/var", "/tmp"])
def test_unanchored_system_roots_rejected(policy, root):
    with pytest.raises(SUEWSMCPSandboxError, match="not authorised"):
        ProjectRoot(root)


def test_launcher_allowlist_accepts_roots_and_descendants(
    policy, tmp_path, monkeypatch
):
    shared = tmp_path / "shared"
    shared.mkdir()
    second = tmp_path / "second"
    second.mkdir()
    monkeypatch.setenv(ENV_ALLOWED_ROOTS, json.dumps([str(shared), str(second)]))
    assert ProjectRoot(shared).root == shared
    assert ProjectRoot(shared / "future-case").root == shared / "future-case"
    monkeypatch.chdir(second)
    assert ProjectRoot().root == second
    with pytest.raises(SUEWSMCPSandboxError):
        ProjectRoot(tmp_path / "shared-sibling")


@pytest.mark.parametrize(
    "value",
    [
        "",
        "not json",
        "{}",
        '"/tmp"',
        "[null]",
        "[1]",
        '[""]',
        '["relative"]',
        '["/"]',
        '["/missing/allowlisted-root"]',
    ],
)
def test_bad_allowlist_fails_closed_even_for_home(policy, monkeypatch, value):
    monkeypatch.setenv(ENV_ALLOWED_ROOTS, value)
    with pytest.raises(SUEWSMCPSandboxError, match=ENV_ALLOWED_ROOTS):
        ProjectRoot(policy)


def test_empty_allowlist_keeps_only_home(policy, tmp_path, monkeypatch):
    monkeypatch.setenv(ENV_ALLOWED_ROOTS, "[]")
    assert ProjectRoot(policy).root == policy
    with pytest.raises(SUEWSMCPSandboxError):
        ProjectRoot(tmp_path)


def test_symlink_canonicalisation_precedes_root_policy(policy, tmp_path, monkeypatch):
    outside = tmp_path / "outside"
    outside.mkdir()
    (policy / "escape").symlink_to(outside, target_is_directory=True)
    with pytest.raises(SUEWSMCPSandboxError):
        ProjectRoot(policy / "escape")
    (outside / "home-alias").symlink_to(policy, target_is_directory=True)
    assert ProjectRoot(outside / "home-alias").root == policy
    monkeypatch.setenv(ENV_ALLOWED_ROOTS, json.dumps([str(policy / "escape")]))
    assert ProjectRoot(outside).root == outside


def test_session_boundary_overrides_broader_allowlist(policy, tmp_path, monkeypatch):
    session = tmp_path / "session"
    session.mkdir()
    child = session / "child"
    child.mkdir()
    monkeypatch.setenv(ENV_ALLOWED_ROOTS, json.dumps([str(tmp_path)]))
    monkeypatch.setenv(ENV_PROJECT_ROOT, str(session))
    assert ProjectRoot().root == session
    assert ProjectRoot(child).root == child
    assert ProjectRoot(policy).root == session
    assert ProjectRoot("/").root == session
    with pytest.raises(SUEWSMCPSandboxError):
        ProjectRoot(policy).resolve(policy / "secret")


def test_session_symlink_canonicalisation(policy, tmp_path, monkeypatch):
    (tmp_path / "session-alias").symlink_to(policy, target_is_directory=True)
    monkeypatch.setenv(ENV_PROJECT_ROOT, str(tmp_path / "session-alias"))
    assert ProjectRoot().root == policy


_CALLS = [
    ("validate_config", {"config_path": "config.yml"}),
    ("inspect_config", {"config_path": "config.yml"}),
    ("assess_readiness", {"config_path": "config.yml"}),
    ("convert_config", {"input_path": "config.yml", "output_path": "out.yml"}),
    ("init_case", {"target_dir": "new-case"}),
    ("diagnose_run", {"run_dir": "run"}),
    ("summarise_run", {"run_dir": "run"}),
    ("compare_runs", {"run_a": "a", "run_b_or_obs": "b"}),
    ("query_knowledge", {"question": "heat", "pack": "pack"}),
    ("read_knowledge_manifest", {"pack": "pack"}),
]


@pytest.mark.parametrize("name,kwargs", _CALLS)
@pytest.mark.parametrize(
    "failure", ["root", "allowlist", "empty_session", "symlink_loop"]
)
def test_all_tool_root_failures_return_envelopes(
    policy, monkeypatch, name, kwargs, failure
):
    from suews_mcp import tools

    original_run = subprocess.run

    def forbidden(command, *args, **kwargs):
        if command[0] == "git":
            return original_run(command, *args, **kwargs)
        pytest.fail("Rejected root reached CLI")

    monkeypatch.setattr(subprocess, "run", forbidden)
    root = "/"
    if failure == "allowlist":
        monkeypatch.setenv(ENV_ALLOWED_ROOTS, "broken")
        root = str(policy)
    elif failure == "empty_session":
        monkeypatch.setenv(ENV_PROJECT_ROOT, "")
    elif failure == "symlink_loop":
        link = policy / "loop"
        link.symlink_to(link)
        root = str(link)
    result = getattr(tools, name)(**kwargs, project_root=root)
    assert result["status"] == "error"
    assert result["errors"]


def test_resource_rejected_root_returns_envelope(policy):
    from suews_mcp.resources.runs import read_run_resource

    result = read_run_resource("run", "provenance", project_root="/")
    assert result["status"] == "error"
    assert "not authorised" in str(result["errors"])


@pytest.mark.parametrize("tool", ["diagnose_run", "summarise_run", "compare_runs"])
@pytest.mark.skipif(
    os.name == "nt" or (hasattr(os, "geteuid") and os.geteuid() == 0),
    reason="POSIX mode-bit denial requires a non-root POSIX account",
)
def test_unreadable_run_preflight_returns_envelope(policy, tool):
    from suews_mcp import tools

    run = policy / "run"
    run.mkdir()
    run.chmod(0)
    try:
        args = [str(run), str(run)] if tool == "compare_runs" else [str(run)]
        result = getattr(tools, tool)(*args, project_root=str(policy))
        assert result["status"] == "error"
        assert "Permission denied" in str(result["errors"])
    finally:
        run.chmod(0o700)


@pytest.mark.skipif(
    os.name == "nt" or (hasattr(os, "geteuid") and os.geteuid() == 0),
    reason="POSIX mode-bit denial requires a non-root POSIX account",
)
def test_unreadable_resource_returns_envelope(policy):
    from suews_mcp.resources.runs import read_run_resource

    target = policy / "provenance.json"
    target.write_text("{}", encoding="utf-8")
    target.chmod(0)
    try:
        result = read_run_resource(".", "provenance", project_root=str(policy))
        assert result["status"] == "error"
        assert "Permission denied" in str(result["errors"])
    finally:
        target.chmod(0o600)
