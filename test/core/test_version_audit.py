"""Tests for policy-neutral version-audit plumbing."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = pytest.mark.api

HELPER_PATH = (
    Path(__file__).resolve().parents[2] / "scripts" / "lint" / "version_audit.py"
)
HELPER_SPEC = importlib.util.spec_from_file_location("version_audit", HELPER_PATH)
assert HELPER_SPEC is not None
assert HELPER_SPEC.loader is not None
version_audit = importlib.util.module_from_spec(HELPER_SPEC)
sys.modules[HELPER_SPEC.name] = version_audit
HELPER_SPEC.loader.exec_module(version_audit)


def _git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        check=True,
        cwd=repo,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _commit(repo: Path, message: str) -> None:
    _git(repo, "add", "-A")
    _git(
        repo,
        "-c",
        "user.name=test",
        "-c",
        "user.email=test@example.com",
        "commit",
        "-qm",
        message,
    )


def test_extract_literal_assignments_reads_plain_and_annotated_values() -> None:
    source = """\
CURRENT = "1.0.0"
VERSIONS: dict[str, str] = {"1.0.0": "sha256:digest"}
"""

    assert version_audit.extract_literal_assignments(
        source,
        ("CURRENT", "VERSIONS"),
    ) == {
        "CURRENT": "1.0.0",
        "VERSIONS": {"1.0.0": "sha256:digest"},
    }


@pytest.mark.parametrize(
    "source",
    [
        "OTHER = 'value'\n",
        "def nested():\n    CURRENT = '1.0.0'\n",
    ],
)
def test_extract_literal_assignments_requires_top_level_names(source: str) -> None:
    with pytest.raises(ValueError, match="could not read CURRENT"):
        version_audit.extract_literal_assignments(source, ("CURRENT",))


def test_extract_literal_assignments_rejects_computed_values() -> None:
    with pytest.raises(ValueError):
        version_audit.extract_literal_assignments(
            "CURRENT = make_version()\n",
            ("CURRENT",),
        )


def test_merge_base_and_ref_file_reading(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")
    version_file = repo / "version.py"
    version_file.write_text('CURRENT = "1.0.0"\n', encoding="utf-8")
    _commit(repo, "initial")
    base_commit = _git(repo, "rev-parse", "HEAD").strip()

    _git(repo, "checkout", "-qb", "feature")
    version_file.write_text('CURRENT = "1.1.0"\n', encoding="utf-8")
    _commit(repo, "feature")
    monkeypatch.chdir(repo)

    assert version_audit.resolve_merge_base("master") == base_commit
    assert (
        version_audit.read_file_at_ref(base_commit, "version.py")
        == 'CURRENT = "1.0.0"\n'
    )
    assert version_audit.read_file_at_ref(base_commit, "missing.py") is None
    with pytest.raises(subprocess.CalledProcessError):
        version_audit.read_file_at_ref("not-a-ref", "version.py")
