"""Tests for scripts/lint/check_schema_version_bump.py.

Covers two follow-up regressions after gh#1304:

- merge-base semantics: the base-side `CURRENT_SCHEMA_VERSION` must be
  read at the merge-base of the branch and `base_ref`, not at the tip
  of `base_ref`. Without this, an unrelated schema bump landing on
  `master` after the branch diverges would false-pass as a bump on the
  branch under review.
- repo-root independence: the script must work regardless of the
  caller's CWD. The `__main__` entry `chdir`s to the repo root so the
  relative paths (`src/...`, `docs/...`) resolve.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = pytest.mark.api


SCRIPT_PATH = (
    Path(__file__).resolve().parents[2]
    / "scripts"
    / "lint"
    / "check_schema_version_bump.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_schema_version_bump", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_schema_version_bump = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_schema_version_bump
SCRIPT_SPEC.loader.exec_module(check_schema_version_bump)


def _git(repo: Path, *args: str) -> str:
    """Run git inside `repo`, returning stdout. Fails fast on non-zero."""
    result = subprocess.run(
        ["git", *args],
        check=True,
        cwd=repo,
        capture_output=True,
        text=True,
        env={
            "GIT_AUTHOR_NAME": "test",
            "GIT_AUTHOR_EMAIL": "test@example.com",
            "GIT_COMMITTER_NAME": "test",
            "GIT_COMMITTER_EMAIL": "test@example.com",
            "PATH": _path_env(),
        },
    )
    return result.stdout


def _path_env() -> str:
    import os

    return os.environ.get("PATH", "/usr/bin:/bin")


def _write(repo: Path, rel: str, text: str) -> None:
    path = repo / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _make_version_file(version: str) -> str:
    return f'CURRENT_SCHEMA_VERSION = "{version}"\n'


@pytest.fixture
def branched_repo(tmp_path: Path) -> Path:
    """Build a miniature repo with `master` and a feature branch.

    Layout mirrors the real paths the script watches so the watched
    prefix logic matches: `src/supy/data_model/schema/version.py` and a
    sibling file under `src/supy/data_model/`.

    Timeline:
      1. master commit sets CURRENT_SCHEMA_VERSION="1.0".
      2. feature branch diverges, touches a watched file, leaves version
         alone.
      3. master advances with a *later* bump to "2.0" — unrelated to
         the feature branch, simulating a parallel PR merging first.
    """
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")

    version_rel = "src/supy/data_model/schema/version.py"
    sibling_rel = "src/supy/data_model/sample.py"

    _write(repo, version_rel, _make_version_file("1.0"))
    _write(repo, sibling_rel, "# initial\n")
    _git(repo, "add", "-A")
    _git(repo, "commit", "-qm", "initial")

    _git(repo, "checkout", "-qb", "feature")
    _write(repo, sibling_rel, "# feature change\n")
    _git(repo, "add", "-A")
    _git(repo, "commit", "-qm", "feature edit")

    _git(repo, "checkout", "-q", "master")
    _write(repo, version_rel, _make_version_file("2.0"))
    _git(repo, "add", "-A")
    _git(repo, "commit", "-qm", "master schema bump")
    _git(repo, "checkout", "-q", "feature")
    return repo


def test_merge_base_ignores_unrelated_master_bump(
    branched_repo: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """A bump that lands on `master` after divergence must NOT be
    attributed to this branch. Reading from the merge-base keeps the
    comparison focused on what the branch itself changed.
    """
    monkeypatch.chdir(branched_repo)
    rc = check_schema_version_bump.main(["--base", "master"])
    captured = capsys.readouterr()

    # The branch only touched a non-version file; version stayed at 1.0
    # at the merge-base too. So the gate should see "unchanged" and
    # fail, flagging the missing bump — NOT silently pass.
    assert rc == 1, captured.err
    assert "still '1.0'" in captured.err


def test_main_entry_chdirs_to_repo_root(branched_repo: Path) -> None:
    """Running the script from a nested subdirectory must still resolve
    the relative `src/...` and `docs/...` paths. The `__main__` block
    pins CWD to the repo root computed from `__file__`.

    Simulated by copying the script into the test repo at the expected
    `scripts/lint/` location, then invoking it from a nested subdir.
    Before fix 2, this invocation failed with "Could not read
    CURRENT_SCHEMA_VERSION".
    """
    target_dir = branched_repo / "scripts" / "lint"
    target_dir.mkdir(parents=True)
    script_copy = target_dir / SCRIPT_PATH.name
    script_copy.write_text(SCRIPT_PATH.read_text(), encoding="utf-8")
    nested = branched_repo / "src"
    assert nested.is_dir()

    result = subprocess.run(
        [sys.executable, str(script_copy), "--base", "master"],
        cwd=nested,
        capture_output=True,
        text=True,
    )
    # Version unchanged on the feature branch (merge-base had 1.0, HEAD
    # still has 1.0), so the gate should fire the "still '1.0'" failure
    # path rather than the "Could not read CURRENT_SCHEMA_VERSION" path
    # that the old relative-path code produced from a subdir.
    assert result.returncode == 1, result.stderr
    assert "still '1.0'" in result.stderr
    assert "Could not read CURRENT_SCHEMA_VERSION" not in result.stderr


def test_extract_current_schema_version_parses_literal() -> None:
    """Sanity check on the regex: it must find the quoted value even
    when other assignments appear above it.
    """
    source = (
        "# preamble\n"
        "SOMETHING = 'else'\n"
        'CURRENT_SCHEMA_VERSION = "2026.4"\n'
        "more_code = 1\n"
    )
    assert (
        check_schema_version_bump._extract_current_schema_version(source)
        == "2026.4"
    )


def test_extract_current_schema_version_returns_none_when_missing() -> None:
    """An unrecognisable file must return None rather than guess."""
    assert (
        check_schema_version_bump._extract_current_schema_version(
            "some other file\n"
        )
        is None
    )
