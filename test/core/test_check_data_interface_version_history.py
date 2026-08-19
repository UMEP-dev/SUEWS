"""Tests for the forcing/output version-history audit."""

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
    / "check_data_interface_version_history.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_data_interface_version_history",
    SCRIPT_PATH,
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
audit = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = audit
SCRIPT_SPEC.loader.exec_module(audit)

_DIGEST_A = f"sha256:{'a' * 64}"
_DIGEST_B = f"sha256:{'b' * 64}"


def _state(current: str | None, *versions: tuple[str, str]) -> audit.VersionState:
    return audit.VersionState(current, versions)


def test_unpublished_interfaces_are_valid() -> None:
    audit._validate_state("forcing", _state(None))
    audit._validate_state("output", _state(None))


@pytest.mark.parametrize(
    "state",
    [
        _state("1.0.0"),
        _state(None, ("1.0.0", _DIGEST_A)),
        _state("1.1.0", ("1.0.0", _DIGEST_A)),
    ],
)
def test_current_version_must_match_history(state: audit.VersionState) -> None:
    with pytest.raises(audit.AuditFailure, match=r"empty history|current must"):
        audit._validate_state("forcing", state)


@pytest.mark.parametrize(
    ("versions", "message"),
    [
        ((("0.1.0", _DIGEST_A),), "first public version"),
        ((("1.0", _DIGEST_A),), "MAJOR.MINOR.PATCH"),
        ((("1.0.0", "not-a-digest"),), "SHA-256"),
        (
            (("1.0.0", _DIGEST_A), ("1.0.0", _DIGEST_B)),
            "increase monotonically",
        ),
        (
            (("1.1.0", _DIGEST_A), ("1.0.0", _DIGEST_B)),
            "first public version",
        ),
    ],
)
def test_invalid_history_is_rejected(
    versions: tuple[tuple[str, str], ...],
    message: str,
) -> None:
    with pytest.raises(audit.AuditFailure, match=message):
        audit._validate_state("output", _state(versions[-1][0], *versions))


def test_stable_semver_history_may_append() -> None:
    state = _state(
        "2.0.0",
        ("1.0.0", _DIGEST_A),
        ("1.3.0", _DIGEST_B),
        ("2.0.0", f"sha256:{'c' * 64}"),
    )

    audit._validate_state("forcing", state)


@pytest.mark.parametrize(
    ("base", "current", "message"),
    [
        (
            _state("1.0.0", ("1.0.0", _DIGEST_A)),
            _state("1.0.0", ("1.0.0", _DIGEST_B)),
            "append-only",
        ),
        (
            _state("1.0.0", ("1.0.0", _DIGEST_A)),
            _state("1.1.0", ("1.0.0", _DIGEST_A)),
            "change together",
        ),
        (
            _state("1.0.0", ("1.0.0", _DIGEST_A)),
            _state(
                "1.0.0",
                ("1.0.0", _DIGEST_A),
                ("1.1.0", _DIGEST_B),
            ),
            "change together",
        ),
    ],
)
def test_history_transition_rejects_rewrites_and_partial_bumps(
    base: audit.VersionState,
    current: audit.VersionState,
    message: str,
) -> None:
    with pytest.raises(audit.AuditFailure, match=message):
        audit._validate_transition("output", base, current)


def test_history_transition_accepts_one_or_more_new_versions() -> None:
    base = _state("1.0.0", ("1.0.0", _DIGEST_A))
    current = _state(
        "2.0.0",
        ("1.0.0", _DIGEST_A),
        ("1.1.0", _DIGEST_B),
        ("2.0.0", f"sha256:{'c' * 64}"),
    )

    assert audit._validate_transition("forcing", base, current)


def test_version_module_parser_reads_literal_dict() -> None:
    source = f'''\
CURRENT_OUTPUT_VERSION: str | None = "1.0.0"
OUTPUT_VERSIONS: dict[str, str] = {{"1.0.0": "{_DIGEST_A}"}}
'''

    assert audit._parse_state(
        source,
        "CURRENT_OUTPUT_VERSION",
        "OUTPUT_VERSIONS",
    ) == _state("1.0.0", ("1.0.0", _DIGEST_A))


def _git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        check=True,
        cwd=repo,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _write_versions(
    repo: Path,
    kind: str,
    current: str | None,
    versions: dict[str, str],
) -> None:
    upper = kind.upper()
    path = repo / f"src/supy/data_model/{kind}/version.py"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        f"CURRENT_{upper}_VERSION: str | None = {current!r}\n"
        f"{upper}_VERSIONS: dict[str, str] = {versions!r}\n",
        encoding="utf-8",
    )


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


def test_cli_audits_the_real_forcing_and_output_layout(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")
    _write_versions(repo, "forcing", None, {})
    _write_versions(repo, "output", None, {})
    _commit(repo, "initial")
    _git(repo, "checkout", "-qb", "feature")
    _write_versions(repo, "output", "1.0.0", {"1.0.0": _DIGEST_A})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 0
    assert "output" in capsys.readouterr().out


def test_cli_rejects_a_released_digest_rewrite(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")
    _write_versions(repo, "forcing", "1.0.0", {"1.0.0": _DIGEST_A})
    _write_versions(repo, "output", None, {})
    _commit(repo, "publish forcing")
    _git(repo, "checkout", "-qb", "feature")
    _write_versions(repo, "forcing", "1.0.0", {"1.0.0": _DIGEST_B})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "append-only" in capsys.readouterr().err
