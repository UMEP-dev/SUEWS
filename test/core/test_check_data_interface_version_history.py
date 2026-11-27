"""Tests for the forcing/output version-history audit."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SCHEMA", "C-TOOLING")]

LINT_ROOT = Path(__file__).resolve().parents[2] / "scripts" / "lint"
if str(LINT_ROOT) not in sys.path:
    sys.path.insert(0, str(LINT_ROOT))
SCRIPT_PATH = LINT_ROOT / "check_data_interface_version_history.py"
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
_DIGEST_C = f"sha256:{'c' * 64}"


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


def test_output_2_0_0_digest_is_immutable() -> None:
    base = _state("2.0.0", ("1.0.0", _DIGEST_A), ("2.0.0", _DIGEST_A))
    current = _state("2.0.0", ("1.0.0", _DIGEST_A), ("2.0.0", _DIGEST_B))
    for kind in ("output", "forcing"):
        with pytest.raises(audit.AuditFailure, match="append-only"):
            audit._validate_transition(kind, base, current)


def _write_forcing_manifests(
    repo: Path,
    digests: dict[str, str],
    *,
    extra_source: str = "",
) -> None:
    path = repo / "src/supy/data_model/contracts.py"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        f"FORCING_PUBLISHED_MANIFEST_DIGESTS: dict[str, str] = {digests!r}\n"
        f"{extra_source}",
        encoding="utf-8",
    )


def test_forcing_manifest_digests_valid_states_pass() -> None:
    audit._validate_manifest_digests(())
    audit._validate_manifest_digests((("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)))


@pytest.mark.parametrize(
    ("digests", "message"),
    [
        ((("1.0.0", "not-a-digest"),), "SHA-256"),
        ((("1.0.0", "a" * 64),), "SHA-256"),
        ((("1.0.0", f"sha256:{'A' * 64}"),), "SHA-256"),
        ((("1.0.0", f"sha256:{'a' * 63}"),), "SHA-256"),
        ((("1.0.0", f"{_DIGEST_A}\n"),), "SHA-256"),
        ((("1.0", _DIGEST_A),), "MAJOR.MINOR.PATCH"),
        ((("0.1.0", _DIGEST_A),), "first public version"),
        ((("1.0.0", _DIGEST_A), ("1.0.0", _DIGEST_B)), "increase monotonically"),
    ],
)
def test_forcing_manifest_digests_invalid_entries_rejected(
    digests: tuple[tuple[str, str], ...],
    message: str,
) -> None:
    with pytest.raises(audit.AuditFailure, match=message):
        audit._validate_manifest_digests(digests)


def test_forcing_manifest_digests_transition_accepts_unchanged_and_append() -> None:
    base = (("1.0.0", _DIGEST_A),)
    assert not audit._validate_manifest_digests_transition(base, base)
    assert audit._validate_manifest_digests_transition(
        base,
        (("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)),
    )


@pytest.mark.parametrize(
    ("base", "current"),
    [
        ((("1.0.0", _DIGEST_A),), ()),
        ((("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)), (("1.0.0", _DIGEST_A),)),
        ((("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)), (("1.1.0", _DIGEST_B),)),
        (
            (("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)),
            (("1.0.0", _DIGEST_A), ("1.2.0", _DIGEST_C)),
        ),
        ((("1.0.0", _DIGEST_A),), (("1.0.0", _DIGEST_B),)),
        (
            (("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B)),
            (("1.1.0", _DIGEST_B), ("1.0.0", _DIGEST_A)),
        ),
        (
            (("1.0.0", _DIGEST_A), ("1.2.0", _DIGEST_C)),
            (("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B), ("1.2.0", _DIGEST_C)),
        ),
    ],
)
def test_forcing_manifest_digests_transition_rejects_delete_mutate_reorder(
    base: tuple[tuple[str, str], ...],
    current: tuple[tuple[str, str], ...],
) -> None:
    with pytest.raises(audit.AuditFailure, match="append-only"):
        audit._validate_manifest_digests_transition(base, current)


def test_forcing_manifest_digests_parity_check() -> None:
    forcing = (("1.0.0", _DIGEST_A), ("1.1.0", _DIGEST_B))
    manifests = (("1.0.0", _DIGEST_B), ("1.1.0", _DIGEST_C))
    audit._validate_manifest_digests_parity(forcing, manifests)

    with pytest.raises(audit.AuditFailure, match="match FORCING_VERSIONS"):
        audit._validate_manifest_digests_parity(forcing, (("1.0.0", _DIGEST_B),))
    with pytest.raises(audit.AuditFailure, match="match FORCING_VERSIONS"):
        audit._validate_manifest_digests_parity((("1.0.0", _DIGEST_A),), manifests)


def test_parse_manifest_digests_reads_literal_and_allows_reads_and_local_shadowing() -> (
    None
):
    source = (
        f'FORCING_PUBLISHED_MANIFEST_DIGESTS: dict[str, str] = {{"1.0.0": "{_DIGEST_A}"}}\n'
        "def lookup(ver: str) -> str | None:\n"
        "    if ver in FORCING_PUBLISHED_MANIFEST_DIGESTS:\n"
        "        return FORCING_PUBLISHED_MANIFEST_DIGESTS[ver]\n"
        "    return FORCING_PUBLISHED_MANIFEST_DIGESTS.get(ver)\n"
        "def local_shadow() -> dict[str, str]:\n"
        "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
        "    FORCING_PUBLISHED_MANIFEST_DIGESTS['x'] = 'y'\n"
        "    return FORCING_PUBLISHED_MANIFEST_DIGESTS\n"
        "local_lambda = lambda: (\n"
        "    (FORCING_PUBLISHED_MANIFEST_DIGESTS := {}),\n"
        "    FORCING_PUBLISHED_MANIFEST_DIGESTS.clear(),\n"
        ")\n"
        "class Namespace:\n"
        "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
    )
    assert audit._parse_manifest_digests(source) == (("1.0.0", _DIGEST_A),)


@pytest.mark.parametrize(
    ("source", "message"),
    [
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}", "1.0.0": "{_DIGEST_B}"}}\n',
            "duplicate version key",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_B}"}}\n',
            "assigned exactly once",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS["1.0.0"] = "{_DIGEST_B}"\n',
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS.update({{"1.0.0": "{_DIGEST_B}"}})\n',
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS.__setitem__("1.0.0", "{_DIGEST_B}")\n',
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "pins = FORCING_PUBLISHED_MANIFEST_DIGESTS\n"
            "pins.clear()\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "match {}:\n"
            "    case FORCING_PUBLISHED_MANIFEST_DIGESTS:\n"
            "        pass\n",
            "must not be reassigned or deleted",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "try:\n"
            "    pass\n"
            "except Exception as FORCING_PUBLISHED_MANIFEST_DIGESTS:\n"
            "    pass\n",
            "must not be reassigned or deleted",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "def mutate() -> None:\n"
            "    global FORCING_PUBLISHED_MANIFEST_DIGESTS\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n",
            "must not be reassigned or deleted",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class Sneaky:\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    def method(self) -> None:\n"
            "        FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class PreBindMutate:\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class ConditionalBindMutate:\n"
            "    if False:\n"
            "        FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n",
            "read-only lookups",
        ),
        (
            f'pins = FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "pins.clear()\n",
            "single-target top-level dict literal",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            '_ = [None for FORCING_PUBLISHED_MANIFEST_DIGESTS["1.0.0"] in ["x"]]\n',
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class DeleteThenMutate:\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    del FORCING_PUBLISHED_MANIFEST_DIGESTS\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class CompSkipsClassScope:\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    result = [FORCING_PUBLISHED_MANIFEST_DIGESTS.clear() for _ in (0,)]\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "def change(dict):\n"
            "    dict(FORCING_PUBLISHED_MANIFEST_DIGESTS)\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "from other_mod import *\n",
            "wildcard imports",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            '_ = "1.0.0" in FORCING_PUBLISHED_MANIFEST_DIGESTS == object()\n',
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "def install():\n"
            "    global dict\n"
            "    dict = lambda mapping: mapping.clear()\n"
            "install()\n"
            "dict(FORCING_PUBLISHED_MANIFEST_DIGESTS)\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "def initialise():\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    class Namespace:\n"
            "        FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n"
            "        FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "class Namespace:\n"
            "    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    type Deferred = FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n"
            "    del FORCING_PUBLISHED_MANIFEST_DIGESTS\n",
            "read-only lookups",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "type FORCING_PUBLISHED_MANIFEST_DIGESTS = int\n",
            "must not be reassigned or deleted",
        ),
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "del FORCING_PUBLISHED_MANIFEST_DIGESTS\n",
            "must not be reassigned or deleted",
        ),
        (
            f'OTHER = {{"1.0.0": "{_DIGEST_A}"}}\n'
            "FORCING_PUBLISHED_MANIFEST_DIGESTS = {**OTHER}\n",
            "string-to-string dict literal",
        ),
        (
            "OTHER_MAP = {}\n",
            "could not read FORCING_PUBLISHED_MANIFEST_DIGESTS",
        ),
    ],
)
def test_parse_manifest_digests_rejects_bypass_constructions(
    source: str,
    message: str,
) -> None:
    with pytest.raises(ValueError, match=message):
        audit._parse_manifest_digests(source)


def _init_forcing_manifest_repo(repo: Path) -> None:
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")
    _write_versions(repo, "forcing", "1.0.0", {"1.0.0": _DIGEST_A})
    _write_versions(repo, "output", None, {})
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_B})
    _commit(repo, "initial forcing release")
    _git(repo, "checkout", "-qb", "feature")


def test_cli_accepts_appending_forcing_manifest_digest(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_versions(
        repo,
        "forcing",
        "1.1.0",
        {"1.0.0": _DIGEST_A, "1.1.0": _DIGEST_C},
    )
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_B, "1.1.0": _DIGEST_A})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 0
    out = capsys.readouterr().out
    assert "forcing" in out
    assert "forcing-manifest" in out


def test_cli_accepts_unrelated_contracts_edit(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_forcing_manifests(
        repo,
        {"1.0.0": _DIGEST_B},
        extra_source="HELPER_CONSTANT = 1\n",
    )
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 0
    assert "no version-history changes" in capsys.readouterr().out


def test_cli_rejects_deleting_forcing_manifest_digest(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_forcing_manifests(repo, {})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "append-only" in capsys.readouterr().err


def test_cli_rejects_mutating_forcing_manifest_digest(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_C})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "append-only" in capsys.readouterr().err


def test_cli_rejects_invalid_forcing_manifest_digest_format(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_versions(
        repo,
        "forcing",
        "1.1.0",
        {"1.0.0": _DIGEST_A, "1.1.0": _DIGEST_C},
    )
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_B, "1.1.0": "sha256:INVALID"})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "SHA-256" in capsys.readouterr().err


def test_cli_rejects_missing_contracts_file_after_manifest_release(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    (repo / "src/supy/data_model/contracts.py").unlink()
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "missing" in capsys.readouterr().err


def test_cli_rejects_forcing_version_bump_without_manifest_digest(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_versions(
        repo,
        "forcing",
        "1.1.0",
        {"1.0.0": _DIGEST_A, "1.1.0": _DIGEST_C},
    )
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 1
    assert "match FORCING_VERSIONS" in capsys.readouterr().err


@pytest.mark.parametrize(
    ("extra_source", "expected_err"),
    [
        (
            f'FORCING_PUBLISHED_MANIFEST_DIGESTS.__setitem__("1.0.0", "{_DIGEST_C}")\n',
            "read-only lookups",
        ),
        (
            "pins = FORCING_PUBLISHED_MANIFEST_DIGESTS\npins.clear()\n",
            "read-only lookups",
        ),
        (
            "match {}:\n    case FORCING_PUBLISHED_MANIFEST_DIGESTS:\n        pass\n",
            "must not be reassigned or deleted",
        ),
        (
            "try:\n    pass\nexcept Exception as FORCING_PUBLISHED_MANIFEST_DIGESTS:\n    pass\n",
            "must not be reassigned or deleted",
        ),
        (
            "def install():\n    global dict\n    dict = lambda mapping: mapping.clear()\n"
            "install()\ndict(FORCING_PUBLISHED_MANIFEST_DIGESTS)\n",
            "read-only lookups",
        ),
        (
            "def initialise():\n    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    class Namespace:\n        FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n"
            "        FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n",
            "read-only lookups",
        ),
        (
            "class Namespace:\n    FORCING_PUBLISHED_MANIFEST_DIGESTS = {}\n"
            "    type Deferred = FORCING_PUBLISHED_MANIFEST_DIGESTS.clear()\n"
            "    del FORCING_PUBLISHED_MANIFEST_DIGESTS\n",
            "read-only lookups",
        ),
    ],
)
def test_cli_rejects_ast_mutation_and_rebinding_bypasses(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
    extra_source: str,
    expected_err: str,
) -> None:
    repo = tmp_path / "repo"
    _init_forcing_manifest_repo(repo)
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_B}, extra_source=extra_source)
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 2
    assert expected_err in capsys.readouterr().err


def test_cli_accepts_introducing_manifest_digests_from_historical_base(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "master")
    _write_versions(repo, "forcing", "1.0.0", {"1.0.0": _DIGEST_A})
    _write_versions(repo, "output", None, {})
    contracts = repo / "src/supy/data_model/contracts.py"
    contracts.parent.mkdir(parents=True, exist_ok=True)
    contracts.write_text('DEFAULT_BASE_URL = "https://suews.io"\n', encoding="utf-8")
    _commit(repo, "historical contracts without manifest digest map")
    _git(repo, "checkout", "-qb", "feature")
    _write_forcing_manifests(repo, {"1.0.0": _DIGEST_B})
    monkeypatch.chdir(repo)

    assert audit.main(["--base", "master"]) == 0
    assert "forcing-manifest" in capsys.readouterr().out
