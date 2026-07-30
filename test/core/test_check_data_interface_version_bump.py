"""Tests for the independent data-interface version audit."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = pytest.mark.api

SCRIPT_PATH = (
    Path(__file__).resolve().parents[2]
    / "scripts"
    / "lint"
    / "check_data_interface_version_bump.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_data_interface_version_bump",
    SCRIPT_PATH,
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
audit = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = audit
SCRIPT_SPEC.loader.exec_module(audit)

VERSION_PATH = "src/supy/data_model/interfaces/version.py"
DOC_PATH = "docs/source/contributing/schema/data_interface_versioning.rst"


def _git(repo: Path, *args: str) -> str:
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
            "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        },
    )
    return result.stdout


def _write(repo: Path, relative: str, content: str) -> None:
    path = repo / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def _canonical_json_bytes(value: object) -> bytes:
    return (
        json.dumps(
            value,
            allow_nan=False,
            ensure_ascii=False,
            separators=(",", ":"),
            sort_keys=True,
        )
        + "\n"
    ).encode()


def _digest(content: bytes) -> str:
    return f"sha256:{hashlib.sha256(content).hexdigest()}"


def _write_snapshot(
    repo: Path,
    kind: str,
    version: str,
    artefacts: dict[str, object],
) -> str:
    snapshot_dir = (
        repo
        / "src"
        / "supy"
        / "data_model"
        / "interfaces"
        / "artefacts"
        / kind
        / version
    )
    entries = []
    for name, value in sorted(artefacts.items()):
        content = _canonical_json_bytes(value)
        path = snapshot_dir / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
        entries.append({
            "path": name,
            "bytes": len(content),
            "digest": _digest(content),
        })
    manifest = _canonical_json_bytes({
        "format_version": 1,
        "interface": kind,
        "interface_version": version,
        "artefacts": entries,
    })
    (snapshot_dir / "manifest.json").write_bytes(manifest)
    return _digest(manifest)


def _record(
    version: str,
    previous: str | None,
    change: str,
    digest: str,
    summary: str | None = None,
) -> audit.ParsedRecord:
    return audit.ParsedRecord(
        version,
        previous,
        change,
        summary or f"Test {version}.",
        digest,
    )


def _registry_source(records: tuple[audit.ParsedRecord, ...]) -> str:
    if not records:
        return "()"
    lines = ["("]
    for record in records:
        lines.extend([
            "    InterfaceVersionRecord(",
            f"        version={record.version!r},",
            f"        previous={record.previous!r},",
            f"        change=InterfaceChange.{record.change.upper()},",
            f"        summary={record.summary!r},",
            f"        manifest_digest={record.manifest_digest!r},",
            "    ),",
        ])
    lines.append(")")
    return "\n".join(lines)


def _versions(
    forcing: tuple[audit.ParsedRecord, ...] = (),
    output: tuple[audit.ParsedRecord, ...] = (),
) -> str:
    forcing_current = forcing[-1].version if forcing else None
    output_current = output[-1].version if output else None
    return (
        f"CURRENT_FORCING_INTERFACE_VERSION: str | None = {forcing_current!r}\n"
        f"CURRENT_OUTPUT_INTERFACE_VERSION: str | None = {output_current!r}\n"
        f"FORCING_INTERFACE_VERSIONS = {_registry_source(forcing)}\n"
        f"OUTPUT_INTERFACE_VERSIONS = {_registry_source(output)}\n"
    )


def _validator_source(kind: str) -> str:
    return f"""\
import argparse
import json
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--snapshot-root", type=Path, required=True)
parser.add_argument("--version", required=True)
args = parser.parse_args()
snapshot = args.snapshot_root / {kind!r} / args.version
schema = json.loads((snapshot / "schema.json").read_text())
catalogue = json.loads((snapshot / "catalogue.json").read_text())
if schema.get("type") != "object" or not catalogue.get("variables"):
    raise SystemExit("fixture contract is incomplete")
"""


@pytest.fixture
def interface_repo(tmp_path: Path) -> Path:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q")
    _git(repo, "symbolic-ref", "HEAD", "refs/heads/master")
    _write(repo, VERSION_PATH, _versions())
    _write(repo, DOC_PATH, "Interface versions\n")
    _write(repo, "src/supy/data_model/output/variables.py", "# output\n")
    _write(repo, "src/supy/data_model/forcing/variables.py", "# forcing\n")
    _write(repo, "src/supy/data_model/validation/controller.py", "# internal\n")
    _write(
        repo,
        audit._VALIDATOR_PATHS["forcing"],
        _validator_source("forcing"),
    )
    _write(
        repo,
        audit._VALIDATOR_PATHS["output"],
        _validator_source("output"),
    )
    _write(
        repo,
        "src/supy/meson.build",
        """\
install_subdir(
  'data_model/interfaces/artefacts',
  install_dir: py.get_install_dir() / 'supy/data_model/interfaces',
)
""",
    )
    _git(repo, "add", "-A")
    _git(repo, "commit", "-qm", "initial")
    _git(repo, "checkout", "-qb", "feature")
    return repo


def _run_worktree(
    repo: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> int:
    monkeypatch.chdir(repo)
    return audit.main(["--base", "master", "--include-worktree"])


def test_unpublished_output_source_change_does_not_force_premature_v1(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/output/variables.py",
        "# changed output\n",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "no governed changes" in capsys.readouterr().out


def test_first_output_contract_version_requires_content_and_docs(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/output/variables.py",
        "# complete output contract\n",
    )
    digest = _write_snapshot(
        interface_repo,
        "output",
        "1.0.0",
        {
            "schema.json": {"type": "object"},
            "catalogue.json": {"variables": ["Year"]},
        },
    )
    output = (_record("1.0.0", None, "initial", digest),)
    _write(interface_repo, VERSION_PATH, _versions(output=output))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nOutput 1.0.0\n")

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "output: None -> '1.0.0'" in capsys.readouterr().out


def test_interface_version_cannot_move_without_a_bound_snapshot(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    forcing = (_record("1.0.0", None, "initial", f"sha256:{'0' * 64}"),)
    _write(interface_repo, VERSION_PATH, _versions(forcing=forcing))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nForcing 1.0.0\n")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "snapshot directories [] do not match registered versions" in (
        capsys.readouterr().err
    )


def test_first_interface_version_requires_a_passing_parity_validator(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    artefacts = {
        "schema.json": {},
        "catalogue.json": {},
    }
    digest = _write_snapshot(interface_repo, "forcing", "1.0.0", artefacts)
    forcing = (_record("1.0.0", None, "initial", digest),)
    _write(interface_repo, VERSION_PATH, _versions(forcing=forcing))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nForcing 1.0.0\n")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "implementation-parity validator failed" in capsys.readouterr().err


def test_first_interface_version_requires_a_validator(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["iy"]},
    }
    digest = _write_snapshot(interface_repo, "forcing", "1.0.0", artefacts)
    forcing = (_record("1.0.0", None, "initial", digest),)
    _write(interface_repo, VERSION_PATH, _versions(forcing=forcing))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nForcing 1.0.0\n")
    (interface_repo / audit._VALIDATOR_PATHS["forcing"]).unlink()

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "requires parity validator" in capsys.readouterr().err


def test_recursive_snapshot_packaging_rule_is_required(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/meson.build",
        "# snapshot install rule removed\n",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "must recursively install data-interface artefacts" in (
        capsys.readouterr().err
    )


def test_interface_version_must_be_stable_semver(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/forcing/variables.py",
        "# changed forcing\n",
    )
    digest = _write_snapshot(
        interface_repo,
        "forcing",
        "0.1.0",
        {
            "schema.json": {"type": "object"},
            "catalogue.json": {"variables": ["iy"]},
        },
    )
    forcing = (_record("0.1.0", None, "initial", digest),)
    _write(interface_repo, VERSION_PATH, _versions(forcing=forcing))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nForcing 0.1.0\n")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "pre-1.0" in capsys.readouterr().err


def test_first_interface_version_must_be_exactly_1_0_0(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/forcing/variables.py",
        "# complete forcing contract\n",
    )
    digest = _write_snapshot(
        interface_repo,
        "forcing",
        "2.7.9",
        {
            "schema.json": {"type": "object"},
            "catalogue.json": {"variables": ["iy"]},
        },
    )
    forcing = (_record("2.7.9", None, "initial", digest),)
    _write(interface_repo, VERSION_PATH, _versions(forcing=forcing))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nForcing 2.7.9\n")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "first public interface version must be '1.0.0'" in capsys.readouterr().err


@pytest.mark.parametrize(
    ("previous", "current"),
    [
        ((1, 0, 0), (1, 0, 2)),
        ((1, 0, 0), (1, 2, 0)),
        ((1, 0, 0), (2, 1, 0)),
        ((1, 2, 3), (2, 0, 1)),
    ],
)
def test_semver_transition_cannot_skip_or_mix_components(
    previous: tuple[int, int, int],
    current: tuple[int, int, int],
) -> None:
    with pytest.raises(audit.AuditFailure, match="increment exactly one"):
        audit._expected_change(previous, current)


def test_internal_data_model_change_does_not_require_any_interface_version(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/validation/controller.py",
        "# changed internal validation\n",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "no governed changes" in capsys.readouterr().out


def test_unpublished_untracked_source_does_not_force_premature_v1(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _write(
        interface_repo,
        "src/supy/data_model/forcing/new_contract.py",
        "# new untracked contract source\n",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "no governed changes" in capsys.readouterr().out


def test_name_status_keeps_both_sides_of_a_rename() -> None:
    output = (
        "R100\0src/supy/data_model/output/variables.py\0src/supy/internal_output.py\0"
    )

    assert audit._paths_from_name_status(output) == {
        "src/supy/data_model/output/variables.py",
        "src/supy/internal_output.py",
    }


def test_name_status_preserves_non_ascii_paths() -> None:
    output = "M\0src/supy/data_model/output/café.py\0"

    assert audit._paths_from_name_status(output) == {
        "src/supy/data_model/output/café.py"
    }


def test_unpublished_source_rename_does_not_force_premature_v1(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _git(
        interface_repo,
        "mv",
        "src/supy/data_model/output/variables.py",
        "src/supy/internal_output.py",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "no governed changes" in capsys.readouterr().out


def _reset_feature_on_output_v1(
    repo: Path,
) -> tuple[audit.ParsedRecord, dict[str, object]]:
    _git(repo, "checkout", "-q", "master")
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["Year"]},
    }
    digest = _write_snapshot(repo, "output", "1.0.0", artefacts)
    record = _record("1.0.0", None, "initial", digest)
    _write(repo, VERSION_PATH, _versions(output=(record,)))
    _write(repo, DOC_PATH, "Interface versions\n\nOutput 1.0.0\n")
    _write(repo, "src/supy/data_model/output/variables.py", "# output v1\n")
    _git(repo, "add", "-A")
    _git(repo, "commit", "-qm", "publish output v1")
    _git(repo, "checkout", "-qB", "feature")
    return record, artefacts


def _add_output_v1_1(
    repo: Path,
    first: audit.ParsedRecord,
) -> audit.ParsedRecord:
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["Year", "DOY"]},
    }
    digest = _write_snapshot(repo, "output", "1.1.0", artefacts)
    second = _record("1.1.0", "1.0.0", "additive", digest)
    _write(repo, VERSION_PATH, _versions(output=(first, second)))
    _write(repo, DOC_PATH, "Interface versions\n\nOutput 1.0.0\nOutput 1.1.0\n")
    _write(repo, "src/supy/data_model/output/variables.py", "# output v1.1\n")
    return second


def test_patch_release_cannot_hide_a_breaking_snapshot_change(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    first, _ = _reset_feature_on_output_v1(interface_repo)
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": [{"name": "Year"}]},
    }
    digest = _write_snapshot(interface_repo, "output", "1.0.1", artefacts)
    second = _record("1.0.1", "1.0.0", "correction", digest)
    _write(interface_repo, VERSION_PATH, _versions(output=(first, second)))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nOutput 1.0.1\n")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "at least breaking; declared correction understates it" in (
        capsys.readouterr().err
    )


def test_major_release_can_conservatively_overstate_machine_diff(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    first, _ = _reset_feature_on_output_v1(interface_repo)
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["Year", "DOY"]},
    }
    digest = _write_snapshot(interface_repo, "output", "2.0.0", artefacts)
    second = _record("2.0.0", "1.0.0", "breaking", digest)
    _write(interface_repo, VERSION_PATH, _versions(output=(first, second)))
    _write(interface_repo, DOC_PATH, "Interface versions\n\nOutput 2.0.0\n")

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert "output: '1.0.0' -> '2.0.0'" in capsys.readouterr().out


def test_released_snapshot_cannot_change_during_a_new_version_bump(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    first, _ = _reset_feature_on_output_v1(interface_repo)
    _add_output_v1_1(interface_repo, first)
    old_catalogue = (
        interface_repo
        / "src/supy/data_model/interfaces/artefacts/output/1.0.0/catalogue.json"
    )
    old_catalogue.write_text('{"rewritten":true}\n', encoding="utf-8")

    assert _run_worktree(interface_repo, monkeypatch) == 1
    captured = capsys.readouterr()
    assert "released snapshot 1.0.0 is immutable" in captured.err
    assert "catalogue.json" in captured.err


def test_released_snapshot_cannot_be_deleted_during_a_new_version_bump(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    first, _ = _reset_feature_on_output_v1(interface_repo)
    _add_output_v1_1(interface_repo, first)
    old_catalogue = (
        interface_repo
        / "src/supy/data_model/interfaces/artefacts/output/1.0.0/catalogue.json"
    )
    old_catalogue.unlink()

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "released snapshot 1.0.0 is immutable" in capsys.readouterr().err


def test_released_registry_record_is_append_only(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    first, _ = _reset_feature_on_output_v1(interface_repo)
    _add_output_v1_1(
        interface_repo,
        first._replace(summary="Rewritten historical summary."),
    )

    assert _run_worktree(interface_repo, monkeypatch) == 1
    assert "version registry is not append-only" in capsys.readouterr().err


def test_source_only_refactor_passes_when_parity_validator_still_matches(
    interface_repo: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _reset_feature_on_output_v1(interface_repo)
    _write(
        interface_repo,
        "src/supy/data_model/interfaces/canonical.py",
        "# serializer change\n",
    )

    assert _run_worktree(interface_repo, monkeypatch) == 0
    assert not capsys.readouterr().err


@pytest.mark.parametrize(
    ("old", "new", "expected"),
    [
        (
            {"variables": [{"name": "Year", "description": "old"}]},
            {"variables": [{"name": "Year", "description": "new"}]},
            "correction",
        ),
        (
            {"variables": [{"name": "Year"}]},
            {"variables": [{"name": "Year"}, {"name": "DOY"}]},
            "additive",
        ),
        (
            {"variables": [{"name": "Year"}, {"name": "DOY"}]},
            {"variables": [{"name": "Year"}]},
            "breaking",
        ),
        (
            {"variables": [{"name": "Year", "unit": "1"}]},
            {"variables": [{"name": "Year", "unit": "s"}]},
            "breaking",
        ),
        (
            {"type": "object", "required": ["Year"]},
            {"type": "object", "required": ["Year", "DOY"]},
            "breaking",
        ),
        (
            {"type": "object"},
            {"type": "object", "required": ["Year"]},
            "breaking",
        ),
        (
            {"type": "number"},
            {"type": "number", "minimum": 0},
            "breaking",
        ),
        (
            {"type": "array"},
            {"type": "array", "minItems": 1},
            "breaking",
        ),
        (
            {"type": "object"},
            {"type": "object", "additionalProperties": False},
            "breaking",
        ),
        (
            {
                "type": "object",
                "properties": {"Year": {"type": "integer"}},
            },
            {
                "type": "object",
                "properties": {
                    "Year": {"type": "integer"},
                    "DOY": {"type": "integer"},
                },
            },
            "additive",
        ),
        (
            {
                "type": "object",
                "properties": {"title": {"type": "string"}},
            },
            {
                "type": "object",
                "properties": {},
            },
            "breaking",
        ),
        (
            {
                "type": "object",
                "properties": {"title": "string"},
            },
            {
                "type": "object",
                "properties": {"title": "number"},
            },
            "breaking",
        ),
        (
            {"type": "string", "allOf": [{"maxLength": 20}]},
            {
                "type": "string",
                "allOf": [{"maxLength": 20}, {"minLength": 2}],
            },
            "breaking",
        ),
        (
            {"type": "array", "prefixItems": [{"type": "string"}]},
            {
                "type": "array",
                "prefixItems": [
                    {"type": "string"},
                    {"type": "number"},
                ],
            },
            "breaking",
        ),
        (
            {"type": "string", "enum": ["a"]},
            {"type": "string", "enum": ["a", "b"]},
            "additive",
        ),
    ],
)
def test_contract_json_compatibility_is_classified_conservatively(
    old: object,
    new: object,
    expected: str,
) -> None:
    assert audit._classify_json_change(old, new) == expected
