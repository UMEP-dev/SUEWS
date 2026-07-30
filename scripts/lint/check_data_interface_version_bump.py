#!/usr/bin/env python3
"""Audit independent forcing/output interface histories and snapshots.

The forcing and output contracts are not part of the YAML configuration
schema. This guard enforces four invariants:

1. Governed contract changes move only the matching interface version.
2. Version registries are append-only and start at exactly ``1.0.0``.
3. Every registry record is bound to a canonical snapshot-manifest digest.
4. Released snapshot files can never be modified, renamed or deleted.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
from itertools import pairwise
import json
import os
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
from typing import Literal, NamedTuple

type InterfaceKind = Literal["forcing", "output"]


class ParsedRecord(NamedTuple):
    """Standard-library representation of ``InterfaceVersionRecord``."""

    version: str
    previous: str | None
    change: str
    summary: str
    manifest_digest: str


class GovernanceState(NamedTuple):
    """Base/worktree inputs after structural and history validation."""

    changed: list[str]
    base_versions: dict[InterfaceKind, str | None]
    base_registries: dict[InterfaceKind, tuple[ParsedRecord, ...]]
    current_versions: dict[InterfaceKind, str | None]
    current_registries: dict[InterfaceKind, tuple[ParsedRecord, ...]]


_VERSION_FILE = "src/supy/data_model/interfaces/version.py"
_VERSION_DOC = "docs/source/contributing/schema/data_interface_versioning.rst"
_MESON_FILE = Path("src/supy/meson.build")
_ARTEFACT_ROOT = Path("src/supy/data_model/interfaces/artefacts")
_VERSION_CONSTANTS: dict[InterfaceKind, str] = {
    "forcing": "CURRENT_FORCING_INTERFACE_VERSION",
    "output": "CURRENT_OUTPUT_INTERFACE_VERSION",
}
_REGISTRY_CONSTANTS: dict[InterfaceKind, str] = {
    "forcing": "FORCING_INTERFACE_VERSIONS",
    "output": "OUTPUT_INTERFACE_VERSIONS",
}
_VALIDATOR_PATHS: dict[InterfaceKind, str] = {
    "forcing": "scripts/lint/check_forcing_interface_contract.py",
    "output": "scripts/lint/check_output_interface_contract.py",
}
_EXPECTED_ARTEFACTS: frozenset[str] = frozenset({
    "catalogue.json",
    "schema.json",
})
_SEMVER_PATTERN = re.compile(r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$")
_DIGEST_PATTERN = re.compile(r"^sha256:[0-9a-f]{64}$")
_ANNOTATION_KEYS: frozenset[str] = frozenset({
    "$comment",
    "deprecated",
    "description",
    "examples",
    "title",
})
_ADDITIVE_MAP_PATHS: frozenset[str] = frozenset({
    "$defs",
    "definitions",
    "properties",
})
_ADDITIVE_LIST_PATHS: frozenset[str] = frozenset({
    "enum",
    "variables",
})
_CHANGE_SEVERITY = {
    "correction": 1,
    "additive": 2,
    "breaking": 3,
}
_ARTEFACT_INSTALL_PATTERN = re.compile(
    r"install_subdir\(\s*"
    r"['\"]data_model/interfaces/artefacts['\"]\s*,\s*"
    r"install_dir\s*:\s*py\.get_install_dir\(\)\s*/\s*"
    r"['\"]supy/data_model/interfaces['\"]\s*,?\s*\)",
    re.MULTILINE,
)


class AuditFailure(RuntimeError):
    """Raised for a user-correctable governance violation."""


def _run(args: list[str]) -> str:
    result = subprocess.run(
        args,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _merge_base(base_ref: str) -> str:
    return _run(["git", "merge-base", base_ref, "HEAD"]).strip()


def _paths_from_name_status(output: str) -> set[str]:
    """Return both paths from NUL-delimited ``git --name-status -z`` output."""
    paths: set[str] = set()
    fields = output.split("\0")
    if fields and not fields[-1]:
        fields.pop()
    index = 0
    while index < len(fields):
        status = fields[index]
        path_count = 2 if status.startswith(("R", "C")) else 1
        record_end = index + 1 + path_count
        if not status or record_end > len(fields):
            raise ValueError("cannot parse NUL-delimited git name-status output")
        paths.update(fields[index + 1 : record_end])
        index = record_end
    return paths


def _list_changed_files(base_ref: str, include_worktree: bool) -> list[str]:
    comparison = _merge_base(base_ref) if include_worktree else f"{base_ref}...HEAD"
    output = _run([
        "git",
        "diff",
        "--name-status",
        "-z",
        "--find-renames",
        comparison,
    ])
    paths = _paths_from_name_status(output)
    if include_worktree:
        paths.update(
            path
            for path in _run([
                "git",
                "ls-files",
                "--others",
                "--exclude-standard",
                "-z",
            ]).split("\0")
            if path
        )
    return sorted(paths)


def _assignment(tree: ast.Module, name: str) -> ast.expr:
    for node in tree.body:
        if isinstance(node, ast.Assign):
            if any(
                isinstance(target, ast.Name) and target.id == name
                for target in node.targets
            ):
                return node.value
        elif (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id == name
            and node.value is not None
        ):
            return node.value
    raise ValueError(f"could not read {name} from {_VERSION_FILE}")


def _extract_version(tree: ast.Module, constant: str) -> str | None:
    value = ast.literal_eval(_assignment(tree, constant))
    if value is not None and not isinstance(value, str):
        raise ValueError(f"{constant} must be a string or None")
    return value


def _call_argument(
    call: ast.Call,
    position: int,
    name: str,
) -> ast.expr:
    if position < len(call.args):
        return call.args[position]
    for keyword in call.keywords:
        if keyword.arg == name:
            return keyword.value
    raise ValueError(f"InterfaceVersionRecord is missing {name!r}")


def _extract_change(node: ast.expr) -> str:
    if (
        isinstance(node, ast.Attribute)
        and isinstance(node.value, ast.Name)
        and node.value.id == "InterfaceChange"
    ):
        return node.attr.lower()
    value = ast.literal_eval(node)
    if not isinstance(value, str):
        raise ValueError("InterfaceVersionRecord.change must be an InterfaceChange")
    return value


def _extract_record(node: ast.expr) -> ParsedRecord:
    if not isinstance(node, ast.Call):
        raise ValueError(
            "interface registries must contain InterfaceVersionRecord calls"
        )
    fields = ("version", "previous", "change", "summary", "manifest_digest")
    values = [
        _call_argument(node, position, name) for position, name in enumerate(fields)
    ]
    version = ast.literal_eval(values[0])
    previous = ast.literal_eval(values[1])
    change = _extract_change(values[2])
    summary = ast.literal_eval(values[3])
    manifest_digest = ast.literal_eval(values[4])
    if (
        not isinstance(version, str)
        or (previous is not None and not isinstance(previous, str))
        or not isinstance(summary, str)
        or not isinstance(manifest_digest, str)
    ):
        raise ValueError("InterfaceVersionRecord contains invalid literal values")
    return ParsedRecord(version, previous, change, summary, manifest_digest)


def _extract_registry(tree: ast.Module, constant: str) -> tuple[ParsedRecord, ...]:
    value = _assignment(tree, constant)
    if not isinstance(value, (ast.Tuple, ast.List)):
        raise ValueError(f"{constant} must be a tuple or list literal")
    return tuple(_extract_record(node) for node in value.elts)


def _extract_governance(
    source: str,
) -> tuple[
    dict[InterfaceKind, str | None],
    dict[InterfaceKind, tuple[ParsedRecord, ...]],
]:
    tree = ast.parse(source)
    versions = {
        kind: _extract_version(tree, constant)
        for kind, constant in _VERSION_CONSTANTS.items()
    }
    registries = {
        kind: _extract_registry(tree, constant)
        for kind, constant in _REGISTRY_CONSTANTS.items()
    }
    return versions, registries


def _load_governance_at_base(
    base_ref: str,
) -> tuple[
    dict[InterfaceKind, str | None],
    dict[InterfaceKind, tuple[ParsedRecord, ...]],
]:
    try:
        source = _run(["git", "show", f"{_merge_base(base_ref)}:{_VERSION_FILE}"])
    except subprocess.CalledProcessError:
        return (
            {"forcing": None, "output": None},
            {"forcing": (), "output": ()},
        )
    return _extract_governance(source)


def _load_governance_worktree() -> tuple[
    dict[InterfaceKind, str | None],
    dict[InterfaceKind, tuple[ParsedRecord, ...]],
]:
    return _extract_governance(Path(_VERSION_FILE).read_text(encoding="utf-8"))


def _parse_semver(version: str) -> tuple[int, int, int]:
    match = _SEMVER_PATTERN.fullmatch(version)
    if match is None:
        raise AuditFailure(
            f"interface version {version!r} must use stable MAJOR.MINOR.PATCH SemVer"
        )
    parsed = tuple(int(part) for part in match.groups())
    if parsed[0] == 0:
        raise AuditFailure(
            f"interface version {version!r} is pre-1.0; register only complete "
            "public contracts"
        )
    return parsed


def _expected_change(
    previous: tuple[int, int, int],
    current: tuple[int, int, int],
) -> str:
    major, minor, patch = previous
    if current == (major, minor, patch + 1):
        return "correction"
    if current == (major, minor + 1, 0):
        return "additive"
    if current == (major + 1, 0, 0):
        return "breaking"
    raise AuditFailure(
        "interface versions must increment exactly one SemVer component "
        "and reset lower components"
    )


def _validate_registry(
    kind: InterfaceKind,
    records: tuple[ParsedRecord, ...],
    current: str | None,
) -> None:
    if not records:
        if current is not None:
            raise AuditFailure(
                f"{kind}: an empty registry cannot have a current version"
            )
        return
    if current is None:
        raise AuditFailure(f"{kind}: a non-empty registry needs a current version")

    previous: ParsedRecord | None = None
    previous_semver: tuple[int, int, int] | None = None
    seen: set[str] = set()
    for record in records:
        parsed = _parse_semver(record.version)
        if record.version in seen:
            raise AuditFailure(f"{kind}: duplicate version {record.version!r}")
        if not record.summary.strip():
            raise AuditFailure(f"{kind}: version {record.version!r} needs a summary")
        if _DIGEST_PATTERN.fullmatch(record.manifest_digest) is None:
            raise AuditFailure(
                f"{kind}: version {record.version!r} needs a sha256 manifest digest"
            )
        if previous is None:
            if record.version != "1.0.0":
                raise AuditFailure(
                    f"{kind}: the first public interface version must be '1.0.0'"
                )
            if record.previous is not None or record.change != "initial":
                raise AuditFailure(
                    f"{kind}: the first record must be initial with no predecessor"
                )
        else:
            if record.previous != previous.version:
                raise AuditFailure(
                    f"{kind}: {record.version!r} must follow {previous.version!r}"
                )
            assert previous_semver is not None
            expected = _expected_change(previous_semver, parsed)
            if record.change != expected:
                raise AuditFailure(
                    f"{kind}: {previous.version} -> {record.version} requires "
                    f"change={expected!r}"
                )
        seen.add(record.version)
        previous = record
        previous_semver = parsed

    if records[-1].version != current:
        raise AuditFailure(
            f"{kind}: current version {current!r} must be the last registry entry"
        )


def _canonical_json_bytes(value: object) -> bytes:
    text = json.dumps(
        value,
        allow_nan=False,
        ensure_ascii=False,
        separators=(",", ":"),
        sort_keys=True,
    )
    return f"{text}\n".encode()


def _sha256_digest(content: bytes) -> str:
    return f"sha256:{hashlib.sha256(content).hexdigest()}"


def _safe_relative_name(name: str) -> PurePosixPath:
    path = PurePosixPath(name)
    if (
        not name
        or path.is_absolute()
        or "\\" in name
        or any(part in {"", ".", ".."} for part in path.parts)
        or path.name == "manifest.json"
    ):
        raise AuditFailure(f"unsafe snapshot artefact path: {name!r}")
    return path


def _verify_snapshot(
    kind: InterfaceKind,
    record: ParsedRecord,
) -> None:
    snapshot_dir = _ARTEFACT_ROOT / kind / record.version
    manifest_path = snapshot_dir / "manifest.json"
    try:
        manifest_content = manifest_path.read_bytes()
        payload = json.loads(manifest_content)
    except (FileNotFoundError, json.JSONDecodeError, UnicodeDecodeError) as exc:
        raise AuditFailure(f"cannot read snapshot manifest: {manifest_path}") from exc

    if _canonical_json_bytes(payload) != manifest_content:
        raise AuditFailure(f"snapshot manifest is not canonical JSON: {manifest_path}")
    if (
        payload.get("format_version") != 1
        or payload.get("interface") != kind
        or payload.get("interface_version") != record.version
    ):
        raise AuditFailure(f"snapshot identity mismatch: {manifest_path}")
    if _sha256_digest(manifest_content) != record.manifest_digest:
        raise AuditFailure(
            f"snapshot manifest digest differs from the registry: {manifest_path}"
        )

    entries = payload.get("artefacts")
    if not isinstance(entries, list):
        raise AuditFailure(f"snapshot manifest has no artefact list: {manifest_path}")

    declared: set[str] = set()
    for entry in entries:
        if not isinstance(entry, dict):
            raise AuditFailure(f"malformed snapshot entry in {manifest_path}")
        try:
            relative = _safe_relative_name(entry["path"]).as_posix()
            expected_size = entry["bytes"]
            expected_digest = entry["digest"]
        except (KeyError, TypeError, AuditFailure) as exc:
            raise AuditFailure(f"malformed snapshot entry in {manifest_path}") from exc
        if relative in declared:
            raise AuditFailure(f"duplicate snapshot entry {relative!r}")
        declared.add(relative)
        path = snapshot_dir / relative
        try:
            content = path.read_bytes()
        except FileNotFoundError as exc:
            raise AuditFailure(f"snapshot artefact is missing: {path}") from exc
        if len(content) != expected_size or _sha256_digest(content) != expected_digest:
            raise AuditFailure(f"snapshot artefact digest mismatch: {path}")

    present = {
        path.relative_to(snapshot_dir).as_posix()
        for path in snapshot_dir.rglob("*")
        if path.is_file() and path != manifest_path
    }
    if present != declared:
        raise AuditFailure(
            f"snapshot contains undeclared or missing artefacts: {snapshot_dir}"
        )
    if declared != _EXPECTED_ARTEFACTS:
        raise AuditFailure(
            f"snapshot must contain exactly {sorted(_EXPECTED_ARTEFACTS)}: "
            f"{snapshot_dir}"
        )


def _verify_snapshot_tree(
    registries: dict[InterfaceKind, tuple[ParsedRecord, ...]],
) -> None:
    for kind in ("forcing", "output"):
        expected = {record.version for record in registries[kind]}
        kind_root = _ARTEFACT_ROOT / kind
        present = (
            {path.name for path in kind_root.iterdir() if path.is_dir()}
            if kind_root.exists()
            else set()
        )
        if present != expected:
            raise AuditFailure(
                f"{kind}: snapshot directories {sorted(present)} do not match "
                f"registered versions {sorted(expected)}"
            )
        for record in registries[kind]:
            _verify_snapshot(kind, record)


def _merge_compatibility(left: str, right: str) -> str:
    return max((left, right), key=_CHANGE_SEVERITY.__getitem__)


def _is_annotation_key(path: tuple[str, ...], key: str) -> bool:
    """Distinguish schema annotations from fields named like annotations."""
    return key in _ANNOTATION_KEYS and (not path or path[-1] not in _ADDITIVE_MAP_PATHS)


def _classify_json_change(  # ruff: ignore[too-many-return-statements]
    old: object,
    new: object,
    path: tuple[str, ...] = (),
) -> str:
    """Conservatively classify canonical contract JSON compatibility."""
    if old == new:
        return "correction"
    if path and _is_annotation_key(path[:-1], path[-1]):
        return "correction"
    if type(old) is not type(new):
        return "breaking"

    if isinstance(old, dict) and isinstance(new, dict):
        removed = old.keys() - new.keys()
        if any(not _is_annotation_key(path, key) for key in removed):
            return "breaking"
        classification = "correction"
        added_contract_keys = {
            key for key in new.keys() - old.keys() if not _is_annotation_key(path, key)
        }
        if added_contract_keys:
            if not path or path[-1] not in _ADDITIVE_MAP_PATHS:
                return "breaking"
            classification = "additive"
        for key in old.keys() & new.keys():
            classification = _merge_compatibility(
                classification,
                _classify_json_change(old[key], new[key], (*path, key)),
            )
        return classification

    if isinstance(old, list) and isinstance(new, list):
        if path and path[-1] == "required":
            return "breaking" if set(new) - set(old) else "additive"

        if path and path[-1] in _ADDITIVE_LIST_PATHS and len(new) >= len(old):
            classification = "additive" if len(new) > len(old) else "correction"
            for index, old_item in enumerate(old):
                classification = _merge_compatibility(
                    classification,
                    _classify_json_change(
                        old_item,
                        new[index],
                        (*path, str(index)),
                    ),
                )
            return classification
        return "breaking"

    return "breaking"


def _load_snapshot_artefact(
    kind: InterfaceKind,
    version: str,
    name: str,
) -> object:
    path = _ARTEFACT_ROOT / kind / version / name
    return json.loads(path.read_bytes())


def _classify_snapshot_change(
    kind: InterfaceKind,
    previous: str,
    current: str,
) -> str:
    classification = "correction"
    for name in sorted(_EXPECTED_ARTEFACTS):
        classification = _merge_compatibility(
            classification,
            _classify_json_change(
                _load_snapshot_artefact(kind, previous, name),
                _load_snapshot_artefact(kind, current, name),
            ),
        )
    return classification


def _validate_snapshot_compatibility(
    kind: InterfaceKind,
    records: tuple[ParsedRecord, ...],
) -> None:
    for previous, current in pairwise(records):
        inferred = _classify_snapshot_change(
            kind,
            previous.version,
            current.version,
        )
        if _CHANGE_SEVERITY[current.change] < _CHANGE_SEVERITY[inferred]:
            raise AuditFailure(
                f"{kind}: snapshot {previous.version} -> {current.version} is at "
                f"least {inferred}; declared {current.change} understates it"
            )


def _validate_packaging_rule() -> None:
    """Ensure every present and future snapshot directory is installed."""
    source = _MESON_FILE.read_text(encoding="utf-8")
    if _ARTEFACT_INSTALL_PATTERN.search(source) is None:
        raise AuditFailure(
            f"{_MESON_FILE} must recursively install data-interface artefacts "
            "under supy/data_model/interfaces"
        )


def _validate_contract_implementation(
    kind: InterfaceKind,
    current: str | None,
) -> None:
    """Require each published contract to supply and pass its parity validator."""
    if current is None:
        return
    validator = Path(_VALIDATOR_PATHS[kind])
    if not validator.is_file():
        raise AuditFailure(
            f"{kind}: publishing {current} requires parity validator {validator}"
        )
    result = subprocess.run(
        [
            sys.executable,
            str(validator),
            "--snapshot-root",
            str(_ARTEFACT_ROOT),
            "--version",
            current,
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip() or "no diagnostic"
        raise AuditFailure(f"{kind}: implementation-parity validator failed: {detail}")


def _assert_append_only(
    kind: InterfaceKind,
    old_records: tuple[ParsedRecord, ...],
    new_records: tuple[ParsedRecord, ...],
    changed: list[str],
) -> None:
    if (
        len(new_records) < len(old_records)
        or new_records[: len(old_records)] != old_records
    ):
        raise AuditFailure(f"{kind}: the version registry is not append-only")

    for record in old_records:
        prefix = f"src/supy/data_model/interfaces/artefacts/{kind}/{record.version}/"
        historical_changes = [path for path in changed if path.startswith(prefix)]
        if historical_changes:
            joined = "\n".join(f"  - {path}" for path in historical_changes)
            raise AuditFailure(
                f"{kind}: released snapshot {record.version} is immutable; "
                f"these paths changed:\n{joined}"
            )


def _report_failure(reason: str) -> None:
    print(
        "[data-interface-version-audit] FAILED\n\n"
        f"{reason}\n\n"
        f"Update {_VERSION_FILE}, {_VERSION_DOC}, and the canonical append-only "
        f"snapshot under {_ARTEFACT_ROOT} as one compatible change.",
        file=sys.stderr,
    )


def _load_validated_state(
    base_ref: str,
    include_worktree: bool,
) -> GovernanceState:
    changed = _list_changed_files(base_ref, include_worktree)
    base_versions, base_registries = _load_governance_at_base(base_ref)
    current_versions, current_registries = _load_governance_worktree()

    for kind in ("forcing", "output"):
        _validate_registry(kind, current_registries[kind], current_versions[kind])
        _assert_append_only(
            kind,
            base_registries[kind],
            current_registries[kind],
            changed,
        )
    _verify_snapshot_tree(current_registries)
    _validate_packaging_rule()
    for kind in ("forcing", "output"):
        _validate_snapshot_compatibility(kind, current_registries[kind])
        _validate_contract_implementation(kind, current_versions[kind])
    return GovernanceState(
        changed,
        base_versions,
        base_registries,
        current_versions,
        current_registries,
    )


def _validate_source_version_coupling(
    state: GovernanceState,
) -> bool:
    """Validate path/version/docs coupling and return whether content moved."""
    docs_changed = _VERSION_DOC in state.changed
    any_governed = False
    for kind in ("forcing", "output"):
        old = state.base_versions[kind]
        new = state.current_versions[kind]
        snapshot_prefix = f"src/supy/data_model/interfaces/artefacts/{kind}/"
        governed = sorted(
            path
            for path in state.changed
            if path.startswith(snapshot_prefix)
            and PurePosixPath(path).name != "README.md"
        )
        content_changed = bool(governed)
        version_changed = old != new
        registry_changed = state.base_registries[kind] != state.current_registries[kind]
        any_governed = any_governed or content_changed

        if registry_changed != version_changed:
            raise AuditFailure(
                f"{kind}: registry and current-version changes must occur together"
            )
        if version_changed and (
            len(state.current_registries[kind]) != len(state.base_registries[kind]) + 1
        ):
            raise AuditFailure(
                f"{kind}: each version bump must append exactly one record"
            )
        if content_changed != version_changed:
            detail = "\n".join(f"  - {path}" for path in governed) or "  - <none>"
            raise AuditFailure(
                f"{kind}: governed content change={content_changed} but "
                f"version change={version_changed}:\n{detail}"
            )
        if version_changed and not docs_changed:
            raise AuditFailure(
                f"{kind}: {old!r} -> {new!r} requires an update to {_VERSION_DOC}"
            )
        if version_changed:
            print(
                f"[data-interface-version-audit] {kind}: "
                f"{old!r} -> {new!r}; append-only registry, snapshot and docs confirmed."
            )
    return any_governed


def main(
    argv: list[str] | None = None,
) -> int:
    """Run registry, history, snapshot and source/version consistency checks."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        default="origin/master",
        help="Git ref to compare against (default: origin/master).",
    )
    parser.add_argument(
        "--include-worktree",
        action="store_true",
        help="Include staged, unstaged and untracked changes.",
    )
    args = parser.parse_args(argv)

    try:
        state = _load_validated_state(args.base, args.include_worktree)
        any_governed = _validate_source_version_coupling(state)
    except AuditFailure as exc:
        _report_failure(str(exc))
        return 1
    except (OSError, SyntaxError, subprocess.CalledProcessError, ValueError) as exc:
        print(f"[data-interface-version-audit] ERROR: {exc}", file=sys.stderr)
        return 2

    if (
        not any_governed
        and state.base_versions == state.current_versions
        and state.base_registries == state.current_registries
    ):
        print("[data-interface-version-audit] no governed changes, skipping.")
    return 0


if __name__ == "__main__":
    repo_root = Path(__file__).resolve().parent.parent.parent
    os.chdir(repo_root)
    sys.exit(main())
