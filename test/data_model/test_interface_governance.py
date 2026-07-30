"""Tests for independent forcing/output interface governance."""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from supy.data_model.interfaces import (
    CURRENT_FORCING_INTERFACE_VERSION,
    CURRENT_OUTPUT_INTERFACE_VERSION,
    FORCING_INTERFACE_VERSIONS,
    OUTPUT_INTERFACE_VERSIONS,
    InterfaceChange,
    InterfaceVersionRecord,
    SnapshotConflictError,
    SnapshotVerificationError,
    canonical_json_bytes,
    sha256_digest,
    snapshot_manifest_digest,
    validate_version_registry,
    verify_all_snapshots,
    verify_snapshot,
    write_current_snapshot,
)
import supy.data_model.interfaces.snapshots as snapshots_module

pytestmark = pytest.mark.api
_DIGEST = f"sha256:{'0' * 64}"


def test_contracts_are_not_versioned_before_their_first_complete_release() -> None:
    """Governance must not present incomplete internal metadata as public v1."""

    assert CURRENT_FORCING_INTERFACE_VERSION is None
    assert CURRENT_OUTPUT_INTERFACE_VERSION is None
    assert FORCING_INTERFACE_VERSIONS == ()
    assert OUTPUT_INTERFACE_VERSIONS == ()


def test_packaged_snapshot_root_matches_the_empty_registries() -> None:
    verify_all_snapshots()


def test_version_registry_accepts_semver_compatible_lineage() -> None:
    records = (
        InterfaceVersionRecord(
            version="1.0.0",
            previous=None,
            change=InterfaceChange.INITIAL,
            summary="Initial contract.",
            manifest_digest=_DIGEST,
        ),
        InterfaceVersionRecord(
            version="1.1.0",
            previous="1.0.0",
            change=InterfaceChange.ADDITIVE,
            summary="Add an optional field.",
            manifest_digest=_DIGEST,
        ),
        InterfaceVersionRecord(
            version="1.1.1",
            previous="1.1.0",
            change=InterfaceChange.CORRECTION,
            summary="Correct metadata without changing its shape.",
            manifest_digest=_DIGEST,
        ),
        InterfaceVersionRecord(
            version="2.0.0",
            previous="1.1.1",
            change=InterfaceChange.BREAKING,
            summary="Remove a public field.",
            manifest_digest=_DIGEST,
        ),
    )

    validate_version_registry(records, "2.0.0")


@pytest.mark.parametrize(
    ("records", "current", "message"),
    [
        (
            (
                InterfaceVersionRecord(
                    "0.1.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Provisional.",
                    _DIGEST,
                ),
            ),
            "0.1.0",
            "pre-1.0",
        ),
        (
            (
                InterfaceVersionRecord(
                    "2.0.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Skipped initial version.",
                    _DIGEST,
                ),
            ),
            "2.0.0",
            "first public interface version must be '1.0.0'",
        ),
        (
            (
                InterfaceVersionRecord(
                    "1.0.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Initial.",
                    _DIGEST,
                ),
                InterfaceVersionRecord(
                    "1.1.0",
                    "1.0.0",
                    InterfaceChange.BREAKING,
                    "Wrong label.",
                    _DIGEST,
                ),
            ),
            "1.1.0",
            "requires change='additive'",
        ),
        (
            (
                InterfaceVersionRecord(
                    "1.0.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Initial.",
                    _DIGEST,
                ),
            ),
            "1.1.0",
            "must be the last registry entry",
        ),
        (
            (
                InterfaceVersionRecord(
                    "1.0.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Initial.",
                    _DIGEST,
                ),
                InterfaceVersionRecord(
                    "1.4.0",
                    "1.0.0",
                    InterfaceChange.ADDITIVE,
                    "Skipped minor versions.",
                    _DIGEST,
                ),
            ),
            "1.4.0",
            "increment exactly one SemVer component",
        ),
        (
            (
                InterfaceVersionRecord(
                    "1.0.0",
                    None,
                    InterfaceChange.INITIAL,
                    "Initial.",
                    _DIGEST,
                ),
                InterfaceVersionRecord(
                    "2.1.0",
                    "1.0.0",
                    InterfaceChange.BREAKING,
                    "Failed to reset minor.",
                    _DIGEST,
                ),
            ),
            "2.1.0",
            "increment exactly one SemVer component",
        ),
    ],
)
def test_version_registry_rejects_invalid_lineage(
    records: tuple[InterfaceVersionRecord, ...],
    current: str,
    message: str,
) -> None:
    with pytest.raises(ValueError, match=message):
        validate_version_registry(records, current)


def test_canonical_json_is_stable_and_rejects_non_json_numbers() -> None:
    left = {"z": ["é", 2], "a": {"b": True, "a": None}}
    right = {"a": {"a": None, "b": True}, "z": ["é", 2]}

    expected = b'{"a":{"a":null,"b":true},"z":["\xc3\xa9",2]}\n'
    expected_digest = (
        "sha256:e00d3b199b685df229701bca3ce06aa7dbc2773d29d20721c29e34e7c4090bf5"
    )

    assert canonical_json_bytes(left) == expected
    assert canonical_json_bytes(right) == expected
    assert sha256_digest(expected) == expected_digest
    with pytest.raises(ValueError):
        canonical_json_bytes({"invalid": math.nan})


def _install_snapshot_registry(
    monkeypatch: pytest.MonkeyPatch,
    kind: str,
    versions: tuple[
        tuple[str, str | None, InterfaceChange, dict[str, object]],
        ...,
    ],
) -> tuple[InterfaceVersionRecord, ...]:
    records = tuple(
        InterfaceVersionRecord(
            version=version,
            previous=previous,
            change=change,
            summary=f"Test {version}.",
            manifest_digest=snapshot_manifest_digest(kind, version, artefacts),
        )
        for version, previous, change, artefacts in versions
    )

    def registry(requested_kind: str) -> tuple[InterfaceVersionRecord, ...]:
        return records if requested_kind == kind else ()

    def current(requested_kind: str) -> str | None:
        return records[-1].version if requested_kind == kind else None

    def record(requested_kind: str, version: str) -> InterfaceVersionRecord:
        if requested_kind == kind:
            for candidate in records:
                if candidate.version == version:
                    return candidate
        raise ValueError("not registered")

    monkeypatch.setattr(snapshots_module, "interface_version_registry", registry)
    monkeypatch.setattr(snapshots_module, "current_interface_version", current)
    monkeypatch.setattr(snapshots_module, "interface_version_record", record)
    return records


def test_snapshot_is_idempotent_but_cannot_be_redefined(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": [{"name": "Year"}]},
    }
    _install_snapshot_registry(
        monkeypatch,
        "output",
        (("1.0.0", None, InterfaceChange.INITIAL, artefacts),),
    )
    manifest = write_current_snapshot("output", artefacts, tmp_path)

    assert write_current_snapshot("output", artefacts, tmp_path) == manifest
    verify_snapshot(manifest)

    changed = dict(artefacts)
    changed["catalogue.json"] = {"variables": [{"name": "DOY"}]}
    with pytest.raises(SnapshotConflictError, match="registered digest"):
        write_current_snapshot("output", changed, tmp_path)

    assert b'"Year"' in (manifest.parent / "catalogue.json").read_bytes()


def test_snapshot_retains_history_and_detects_tampering(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    first_artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["iy", "id"]},
    }
    second_artefacts = {
        "schema.json": {"type": "object"},
        "catalogue.json": {"variables": ["iy", "id", "it"]},
    }
    _install_snapshot_registry(
        monkeypatch,
        "forcing",
        (("1.0.0", None, InterfaceChange.INITIAL, first_artefacts),),
    )
    first = write_current_snapshot("forcing", first_artefacts, tmp_path)

    _install_snapshot_registry(
        monkeypatch,
        "forcing",
        (
            ("1.0.0", None, InterfaceChange.INITIAL, first_artefacts),
            (
                "1.1.0",
                "1.0.0",
                InterfaceChange.ADDITIVE,
                second_artefacts,
            ),
        ),
    )
    second = write_current_snapshot("forcing", second_artefacts, tmp_path)

    assert first.exists()
    assert second.exists()
    verify_snapshot(first)
    verify_snapshot(second)
    verify_all_snapshots(tmp_path)

    (second.parent / "catalogue.json").write_text("changed\n", encoding="utf-8")
    with pytest.raises(SnapshotVerificationError, match="does not match"):
        verify_snapshot(second)
    verify_snapshot(first)


@pytest.mark.parametrize(
    "name",
    ["../schema.json", "/schema.json", r"group\schema.json", "manifest.json"],
)
def test_snapshot_rejects_unsafe_or_reserved_paths(
    name: str,
) -> None:
    with pytest.raises(ValueError):
        snapshot_manifest_digest("output", "1.0.0", {name: {}})


def test_snapshot_rejects_non_public_version() -> None:
    with pytest.raises(ValueError, match=r"pre-1\.0"):
        snapshot_manifest_digest(
            "output",
            "0.1.0",
            {"schema.json": {}, "catalogue.json": {}},
        )


def test_snapshot_writer_rejects_unregistered_interface(tmp_path: Path) -> None:
    with pytest.raises(ValueError, match="no registered public interface"):
        write_current_snapshot(
            "output",
            {"schema.json": {}, "catalogue.json": {}},
            tmp_path,
        )
