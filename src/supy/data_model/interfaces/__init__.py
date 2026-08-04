"""Governance primitives for SUEWS forcing and output data interfaces.

The forcing and output contracts are versioned independently from the YAML
configuration schema.  Contract definitions live in their respective data-model
packages; this package owns only their shared versioning, canonicalisation and
immutable-snapshot rules.
"""

from .canonical import canonical_json_bytes, sha256_digest
from .snapshots import (
    ARTEFACT_ROOT,
    SnapshotConflictError,
    SnapshotVerificationError,
    snapshot_manifest_digest,
    verify_all_snapshots,
    verify_snapshot,
    write_current_snapshot,
)
from .version import (
    CURRENT_FORCING_INTERFACE_VERSION,
    CURRENT_OUTPUT_INTERFACE_VERSION,
    FORCING_INTERFACE_VERSIONS,
    OUTPUT_INTERFACE_VERSIONS,
    InterfaceChange,
    InterfaceKind,
    InterfaceVersionRecord,
    current_interface_version,
    interface_version_record,
    interface_version_registry,
    validate_interface_version,
    validate_version_registry,
)

__all__ = [
    "ARTEFACT_ROOT",
    "CURRENT_FORCING_INTERFACE_VERSION",
    "CURRENT_OUTPUT_INTERFACE_VERSION",
    "FORCING_INTERFACE_VERSIONS",
    "OUTPUT_INTERFACE_VERSIONS",
    "InterfaceChange",
    "InterfaceKind",
    "InterfaceVersionRecord",
    "SnapshotConflictError",
    "SnapshotVerificationError",
    "canonical_json_bytes",
    "current_interface_version",
    "interface_version_record",
    "interface_version_registry",
    "sha256_digest",
    "snapshot_manifest_digest",
    "validate_interface_version",
    "validate_version_registry",
    "verify_all_snapshots",
    "verify_snapshot",
    "write_current_snapshot",
]
