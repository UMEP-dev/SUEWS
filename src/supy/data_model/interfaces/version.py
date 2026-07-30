"""Independent version registries for forcing and output contracts.

The YAML configuration schema describes configuration structure.  It does not
own either data interface, so its ``CURRENT_SCHEMA_VERSION`` must never be used
as the identity of a forcing or output artefact.

No external contract is registered here yet.  The forcing and output registry
issues will each add their first ``1.0.0`` record when the corresponding
contract is complete.  Until then, a change to governed contract content is
rejected by the data-interface audit unless it introduces that first record.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import StrEnum
import re
from typing import Literal

type InterfaceKind = Literal["forcing", "output"]


class InterfaceChange(StrEnum):
    """Compatibility class attached to an interface-version record."""

    INITIAL = "initial"
    CORRECTION = "correction"
    ADDITIVE = "additive"
    BREAKING = "breaking"


@dataclass(frozen=True, slots=True)
class InterfaceVersionRecord:
    """One immutable entry in an interface's version lineage."""

    version: str
    previous: str | None
    change: InterfaceChange
    summary: str
    manifest_digest: str


# The first complete forcing and output contracts will register their own
# independent 1.0.0 records in #1655 and #1656 respectively.  Keeping these
# values as None avoids presenting today's incomplete internal metadata as a
# published external contract.
CURRENT_FORCING_INTERFACE_VERSION: str | None = None
CURRENT_OUTPUT_INTERFACE_VERSION: str | None = None

FORCING_INTERFACE_VERSIONS: tuple[InterfaceVersionRecord, ...] = ()
OUTPUT_INTERFACE_VERSIONS: tuple[InterfaceVersionRecord, ...] = ()

_SEMVER_PATTERN = re.compile(r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$")
_DIGEST_PATTERN = re.compile(r"^sha256:[0-9a-f]{64}$")


def _parse_semver(version: str) -> tuple[int, int, int]:
    """Parse the stable SemVer subset used for public interface contracts."""
    match = _SEMVER_PATTERN.fullmatch(version)
    if match is None:
        raise ValueError(
            f"interface version {version!r} must use stable MAJOR.MINOR.PATCH SemVer"
        )
    parsed = tuple(int(part) for part in match.groups())
    if parsed[0] == 0:
        raise ValueError(
            f"interface version {version!r} is pre-1.0; register only complete "
            "public contracts"
        )
    return parsed


def validate_interface_version(version: str) -> None:
    """Raise if ``version`` is not a stable, public SemVer identifier."""
    _parse_semver(version)


def _expected_change(
    previous: tuple[int, int, int],
    current: tuple[int, int, int],
) -> InterfaceChange:
    """Return the compatibility class implied by a SemVer transition."""
    major, minor, patch = previous
    if current == (major, minor, patch + 1):
        return InterfaceChange.CORRECTION
    if current == (major, minor + 1, 0):
        return InterfaceChange.ADDITIVE
    if current == (major + 1, 0, 0):
        return InterfaceChange.BREAKING
    raise ValueError(
        "interface versions must increment exactly one SemVer component "
        "and reset lower components"
    )


def validate_version_registry(
    records: tuple[InterfaceVersionRecord, ...],
    current: str | None,
) -> None:
    """Validate lineage, compatibility labels and current-version ownership."""
    if not records:
        if current is not None:
            raise ValueError(
                "an empty interface registry cannot have a current version"
            )
        return

    if current is None:
        raise ValueError("a non-empty interface registry must have a current version")

    seen: set[str] = set()
    previous_record: InterfaceVersionRecord | None = None
    previous_version: tuple[int, int, int] | None = None

    for record in records:
        parsed = _parse_semver(record.version)
        if record.version in seen:
            raise ValueError(f"duplicate interface version {record.version!r}")
        if not record.summary.strip():
            raise ValueError(f"interface version {record.version!r} needs a summary")
        if _DIGEST_PATTERN.fullmatch(record.manifest_digest) is None:
            raise ValueError(
                f"interface version {record.version!r} needs a canonical "
                "sha256 manifest digest"
            )

        if previous_record is None:
            if record.version != "1.0.0":
                raise ValueError("the first public interface version must be '1.0.0'")
            if (
                record.previous is not None
                or record.change is not InterfaceChange.INITIAL
            ):
                raise ValueError(
                    "the first interface version must be an initial record "
                    "without a previous version"
                )
        else:
            if record.previous != previous_record.version:
                raise ValueError(
                    f"interface version {record.version!r} must name "
                    f"{previous_record.version!r} as its previous version"
                )
            assert previous_version is not None
            expected = _expected_change(previous_version, parsed)
            if record.change is not expected:
                raise ValueError(
                    f"{previous_record.version} -> {record.version} requires "
                    f"change={expected.value!r}, not {record.change.value!r}"
                )

        seen.add(record.version)
        previous_record = record
        previous_version = parsed

    if current != records[-1].version:
        raise ValueError(
            f"current interface version {current!r} must be the last registry entry "
            f"({records[-1].version!r})"
        )


def interface_version_registry(
    kind: InterfaceKind,
) -> tuple[InterfaceVersionRecord, ...]:
    """Return the immutable version registry for an interface."""
    if kind == "forcing":
        return FORCING_INTERFACE_VERSIONS
    if kind == "output":
        return OUTPUT_INTERFACE_VERSIONS
    raise ValueError(f"unknown data-interface kind: {kind!r}")


def current_interface_version(kind: InterfaceKind) -> str | None:
    """Return the current public version, or ``None`` before first release."""
    if kind == "forcing":
        return CURRENT_FORCING_INTERFACE_VERSION
    if kind == "output":
        return CURRENT_OUTPUT_INTERFACE_VERSION
    raise ValueError(f"unknown data-interface kind: {kind!r}")


def interface_version_record(
    kind: InterfaceKind,
    version: str,
) -> InterfaceVersionRecord:
    """Return a registered version record or raise for an unknown version."""
    for record in interface_version_registry(kind):
        if record.version == version:
            return record
    raise ValueError(f"{kind} interface version {version!r} is not registered")


validate_version_registry(
    FORCING_INTERFACE_VERSIONS,
    CURRENT_FORCING_INTERFACE_VERSION,
)
validate_version_registry(
    OUTPUT_INTERFACE_VERSIONS,
    CURRENT_OUTPUT_INTERFACE_VERSION,
)
