#!/usr/bin/env python3
"""Check that forcing and output version histories remain append-only.

This audit covers only the version ownership established in ``data_model``.
Contract-specific checks belong with the forcing and output definitions once
their machine-readable representations are available.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import re
import subprocess
import sys
from typing import Literal, NamedTuple

from version_audit import (
    extract_literal_assignments,
    read_file_at_ref,
    resolve_merge_base,
)

type InterfaceKind = Literal["forcing", "output"]


class VersionState(NamedTuple):
    """Literal values read from one interface version module."""

    current: str | None
    versions: tuple[tuple[str, str], ...]


_INTERFACES: dict[InterfaceKind, tuple[str, str, str]] = {
    "forcing": (
        "src/supy/data_model/forcing/version.py",
        "CURRENT_FORCING_VERSION",
        "FORCING_VERSIONS",
    ),
    "output": (
        "src/supy/data_model/output/version.py",
        "CURRENT_OUTPUT_VERSION",
        "OUTPUT_VERSIONS",
    ),
}
_SEMVER = re.compile(r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$")
_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")


class AuditFailure(RuntimeError):
    """Raised for a version-history violation that a contributor can fix."""


def _parse_state(source: str, current_name: str, versions_name: str) -> VersionState:
    assignments = extract_literal_assignments(source, (current_name, versions_name))
    current = assignments[current_name]
    versions = assignments[versions_name]
    if current is not None and not isinstance(current, str):
        raise ValueError(f"{current_name} must be a string or None")
    if not isinstance(versions, dict) or any(
        not isinstance(version, str) or not isinstance(digest, str)
        for version, digest in versions.items()
    ):
        raise ValueError(f"{versions_name} must be a string-to-string dict literal")
    return VersionState(current, tuple(versions.items()))


def _validate_state(kind: InterfaceKind, state: VersionState) -> None:
    if not state.versions:
        if state.current is not None:
            raise AuditFailure(f"{kind}: an empty history requires current=None")
        return

    if state.current != state.versions[-1][0]:
        raise AuditFailure(f"{kind}: current must be the last registered version")

    previous: tuple[int, int, int] | None = None
    for index, (version, digest) in enumerate(state.versions):
        match = _SEMVER.fullmatch(version)
        if match is None:
            raise AuditFailure(
                f"{kind}: version {version!r} must use stable MAJOR.MINOR.PATCH SemVer"
            )
        parsed = tuple(int(part) for part in match.groups())
        if index == 0 and parsed != (1, 0, 0):
            raise AuditFailure(f"{kind}: the first public version must be '1.0.0'")
        if previous is not None and parsed <= previous:
            raise AuditFailure(f"{kind}: version history must increase monotonically")
        if _DIGEST.fullmatch(digest) is None:
            raise AuditFailure(
                f"{kind}: version {version!r} needs a lowercase SHA-256 digest"
            )
        previous = parsed


def _validate_transition(
    kind: InterfaceKind,
    base: VersionState,
    current: VersionState,
) -> bool:
    base_length = len(base.versions)
    if (
        len(current.versions) < base_length
        or current.versions[:base_length] != base.versions
    ):
        raise AuditFailure(f"{kind}: released version history is append-only")

    appended = len(current.versions) > base_length
    current_changed = current.current != base.current
    if appended != current_changed:
        raise AuditFailure(
            f"{kind}: current version and version history must change together"
        )
    return appended


def _state_at_base(
    merge_base: str,
    path: str,
    current_name: str,
    versions_name: str,
) -> VersionState:
    source = read_file_at_ref(merge_base, path)
    if source is None:
        return VersionState(None, ())
    return _parse_state(source, current_name, versions_name)


def _audit(base_ref: str) -> list[InterfaceKind]:
    merge_base = resolve_merge_base(base_ref)
    changed: list[InterfaceKind] = []
    for kind, (path, current_name, versions_name) in _INTERFACES.items():
        base = _state_at_base(merge_base, path, current_name, versions_name)
        current = _parse_state(
            Path(path).read_text(encoding="utf-8"),
            current_name,
            versions_name,
        )
        _validate_state(kind, current)
        if _validate_transition(kind, base, current):
            changed.append(kind)
    return changed


def main(argv: list[str] | None = None) -> int:
    """Run the forcing/output version-history audit."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        default="origin/master",
        help="Git ref to compare against (default: origin/master).",
    )
    args = parser.parse_args(argv)

    try:
        changed = _audit(args.base)
    except AuditFailure as exc:
        print(f"[data-interface-version-history] FAILED: {exc}", file=sys.stderr)
        return 1
    except (OSError, SyntaxError, subprocess.CalledProcessError, ValueError) as exc:
        print(f"[data-interface-version-history] ERROR: {exc}", file=sys.stderr)
        return 2

    if changed:
        print(
            "[data-interface-version-history] appended version history: "
            + ", ".join(changed)
        )
    else:
        print("[data-interface-version-history] no version-history changes.")
    return 0


if __name__ == "__main__":
    os.chdir(Path(__file__).resolve().parents[2])
    sys.exit(main())
