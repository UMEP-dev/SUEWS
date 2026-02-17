#!/usr/bin/env python3
"""Validate bridge-manifest coverage for public SUEWS derived types.

This script is intentionally stdlib-only for easy CI use.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path

TYPE_PUBLIC_RE = re.compile(r"^\s*TYPE\s*,\s*PUBLIC\s*::\s*(\w+)", re.IGNORECASE)
ALLOWED_STATUSES = {"bridged", "planned", "excluded"}
REPO_ROOT = Path(__file__).resolve().parents[1]


def discover_public_types(source_root: Path) -> dict[str, str]:
    discovered: dict[str, str] = {}
    duplicates: list[str] = []

    for path in sorted(source_root.glob("*.f95")):
        try:
            rel_path = path.resolve().relative_to(REPO_ROOT.resolve()).as_posix()
        except ValueError:
            rel_path = path.resolve().as_posix()
        for line in path.read_text(encoding="utf-8").splitlines():
            match = TYPE_PUBLIC_RE.match(line)
            if not match:
                continue
            name = match.group(1)
            if name in discovered:
                duplicates.append(name)
            discovered[name] = rel_path

    if duplicates:
        unique = ", ".join(sorted(set(duplicates)))
        raise ValueError(f"duplicate public type declarations detected: {unique}")

    return discovered


def validate_manifest(
    manifest_data: dict,
    discovered: dict[str, str],
) -> list[str]:
    errors: list[str] = []

    entries = manifest_data.get("types")
    if not isinstance(entries, list):
        return ["manifest field `types` must be a list"]

    names: list[str] = []
    manifest_sources: dict[str, str] = {}
    statuses: Counter[str] = Counter()

    for idx, entry in enumerate(entries):
        label = f"types[{idx}]"
        if not isinstance(entry, dict):
            errors.append(f"{label} must be an object")
            continue

        name = entry.get("name")
        status = entry.get("status")
        source = entry.get("source")

        if not isinstance(name, str) or not name:
            errors.append(f"{label}.name must be a non-empty string")
            continue
        names.append(name)

        if not isinstance(status, str) or status not in ALLOWED_STATUSES:
            allowed = ", ".join(sorted(ALLOWED_STATUSES))
            errors.append(f"{label}.status for `{name}` must be one of: {allowed}")
        else:
            statuses[status] += 1

        if not isinstance(source, str) or not source:
            errors.append(f"{label}.source for `{name}` must be a non-empty string")
        else:
            manifest_sources[name] = source

        if status == "excluded":
            rationale = entry.get("rationale")
            if not isinstance(rationale, str) or not rationale.strip():
                errors.append(f"{label}.rationale is required when `{name}` is excluded")

    duplicate_names = sorted(name for name, count in Counter(names).items() if count > 1)
    if duplicate_names:
        errors.append(f"manifest contains duplicate type names: {', '.join(duplicate_names)}")

    discovered_names = set(discovered)
    manifest_names = set(names)

    missing = sorted(discovered_names - manifest_names)
    if missing:
        errors.append(f"manifest missing public types: {', '.join(missing)}")

    extras = sorted(manifest_names - discovered_names)
    if extras:
        errors.append(f"manifest contains unknown types: {', '.join(extras)}")

    for name in sorted(discovered_names & manifest_names):
        expected_source = discovered[name]
        manifest_source = manifest_sources.get(name)
        if manifest_source and manifest_source != expected_source:
            errors.append(
                f"source mismatch for `{name}`: manifest `{manifest_source}`, discovered `{expected_source}`"
            )

    if not errors:
        summary = ", ".join(f"{key}={statuses.get(key, 0)}" for key in sorted(ALLOWED_STATUSES))
        print(
            f"Bridge manifest is valid: {len(discovered_names)} public types covered ({summary})."
        )

    return errors


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        type=Path,
        default=REPO_ROOT / "src/suews_bridge/bridge-manifest.json",
        help="Path to bridge-manifest JSON file",
    )
    parser.add_argument(
        "--source-root",
        type=Path,
        default=REPO_ROOT / "src/suews/src",
        help="Directory containing canonical Fortran type definitions",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    if not args.manifest.is_file():
        print(f"error: manifest not found: {args.manifest}", file=sys.stderr)
        return 1
    if not args.source_root.is_dir():
        print(f"error: source root not found: {args.source_root}", file=sys.stderr)
        return 1

    try:
        manifest_data = json.loads(args.manifest.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        print(f"error: invalid JSON in {args.manifest}: {exc}", file=sys.stderr)
        return 1

    try:
        discovered = discover_public_types(args.source_root)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    errors = validate_manifest(manifest_data, discovered)
    if errors:
        print("Bridge manifest validation failed:", file=sys.stderr)
        for err in errors:
            print(f"- {err}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
