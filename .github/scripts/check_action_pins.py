#!/usr/bin/env python3
"""Enforce commit-SHA pinning for external GitHub Actions.

This blocks floating refs like ``@v4``, ``@main``, ``@beta``, and branch names in
workflow definitions. Local actions (``./...``) are allowed.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


USES_PATTERN = re.compile(r"\buses:\s*([^\s]+)")
SHA_REF_PATTERN = re.compile(r"^[^@\s]+@[0-9a-fA-F]{40}$")


def iter_workflow_files() -> list[Path]:
    """Return workflow and composite action definitions to scan."""
    workflow_files = sorted(Path(".github/workflows").glob("*.yml"))
    action_files = sorted(Path(".github/actions").rglob("*.yml"))
    return [*workflow_files, *action_files]


def main() -> int:
    """Run the pinning audit."""
    violations: list[str] = []

    for path in iter_workflow_files():
        for line_number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
            match = USES_PATTERN.search(line)
            if not match:
                continue

            action_ref = match.group(1)
            if action_ref.startswith("./") or action_ref.startswith("docker://"):
                continue
            if SHA_REF_PATTERN.match(action_ref):
                continue

            violations.append(f"{path}:{line_number}: external action must be pinned to a 40-character commit SHA: {action_ref}")

    if violations:
        print("Found non-SHA-pinned external GitHub Actions:\n", file=sys.stderr)
        for violation in violations:
            print(f"- {violation}", file=sys.stderr)
        return 1

    print("All external GitHub Actions are pinned to commit SHAs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
