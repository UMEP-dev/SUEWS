#!/usr/bin/env python3
"""Guard: PRs touching data_model must bump CURRENT_SCHEMA_VERSION.

Usage
-----
    python scripts/lint/check_schema_version_bump.py [--base BASE_REF]

Compares the working tree (or HEAD in CI) against `BASE_REF` (defaults
to `origin/master`). If any file under `src/supy/data_model/` or the
`src/supy/sample_data/sample_config.yml` sample has changed *and*
`src/supy/data_model/schema/version.py` has *not* changed, the script
exits non-zero with a remediation message.

The gate exists because the schema-drift that motivated gh#1304 slipped
through several releases: code changes landed without a matching bump
to `CURRENT_SCHEMA_VERSION`, and the upgrade-tool registry had no real
schema version to dispatch on. See
`.claude/rules/python/schema-versioning.md` for when a bump is (and is
not) required.

Bypassing
---------
If the diff is genuinely cosmetic (docstring edits, comment reformats,
a non-structural sample_config value change), a maintainer can add the
label `schema-audit-ok` to the pull request. The CI workflow at
`.github/workflows/schema-version-audit.yml` skips this script when
that label is present, so this script itself has no label
awareness — its only job is to detect the file-level change pattern.

Exit codes
----------
* 0 — no structural change detected, or schema/version.py was also
  touched.
* 1 — structural change detected without a schema bump.
* 2 — script failure (bad args, git unavailable, etc.).
"""

from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import sys

# Paths whose change implies a schema change (relative to repo root).
_WATCHED_PREFIXES: tuple[str, ...] = (
    "src/supy/data_model/",
    "src/supy/sample_data/sample_config.yml",
)

# The file that must be touched when any of the watched paths changes.
_SCHEMA_VERSION_FILE = "src/supy/data_model/schema/version.py"


def _run(args: list[str]) -> str:
    """Run a subprocess and return stdout; raise on non-zero exit."""
    result = subprocess.run(
        args,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _list_changed_files(base_ref: str, include_worktree: bool) -> list[str]:
    """Return files changed between `base_ref` and the branch tip.

    By default, uses symmetric-difference diff (`base_ref...HEAD`) so
    commits landing on `base_ref` in parallel don't show up as
    "removed". With `include_worktree=True`, compares the working tree
    against `base_ref` directly — which pulls in uncommitted edits too
    (useful for local dry-runs).
    """
    if include_worktree:
        output = _run(["git", "diff", "--name-only", base_ref])
    else:
        output = _run(["git", "diff", "--name-only", f"{base_ref}...HEAD"])
    return [line.strip() for line in output.splitlines() if line.strip()]


def _is_watched(path: str) -> bool:
    """Return True if `path` falls under the schema-monitored prefix set."""
    return any(path == p or path.startswith(p) for p in _WATCHED_PREFIXES)


def _report_failure(watched: list[str]) -> None:
    print(
        "[schema-version-audit] FAILED\n"
        "\n"
        "The following files changed but "
        f"{_SCHEMA_VERSION_FILE} was not touched:\n"
        + "".join(f"  - {p}\n" for p in sorted(watched))
        + "\n"
        "If this PR renames, removes, or changes the type of a public "
        "field, adds a required field, or restructures a nested section, "
        f"bump CURRENT_SCHEMA_VERSION in {_SCHEMA_VERSION_FILE} and add "
        "a matching entry to SCHEMA_VERSIONS + COMPATIBLE_VERSIONS. See "
        ".claude/rules/python/schema-versioning.md for the checklist.\n"
        "\n"
        "If the diff is genuinely cosmetic (docstring, comment, formatting, "
        "non-structural edit), a maintainer can add the 'schema-audit-ok' "
        "label to this PR to skip this check.",
        file=sys.stderr,
    )


def main(argv: list[str] | None = None) -> int:
    """CLI entry point — returns a process exit code (0/1/2)."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        default="origin/master",
        help="Git ref to compare against (default: origin/master).",
    )
    parser.add_argument(
        "--include-worktree",
        action="store_true",
        help=(
            "Compare the working tree against the base instead of the "
            "last commit. Useful for local dry-runs before the changes "
            "are committed. In CI, defaults are correct."
        ),
    )
    args = parser.parse_args(argv)

    try:
        changed = _list_changed_files(args.base, args.include_worktree)
    except subprocess.CalledProcessError as exc:
        print(
            f"[schema-version-audit] git diff failed against base "
            f"{args.base!r}: {exc.stderr}",
            file=sys.stderr,
        )
        return 2

    watched_changed = [p for p in changed if _is_watched(p)]
    if not watched_changed:
        print("[schema-version-audit] no watched files changed, skipping.")
        return 0

    if _SCHEMA_VERSION_FILE in changed:
        print(
            f"[schema-version-audit] watched files changed AND "
            f"{_SCHEMA_VERSION_FILE} was modified; trusting the bump."
        )
        return 0

    _report_failure(watched_changed)
    return 1


if __name__ == "__main__":
    # Make sure we run from the repo root so relative paths are stable.
    repo_root = Path(__file__).resolve().parent.parent.parent
    sys.exit(main())
