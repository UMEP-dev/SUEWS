#!/usr/bin/env python3
"""Guard: PRs touching data_model must bump CURRENT_SCHEMA_VERSION
AND sync the user-facing schema documentation.

Usage
-----
    python scripts/lint/check_schema_version_bump.py [--base BASE_REF]

Compares the working tree (or HEAD in CI) against `BASE_REF` (defaults
to `origin/master`). The script enforces two coupled invariants:

1. Schema bump invariant — if any file under `src/supy/data_model/` or
   the `src/supy/sample_data/sample_config.yml` sample has changed
   *and* `CURRENT_SCHEMA_VERSION` in
   `src/supy/data_model/schema/version.py` has *not* changed, exit
   non-zero with remediation guidance.
2. Documentation-sync invariant — if `CURRENT_SCHEMA_VERSION` *did*
   move, at least one of the schema-versioning user docs
   (`docs/source/contributing/schema/schema_versioning.rst`,
   `docs/source/inputs/transition_guide.rst`) must have been touched in
   the same diff. Otherwise exit non-zero.

The first gate exists because schema drift (gh#1304) slipped through
several releases: code changes landed without a matching bump to
`CURRENT_SCHEMA_VERSION`. The second gate closes the sibling gap
surfaced after that fix — the rule and migration handler were updated
but the user-facing docs still described the old major.minor policy
and never learned about the 2026.x migrations.

See `.claude/rules/python/schema-versioning.md` for when a bump is (and
is not) required.

The first check compares the literal value of `CURRENT_SCHEMA_VERSION`
at the base ref and in the current tree — touching `version.py`
without changing the value does not count as a bump.

Bypassing
---------
If the diff is genuinely cosmetic (docstring edits, comment reformats,
a non-structural sample_config value change), a maintainer can add the
label `schema-audit-ok` to the pull request. The CI workflow at
`.github/workflows/schema-version-audit.yml` skips this script when
that label is present, so this script itself has no label
awareness — its only job is to detect the file-level change pattern.
The bypass label also waives the documentation-sync gate, so it should
only be applied when both the code and the docs are genuinely
untouched by the PR.

Exit codes
----------
* 0 — no structural change detected, or `CURRENT_SCHEMA_VERSION`
  changed value *and* the user-facing docs moved with it.
* 1 — structural change detected without a `CURRENT_SCHEMA_VERSION`
  bump, OR a bump landed without matching documentation updates.
* 2 — script failure (bad args, git unavailable, etc.).
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import re
import subprocess
import sys

# Paths whose change implies a schema change (relative to repo root).
_WATCHED_PREFIXES: tuple[str, ...] = (
    "src/supy/data_model/",
    "src/supy/sample_data/sample_config.yml",
)

# The file that must be touched when any of the watched paths changes.
_SCHEMA_VERSION_FILE = "src/supy/data_model/schema/version.py"

# User-facing docs that explain the YAML schema and its migrations.
# Whenever `CURRENT_SCHEMA_VERSION` changes, at least one of these
# paths must also appear in the same diff. The pair is deliberately
# narrow — a real schema bump touches either the policy page or the
# user migration guide (usually both); other docs are optional.
_SCHEMA_DOC_PATHS: tuple[str, ...] = (
    "docs/source/contributing/schema/schema_versioning.rst",
    "docs/source/inputs/transition_guide.rst",
)


def _run(args: list[str]) -> str:
    """Run a subprocess and return stdout; raise on non-zero exit."""
    result = subprocess.run(
        args,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _merge_base(base_ref: str) -> str:
    """Return the merge-base commit between `base_ref` and `HEAD`.

    The audit cares about what *this branch* did, not about what landed
    on `base_ref` after the branch diverged. Resolving the merge-base
    explicitly lets both the changed-file listing and the base-side
    schema-version lookup compare against the same reference point,
    even when `base_ref` has advanced with unrelated commits.
    """
    return _run(["git", "merge-base", base_ref, "HEAD"]).strip()


def _list_changed_files(base_ref: str, include_worktree: bool) -> list[str]:
    """Return files changed between `base_ref` and the branch tip.

    By default, uses symmetric-difference diff (`base_ref...HEAD`) so
    commits landing on `base_ref` in parallel don't show up as
    "removed". With `include_worktree=True`, compares the working tree
    against the merge-base so uncommitted edits are included but
    unrelated commits on `base_ref` are still filtered out.
    """
    if include_worktree:
        output = _run(["git", "diff", "--name-only", _merge_base(base_ref)])
    else:
        output = _run(["git", "diff", "--name-only", f"{base_ref}...HEAD"])
    return [line.strip() for line in output.splitlines() if line.strip()]


def _is_watched(path: str) -> bool:
    """Return True if `path` falls under the schema-monitored prefix set."""
    return any(path == p or path.startswith(p) for p in _WATCHED_PREFIXES)


_CURRENT_SCHEMA_VERSION_PATTERN = re.compile(
    r"^\s*CURRENT_SCHEMA_VERSION\s*=\s*['\"]([^'\"]+)['\"]",
    re.MULTILINE,
)


def _extract_current_schema_version(source: str) -> str | None:
    """Return the quoted value of the top-level `CURRENT_SCHEMA_VERSION`.

    Matches the first `CURRENT_SCHEMA_VERSION = "<value>"` assignment in
    `source`. Returns None when the constant is absent, which lets callers
    treat "file missing" and "file refactored beyond recognition" as hard
    failures rather than silent passes.
    """
    match = _CURRENT_SCHEMA_VERSION_PATTERN.search(source)
    if match is None:
        return None
    return match.group(1)


def _load_schema_version_at_base(base_ref: str) -> str | None:
    """Return `CURRENT_SCHEMA_VERSION` at the merge-base of `base_ref`.

    Reading from the merge-base (rather than the tip of `base_ref`)
    makes the audit immune to unrelated bumps that land on `base_ref`
    after this branch diverges. Without this, a later schema bump on
    `master` would make any branch that never touched the schema look
    as if it had bumped — the exact false-pass this gate is meant to
    prevent.

    `git show <ref>:<path>` can legitimately fail if the file did not
    yet exist at the merge-base (new file on this PR). In that case we
    treat the base-side version as unknown, which causes the gate to
    reject the PR unless the current tree actually has a value (any
    value counts as a bump from "nothing").
    """
    try:
        merge_base = _merge_base(base_ref)
    except subprocess.CalledProcessError:
        return None
    try:
        return _extract_current_schema_version(
            _run(["git", "show", f"{merge_base}:{_SCHEMA_VERSION_FILE}"])
        )
    except subprocess.CalledProcessError:
        return None


def _load_schema_version_worktree() -> str | None:
    """Return `CURRENT_SCHEMA_VERSION` from the file on disk, or None."""
    try:
        return _extract_current_schema_version(
            Path(_SCHEMA_VERSION_FILE).read_text(encoding="utf-8")
        )
    except FileNotFoundError:
        return None


def _report_failure(watched: list[str], reason: str) -> None:
    print(
        "[schema-version-audit] FAILED\n"
        "\n"
        f"{reason}\n"
        "\n"
        "The following files changed:\n"
        + "".join(f"  - {p}\n" for p in sorted(watched))
        + "\n"
        "If this PR renames, removes, or changes the type of a public "
        "field, adds a required field, or restructures a nested section, "
        f"bump CURRENT_SCHEMA_VERSION in {_SCHEMA_VERSION_FILE}, add a "
        "matching entry to SCHEMA_VERSIONS, and register a "
        "(previous_schema -> current_schema) handler in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS "
        "(that handler is what makes the previous schema compatible). "
        "See .claude/rules/python/schema-versioning.md for the checklist.\n"
        "\n"
        "If the diff is genuinely cosmetic (docstring, comment, formatting, "
        "non-structural edit), a maintainer can add the 'schema-audit-ok' "
        "label to this PR to skip this check.",
        file=sys.stderr,
    )


def _report_docs_failure(
    base_version: str | None,
    current_version: str,
) -> None:
    base_label = base_version if base_version is not None else "<unknown>"
    print(
        "[schema-version-audit] FAILED — docs out of sync\n"
        "\n"
        f"CURRENT_SCHEMA_VERSION moved from {base_label!r} to "
        f"{current_version!r}, but none of the user-facing schema docs "
        "were touched in this PR:\n"
        + "".join(f"  - {p}\n" for p in _SCHEMA_DOC_PATHS)
        + "\n"
        "A schema bump is a user-visible change. Update at least one of "
        "the paths above so the YAML upgrade path is discoverable from "
        "the docs site. Step 6 of "
        ".claude/rules/python/schema-versioning.md spells out which "
        "sections typically need edits.\n"
        "\n"
        "If the bump is genuinely internal and has no user-visible "
        "footprint (very rare), a maintainer can add the "
        "'schema-audit-ok' label to this PR to skip this check.",
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
    base_version = _load_schema_version_at_base(args.base)
    current_version = _load_schema_version_worktree()
    version_bumped = (
        current_version is not None
        and base_version is not None
        and base_version != current_version
    )

    if not watched_changed and not version_bumped:
        print("[schema-version-audit] no watched files changed, skipping.")
        return 0

    if watched_changed:
        if current_version is None:
            _report_failure(
                watched_changed,
                reason=(
                    f"Could not read CURRENT_SCHEMA_VERSION from "
                    f"{_SCHEMA_VERSION_FILE} in the current tree. The file must "
                    "exist and define CURRENT_SCHEMA_VERSION as a string literal."
                ),
            )
            return 1

        if base_version == current_version:
            _report_failure(
                watched_changed,
                reason=(
                    f"CURRENT_SCHEMA_VERSION is still {current_version!r} on "
                    f"this branch. Touching {_SCHEMA_VERSION_FILE} without "
                    "changing the CURRENT_SCHEMA_VERSION value does not count "
                    "as a bump."
                ),
            )
            return 1

    if version_bumped:
        changed_set = set(changed)
        docs_touched = [p for p in _SCHEMA_DOC_PATHS if p in changed_set]
        if not docs_touched:
            _report_docs_failure(base_version, current_version)
            return 1
        print(
            "[schema-version-audit] docs touched alongside bump: "
            + ", ".join(docs_touched)
        )

    if watched_changed and version_bumped:
        print(
            "[schema-version-audit] watched files changed AND "
            f"CURRENT_SCHEMA_VERSION moved from {base_version!r} to "
            f"{current_version!r}; bump confirmed."
        )
    elif version_bumped:
        print(
            "[schema-version-audit] CURRENT_SCHEMA_VERSION moved from "
            f"{base_version!r} to {current_version!r} without "
            "data_model changes; docs sync confirmed."
        )
    return 0


if __name__ == "__main__":
    # Relative paths (_SCHEMA_VERSION_FILE, watched prefixes, doc paths)
    # are resolved against CWD, so pin CWD to the repo root regardless
    # of where the caller invoked the script from. CI checkouts run
    # from the root already; local dry-runs from a subdirectory would
    # otherwise fail with "Could not read CURRENT_SCHEMA_VERSION".
    repo_root = Path(__file__).resolve().parent.parent.parent
    os.chdir(repo_root)
    sys.exit(main())
