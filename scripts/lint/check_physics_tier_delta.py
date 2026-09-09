#!/usr/bin/env python3
"""Keep the `physics-full` CI tier honest about what it adds over `standard`.

The physics axis of the `standard` tier runs `-m "physics and (core or not
slow)"`; the `physics-full` tier (selected for `0-physics:change` PRs and in
their merge-queue runs, gh#1576) runs `-m physics`. The only tests the label
can add are therefore those matching::

    physics and slow and not core

`.claude/rules/physics-change-evidence.md` records that set between two HTML
comment markers (see `RULE_BEGIN` / `RULE_END`). This module compares the set
pytest actually collects against that record and fails on a difference in
either direction:

- a collected test that is not recorded runs only in `physics-full` and in the
  nightly, and nobody said so;
- a recorded pattern that matches no collected test means the rule describes
  coverage that no longer exists.

The comparison itself is pure Python and needs neither pytest nor supy, so it
is unit-tested with synthetic node ids. The collection it consumes needs a
built supy, which the "Check pytest marker axis" CI job does not have; the
live check therefore runs as
`test/core/test_physics_tier_delta.py::test_physics_full_tier_adds_exactly_what_the_rule_records`
(a `core` test, so every `standard` and fuller tier and `make test` run it),
not in that job. Run this file directly for a one-off local check; it collects
with the current interpreter's pytest.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
import re
import subprocess
import sys

RULE_PATH = Path(".claude/rules/physics-change-evidence.md")
RULE_BEGIN = "<!-- physics-full-only nodes: begin -->"
RULE_END = "<!-- physics-full-only nodes: end -->"
NONE_SENTINEL = "(none)"

STANDARD_EXPR = "physics and (core or not slow)"
FULL_EXPR = "physics"
DELTA_EXPR = "physics and slow and not core"


class RuleFormatError(ValueError):
    """The rule file does not carry a parseable record of the tier delta."""


def node_id_matches(node_id: str, pattern: str) -> bool:
    """Match a pytest node id against a recorded pattern.

    Only `*` (any run of characters) and `?` (one character) are wildcards.
    Square brackets are literal, unlike in `fnmatch`, because pytest writes
    parameter ids as `test_name[a-b]` and a bracket character class would
    silently change what `test_name[*]` means.
    """
    regex = "".join(
        ".*" if part == "*" else "." if part == "?" else re.escape(part)
        for part in re.split(r"([*?])", pattern)
        if part
    )
    return re.fullmatch(regex, node_id) is not None


def parse_recorded_nodes(rule_text: str) -> list[str]:
    """Return the node-id patterns recorded between the rule's markers.

    Lines inside the block that are blank, code fences, or `#` comments are
    ignored; the literal `(none)` records an empty set. Each remaining line is
    a pattern against a pytest node id in which `*` matches any run of
    characters and `?` one character (see `node_id_matches`), so a
    parametrised test can be recorded once as `path::test_name[*]`.
    """
    try:
        begin = rule_text.index(RULE_BEGIN) + len(RULE_BEGIN)
        end = rule_text.index(RULE_END, begin)
    except ValueError as exc:
        raise RuleFormatError(
            f"{RULE_PATH.as_posix()} must carry the block delimited by "
            f"'{RULE_BEGIN}' and '{RULE_END}' recording which tests the "
            "physics-full tier adds over standard."
        ) from exc

    patterns: list[str] = []
    for raw in rule_text[begin:end].splitlines():
        line = raw.strip()
        if not line or line.startswith("```") or line.startswith("#"):
            continue
        if line == NONE_SENTINEL:
            continue
        patterns.append(line)
    return patterns


@dataclass
class TierDeltaReport:
    """Outcome of comparing collected delta nodes with the recorded list."""

    unrecorded: list[str] = field(default_factory=list)
    stale: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        """True when the collected set and the recorded list agree."""
        return not self.unrecorded and not self.stale

    def message(self) -> str:
        """Human-readable outcome naming offending ids and the remedy."""
        parts: list[str] = []
        if self.unrecorded:
            bullet = "\n  - ".join(self.unrecorded)
            parts.append(
                "[X] physics-full tier check: these tests are marked "
                "`physics` and `slow` without `core`, so they run only in the "
                "physics-full tier (0-physics:change PRs) and in the nightly, "
                "not in `standard`, and the rule does not say so:\n  - "
                f"{bullet}\n"
                "Either mark each one `core` as well (so `standard` keeps "
                "running it), or record its node id between the "
                f"'{RULE_BEGIN}' and '{RULE_END}' markers in "
                f"{RULE_PATH.as_posix()} and restate there what the tier adds."
            )
        if self.stale:
            bullet = "\n  - ".join(self.stale)
            parts.append(
                "[X] physics-full tier check: these node ids recorded in "
                f"{RULE_PATH.as_posix()} match no collected test that is "
                "`physics` and `slow` without `core`:\n  - "
                f"{bullet}\n"
                "Remove them from the rule (or fix the pattern) so the rule "
                "does not claim coverage the physics-full tier no longer adds."
            )
        if not parts:
            return (
                "[OK] physics-full tier adds exactly the recorded tests over standard."
            )
        return "\n".join(parts)


def check_tier_delta(collected: list[str], recorded: list[str]) -> TierDeltaReport:
    """Compare the collected `physics and slow and not core` node ids with the record.

    `collected` holds pytest node ids; `recorded` holds patterns from
    `parse_recorded_nodes`. Order in the report follows the input order.
    """
    report = TierDeltaReport()
    for node_id in collected:
        if not any(node_id_matches(node_id, pattern) for pattern in recorded):
            report.unrecorded.append(node_id)
    for pattern in recorded:
        if not any(node_id_matches(node_id, pattern) for node_id in collected):
            report.stale.append(pattern)
    return report


def check_against_rule(collected: list[str], repo_root: Path) -> TierDeltaReport:
    """Read the rule under `repo_root` and compare it with `collected`."""
    rule_text = (repo_root / RULE_PATH).read_text(encoding="utf-8")
    return check_tier_delta(collected, parse_recorded_nodes(rule_text))


def collect_delta_nodes(repo_root: Path) -> list[str]:
    """Collect the `physics and slow and not core` node ids with pytest.

    Needs an importable supy. pytest evaluates the marker expression, so the
    result is exactly what the CI tiers would select, however the markers were
    applied (module `pytestmark`, decorators, conftest hooks, `pytest.param`).
    """
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "test",
            "--collect-only",
            "-q",
            "-p",
            "no:cacheprovider",
            "-m",
            DELTA_EXPR,
        ],
        cwd=repo_root,
        text=True,
        capture_output=True,
        check=False,
    )
    # pytest exits 5 when the expression selects nothing; that is the
    # expected state today, not an error.
    if result.returncode not in {0, 5}:
        raise RuntimeError(
            f"pytest collection failed (exit {result.returncode}):\n{result.stderr}"
        )
    return [line for line in result.stdout.splitlines() if "::" in line]


def main(argv: list[str]) -> int:
    """Collect the tier delta on the tree at `argv[1]` (default cwd) and check it."""
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd()
    try:
        collected = collect_delta_nodes(repo_root)
    except RuntimeError as exc:
        print(exc, file=sys.stderr)
        return 1
    report = check_against_rule(collected, repo_root)
    print(report.message(), file=sys.stderr if not report.ok else sys.stdout)
    print(
        f'standard runs `-m "{STANDARD_EXPR}"`; physics-full runs '
        f"`-m {FULL_EXPR}`; `{DELTA_EXPR}` collects {len(collected)} node(s)."
    )
    return 0 if report.ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
