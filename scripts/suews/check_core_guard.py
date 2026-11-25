#!/usr/bin/env python3
"""Check that every core test passed on Linux before a merge.

The merge rule (22 Nov 2026) is that every test marked ``core`` runs, and
passes, on Linux before a pull request merges; the merge queue re-runs the
standard tier for that reason. A green job only shows that the tests it
selected did not fail, so this guard checks node ids instead:

* the inventory (``scripts/suews/pytest_inventory.py``, recorded by the Linux
  cp312 api lane with ``--collect-only`` and no ``-m``) lists every test and
  its markers, independently of the tier expression and of any shard split;
* each Linux lane (physics on the build Python, api on every requested
  Python) must have run exactly the tests its tier expression selects from
  that inventory: the shards of a lane must all be present, must be disjoint,
  each test must sit in the shard its node id hashes to, and their union must
  equal the expected selection;
* every core test of the lane's axis must have the outcome ``passed``. A
  skip, an xfail, a failure or an absence fails the guard, unless the exact
  node id is listed in ``scripts/suews/core_guard_allowlist.json`` with its
  reason; an allowlist entry that no longer applies also fails, so the list
  cannot go stale;
* ``test_sample_run_reproduces_reference_exactly`` is named explicitly: it
  must be in the inventory, marked core, and passed in the physics lane,
  because outside its reference environment it skips instead of failing.

Two kinds of test cannot run in a Linux lane and so need an explicit
allowlist entry each: core tests marked ``qgis`` (no tier before merge runs
``qgis``; they need the QGIS runtime on Windows), listed by node id with the
outcome ``not run``; and test modules that skip while they are collected
(``pytest.importorskip``), which contribute no node ids at all and are listed
by module path.

The guard runs on ready tiers only (``standard`` and ``physics-full``): draft
tiers deliberately run a subset. A pull request whose paths need no build
(``needs-build`` false and no physics or numerics label, in practice
documentation-only changes) runs no tests at all; that is the one documented
exemption, and the guard job is skipped there.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
from dataclasses import dataclass, field
import json
from pathlib import Path
import re
import sys

if __package__:
    from .ci_tiers import READY_TIERS, matches, tier_expression
    from .pytest_shard import shard_of
else:
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    from ci_tiers import READY_TIERS, matches, tier_expression
    from pytest_shard import shard_of

EXACT_REFERENCE_TEST = (
    "test/core/test_sample_output_exact.py::"
    "test_sample_run_reproduces_reference_exactly"
)
LINUX = ("manylinux", "x86_64")
DEFAULT_ALLOWLIST = Path(__file__).resolve().parent / "core_guard_allowlist.json"

# physics-<py>-<platform>-<arch>[-shard-K-of-N]-pytest.json (wheel and physics
# jobs) and api-<py>-<platform>-<arch>[-shard-K-of-N].json (api lane).
_LANE_FILE = re.compile(
    r"^(?P<kind>physics|api)-(?P<python>cp3\d+)-(?P<platform>[a-z]+)-(?P<arch>[A-Za-z0-9_]+)"
    r"(?:-shard-(?P<index>\d+)-of-(?P<total>\d+))?(?P<suffix>-pytest)?\.json$"
)
_INVENTORY_FILE = re.compile(r"^inventory-.*\.json$")
_MAX_LISTED = 25


@dataclass
class Shard:
    path: Path
    index: int | None
    total: int | None
    outcomes: dict[str, str]


@dataclass
class Report:
    errors: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)

    def error(self, message: str, items: list[str] | None = None) -> None:
        """Record a failure, listing the first offending items."""
        self.errors.append(_with_items(message, items))

    def note(self, message: str, items: list[str] | None = None) -> None:
        """Record a passing observation for the summary."""
        self.notes.append(_with_items(message, items))


def _with_items(message: str, items: list[str] | None) -> str:
    if not items:
        return message
    shown = sorted(items)[:_MAX_LISTED]
    more = len(items) - len(shown)
    lines = [message, *(f"    {item}" for item in shown)]
    if more > 0:
        lines.append(f"    ... and {more} more")
    return "\n".join(lines)


def load_lanes(
    metrics_dir: Path,
) -> tuple[dict[tuple[str, str, str, str], list[Shard]], list[Path]]:
    """Group the pytest metrics files under ``metrics_dir`` by lane."""
    lanes: dict[tuple[str, str, str, str], list[Shard]] = defaultdict(list)
    inventories: list[Path] = []
    for path in sorted(metrics_dir.rglob("*.json")):
        if _INVENTORY_FILE.match(path.name):
            inventories.append(path)
            continue
        match = _LANE_FILE.match(path.name)
        if match is None:
            continue
        kind = match["kind"]
        # The wheel job writes physics-...-pytest.json beside phases and
        # wheel-job files; the api lane writes api-....json with no suffix.
        if (kind == "physics") != bool(match["suffix"]):
            continue
        document = json.loads(path.read_text(encoding="utf-8"))
        outcomes = {
            record["node_id"]: record.get("outcome") or "missing"
            for record in document.get("tests", [])
        }
        index = int(match["index"]) if match["index"] else None
        total = int(match["total"]) if match["total"] else None
        key = (kind, match["python"], match["platform"], match["arch"])
        lanes[key].append(Shard(path, index, total, outcomes))
    return lanes, inventories


def load_inventory(path: Path) -> tuple[dict[str, set[str]], dict[str, str]]:
    """Return ``{node_id: markers}`` and ``{collector: skip reason}`` from an inventory."""
    document = json.loads(path.read_text(encoding="utf-8"))
    nodes = {record["node_id"]: set(record["markers"]) for record in document["nodes"]}
    skips = {
        record["node_id"]: record.get("reason", "")
        for record in document.get("collection_skips", [])
    }
    return nodes, skips


def load_allowlist(path: Path) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    """Return the test and collection-skip allowlist entries (empty when absent)."""
    if not path.exists():
        return [], []
    document = json.loads(path.read_text(encoding="utf-8"))
    return list(document.get("allowed", [])), list(document.get("collection_skips", []))


def check_collection_skips(
    skips: dict[str, str], allowed: list[dict[str, str]], report: Report
) -> None:
    """Refuse modules that skipped at collection unless each is allowlisted."""
    listed = {entry["node_id"]: entry for entry in allowed}
    unlisted = [f"{node}: {reason}" for node, reason in sorted(skips.items()) if node not in listed]
    if unlisted:
        report.error(
            f"{len(unlisted)} test modules skipped at collection, so none of their tests"
            " (core or not) are in the inventory; fix the dependency or allowlist the module",
            unlisted,
        )
    stale = sorted(node for node in listed if node not in skips)
    if stale:
        report.error("collection-skip allowlist entries no longer skip; remove them", stale)
    excused = sorted(node for node in skips if node in listed)
    if excused:
        report.note(f"{len(excused)} modules skipped at collection, allowlisted", excused)


def _lane_name(key: tuple[str, str, str, str]) -> str:
    kind, python, platform, arch = key
    return f"{kind} | {python} | {platform} {arch}"


def merge_shards(
    key: tuple[str, str, str, str], shards: list[Shard], report: Report
) -> dict[str, str] | None:
    """Return the union of a lane's outcomes after checking its shard split."""
    name = _lane_name(key)
    sharded = [shard for shard in shards if shard.total is not None]
    if sharded and len(sharded) != len(shards):
        report.error(f"{name}: both sharded and unsharded metrics files present")
        return None
    if len(shards) > 1 and not sharded:
        report.error(
            f"{name}: {len(shards)} unsharded metrics files",
            [str(shard.path) for shard in shards],
        )
        return None
    if sharded:
        totals = {shard.total for shard in sharded}
        if len(totals) != 1:
            report.error(f"{name}: shards disagree on N: {sorted(totals)}")
            return None
        (total,) = totals
        indices = sorted(shard.index for shard in sharded)
        if indices != list(range(1, total + 1)):
            report.error(f"{name}: expected shards 1..{total}, found {indices}")
            return None
        for shard in sharded:
            misplaced = [
                node for node in shard.outcomes if shard_of(node, total) != shard.index
            ]
            if misplaced:
                report.error(
                    f"{name}: shard {shard.index}-of-{total} ran tests that hash to another shard",
                    misplaced,
                )
    union: dict[str, str] = {}
    duplicated: list[str] = []
    for shard in shards:
        for node, outcome in shard.outcomes.items():
            if node in union:
                duplicated.append(node)
            union[node] = outcome
    if duplicated:
        report.error(f"{name}: tests ran in more than one shard", duplicated)
    if sharded:
        report.note(
            f"{name}: {len(sharded)} shards, {len(union)} tests, disjoint and complete"
        )
    return union


def check_linux_lane(
    key: tuple[str, str, str, str],
    outcomes: dict[str, str],
    inventory: dict[str, set[str]],
    tier: str,
    allowlist: list[dict[str, str]],
    report: Report,
) -> None:
    """Check one Linux lane's selection and its core-test outcomes."""
    kind = key[0]
    name = _lane_name(key)
    expression = tier_expression(kind, tier)
    expected = {
        node for node, markers in inventory.items() if matches(expression, markers)
    }
    ran = set(outcomes)
    missing = sorted(expected - ran)
    unexpected = sorted(ran - expected)
    if missing:
        report.error(
            f"{name}: {len(missing)} tests selected by -m '{expression}' did not run",
            missing,
        )
    if unexpected:
        report.error(
            f"{name}: {len(unexpected)} tests ran that the inventory does not select with -m '{expression}'",
            unexpected,
        )

    core = {
        node
        for node, markers in inventory.items()
        if kind in markers and "core" in markers
    }
    allowed = {
        entry["node_id"]: entry for entry in allowlist if entry.get("lane") == kind
    }
    if kind == "api":
        # No tier before merge runs qgis, so each such core test is an explicit
        # allowlist entry (outcome "not run"), never a blanket exemption.
        qgis = sorted(node for node in core if "qgis" in inventory[node])
        unlisted = [
            node
            for node in qgis
            if allowed.get(node, {}).get("outcome") != "not run"
        ]
        if unlisted:
            report.error(
                f"{name}: {len(unlisted)} core tests are marked qgis, which no tier before"
                " merge runs; drop the core marker or allowlist each with outcome 'not run'",
                unlisted,
            )
        if len(qgis) > len(unlisted):
            report.note(
                f"{name}: {len(qgis) - len(unlisted)} allowlisted qgis core tests not run before merge"
            )
        core -= set(qgis)
    unselected = sorted(core - expected)
    if unselected:
        report.error(
            f"{name}: {len(unselected)} core tests are not selected by -m '{expression}'",
            unselected,
        )

    bad: list[str] = []
    excused: list[str] = []
    for node in sorted(core):
        outcome = outcomes.get(node, "not run")
        if outcome == "passed":
            continue
        entry = allowed.get(node)
        if entry is not None and outcome == entry.get("outcome", "skipped"):
            excused.append(
                f"{node} ({outcome}: {entry.get('reason', 'no reason given')})"
            )
            continue
        bad.append(f"{node}: {outcome}")
    if bad:
        report.error(f"{name}: {len(bad)} core tests did not pass", bad)
    if excused:
        report.note(
            f"{name}: {len(excused)} core tests excused by the allowlist", excused
        )
    report.note(
        f"{name}: {len(core) - len(bad) - len(excused)} of {len(core)} core tests passed"
        f" ({len(ran)} tests ran)"
    )

    for node in sorted(allowed):
        if node not in inventory:
            report.error(f"{name}: allowlist entry is not in the inventory: {node}")
        elif entry_outcome(allowed[node]) == "not run":
            if "qgis" not in inventory[node]:
                report.error(f"{name}: 'not run' allowlist entry is not marked qgis: {node}")
        elif node not in core:
            report.error(f"{name}: allowlist entry is not a {kind} core test: {node}")
        elif outcomes.get(node) == "passed":
            report.error(f"{name}: allowlist entry now passes; remove it: {node}")

    if kind == "physics":
        markers = inventory.get(EXACT_REFERENCE_TEST)
        if markers is None:
            report.error(f"{name}: {EXACT_REFERENCE_TEST} is not in the inventory")
        elif "core" not in markers or "physics" not in markers:
            report.error(
                f"{name}: {EXACT_REFERENCE_TEST} lost its physics or core marker"
                f" (markers: {sorted(markers)})"
            )
        elif EXACT_REFERENCE_TEST in allowed:
            report.error(f"{name}: {EXACT_REFERENCE_TEST} may not be allowlisted")
        else:
            outcome = outcomes.get(EXACT_REFERENCE_TEST, "not run")
            if outcome == "passed":
                report.note(f"{name}: {EXACT_REFERENCE_TEST} passed")
            else:
                report.error(f"{name}: {EXACT_REFERENCE_TEST} {outcome}")


def entry_outcome(entry: dict[str, str]) -> str:
    """Return the outcome an allowlist entry excuses (``skipped`` by default)."""
    return entry.get("outcome", "skipped")


def run_guard(
    metrics_dir: Path,
    tier: str,
    api_pythons: list[str],
    physics_python: str = "cp312",
    allowlist_path: Path = DEFAULT_ALLOWLIST,
    bookend_core: bool = False,
) -> Report:
    """Run every check over the metrics under ``metrics_dir``."""
    report = Report()
    if tier not in READY_TIERS:
        report.error(
            f"tier {tier!r} is not a ready tier ({', '.join(READY_TIERS)}); the guard only judges those"
        )
        return report
    lanes, inventories = load_lanes(metrics_dir)
    if len(inventories) != 1:
        report.error(
            f"expected one test inventory, found {len(inventories)}",
            [str(path) for path in inventories],
        )
        return report
    inventory, collection_skips = load_inventory(inventories[0])
    report.note(f"inventory: {len(inventory)} tests from {inventories[0].name}")
    allowlist, allowed_skips = load_allowlist(allowlist_path)
    check_collection_skips(collection_skips, allowed_skips, report)

    required = [("physics", physics_python, *LINUX)] + [
        ("api", python, *LINUX) for python in api_pythons
    ]
    for key in required:
        if key not in lanes:
            report.error(
                f"{_lane_name(key)}: no metrics file (lane missing or not uploaded)"
            )
    for key in sorted(lanes):
        union = merge_shards(key, lanes[key], report)
        if union is None or key not in required:
            continue
        # A ready pull request runs the newer api bookend at core-bookend
        # (every core api test plus smoke); the build Python runs the full tier.
        lane_tier = (
            "core-bookend"
            if bookend_core and key[0] == "api" and key[1] != physics_python
            else tier
        )
        check_linux_lane(key, union, inventory, lane_tier, allowlist, report)
    return report


def _markdown(report: Report) -> str:
    status = "failed" if report.errors else "passed"
    lines = [f"## Core-test guard: {status}", ""]
    for heading, entries in (("Errors", report.errors), ("Notes", report.notes)):
        if not entries:
            continue
        lines += [f"### {heading}", "", "```", *entries, "```", ""]
    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    """Command-line entry point; returns the exit code."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("metrics_dir", type=Path)
    parser.add_argument("--tier", required=True)
    parser.add_argument(
        "--api-python",
        required=True,
        help='JSON array of the api lane\'s Python tags, e.g. ["cp312","cp315"]',
    )
    parser.add_argument("--physics-python", default="cp312")
    parser.add_argument("--allowlist", type=Path, default=DEFAULT_ALLOWLIST)
    parser.add_argument(
        "--bookend-core",
        action="store_true",
        help="api Pythons other than the physics Python ran the core-bookend tier",
    )
    parser.add_argument("--summary", type=Path, help="Append a Markdown summary here")
    args = parser.parse_args(argv)

    api_pythons = json.loads(args.api_python)
    report = run_guard(
        args.metrics_dir,
        args.tier,
        api_pythons,
        args.physics_python,
        args.allowlist,
        bookend_core=args.bookend_core,
    )
    for note in report.notes:
        print(f"[OK] {note}")
    for error in report.errors:
        print(f"[X] {error}")
    if args.summary:
        with args.summary.open("a", encoding="utf-8") as stream:
            stream.write(_markdown(report))
    if report.errors:
        print(f"Core-test guard failed with {len(report.errors)} error(s)")
        return 1
    print("Core-test guard passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
