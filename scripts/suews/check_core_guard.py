#!/usr/bin/env python3
"""Check that every core test passed on Linux before a merge.

The merge rule (22 Nov 2026) is that every test marked ``core`` runs, and
passes, on Linux before a pull request merges; the merge queue re-runs the
standard tier for that reason. A green job only shows that the tests it
selected did not fail, so this guard checks node ids instead:

* the inventory (``scripts/suews/pytest_inventory.py``, recorded on Linux
  with ``--collect-only`` and no ``-m``) lists every test and its markers,
  independently of the tier expression and of any shard split;
* each required Linux lane (Schema 1: ``physics`` on the build Python and
  ``api`` on every requested Python; Schema 2: ``tooling``, ``unit``,
  ``native`` and ``physics`` on Linux, plus any requested ``abi3_smoke`` and
  ``os_compat`` lanes) must have run exactly the tests its tier expression
  selects from that inventory: the shards of a lane must all be present, must
  be disjoint, each test must sit in the shard its node id hashes to, and
  their union must equal the expected selection;
* every core test of the lane's axis must have the outcome ``passed``. A
  skip, an xfail, a failure or an absence fails the guard, unless the exact
  node id is listed in ``scripts/suews/core_guard_allowlist.json`` with its
  reason; an allowlist entry that no longer applies also fails, so the list
  cannot go stale;
* ``test_sample_run_reproduces_reference_exactly`` is named explicitly: it
  must be in the inventory, marked core, and passed in the physics lane
  (including under the Schema 2 ``physics-core`` tier), because outside its
  reference environment it skips instead of failing.

Two kinds of test cannot run in a Linux lane and so need an explicit
allowlist entry each: core tests marked ``qgis`` (no tier before merge runs
``qgis``; they need the QGIS runtime on Windows), listed by node id with the
outcome ``not run``; and test modules that skip while they are collected
(``pytest.importorskip``), which contribute no node ids at all and are listed
by module path.

The guard runs on ready tiers only (``standard`` and ``physics-full`` in
Schema 1; ``physics-core``, ``standard`` and ``physics-full`` in Schema 2):
draft tiers deliberately run a subset. A pull request whose paths need no
build (``needs-build`` false and no physics or numerics label, in practice
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
    from .ci_tiers import (
        SCHEMA_V1,
        SCHEMA_V2,
        matches,
        ready_tiers,
        schema_from_env,
        tier_expression,
        validate_schema,
    )
    from .pytest_shard import shard_of
else:
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    from ci_tiers import (
        SCHEMA_V1,
        SCHEMA_V2,
        matches,
        ready_tiers,
        schema_from_env,
        tier_expression,
        validate_schema,
    )
    from pytest_shard import shard_of

EXACT_REFERENCE_TEST = (
    "test/core/test_sample_output_exact.py::"
    "test_sample_run_reproduces_reference_exactly"
)
LINUX = ("manylinux", "x86_64")
DEFAULT_ALLOWLIST = Path(__file__).resolve().parent / "core_guard_allowlist.json"

# Schema 1: physics-<py>-<platform>-<arch>[-shard-K-of-N]-pytest.json (wheel
# and physics jobs) and api-<py>-<platform>-<arch>[-shard-K-of-N].json (api).
_LANE_FILE = re.compile(
    r"^(?P<kind>physics|api)-(?P<python>cp3\d+)-(?P<platform>[a-z]+)-(?P<arch>[A-Za-z0-9_]+)"
    r"(?:-shard-(?P<index>\d+)-of-(?P<total>\d+))?(?P<suffix>-pytest)?\.json$"
)
# Schema 2: tooling/unit/native/physics/physics-core/os_compat/abi3_smoke.
_LANE_FILE_V2 = re.compile(
    r"^(?P<kind>physics-core|physics|tooling|unit|native|os[_-]compat|abi3[_-]smoke)"
    r"-(?P<python>cp3\d+)-(?P<platform>[a-z]+)-(?P<arch>[A-Za-z0-9_]+)"
    r"(?:-shard-(?P<index>\d+)-of-(?P<total>\d+))?(?P<suffix>-pytest)?\.json$"
)
_KIND_ALIASES_V2: dict[str, str] = {
    "physics-core": "physics",
    "physics": "physics",
    "tooling": "tooling",
    "unit": "unit",
    "native": "native",
    "os_compat": "os_compat",
    "os-compat": "os_compat",
    "abi3_smoke": "abi3_smoke",
    "abi3-smoke": "abi3_smoke",
}
_QGIS_EXCLUDING_LANES = frozenset({"api", "tooling", "unit", "native", "os_compat"})
_INVENTORY_FILE = re.compile(r"^inventory-.*\.json$")
_MAX_LISTED = 25


@dataclass
class Shard:
    """Outcomes collected from one shard (or unsharded) metrics file."""

    path: Path
    index: int | None
    total: int | None
    outcomes: dict[str, str]
    raw_kind: str = ""


@dataclass
class Report:
    """Structured guard verdict collecting errors and summary notes."""

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


def _dedupe_tags(tags: list[str], label: str) -> list[str]:
    """Validate and deduplicate Python tag strings preserving order."""
    seen: set[str] = set()
    result: list[str] = []
    for tag in tags:
        if not isinstance(tag, str) or not tag.strip():
            raise ValueError(f"{label} entries must be non-empty strings, got {tag!r}")
        if tag not in seen:
            seen.add(tag)
            result.append(tag)
    return result


def _parse_python_list(raw: str, flag: str) -> list[str]:
    """Parse a JSON array of non-empty Python tags."""
    try:
        parsed = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise ValueError(f"{flag} is not valid JSON: {exc}") from None
    if not isinstance(parsed, list):
        raise ValueError(f"{flag} must be a JSON array of strings, got {raw!r}")
    return _dedupe_tags(parsed, flag)


def load_lanes(
    metrics_dir: Path,
    *,
    schema: int = SCHEMA_V1,
) -> tuple[dict[tuple[str, str, str, str], list[Shard]], list[Path]]:
    """Group the pytest metrics files under ``metrics_dir`` by lane for ``schema``."""
    resolved = validate_schema(schema)
    pattern = _LANE_FILE if resolved == SCHEMA_V1 else _LANE_FILE_V2
    lanes: dict[tuple[str, str, str, str], list[Shard]] = defaultdict(list)
    inventories: list[Path] = []
    for path in sorted(metrics_dir.rglob("*.json")):
        if _INVENTORY_FILE.match(path.name):
            inventories.append(path)
            continue
        match = pattern.match(path.name)
        if match is None:
            continue
        raw_kind = match["kind"]
        if resolved == SCHEMA_V1:
            kind = raw_kind
            # The wheel job writes physics-...-pytest.json beside phases and
            # wheel-job files; the api lane writes api-....json with no suffix.
            if (kind == "physics") != bool(match["suffix"]):
                continue
        else:
            kind = _KIND_ALIASES_V2[raw_kind]
            # Wheel/physics lanes always write -pytest.json beside phases and
            # wheel-job files; non-physics Schema 2 lanes accept either suffix.
            if kind == "physics" and not match["suffix"]:
                continue
        document = json.loads(path.read_text(encoding="utf-8"))
        outcomes = {
            record["node_id"]: record.get("outcome") or "missing"
            for record in document.get("tests", [])
        }
        index = int(match["index"]) if match["index"] else None
        total = int(match["total"]) if match["total"] else None
        key = (kind, match["python"], match["platform"], match["arch"])
        lanes[key].append(Shard(path, index, total, outcomes, raw_kind=raw_kind))
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
    unlisted = [
        f"{node}: {reason}"
        for node, reason in sorted(skips.items())
        if node not in listed
    ]
    if unlisted:
        report.error(
            f"{len(unlisted)} test modules skipped at collection, so none of their tests"
            " (core or not) are in the inventory; fix the dependency or allowlist the module",
            unlisted,
        )
    stale = sorted(node for node in listed if node not in skips)
    if stale:
        report.error(
            "collection-skip allowlist entries no longer skip; remove them", stale
        )
    excused = sorted(node for node in skips if node in listed)
    if excused:
        report.note(
            f"{len(excused)} modules skipped at collection, allowlisted", excused
        )


def _lane_name(key: tuple[str, str, str, str]) -> str:
    kind, python, platform, arch = key
    return f"{kind} | {python} | {platform} {arch}"


def merge_shards(
    key: tuple[str, str, str, str], shards: list[Shard], report: Report
) -> dict[str, str] | None:
    """Return the union of a lane's outcomes after checking its shard split."""
    name = _lane_name(key)
    raw_kinds = {shard.raw_kind for shard in shards if shard.raw_kind}
    if len(raw_kinds) > 1:
        report.error(
            f"{name}: mixed metrics filename prefixes: {sorted(raw_kinds)}",
            [str(shard.path) for shard in shards],
        )
        return None
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


def _check_abi3_smoke_lane(
    name: str,
    expression: str,
    expected: set[str],
    outcomes: dict[str, str],
    inventory: dict[str, set[str]],
    allowed: dict[str, dict[str, str]],
    report: Report,
) -> None:
    """Verify every expected abi3_smoke test passed and validate its allowlist."""
    bad: list[str] = []
    excused: list[str] = []
    for node in sorted(expected):
        outcome = outcomes.get(node, "not run")
        if outcome == "passed":
            continue
        entry = allowed.get(node)
        if (
            entry is not None
            and outcome == entry_outcome(entry)
            and outcome != "not run"
        ):
            excused.append(
                f"{node} ({outcome}: {entry.get('reason', 'no reason given')})"
            )
            continue
        bad.append(f"{node}: {outcome}")
    if bad:
        report.error(f"{name}: {len(bad)} abi3_smoke tests did not pass", bad)
    if excused:
        report.note(
            f"{name}: {len(excused)} abi3_smoke tests excused by the allowlist", excused
        )
    report.note(
        f"{name}: {len(expected) - len(bad) - len(excused)} of {len(expected)} abi3_smoke tests passed"
        f" ({len(outcomes)} tests ran)"
    )
    for node in sorted(allowed):
        if node not in inventory:
            report.error(f"{name}: allowlist entry is not in the inventory: {node}")
        elif entry_outcome(allowed[node]) == "not run":
            report.error(
                f"{name}: abi3_smoke does not accept 'not run' allowlist entries: {node}"
            )
        elif node not in expected:
            report.error(
                f"{name}: allowlist entry is not selected by -m '{expression}': {node}"
            )
        elif outcomes.get(node) == "passed":
            report.error(f"{name}: allowlist entry now passes; remove it: {node}")


def _strip_qgis_core(
    name: str,
    core: set[str],
    inventory: dict[str, set[str]],
    allowed: dict[str, dict[str, str]],
    report: Report,
) -> set[str]:
    """Require an explicit 'not run' allowlist entry for each qgis core test."""
    # No tier before merge runs qgis, so each such core test is an explicit
    # allowlist entry (outcome "not run"), never a blanket exemption.
    qgis = sorted(node for node in core if "qgis" in inventory[node])
    unlisted = [
        node for node in qgis if allowed.get(node, {}).get("outcome") != "not run"
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
    return core - set(qgis)


def _check_core_outcomes(
    name: str,
    checked_core: set[str],
    outcomes: dict[str, str],
    allowed: dict[str, dict[str, str]],
    report: Report,
) -> None:
    """Verify every checked core test passed unless explicitly allowlisted."""
    bad: list[str] = []
    excused: list[str] = []
    for node in sorted(checked_core):
        outcome = outcomes.get(node, "not run")
        if outcome == "passed":
            continue
        entry = allowed.get(node)
        if entry is not None and outcome == entry_outcome(entry):
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
        f"{name}: {len(checked_core) - len(bad) - len(excused)} of {len(checked_core)} core tests passed"
        f" ({len(outcomes)} tests ran)"
    )


def _check_allowlist_entries(
    name: str,
    kind: str,
    inventory: dict[str, set[str]],
    all_lane_core: set[str],
    core: set[str],
    checked_core: set[str],
    outcomes: dict[str, str],
    allowed: dict[str, dict[str, str]],
    report: Report,
) -> None:
    """Fail on stale, unknown, or misclassified allowlist entries for a lane."""
    for node in sorted(allowed):
        if node not in inventory:
            report.error(f"{name}: allowlist entry is not in the inventory: {node}")
        elif entry_outcome(allowed[node]) == "not run":
            if kind not in _QGIS_EXCLUDING_LANES:
                report.error(
                    f"{name}: {kind} lane does not accept 'not run' allowlist entries: {node}"
                )
            elif "qgis" not in inventory[node]:
                report.error(
                    f"{name}: 'not run' allowlist entry is not marked qgis: {node}"
                )
            elif node not in all_lane_core:
                report.error(
                    f"{name}: 'not run' allowlist entry is not a {kind} core test: {node}"
                )
        elif node not in core:
            report.error(f"{name}: allowlist entry is not a {kind} core test: {node}")
        elif node in checked_core and outcomes.get(node) == "passed":
            report.error(f"{name}: allowlist entry now passes; remove it: {node}")


def _check_exact_reference(
    name: str,
    inventory: dict[str, set[str]],
    outcomes: dict[str, str],
    allowed: dict[str, dict[str, str]],
    report: Report,
) -> None:
    """Verify the exact sample-output reference witness is present, unallowlisted, and passed."""
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


def check_linux_lane(
    key: tuple[str, str, str, str],
    outcomes: dict[str, str],
    inventory: dict[str, set[str]],
    tier: str,
    allowlist: list[dict[str, str]],
    report: Report,
    *,
    schema: int = SCHEMA_V1,
) -> None:
    """Check one Linux lane's selection and its core-test outcomes."""
    resolved = validate_schema(schema)
    kind = key[0]
    name = _lane_name(key)
    expression = tier_expression(kind, tier, schema=resolved)
    expected = {
        node for node, markers in inventory.items() if matches(expression, markers)
    }
    is_physics_core = (
        resolved == SCHEMA_V2 and kind == "physics" and tier == "physics-core"
    )
    if is_physics_core and EXACT_REFERENCE_TEST in inventory:
        expected.add(EXACT_REFERENCE_TEST)
    if resolved == SCHEMA_V2 and not expected:
        report.error(f"{name}: -m '{expression}' selected 0 tests from the inventory")
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

    allowed = {
        entry["node_id"]: entry for entry in allowlist if entry.get("lane") == kind
    }
    if kind == "abi3_smoke":
        _check_abi3_smoke_lane(
            name, expression, expected, outcomes, inventory, allowed, report
        )
        return

    all_lane_core = {
        node
        for node, markers in inventory.items()
        if kind in markers and "core" in markers
    }
    core = (
        _strip_qgis_core(name, all_lane_core, inventory, allowed, report)
        if kind in _QGIS_EXCLUDING_LANES
        else set(all_lane_core)
    )
    if is_physics_core:
        # physics-core intentionally runs only smoke plus the exact reference
        # witness; verify every core test inside that subset without demanding
        # full-physics core tests that run on the four full-physics triggers.
        checked_core = core & expected
    else:
        checked_core = core
        unselected = sorted(core - expected)
        if unselected:
            report.error(
                f"{name}: {len(unselected)} core tests are not selected by -m '{expression}'",
                unselected,
            )

    _check_core_outcomes(name, checked_core, outcomes, allowed, report)
    _check_allowlist_entries(
        name,
        kind,
        inventory,
        all_lane_core,
        core,
        checked_core,
        outcomes,
        allowed,
        report,
    )
    if kind == "physics":
        _check_exact_reference(name, inventory, outcomes, allowed, report)


def entry_outcome(entry: dict[str, str]) -> str:
    """Return the outcome an allowlist entry excuses (``skipped`` by default)."""
    return entry.get("outcome", "skipped")


def _validate_python_tag(tag: str, label: str) -> str:
    """Require a non-empty Python tag string."""
    if not isinstance(tag, str) or not tag.strip():
        raise ValueError(f"{label} must be a non-empty string, got {tag!r}")
    return tag


def _required_lanes(
    *,
    schema: int,
    api_pythons: list[str] | None,
    physics_python: str,
    abi3_smoke_pythons: list[str] | None,
    require_os_compat: bool,
    source_python: str | None,
) -> list[tuple[str, str, str, str]]:
    """Build the ordered list of required Linux lane keys for ``schema``."""
    _validate_python_tag(physics_python, "physics_python")
    if schema == SCHEMA_V1:
        if not api_pythons:
            raise ValueError("api_pythons must be non-empty for schema 1")
        pythons = _dedupe_tags(list(api_pythons), "api_pythons")
        return [("physics", physics_python, *LINUX)] + [
            ("api", python, *LINUX) for python in pythons
        ]
    src_python = (
        physics_python
        if source_python is None
        else _validate_python_tag(source_python, "source_python")
    )
    raw_smoke = abi3_smoke_pythons if abi3_smoke_pythons is not None else api_pythons
    smoke_label = (
        "abi3_smoke_pythons" if abi3_smoke_pythons is not None else "api_pythons"
    )
    smoke_pythons = (
        [
            python
            for python in _dedupe_tags(list(raw_smoke), smoke_label)
            if python != physics_python
        ]
        if raw_smoke is not None
        else []
    )
    required: list[tuple[str, str, str, str]] = [
        ("tooling", src_python, *LINUX),
        ("unit", src_python, *LINUX),
        ("native", physics_python, *LINUX),
        ("physics", physics_python, *LINUX),
        *(("abi3_smoke", python, *LINUX) for python in smoke_pythons),
    ]
    if require_os_compat:
        required.append(("os_compat", physics_python, *LINUX))
    return required


def run_guard(
    metrics_dir: Path,
    tier: str,
    api_pythons: list[str] | None = None,
    physics_python: str = "cp312",
    allowlist_path: Path = DEFAULT_ALLOWLIST,
    bookend_core: bool = False,
    *,
    schema: int = SCHEMA_V1,
    abi3_smoke_pythons: list[str] | None = None,
    require_os_compat: bool = False,
    source_python: str | None = None,
) -> Report:
    """Run every check over the metrics under ``metrics_dir``."""
    resolved = validate_schema(schema)
    if resolved == SCHEMA_V1:
        if not api_pythons:
            raise ValueError("api_pythons must be non-empty for schema 1")
        if (
            abi3_smoke_pythons is not None
            or require_os_compat
            or source_python is not None
        ):
            raise ValueError(
                "abi3_smoke_pythons, require_os_compat and source_python "
                "are only valid for schema 2"
            )
    elif bookend_core:
        raise ValueError("bookend_core is only valid for schema 1")

    report = Report()
    allowed_tiers = ready_tiers(resolved)
    if tier not in allowed_tiers:
        report.error(
            f"tier {tier!r} is not a ready tier ({', '.join(allowed_tiers)}); the guard only judges those"
        )
        return report
    lanes, inventories = load_lanes(metrics_dir, schema=resolved)
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

    required = _required_lanes(
        schema=resolved,
        api_pythons=api_pythons,
        physics_python=physics_python,
        abi3_smoke_pythons=abi3_smoke_pythons,
        require_os_compat=require_os_compat,
        source_python=source_python,
    )
    for key in required:
        if key not in lanes:
            report.error(
                f"{_lane_name(key)}: no metrics file (lane missing or not uploaded)"
            )
    for key in sorted(lanes):
        union = merge_shards(key, lanes[key], report)
        if union is None or key not in required:
            continue
        if (
            resolved == SCHEMA_V2
            and key[0] == "physics"
            and tier != "physics-core"
            and any(shard.raw_kind == "physics-core" for shard in lanes[key])
        ):
            report.error(
                f"{_lane_name(key)}: physics-core metrics file cannot satisfy tier {tier!r}",
                [
                    str(shard.path)
                    for shard in lanes[key]
                    if shard.raw_kind == "physics-core"
                ],
            )
            continue
        # In Schema 1, a ready pull request runs the newer api bookend at
        # core-bookend (every core api test plus smoke); the build Python runs
        # the full tier.
        lane_tier = (
            "core-bookend"
            if resolved == SCHEMA_V1
            and bookend_core
            and key[0] == "api"
            and key[1] != physics_python
            else tier
        )
        check_linux_lane(
            key, union, inventory, lane_tier, allowlist, report, schema=resolved
        )
    return report


def _markdown(report: Report) -> str:
    status = "failed" if report.errors else "passed"
    lines = [f"## Core-test guard: {status}", ""]
    for heading, entries in (("Errors", report.errors), ("Notes", report.notes)):
        if not entries:
            continue
        lines += [f"### {heading}", "", "```", *entries, "```", ""]
    return "\n".join(lines) + "\n"


def _run_guard_from_args(args: argparse.Namespace) -> Report:
    """Resolve schema and Python-tag flags from ``args`` and run the guard."""
    schema = schema_from_env() if args.schema is None else validate_schema(args.schema)
    if schema == SCHEMA_V1 and args.api_python is None:
        raise ValueError("--api-python is required for schema 1")
    api_pythons = (
        _parse_python_list(args.api_python, "--api-python")
        if args.api_python is not None
        else None
    )
    abi3_smoke_pythons = (
        _parse_python_list(args.abi3_smoke_python, "--abi3-smoke-python")
        if args.abi3_smoke_python is not None
        else None
    )
    return run_guard(
        args.metrics_dir,
        args.tier,
        api_pythons,
        args.physics_python,
        args.allowlist,
        bookend_core=args.bookend_core,
        schema=schema,
        abi3_smoke_pythons=abi3_smoke_pythons,
        require_os_compat=args.require_os_compat,
        source_python=args.source_python,
    )


def main(argv: list[str] | None = None) -> int:
    """Command-line entry point; returns the exit code."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("metrics_dir", type=Path)
    parser.add_argument(
        "--schema",
        type=int,
        choices=(SCHEMA_V1, SCHEMA_V2),
        default=None,
        help="Axis schema version (1 or 2; defaults to GATE_AXIS_SCHEMA or 1)",
    )
    parser.add_argument("--tier", required=True)
    parser.add_argument(
        "--api-python",
        default=None,
        help='JSON array of the api lane\'s Python tags, e.g. ["cp312","cp315"] (required for schema 1)',
    )
    parser.add_argument("--physics-python", default="cp312")
    parser.add_argument("--allowlist", type=Path, default=DEFAULT_ALLOWLIST)
    parser.add_argument(
        "--bookend-core",
        action="store_true",
        help="Schema 1 only: api Pythons other than the physics Python ran the core-bookend tier",
    )
    parser.add_argument(
        "--abi3-smoke-python",
        default=None,
        help='Schema 2 only: JSON array of Python tags for the abi3_smoke lane, e.g. ["cp315"]',
    )
    parser.add_argument(
        "--source-python",
        default=None,
        help="Schema 2 only: Python tag for the source-checkout tooling and unit lanes (defaults to --physics-python)",
    )
    parser.add_argument(
        "--require-os-compat",
        action="store_true",
        help="Schema 2 only: require a Linux os_compat lane in metrics_dir",
    )
    parser.add_argument("--summary", type=Path, help="Append a Markdown summary here")
    args = parser.parse_args(argv)

    try:
        report = _run_guard_from_args(args)
    except ValueError as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 2

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
