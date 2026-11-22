"""Contract tests for the Linux core-test guard (scripts/suews/check_core_guard.py).

A green CI run only shows the guard passing; these fixtures show it failing
for each way a core test can go missing before a merge.
"""

from __future__ import annotations

import json
from pathlib import Path
import re

import pytest
import yaml

from scripts.suews import ci_tiers
from scripts.suews.check_core_guard import EXACT_REFERENCE_TEST, main, run_guard
from scripts.suews.pytest_shard import shard_of

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

PROJECT_ROOT = Path(__file__).resolve().parents[2]

SKIPPY = "test/physics/test_skippy.py::test_needs_tool"
INVENTORY = {
    EXACT_REFERENCE_TEST: ["contract", "core", "physics", "slow"],
    "test/physics/test_a.py::test_core_fast": ["core", "physics"],
    "test/physics/test_a.py::test_plain": ["physics"],
    "test/physics/test_a.py::test_slow_plain": ["physics", "slow"],
    SKIPPY: ["core", "physics"],
    "test/core/test_api.py::test_core_api": ["api", "core"],
    "test/core/test_api.py::test_plain_api": ["api"],
    "test/core/test_api.py::test_qgis_core": ["api", "core", "qgis"],
    **{f"test/core/test_many.py::test_case[{i}]": ["api"] for i in range(40)},
}


def _selected(kind: str, tier: str = "standard") -> list[str]:
    expression = ci_tiers.tier_expression(kind, tier)
    return [n for n, m in INVENTORY.items() if ci_tiers.matches(expression, m)]


def _record(node: str, outcome: str = "passed") -> dict[str, object]:
    return {"node_id": node, "markers": INVENTORY[node], "outcome": outcome}


def _write(
    path: Path, nodes: list[str], outcomes: dict[str, str] | None = None
) -> None:
    outcomes = outcomes or {}
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"tests": [_record(n, outcomes.get(n, "passed")) for n in nodes]}),
        encoding="utf-8",
    )


def _layout(
    root: Path,
    *,
    api_shards: int = 0,
    physics_outcomes: dict[str, str] | None = None,
    api_nodes: list[str] | None = None,
    drop_shard: int | None = None,
    duplicate: str | None = None,
    collection_skips: tuple[str, ...] = (),
    allowed_skips: tuple[str, ...] = (),
    allow_qgis: bool = True,
) -> Path:
    """Write one green standard-tier run, then apply one defect."""
    metrics = root / "ci-metrics"
    inventory = [{"node_id": n, "markers": m} for n, m in INVENTORY.items()]
    lane = metrics / "ci-metrics-api-cp312-manylinux-x86_64"
    lane.mkdir(parents=True)
    (lane / "inventory-cp312-manylinux-x86_64.json").write_text(
        json.dumps(
            {
                "kind": "test-inventory",
                "nodes": inventory,
                "collection_skips": [
                    {"node_id": m, "reason": "could not import 'x'"} for m in collection_skips
                ],
            }
        ), encoding="utf-8"
    )
    physics = _selected("physics")
    outcomes = {SKIPPY: "skipped", **(physics_outcomes or {})}
    _write(
        metrics
        / "ci-metrics-physics-cp312-manylinux-x86_64"
        / "physics-cp312-manylinux-x86_64-pytest.json",
        physics,
        outcomes,
    )
    api = api_nodes if api_nodes is not None else _selected("api")
    if api_shards:
        for k in range(1, api_shards + 1):
            if k == drop_shard:
                continue
            nodes = [n for n in api if shard_of(n, api_shards) == k]
            if duplicate and k == 1 and duplicate not in nodes:
                nodes.append(duplicate)
            _write(
                metrics
                / f"ci-metrics-api-cp312-manylinux-x86_64-shard-{k}-of-{api_shards}"
                / f"api-cp312-manylinux-x86_64-shard-{k}-of-{api_shards}.json",
                nodes,
            )
    else:
        _write(lane / "api-cp312-manylinux-x86_64.json", api)
    allowed = [
        {"node_id": SKIPPY, "lane": "physics", "outcome": "skipped", "reason": "tool not built"}
    ]
    if allow_qgis:
        allowed.append(
            {
                "node_id": "test/core/test_api.py::test_qgis_core",
                "lane": "api",
                "outcome": "not run",
                "reason": "needs QGIS on Windows",
            }
        )
    (root / "allow.json").write_text(
        json.dumps(
            {
                "allowed": allowed,
                "collection_skips": [
                    {"node_id": m, "reason": "optional tool"} for m in allowed_skips
                ],
            }
        ),
        encoding="utf-8",
    )
    return metrics


def _guard(root: Path, metrics: Path, tier: str = "standard") -> list[str]:
    report = run_guard(metrics, tier, ["cp312"], allowlist_path=root / "allow.json")
    return report.errors


@pytest.mark.parametrize("api_shards", [0, 2, 4])
def test_green_run_passes_unsharded_and_sharded(
    tmp_path: Path, api_shards: int
) -> None:
    assert _guard(tmp_path, _layout(tmp_path, api_shards=api_shards)) == []


@pytest.mark.parametrize(
    ("defect", "expected"),
    [
        (
            {"physics_outcomes": {"test/physics/test_a.py::test_core_fast": "skipped"}},
            "core tests did not pass",
        ),
        (
            {"physics_outcomes": {"test/physics/test_a.py::test_core_fast": "xfailed"}},
            "core tests did not pass",
        ),
        (
            {"physics_outcomes": {EXACT_REFERENCE_TEST: "skipped"}},
            "test_sample_run_reproduces_reference_exactly skipped",
        ),
        ({"physics_outcomes": {SKIPPY: "passed"}}, "allowlist entry now passes"),
        (
            {
                "api_nodes": [
                    n
                    for n in _selected("api")
                    if n != "test/core/test_api.py::test_core_api"
                ]
            },
            "did not run",
        ),
        ({"api_shards": 4, "drop_shard": 3}, "expected shards 1..4"),
        (
            {"api_shards": 2, "duplicate": "test/core/test_many.py::test_case[0]"},
            "more than one shard",
        ),
    ],
    ids=[
        "skipped-core",
        "xfailed-core",
        "exact-reference-skipped",
        "stale-allowlist",
        "omitted-core-node",
        "missing-shard",
        "duplicate-across-shards",
    ],
)
def test_each_defect_fails_the_guard(
    tmp_path: Path, defect: dict[str, object], expected: str
) -> None:
    metrics = _layout(tmp_path, **defect)
    errors = _guard(tmp_path, metrics)
    assert any(expected in error for error in errors), errors


def test_missing_lane_and_missing_inventory_fail(tmp_path: Path) -> None:
    metrics = _layout(tmp_path)
    report = run_guard(
        metrics, "standard", ["cp312", "cp315"], allowlist_path=tmp_path / "allow.json"
    )
    assert any(
        "api | cp315 | manylinux x86_64: no metrics file" in e for e in report.errors
    )
    next(metrics.rglob("inventory-*.json")).unlink()
    assert any("expected one test inventory" in e for e in _guard(tmp_path, metrics))


def test_draft_tiers_are_refused(tmp_path: Path) -> None:
    metrics = _layout(tmp_path)
    assert any("not a ready tier" in e for e in _guard(tmp_path, metrics, tier="smoke"))


def test_qgis_core_tests_need_an_allowlist_entry_each(tmp_path: Path) -> None:
    metrics = _layout(tmp_path)
    report = run_guard(metrics, "standard", ["cp312"], allowlist_path=tmp_path / "allow.json")
    assert report.errors == []
    assert any("allowlisted qgis core tests not run" in n for n in report.notes)
    errors = _guard(tmp_path / "b", _layout(tmp_path / "b", allow_qgis=False))
    assert any("marked qgis, which no tier before merge runs" in e for e in errors), errors


def test_collection_skips_need_an_allowlist_entry_each(tmp_path: Path) -> None:
    module = "test/core/test_optional.py"
    errors = _guard(tmp_path / "a", _layout(tmp_path / "a", collection_skips=(module,)))
    assert any("skipped at collection" in e and module in e for e in errors), errors
    listed = _layout(tmp_path / "b", collection_skips=(module,), allowed_skips=(module,))
    assert _guard(tmp_path / "b", listed) == []
    stale = _layout(tmp_path / "c", allowed_skips=(module,))
    assert any("no longer skip" in e for e in _guard(tmp_path / "c", stale))



def test_cli_exit_codes(tmp_path: Path) -> None:
    metrics = _layout(tmp_path)
    args = [
        str(metrics),
        "--tier",
        "standard",
        "--api-python",
        '["cp312"]',
        "--allowlist",
        str(tmp_path / "allow.json"),
    ]
    assert main(args) == 0
    _write(
        metrics
        / "ci-metrics-physics-cp312-manylinux-x86_64"
        / "physics-cp312-manylinux-x86_64-pytest.json",
        [n for n in _selected("physics") if n != EXACT_REFERENCE_TEST],
        {SKIPPY: "skipped"},
    )
    assert main(args) == 1


def test_repository_allowlist_entries_name_physics_core_tests() -> None:
    document = json.loads(
        (PROJECT_ROOT / "scripts/suews/core_guard_allowlist.json").read_text(
            encoding="utf-8"
        )
    )
    for entry in document["allowed"]:
        assert entry["lane"] in {"physics", "api"}
        assert entry["outcome"] in {"skipped", "not run"}
        assert entry["reason"].strip()
        assert entry["node_id"] != EXACT_REFERENCE_TEST


def test_tier_tables_match_the_workflow_expressions() -> None:
    """The guard judges each lane by the expression that lane actually ran."""
    action = (PROJECT_ROOT / ".github/actions/build-suews/action.yml").read_text(
        encoding="utf-8"
    )
    physics = dict(
        re.findall(r"inputs\.test_tier == '([a-z-]+)' && '-m \"?([^'\"]+)\"?'", action)
    )
    assert physics == ci_tiers.PHYSICS_TIERS

    api_workflow = yaml.safe_load(
        (
            PROJECT_ROOT / ".github/workflows/test-api-cross-python-reusable.yml"
        ).read_text(encoding="utf-8")
    )
    steps = api_workflow["jobs"]["api_cross_python"]["steps"]
    script = next(s["run"] for s in steps if s.get("id") == "marker")
    api = dict(re.findall(r"^\s*([a-z-]+)\)\s+EXPR='([^']+)'", script, flags=re.M))
    assert api == ci_tiers.API_TIERS


def test_guard_is_wired_into_the_gate() -> None:
    workflow = yaml.safe_load(
        (PROJECT_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    jobs = workflow["jobs"]
    guard = jobs["core_guard"]
    assert guard["name"] == "Core-test guard (Linux)"
    assert {"determine_matrix", "build_wheels"} <= set(guard["needs"])
    gate = jobs["pr-gate"]
    assert "core_guard" in gate["needs"]
    env = next(
        s["env"] for s in gate["steps"] if s.get("name") == "Check build results"
    )
    assert env["CORE_GUARD_RESULT"] == "${{ needs.core_guard.result }}"
    assert env["TEST_TIER"] == "${{ needs.determine_matrix.outputs.test_tier }}"
    # The gate script is read from the base branch, so the workflow enforces
    # the guard itself as well.
    inline = next(
        s for s in gate["steps"] if s.get("name") == "Require the core-test guard at ready tiers"
    )
    assert inline["env"]["CORE_GUARD_RESULT"] == "${{ needs.core_guard.result }}"
    assert 'exit 1' in inline["run"]
    needs_map = json.loads(
        (PROJECT_ROOT / ".github/ci-metrics-needs.json").read_text(encoding="utf-8")
    )
    assert "Core-test guard (Linux)" in needs_map["PR build validation"]
    api_workflow = (
        PROJECT_ROOT / ".github/workflows/test-api-cross-python-reusable.yml"
    ).read_text(encoding="utf-8")
    assert "scripts.suews.pytest_inventory" in api_workflow


def test_inventory_is_not_read_as_pytest_metrics(tmp_path: Path) -> None:
    """The inventory sits beside the api metrics, which other readers glob."""
    from scripts.lint.check_cost_markers import load_tests

    metrics = _layout(tmp_path)
    inventory = next(metrics.rglob("inventory-*.json"))
    skipped: list[str] = []
    load_tests([inventory], skipped=skipped)
    assert skipped == [inventory.as_posix()]
