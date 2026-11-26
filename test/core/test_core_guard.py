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

from scripts.lint.check_cost_markers import load_tests
from scripts.suews import ci_test_layout, ci_tiers
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
        json.dumps({
            "kind": "test-inventory",
            "nodes": inventory,
            "collection_skips": [
                {"node_id": m, "reason": "could not import 'x'"}
                for m in collection_skips
            ],
        }),
        encoding="utf-8",
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
        {
            "node_id": SKIPPY,
            "lane": "physics",
            "outcome": "skipped",
            "reason": "tool not built",
        }
    ]
    if allow_qgis:
        allowed.append({
            "node_id": "test/core/test_api.py::test_qgis_core",
            "lane": "api",
            "outcome": "not run",
            "reason": "needs QGIS on Windows",
        })
    (root / "allow.json").write_text(
        json.dumps({
            "allowed": allowed,
            "collection_skips": [
                {"node_id": m, "reason": "optional tool"} for m in allowed_skips
            ],
        }),
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
    report = run_guard(
        metrics, "standard", ["cp312"], allowlist_path=tmp_path / "allow.json"
    )
    assert report.errors == []
    assert any("allowlisted qgis core tests not run" in n for n in report.notes)
    errors = _guard(tmp_path / "b", _layout(tmp_path / "b", allow_qgis=False))
    assert any("marked qgis, which no tier before merge runs" in e for e in errors), (
        errors
    )


def test_collection_skips_need_an_allowlist_entry_each(tmp_path: Path) -> None:
    module = "test/core/test_optional.py"
    errors = _guard(tmp_path / "a", _layout(tmp_path / "a", collection_skips=(module,)))
    assert any("skipped at collection" in e and module in e for e in errors), errors
    listed = _layout(
        tmp_path / "b", collection_skips=(module,), allowed_skips=(module,)
    )
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
        s
        for s in gate["steps"]
        if s.get("name") == "Require the core-test guard at ready tiers"
    )
    assert inline["env"]["CORE_GUARD_RESULT"] == "${{ needs.core_guard.result }}"
    assert "exit 1" in inline["run"]
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
    metrics = _layout(tmp_path)
    inventory = next(metrics.rglob("inventory-*.json"))
    skipped: list[str] = []
    load_tests([inventory], skipped=skipped)
    assert skipped == [inventory.as_posix()]


def test_bookend_core_lane_is_judged_by_its_own_tier(tmp_path: Path) -> None:
    """A ready PR runs cp315 at core-bookend: all core api tests, not the full tier."""
    metrics = _layout(tmp_path)
    _write(
        metrics
        / "ci-metrics-api-cp315-manylinux-x86_64"
        / "api-cp315-manylinux-x86_64.json",
        _selected("api", "core-bookend"),
    )
    allow = tmp_path / "allow.json"
    ok = run_guard(
        metrics, "standard", ["cp312", "cp315"], allowlist_path=allow, bookend_core=True
    )
    assert ok.errors == []
    full = run_guard(metrics, "standard", ["cp312", "cp315"], allowlist_path=allow)
    assert any("api | cp315" in e and "did not run" in e for e in full.errors), (
        full.errors
    )


SCHEMA2_SKIPPY = "test/physics/test_skippy.py::test_needs_tool"
SCHEMA2_SMOKE_CORE = "test/physics/test_smoke.py::test_physics_smoke_core"
SCHEMA2_INVENTORY: dict[str, list[str]] = {
    EXACT_REFERENCE_TEST: ["contract", "core", "physics", "slow"],
    SCHEMA2_SMOKE_CORE: ["core", "physics", "smoke"],
    "test/physics/test_a.py::test_core_fast": ["core", "physics"],
    "test/physics/test_a.py::test_plain": ["physics"],
    "test/physics/test_a.py::test_slow_plain": ["physics", "slow"],
    SCHEMA2_SKIPPY: ["core", "physics"],
    "test/core/test_tooling.py::test_lint_core": ["core", "tooling"],
    "test/core/test_tooling.py::test_lint_plain": ["tooling"],
    "test/core/test_unit.py::test_unit_core": ["core", "unit", "os_compat"],
    "test/core/test_unit.py::test_unit_plain": ["unit"],
    "test/core/test_native.py::test_native_core": [
        "core",
        "native",
        "smoke",
        "smoke_bridge",
    ],
    "test/core/test_native.py::test_native_qgis_core": ["core", "native", "qgis"],
    **{f"test/core/test_native_many.py::test_case[{i}]": ["native"] for i in range(16)},
}


def _selected_v2(kind: str, tier: str = "standard") -> list[str]:
    expression = ci_tiers.tier_expression(kind, tier, schema=2)
    nodes = [n for n, m in SCHEMA2_INVENTORY.items() if ci_tiers.matches(expression, m)]
    if (
        kind == "physics"
        and tier == "physics-core"
        and EXACT_REFERENCE_TEST not in nodes
    ):
        nodes.append(EXACT_REFERENCE_TEST)
    return nodes


def _write_v2(
    path: Path, nodes: list[str], outcomes: dict[str, str] | None = None
) -> None:
    outcomes = outcomes or {}
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({
            "tests": [
                {
                    "node_id": n,
                    "markers": SCHEMA2_INVENTORY[n],
                    "outcome": outcomes.get(n, "passed"),
                }
                for n in nodes
            ]
        }),
        encoding="utf-8",
    )


def _layout_v2(
    root: Path,
    *,
    tier: str = "standard",
    native_shards: int = 0,
    physics_prefix: str = "physics",
    physics_outcomes: dict[str, str] | None = None,
    physics_nodes: list[str] | None = None,
    unit_outcomes: dict[str, str] | None = None,
    abi3_smoke_pythons: tuple[str, ...] = ("cp315",),
    abi3_smoke_outcomes: dict[str, str] | None = None,
    abi3_smoke_prefix: str = "abi3_smoke",
    include_os_compat: bool = False,
    os_compat_prefix: str = "os_compat",
    omit_lane: str | None = None,
    allow_qgis: bool = True,
    extra_allowed: list[dict[str, str]] | None = None,
    add_legacy_api_file: bool = False,
) -> Path:
    """Write one green Schema 2 multi-axis run, optionally injecting one defect."""
    metrics = root / "ci-metrics"
    inv_nodes = [{"node_id": n, "markers": m} for n, m in SCHEMA2_INVENTORY.items()]
    inv_dir = metrics / "ci-metrics-tooling-cp312-manylinux-x86_64"
    inv_dir.mkdir(parents=True)
    (inv_dir / "inventory-cp312-manylinux-x86_64.json").write_text(
        json.dumps({
            "kind": "test-inventory",
            "nodes": inv_nodes,
            "collection_skips": [],
        }),
        encoding="utf-8",
    )

    if omit_lane != "tooling":
        _write_v2(
            inv_dir / "tooling-cp312-manylinux-x86_64.json",
            _selected_v2("tooling", tier),
        )
    if omit_lane != "unit":
        _write_v2(
            metrics
            / "ci-metrics-unit-cp312-manylinux-x86_64"
            / "unit-cp312-manylinux-x86_64-pytest.json",
            _selected_v2("unit", tier),
            unit_outcomes,
        )
    if omit_lane != "native":
        native = _selected_v2("native", tier)
        if native_shards:
            for k in range(1, native_shards + 1):
                shard_nodes = [n for n in native if shard_of(n, native_shards) == k]
                _write_v2(
                    metrics
                    / f"ci-metrics-native-cp312-manylinux-x86_64-shard-{k}-of-{native_shards}"
                    / f"native-cp312-manylinux-x86_64-shard-{k}-of-{native_shards}.json",
                    shard_nodes,
                )
        else:
            _write_v2(
                metrics
                / "ci-metrics-native-cp312-manylinux-x86_64"
                / "native-cp312-manylinux-x86_64.json",
                native,
            )
    if omit_lane != "physics":
        phys = (
            physics_nodes
            if physics_nodes is not None
            else _selected_v2("physics", tier)
        )
        phys_outcomes = (
            {SCHEMA2_SKIPPY: "skipped", **(physics_outcomes or {})}
            if tier != "physics-core"
            else (physics_outcomes or {})
        )
        _write_v2(
            metrics
            / f"ci-metrics-{physics_prefix}-cp312-manylinux-x86_64"
            / f"{physics_prefix}-cp312-manylinux-x86_64-pytest.json",
            phys,
            phys_outcomes,
        )
    if omit_lane != "abi3_smoke":
        for py in abi3_smoke_pythons:
            _write_v2(
                metrics
                / f"ci-metrics-{abi3_smoke_prefix}-{py}-manylinux-x86_64"
                / f"{abi3_smoke_prefix}-{py}-manylinux-x86_64.json",
                _selected_v2("abi3_smoke", tier),
                abi3_smoke_outcomes,
            )
    if include_os_compat and omit_lane != "os_compat":
        _write_v2(
            metrics
            / f"ci-metrics-{os_compat_prefix}-cp312-manylinux-x86_64"
            / f"{os_compat_prefix}-cp312-manylinux-x86_64.json",
            _selected_v2("os_compat", tier),
        )
    if add_legacy_api_file:
        _write_v2(
            metrics
            / "ci-metrics-api-cp312-manylinux-x86_64"
            / "api-cp312-manylinux-x86_64.json",
            [],
        )

    allowed: list[dict[str, str]] = [
        {
            "node_id": SCHEMA2_SKIPPY,
            "lane": "physics",
            "outcome": "skipped",
            "reason": "tool not built",
        }
    ]
    if allow_qgis:
        allowed.append({
            "node_id": "test/core/test_native.py::test_native_qgis_core",
            "lane": "native",
            "outcome": "not run",
            "reason": "needs QGIS on Windows",
        })
    if extra_allowed:
        allowed.extend(extra_allowed)
    (root / "allow.json").write_text(
        json.dumps({"allowed": allowed, "collection_skips": []}),
        encoding="utf-8",
    )
    return metrics


def test_ci_tiers_dual_schema_expressions_and_cli(
    monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """ci_tiers preserves Schema 1 defaults and resolves Schema 2 axes and physics-core."""
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)
    assert ci_tiers.ready_tiers() == ("standard", "physics-full")
    assert ci_tiers.ready_tiers(2) == ("physics-core", "standard", "physics-full")
    assert (
        ci_tiers.tier_expression("physics", "standard")
        == ci_tiers.PHYSICS_TIERS["standard"]
    )
    assert (
        ci_tiers.tier_expression("physics", "physics-core", schema=2)
        == "physics and smoke and not (medium or slow)"
    )
    assert (
        ci_tiers.tier_expression("tooling", "physics-core", schema=2)
        == "tooling and (core or not slow) and not qgis"
    )
    assert (
        ci_tiers.tier_expression("unit", "standard", schema=2)
        == "unit and (core or not slow) and not qgis"
    )
    assert (
        ci_tiers.tier_expression("native", "standard", schema=2)
        == "native and (core or not slow) and not qgis"
    )
    assert (
        ci_tiers.tier_expression("os_compat", "standard", schema=2)
        == "os_compat and (core or not slow) and not qgis"
    )
    assert (
        ci_tiers.tier_expression("abi3_smoke", "physics-core", schema=2)
        == "smoke_bridge and not (medium or slow) and not qgis"
    )

    with pytest.raises(ValueError, match="unknown lane kind"):
        ci_tiers.tier_expression("tooling", "standard", schema=1)
    with pytest.raises(ValueError, match="unknown test tier"):
        ci_tiers.tier_expression("physics", "physics-core", schema=1)
    with pytest.raises(ValueError, match="unknown lane kind"):
        ci_tiers.tier_expression("api", "standard", schema=2)
    with pytest.raises(ValueError, match="unsupported axis schema"):
        ci_tiers.validate_schema(True)  # type: ignore[arg-type]

    assert ci_tiers.main(["--schema", "2", "physics", "physics-core"]) == 0
    assert (
        capsys.readouterr().out.strip() == "physics and smoke and not (medium or slow)"
    )

    monkeypatch.setenv("GATE_AXIS_SCHEMA", "2")
    assert ci_tiers.main(["native", "standard"]) == 0
    assert (
        capsys.readouterr().out.strip() == "native and (core or not slow) and not qgis"
    )

    monkeypatch.setenv("GATE_AXIS_SCHEMA", "invalid")
    assert ci_tiers.main(["physics", "standard"]) == 2
    assert ci_tiers.main(["--schema=9", "physics", "standard"]) == 2
    assert ci_tiers.main(["only-one"]) == 2


@pytest.mark.parametrize(
    ("tier", "native_shards", "physics_prefix", "abi3_prefix", "os_prefix"),
    [
        ("standard", 0, "physics", "abi3_smoke", "os_compat"),
        ("physics-full", 2, "physics", "abi3-smoke", "os-compat"),
        ("physics-core", 0, "physics", "abi3_smoke", "os_compat"),
        ("physics-core", 2, "physics-core", "abi3-smoke", "os-compat"),
    ],
)
def test_schema2_green_run_passes_across_ready_tiers(
    tmp_path: Path,
    tier: str,
    native_shards: int,
    physics_prefix: str,
    abi3_prefix: str,
    os_prefix: str,
) -> None:
    """Schema 2 accepts complete green runs for standard, physics-full, and physics-core."""
    metrics = _layout_v2(
        tmp_path,
        tier=tier,
        native_shards=native_shards,
        physics_prefix=physics_prefix,
        abi3_smoke_prefix=abi3_prefix,
        include_os_compat=True,
        os_compat_prefix=os_prefix,
        add_legacy_api_file=True,
    )
    report = run_guard(
        metrics,
        tier,
        allowlist_path=tmp_path / "allow.json",
        schema=2,
        abi3_smoke_pythons=["cp315"],
        require_os_compat=True,
    )
    assert report.errors == []


@pytest.mark.parametrize(
    ("tier", "layout_kwargs", "guard_kwargs", "expected"),
    [
        (
            "standard",
            {"omit_lane": "tooling"},
            {},
            "tooling | cp312 | manylinux x86_64: no metrics file",
        ),
        (
            "standard",
            {"omit_lane": "abi3_smoke"},
            {"abi3_smoke_pythons": ["cp315"]},
            "abi3_smoke | cp315 | manylinux x86_64: no metrics file",
        ),
        (
            "standard",
            {"include_os_compat": False},
            {"require_os_compat": True},
            "os_compat | cp312 | manylinux x86_64: no metrics file",
        ),
        (
            "standard",
            {"unit_outcomes": {"test/core/test_unit.py::test_unit_core": "skipped"}},
            {},
            "core tests did not pass",
        ),
        (
            "standard",
            {
                "abi3_smoke_outcomes": {
                    "test/core/test_native.py::test_native_core": "skipped"
                }
            },
            {"abi3_smoke_pythons": ["cp315"]},
            "abi3_smoke tests did not pass",
        ),
        (
            "physics-core",
            {"physics_outcomes": {EXACT_REFERENCE_TEST: "skipped"}},
            {},
            "test_sample_run_reproduces_reference_exactly skipped",
        ),
        (
            "physics-core",
            {
                "physics_nodes": [
                    n
                    for n in _selected_v2("physics", "physics-core")
                    if n != EXACT_REFERENCE_TEST
                ]
            },
            {},
            "did not run",
        ),
        (
            "physics-core",
            {
                "extra_allowed": [
                    {
                        "node_id": SCHEMA2_SMOKE_CORE,
                        "lane": "physics",
                        "outcome": "skipped",
                        "reason": "stale allowlist test",
                    }
                ]
            },
            {},
            "allowlist entry now passes",
        ),
        (
            "standard",
            {"physics_prefix": "physics-core"},
            {},
            "physics-core metrics file cannot satisfy tier 'standard'",
        ),
        (
            "standard",
            {"allow_qgis": False},
            {},
            "marked qgis, which no tier before merge runs",
        ),
    ],
    ids=[
        "missing-tooling-lane",
        "missing-abi3-smoke-lane",
        "missing-os-compat-lane",
        "skipped-unit-core",
        "skipped-abi3-smoke",
        "physics-core-exact-ref-skipped",
        "physics-core-exact-ref-missing",
        "physics-core-stale-selected-allowlist",
        "physics-core-file-rejected-for-standard",
        "native-qgis-unlisted",
    ],
)
def test_schema2_each_defect_fails_the_guard(
    tmp_path: Path,
    tier: str,
    layout_kwargs: dict[str, object],
    guard_kwargs: dict[str, object],
    expected: str,
) -> None:
    metrics = _layout_v2(tmp_path, tier=tier, **layout_kwargs)
    report = run_guard(
        metrics,
        tier,
        allowlist_path=tmp_path / "allow.json",
        schema=2,
        **guard_kwargs,
    )
    assert any(expected in error for error in report.errors), report.errors


def test_schema2_rejects_empty_axis_selection_draft_tiers_and_mixed_prefixes(
    tmp_path: Path,
) -> None:
    """Schema 2 fails closed on empty axes, draft tiers, and mixed filename prefixes."""
    # Schema 1 inventory fed into Schema 2 selects 0 tests for tooling/unit/native.
    v1_metrics = _layout(tmp_path / "v1")
    for lane_name in ("tooling", "unit", "native"):
        _write_v2(
            v1_metrics
            / f"ci-metrics-{lane_name}-cp312-manylinux-x86_64"
            / f"{lane_name}-cp312-manylinux-x86_64.json",
            [],
        )
    empty_report = run_guard(
        v1_metrics, "standard", allowlist_path=tmp_path / "v1" / "allow.json", schema=2
    )
    assert any("selected 0 tests from the inventory" in e for e in empty_report.errors)

    # Draft tiers are refused in Schema 2; physics-core is refused in Schema 1.
    v2_metrics = _layout_v2(tmp_path / "v2", tier="physics-core")
    draft_report = run_guard(
        v2_metrics, "smoke", allowlist_path=tmp_path / "v2" / "allow.json", schema=2
    )
    assert any("not a ready tier" in e for e in draft_report.errors)
    v1_phys_core = run_guard(
        v1_metrics,
        "physics-core",
        ["cp312"],
        allowlist_path=tmp_path / "v1" / "allow.json",
        schema=1,
    )
    assert any("not a ready tier" in e for e in v1_phys_core.errors)

    # Mixing physics-core and physics shard prefixes fails closed.
    mixed = _layout_v2(tmp_path / "mixed", tier="physics-core", omit_lane="physics")
    phys_nodes = _selected_v2("physics", "physics-core")
    _write_v2(
        mixed
        / "ci-metrics-physics-s1"
        / "physics-cp312-manylinux-x86_64-shard-1-of-2-pytest.json",
        [n for n in phys_nodes if shard_of(n, 2) == 1],
    )
    _write_v2(
        mixed
        / "ci-metrics-physics-s2"
        / "physics-core-cp312-manylinux-x86_64-shard-2-of-2-pytest.json",
        [n for n in phys_nodes if shard_of(n, 2) == 2],
    )
    mixed_report = run_guard(
        mixed,
        "physics-core",
        allowlist_path=tmp_path / "mixed" / "allow.json",
        schema=2,
    )
    assert any("mixed metrics filename prefixes" in e for e in mixed_report.errors)


def test_schema2_cli_and_cross_schema_flag_validation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """CLI supports --schema 2, GATE_AXIS_SCHEMA=2, api-python fallback, and rejects contradictions."""
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)
    metrics = _layout_v2(
        tmp_path,
        tier="physics-core",
        abi3_smoke_pythons=("cp315",),
        include_os_compat=True,
    )
    allow = str(tmp_path / "allow.json")
    assert (
        main([
            str(metrics),
            "--schema",
            "2",
            "--tier",
            "physics-core",
            "--abi3-smoke-python",
            '["cp315"]',
            "--require-os-compat",
            "--allowlist",
            allow,
        ])
        == 0
    )

    # GATE_AXIS_SCHEMA=2 with legacy --api-python derives abi3_smoke pythons != physics_python.
    monkeypatch.setenv("GATE_AXIS_SCHEMA", "2")
    assert (
        main([
            str(metrics),
            "--tier",
            "physics-core",
            "--api-python",
            '["cp312","cp315"]',
            "--allowlist",
            allow,
        ])
        == 0
    )

    # Contradictory flags and invalid inputs fail closed with exit code 2.
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)
    assert main([str(metrics), "--tier", "standard", "--allowlist", allow]) == 2
    assert (
        main([
            str(metrics),
            "--tier",
            "standard",
            "--api-python",
            '["cp312"]',
            "--require-os-compat",
            "--allowlist",
            allow,
        ])
        == 2
    )
    assert (
        main([
            str(metrics),
            "--schema",
            "2",
            "--tier",
            "standard",
            "--bookend-core",
            "--allowlist",
            allow,
        ])
        == 2
    )
    assert (
        main([
            str(metrics),
            "--schema",
            "2",
            "--tier",
            "standard",
            "--abi3-smoke-python",
            "not-json",
            "--allowlist",
            allow,
        ])
        == 2
    )


def test_schema1_omitted_api_pythons_fails_closed(tmp_path: Path) -> None:
    """Schema 1 rejects omitted or empty api_pythons even with valid physics evidence."""
    metrics = _layout(tmp_path)
    allow = tmp_path / "allow.json"
    with pytest.raises(ValueError, match="api_pythons must be non-empty for schema 1"):
        run_guard(metrics, "standard", allowlist_path=allow)
    with pytest.raises(ValueError, match="api_pythons must be non-empty for schema 1"):
        run_guard(metrics, "standard", [], allowlist_path=allow)
    assert (
        main([
            str(metrics),
            "--tier",
            "standard",
            "--api-python",
            "[]",
            "--allowlist",
            str(allow),
        ])
        == 2
    )


def test_schema2_layout_and_guard_agree_when_abi3_list_includes_build_python(
    tmp_path: Path,
) -> None:
    """Layout and guard both exclude physics_python and deduplicate explicit abi3_smoke lists."""
    explicit_pythons = ["cp312", "cp315", "cp315"]
    cells = ci_test_layout.abi3_smoke_cells(explicit_pythons, "physics-core", "cp312")
    assert [cell["python_version"] for cell in cells] == ["cp315"]

    metrics = _layout_v2(
        tmp_path,
        tier="physics-core",
        abi3_smoke_pythons=tuple(cell["python_version"] for cell in cells),
    )
    allow = tmp_path / "allow.json"
    report = run_guard(
        metrics,
        "physics-core",
        physics_python="cp312",
        allowlist_path=allow,
        schema=2,
        abi3_smoke_pythons=explicit_pythons,
    )
    assert report.errors == []
    assert (
        main([
            str(metrics),
            "--schema",
            "2",
            "--tier",
            "physics-core",
            "--physics-python",
            "cp312",
            "--abi3-smoke-python",
            json.dumps(explicit_pythons),
            "--allowlist",
            str(allow),
        ])
        == 0
    )
