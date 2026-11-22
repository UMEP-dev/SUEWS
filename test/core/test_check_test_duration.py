"""Contract tests for the report-only CI test-duration guard.

`scripts/lint/check_test_duration.py` normalises each Linux cp312 metrics
artefact (including multi-shard API runs) against a committed baseline
(`scripts/lint/test_duration_baseline.json`), aggregates shards per lane, and
reports (a) new or changed tests over the cost-marker thresholds without the
matching marker and (b) normalised lane totals more than 10% above baseline.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import sys
from typing import Any

import pytest
import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.lint import check_cost_markers as cost_markers  # noqa: E402
from scripts.lint import check_test_duration as guard  # noqa: E402

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


def _record(
    node_id: str,
    *,
    call: float,
    setup: float = 0.0,
    markers: tuple[str, ...] = ("api",),
    outcome: str = "passed",
    reasons: dict[str, str] | None = None,
) -> dict[str, Any]:
    phases = {"setup": setup, "call": call, "teardown": 0.0, "total": setup + call}
    return {
        "cpu_method": "os.times",
        "cpu_seconds": phases,
        "marker_reasons": reasons or {},
        "markers": sorted(markers),
        "node_id": node_id,
        "outcome": outcome,
        "wall_seconds": {key: value * 1.2 for key, value in phases.items()},
    }


def _write_artefact(path: Path, records: list[dict[str, Any]]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"schema_version": 2, "tests": records}), encoding="utf-8"
    )
    return path


def _shard_of(node_id: str, total: int) -> int:
    digest = hashlib.sha256(node_id.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], "big") % total + 1


def _synthetic_baseline() -> guard.DurationBaseline:
    api_refs = {f"test/core/test_ref_{i:02d}.py::test_r": 2.0 for i in range(40)}
    phys_refs = {f"test/physics/test_ref_{i:02d}.py::test_p": 3.0 for i in range(10)}
    return guard.DurationBaseline(
        schema_version=guard.BASELINE_SCHEMA_VERSION,
        phase="call",
        min_reference_seconds=1.0,
        max_references_per_lane=200,
        generated_from_runs=("1001", "1002"),
        lanes={
            "api": guard.LaneBaseline("api", 80.0, 40, api_refs),
            "physics": guard.LaneBaseline("physics", 30.0, 10, phys_refs),
        },
    )


def _api_only_baseline() -> guard.DurationBaseline:
    full = _synthetic_baseline()
    return guard.DurationBaseline(
        schema_version=full.schema_version,
        phase=full.phase,
        min_reference_seconds=full.min_reference_seconds,
        max_references_per_lane=full.max_references_per_lane,
        generated_from_runs=full.generated_from_runs,
        lanes={"api": full.lanes["api"]},
    )


def _artefact_from_costs(
    source: str,
    lane: str,
    items: list[tuple[str, float, tuple[str, ...]]],
    *,
    outcome: str = "passed",
    reasons: dict[str, tuple[tuple[str, str], ...]] | None = None,
) -> guard.ArtefactTests:
    reason_map = reasons or {}
    tests = tuple(
        cost_markers.TestCost(
            node_id=node_id,
            markers=tuple(sorted(markers)),
            cpu_seconds=cpu,
            wall_seconds=cpu * 1.2,
            source=source,
            outcome=outcome,
            marker_reasons=reason_map.get(node_id, ()),
        )
        for node_id, cpu, markers in items
    )
    return guard.ArtefactTests(source=source, lane=lane, tests=tests)


@pytest.mark.smoke
@pytest.mark.core
def test_bimodal_runners_normalise_without_false_positive() -> None:
    """Fast API (0.65x) and slow physics (1.60x) in one run both pass within 10%."""
    baseline = _synthetic_baseline()
    api_items = [
        (nid, cpu * 0.65, ("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    phys_items = [
        (nid, cpu * 1.60, ("physics",))
        for nid, cpu in baseline.lanes["physics"].reference_tests.items()
    ]
    report = guard.check_durations(
        [
            _artefact_from_costs("api-cp312-manylinux-x86_64.json", "api", api_items),
            _artefact_from_costs(
                "physics-cp312-manylinux-x86_64-pytest.json", "physics", phys_items
            ),
        ],
        baseline,
        changed_test_files=[],
    )

    assert report.ok
    assert report.lanes["api"].status == "ok"
    assert report.lanes["physics"].status == "ok"
    assert report.lanes["api"].scaled_total_seconds == pytest.approx(80.0)
    assert report.lanes["physics"].scaled_total_seconds == pytest.approx(30.0)
    assert report.lanes["api"].artefacts[0].runner_factor == pytest.approx(0.65)
    assert report.lanes["physics"].artefacts[0].runner_factor == pytest.approx(1.60)
    assert report.annotations() == []


@pytest.mark.smoke
@pytest.mark.core
def test_four_way_sharded_api_aggregates_across_mixed_speed_runners() -> None:
    """Four API shards on mixed fast/slow VMs normalise per shard before summing."""
    baseline = _api_only_baseline()
    shard_factors = {1: 0.65, 2: 1.55, 3: 0.70, 4: 1.50}
    shard_artefacts: list[guard.ArtefactTests] = []
    for shard_idx in range(1, 5):
        factor = shard_factors[shard_idx]
        items = [
            (nid, cpu * factor, ("api",))
            for nid, cpu in baseline.lanes["api"].reference_tests.items()
            if _shard_of(nid, 4) == shard_idx
        ]
        shard_artefacts.append(
            _artefact_from_costs(
                f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
                "api",
                items,
            )
        )

    # Also test a genuine +15% regression injected into shard 1 only.
    regressed_shards = list(shard_artefacts)
    extra = cost_markers.TestCost(
        node_id="test/core/test_added.py::test_extra",
        markers=("api",),
        cpu_seconds=12.0 * shard_factors[1],  # 12.0 s normalised -> +15% on 80 s lane
        wall_seconds=12.0,
        source=regressed_shards[0].source,
    )
    regressed_shards[0] = guard.ArtefactTests(
        source=regressed_shards[0].source,
        lane="api",
        tests=(*regressed_shards[0].tests, extra),
    )

    clean = guard.check_durations(shard_artefacts, baseline, changed_test_files=[])
    assert clean.ok
    assert clean.lanes["api"].measured_count == 40
    assert clean.lanes["api"].scaled_total_seconds == pytest.approx(80.0)
    assert "4 shards" in clean.lanes["api"].factor_summary()

    slowed = guard.check_durations(
        regressed_shards,
        baseline,
        changed_test_files=["test/core/test_added.py"],
    )
    assert not slowed.ok
    assert slowed.lanes["api"].regressed is True
    assert slowed.lanes["api"].delta_ratio == pytest.approx(0.15)
    assert slowed.lanes["api"].measured_count == 41
    # 12.0 s normalised is inside the medium band (10 * 2 = 20), so no marker warning.
    assert slowed.marker_warnings == []
    annotations = slowed.annotations()
    assert len(annotations) == 1
    assert "::warning title=Test duration regression::api lane" in annotations[0]


@pytest.mark.smoke
@pytest.mark.core
def test_normalised_cost_marker_check_reuses_policy_and_excludes_unchanged_files() -> None:
    """Changed tests are judged on normalised call CPU-s with medium=10, slow=30, band=2."""
    baseline = _api_only_baseline()
    # Fast runner (0.65x): a raw 15 s unmarked test normalises to 23.1 s (>= 20 s -> medium).
    # Raw check_cost_markers would miss 15 s; normalised guard catches it.
    # A raw 6 s test (9.2 s normalised) must NOT be flagged (no competing 5 s rule).
    # An unmarked 40 s test in an UNCHANGED file is ignored when changed_test_files is given.
    fast = 0.65
    api_items = [
        (nid, cpu * fast, ("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    api_items.extend([
        ("test/core/test_changed.py::test_unmarked_medium_fast_vm", 15.0, ("api",)),
        ("test/core/test_changed.py::test_medium_over_slow", 42.0, ("api", "medium")),
        ("test/core/test_changed.py::test_six_second_fast", 6.0, ("api",)),
        ("test/core/test_changed.py::test_cheap_medium_ok_for_guard", 0.5, ("api", "medium")),
        ("test/core/test_changed.py::test_slow_reason", 0.2, ("api", "slow")),
        ("test/core/test_untouched.py::test_preexisting_heavy", 40.0, ("api",)),
    ])
    artefact = _artefact_from_costs(
        "api-cp312-manylinux-x86_64.json",
        "api",
        api_items,
        reasons={
            "test/core/test_changed.py::test_slow_reason": (
                ("slow", "non-CPU reason"),
            )
        },
    )

    report = guard.check_durations(
        [artefact],
        baseline,
        changed_test_files=["./test/core/test_changed.py"],
        max_regression=5.0,  # isolate check (a) from lane-total warning
    )

    assert not report.ok
    by_node = {w.node_id: (w.kind, w.wanted) for w in report.marker_warnings}
    assert by_node == {
        "test/core/test_changed.py::test_unmarked_medium_fast_vm": (
            "unmarked-over-medium",
            "medium",
        ),
        "test/core/test_changed.py::test_medium_over_slow": (
            "medium-over-slow",
            "slow",
        ),
    }
    assert report.checked_changed_tests == 5
    # Slow runner (1.6x): a raw 24 s unmarked test normalises to 15 s (< 20 s) -> clean.
    slow_items = [
        (nid, cpu * 1.6, ("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    slow_items.append(("test/core/test_changed.py::test_ok_on_slow_vm", 24.0, ("api",)))
    slow_report = guard.check_durations(
        [_artefact_from_costs("api-cp312-manylinux-x86_64.json", "api", slow_items)],
        baseline,
        changed_test_files=["test/core/test_changed.py"],
        max_regression=5.0,
    )
    assert slow_report.ok
    assert slow_report.marker_warnings == []


def test_changed_reference_files_are_excluded_from_runner_factor() -> None:
    """Slowing down a changed reference file cannot hide itself in the runner factor."""
    baseline = _synthetic_baseline()
    # Suppose 10 reference tests live in one changed file and all slow down 5x;
    # the remaining 30 unchanged reference tests still hold the runner factor at 1.0x.
    changed_file = "test/core/test_big_ref.py"
    refs = {
        **{f"{changed_file}::test_c{i:02d}": 2.0 for i in range(10)},
        **{f"test/core/test_unchanged_{i:02d}.py::test_u": 2.0 for i in range(30)},
    }
    custom_base = guard.DurationBaseline(
        schema_version=guard.BASELINE_SCHEMA_VERSION,
        phase="call",
        min_reference_seconds=1.0,
        max_references_per_lane=200,
        generated_from_runs=("1",),
        lanes={"api": guard.LaneBaseline("api", 80.0, 40, refs)},
    )
    items = [
        (nid, 10.0 if nid.startswith(changed_file) else 2.0, ("api",))
        for nid in refs
    ]
    report = guard.check_durations(
        [_artefact_from_costs("api-cp312-manylinux-x86_64.json", "api", items)],
        custom_base,
        changed_test_files=[changed_file],
    )

    assert report.lanes["api"].artefacts[0].matched_references == 30
    assert report.lanes["api"].artefacts[0].runner_factor == pytest.approx(1.0)
    assert report.lanes["api"].regressed is True
    assert report.lanes["api"].delta_ratio == pytest.approx(1.0)  # 160 vs 80


def test_insufficient_reference_matches_marks_lane_unavailable_without_false_alarm() -> None:
    """Fewer than min_reference_matches unchanged references reports incomplete, not regressed."""
    baseline = _synthetic_baseline()
    # Full test count (100% coverage) so this isolates the reference-match gate.
    ref_nodes = list(baseline.lanes["api"].reference_tests)
    changed_files = [cost_markers.node_file(nid) for nid in ref_nodes[2:]]
    items = [(nid, 20.0, ("api",)) for nid in ref_nodes]
    report = guard.check_durations(
        [_artefact_from_costs("api-cp312-manylinux-x86_64.json", "api", items)],
        baseline,
        changed_test_files=changed_files,
        min_reference_matches=5,
    )

    assert not report.ok
    assert not report.complete
    assert report.status == "incomplete"
    assert report.regressed_lanes == []
    assert report.lanes["api"].status == "unavailable"
    assert "too few unchanged reference matches" in (report.lanes["api"].reason or "")
    assert report.lanes["api"].regressed is False
    assert report.lanes["physics"].status == "missing"
    assert report.lanes["physics"].regressed is False


@pytest.mark.smoke
@pytest.mark.core
def test_wholly_missing_api_lane_with_clean_physics_reports_incomplete_never_ok() -> None:
    """Wholly missing API artefacts with clean physics never print [OK] within threshold."""
    baseline = _synthetic_baseline()
    phys_items = [
        (nid, cpu, ("physics",))
        for nid, cpu in baseline.lanes["physics"].reference_tests.items()
    ]
    report = guard.check_durations(
        [
            _artefact_from_costs(
                "physics-cp312-manylinux-x86_64-pytest.json", "physics", phys_items
            )
        ],
        baseline,
        changed_test_files=[],
    )

    assert not report.ok
    assert not report.complete
    assert report.status == "incomplete"
    assert report.lanes["physics"].status == "ok"
    assert report.lanes["api"].status == "missing"
    assert [res.lane for res in report.incomplete_lanes] == ["api"]

    msg = report.message()
    assert "[OK] normalised lane totals within threshold" not in msg
    assert "[INCOMPLETE] comparison incomplete; could not compare: api (missing:" in msg

    md = report.markdown()
    assert "Status: **INCOMPLETE**" in md
    assert "`api` (missing: no metrics artefacts found for lane)" in md

    annotations = report.annotations()
    assert len(annotations) == 1
    assert "::warning title=Test duration guard incomplete::api lane comparison incomplete (missing:" in annotations[0]

    payload = report.to_dict()
    assert payload["ok"] is False
    assert payload["complete"] is False
    assert payload["status"] == "incomplete"
    assert payload["incomplete_lanes"] == ["api"]


@pytest.mark.smoke
@pytest.mark.core
def test_partial_shard_failure_concludes_incomplete_across_all_reporting_surfaces() -> None:
    """A 3-of-4 shard run with clean physics reports incomplete on text, markdown, JSON, and annotations."""
    baseline = _synthetic_baseline()
    api_refs = baseline.lanes["api"].reference_tests
    phys_items = [
        (nid, cpu, ("physics",))
        for nid, cpu in baseline.lanes["physics"].reference_tests.items()
    ]
    artefacts: list[guard.ArtefactTests] = [
        _artefact_from_costs(
            "physics-cp312-manylinux-x86_64-pytest.json", "physics", phys_items
        )
    ]
    for shard_idx in (1, 2, 3):
        items = [
            (nid, cpu * (1.15 if shard_idx == 1 else 1.0), ("api",))
            for nid, cpu in api_refs.items()
            if _shard_of(nid, 4) == shard_idx
        ]
        artefacts.append(
            _artefact_from_costs(
                f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
                "api",
                items,
            )
        )

    report = guard.check_durations(artefacts, baseline, changed_test_files=[])

    assert not report.ok
    assert not report.complete
    assert report.status == "incomplete"
    assert report.lanes["physics"].status == "ok"
    assert report.lanes["api"].status == "unavailable"

    msg = report.message()
    assert "[OK] normalised lane totals within threshold" not in msg
    assert "[INCOMPLETE] comparison incomplete; could not compare: api (unavailable: incomplete shard set: missing shard(s) 4 of 4 (found 3/4))." in msg

    md = report.markdown()
    assert "Status: **INCOMPLETE**" in md
    assert "missing shard(s) 4 of 4" in md

    annotations = report.annotations()
    assert len(annotations) == 1
    assert "::warning title=Test duration guard incomplete::api lane comparison incomplete (unavailable:" in annotations[0]
    assert "missing shard(s) 4 of 4" in annotations[0]

    payload = report.to_dict()
    assert payload["ok"] is False
    assert payload["complete"] is False
    assert payload["status"] == "incomplete"
    assert payload["incomplete_lanes"] == ["api"]


@pytest.mark.smoke
@pytest.mark.core
def test_missing_shard_or_incomplete_session_marks_lane_unavailable_never_ok() -> None:
    """Missing shards, interrupted sessions, or low coverage never read as ok."""
    baseline = _synthetic_baseline()
    api_refs = baseline.lanes["api"].reference_tests

    # 3 of 4 shards present, with a +15% slowdown injected into shard 1:
    # raw/scaled partial total is below baseline (~75% * 1.15 < 100%), yet
    # the lane MUST be unavailable rather than ok.
    three_shards: list[guard.ArtefactTests] = []
    for shard_idx in (1, 2, 3):
        items = [
            (nid, cpu * (1.15 if shard_idx == 1 else 1.0), ("api",))
            for nid, cpu in api_refs.items()
            if _shard_of(nid, 4) == shard_idx
        ]
        three_shards.append(
            _artefact_from_costs(
                f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
                "api",
                items,
            )
        )
    missing_shard_report = guard.check_durations(
        three_shards, baseline, changed_test_files=[]
    )
    assert missing_shard_report.lanes["api"].status == "unavailable"
    assert missing_shard_report.lanes["api"].scaled_total_seconds is None
    assert "missing shard(s) 4 of 4" in (missing_shard_report.lanes["api"].reason or "")
    assert "missing shard(s) 4 of 4" in missing_shard_report.markdown()

    # Complete 4 shards, but shard 2 exited with pytest interrupted code 2.
    all_four = list(three_shards)
    shard4_items = [
        (nid, cpu, ("api",))
        for nid, cpu in api_refs.items()
        if _shard_of(nid, 4) == 4
    ]
    all_four.append(
        _artefact_from_costs(
            "api-cp312-manylinux-x86_64-shard-4-of-4.json", "api", shard4_items
        )
    )
    all_four[1] = guard.ArtefactTests(
        source=all_four[1].source,
        lane="api",
        tests=all_four[1].tests,
        shard=(2, 4),
        exit_code=2,
    )
    interrupted = guard.check_durations(all_four, baseline, changed_test_files=[])
    assert interrupted.lanes["api"].status == "unavailable"
    assert "exit_code=2" in (interrupted.lanes["api"].reason or "")

    # Complete shard filenames, but one shard recorded fewer outcomes than collected.
    all_four[1] = guard.ArtefactTests(
        source=all_four[1].source,
        lane="api",
        tests=all_four[1].tests,
        shard=(2, 4),
        exit_code=1,
        collected_count=len(all_four[1].tests) + 3,
        recorded_outcome_count=len(all_four[1].tests),
    )
    short_session = guard.check_durations(all_four, baseline, changed_test_files=[])
    assert short_session.lanes["api"].status == "unavailable"
    assert "collected tests recorded" in (short_session.lanes["api"].reason or "")

    # Unsharded lane with < 90% of baseline measured_test_count (30 of 40 = 75%).
    partial_unsharded = [
        (nid, cpu, ("api",)) for nid, cpu in list(api_refs.items())[:30]
    ]
    low_cov = guard.check_durations(
        [_artefact_from_costs("api-cp312-manylinux-x86_64.json", "api", partial_unsharded)],
        baseline,
        changed_test_files=[],
    )
    assert low_cov.lanes["api"].status == "unavailable"
    assert "below 90% of baseline" in (low_cov.lanes["api"].reason or "")


@pytest.mark.smoke
@pytest.mark.core
def test_build_baseline_normalises_mixed_speed_rotating_shards_per_artefact(
    tmp_path: Path,
) -> None:
    """Rotating 2x slow shards across runs must not inflate the baseline total.

    Pooling raw shards before computing one lane-wide median factor leaves the
    2x shard un-normalised (baseline ~25% high), masking a genuine +20%
    regression as ~-4%. Normalising each shard with the evaluation estimator
    recovers the exact 1x baseline total and catches the +20% slowdown.
    """
    api_nodes = [f"test/core/test_rot_{i:02d}.py::test_a" for i in range(48)]
    base_cost = 2.0  # true 1x lane total = 48 * 2.0 = 96.0 s

    run_dirs: list[Path] = []
    for run_idx in range(1, 5):
        run_dir = tmp_path / f"run_{run_idx}"
        run_dirs.append(run_dir)
        for shard_idx in range(1, 5):
            factor = 2.0 if shard_idx == run_idx else 1.0
            recs = [
                _record(nid, call=base_cost * factor, markers=("api",))
                for nid in api_nodes
                if _shard_of(nid, 4) == shard_idx
            ]
            _write_artefact(
                run_dir
                / "api"
                / f"ci-metrics-api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4"
                / f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
                recs,
            )

    baseline = guard.build_baseline(run_dirs)
    assert baseline.lanes["api"].total_cpu_seconds == pytest.approx(96.0)
    assert baseline.lanes["api"].measured_test_count == 48

    # Now evaluate a new 4-shard run with a genuine +20% slowdown (+19.2 s in a changed file).
    eval_shards: list[guard.ArtefactTests] = []
    for shard_idx in range(1, 5):
        items = [
            (nid, base_cost, ("api",))
            for nid in api_nodes
            if _shard_of(nid, 4) == shard_idx
        ]
        if shard_idx == 1:
            items.append(("test/core/test_added.py::test_slow", 19.2, ("api", "medium")))
        eval_shards.append(
            _artefact_from_costs(
                f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
                "api",
                items,
            )
        )

    report = guard.check_durations(
        eval_shards,
        baseline,
        changed_test_files=["test/core/test_added.py"],
    )
    assert not report.ok
    assert report.lanes["api"].regressed is True
    assert report.lanes["api"].delta_ratio == pytest.approx(0.20)


def test_dual_marked_test_is_judged_once_across_api_and_physics() -> None:
    """A test appearing in both lanes is judged once on its largest normalised cost."""
    baseline = _synthetic_baseline()
    api_items = [
        (nid, cpu, ("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    api_items.append(("test/core/test_dual.py::test_both", 5.0, ("api", "physics")))
    phys_items = [
        (nid, cpu, ("physics",))
        for nid, cpu in baseline.lanes["physics"].reference_tests.items()
    ]
    phys_items.append(("test/core/test_dual.py::test_both", 25.0, ("api", "physics")))

    report = guard.check_durations(
        [
            _artefact_from_costs("api.json", "api", api_items),
            _artefact_from_costs("physics-pytest.json", "physics", phys_items),
        ],
        baseline,
        changed_test_files=["test/core/test_dual.py"],
        max_regression=5.0,
    )

    assert len(report.marker_warnings) == 1
    assert report.marker_warnings[0].node_id == "test/core/test_dual.py::test_both"
    assert report.marker_warnings[0].lane == "physics"
    assert report.marker_warnings[0].scaled_cpu_seconds == pytest.approx(25.0)


def test_write_baseline_round_trip_with_sharded_runs_and_cli_check(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """`--write-baseline` aggregates sharded runs, strips local paths, and checks cleanly."""
    # Build two synthetic runs: run_9001 (unsharded fast) and run_9002 (4-shard slow).
    # 48 API nodes give well over 5 references per shard under 4-way SHA-256 splitting.
    api_nodes = [f"test/core/test_m_{i:02d}.py::test_a" for i in range(48)]
    phys_nodes = [f"test/physics/test_p_{i:02d}.py::test_p" for i in range(10)]

    run1 = tmp_path / "runs" / "9001"
    _write_artefact(
        run1 / "api" / "api-cp312-manylinux-x86_64.json",
        [_record(nid, call=1.5, markers=("api",)) for nid in api_nodes],
    )
    _write_artefact(
        run1 / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=2.0, markers=("physics",)) for nid in phys_nodes],
    )
    # Non-pytest physics siblings must be ignored without double-counting.
    (run1 / "physics" / "physics-cp312-manylinux-x86_64-phases.json").write_text(
        json.dumps({"schema_version": 1, "phases": {}}), encoding="utf-8"
    )
    (run1 / "physics" / "physics-cp312-manylinux-x86_64-wheel-job.json").write_text(
        json.dumps({"schema_version": 1, "kind": "wheel-job-ci-metrics"}),
        encoding="utf-8",
    )

    run2 = tmp_path / "runs" / "9002"
    for shard_idx in range(1, 5):
        shard_recs = [
            _record(nid, call=3.0, markers=("api",))
            for nid in api_nodes
            if _shard_of(nid, 4) == shard_idx
        ]
        _write_artefact(
            run2
            / "api"
            / f"ci-metrics-api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4"
            / f"api-cp312-manylinux-x86_64-shard-{shard_idx}-of-4.json",
            shard_recs,
        )
    _write_artefact(
        run2 / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=4.0, markers=("physics",)) for nid in phys_nodes],
    )

    baseline_path = tmp_path / "baseline.json"
    rc = guard.main([
        "--write-baseline",
        "--baseline",
        str(baseline_path),
        str(run1),
        str(run2),
    ])
    assert rc == 0
    raw_baseline = json.loads(baseline_path.read_text(encoding="utf-8"))
    # Only directory basenames are recorded; no local paths leak into JSON.
    assert raw_baseline["generated_from_runs"] == ["9001", "9002"]
    assert str(tmp_path) not in baseline_path.read_text(encoding="utf-8")
    assert raw_baseline["lanes"]["api"]["total_cpu_seconds"] == pytest.approx(108.0)
    assert raw_baseline["lanes"]["physics"]["total_cpu_seconds"] == pytest.approx(30.0)

    # Checking run2 against the generated baseline passes with exit 0 and writes JSON/Markdown.
    out_json = tmp_path / "report.json"
    out_md = tmp_path / "summary.md"
    capsys.readouterr()
    rc_check = guard.main([
        str(run2),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
        "--markdown",
        str(out_md),
        "--annotations",
    ])
    assert rc_check == 0
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is True
    assert payload["lanes"]["api"]["status"] == "ok"
    assert payload["lanes"]["physics"]["status"] == "ok"
    assert payload["lanes"]["api"]["artefact_count"] == 4
    assert "## Test duration guard (report-only)" in out_md.read_text(encoding="utf-8")


def test_unsupported_metrics_schema_exits_2(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """An unknown per-test metrics schema stops the check rather than reporting clean."""
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(_synthetic_baseline(), baseline_path)
    bad = tmp_path / "api-cp312-manylinux-x86_64.json"
    bad.write_text(
        json.dumps({"schema_version": 99, "tests": [_record("t.py::t", call=1.0)]}),
        encoding="utf-8",
    )

    assert guard.main([str(bad), "--baseline", str(baseline_path)]) == 2
    assert "metrics schema 99 not supported" in capsys.readouterr().out


@pytest.mark.smoke
def test_committed_baseline_and_workflow_wiring_satisfy_guard_contracts() -> None:
    """Committed baseline is valid; CI job is report-only, standard-PR-only, and path-filtered."""
    baseline_path = REPO_ROOT / "scripts/lint/test_duration_baseline.json"
    baseline = guard.load_baseline(baseline_path)
    assert set(baseline.lanes) == {"api", "physics"}
    assert len(baseline.generated_from_runs) >= 6
    for lane, lb in baseline.lanes.items():
        assert lb.total_cpu_seconds > 0.0, lane
        assert len(lb.reference_tests) >= 50, lane

    workflow = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    job = workflow["jobs"]["test_duration_guard"]
    # Report-only and never wired into pr-gate or report_scheduled_run.
    assert job["continue-on-error"] is True
    assert "test_duration_guard" not in workflow["jobs"]["pr-gate"]["needs"]
    assert (
        "test_duration_guard"
        not in workflow["jobs"]["report_scheduled_run"]["needs"]
    )
    # Standard PR tier only (not smoke/core/cfg/physics-full/merge_group/schedule).
    assert "github.event_name == 'pull_request'" in job["if"]
    assert "needs.determine_matrix.outputs.test_tier == 'standard'" in job["if"]
    assert "merge_group" not in job["if"]
    assert "schedule" not in job["if"]

    # Base-branch sparse checkout security pattern.
    checkout = job["steps"][0]
    assert checkout["with"]["ref"] == "${{ github.event.pull_request.base.sha }}"
    assert "scripts/lint/check_test_duration.py" in checkout["with"]["sparse-checkout"]
    assert (
        "scripts/lint/test_duration_baseline.json"
        in checkout["with"]["sparse-checkout"]
    )

    # Downloads both unsharded and N-sharded Linux cp312 API artefacts.
    api_dl = next(
        s
        for s in job["steps"]
        if s.get("name") == "Download the Linux cp312 api metrics"
    )
    assert api_dl["with"]["pattern"] == "ci-metrics-api-cp312-manylinux-x86_64*"

    # PR test-file list is passed via env + jq, never interpolated into the command.
    check_step = next(
        s
        for s in job["steps"]
        if s.get("name") == "Check runner-normalised test durations against baseline"
    )
    assert "${{" not in check_step["run"]
    assert check_step["env"]["TESTS_FILES"] == (
        "${{ needs.detect-changes.outputs.tests-files || '[]' }}"
    )

    # New script and baseline are covered by .github/path-filters.yml.
    filters = yaml.safe_load(
        (REPO_ROOT / ".github/path-filters.yml").read_text(encoding="utf-8")
    )
    assert "scripts/lint/**" in filters["ci"]
    assert "**/*.py" in filters["encoding_audit"]
