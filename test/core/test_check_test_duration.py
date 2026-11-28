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

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]


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
    phys_nodes = [f"test/physics/test_rot_p_{i:02d}.py::test_p" for i in range(10)]
    base_cost = 2.0  # true 1x api lane total = 48 * 2.0 = 96.0 s

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
        _write_artefact(
            run_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
            [_record(nid, call=3.0, markers=("physics",)) for nid in phys_nodes],
        )

    baseline = guard.build_baseline(run_dirs)
    assert baseline.lanes["api"].total_cpu_seconds == pytest.approx(96.0)
    assert baseline.lanes["api"].measured_test_count == 48
    assert baseline.lanes["physics"].total_cpu_seconds == pytest.approx(30.0)

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
@pytest.mark.core
def test_cli_empty_and_absent_metrics_dirs_write_incomplete_json_markdown_and_annotations(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Complete metrics loss (empty or absent dir) still emits an incomplete report."""
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(_synthetic_baseline(), baseline_path)

    empty_dir = tmp_path / "empty-metrics"
    empty_dir.mkdir()
    absent_dir = tmp_path / "absent-metrics"

    for label, target in (("empty", empty_dir), ("absent", absent_dir)):
        out_json = tmp_path / f"{label}.json"
        out_md = tmp_path / f"{label}.md"
        rc = guard.main([
            str(target),
            "--baseline",
            str(baseline_path),
            "--no-changed-test-files",
            "--json",
            str(out_json),
            "--markdown",
            str(out_md),
            "--annotations",
        ])
        out = capsys.readouterr().out
        assert rc == 1, label
        assert out_json.is_file(), label
        payload = json.loads(out_json.read_text(encoding="utf-8"))
        assert payload["ok"] is False, label
        assert payload["complete"] is False, label
        assert payload["status"] == "incomplete", label
        assert sorted(payload["incomplete_lanes"]) == ["api", "physics"], label
        assert payload["lanes"]["api"]["status"] == "missing", label
        assert payload["lanes"]["physics"]["status"] == "missing", label
        assert "Status: **INCOMPLETE**" in out_md.read_text(encoding="utf-8"), label
        assert "::warning title=Test duration guard incomplete::api lane" in out, label
        assert "::warning title=Test duration guard incomplete::physics lane" in out, label


@pytest.mark.smoke
@pytest.mark.core
def test_newline_bearing_node_id_cannot_forge_workflow_commands(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Control chars in untrusted node IDs are sanitised and guarded by stop-commands."""
    baseline = _synthetic_baseline()
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(baseline, baseline_path)

    forged_node = "test/core/test_evil.py::test_x[\n::error title=forged::pwned\r\n]"
    records = [
        _record(nid, call=cpu, markers=("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    records.append(_record(forged_node, call=25.0, markers=("api",)))
    metrics_dir = tmp_path / "metrics"
    _write_artefact(
        metrics_dir / "api" / "api-cp312-manylinux-x86_64.json", records
    )
    _write_artefact(
        metrics_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [
            _record(nid, call=cpu, markers=("physics",))
            for nid, cpu in baseline.lanes["physics"].reference_tests.items()
        ],
    )

    # Without --annotations: no output line may start with `::`.
    rc_plain = guard.main([
        str(metrics_dir),
        "--baseline",
        str(baseline_path),
        "--test-file",
        "test/core/test_evil.py",
        "--max-regression",
        "5.0",
    ])
    plain_lines = capsys.readouterr().out.splitlines()
    assert rc_plain == 1
    assert not any(line.lstrip().startswith("::") for line in plain_lines), plain_lines

    # With --annotations: plain text is wrapped in ::stop-commands::<tok> ... ::<tok>::,
    # and the only other `::` lines are the intended ::warning annotations.
    rc_ann = guard.main([
        str(metrics_dir),
        "--baseline",
        str(baseline_path),
        "--test-file",
        "test/core/test_evil.py",
        "--max-regression",
        "5.0",
        "--annotations",
    ])
    ann_lines = capsys.readouterr().out.splitlines()
    assert rc_ann == 1
    stop_lines = [line for line in ann_lines if line.startswith("::stop-commands::")]
    assert len(stop_lines) == 1
    token = stop_lines[0].removeprefix("::stop-commands::")
    assert token
    resume_line = f"::{token}::"
    assert resume_line in ann_lines
    assert ann_lines.index(stop_lines[0]) < ann_lines.index(resume_line)

    command_lines = [line for line in ann_lines if line.lstrip().startswith("::")]
    for line in command_lines:
        assert (
            line == stop_lines[0]
            or line == resume_line
            or line.startswith("::warning title=Test cost marker::")
        ), f"unexpected workflow command line: {line!r}"
    assert not any(line.lstrip().startswith("::error") for line in ann_lines)


@pytest.mark.smoke
@pytest.mark.core
def test_sharded_physics_with_unsuffixed_wheel_metadata_and_missing_shard_detection(
    tmp_path: Path,
) -> None:
    """Wildcard physics download isolates wheel bookkeeping from 2 physics shards and catches any missing shard."""
    api_nodes = [f"test/core/test_a_{i:02d}.py::test_a" for i in range(40)]
    phys_nodes = [f"test/physics/test_p_{i:02d}.py::test_p" for i in range(24)]
    baseline = guard.DurationBaseline(
        schema_version=guard.BASELINE_SCHEMA_VERSION,
        phase="call",
        min_reference_seconds=1.0,
        max_references_per_lane=200,
        generated_from_runs=("1001",),
        lanes={
            "api": guard.LaneBaseline(
                "api", 80.0, len(api_nodes), dict.fromkeys(api_nodes, 2.0)
            ),
            "physics": guard.LaneBaseline(
                "physics", 72.0, len(phys_nodes), dict.fromkeys(phys_nodes, 3.0)
            ),
        },
    )

    def _build_tree(root: Path, *, include_phys_shards: tuple[int, ...]) -> None:
        # 4 API shards in separate subdirectories.
        for s in range(1, 5):
            recs = [
                _record(nid, call=2.0, markers=("api",))
                for nid in api_nodes
                if _shard_of(nid, 4) == s
            ]
            _write_artefact(
                root
                / "api"
                / f"ci-metrics-api-cp312-manylinux-x86_64-shard-{s}-of-4"
                / f"api-cp312-manylinux-x86_64-shard-{s}-of-4.json",
                recs,
            )
        # Unsuffixed wheel-job bookkeeping artefact (contains NO pytest records).
        wheel_dir = root / "physics" / "ci-metrics-physics-cp312-manylinux-x86_64"
        wheel_dir.mkdir(parents=True, exist_ok=True)
        (wheel_dir / "physics-cp312-manylinux-x86_64-wheel-job.json").write_text(
            json.dumps({"schema_version": 1, "kind": "wheel-job-ci-metrics"}),
            encoding="utf-8",
        )
        (wheel_dir / "physics-cp312-manylinux-x86_64-phases.json").write_text(
            json.dumps({"schema_version": 1, "phases": {}}), encoding="utf-8"
        )
        # Sharded physics pytest artefacts in separate subdirectories.
        for s in include_phys_shards:
            recs = [
                _record(nid, call=3.0, markers=("physics",))
                for nid in phys_nodes
                if _shard_of(nid, 2) == s
            ]
            _write_artefact(
                root
                / "physics"
                / f"ci-metrics-physics-cp312-manylinux-x86_64-shard-{s}-of-2"
                / f"physics-cp312-manylinux-x86_64-shard-{s}-of-2-pytest.json",
                recs,
            )

    full_tree = tmp_path / "full"
    _build_tree(full_tree, include_phys_shards=(1, 2))
    full_report = guard.check_durations(
        guard.load_artefacts([full_tree]), baseline, changed_test_files=[]
    )
    assert full_report.ok is True
    assert full_report.complete is True
    assert full_report.lanes["physics"].status == "ok"
    assert len(full_report.lanes["physics"].artefacts) == 2

    # Removing either physics shard marks the comparison incomplete.
    for kept, missing_idx in (((1,), 2), ((2,), 1)):
        partial_tree = tmp_path / f"missing_shard_{missing_idx}"
        _build_tree(partial_tree, include_phys_shards=kept)
        partial_report = guard.check_durations(
            guard.load_artefacts([partial_tree]), baseline, changed_test_files=[]
        )
        assert partial_report.ok is False
        assert partial_report.complete is False
        assert partial_report.lanes["physics"].status == "unavailable"
        assert partial_report.lanes["physics"].reason_code == "missing_shards"
        assert f"missing shard(s) {missing_idx} of 2" in (
            partial_report.lanes["physics"].reason or ""
        )

    # Only the unsuffixed wheel metadata present -> physics is missing, never clean.
    wheel_only_tree = tmp_path / "wheel_only"
    _build_tree(wheel_only_tree, include_phys_shards=())
    wheel_only_report = guard.check_durations(
        guard.load_artefacts([wheel_only_tree]), baseline, changed_test_files=[]
    )
    assert wheel_only_report.ok is False
    assert wheel_only_report.complete is False
    assert wheel_only_report.lanes["physics"].status == "missing"
    assert wheel_only_report.lanes["physics"].reason_code == "no_artefacts"


@pytest.mark.smoke
@pytest.mark.core
def test_baseline_builder_and_loader_require_complete_api_and_physics_lanes(
    tmp_path: Path,
) -> None:
    """Baseline refresh and loader reject runs or files missing either required lane."""
    api_nodes = [f"test/core/test_b_{i:02d}.py::test_a" for i in range(10)]
    phys_nodes = [f"test/physics/test_b_{i:02d}.py::test_p" for i in range(10)]

    run_complete = tmp_path / "run_complete"
    _write_artefact(
        run_complete / "api" / "api-cp312-manylinux-x86_64.json",
        [_record(nid, call=2.0, markers=("api",)) for nid in api_nodes],
    )
    _write_artefact(
        run_complete / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=3.0, markers=("physics",)) for nid in phys_nodes],
    )

    run_api_only = tmp_path / "run_api_only"
    _write_artefact(
        run_api_only / "api" / "api-cp312-manylinux-x86_64.json",
        [_record(nid, call=2.0, markers=("api",)) for nid in api_nodes],
    )
    # Only wheel bookkeeping under physics, no pytest shard JSONs.
    (run_api_only / "physics").mkdir(parents=True, exist_ok=True)
    (run_api_only / "physics" / "physics-cp312-manylinux-x86_64-wheel-job.json").write_text(
        json.dumps({"schema_version": 1, "kind": "wheel-job-ci-metrics"}),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="missing measured artefacts for required lane"):
        guard.build_baseline([run_api_only])

    with pytest.raises(ValueError, match="missing measured artefacts for required lane"):
        guard.build_baseline([run_complete, run_api_only])

    # Loader rejects a baseline missing either api or physics.
    for drop_lane in ("physics", "api"):
        bad_base = guard.baseline_to_dict(_synthetic_baseline())
        del bad_base["lanes"][drop_lane]
        bad_path = tmp_path / f"missing_{drop_lane}.json"
        bad_path.write_text(json.dumps(bad_base), encoding="utf-8")
        with pytest.raises(ValueError, match=f"missing required lane.*{drop_lane}"):
            guard.load_baseline(bad_path)


@pytest.mark.smoke
@pytest.mark.core
def test_malformed_filename_payloads_sanitised_cross_platform(
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Newline, workflow-command, and HTML-comment filename payloads are sanitised on all platforms."""
    baseline = _synthetic_baseline()
    evil_schema_name = "api\n::error title=forged::pwned.json"
    evil_comment_name = (
        "api\n<!-- duration-guard:end -->[click](https://evil.test).json"
    )

    guard._emit_cli_error(
        "",
        cost_markers.UnsupportedMetricsSchema(
            f"bad_schema/{evil_schema_name}: unsupported schema_version=99"
        ),
        emit_annotations=True,
    )
    err_lines = capsys.readouterr().out.splitlines()
    assert not any(line.lstrip().startswith("::error") for line in err_lines), err_lines
    assert any(line.startswith("::stop-commands::") for line in err_lines), err_lines

    artefact = _artefact_from_costs(
        evil_comment_name,
        "api",
        [("test/core/test_x.py::test_1", 2.0, ("api",))],
    )
    report = guard.check_durations([artefact], baseline, changed_test_files=[])
    guard._emit_cli_output(report, [], emit_annotations=True)
    out_lines = capsys.readouterr().out.splitlines()
    assert not any(line.lstrip().startswith("::error") for line in out_lines)
    md_text = report.markdown()
    assert "<!-- duration-guard:end -->" not in md_text
    assert "[click]" not in md_text
    api_lane = report.to_dict()["lanes"]["api"]
    assert api_lane["status"] == "unavailable"
    assert api_lane["reason_code"] in {"low_coverage", "insufficient_references"}
    assert "<!--" not in (api_lane["reason"] or "")


@pytest.mark.smoke
@pytest.mark.core
def test_malformed_filenames_cannot_inject_workflow_commands_or_comment_markers(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """On-disk adversarial PR artefact filenames are sanitised on error and report paths."""
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(_synthetic_baseline(), baseline_path)

    # 1. Unsupported-schema error path. POSIX allows newline/colon filenames;
    # Windows NTFS rejects those characters, so use NTFS-legal Markdown chars.
    bad_schema_dir = tmp_path / "bad_schema"
    bad_schema_dir.mkdir()
    schema_name = (
        "api[click](https__evil.test)~bad.json"
        if sys.platform == "win32"
        else "api\n::error title=forged::pwned.json"
    )
    (bad_schema_dir / schema_name).write_text(
        json.dumps({"schema_version": 99, "tests": [_record("t.py::t", call=1.0)]}),
        encoding="utf-8",
    )
    rc = guard.main([
        str(bad_schema_dir),
        "--baseline",
        str(baseline_path),
        "--annotations",
    ])
    out_lines = capsys.readouterr().out.splitlines()
    assert rc == 2
    assert not any(line.lstrip().startswith("::error") for line in out_lines), out_lines
    assert any(line.startswith("::stop-commands::") for line in out_lines), out_lines

    # 2. Incomplete-lane artefact filename carrying delimiter / Markdown link payload.
    evil_dir = tmp_path / "evil_incomplete" / "api"
    evil_dir.mkdir(parents=True)
    comment_name = (
        "api[click](https__evil.test)_~bad.json"
        if sys.platform == "win32"
        else "api\n<!-- duration-guard:end -->[click](https://evil.test).json"
    )
    _write_artefact(
        evil_dir / comment_name,
        [_record("test/core/test_x.py::test_1", call=2.0)],
    )
    out_json = tmp_path / "evil_report.json"
    out_md = tmp_path / "evil_report.md"
    rc2 = guard.main([
        str(tmp_path / "evil_incomplete"),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
        "--markdown",
        str(out_md),
        "--annotations",
    ])
    out2_lines = capsys.readouterr().out.splitlines()
    assert rc2 == 1
    assert not any(line.lstrip().startswith("::error") for line in out2_lines)
    md_text = out_md.read_text(encoding="utf-8")
    assert "<!-- duration-guard:end -->" not in md_text
    assert "[click]" not in md_text
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    api_lane = payload["lanes"]["api"]
    assert api_lane["status"] == "unavailable"
    assert api_lane["reason_code"] in {"low_coverage", "insufficient_references"}
    assert "<!--" not in (api_lane["reason"] or "")


@pytest.mark.smoke
@pytest.mark.core
def test_malformed_shard_totals_and_duplicate_shards_are_rejected_boundedly(
    tmp_path: Path,
) -> None:
    """Huge shard totals, malformed shard labels, and duplicate shard indices fail cleanly with bounded work."""
    baseline = _synthetic_baseline()
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(baseline, baseline_path)
    api_items = [
        (nid, cpu, ("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    phys_items = [
        (nid, cpu, ("physics",))
        for nid, cpu in baseline.lanes["physics"].reference_tests.items()
    ]
    phys = _artefact_from_costs(
        "physics-cp312-manylinux-x86_64-pytest.json", "physics", phys_items
    )

    for bad_name in (
        "api-cp312-manylinux-x86_64-shard-1-of-999999999.json",
        "api-cp312-manylinux-x86_64-shard-1-of-999999999999999999999999999999.json",
        "api-cp312-manylinux-x86_64-shard-x-of-2.json",
        "api-cp312-manylinux-x86_64-shard--1-of-2.json",
        "api-cp312-manylinux-x86_64-shard-1-of.json",
        "api-cp312-manylinux-x86_64-shard-x-of-2-shard-1-of-1.json",
        "api-cp312-manylinux-x86_64-shard-1-of-2-shard-2-of-2.json",
        "api-cp312-manylinux-x86_64-shard-shard-1-of-1.json",
        "api-cp312-manylinux-x86_64_shard_shard_1_of_1.json",
    ):
        report = guard.check_durations(
            [_artefact_from_costs(bad_name, "api", api_items), phys],
            baseline,
            changed_test_files=[],
        )
        assert report.ok is False, bad_name
        assert report.complete is False, bad_name
        assert report.lanes["api"].status == "unavailable", bad_name
        assert report.lanes["api"].reason_code == "invalid_shard_id", bad_name

    # Contradictory parent-directory vs filename shard labels are also rejected.
    mismatch_dir = tmp_path / "mismatch_parent_stem"
    _write_artefact(
        mismatch_dir
        / "api"
        / "ci-metrics-api-cp312-manylinux-x86_64-shard-2-of-2"
        / "api-cp312-manylinux-x86_64-shard-1-of-2.json",
        [_record(nid, call=cpu, markers=("api",)) for nid, cpu, _ in api_items],
    )
    _write_artefact(
        mismatch_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=cpu, markers=("physics",)) for nid, cpu, _ in phys_items],
    )
    mismatch_json = tmp_path / "mismatch_report.json"
    assert (
        guard.main([
            str(mismatch_dir),
            "--baseline",
            str(baseline_path),
            "--no-changed-test-files",
            "--json",
            str(mismatch_json),
        ])
        == 1
    )
    mismatch_payload = json.loads(mismatch_json.read_text(encoding="utf-8"))
    assert mismatch_payload["lanes"]["api"]["status"] == "unavailable"
    assert mismatch_payload["lanes"]["api"]["reason_code"] == "invalid_shard_id"
    with pytest.raises(ValueError, match="invalid shard identifier"):
        guard.build_baseline([mismatch_dir])

    # Malformed shard labels are also rejected through CLI evaluation and baseline regeneration.
    for idx, bad_label in enumerate(("shard-x-of-2", "shard--1-of-2", "shard-1-of")):
        run_dir = tmp_path / f"bad_shard_run_{idx}"
        _write_artefact(
            run_dir / "api" / f"api-cp312-manylinux-x86_64-{bad_label}.json",
            [_record(nid, call=cpu, markers=("api",)) for nid, cpu, _ in api_items],
        )
        _write_artefact(
            run_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
            [
                _record(nid, call=cpu, markers=("physics",))
                for nid, cpu, _ in phys_items
            ],
        )
        out_json = tmp_path / f"bad_shard_report_{idx}.json"
        rc = guard.main([
            str(run_dir),
            "--baseline",
            str(baseline_path),
            "--no-changed-test-files",
            "--json",
            str(out_json),
        ])
        assert rc == 1, bad_label
        payload = json.loads(out_json.read_text(encoding="utf-8"))
        assert payload["complete"] is False, bad_label
        assert payload["lanes"]["api"]["status"] == "unavailable", bad_label
        assert payload["lanes"]["api"]["reason_code"] == "invalid_shard_id", bad_label

        with pytest.raises(ValueError, match="invalid shard identifier"):
            guard.build_baseline([run_dir])

    # Duplicate shard indices (two shard-1-of-2 files, or [1, 1, 2] of 2).
    half = len(api_items) // 2
    dup_report = guard.check_durations(
        [
            _artefact_from_costs(
                "api-cp312-manylinux-x86_64-shard-1-of-2.json", "api", api_items[:half]
            ),
            _artefact_from_costs(
                "api-cp312-manylinux-x86_64-shard-1-of-2-copy.json",
                "api",
                api_items[:half],
            ),
            _artefact_from_costs(
                "api-cp312-manylinux-x86_64-shard-2-of-2.json", "api", api_items[half:]
            ),
            phys,
        ],
        baseline,
        changed_test_files=[],
    )
    assert dup_report.ok is False
    assert dup_report.complete is False
    assert dup_report.lanes["api"].status == "unavailable"
    assert dup_report.lanes["api"].reason_code == "duplicate_shard_id"


@pytest.mark.smoke
@pytest.mark.core
@pytest.mark.parametrize(
    "bad_value", ["NaN", "Infinity", "-Infinity", "-1.5", "1" + "0" * 400]
)
def test_non_finite_and_negative_cpu_measurements_mark_lane_unavailable_and_emit_strict_json(
    tmp_path: Path, bad_value: str
) -> None:
    """NaN, Inf, negative, and overflowing integer CPU timings never pass as ok and always write strict JSON."""
    baseline = _synthetic_baseline()
    baseline_path = tmp_path / "baseline.json"
    guard.write_baseline(baseline, baseline_path)

    case_slug = "overflow_int" if len(bad_value) > 20 else bad_value
    metrics_dir = tmp_path / f"case_{case_slug}"
    api_records = [
        _record(nid, call=cpu, markers=("api",))
        for nid, cpu in baseline.lanes["api"].reference_tests.items()
    ]
    api_path = metrics_dir / "api" / "api-cp312-manylinux-x86_64.json"
    api_path.parent.mkdir(parents=True, exist_ok=True)
    # Inject raw JSON numeric token (including non-standard NaN / Infinity / 10^400).
    raw_json = json.dumps({"schema_version": 2, "tests": api_records})
    raw_json = raw_json.replace('"call": 2.0', f'"call": {bad_value}', 1)
    api_path.write_text(raw_json, encoding="utf-8")

    _write_artefact(
        metrics_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [
            _record(nid, call=cpu, markers=("physics",))
            for nid, cpu in baseline.lanes["physics"].reference_tests.items()
        ],
    )

    out_json = tmp_path / f"report_{case_slug}.json"
    rc = guard.main([
        str(metrics_dir),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
    ])
    assert rc == 1
    raw_out = out_json.read_text(encoding="utf-8")
    assert "NaN" not in raw_out
    assert "Infinity" not in raw_out
    payload = json.loads(raw_out)
    assert payload["ok"] is False
    assert payload["complete"] is False
    assert payload["lanes"]["api"]["status"] == "unavailable"
    assert payload["lanes"]["api"]["reason_code"] == "invalid_measurements"
    assert payload["lanes"]["api"]["scaled_total_seconds"] is None
    assert payload["lanes"]["physics"]["status"] == "ok"


@pytest.mark.smoke
@pytest.mark.core
def test_derived_arithmetic_overflow_in_evaluation_and_baseline_is_handled_cleanly(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Finite inputs whose derived percentage or aggregate overflows never crash JSON serialisation."""
    # 1. Evaluation overflow: finite baseline total (5.0), finite 1e308 measurement ->
    #    finite delta_ratio (~2e307) whose delta_percent (* 100) overflows to inf.
    api_refs = {f"test/core/test_ov_{i:02d}.py::test_r": 1.0 for i in range(5)}
    phys_refs = {f"test/physics/test_ov_{i:02d}.py::test_p": 1.0 for i in range(5)}
    small_base = guard.DurationBaseline(
        schema_version=guard.BASELINE_SCHEMA_VERSION,
        phase="call",
        min_reference_seconds=1.0,
        max_references_per_lane=200,
        generated_from_runs=("1001",),
        lanes={
            "api": guard.LaneBaseline("api", 5.0, 5, api_refs),
            "physics": guard.LaneBaseline("physics", 5.0, 5, phys_refs),
        },
    )
    baseline_path = tmp_path / "small_baseline.json"
    guard.write_baseline(small_base, baseline_path)

    eval_dir = tmp_path / "eval_overflow"
    api_recs = [_record(nid, call=1.0, markers=("api",)) for nid in api_refs]
    api_recs.append(_record("test/core/test_huge.py::test_huge", call=1e308, markers=("api",)))
    _write_artefact(eval_dir / "api" / "api-cp312-manylinux-x86_64.json", api_recs)
    _write_artefact(
        eval_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=1.0, markers=("physics",)) for nid in phys_refs],
    )
    out_json = tmp_path / "overflow_eval.json"
    rc = guard.main([
        str(eval_dir),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
    ])
    assert rc == 1
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is False
    assert payload["complete"] is False
    assert payload["lanes"]["api"]["status"] == "unavailable"
    assert payload["lanes"]["api"]["reason_code"] == "invalid_measurements"
    assert payload["lanes"]["physics"]["status"] == "ok"

    # 2. Baseline generation overflow: two finite 1e308 measurements sum to inf.
    gen_dir = tmp_path / "gen_overflow"
    gen_api = [_record(nid, call=1.0, markers=("api",)) for nid in api_refs]
    gen_api.extend([
        _record("test/core/test_big1.py::test_a", call=1e308, markers=("api",)),
        _record("test/core/test_big2.py::test_b", call=1e308, markers=("api",)),
    ])
    _write_artefact(gen_dir / "api" / "api-cp312-manylinux-x86_64.json", gen_api)
    _write_artefact(
        gen_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record(nid, call=1.0, markers=("physics",)) for nid in phys_refs],
    )
    out_base = tmp_path / "overflow_baseline.json"
    capsys.readouterr()
    rc_gen = guard.main([
        "--write-baseline",
        "--baseline",
        str(out_base),
        str(gen_dir),
    ])
    assert rc_gen == 2
    assert not out_base.exists()
    assert "[X]" in capsys.readouterr().out

    # 3. Huge baseline measured_test_count (10^400) is rejected cleanly by load_baseline.
    huge_base = guard.baseline_to_dict(small_base)
    huge_base["lanes"]["api"]["measured_test_count"] = 10**400
    huge_base_path = tmp_path / "huge_count_baseline.json"
    huge_base_path.write_text(json.dumps(huge_base), encoding="utf-8")
    with pytest.raises(ValueError, match="measured_test_count"):
        guard.load_baseline(huge_base_path)

    # 4. Malformed supplied session metadata (string exit_code, negative node_count, negative outcome)
    #    marks the lane unavailable in evaluation and is rejected by build_baseline.
    for case_name, extra_meta in (
        ("str_exit", {"result": {"exit_code": "2"}}),
        ("neg_collected", {"inventory": {"node_count": -1}}),
        ("neg_outcome", {"result": {"exit_code": 0, "outcomes": {"passed": -3}}}),
    ):
        meta_dir = tmp_path / f"meta_{case_name}"
        api_p = meta_dir / "api" / "api-cp312-manylinux-x86_64.json"
        api_p.parent.mkdir(parents=True, exist_ok=True)
        api_p.write_text(
            json.dumps({
                "schema_version": 2,
                "tests": [_record(nid, call=1.0, markers=("api",)) for nid in api_refs],
                **extra_meta,
            }),
            encoding="utf-8",
        )
        _write_artefact(
            meta_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
            [_record(nid, call=1.0, markers=("physics",)) for nid in phys_refs],
        )
        meta_json = tmp_path / f"meta_{case_name}.json"
        assert (
            guard.main([
                str(meta_dir),
                "--baseline",
                str(baseline_path),
                "--no-changed-test-files",
                "--json",
                str(meta_json),
            ])
            == 1
        ), case_name
        meta_payload = json.loads(meta_json.read_text(encoding="utf-8"))
        assert meta_payload["lanes"]["api"]["status"] == "unavailable", case_name
        assert meta_payload["lanes"]["api"]["reason_code"] == "invalid_session_metadata", (
            case_name
        )
        with pytest.raises(ValueError, match="malformed session metadata"):
            guard.build_baseline([meta_dir])

    # 5. Overflowing max_regression (1e308 * 100 = inf) is rejected; max_regression = 0.0 is accepted.
    with pytest.raises(ValueError, match="max_regression"):
        guard.check_durations([], small_base, max_regression=1e308)
    zero_thresh = guard.check_durations([], small_base, max_regression=0.0)
    assert zero_thresh.max_regression == 0.0


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

    # Documented baseline refresh uses wildcard for both API and physics.
    assert '-p "ci-metrics-physics-cp312-manylinux-x86_64*"' in (guard.__doc__ or "")

    workflow = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    job = workflow["jobs"]["test_duration_guard"]
    cost_job = workflow["jobs"]["cost_markers_pr"]
    # Report-only and never wired into pr-gate or report_scheduled_run.
    assert job["continue-on-error"] is True
    assert "test_duration_guard" not in workflow["jobs"]["pr-gate"]["needs"]
    assert (
        "test_duration_guard"
        not in workflow["jobs"]["report_scheduled_run"]["needs"]
    )
    # Sticky-comment writers are strictly serialised: post-ci-summary -> cost_markers_pr -> test_duration_guard.
    assert "post-ci-summary" in cost_job["needs"]
    assert "post-ci-summary" in job["needs"]
    assert "cost_markers_pr" in job["needs"]
    # Ready PR tiers (standard / physics-core), never smoke/core/cfg/physics-full/merge_group/schedule.
    assert "github.event_name == 'pull_request'" in job["if"]
    assert "needs.determine_matrix.outputs.test_tier == 'standard'" in job["if"]
    assert "needs.determine_matrix.outputs.test_tier == 'physics-core'" in job["if"]
    assert {"tooling", "unit", "build_wheels"} <= set(job["needs"])
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

    # Downloads both unsharded and N-sharded Linux cp312 artefacts (legacy api + Schema 2 lanes) into separate subdirectories.
    for lane in ("api", "tooling", "unit", "native", "physics"):
        dl = next(
            s
            for s in job["steps"]
            if s.get("name") == f"Download the Linux cp312 {lane} metrics"
        )
        assert dl["with"]["pattern"] == f"ci-metrics-{lane}-cp312-manylinux-x86_64*"
        assert "name" not in dl["with"]
        assert not dl["with"].get("merge-multiple")

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


def _write_multi_axis_run(
    root: Path,
    *,
    factors: dict[str, float],
    unit_shard_factors: tuple[float, float],
    include_tooling: bool = True,
    extra_native: tuple[str, float] | None = None,
    extra_unit: tuple[str, float] | None = None,
    include_legacy_api: bool = False,
) -> None:
    tooling_nodes = [f"test/core/test_tool_{i:02d}.py::test_t" for i in range(8)]
    unit_nodes = [f"test/data_model/test_unit_{i:02d}.py::test_u" for i in range(30)]
    native_nodes = [f"test/core/test_native_{i:02d}.py::test_n" for i in range(10)]
    phys_nodes = [f"test/physics/test_phys_{i:02d}.py::test_p" for i in range(10)]

    if include_tooling:
        _write_artefact(
            root / "tooling" / "tooling-cp312-linux-x86_64.json",
            [
                _record(nid, call=1.5 * factors["tooling"], markers=("tooling",))
                for nid in tooling_nodes
            ],
        )
    for shard_idx, s_factor in enumerate(unit_shard_factors, start=1):
        recs = [
            _record(nid, call=2.0 * s_factor, markers=("unit",))
            for nid in unit_nodes
            if _shard_of(nid, 2) == shard_idx
        ]
        if extra_unit is not None and shard_idx == 1:
            recs.append(
                _record(extra_unit[0], call=extra_unit[1] * s_factor, markers=("unit",))
            )
        _write_artefact(
            root
            / "unit"
            / f"ci-metrics-unit-cp312-linux-x86_64-shard-{shard_idx}-of-2"
            / f"unit-cp312-linux-x86_64-shard-{shard_idx}-of-2.json",
            recs,
        )
    native_recs = [
        _record(nid, call=3.0 * factors["native"], markers=("native",))
        for nid in native_nodes
    ]
    if extra_native is not None:
        native_recs.append(
            _record(
                extra_native[0],
                call=extra_native[1] * factors["native"],
                markers=("native",),
            )
        )
    _write_artefact(root / "native" / "native-cp312-manylinux-x86_64.json", native_recs)
    _write_artefact(
        root / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [
            _record(nid, call=4.0 * factors["physics"], markers=("physics",))
            for nid in phys_nodes
        ],
    )
    if include_legacy_api:
        # Transitional legacy API artefact must be ignored when building/evaluating schema 2.
        _write_artefact(
            root / "api" / "api-cp312-manylinux-x86_64.json",
            [_record("test/core/test_legacy.py::test_old", call=5.0, markers=("api",))],
        )


def _build_schema2_baseline_file(tmp_path: Path) -> Path:
    unit_factors = {"tooling": 1.0, "native": 1.0, "physics": 1.0}
    run1 = tmp_path / "runs" / "2001"
    run2 = tmp_path / "runs" / "2002"
    _write_multi_axis_run(
        run1,
        factors=unit_factors,
        unit_shard_factors=(0.8, 1.2),
        include_legacy_api=True,
    )
    _write_multi_axis_run(
        run2,
        factors=unit_factors,
        unit_shard_factors=(1.2, 0.8),
        include_legacy_api=True,
    )
    baseline_path = tmp_path / "baseline_v2.json"
    rc = guard.main([
        "--write-baseline",
        "--baseline-schema",
        "2",
        "--baseline",
        str(baseline_path),
        str(run1),
        str(run2),
    ])
    assert rc == 0
    return baseline_path


@pytest.mark.smoke
@pytest.mark.core
def test_schema2_multi_axis_baseline_build_and_clean_bimodal_eval(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Schema 2 (`tooling|unit|native|physics`) builds, loads, and normalises bimodal shards."""
    baseline_path = _build_schema2_baseline_file(tmp_path)
    capsys.readouterr()

    raw = json.loads(baseline_path.read_text(encoding="utf-8"))
    assert raw["schema_version"] == 2
    assert list(raw["lanes"]) == ["tooling", "unit", "native", "physics"]
    assert raw["lanes"]["tooling"]["total_cpu_seconds"] == pytest.approx(12.0)
    assert raw["lanes"]["unit"]["total_cpu_seconds"] == pytest.approx(60.0)
    assert raw["lanes"]["native"]["total_cpu_seconds"] == pytest.approx(30.0)
    assert raw["lanes"]["physics"]["total_cpu_seconds"] == pytest.approx(40.0)
    loaded = guard.load_baseline(baseline_path)
    assert loaded.schema_version == 2
    assert tuple(loaded.lanes) == ("tooling", "unit", "native", "physics")

    clean_run = tmp_path / "eval_clean"
    _write_multi_axis_run(
        clean_run,
        factors={"tooling": 0.75, "native": 1.35, "physics": 1.60},
        unit_shard_factors=(0.70, 1.50),
        include_legacy_api=True,
    )
    clean_json = tmp_path / "clean_v2.json"
    clean_md = tmp_path / "clean_v2.md"
    rc = guard.main([
        str(clean_run),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(clean_json),
        "--markdown",
        str(clean_md),
        "--annotations",
    ])
    capsys.readouterr()
    assert rc == 0
    payload = json.loads(clean_json.read_text(encoding="utf-8"))
    assert payload["ok"] is True
    assert payload["complete"] is True
    assert set(payload["lanes"]) == {"tooling", "unit", "native", "physics"}
    assert "api-cp312-manylinux-x86_64.json" not in payload["sources"]
    assert payload["lanes"]["unit"]["artefact_count"] == 2


@pytest.mark.smoke
@pytest.mark.core
def test_schema2_multi_axis_detects_regression_marker_drift_and_missing_lane(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Schema 2 flags lane regressions, under-marked changed tests, and missing axes."""
    baseline_path = _build_schema2_baseline_file(tmp_path)
    capsys.readouterr()

    drift_run = tmp_path / "eval_drift"
    _write_multi_axis_run(
        drift_run,
        factors={"tooling": 1.0, "native": 1.2, "physics": 1.0},
        unit_shard_factors=(0.70, 1.40),
        extra_native=("test/core/test_added_native.py::test_slow_bridge", 4.5),
        extra_unit=("test/data_model/test_added_unit.py::test_heavy_unit", 25.0),
    )
    drift_json = tmp_path / "drift_v2.json"
    rc_drift = guard.main([
        str(drift_run),
        "--baseline",
        str(baseline_path),
        "--test-file",
        "test/data_model/test_added_unit.py",
        "--json",
        str(drift_json),
        "--annotations",
    ])
    drift_out = capsys.readouterr().out
    assert rc_drift == 1
    drift_payload = json.loads(drift_json.read_text(encoding="utf-8"))
    assert drift_payload["lanes"]["native"]["regressed"] is True
    assert drift_payload["lanes"]["native"]["delta_ratio"] == pytest.approx(0.15)
    assert len(drift_payload["marker_warnings"]) == 1
    assert drift_payload["marker_warnings"][0]["lane"] == "unit"
    assert drift_payload["marker_warnings"][0]["wanted"] == "medium"
    assert "::warning title=Test duration regression::native lane" in drift_out
    assert (
        "::warning title=Test cost marker::"
        "test/data_model/test_added_unit.py::test_heavy_unit (unit)" in drift_out
    )

    missing_tooling = tmp_path / "eval_missing_tooling"
    _write_multi_axis_run(
        missing_tooling,
        factors={"tooling": 1.0, "native": 1.0, "physics": 1.0},
        unit_shard_factors=(1.0, 1.0),
        include_tooling=False,
    )
    incomplete_json = tmp_path / "incomplete_v2.json"
    rc_inc = guard.main([
        str(missing_tooling),
        "--baseline",
        str(baseline_path),
        "--no-changed-test-files",
        "--json",
        str(incomplete_json),
    ])
    capsys.readouterr()
    assert rc_inc == 1
    inc_payload = json.loads(incomplete_json.read_text(encoding="utf-8"))
    assert inc_payload["complete"] is False
    assert inc_payload["status"] == "incomplete"
    assert inc_payload["incomplete_lanes"] == ["tooling"]


@pytest.mark.smoke
@pytest.mark.core
def test_schema2_cohorts_alias_and_schema_validation_fail_closed(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Schema 2 accepts `lanes` or `cohorts`, and both schemas fail closed on mismatches."""
    v2_base = guard.DurationBaseline(
        schema_version=2,
        phase="call",
        min_reference_seconds=1.0,
        max_references_per_lane=200,
        generated_from_runs=("2001",),
        lanes={
            lane: guard.LaneBaseline(
                lane,
                10.0,
                5,
                {f"test/core/test_{lane}_{i}.py::test_r": 2.0 for i in range(5)},
            )
            for lane in ("tooling", "unit", "native", "physics")
        },
    )
    v2_dict = guard.baseline_to_dict(v2_base)

    # Schema 2 with `cohorts` alias loads identically to `lanes`.
    cohorts_path = tmp_path / "v2_cohorts.json"
    cohorts_path.write_text(
        json.dumps({
            **{k: v for k, v in v2_dict.items() if k != "lanes"},
            "cohorts": v2_dict["lanes"],
        }),
        encoding="utf-8",
    )
    loaded = guard.load_baseline(cohorts_path)
    assert loaded.schema_version == 2
    assert tuple(loaded.lanes) == ("tooling", "unit", "native", "physics")

    # Schema 2 defining both `lanes` and `cohorts` is rejected.
    both_path = tmp_path / "v2_both.json"
    both_path.write_text(
        json.dumps({**v2_dict, "cohorts": v2_dict["lanes"]}), encoding="utf-8"
    )
    with pytest.raises(ValueError, match="cannot define both 'lanes' and 'cohorts'"):
        guard.load_baseline(both_path)

    # Schema 2 missing any required axis or carrying unexpected lanes is rejected.
    for drop_lane in ("tooling", "unit", "native", "physics"):
        missing_dict = json.loads(json.dumps(v2_dict))
        del missing_dict["lanes"][drop_lane]
        bad_path = tmp_path / f"v2_missing_{drop_lane}.json"
        bad_path.write_text(json.dumps(missing_dict), encoding="utf-8")
        with pytest.raises(ValueError, match=rf"missing required lane.*{drop_lane}"):
            guard.load_baseline(bad_path)

    for extra_lane in ("api", "bogus"):
        extra_dict = json.loads(json.dumps(v2_dict))
        extra_dict["lanes"][extra_lane] = extra_dict["lanes"]["unit"]
        bad_path = tmp_path / f"v2_extra_{extra_lane}.json"
        bad_path.write_text(json.dumps(extra_dict), encoding="utf-8")
        with pytest.raises(ValueError, match=rf"unexpected lane.*{extra_lane}"):
            guard.load_baseline(bad_path)

    # Schema 1 rejects `cohorts` alias, extra schema 2 lanes, and invalid schema versions.
    v1_dict = guard.baseline_to_dict(_synthetic_baseline())
    bad_v1_cohorts = tmp_path / "v1_cohorts.json"
    bad_v1_cohorts.write_text(
        json.dumps({
            **{k: v for k, v in v1_dict.items() if k != "lanes"},
            "cohorts": v1_dict["lanes"],
        }),
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="must define required lanes"):
        guard.load_baseline(bad_v1_cohorts)

    v1_extra = json.loads(json.dumps(v1_dict))
    v1_extra["lanes"]["tooling"] = v1_extra["lanes"]["api"]
    bad_v1_extra = tmp_path / "v1_extra.json"
    bad_v1_extra.write_text(json.dumps(v1_extra), encoding="utf-8")
    with pytest.raises(ValueError, match=r"unexpected lane.*tooling"):
        guard.load_baseline(bad_v1_extra)

    for idx, bad_ver in enumerate((0, 3, True, [], {})):
        bad_ver_path = tmp_path / f"bad_ver_{idx}.json"
        bad_ver_path.write_text(
            json.dumps({**v1_dict, "schema_version": bad_ver}), encoding="utf-8"
        )
        with pytest.raises(ValueError, match="unsupported baseline schema"):
            guard.load_baseline(bad_ver_path)
        with pytest.raises(ValueError, match="unsupported baseline schema"):
            guard.build_baseline([tmp_path], schema_version=bad_ver)  # type: ignore[arg-type]
        assert guard.main([str(tmp_path), "--baseline", str(bad_ver_path)]) == 2
        cli_out = capsys.readouterr().out
        assert "[X]" in cli_out
        assert "unsupported baseline schema" in cli_out


@pytest.mark.smoke
@pytest.mark.core
def test_infer_lane_dual_schema_ambiguity_and_transitional_shim_isolation(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """`infer_lane` handles schema 1/2 paths, transitional shim markers, and cross-schema isolation."""
    # Path-based inference across schema 1 & 2, plus ambiguity & tmp_path collision resistance.
    assert (
        guard.infer_lane(tmp_path / "tooling-cp312-linux-x86_64.json", []) == "tooling"
    )
    assert guard.infer_lane(tmp_path / "unit-cp312-linux-x86_64.json", []) == "unit"
    assert (
        guard.infer_lane(tmp_path / "native-cp312-manylinux-x86_64.json", [])
        == "native"
    )
    assert (
        guard.infer_lane(
            tmp_path / "physics-core-cp312-manylinux-x86_64-pytest.json", []
        )
        == "physics"
    )
    assert (
        guard.infer_lane(
            tmp_path / "ci-metrics-native-cp312-manylinux-x86_64" / "pytest.json", []
        )
        == "native"
    )
    assert guard.infer_lane(tmp_path / "api-physics.json", []) is None
    assert guard.infer_lane(tmp_path / "unit-native-cp312.json", []) is None
    assert (
        guard.infer_lane(tmp_path / "ci-metrics-unit-native-cp312" / "pytest.json", [])
        is None
    )
    # Labelled filenames inside ambiguous or contradictory directories must fail closed.
    assert (
        guard.infer_lane(
            tmp_path / "ci-metrics-unit-native-cp312" / "unit-cp312.json", []
        )
        is None
    )
    assert (
        guard.infer_lane(tmp_path / "native" / "unit-cp312.json", []) is None
    )

    # Unrelated pytest temp dir containing `unit` must not override marker fallback.
    api_test = cost_markers.TestCost(
        "test/core/test_x.py::test_a", ("api",), 1.0, 1.2, "results.json"
    )
    assert (
        guard.infer_lane(tmp_path / "test_unit_something0" / "results.json", [api_test])
        == "api"
    )

    # Transitional dual-marked (`unit` + `api` shim) artefact with generic filename resolves
    # by active `allowed_lanes` and fails closed when unscoped.
    shim_test = cost_markers.TestCost(
        "test/data_model/test_x.py::test_u", ("api", "unit"), 1.0, 1.2, "results.json"
    )
    generic_path = tmp_path / "run_x" / "results.json"
    assert guard.infer_lane(generic_path, [shim_test]) is None
    assert (
        guard.infer_lane(generic_path, [shim_test], allowed_lanes=("api", "physics"))
        == "api"
    )
    assert (
        guard.infer_lane(
            generic_path,
            [shim_test],
            allowed_lanes=("tooling", "unit", "native", "physics"),
        )
        == "unit"
    )
    # Mixed-lane generic artefact (`physics` + foreign `api`-only test) must fail closed
    # under schema 2 rather than assigning the foreign test to `physics`.
    phys_test = cost_markers.TestCost(
        "test/physics/test_p.py::test_p", ("physics",), 2.0, 2.4, "results.json"
    )
    assert (
        guard.infer_lane(
            generic_path,
            [phys_test, api_test],
            allowed_lanes=("tooling", "unit", "native", "physics"),
        )
        is None
    )

    # Cross-schema CLI isolation + ambiguous-directory rejection: schema 2 artefacts
    # (and a labelled api file under an ambiguous directory) evaluated against a
    # schema 1 baseline must leave `api` missing rather than entering evaluation.
    v1_baseline = _synthetic_baseline()
    v1_path = tmp_path / "v1_baseline.json"
    guard.write_baseline(v1_baseline, v1_path)

    mixed_dir = tmp_path / "schema2_artefacts"
    _write_artefact(
        mixed_dir / "unit" / "unit-cp312-linux-x86_64.json",
        [
            _record(nid, call=cpu, markers=("api", "unit"))
            for nid, cpu in v1_baseline.lanes["api"].reference_tests.items()
        ],
    )
    _write_artefact(
        mixed_dir
        / "ci-metrics-api-unit-cp312"
        / "api-cp312-manylinux-x86_64.json",
        [
            _record(nid, call=cpu, markers=("api",))
            for nid, cpu in v1_baseline.lanes["api"].reference_tests.items()
        ],
    )
    _write_artefact(
        mixed_dir / "physics" / "physics-cp312-manylinux-x86_64-pytest.json",
        [
            _record(nid, call=cpu, markers=("physics",))
            for nid, cpu in v1_baseline.lanes["physics"].reference_tests.items()
        ],
    )
    out_json = tmp_path / "cross_schema.json"
    rc = guard.main([
        str(mixed_dir),
        "--baseline",
        str(v1_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
    ])
    capsys.readouterr()
    assert rc == 1
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["complete"] is False
    assert payload["lanes"]["physics"]["status"] == "ok"
    assert payload["lanes"]["api"]["status"] == "missing"
    assert payload["incomplete_lanes"] == ["api"]
    assert "unit-cp312-linux-x86_64.json" not in payload["sources"]


@pytest.mark.smoke
@pytest.mark.core
def test_mixed_foreign_lane_generic_artefact_fails_closed_under_schema2(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """An unlabelled `physics` + foreign `api`-only artefact must not pollute schema 2 `physics`."""
    v2_path = _build_schema2_baseline_file(tmp_path / "base")
    capsys.readouterr()
    v2_loaded = guard.load_baseline(v2_path)
    eval_dir = tmp_path / "eval"
    mixed_recs = [
        _record(nid, call=cpu, markers=("physics",))
        for nid, cpu in v2_loaded.lanes["physics"].reference_tests.items()
    ]
    mixed_recs.append(
        _record(
            "test/core/test_foreign_api.py::test_heavy", call=500.0, markers=("api",)
        )
    )
    _write_artefact(eval_dir / "results.json", mixed_recs)
    out_json = tmp_path / "report.json"
    rc = guard.main([
        str(eval_dir),
        "--baseline",
        str(v2_path),
        "--no-changed-test-files",
        "--json",
        str(out_json),
    ])
    capsys.readouterr()
    assert rc == 1
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["lanes"]["physics"]["status"] == "missing"
    assert payload["lanes"]["physics"]["regressed"] is False
    assert "results.json" not in payload["sources"]
