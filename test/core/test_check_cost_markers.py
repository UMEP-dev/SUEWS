"""Contract tests for the cost-marker drift check.

`scripts/lint/check_cost_markers.py` compares the `medium` and `slow` markers
recorded per test in a ci-metrics artefact with the CPU seconds recorded beside
them. These tests drive it with synthetic artefacts so no real test in the
suite has to be mis-marked to prove the check trips.
"""

from __future__ import annotations

import json
from pathlib import Path
import sys
from typing import Any

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
# scripts/ is not a package on sys.path under a bare `pytest` invocation
# (only `python -m pytest` puts the repository root there), so add it.
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.lint import check_cost_markers as checker  # noqa: E402

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]

MEDIUM = 10.0
SLOW = 30.0
BAND = 1.5


def _record(
    node_id: str,
    *,
    call: float,
    setup: float = 0.0,
    markers: tuple[str, ...] = (),
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
        "wall_seconds": {key: value * 1.5 for key, value in phases.items()},
    }


def _artefact(path: Path, records: list[dict[str, Any]]) -> Path:
    path.write_text(json.dumps({"schema_version": 2, "tests": records}), encoding="utf-8")
    return path


def _cost(
    node_id: str,
    cpu: float,
    *markers: str,
    outcome: str = "passed",
    slow_reason: str | None = None,
) -> checker.TestCost:
    reasons = (("slow", slow_reason),) if slow_reason else ()
    return checker.TestCost(
        node_id, tuple(sorted(markers)), cpu, cpu * 1.5, "synthetic.json", outcome, reasons
    )


def _check(tests: list[checker.TestCost]) -> checker.Report:
    return checker.check_costs(tests, medium_seconds=MEDIUM, slow_seconds=SLOW, band=BAND)


# Marked smoke as well as core: a tests-only PR runs the smoke tier, and the
# test that proves the check trips must run in the CI of the PR that edits it.
@pytest.mark.smoke
@pytest.mark.core
def test_each_disagreement_is_flagged_with_its_class_and_wanted_marker() -> None:
    """The four drift classes are named; agreeing markers are silent."""
    tests = [
        _cost("t/a.py::test_fast_unmarked", 1.0),
        _cost("t/a.py::test_medium_unmarked", 16.0),
        _cost("t/a.py::test_slow_unmarked", 45.0),
        _cost("t/a.py::test_medium_ok", 20.0, "medium"),
        _cost("t/a.py::test_medium_is_slow", 90.0, "medium"),
        _cost("t/a.py::test_medium_is_fast", 2.0, "medium", "core"),
        _cost("t/a.py::test_slow_ok", 120.0, "slow"),
        _cost("t/a.py::test_bare_slow_is_fast", 0.5, "slow"),
        _cost("t/a.py::test_bare_slow_is_medium", 15.0, "slow"),
    ]

    report = _check(tests)

    assert not report.ok
    flagged = {
        finding.test.node_id: (finding.kind, finding.wanted) for finding in report.findings
    }
    assert flagged == {
        "t/a.py::test_medium_unmarked": ("unmarked-over-medium", "medium"),
        "t/a.py::test_slow_unmarked": ("unmarked-over-medium", "slow"),
        "t/a.py::test_medium_is_slow": ("medium-over-slow", "slow"),
        "t/a.py::test_medium_is_fast": ("medium-under-medium", "no cost marker"),
        "t/a.py::test_bare_slow_is_fast": ("slow-without-reason", "no cost marker"),
        "t/a.py::test_bare_slow_is_medium": ("slow-without-reason", "medium"),
    }
    message = report.message()
    assert "[X] 6 marker(s)" in message
    for node_id in flagged:
        assert node_id in message
    assert "t/a.py::test_fast_unmarked" not in message
    assert "t/a.py::test_medium_ok" not in message
    assert "patterns.md" in message
    assert "pytest.mark.slow(reason=...)" in message


def test_slow_marker_with_a_stated_reason_is_accepted_at_any_cost() -> None:
    """`slow(reason=...)` states a non-CPU cause; the check does not second-guess it."""
    report = _check([
        _cost("t/a.py::test_needs_network", 0.2, "slow", slow_reason="downloads from the CDS API"),
        _cost("t/a.py::test_policy", 3.0, "slow", slow_reason="non-anchor version"),
    ])

    assert report.ok
    assert "[OK]" in report.message()


def test_readings_inside_a_band_accept_either_marking() -> None:
    """Between threshold / band and threshold * band, marked and unmarked both hold."""
    inside = _check([
        _cost("t/a.py::test_medium_just_under", MEDIUM / BAND, "medium"),
        _cost("t/a.py::test_unmarked_just_over", MEDIUM * BAND - 0.1),
        _cost("t/a.py::test_medium_near_slow", SLOW * BAND - 0.1, "medium"),
        _cost("t/a.py::test_bare_slow_just_under", SLOW / BAND, "slow"),
    ])
    beyond = _check([
        _cost("t/a.py::test_medium_far_under", MEDIUM / BAND - 0.1, "medium"),
        _cost("t/a.py::test_unmarked_far_over", MEDIUM * BAND),
        _cost("t/a.py::test_medium_far_over_slow", SLOW * BAND, "medium"),
        _cost("t/a.py::test_bare_slow_far_under", SLOW / BAND - 0.1, "slow"),
    ])

    assert inside.ok
    assert {finding.test.node_id: finding.kind for finding in beyond.findings} == {
        "t/a.py::test_medium_far_under": "medium-under-medium",
        "t/a.py::test_unmarked_far_over": "unmarked-over-medium",
        "t/a.py::test_medium_far_over_slow": "medium-over-slow",
        "t/a.py::test_bare_slow_far_under": "slow-without-reason",
    }


def test_nodes_without_a_call_phase_are_not_judged() -> None:
    """Skipped and xfailed nodes carry no measurement; they are counted, not flagged."""
    report = _check([
        _cost("t/a.py::test_skipped_slow", 0.0, "slow", outcome="skipped"),
        _cost("t/a.py::test_xfailed_medium", 0.0, "medium", outcome="xfailed"),
        _cost("t/a.py::test_ran", 1.0),
    ])

    assert report.ok
    assert report.unmeasured == 2
    assert [test.node_id for test in report.tests] == ["t/a.py::test_ran"]
    assert "2 skipped or xfailed not judged" in report.message()


def test_node_seen_in_two_artefacts_is_judged_on_its_largest_cost() -> None:
    """A physics-and-api file runs in both lanes; the larger reading decides."""
    tests = [
        checker.TestCost("t/a.py::test_shared", ("api", "physics"), 4.0, 6.0, "api.json"),
        checker.TestCost("t/a.py::test_shared", ("api", "physics"), 16.0, 20.0, "physics.json"),
    ]

    report = _check(tests)

    assert len(report.tests) == 1
    assert report.tests[0].source == "physics.json"
    assert [finding.kind for finding in report.findings] == ["unmarked-over-medium"]


def test_thresholds_and_band_are_validated() -> None:
    with pytest.raises(ValueError, match="medium threshold must be below"):
        checker.check_costs([], medium_seconds=60.0, slow_seconds=10.0)
    with pytest.raises(ValueError, match="band must be at least 1"):
        checker.check_costs([], medium_seconds=10.0, slow_seconds=30.0, band=0.5)


def test_loader_reads_api_and_physics_artefacts_and_skips_the_rest(tmp_path: Path) -> None:
    """A downloaded artefact directory is read whole: pytest JSONs in, the rest skipped."""
    api_dir = tmp_path / "ci-metrics-api-cp312-manylinux-x86_64"
    physics_dir = tmp_path / "ci-metrics-physics-cp312-manylinux-x86_64"
    api_dir.mkdir()
    physics_dir.mkdir()
    _artefact(
        api_dir / "api-cp312-manylinux-x86_64.json",
        [
            _record(
                "t/api.py::test_api",
                call=0.2,
                markers=("api", "slow"),
                reasons={"slow": "needs credentials"},
            )
        ],
    )
    pytest_payload = _artefact(
        physics_dir / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record("t/phys.py::test_phys", call=3.0, setup=40.0, markers=("physics",))],
    )
    # The wheel-job file embeds the same pytest payload; it must not be
    # counted a second time.
    (physics_dir / "physics-cp312-manylinux-x86_64-wheel-job.json").write_text(
        json.dumps({
            "schema_version": 1,
            "kind": "wheel-job-ci-metrics",
            "pytest_metrics": json.loads(pytest_payload.read_text(encoding="utf-8")),
        }),
        encoding="utf-8",
    )
    (physics_dir / "physics-cp312-manylinux-x86_64-phases.json").write_text(
        json.dumps({"schema_version": 1, "phases": {}}), encoding="utf-8"
    )
    # A schema-2 artefact from before per-test records were added.
    (tmp_path / "old.json").write_text(
        json.dumps({"schema_version": 2, "inventory": {"node_count": 0}}), encoding="utf-8"
    )

    skipped: list[str] = []
    tests, sources = checker.load_tests([tmp_path], skipped=skipped)

    assert sorted(sources) == [
        "api-cp312-manylinux-x86_64.json",
        "physics-cp312-manylinux-x86_64-pytest.json",
    ]
    assert [test.node_id for test in tests] == ["t/api.py::test_api", "t/phys.py::test_phys"]
    assert tests[0].slow_reason == "needs credentials"
    assert {Path(path).name for path in skipped} == {
        "old.json",
        "physics-cp312-manylinux-x86_64-phases.json",
        "physics-cp312-manylinux-x86_64-wheel-job.json",
    }
    # The default cost is the call phase; the fixture setup is available on request.
    assert tests[1].cpu_seconds == 3.0
    total_tests, _ = checker.load_tests([tmp_path], phase="total")
    assert total_tests[1].cpu_seconds == 43.0


def test_unsupported_metrics_schema_is_an_error_not_a_skip(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """A per-test artefact under another schema version stops the check.

    Skipping it would judge the other lane alone and print [OK], so a schema
    bump could read as no drift.
    """
    _artefact(tmp_path / "api.json", [_record("t/a.py::test_fast", call=0.1)])
    (tmp_path / "physics-pytest.json").write_text(
        json.dumps({
            "schema_version": 3,
            "tests": [json.loads(json.dumps(_record("t/p.py::test_heavy", call=99.0)))],
        }),
        encoding="utf-8",
    )

    with pytest.raises(checker.UnsupportedMetricsSchema, match="schema 3 not supported"):
        checker.load_tests([tmp_path])

    assert checker.main([str(tmp_path)]) == 2
    out = capsys.readouterr().out
    assert "[X]" in out
    assert "metrics schema 3 not supported by this lint (expects 2)" in out
    assert "[OK]" not in out


def test_cli_exit_codes_and_markdown_summary(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Exit 1 on drift, 0 when clean, 2 with nothing to read; Markdown appended when asked."""
    clean = _artefact(
        tmp_path / "clean.json",
        [
            _record("t/a.py::test_fast", call=0.1),
            _record("t/a.py::test_slow", call=99.0, markers=("slow",)),
        ],
    )
    drift = _artefact(
        tmp_path / "drift.json",
        [_record("t/a.py::test_heavy_unmarked", call=42.0)],
    )
    summary = tmp_path / "summary.md"
    thresholds = ["--medium-seconds", "10", "--slow-seconds", "30"]

    assert checker.main([str(clean), *thresholds]) == 0
    assert "[OK]" in capsys.readouterr().out

    assert (
        checker.main([str(drift), *thresholds, "--markdown", str(summary), "--histogram"])
        == 1
    )
    out = capsys.readouterr().out
    # 42 is over the slow threshold but inside its band (30 * 1.5), so medium is asked for.
    assert "t/a.py::test_heavy_unmarked: 42.0 CPU-s (no cost marker -> medium" in out
    assert "| CPU-s | tests | cumulative |" in out
    written = summary.read_text(encoding="utf-8")
    assert "## Cost-marker drift" in written
    assert "| `t/a.py::test_heavy_unmarked` | 42.0 | - | medium | unmarked-over-medium |" in written
    assert "### Distribution" in written

    (tmp_path / "phases.json").write_text(json.dumps({"schema_version": 1}), encoding="utf-8")
    assert checker.main([str(tmp_path / "phases.json")]) == 2
    assert "[skip]" in capsys.readouterr().out


def test_restriction_judges_only_the_tests_in_the_named_files() -> None:
    """`test_files` narrows both the judged and the counted tests to those files."""
    tests = [
        _cost("test/core/test_new.py::test_heavy", 45.0),
        _cost("test/core/test_new.py::test_fast", 0.1),
        _cost("test/core/test_new.py::test_skipped", 0.0, "slow", outcome="skipped"),
        _cost("test/core/test_old.py::test_heavy_too", 45.0),
        _cost("test/core/test_old.py::test_skipped", 0.0, outcome="skipped"),
    ]

    narrowed = checker.check_costs(
        tests,
        medium_seconds=MEDIUM,
        slow_seconds=SLOW,
        band=BAND,
        # A leading ./ and Windows separators name the same file as the node id.
        test_files=["./test/core/test_new.py", "test\\core\\test_new.py"],
    )

    assert narrowed.restricted_to == ("test/core/test_new.py",)
    assert [finding.test.node_id for finding in narrowed.findings] == [
        "test/core/test_new.py::test_heavy"
    ]
    assert len(narrowed.tests) == 2
    assert narrowed.unmeasured == 1
    assert "2 measured tests in 1 given test file(s)" in narrowed.message()

    unrelated = checker.check_costs(
        tests, medium_seconds=MEDIUM, slow_seconds=SLOW, band=BAND,
        test_files=["test/core/test_unrelated.py"],
    )
    assert unrelated.ok
    assert unrelated.tests == []
    assert unrelated.unmeasured == 0

    unrestricted = _check(tests)
    assert unrestricted.restricted_to is None
    assert len(unrestricted.findings) == 2


def test_node_file_is_the_part_before_the_first_separator() -> None:
    assert checker.node_file("test/a.py::TestX::test_y[1-a::b]") == "test/a.py"
    assert checker.node_file("test/a.py") == "test/a.py"


def test_cli_restricts_to_test_files_and_writes_json(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """`--test-file` narrows the check; `--json` records counts and each flagged test."""
    artefact = _artefact(
        tmp_path / "api.json",
        [
            _record("test/core/test_new.py::test_heavy_unmarked", call=42.0),
            _record("test/core/test_new.py::test_fast", call=0.1),
            _record("test/core/test_old.py::test_medium_is_fast", call=1.0, markers=("medium",)),
        ],
    )
    thresholds = ["--medium-seconds", "10", "--slow-seconds", "30"]
    out_json = tmp_path / "out" / "cost-markers.json"

    assert checker.main([
        str(artefact), *thresholds,
        "--test-file", "test/core/test_new.py", "--json", str(out_json),
    ]) == 1
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is False
    assert payload["flagged"] == 1
    assert payload["measured"] == 2
    assert payload["restricted_to"] == ["test/core/test_new.py"]
    assert payload["findings"] == [
        {
            "node_id": "test/core/test_new.py::test_heavy_unmarked",
            "kind": "unmarked-over-medium",
            "cpu_seconds": 42.0,
            "current": None,
            "wanted": "medium",
            "source": "api.json",
        }
    ]
    assert "test_old.py" not in capsys.readouterr().out

    # A file with no measured tests is clean and silent about the rest.
    assert checker.main([
        str(artefact), *thresholds,
        "--test-file", "test/core/test_unrelated.py", "--json", str(out_json),
    ]) == 0
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert (payload["ok"], payload["flagged"], payload["measured"]) == (True, 0, 0)
    capsys.readouterr()

    # Without --test-file every test is judged and restricted_to is null.
    assert checker.main([str(artefact), *thresholds, "--json", str(out_json)]) == 1
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["restricted_to"] is None
    assert payload["flagged"] == 2


def test_require_axis_rejects_partial_lane_metrics_and_workflow_wires_both_axes(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Custom dispatches with only physics (or only api) metrics cannot report clean (#2284)."""
    import yaml

    metrics_dir = tmp_path / "ci-metrics"
    physics_dir = metrics_dir / "physics"
    api_dir = metrics_dir / "api"
    physics_dir.mkdir(parents=True)
    api_dir.mkdir(parents=True)

    # Physics-only artefact, even carrying a dual-marked (api + physics) test, must NOT
    # satisfy --require-axis api.
    _artefact(
        physics_dir / "physics-cp312-manylinux-x86_64-pytest.json",
        [_record("test/physics/test_dual.py::test_x", call=0.2, markers=("api", "physics"))],
    )
    out_json = tmp_path / "cost-markers.json"
    rc_partial = checker.main([
        str(metrics_dir),
        "--require-axis",
        "api",
        "--require-axis",
        "physics",
        "--json",
        str(out_json),
    ])
    assert rc_partial == 2
    assert not out_json.exists()
    out = capsys.readouterr().out
    assert "missing measured metrics for required axis/axes: api" in out

    # An api artefact containing only skipped tests also fails closed.
    _artefact(
        api_dir / "api-cp312-manylinux-x86_64.json",
        [_record("test/core/test_api.py::test_skip", call=0.0, outcome="skipped")],
    )
    rc_skipped_api = checker.main([
        str(metrics_dir),
        "--require-axis",
        "api",
        "--require-axis",
        "physics",
        "--json",
        str(out_json),
    ])
    assert rc_skipped_api == 2
    assert not out_json.exists()
    capsys.readouterr()

    # Once both axes carry measured tests, the check runs and writes the JSON report.
    _artefact(
        api_dir / "api-cp312-manylinux-x86_64.json",
        [_record("test/core/test_api.py::test_ok", call=0.2, markers=("api",))],
    )
    rc_both = checker.main([
        str(metrics_dir),
        "--require-axis",
        "api",
        "--require-axis",
        "physics",
        "--json",
        str(out_json),
    ])
    assert rc_both == 0
    assert out_json.exists()
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert (payload["ok"], payload["flagged"], payload["measured"]) == (True, 0, 2)

    wf = yaml.safe_load(
        (REPO_ROOT / ".github/workflows/build-publish_to_pypi.yml").read_text(
            encoding="utf-8"
        )
    )
    check_step = next(
        step
        for step in wf["jobs"]["cost_markers"]["steps"]
        if step.get("name") == "Check cost markers against measured CPU seconds"
    )
    for axis in ("tooling", "unit", "native", "physics"):
        assert f"--require-axis {axis}" in check_step["run"]


@pytest.mark.smoke
@pytest.mark.core
def test_source_axis_supports_schema1_and_schema2_paths_and_rejects_ambiguity(
    tmp_path: Path,
) -> None:
    """`source_axis` recognises schema 1 and schema 2 paths and fails closed on ambiguity."""
    cases = [
        (tmp_path / "api-cp312-manylinux-x86_64.json", "api"),
        (tmp_path / "physics-cp312-manylinux-x86_64-pytest.json", "physics"),
        (tmp_path / "physics-core-cp312-manylinux-x86_64-pytest.json", "physics"),
        (tmp_path / "tooling-cp312-linux-x86_64.json", "tooling"),
        (tmp_path / "unit-cp312-linux-x86_64-shard-1-of-2.json", "unit"),
        (tmp_path / "native-cp312-manylinux-x86_64.json", "native"),
        (tmp_path / "ci-metrics-tooling-cp312-linux-x86_64" / "pytest.json", "tooling"),
        (tmp_path / "ci-metrics-unit-cp312-linux-x86_64" / "pytest.json", "unit"),
        (
            tmp_path / "ci-metrics-native-cp312-manylinux-x86_64" / "pytest.json",
            "native",
        ),
        (tmp_path / "ci-metrics" / "tooling" / "pytest.json", "tooling"),
        (tmp_path / "ci-metrics" / "physics-core" / "pytest.json", "physics"),
        # Unrelated directory names and ambiguous multi-axis names must not match.
        (tmp_path / "unit-tests" / "clean.json", None),
        (tmp_path / "api-physics.json", None),
        (tmp_path / "unit-native-cp312.json", None),
        (tmp_path / "api" / "api-physics.json", None),
        (tmp_path / "ci-metrics-unit-native-cp312" / "pytest.json", None),
        # Labelled filenames inside ambiguous or contradictory directories must fail closed.
        (tmp_path / "ci-metrics-unit-native-cp312" / "unit-cp312.json", None),
        (tmp_path / "unit-native" / "unit-cp312.json", None),
        (tmp_path / "ci-metrics-native-cp312" / "unit-cp312.json", None),
        (tmp_path / "native" / "unit-cp312.json", None),
        (tmp_path / "ci-metrics-native-cp312" / "unit" / "pytest.json", None),
        (tmp_path / "native" / "ci-metrics-unit-cp312" / "pytest.json", None),
    ]
    for path, expected in cases:
        assert checker.source_axis(path) == expected, path


@pytest.mark.smoke
@pytest.mark.core
def test_require_axis_supports_schema2_multi_axis_lanes_and_fails_closed(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Schema 2 axes (`tooling|unit|native|physics`) enforce completeness and detect drift."""
    metrics_dir = tmp_path / "ci-metrics"
    for axis in ("tooling", "unit", "native", "physics"):
        (metrics_dir / axis).mkdir(parents=True)

    _artefact(
        metrics_dir / "tooling" / "tooling-cp312-linux-x86_64.json",
        [_record("test/core/test_lint.py::test_ok", call=0.1, markers=("tooling",))],
    )
    # A unit artefact containing a test carrying both `unit` and `native` markers must
    # NOT satisfy --require-axis native.
    _artefact(
        metrics_dir / "unit" / "unit-cp312-linux-x86_64.json",
        [
            _record(
                "test/data_model/test_cfg.py::test_dual",
                call=0.2,
                markers=("unit", "native"),
            )
        ],
    )
    _artefact(
        metrics_dir / "physics" / "physics-core-cp312-manylinux-x86_64-pytest.json",
        [
            _record(
                "test/physics/test_exact.py::test_ok", call=2.0, markers=("physics",)
            )
        ],
    )

    out_json = tmp_path / "schema2-cost-markers.json"
    req_args = [
        "--require-axis",
        "tooling",
        "--require-axis",
        "unit",
        "--require-axis",
        "native",
        "--require-axis",
        "physics",
        "--json",
        str(out_json),
    ]

    # Missing `native` fails closed without writing JSON.
    assert checker.main([str(metrics_dir), *req_args]) == 2
    assert not out_json.exists()
    out = capsys.readouterr().out
    assert "missing measured metrics for required axis/axes: native" in out

    # Labelled filename inside an ambiguous directory, or generic filename under
    # conflicting enclosing axis directories, must NOT satisfy --require-axis native.
    ambig_path = (
        metrics_dir
        / "ci-metrics-unit-native-cp312"
        / "native-cp312-manylinux-x86_64.json"
    )
    ambig_path.parent.mkdir(parents=True, exist_ok=True)
    _artefact(
        ambig_path,
        [_record("test/core/test_native.py::test_ambig", call=0.5, markers=("native",))],
    )
    conflict_parent_path = (
        metrics_dir / "ci-metrics-unit-cp312" / "native" / "pytest.json"
    )
    conflict_parent_path.parent.mkdir(parents=True, exist_ok=True)
    _artefact(
        conflict_parent_path,
        [
            _record(
                "test/core/test_native.py::test_conflict_parent",
                call=0.5,
                markers=("native",),
            )
        ],
    )
    assert checker.main([str(metrics_dir), *req_args]) == 2
    assert not out_json.exists()
    ambig_out = capsys.readouterr().out
    assert "missing measured metrics for required axis/axes: native" in ambig_out

    # Skipped-only `native` artefact also fails closed.
    _artefact(
        metrics_dir / "native" / "native-cp312-manylinux-x86_64.json",
        [_record("test/core/test_native.py::test_skip", call=0.0, outcome="skipped")],
    )
    assert checker.main([str(metrics_dir), *req_args]) == 2
    assert not out_json.exists()
    capsys.readouterr()

    # Once all four schema 2 axes carry measured tests, clean runs exit 0.
    _artefact(
        metrics_dir / "native" / "native-cp312-manylinux-x86_64.json",
        [_record("test/core/test_native.py::test_ok", call=0.5, markers=("native",))],
    )
    assert checker.main([str(metrics_dir), *req_args]) == 0
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert (payload["ok"], payload["flagged"], payload["measured"]) == (True, 0, 6)
    capsys.readouterr()

    # Unknown axis is rejected directly by missing_required_axes.
    with pytest.raises(ValueError, match="unknown axis 'bogus'"):
        checker.missing_required_axes([], ["bogus"])


# =============================================================================
# Warn-only dynamic CPU budget reporting (gh#2349 W1-8)
# =============================================================================


def test_base_function_id_strips_param_suffix_and_normalises_paths() -> None:
    """`base_function_id` preserves `file::[Class::]func` even when param IDs contain `::`."""
    assert (
        checker.base_function_id("test/a.py::TestX::test_y[1-a::b]")
        == "test/a.py::TestX::test_y"
    )
    assert checker.base_function_id("./test\\a.py::test_plain") == "test/a.py::test_plain"
    assert checker.base_function_id("test/a.py::test_p[case-1]") == "test/a.py::test_p"


@pytest.mark.smoke
@pytest.mark.core
def test_parametrize_explosion_warns_without_changing_exit_code(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Non-slow parametrised functions exceeding node_count * median > 20s warn only."""
    # 10 nodes x 2.5s median = 25.0s > 20.0s, yet each node is well under the 10s medium bar.
    exploding = [
        _record(f"test/unit/test_many.py::TestGrid::test_sweep[{i}]", call=2.5)
        for i in range(10)
    ]
    # 9 fast nodes + 1 outlier spike: median=0.5s, 10 * 0.5 = 5.0 <= 20.0 (no explosion).
    outlier_only = [
        _record(f"test/unit/test_many.py::test_outlier[{i}]", call=0.5)
        for i in range(9)
    ] + [_record("test/unit/test_many.py::test_outlier[9]", call=15.0, markers=("medium",))]
    # Slow-marked parametrised function is excluded from explosion warnings.
    slow_param = [
        _record(
            f"test/unit/test_many.py::test_slow_sweep[{i}]",
            call=35.0,
            markers=("slow",),
        )
        for i in range(5)
    ]
    # Single-node medium test (25s) is not a parametrisation explosion.
    single_medium = [
        _record("test/unit/test_many.py::test_single_medium", call=25.0, markers=("medium",))
    ]
    # Exact boundary: 4 nodes x 5.0s = 20.0s (not strictly greater than 20.0).
    at_boundary = [
        _record(f"test/unit/test_many.py::test_boundary[{i}]", call=5.0)
        for i in range(4)
    ]

    unit_dir = tmp_path / "unit"
    unit_dir.mkdir()
    artefact = _artefact(
        unit_dir / "unit-cp312-linux-x86_64.json",
        exploding + outlier_only + slow_param + single_medium + at_boundary,
    )
    out_json = tmp_path / "out.json"
    summary_md = tmp_path / "summary.md"

    # Exit code MUST stay 0: explosion warnings are strictly warn-only.
    rc = checker.main([
        str(artefact),
        "--json",
        str(out_json),
        "--markdown",
        str(summary_md),
    ])
    assert rc == 0
    out = capsys.readouterr().out
    assert "[OK]" in out
    assert "[WARN] 1 parametrised non-slow function(s) exceed 20 CPU-s" in out
    assert "test/unit/test_many.py::TestGrid::test_sweep" in out
    assert "test_outlier" not in out
    assert "test_slow_sweep" not in out
    assert "test_single_medium" not in out
    assert "test_boundary" not in out

    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is True
    assert payload["flagged"] == 0
    assert payload["parametrize_explosion_threshold_seconds"] == pytest.approx(20.0)
    assert payload["parametrize_explosion_warnings"] == [
        {
            "function_id": "test/unit/test_many.py::TestGrid::test_sweep",
            "node_count": 10,
            "median_node_cpu": 2.5,
            "estimated_cpu_seconds": 25.0,
            "total_call_cpu": 25.0,
            "threshold_seconds": 20.0,
            "axis": "unit",
        }
    ]
    md = summary_md.read_text(encoding="utf-8")
    assert "### Parametrisation explosion warnings (warn-only)" in md
    assert "`test/unit/test_many.py::TestGrid::test_sweep`" in md


@pytest.mark.smoke
@pytest.mark.core
def test_per_axis_cpu_summary_supports_multi_axis_and_legacy_axis_labels(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Per-axis CPU summary orders 4-axis labels and recognises legacy `api`/`physics` labels."""
    s2_dir = tmp_path / "schema2"
    for axis, cpu in (("tooling", 1.2), ("unit", 3.4), ("native", 5.6), ("physics", 7.8)):
        d = s2_dir / axis
        d.mkdir(parents=True)
        _artefact(
            d / f"{axis}-cp312-linux-x86_64.json",
            [_record(f"test/core/test_{axis}.py::test_ok", call=cpu, markers=(axis,))],
        )
    s2_json = tmp_path / "s2.json"
    assert checker.main([str(s2_dir), "--json", str(s2_json)]) == 0
    s2_out = capsys.readouterr().out
    assert "Axis call CPU summary: tooling=1.2 CPU-s (1), unit=3.4 CPU-s (1), native=5.6 CPU-s (1), physics=7.8 CPU-s (1)." in s2_out
    s2_payload = json.loads(s2_json.read_text(encoding="utf-8"))
    assert [row["axis"] for row in s2_payload["axis_cpu_summary"]] == [
        "tooling",
        "unit",
        "native",
        "physics",
    ]

    # Legacy `api`/`physics` axis labels inside metrics schema_version 2 artefacts.
    legacy_dir = tmp_path / "legacy_axes"
    for axis, cpu in (("api", 4.0), ("physics", 6.0)):
        d = legacy_dir / axis
        d.mkdir(parents=True)
        _artefact(
            d / f"{axis}-cp312-manylinux-x86_64.json",
            [_record(f"test/core/test_{axis}.py::test_ok", call=cpu, markers=(axis,))],
        )
    legacy_json = tmp_path / "legacy.json"
    assert checker.main([str(legacy_dir), "--json", str(legacy_json)]) == 0
    capsys.readouterr()
    legacy_payload = json.loads(legacy_json.read_text(encoding="utf-8"))
    assert [row["axis"] for row in legacy_payload["axis_cpu_summary"]] == [
        "physics",
        "api",
    ]


@pytest.mark.smoke
@pytest.mark.core
def test_baseline_metrics_and_budget_warn_report_delta_without_failing(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """`--baseline-metrics` + `--budget-warn` reports PR delta strictly warn-only."""
    base_dir = tmp_path / "base" / "unit"
    curr_dir = tmp_path / "curr" / "unit"
    base_dir.mkdir(parents=True)
    curr_dir.mkdir(parents=True)

    _artefact(
        base_dir / "unit-cp312-linux-x86_64.json",
        [
            _record("test/core/test_existing.py::test_a", call=2.0),
            _record("test/core/test_other.py::test_b", call=3.0),
        ],
    )
    _artefact(
        curr_dir / "unit-cp312-linux-x86_64.json",
        [
            _record("test/core/test_existing.py::test_a", call=2.5),
            _record("test/core/test_existing.py::test_new_heavy", call=8.0),
            _record("test/core/test_other.py::test_b", call=3.0),
        ],
    )
    out_json = tmp_path / "delta.json"
    summary_md = tmp_path / "delta.md"

    # Delta is +8.5 CPU-s across the restricted file, exceeding budget 5.0 -> WARN, rc=0!
    rc = checker.main([
        str(tmp_path / "curr"),
        "--baseline-metrics",
        str(tmp_path / "base"),
        "--test-file",
        "test/core/test_existing.py",
        "--budget-warn",
        "5.0",
        "--json",
        str(out_json),
        "--markdown",
        str(summary_md),
    ])
    assert rc == 0
    out = capsys.readouterr().out
    assert "[WARN] PR CPU budget (warn-only)" in out
    assert "exceeds advisory budget 5 CPU-s" in out

    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is True
    delta = payload["budget_delta"]
    assert delta["current_cpu_seconds"] == pytest.approx(10.5)
    assert delta["baseline_cpu_seconds"] == pytest.approx(2.0)
    assert delta["delta_cpu_seconds"] == pytest.approx(8.5)
    assert delta["added_test_count"] == 1
    assert delta["added_tests_cpu_seconds"] == pytest.approx(8.0)
    assert delta["budget_seconds"] == pytest.approx(5.0)
    assert delta["exceeded"] is True

    md = summary_md.read_text(encoding="utf-8")
    assert "### PR CPU budget delta (warn-only)" in md

    # Bare `--budget-warn` uses DEFAULT_PR_ADDED_CPU_BUDGET_SECONDS (30.0) -> within budget.
    rc_default = checker.main([
        str(tmp_path / "curr"),
        "--baseline-metrics",
        str(tmp_path / "base"),
        "--budget-warn",
        "--json",
        str(out_json),
    ])
    assert rc_default == 0
    out_default = capsys.readouterr().out
    assert "[INFO] PR CPU budget (warn-only)" in out_default
    assert "within advisory budget 30 CPU-s" in out_default
    payload_default = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload_default["budget_delta"]["exceeded"] is False

    # Invalid advisory thresholds and unreadable/unsupported --baseline-metrics
    # are strictly warn-only: exit code stays 0 and normal JSON output is still written.
    bad_opt_json = tmp_path / "bad_opt.json"
    assert (
        checker.main([
            str(tmp_path / "curr"),
            "--parametrize-budget-seconds",
            "0",
            "--budget-warn",
            "-1",
            "--baseline-metrics",
            str(tmp_path / "does-not-exist.json"),
            "--json",
            str(bad_opt_json),
        ])
        == 0
    )
    assert "[WARN] advisory budget diagnostic unavailable (warn-only):" in capsys.readouterr().out
    assert len(json.loads(bad_opt_json.read_text(encoding="utf-8"))["advisory_errors"]) == 3

    # Unsupported schema or malformed records in --baseline-metrics also stay warn-only.
    bad_schema_base = tmp_path / "bad_schema_base.json"
    bad_schema_base.write_text(
        json.dumps({"schema_version": 99, "tests": []}), encoding="utf-8"
    )
    assert (
        checker.main([
            str(tmp_path / "curr"),
            "--baseline-metrics",
            str(bad_schema_base),
            "--json",
            str(bad_opt_json),
        ])
        == 0
    )
    assert "advisory budget diagnostic unavailable" in capsys.readouterr().out
    assert json.loads(bad_opt_json.read_text(encoding="utf-8"))["advisory_errors"]


def test_non_ascii_explosion_warning_stays_ascii_safe(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Explosion warnings sanitise non-ASCII node IDs so ASCII stdout/Markdown never fails."""
    artefact = _artefact(
        tmp_path / "unit.json",
        [
            _record("test/test_caf\u00e9.py::test_x[0]", call=11.0),
            _record("test/test_caf\u00e9.py::test_x[1]", call=11.0),
        ],
    )
    md_path = tmp_path / "summary.md"
    out_json = tmp_path / "out.json"
    assert (
        checker.main([
            str(artefact),
            "--markdown",
            str(md_path),
            "--json",
            str(out_json),
        ])
        == 0
    )
    out = capsys.readouterr().out
    assert out.isascii()
    assert "[WARN] 1 parametrised non-slow function(s)" in out
    md = md_path.read_text(encoding="utf-8")
    assert md.isascii()
    assert out_json.is_file()


def test_per_axis_cpu_summary_preserves_dual_lane_measurements(
    tmp_path: Path,
) -> None:
    """Dual-lane node measured in both `api` and `physics` keeps both axis totals."""
    tests = [
        checker.TestCost(
            "test/core/test_dual.py::test_x", ("api", "physics"), 4.0, 5.0, "api.json", axis="api"
        ),
        checker.TestCost(
            "test/core/test_dual.py::test_x", ("api", "physics"), 6.0, 8.0, "physics.json", axis="physics"
        ),
    ]
    report = checker.check_costs(tests)
    # Global deduplication retains 1 test (max=6.0) for marker classification.
    assert len(report.tests) == 1
    assert report.tests[0].cpu_seconds == pytest.approx(6.0)
    # Per-axis summary retains both axes without losing the smaller api reading.
    by_axis = {row.axis: row for row in report.axis_summary}
    assert set(by_axis) == {"api", "physics"}
    assert by_axis["api"].total_cpu_seconds == pytest.approx(4.0)
    assert by_axis["physics"].total_cpu_seconds == pytest.approx(6.0)


def test_advisory_boundary_isolates_malformed_baseline_nonfinite_and_invalid_cli_args(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Null/overflow/infinite baseline records and non-numeric CLI advisory flags stay warn-only."""
    curr = _artefact(
        tmp_path / "unit-cp312-linux-x86_64.json",
        [_record("test/core/test_ok.py::test_fast", call=1.5)],
    )
    out_json = tmp_path / "out.json"

    # 1) Non-numeric --parametrize-budget-seconds and --budget-warn must NOT trigger argparse exit 2.
    assert (
        checker.main([
            str(curr),
            "--parametrize-budget-seconds",
            "invalid",
            "--budget-warn",
            "not-a-number",
            "--json",
            str(out_json),
        ])
        == 0
    )
    out = capsys.readouterr().out
    assert "[OK]" in out
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["ok"] is True
    assert len(payload["advisory_errors"]) == 2

    # 2) Python entry point check_costs(..., explosion_seconds=0) degrades to advisory_errors.
    report = checker.check_costs(
        [checker.TestCost("test/core/test_ok.py::test_fast", (), 1.5, 1.5, "unit.json")],
        explosion_seconds=0,
    )
    assert report.ok is True
    assert report.advisory_errors

    # 3) Baseline containing [null], huge int (OverflowError), and 1e309 (Infinity)
    #    must never crash or emit non-standard JSON Infinity, exercising each exact failure path.
    for name, raw_text, expected_diag in (
        ("null_rec.json", '{"schema_version": 2, "tests": [null]}', "baseline metrics:"),
        (
            "huge_int.json",
            '{"schema_version": 2, "tests": [{"node_id": "test/a.py::test_x", '
            '"outcome": "passed", "cpu_seconds": {"call": '
            + ("9" * 400)
            + '}, "wall_seconds": {"call": 1.0}}]}',
            "int too large",
        ),
        (
            "inf_rec.json",
            '{"schema_version": 2, "tests": [{"node_id": "test/a.py::test_x", '
            '"outcome": "passed", "cpu_seconds": {"call": 1e309}, '
            '"wall_seconds": {"call": 1.0}}]}',
            "non-finite call CPU/wall",
        ),
    ):
        bad_base = tmp_path / name
        bad_base.write_text(raw_text, encoding="utf-8")
        out_path = tmp_path / f"out_{name}"
        assert (
            checker.main([
                str(curr),
                "--baseline-metrics",
                str(bad_base),
                "--budget-warn",
                "10",
                "--json",
                str(out_path),
            ])
            == 0
        )
        capsys.readouterr()
        raw_out = out_path.read_text(encoding="utf-8")
        assert "Infinity" not in raw_out
        assert "NaN" not in raw_out
        parsed = json.loads(raw_out)
        assert parsed["ok"] is True
        assert parsed["budget_delta"] is None
        assert any(expected_diag in err for err in parsed["advisory_errors"]), (
            name,
            parsed["advisory_errors"],
        )


def test_incomplete_optional_baseline_suppresses_delta_with_advisory_error(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Valid + missing/corrupt optional baseline suppresses delta; valid JSON sidecars stay allowed."""
    curr = _artefact(
        tmp_path / "unit-cp312-linux-x86_64.json",
        [_record("test/core/test_ok.py::test_fast", call=1.5)],
    )
    valid_base = _artefact(
        tmp_path / "valid_base.json",
        [_record("test/core/test_ok.py::test_fast", call=1.0)],
    )
    corrupt_base = tmp_path / "corrupt_base.json"
    corrupt_base.write_text("{not valid json", encoding="utf-8")
    for second_path, diag_frag in (
        (tmp_path / "missing_base.json", "path not found"),
        (corrupt_base, "unreadable or malformed"),
    ):
        out_partial = tmp_path / f"out_{second_path.stem}.json"
        assert (
            checker.main([
                str(curr),
                "--baseline-metrics",
                str(valid_base),
                "--baseline-metrics",
                str(second_path),
                "--budget-warn",
                "10",
                "--json",
                str(out_partial),
            ])
            == 0
        )
        capsys.readouterr()
        parsed_partial = json.loads(out_partial.read_text(encoding="utf-8"))
        assert parsed_partial["ok"] is True
        assert parsed_partial["budget_delta"] is None
        assert any(diag_frag in err for err in parsed_partial["advisory_errors"])

    mixed_dir = tmp_path / "mixed_base_dir"
    mixed_dir.mkdir()
    _artefact(
        mixed_dir / "unit-cp312-linux-x86_64.json",
        [_record("test/core/test_ok.py::test_fast", call=1.0)],
    )
    (mixed_dir / "workflow.json").write_text(
        json.dumps({"workflow": "summary"}), encoding="utf-8"
    )
    out_mixed = tmp_path / "out_mixed.json"
    assert (
        checker.main([
            str(curr),
            "--baseline-metrics",
            str(mixed_dir),
            "--budget-warn",
            "10",
            "--json",
            str(out_mixed),
        ])
        == 0
    )
    capsys.readouterr()
    parsed_mixed = json.loads(out_mixed.read_text(encoding="utf-8"))
    assert parsed_mixed["budget_delta"] is not None
    assert parsed_mixed["advisory_errors"] == []


def test_budget_delta_tracks_added_lane_execution_and_call_phase_under_setup_phase(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Budget delta counts new axis executions and call-CPU diagnostics ignore --phase setup."""
    # 1) Baseline runs test_x in api (6s); current keeps api (6s) and adds physics (4s).
    base_tests = [
        checker.TestCost(
            "test/core/test_dual.py::test_x", ("api",), 6.0, 6.0, "api.json", axis="api"
        )
    ]
    curr_tests = [
        checker.TestCost(
            "test/core/test_dual.py::test_x", ("api",), 6.0, 6.0, "api.json", axis="api"
        ),
        checker.TestCost(
            "test/core/test_dual.py::test_x", ("physics",), 4.0, 4.0, "physics.json", axis="physics"
        ),
    ]
    report = checker.check_costs(curr_tests, baseline_tests=base_tests, budget_warn_seconds=3.0)
    assert report.budget_delta is not None
    assert report.budget_delta.baseline_cpu_seconds == pytest.approx(6.0)
    assert report.budget_delta.current_cpu_seconds == pytest.approx(10.0)
    assert report.budget_delta.delta_cpu_seconds == pytest.approx(4.0)
    assert report.budget_delta.added_test_count == 1
    assert report.budget_delta.added_tests_cpu_seconds == pytest.approx(4.0)
    assert report.budget_delta.exceeded is True

    # 2) Under --phase setup, high setup CPU (11s x 2) with tiny call CPU (0.1s x 2)
    #    must NOT trigger a call-CPU parametrisation explosion warning.
    setup_heavy = _artefact(
        tmp_path / "unit-setup.json",
        [
            {
                "node_id": "test/core/test_s.py::test_p[0]",
                "outcome": "passed",
                "markers": ["unit"],
                "cpu_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
                "wall_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
            },
            {
                "node_id": "test/core/test_s.py::test_p[1]",
                "outcome": "passed",
                "markers": ["unit"],
                "cpu_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
                "wall_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
            },
        ],
    )
    out_json = tmp_path / "setup_phase.json"
    # Setup is 11s x 2 (= 22s > 20s if setup leaked into call diagnostics),
    # while call CPU is 0.1s x 2 = 0.2s (no explosion warning, 0.2s axis summary).
    rc = checker.main([str(setup_heavy), "--phase", "setup", "--json", str(out_json)])
    assert rc == 0
    capsys.readouterr()
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["parametrize_explosion_warnings"] == []
    assert payload["axis_cpu_summary"][0]["total_cpu_seconds"] == pytest.approx(0.2)


def test_setup_phase_isolates_null_call_and_deduplicates_explosions_by_call_cpu(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """Under --phase setup, null call values degrade cleanly and explosions deduplicate by call CPU."""
    # 1) Valid numeric setup with "call": null must NOT crash load_tests or alter exit code.
    null_call = _artefact(
        tmp_path / "unit-null-call.json",
        [
            {
                "node_id": "test/core/test_s.py::test_setup_only",
                "outcome": "passed",
                "markers": ["unit"],
                "cpu_seconds": {"setup": 1.0, "call": None, "teardown": 0.0, "total": 1.0},
                "wall_seconds": {"setup": 1.0, "call": None, "teardown": 0.0, "total": 1.0},
            }
        ],
    )
    out_null = tmp_path / "out_null.json"
    assert checker.main([str(null_call), "--phase", "setup", "--json", str(out_null)]) == 0
    capsys.readouterr()
    raw_null = out_null.read_text(encoding="utf-8")
    assert "NaN" not in raw_null
    parsed_null = json.loads(raw_null)
    assert parsed_null["ok"] is True
    assert parsed_null["measured"] == 1
    assert parsed_null["advisory_errors"]

    # 2) Two measurements per node: (setup=11, call=0.1) and (setup=0.1, call=11).
    #    Under --phase setup, explosion check must deduplicate by call CPU (2 x 11 = 22 > 20).
    dual_phase_a = _artefact(
        tmp_path / "unit-a.json",
        [
            {
                "node_id": f"test/core/test_m.py::test_p[{i}]",
                "outcome": "passed",
                "markers": ["unit"],
                "cpu_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
                "wall_seconds": {"setup": 11.0, "call": 0.1, "teardown": 0.0, "total": 11.1},
            }
            for i in range(2)
        ],
    )
    dual_phase_b = _artefact(
        tmp_path / "unit-b.json",
        [
            {
                "node_id": f"test/core/test_m.py::test_p[{i}]",
                "outcome": "passed",
                "markers": ["unit"],
                "cpu_seconds": {"setup": 0.1, "call": 11.0, "teardown": 0.0, "total": 11.1},
                "wall_seconds": {"setup": 0.1, "call": 11.0, "teardown": 0.0, "total": 11.1},
            }
            for i in range(2)
        ],
    )
    out_dual = tmp_path / "out_dual.json"
    assert (
        checker.main([
            str(dual_phase_a),
            str(dual_phase_b),
            "--phase",
            "setup",
            "--json",
            str(out_dual),
        ])
        == 0
    )
    capsys.readouterr()
    parsed_dual = json.loads(out_dual.read_text(encoding="utf-8"))
    assert len(parsed_dual["parametrize_explosion_warnings"]) == 1
    assert parsed_dual["parametrize_explosion_warnings"][0]["total_call_cpu"] == pytest.approx(22.0)
    assert parsed_dual["axis_cpu_summary"][0]["total_cpu_seconds"] == pytest.approx(22.0)
