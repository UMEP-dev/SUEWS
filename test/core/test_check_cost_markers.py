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

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

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
    assert "--require-axis api" in check_step["run"]
    assert "--require-axis physics" in check_step["run"]


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
