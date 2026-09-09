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

pytestmark = pytest.mark.api

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
        _cost("t/a.py::test_medium_unmarked", 12.0),
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


def test_medium_marker_inside_the_band_is_not_questioned() -> None:
    """A medium mark just under the threshold stays quiet; well under, it is flagged."""
    inside = _check([_cost("t/a.py::test_near", MEDIUM / BAND, "medium")])
    below = _check([_cost("t/a.py::test_far", MEDIUM / BAND - 0.1, "medium")])

    assert inside.ok
    assert [finding.kind for finding in below.findings] == ["medium-under-medium"]


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
        checker.TestCost("t/a.py::test_shared", ("api", "physics"), 14.0, 20.0, "physics.json"),
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
    assert "t/a.py::test_heavy_unmarked: 42.0 CPU-s (no cost marker -> slow" in out
    assert "| CPU-s | tests | cumulative |" in out
    written = summary.read_text(encoding="utf-8")
    assert "## Cost-marker drift" in written
    assert "| `t/a.py::test_heavy_unmarked` | 42.0 | - | slow | unmarked-over-medium |" in written
    assert "### Distribution" in written

    (tmp_path / "phases.json").write_text(json.dumps({"schema_version": 1}), encoding="utf-8")
    assert checker.main([str(tmp_path / "phases.json")]) == 2
    assert "[skip]" in capsys.readouterr().out
