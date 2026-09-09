"""Contract tests for the physics-full tier-delta check (gh#1576).

The check keeps `.claude/rules/physics-change-evidence.md` honest about which
tests the `physics-full` CI tier runs that `standard` does not: exactly those
marked `physics` and `slow` without `core`. The comparison lives in
`scripts/lint/check_physics_tier_delta.py`. One test here runs it against a
live collection of this tree; the others drive it with synthetic node ids, so
no real test in the suite has to carry `physics` + `slow` without `core` to
prove the check trips.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
# scripts/ is not a package on sys.path under a bare `pytest` invocation
# (only `python -m pytest` puts the repository root there), so add it.
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.lint import check_physics_tier_delta as checker  # noqa: E402

pytestmark = pytest.mark.api

UNRECORDED_NODE = "test/physics/test_synthetic.py::test_slow_without_core"


def _rule_text(*recorded_lines: str) -> str:
    body = "\n".join(recorded_lines) if recorded_lines else checker.NONE_SENTINEL
    return (
        "# rule prose\n\n"
        f"{checker.RULE_BEGIN}\n```text\n{body}\n```\n{checker.RULE_END}\n\n"
        "more prose\n"
    )


# Marked smoke as well as core: a tests-only PR runs the smoke tier, and the
# test that proves the check trips must run in the CI of the PR that
# introduces or edits it.
@pytest.mark.smoke
@pytest.mark.core
def test_unrecorded_physics_slow_test_trips_the_check() -> None:
    """A physics + slow test without core, absent from the rule, fails and is named."""
    report = checker.check_tier_delta([UNRECORDED_NODE], recorded=[])

    assert not report.ok
    assert report.unrecorded == [UNRECORDED_NODE]
    assert report.stale == []
    message = report.message()
    assert UNRECORDED_NODE in message
    assert "mark each one `core`" in message
    assert checker.RULE_PATH.as_posix() in message
    assert checker.RULE_BEGIN in message


# Collects the whole suite in a subprocess: about 4 s against an installed
# wheel on the Linux runner, about 30 s against a local editable build (the
# subprocess re-imports the editable supy). Not `smoke`: it belongs to the
# `standard` and fuller tiers, where the slow physics tests it reasons about
# are themselves selected.
@pytest.mark.core
def test_physics_full_tier_adds_exactly_what_the_rule_records() -> None:
    """Live check: the collected `physics and slow and not core` set matches the rule.

    pytest itself evaluates the marker expression in the subprocess, so this
    sees markers however they were applied. A `physics` + `slow` test without
    `core` that the rule does not record fails this test with a message naming
    the node id and the two remedies (see the synthetic cases in this module).
    """
    collected = checker.collect_delta_nodes(REPO_ROOT)

    report = checker.check_against_rule(collected, REPO_ROOT)

    assert report.ok, report.message()


@pytest.mark.core
def test_empty_delta_with_nothing_recorded_passes() -> None:
    report = checker.check_tier_delta([], recorded=[])

    assert report.ok
    assert report.message().startswith("[OK]")


@pytest.mark.core
def test_recorded_pattern_covers_collected_test() -> None:
    """A recorded exact id or wildcard pattern makes the collected test acceptable."""
    collected = [
        UNRECORDED_NODE,
        "test/physics/test_synthetic.py::test_param[a]",
        "test/physics/test_synthetic.py::test_param[b]",
    ]
    recorded = [UNRECORDED_NODE, "test/physics/test_synthetic.py::test_param[*]"]

    report = checker.check_tier_delta(collected, recorded)

    assert report.ok, report.message()


@pytest.mark.core
def test_brackets_in_patterns_are_literal() -> None:
    """`[` and `]` are pytest parameter delimiters, not character classes."""
    assert checker.node_id_matches("t.py::test_p[a-b]", "t.py::test_p[a-b]")
    assert checker.node_id_matches("t.py::test_p[a-b]", "t.py::test_p[*]")
    assert checker.node_id_matches("t.py::test_p[a]", "t.py::test_p[?]")
    assert not checker.node_id_matches("t.py::test_p", "t.py::test_p[*]")
    assert not checker.node_id_matches("t.py::test_pa", "t.py::test_p[a]")
    assert not checker.node_id_matches("t.py::test_p[a]", "t.py::test_q[*]")


@pytest.mark.core
def test_stale_recorded_pattern_trips_the_check() -> None:
    """A recorded id that matches nothing fails, so the rule cannot overclaim."""
    report = checker.check_tier_delta([], recorded=[UNRECORDED_NODE])

    assert not report.ok
    assert report.stale == [UNRECORDED_NODE]
    assert report.unrecorded == []
    assert "match no collected test" in report.message()


@pytest.mark.core
def test_parse_ignores_fences_comments_blanks_and_none_sentinel() -> None:
    text = _rule_text(
        "# a comment",
        "",
        "test/physics/test_x.py::test_one",
        "   test/physics/test_y.py::test_two[*]   ",
        checker.NONE_SENTINEL,
    )

    assert checker.parse_recorded_nodes(text) == [
        "test/physics/test_x.py::test_one",
        "test/physics/test_y.py::test_two[*]",
    ]
    assert checker.parse_recorded_nodes(_rule_text()) == []


@pytest.mark.core
def test_missing_rule_block_is_an_error() -> None:
    """A rule without the marker block cannot silently disable the check."""
    with pytest.raises(checker.RuleFormatError, match="must carry the block"):
        checker.parse_recorded_nodes("# rule prose without the block\n")


@pytest.mark.core
def test_real_rule_records_an_empty_delta_today() -> None:
    """The committed rule parses and, as its prose states, records nothing."""
    rule_text = (REPO_ROOT / checker.RULE_PATH).read_text(encoding="utf-8")

    assert checker.parse_recorded_nodes(rule_text) == []
    assert checker.check_against_rule([], REPO_ROOT).ok
    assert not checker.check_against_rule([UNRECORDED_NODE], REPO_ROOT).ok
