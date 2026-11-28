"""Fixture wellformedness checks for the canonical Q&A suite.

Loads ``test/mcp/fixtures/canonical_questions.yml`` (Sue Grimmond's
domain-expert stress-test set) and checks that every positive and negative
case carries the schema required by the real-CLI retrieval runner in
``test/mcp/test_canonical_questions_cli.py``.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-API")]


REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURE_PATH = REPO_ROOT / "test" / "mcp" / "fixtures" / "canonical_questions.yml"


def _load_fixture() -> dict:
    """Return the parsed canonical_questions.yml fixture."""
    return yaml.safe_load(FIXTURE_PATH.read_text(encoding="utf-8"))


def _positive_questions() -> list[dict]:
    fixture = _load_fixture()
    return [q for q in fixture["questions"] if not q["id"].startswith("N")]


def _negative_questions() -> list[dict]:
    fixture = _load_fixture()
    return [q for q in fixture["questions"] if q["id"].startswith("N")]


# -----------------------------------------------------------------------
# Fixture wellformedness — runs without the CLI, catches schema drift early
# -----------------------------------------------------------------------


def test_fixture_loads_and_has_positive_and_negative_cases() -> None:
    """The fixture parses, has a contract block, and contains both the
    six positive (Sue) cases and the three negative (out-of-scope) cases."""
    fixture = _load_fixture()

    assert fixture.get("fixture_version") == 1, (
        f"Unexpected fixture_version: {fixture.get('fixture_version')!r}. "
        "Update this assertion if the fixture schema bumps."
    )
    assert fixture.get("contract"), "Fixture missing top-level `contract` block."

    positive = _positive_questions()
    negative = _negative_questions()

    assert {q["id"] for q in positive} == {"B1", "B2", "B3", "B4", "B5", "B6"}, (
        f"Positive case IDs drift: {sorted(q['id'] for q in positive)}"
    )
    assert {q["id"] for q in negative} == {"N1", "N2", "N3"}, (
        f"Negative case IDs drift: {sorted(q['id'] for q in negative)}"
    )


@pytest.mark.parametrize("question", _positive_questions(), ids=lambda q: q["id"])
def test_positive_question_is_wellformed(question: dict) -> None:
    """Every positive question carries the contract fields the runner needs."""
    assert question.get("text"), f"{question['id']}: missing 'text'."
    assert question.get("class"), f"{question['id']}: missing 'class'."
    assert isinstance(question.get("required_evidence"), list), (
        f"{question['id']}: 'required_evidence' must be a list."
    )
    assert question["required_evidence"], (
        f"{question['id']}: positive cases must list at least one required "
        "evidence item; otherwise the test cannot assert anything."
    )
    assert isinstance(question.get("expected_tool_calls"), list), (
        f"{question['id']}: 'expected_tool_calls' must be a list "
        "(empty allowed)."
    )

    for ev in question["required_evidence"]:
        assert ev.get("kind") in {"schema", "code", "docs", "example"}, (
            f"{question['id']}: evidence kind must be one of "
            f"schema/code/docs/example, got {ev.get('kind')!r}."
        )
        # schema kind names a field; others name a path or symbol.
        if ev["kind"] == "schema":
            assert ev.get("field"), (
                f"{question['id']}: schema evidence needs 'field': {ev}"
            )
        else:
            assert ev.get("path"), (
                f"{question['id']}: {ev['kind']} evidence needs 'path': {ev}"
            )


@pytest.mark.parametrize("question", _negative_questions(), ids=lambda q: q["id"])
def test_negative_question_is_wellformed(question: dict) -> None:
    """Every negative case declares an `expected_response_shape` and an
    empty `required_evidence` list (the contract is refusal, not evidence)."""
    assert question.get("text"), f"{question['id']}: missing 'text'."
    assert question.get("expected_response_shape"), (
        f"{question['id']}: negative cases must declare "
        "'expected_response_shape' (out_of_scope | "
        "out_of_scope_or_unknown | refusal_with_reason)."
    )
    assert question.get("required_evidence") == [], (
        f"{question['id']}: negative cases must have empty "
        "'required_evidence' — the contract is refusal, not retrieval."
    )
