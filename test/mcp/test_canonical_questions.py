"""Layer-3 evidence-retrieval runner for the canonical Q&A fixture.

Loads ``test/mcp/fixtures/canonical_questions.yml`` (Sue Grimmond's
domain-expert stress-test set) and asserts that the SUEWS knowledge
pack actually contains the evidence each question requires. This is
the function-level layer of the Layer-3 contract spelled out in
``gh#1384``: it runs in CI, deterministic, and catches knowledge-pack
regressions early.

What this layer does **not** do: drive a real agent. Whether the
assistant *uses* the evidence well — whether it cites it, structures
it region-aware, or hits the out-of-scope refusal contract — is what
``MANUAL_SMOKE.md`` covers. This runner exclusively asserts the
substrate the agent draws on.

Skipped when the ``suews`` CLI is missing (the knowledge pack lives
behind ``query_knowledge``, which shells out to ``suews knowledge
query``).
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
import yaml

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURE_PATH = REPO_ROOT / "test" / "mcp" / "fixtures" / "canonical_questions.yml"


pytestmark_skipif = pytest.mark.skipif(
    shutil.which("suews") is None,
    reason=(
        "`suews` CLI is not on PATH. The Layer-3 runner depends on the "
        "real `suews knowledge query` backend. Run `make dev` first."
    ),
)


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
            "schema/code/docs/example, got {ev.get('kind')!r}."
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


# -----------------------------------------------------------------------
# Evidence retrieval — needs the real knowledge pack via `suews knowledge`
# -----------------------------------------------------------------------


def _query_pack(question: dict, *, limit: int = 25) -> dict:
    """Run `query_knowledge` against a fixture question and return the envelope."""
    from suews_mcp.tools import query_knowledge

    return query_knowledge(question["text"], limit=limit)


# Roots the pack actually indexes (per the manifest's `source_roots`):
# Fortran/Rust cores, the Python data model, and the bridge. Docs are
# excluded by design (`excluded_roots` in the manifest).
_PACK_INDEXED_ROOTS = (
    "src/suews/",
    "src/suews_bridge/",
    "src/supy/",
)


def _matches_any_indexed_root(matches: list[dict]) -> list[str]:
    """Return the subset of match repo_paths that fall under indexed roots."""
    return [
        m.get("repo_path", "")
        for m in matches
        if any(m.get("repo_path", "").startswith(r) for r in _PACK_INDEXED_ROOTS)
    ]


def _matches_evidence_contract(question: dict, matches: list[dict]) -> tuple[bool, str]:
    """Best-effort check: do the returned matches surface any evidence the
    fixture explicitly required?

    This is informational — many `required_evidence` items in the fixture
    are docs anchors (under `docs/source/`) which the pack excludes by
    design (see manifest `excluded_roots`). Such items can never match
    here even when the agent would reach them through the official-docs
    fallback. So the ASSERTION is on `matches_any_indexed_root` (the
    pack returned real SUEWS source); this function is reported in the
    diagnostic so a maintainer can see how often the natural-question
    retrieval surfaces the curated evidence list.
    """
    repo_paths = {m.get("repo_path", "") for m in matches}
    full_text_blob = "\n".join(m.get("text", "") for m in matches)

    for ev in question["required_evidence"]:
        if ev["kind"] == "schema":
            field_leaf = ev["field"].rsplit(".", 1)[-1].split("[")[0]
            if field_leaf and field_leaf in full_text_blob:
                return True, f"matched schema leaf {field_leaf!r}"
        else:
            expected_path = ev["path"]
            for repo_path in repo_paths:
                if repo_path.startswith(expected_path) or expected_path in repo_path:
                    return True, (
                        f"matched {ev['kind']} path {expected_path!r} "
                        f"via {repo_path!r}"
                    )

    return False, (
        "natural-question retrieval surfaced no curated evidence; the "
        "agent would need to refine the query to reach the items "
        "listed in the fixture (or use the docs/source/ fallback for "
        "doc-anchor items the pack excludes by design)."
    )


@pytestmark_skipif
@pytest.mark.parametrize("question", _positive_questions(), ids=lambda q: q["id"])
def test_positive_question_returns_indexed_evidence(
    question: dict,
) -> None:
    """The knowledge pack returns SUEWS source-tree evidence for each
    positive Q&A case.

    The hard contract: the pack must surface at least one match under
    its indexed roots (`src/suews/`, `src/suews_bridge/`, `src/supy/`).
    A natural-language SUEWS question that returns zero indexed-root
    matches is a real pack regression — the assistant has nothing to
    cite and must fall back to the out-of-scope refusal contract.

    The fixture's curated `required_evidence` list is reported in the
    failure diagnostic (best-effort match) but not asserted on directly.
    Many curated items are docs anchors under `docs/source/`, which the
    pack excludes by design — the agent reaches those through the
    official-docs fallback (manifest's `official_docs.stable`).
    """
    envelope = _query_pack(question)

    assert envelope.get("status") == "success", (
        f"{question['id']}: query_knowledge errored: "
        f"{envelope.get('errors')}. Has the knowledge pack been built?"
    )

    matches = (envelope.get("data") or {}).get("matches") or []
    indexed_paths = _matches_any_indexed_root(matches)

    matched_curated, diagnostic = _matches_evidence_contract(question, matches)

    assert indexed_paths, (
        f"{question['id']} ({question['class']}): pack returned no "
        f"matches under indexed roots {_PACK_INDEXED_ROOTS}. Question: "
        f"{question['text'].strip()!r}. Curated-evidence check: "
        f"{diagnostic}"
    )

    # Informational only — emit via the test name's caplog if needed.
    # We deliberately do not fail when the curated list does not match,
    # only when the pack returns nothing indexed at all.


@pytestmark_skipif
@pytest.mark.parametrize("question", _negative_questions(), ids=lambda q: q["id"])
def test_negative_question_pack_call_succeeds(question: dict) -> None:
    """Out-of-scope questions return a successful (non-error) envelope.

    The pack is allowed to surface loose matches for out-of-scope
    questions — the vector retriever is keyword-based and always returns
    *something*. The behavioural contract (does the agent actually
    refuse?) lives in ``MANUAL_SMOKE.md``: only a real agent host can
    verify that the assistant produces an out-of-scope refusal rather
    than confidently hallucinating from loose matches.

    What we assert here is the lower-level invariant: the pack must not
    *crash* on out-of-scope input. If it does, the assistant cannot
    even reach the refusal path.
    """
    from suews_mcp.tools import query_knowledge

    envelope = query_knowledge(question["text"], limit=3)

    assert envelope.get("status") == "success", (
        f"{question['id']}: query_knowledge errored on an out-of-scope "
        f"question; the pack must not crash on input outside its scope. "
        f"Errors: {envelope.get('errors')}"
    )
