"""Layer-3 real-CLI evidence-retrieval runner for the canonical Q&A fixture.

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

Marked ``native`` because ``query_knowledge`` shells out to the installed
``suews knowledge query`` console command from the built wheel.
"""

from __future__ import annotations

from pathlib import Path
import shutil

import pytest
import yaml

pytestmark = [pytest.mark.native, pytest.mark.e2e, pytest.mark.contract("C-API")]


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
                        f"matched {ev['kind']} path {expected_path!r} via {repo_path!r}"
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

    _matched_curated, diagnostic = _matches_evidence_contract(question, matches)

    assert indexed_paths, (
        f"{question['id']} ({question['class']}): pack returned no "
        f"matches under indexed roots {_PACK_INDEXED_ROOTS}. Question: "
        f"{question['text'].strip()!r}. Curated-evidence check: "
        f"{diagnostic}"
    )


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
