"""Source-binding contracts for the migration-guide pilot."""

from pathlib import Path
import sys

import pytest

pytest.importorskip(
    "docutils",
    reason="Wheel test lanes do not install the docs extra containing docutils",
)

# Script packages are not installed in the wheel; support the pytest console entry point.
_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.suews.guide_journeys.extractor import (  # noqa: E402 - source script imports follow path setup
    Document,
    digest,
    python_spans,
    shell_spans,
)
from scripts.suews.guide_journeys.manifest import (  # noqa: E402 - source script imports follow path setup
    check,
    load,
)

pytestmark = pytest.mark.api
ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "test/docs/journeys/transition_guide.yml"


def test_reviewed_bindings_and_lines():
    """Every published command has a reviewed span and original source lines."""
    manifest = load(MANIFEST)
    text = (ROOT / manifest["guide"]).read_text(encoding="utf-8")
    document = Document(text, manifest["title"])
    assert check(document, manifest, "test-revision") == []
    assert len(manifest["steps"]) == 6
    for step in manifest["steps"]:
        span = document.span(step)
        source = text.splitlines()[span.first - 1 : span.last]
        assert span.text.splitlines()[0] in source[0]
        assert step["reviewed_revision"]


@pytest.mark.parametrize(
    "before,after",
    [
        (
            "suews schema migrate site.yml --target-version 2026.6.dev12",
            "suews schema migrate other.yml --target-version 2026.6.dev12",
        ),
        ("Use a fresh output filename", "Reuse the previous output filename"),
        (
            ".. journey: transition_guide/directory",
            "Never migrate an untagged file.\n\n.. journey: transition_guide/directory",
        ),
        ("   suews convert -i", "   suews schema info\n   suews convert -i"),
    ],
)
def test_staleness_mutations(before, after):
    """Commands, conditions, new prose and unbound commands all require review."""
    manifest = load(MANIFEST)
    text = (ROOT / manifest["guide"]).read_text(encoding="utf-8")
    assert before in text
    mutated = Document(text.replace(before, after, 1), manifest["title"])
    errors = check(mutated, manifest, "mutated")
    assert any("Section envelope changed" in error for error in errors)
    assert all("reviewed" in error and "current mutated" in error for error in errors)
    if after.startswith("Never migrate"):
        assert len(errors) == 1, (
            "Only the section envelope should detect new unbound prose"
        )
    if "schema info" in after:
        assert any("Unbound" in error for error in errors)


def test_duplicate_and_dangling_anchors():
    """Ambiguous or detached anchors cannot silently bind to a different node."""
    for source, message in [
        (".. journey: x\n\ntext\n\n.. journey: x\n\nother", "Duplicate"),
        (".. journey: x", "Dangling"),
    ]:
        with pytest.raises(ValueError, match=message):
            Document("Title\n=====\n\n" + source, "Title")


def test_shell_and_python_span_splitting():
    """Continuations and compound statements remain one reviewable step each."""
    spans = shell_spans("suews validate \\\n  site.yml\nsuews schema info", 12)
    assert [(s.first, s.last) for s in spans] == [(12, 13), (14, 14)]
    assert "\\\n" in spans[0].text
    spans = python_spans("x = 1\nif x:\n    y = ' a ' \nz = y", 30)
    assert [(s.first, s.last) for s in spans] == [(30, 30), (31, 32), (33, 33)]
    assert "    y" in spans[1].text
    assert digest("  x\r\n  y") == digest("x\ny")
    assert digest("x = ' a '") != digest("x = 'a'")
    assert digest("if x:\n    y = 1") != digest("if x:\n  y = 1")


def test_anchor_inventory_requires_both_directions():
    """Missing and newly introduced anchor identities cannot silently pass."""
    manifest = load(MANIFEST)
    text = (ROOT / manifest["guide"]).read_text(encoding="utf-8")
    renamed = text.replace(
        "journey: transition_guide/recovery", "journey: transition_guide/new-condition"
    )
    errors = check(Document(renamed, manifest["title"]), manifest, "renamed")
    assert any("Anchor inventory differs" in error for error in errors)
    assert any("Binding recovery" in error for error in errors)


def test_hash_preserves_blank_line_string_content():
    """Whitespace inside a multi-line string is source content, not a blank margin."""
    assert digest('x = """a\n  \nb"""') != digest('x = """a\n\nb"""')
