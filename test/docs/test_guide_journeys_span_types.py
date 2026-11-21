"""Language bindings select statements without silently treating Python as shell."""

from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from scripts.suews.guide_journeys.extractor import Document  # noqa: E402

pytestmark = pytest.mark.api


def test_python_and_shell_types_come_from_source():
    """Compound Python statements retain their full original lines."""
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    document = Document(text, "Small reader journey")
    span = document.span({
        "anchor": "fixture/python",
        "statement": 2,
        "span_type": "python",
    })
    assert span.span_type == "python"
    assert span.text == "for value in (1, 2, 3):\n    total += value"
    assert text.splitlines()[span.first - 1].strip().startswith("for value")
    install = document.span({
        "anchor": "fixture/install",
        "command": 0,
        "span_type": "shell",
    })
    assert install.span_type == "shell"
    assert install.text == "pip install supy"


@pytest.mark.parametrize(
    "binding",
    [
        {"anchor": "fixture/python", "command": 0, "span_type": "shell"},
        {"anchor": "fixture/python", "statement": 0, "span_type": "shell"},
        {"anchor": "fixture/install", "statement": 0, "span_type": "python"},
    ],
)
def test_language_mismatch_is_rejected(binding):
    """Manifest text cannot override the page's interpreter choice."""
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    with pytest.raises(ValueError, match=r"language|type|index"):
        Document(text, "Small reader journey").span(binding)
