"""Source selection must neither drop decorators nor hide neighbouring commands."""

from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.extractor import (  # noqa: E402
    Document,
    digest,
    python_spans,
)
from scripts.suews.guide_journeys.manifest import (  # noqa: E402
    step_coverage,
    unbound_instructions,
)

pytestmark = pytest.mark.api


def test_decorated_statement_keeps_original_source():
    """A decorator changes behaviour and is part of the executed statement."""
    source = "@decorate(\n    2\n)\ndef answer():\n    return 3"
    span = python_spans(source, 20)[0]
    assert span.text == source
    assert (span.first, span.last) == (20, 24)


def test_install_exclusion_does_not_cover_neighbouring_command():
    """Only the setup span is excluded from ordinary documented-step coverage."""
    text = "Title\n=====\n\n.. journey: install\n\n.. code-block:: bash\n\n   pip install supy\n   python --version\n"
    document = Document(text, "Title")
    install = {
        "id": "install",
        "anchor": "install",
        "command": 0,
        "span_type": "shell",
        "sha256": digest("pip install supy"),
        "kind": "installation",
    }
    manifest = {"steps": [], "exclusions": [install]}
    assert unbound_instructions(document, manifest)
    assert step_coverage(document, manifest) == {
        "documented_steps": 1,
        "bound_steps": 0,
    }
