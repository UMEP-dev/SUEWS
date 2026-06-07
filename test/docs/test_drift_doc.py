"""The drift docs page must exist, be ASCII, and explain the taxonomy."""

from pathlib import Path

import pytest

DOC = Path(__file__).resolve().parents[2] / "docs/source/benchmark/drift.rst"

# gh#1300: every test file must carry a nature marker (api | physics).
pytestmark = pytest.mark.api


def test_drift_doc_exists():
    assert DOC.is_file()


def test_drift_doc_is_ascii():
    DOC.read_text(encoding="ascii")  # raises if any non-ASCII byte present


def test_drift_doc_mentions_taxonomy_terms():
    text = DOC.read_text(encoding="ascii")
    for term in ("benchmarked", "legacy-external-ref", "drift", "baseline"):
        assert term in text, term
