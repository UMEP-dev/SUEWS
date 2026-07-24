"""Tests for the bibliography convention audit."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]
AUDIT_PATH = (
    PROJECT_ROOT / ".claude" / "skills" / "audit-docs" / "scripts" / "audit.py"
)


def _load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_docs_for_test", AUDIT_PATH)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


AUDIT = _load_audit_module()


def _bib_entry(
    entry_type: str,
    *,
    omit: tuple[str, ...] = (),
    keywords: str = "storage-heat",
    include_doi: bool = False,
) -> str:
    fields = {
        "title": "Example manuscript",
        "author": "Example, Alice",
        "year": "2026",
        "keywords": keywords,
        "abstract": "An example abstract.",
    }
    if include_doi:
        fields["doi"] = "10.0000/example"

    rendered_fields = [
        f"  {name} = {{{value}}}," for name, value in fields.items() if name not in omit
    ]
    return "\n".join([f"@{entry_type}{{Example2026,", *rendered_fields, "}", ""])


def _audit_text(tmp_path: Path, text: str):
    bib_path = tmp_path / "references.bib"
    bib_path.write_text(text, encoding="utf-8")
    return AUDIT.audit_file(bib_path, {})


def test_parser_records_normalized_entry_type() -> None:
    entries = AUDIT.find_entries(_bib_entry("Unpublished"))

    assert len(entries) == 1
    assert entries[0]["entry_type"] == "unpublished"
    assert entries[0]["key"] == "Example2026"


def test_unpublished_entry_without_doi_passes(tmp_path: Path) -> None:
    count, violations, warnings = _audit_text(
        tmp_path, _bib_entry("unpublished")
    )

    assert count == 1
    assert violations == []
    assert warnings == []


@pytest.mark.parametrize("entry_type", ["article", "inproceedings"])
def test_other_entry_types_without_doi_fail(
    tmp_path: Path, entry_type: str
) -> None:
    _, violations, _ = _audit_text(tmp_path, _bib_entry(entry_type))

    assert any("missing or empty `doi`" in violation for violation in violations)


def test_other_entry_type_with_doi_passes(tmp_path: Path) -> None:
    _, violations, warnings = _audit_text(
        tmp_path, _bib_entry("article", include_doi=True)
    )

    assert violations == []
    assert warnings == []


@pytest.mark.parametrize("field", ["title", "author", "year"])
def test_unpublished_entry_still_requires_common_fields(
    tmp_path: Path, field: str
) -> None:
    _, violations, _ = _audit_text(
        tmp_path, _bib_entry("unpublished", omit=(field,))
    )

    assert any(f"missing or empty `{field}`" in violation for violation in violations)


def test_unpublished_entry_still_requires_controlled_keywords(
    tmp_path: Path,
) -> None:
    _, violations, _ = _audit_text(
        tmp_path, _bib_entry("unpublished", keywords="not-controlled")
    )

    assert any(
        "slug `not-controlled` not in controlled vocabulary" in violation
        for violation in violations
    )
