"""The drift site page must exist and reference the data + accessibility."""

from pathlib import Path

import pytest

PAGE = Path(__file__).resolve().parents[2] / "site/benchmark/drift/index.html"

# gh#1300: every test file must carry a nature marker (api | physics).
pytestmark = pytest.mark.api


def test_page_exists():
    assert PAGE.is_file()


def test_page_loads_drift_csv_and_has_toggle():
    html = PAGE.read_text(encoding="utf-8")
    assert "drift_table.csv" in html
    assert "consecutive" in html and "baseline" in html


def test_page_honours_reduced_motion():
    html = PAGE.read_text(encoding="utf-8")
    assert "prefers-reduced-motion" in html
