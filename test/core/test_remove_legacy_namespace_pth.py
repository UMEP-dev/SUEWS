"""Tests for scripts/security/remove_legacy_namespace_pth.py."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest

pytestmark = pytest.mark.api


SCRIPT_PATH = Path(__file__).resolve().parents[2] / "scripts" / "security" / "remove_legacy_namespace_pth.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location("remove_legacy_namespace_pth", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
remove_legacy_namespace_pth = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = remove_legacy_namespace_pth
SCRIPT_SPEC.loader.exec_module(remove_legacy_namespace_pth)


def test_remove_legacy_namespace_pth_deletes_matching_files(tmp_path):
    """Only legacy namespace bootstrap files should be removed."""
    site_dir = tmp_path / "site-packages"
    site_dir.mkdir()
    doomed = site_dir / "sphinxcontrib_jsmath-1.0.1-py3.7-nspkg.pth"
    keep = site_dir / "ordinary.pth"
    doomed.write_text("import something\n", encoding="utf-8")
    keep.write_text("/tmp/example\n", encoding="utf-8")

    _, removed = remove_legacy_namespace_pth.remove_legacy_namespace_pth(extra_site_dirs=[str(site_dir)])

    assert removed == [doomed.resolve()]
    assert not doomed.exists()
    assert keep.exists()


def test_remove_legacy_namespace_pth_supports_dry_run(tmp_path):
    """Dry-run mode should report files without deleting them."""
    site_dir = tmp_path / "site-packages"
    site_dir.mkdir()
    doomed = site_dir / "pkg-1.0-py3.7-nspkg.pth"
    doomed.write_text("import something\n", encoding="utf-8")

    _, removed = remove_legacy_namespace_pth.remove_legacy_namespace_pth(
        extra_site_dirs=[str(site_dir)],
        dry_run=True,
    )

    assert removed == [doomed.resolve()]
    assert doomed.exists()
