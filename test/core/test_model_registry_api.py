"""The registry API must be reachable from the supy top level."""

import pytest

import supy as sp

# gh#1300: every test file must carry a nature marker (api | physics).
pytestmark = pytest.mark.api


def test_top_level_exports_present():
    for name in ("list_model_versions", "model_version_info", "schema_for"):
        assert name in sp.__all__, name
        assert callable(getattr(sp, name)), name


def test_schema_for_via_top_level():
    assert sp.schema_for("2026.4.3") == "2026.4"


def test_model_version_info_via_top_level():
    info = sp.model_version_info("2026.6.5")
    assert info.schema_version == "2026.5"
    assert info.reproducibility == "benchmarked"
