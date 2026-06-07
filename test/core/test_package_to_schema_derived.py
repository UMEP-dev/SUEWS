"""The package->schema map must be derived from the registry and unchanged."""

import pytest

from supy.util.converter.yaml_upgrade import (
    _PACKAGE_TO_SCHEMA,
    _PACKAGE_TO_SCHEMA_FALLBACK,
    _resolve_package_to_schema,
)

# gh#1300: every test file must carry a nature marker (api | physics).
pytestmark = pytest.mark.api

HISTORICAL = {
    "2025.10.15": "2025.12",
    "2025.11.20": "2025.12",
    "2026.1.28": "2026.1",
    "2026.4.3": "2026.4",
    "2026.6.5": "2026.5",
}


def test_derived_map_equals_historical_literal():
    assert _PACKAGE_TO_SCHEMA == HISTORICAL


def test_fallback_equals_historical_literal():
    assert _PACKAGE_TO_SCHEMA_FALLBACK == HISTORICAL


def test_resolve_still_works():
    assert _resolve_package_to_schema("2026.4.3") == "2026.4"
