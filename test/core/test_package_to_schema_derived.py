"""The package->schema map must be derived from the registry and unchanged."""

import pytest

import supy._model_registry as _registry
from supy.util.converter.yaml_upgrade import (
    _PACKAGE_TO_SCHEMA,
    _PACKAGE_TO_SCHEMA_FALLBACK,
    _build_package_to_schema,
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


def test_build_degrades_to_fallback_on_broken_registry(monkeypatch):
    """A missing/malformed registry must degrade to the literal fallback.

    registry.yaml is read lazily on the first call to list_model_versions(),
    so the failure surfaces inside _build_package_to_schema()'s comprehension,
    not at the import guard. The build must still return the historical map.
    """

    def _raise():
        raise FileNotFoundError("registry.yaml missing (simulated partial install)")

    monkeypatch.setattr(_registry, "list_model_versions", _raise)
    assert _build_package_to_schema() == _PACKAGE_TO_SCHEMA_FALLBACK
