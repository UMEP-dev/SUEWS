"""Tests for the shipped SUEWS model-version registry (Unit A)."""

import pytest

from supy._model_registry import (
    ModelVersion,
    list_model_versions,
    model_version_info,
    schema_for,
    drift_baseline_tag,
    _validate,
)

# API-surface tests (read-only registry loader); see .claude/rules/tests/patterns.md.
pytestmark = pytest.mark.api

# The release-tag -> schema mapping that yaml_upgrade._PACKAGE_TO_SCHEMA
# shipped before the registry existed. The registry must reproduce it
# exactly for these keys.
HISTORICAL_PACKAGE_TO_SCHEMA = {
    "2025.10.15": "2025.12",
    "2025.11.20": "2025.12",
    "2026.1.28": "2026.1",
    "2026.4.3": "2026.4",
    "2026.6.5": "2026.5",
}

_ALLOWED_ERAS = {"fortran-binary", "f90wrap", "modern-fortran", "rust-bridge"}
_ALLOWED_INSTALL = {"pip", "compile", "unavailable"}
_ALLOWED_REPRO = {
    "benchmarked",
    "pip-installable",
    "legacy-external-ref",
    "documented-only",
}


def test_list_returns_typed_sorted_records():
    versions = list_model_versions()
    assert versions, "registry must not be empty"
    assert all(isinstance(v, ModelVersion) for v in versions)
    # Sorted ascending by (released, tag).
    keys = [(v.released, v.tag) for v in versions]
    assert keys == sorted(keys)


def test_enums_are_constrained():
    for v in list_model_versions():
        assert v.physics_era in _ALLOWED_ERAS, v.tag
        assert v.install_method in _ALLOWED_INSTALL, v.tag
        assert v.reproducibility in _ALLOWED_REPRO, v.tag


def test_schema_for_matches_historical_literal():
    for tag, schema in HISTORICAL_PACKAGE_TO_SCHEMA.items():
        assert schema_for(tag) == schema


def test_model_version_info_unknown_tag_raises():
    with pytest.raises(KeyError):
        model_version_info("9999.9.9")


def test_benchmarked_versions_point_at_local_results():
    for v in list_model_versions():
        if v.reproducibility == "benchmarked":
            assert v.reference_source == f"benchmark/results/{v.tag}", v.tag


def test_drift_baseline_is_a_known_benchmarked_tag():
    baseline = drift_baseline_tag()
    info = model_version_info(baseline)
    assert info.reproducibility == "benchmarked"


def _good_record():
    return {
        "tag": "9.9.9",
        "released": "2099",
        "schema_version": None,
        "physics_era": "rust-bridge",
        "install_method": "pip",
        "reproducibility": "benchmarked",
        "reference_source": None,
        "version_history_anchor": None,
    }


def test_validate_rejects_bad_enum():
    rec = _good_record()
    rec["physics_era"] = "nonsense"
    with pytest.raises(ValueError):
        _validate(rec)


def test_validate_rejects_missing_field():
    rec = _good_record()
    del rec["install_method"]
    with pytest.raises(ValueError):
        _validate(rec)


def test_validate_rejects_unknown_field():
    rec = _good_record()
    rec["extra"] = 1
    with pytest.raises(ValueError):
        _validate(rec)


def test_validate_rejects_bad_date():
    rec = _good_record()
    rec["released"] = "not-a-date"
    with pytest.raises(ValueError):
        _validate(rec)
