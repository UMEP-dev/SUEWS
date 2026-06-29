"""
Guard against `CURRENT_SCHEMA_VERSION` drifting from `sample_config.yml`.

`src/supy/data_model/schema/version.py::CURRENT_SCHEMA_VERSION` is the
canonical version label; `src/supy/sample_data/sample_config.yml` ships
with `schema_version: '<version>'`. The two must agree, otherwise:

* a fresh install reads a sample_config whose signature the validator
  treats as drifted,
* the YAML-upgrade path dispatches through a migration handler that
  doesn't exist (the sample is already at current),
* the `verify-build` skill's sync check flags a mismatch on every run.

The mismatch that motivated this test (gh#1304) went undetected for
several releases because the `verify-build` check is only run on
demand. Having it as a pytest-collected regression makes every PR fail
fast when the two diverge.
"""

from importlib.resources import files
from pathlib import Path

import pytest
import yaml

from supy.data_model.schema import CURRENT_SCHEMA_VERSION

pytestmark = pytest.mark.api

SAMPLE_CONFIG = Path(str(files("supy").joinpath("sample_data/sample_config.yml")))


@pytest.mark.cfg
def test_sample_config_schema_version_matches_current():
    """`sample_config.yml::schema_version` must equal CURRENT_SCHEMA_VERSION."""
    # ARRANGE
    assert SAMPLE_CONFIG.exists(), SAMPLE_CONFIG

    # ACT
    payload = yaml.safe_load(SAMPLE_CONFIG.read_text(encoding="utf-8"))

    # ASSERT
    assert payload.get("schema_version") == CURRENT_SCHEMA_VERSION, (
        f"sample_config.yml::schema_version = {payload.get('schema_version')!r} "
        f"but CURRENT_SCHEMA_VERSION = {CURRENT_SCHEMA_VERSION!r}. "
        "Bump one side to match the other — see "
        ".claude/rules/python/schema-versioning.md for when structural "
        "changes require a CURRENT_SCHEMA_VERSION bump."
    )


def _iter_ref_blocks(value, path=""):
    if isinstance(value, dict):
        ref = value.get("ref")
        if isinstance(ref, dict):
            yield f"{path}.ref" if path else "ref", ref
        for key, child in value.items():
            child_path = f"{path}.{key}" if path else str(key)
            yield from _iter_ref_blocks(child, child_path)
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from _iter_ref_blocks(child, f"{path}[{index}]")


@pytest.mark.cfg
def test_sample_config_includes_complete_reference_metadata_example():
    """`sample_config.yml` should demonstrate refs at several config levels."""
    assert SAMPLE_CONFIG.exists(), SAMPLE_CONFIG

    payload = yaml.safe_load(SAMPLE_CONFIG.read_text(encoding="utf-8"))
    complete_ref_paths = {
        path
        for path, ref in _iter_ref_blocks(payload)
        if {"desc", "ID", "DOI"}.issubset(ref)
    }

    expected_ref_paths = {
        "model.control.ref",
        "model.physics.ref",
        "sites[0].properties.ref",
        "sites[0].properties.conductance.ref",
        "sites[0].properties.land_cover.ref",
    }

    assert expected_ref_paths.issubset(complete_ref_paths), (
        "sample_config.yml should include complete reference metadata examples "
        f"with desc, ID, and DOI at: {sorted(expected_ref_paths)}"
    )
