"""Compatibility tests for the configuration-schema namespace."""

from __future__ import annotations

import importlib
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = pytest.mark.api

_MODULE_EXPORTS: dict[str, tuple[str, ...]] = {
    "": (
        "CURRENT_SCHEMA_VERSION",
        "SCHEMA_VERSIONS",
        "SchemaMigrator",
        "create_schema_bundle",
        "export_schema",
        "generate_json_schema",
        "get_schema_compatibility_message",
        "increment_schema_version",
        "is_schema_compatible",
        "migrate_config_file",
        "save_schema",
        "update_yaml_schema_version",
        "validate_config_against_schema",
        "validate_schema_version",
    ),
    ".version": (
        "CURRENT_SCHEMA_VERSION",
        "SCHEMA_VERSIONS",
        "get_schema_compatibility_message",
        "is_schema_compatible",
        "validate_schema_version",
    ),
    ".migration": (
        "SchemaMigrator",
        "check_migration_needed",
        "migrate_config_file",
    ),
    ".updater": (
        "find_yaml_configs",
        "increment_schema_version",
        "main",
        "update_yaml_schema_version",
    ),
    ".publisher": (
        "create_schema_bundle",
        "generate_json_schema",
        "main",
        "save_schema",
        "validate_config_against_schema",
    ),
    ".exporter": ("BASE_URL", "export_schema", "main"),
    ".registry": ("SchemaRegistry",),
}


@pytest.mark.parametrize(
    ("module_suffix", "export_names"),
    _MODULE_EXPORTS.items(),
)
def test_legacy_exports_delegate_to_canonical_namespace(
    module_suffix: str,
    export_names: tuple[str, ...],
) -> None:
    """Legacy imports expose the canonical implementation objects."""
    canonical = importlib.import_module(f"supy.data_model.configuration{module_suffix}")
    legacy = importlib.import_module(f"supy.data_model.schema{module_suffix}")

    for export_name in export_names:
        canonical_value = getattr(canonical, export_name)
        legacy_value = getattr(legacy, export_name)
        if isinstance(canonical_value, str):
            assert legacy_value == canonical_value
        else:
            assert legacy_value is canonical_value


def test_package_star_exports_remain_unchanged() -> None:
    """The compatibility package preserves the established star-import API."""
    canonical = importlib.import_module("supy.data_model.configuration")
    legacy = importlib.import_module("supy.data_model.schema")

    assert legacy.__all__ == canonical.__all__


@pytest.mark.parametrize("module_name", ["exporter", "publisher", "updater"])
def test_legacy_executable_modules_forward_help(
    module_name: str,
    tmp_path: Path,
) -> None:
    """Legacy ``python -m`` entry points still invoke the canonical CLI."""
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            f"supy.data_model.schema.{module_name}",
            "--help",
        ],
        check=False,
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
    )

    assert result.returncode == 0, result.stderr
    assert "usage:" in result.stdout.lower()


def test_legacy_updater_preserves_unsuccessful_exit_code(tmp_path: Path) -> None:
    """The updater wrapper forwards the canonical command's return value."""
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "supy.data_model.schema.updater",
            "--directory",
            str(tmp_path),
            "--current",
        ],
        check=False,
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
    )

    assert result.returncode == 1, result.stderr
    assert "No files to update" in result.stdout
