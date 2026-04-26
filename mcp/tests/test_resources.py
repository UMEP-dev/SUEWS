from __future__ import annotations

import subprocess
import sys
import types

import pytest
from suews_mcp import resources

pytestmark = pytest.mark.api


def _install_fake_schema_modules(monkeypatch):
    calls = {"count": 0}

    supy = types.ModuleType("supy")
    data_model = types.ModuleType("supy.data_model")
    schema = types.ModuleType("supy.data_model.schema")
    publisher = types.ModuleType("supy.data_model.schema.publisher")
    version = types.ModuleType("supy.data_model.schema.version")

    def generate_json_schema(version=None):
        calls["count"] += 1
        return {"title": f"SUEWS {version}"}

    publisher.generate_json_schema = generate_json_schema
    version.CURRENT_SCHEMA_VERSION = "2026.5"
    version.SCHEMA_VERSIONS = {"2026.5": "current schema"}

    for module in [supy, data_model, schema, publisher, version]:
        monkeypatch.setitem(sys.modules, module.__name__, module)

    resources.schema_json.cache_clear()
    return calls


def test_schema_json_uses_registry_and_cache(monkeypatch):
    calls = _install_fake_schema_modules(monkeypatch)

    first = resources.schema_json("current")
    second = resources.schema_json("current")

    assert '"version": "2026.5"' in first
    assert first == second
    assert calls["count"] == 1


def test_schema_json_rejects_unknown_version(monkeypatch):
    _install_fake_schema_modules(monkeypatch)

    with pytest.raises(ValueError, match="unknown SUEWS schema version"):
        resources.schema_json("1900.1")


def test_sample_config_reads_packaged_resource(tmp_path, monkeypatch):
    package_root = tmp_path / "supy"
    sample_dir = package_root / "sample_data"
    sample_dir.mkdir(parents=True)
    sample = sample_dir / "sample_config.yml"
    sample.write_text("name: sample\n", encoding="utf-8")

    monkeypatch.setattr(resources, "files", lambda package: package_root)

    assert resources.sample_config() == "name: sample\n"


def test_cli_help_rejects_non_allowlisted_command(tmp_path):
    with pytest.raises(ValueError, match="allowlisted"):
        resources.cli_help("python", tmp_path)


def test_cli_help_returns_combined_output(tmp_path, monkeypatch):
    def fake_run(command, args, *, cwd, timeout_s):
        return subprocess.CompletedProcess(args, 0, "help text", "warning")

    monkeypatch.setattr(resources, "run_allowlisted_command", fake_run)

    assert resources.cli_help("suews-validate", tmp_path) == "help text\nwarning"
