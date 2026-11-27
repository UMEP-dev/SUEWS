"""Unit and real-CLI integration tests for the ``validate_config`` MCP tool."""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path
import shutil
import subprocess
import sys

import pytest
import yaml

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API", "C-ERROR")]


def _installed_suews_available() -> bool:
    sibling = Path(sys.executable).parent / "suews"
    return sibling.is_file() or shutil.which("suews") is not None


_require_installed_cli = pytest.mark.skipif(
    not _installed_suews_available(),
    reason=(
        "`suews` CLI is not installed in the current environment. Run "
        "`make dev` before running real-CLI MCP integration tests."
    ),
)


def _load_sample_config_dict() -> dict:
    text = (files("supy") / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _write_config(path: Path, data: dict) -> Path:
    path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return path


def test_returns_envelope_from_subprocess(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import validate_config

    config = tmp_path / "config.yml"
    config.write_text("x: 1", encoding="utf-8")

    envelope = {
        "status": "success",
        "data": {"errors": [], "warnings": [], "schema_version": "2026.5.dev5"},
        "errors": [],
        "warnings": [],
        "meta": {"command": "suews validate config.yml --format json"},
    }

    captured: dict = {}

    def fake_run(cmd, **_kwargs):
        captured["cmd"] = cmd
        assert cmd[0].lower().endswith(("suews", "suews.exe"))
        assert cmd[1] == "validate"
        return subprocess.CompletedProcess(
            args=cmd, returncode=0, stdout=json.dumps(envelope), stderr=""
        )

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = validate_config(str(config), project_root=str(tmp_path))
    assert result == envelope
    # `suews validate` envelope path requires --dry-run BEFORE the file
    # because [FILES]... is a greedy variadic; lock that contract in.
    assert "--dry-run" in captured["cmd"]
    assert "--format" in captured["cmd"]
    dry_run_idx = captured["cmd"].index("--dry-run")
    file_idx = captured["cmd"].index(str(config))
    assert dry_run_idx < file_idx


def test_sandbox_violation_returns_error_envelope(tmp_path: Path) -> None:
    from suews_mcp.tools import validate_config

    (tmp_path / "case").mkdir()
    result = validate_config("/etc/passwd", project_root=str(tmp_path / "case"))
    assert result["status"] == "error"
    assert any("outside the project root" in str(e) for e in result["errors"])


def test_executable_missing_returns_error_envelope(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    from suews_mcp.tools import validate_config

    config = tmp_path / "config.yml"
    config.write_text("x: 1", encoding="utf-8")

    def fake_run(cmd, **_kwargs):
        raise FileNotFoundError(cmd[0])

    monkeypatch.setattr(subprocess, "run", fake_run)
    result = validate_config(str(config), project_root=str(tmp_path))
    assert result["status"] == "error"


@_require_installed_cli
def test_real_cli_validate_config_valid_struct_science_and_public_mode(
    tmp_path: Path,
) -> None:
    """Real installed CLI through MCP verifies valid, structural, scientific, and public-mode cases with zero writes."""
    from suews_mcp.tools import validate_config

    # 1. Valid configuration returns a success envelope with provenance and zero writes.
    valid_dir = tmp_path / "valid"
    valid_dir.mkdir()
    valid_cfg = _write_config(valid_dir / "config.yml", _load_sample_config_dict())
    before_valid = set(valid_dir.rglob("*"))

    valid_env = validate_config(str(valid_cfg), project_root=str(valid_dir))
    assert set(valid_dir.rglob("*")) == before_valid
    assert valid_env["status"] == "success", valid_env
    assert valid_env["data"]["is_valid"] is True
    assert valid_env["errors"] == []
    assert valid_env["meta"]["schema_version"]
    assert "--dry-run" in valid_env["meta"]["command"]

    # 2. Structurally invalid configuration (nlayer dimension mismatch) returns an error envelope.
    struct_dir = tmp_path / "struct"
    struct_dir.mkdir()
    struct_data = _load_sample_config_dict()
    struct_data["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 4}
    struct_cfg = _write_config(struct_dir / "bad_nlayer.yml", struct_data)
    before_struct = set(struct_dir.rglob("*"))

    struct_env = validate_config(str(struct_cfg), project_root=str(struct_dir))
    assert set(struct_dir.rglob("*")) == before_struct
    assert struct_env["status"] == "error", struct_env
    assert struct_env["data"]["is_valid"] is False
    assert struct_env["errors"]
    assert any(
        "nlayer" in str(err.get("message", "")).lower()
        or "expected 4" in str(err.get("message", "")).lower()
        for err in struct_env["errors"]
    ), struct_env["errors"]

    # 3. Scientifically invalid configuration (Phase B land-cover sum != 1.0) is caught in dry-run.
    science_dir = tmp_path / "science"
    science_dir.mkdir()
    science_data = _load_sample_config_dict()
    science_data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {
        "value": 0.99
    }
    science_cfg = _write_config(science_dir / "bad_sfr.yml", science_data)
    before_science = set(science_dir.rglob("*"))

    science_env = validate_config(str(science_cfg), project_root=str(science_dir))
    assert set(science_dir.rglob("*")) == before_science
    assert science_env["status"] == "error", science_env
    assert science_env["data"]["is_valid"] is False
    assert science_env["errors"]
    assert any(
        "fraction" in str(err.get("message", "")).lower()
        or "land" in str(err.get("message", "")).lower()
        or "sfr" in str(err.get("message", "")).lower()
        for err in science_env["errors"]
    ), science_env["errors"]

    # 4. Public-mode-restricted configuration (STEBBS enabled) is blocked under default public dry-run.
    public_dir = tmp_path / "public_mode"
    public_dir.mkdir()
    public_data = _load_sample_config_dict()
    public_data["model"]["physics"]["stebbs"]["enabled"] = {"value": True}
    public_cfg = _write_config(public_dir / "stebbs_public.yml", public_data)
    before_public = set(public_dir.rglob("*"))

    public_env = validate_config(str(public_cfg), project_root=str(public_dir))
    assert set(public_dir.rglob("*")) == before_public
    assert public_env["status"] == "error", public_env
    assert public_env["data"]["is_valid"] is False
    assert public_env["errors"]
    assert any(
        "stebbs" in str(err.get("message", "")).lower()
        for err in public_env["errors"]
    ), public_env["errors"]

    # 5. Null bounds + null forcing surfaces the skipped-seasonal-checks warning through MCP.
    warn_dir = tmp_path / "warn_null_bounds"
    warn_dir.mkdir()
    warn_data = _load_sample_config_dict()
    warn_data["model"]["control"]["start_time"] = None
    warn_data["model"]["control"]["end_time"] = None
    warn_data["model"]["control"]["forcing"]["file"] = None
    warn_cfg = _write_config(warn_dir / "null_bounds.yml", warn_data)
    before_warn = set(warn_dir.rglob("*"))

    warn_env = validate_config(str(warn_cfg), project_root=str(warn_dir))
    assert set(warn_dir.rglob("*")) == before_warn
    assert warn_env["status"] == "warning", warn_env
    assert warn_env["data"]["is_valid"] is True
    assert any(
        isinstance(w, dict)
        and w.get("phase") == "B"
        and w.get("issue_code") == "SEASONAL.MODEL_CONTROL_START_TIME"
        for w in warn_env["warnings"]
    ), warn_env["warnings"]

    # 6. Inactive irrigation (water_use=1, ie_start=20.5, ie_end=None) passes MCP dry-run.
    irr_dir = tmp_path / "inactive_irr"
    irr_dir.mkdir()
    irr_data = _load_sample_config_dict()
    irr_data["model"]["physics"]["water_use"] = {"value": 1}
    irr_data["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    irr_data["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    irr_cfg = _write_config(irr_dir / "inactive_irr.yml", irr_data)
    before_irr = set(irr_dir.rglob("*"))

    irr_env = validate_config(str(irr_cfg), project_root=str(irr_dir))
    assert set(irr_dir.rglob("*")) == before_irr
    assert irr_env["status"] in {"success", "warning"}, irr_env
    assert irr_env["data"]["is_valid"] is True
