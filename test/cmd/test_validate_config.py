"""Tests for ``suews validate`` — Envelope shape + invalid-input regressions.

Acceptance for gh#1360:
- ``--format json`` output validates against the canonical SUEWS Envelope.
- Validation errors surface a ``schema_path`` and a ``hint``.
- Per ``.claude/rules/tests/patterns.md`` (Validation Edge Cases): every
  tightened input contract carries explicit regressions for negative
  values, the ``-999`` sentinel, and ``NaN`` — and at least one
  regression per public path. Here that's the CLI (``suews validate``)
  and the library function ``validate_single_file()``.

The fixtures all start from the bundled sample config and inject one
breaking change per parametrised case so the regression target is
unambiguous.
"""

from __future__ import annotations

from contextlib import contextmanager, nullcontext
import copy
from importlib.resources import as_file, files
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sysconfig
from typing import Any, Callable, Dict, Iterator

from click.testing import CliRunner
import pytest
import yaml

pytestmark = [pytest.mark.api, pytest.mark.contract("C-VALID", "C-ERROR")]


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@contextmanager
def _sample_yaml_path() -> Iterator[Path]:
    """Yield the bundled sample config as a filesystem path for Click."""
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    with as_file(resource) as path_sample:
        yield path_sample


def _load_sample_dict() -> Dict[str, Any]:
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    if not resource.is_file():
        pytest.skip(f"Sample config not available at {resource}")
    return yaml.safe_load(resource.read_text(encoding="utf-8"))


def _write_yaml(path: Path, payload: Dict[str, Any]) -> None:
    path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")


def _set_first_paved_sfr(payload: Dict[str, Any], value: float) -> None:
    """Override the first site's ``land_cover.paved.sfr`` to ``value``.

    The sample config nests this as ``{"value": 0.43}``; we keep the
    RefValue wrapper so the change is structurally valid (only the
    numeric content is invalid).
    """
    sites = payload.get("sites") or []
    if not sites:
        pytest.skip("Sample config has no sites — fixture cannot be built")
    paved = sites[0]["properties"]["land_cover"]["paved"]
    if isinstance(paved.get("sfr"), dict) and "value" in paved["sfr"]:
        paved["sfr"]["value"] = value
    else:
        paved["sfr"] = {"value": value}


# ---------------------------------------------------------------------------
# Envelope shape — happy path
# ---------------------------------------------------------------------------



@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("dry_run", [False, True])
@pytest.mark.parametrize("field", ["model", "physics"])
@pytest.mark.parametrize("value", [None, [], "invalid", -999])
def test_validate_malformed_mapping_emits_location(
    tmp_path, mode, dry_run, field, value
):
    """Malformed containers fail through the real public command envelope."""
    from supy.cmd.suews_cli import cli

    payload = _load_sample_dict()
    container = payload if field == "model" else payload["model"]
    container[field] = value
    path = tmp_path / "malformed.yml"
    _write_yaml(path, payload)
    args = ["validate", str(path), "-p", "C", "--mode", mode, "--format", "json"]
    if dry_run:
        args.append("--dry-run")
    result = CliRunner().invoke(cli, args)

    assert result.exit_code == 1, result.output
    assert "Traceback" not in result.output
    envelope = json.loads(result.stdout)
    assert set(envelope) == {"status", "data", "errors", "warnings", "meta"}
    assert envelope["status"] == "error"
    expected = "model" if field == "model" else "model.physics"
    assert any(
        (error.get("yaml_path") or error.get("schema_path") or "")
        .replace(" -> ", ".") == expected
        and error.get("message")
        for error in envelope["errors"]
    ), envelope["errors"]


@pytest.mark.parametrize("mode", ["public", "dev"])
def test_validate_c_valid_mapping_executes_phase(tmp_path, mode):
    """A valid control must execute C and succeed, not merely avoid a crash."""
    from supy.cmd.suews_cli import cli

    path = _copy_sample_data(tmp_path)
    result = CliRunner().invoke(
        cli, ["validate", str(path), "-p", "C", "--mode", mode, "--format", "json"]
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert not envelope["errors"]
    assert envelope["data"]["phases_run"] == ["C"]

def _resolved_sample_dict():
    """Use the supported model dump, not human-facing physics shorthand."""
    from supy.data_model.core.config import SUEWSConfig

    return SUEWSConfig.from_dict(_load_sample_dict()).model_dump(
        mode="json", exclude_none=True, round_trip=True
    )


@pytest.mark.parametrize("pipeline", ["C", "ABC"])
def test_site_longitude_source_compatibility(tmp_path, pipeline):
    """Run the public CLI with canonical, alias-only and duplicate input."""
    import jsonschema
    from supy.cmd.suews_cli import cli
    from supy.data_model.configuration.publisher import generate_json_schema

    canonical = _resolved_sample_dict()
    # Establish the positive control independently before exercising aliases.
    jsonschema.Draft7Validator(generate_json_schema()).validate(canonical)
    path = tmp_path / "longitude.yml"
    for spelling in ["canonical", "alias", "duplicate"]:
        data = copy.deepcopy(canonical)
        props = data["sites"][0]["properties"]
        if spelling == "alias":
            props["lon"] = props.pop("lng")
        elif spelling == "duplicate":
            props["lon"] = copy.deepcopy(props["lng"])
        _write_yaml(path, data)
        original = path.read_bytes()
        result = CliRunner().invoke(
            cli,
            ["validate", "--pipeline", pipeline, "--dry-run",
             "--format", "json", str(path)],
        )
        assert path.read_bytes() == original
        envelope = json.loads(result.stdout)
        assert envelope["data"]["files"][0]["file"] == str(path)
        if spelling != "canonical":
            assert result.exit_code == 1, result.output
            errors = envelope["errors"]
            assert errors
            assert all(error["file"] == str(path) for error in errors)
            assert any("lon" in error["message"] for error in errors)
        else:
            assert result.exit_code == 0, result.output
            assert envelope["errors"] == []


@pytest.mark.parametrize(
    "entry", ["cli-C", "cli-ABC", "standalone-C", "standalone-ABC", "orchestrator-ABC"]
)
@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize(
    "spelling",
    ["canonical", "alias", "alias-null", "alias-wrapped-null", "duplicate",
     "duplicate-null", "duplicate-invalid-both", "canonical-invalid"],
)
def test_site_longitude_normal_pipeline(
    tmp_path, monkeypatch, capsys, entry, mode, spelling
):
    """Only lng is public; real phases must reject every lon spelling.

    The installed commands run with the candidate environment, including an
    uncommitted working tree. No git archive or HEAD checkout supplies code.
    ABC stops at B for missing/invalid lng, otherwise C rejects unknown lon.
    Rejection publishes the last successful phase's YAML and the diagnosis.
    """
    from supy._run_rust import config_to_engine_yaml
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.validation.pipeline import orchestrator

    path = _copy_sample_data(tmp_path)
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    props = data["sites"][0]["properties"]
    props["lng"] = {"value": 0.0}
    if spelling.startswith("alias"):
        props["lon"] = props.pop("lng")
        if spelling == "alias-null":
            props["lon"] = None
        elif spelling == "alias-wrapped-null":
            props["lon"] = {"value": None}
    elif spelling == "duplicate-invalid-both":
        props["lng"] = {"value": "invalid"}
        props["lon"] = {"value": None}
    elif spelling.startswith("duplicate"):
        props["lon"] = (
            None if spelling == "duplicate-null" else copy.deepcopy(props["lng"])
        )
    elif spelling == "canonical-invalid":
        props["lng"] = {"value": "invalid"}
    _write_yaml(path, data)
    original = {
        item: (item.read_bytes(), item.stat().st_mtime_ns, item.stat().st_ctime_ns)
        for item in tmp_path.iterdir() if item.is_file()
    }
    updated_path = path.with_name(f"updated_{path.name}")
    report_path = path.with_name(f"report_{path.stem}.txt")
    pipeline = entry.rsplit("-", 1)[1]
    rejected = spelling != "canonical"
    stops_at_b = pipeline == "ABC" and (
        spelling.startswith("alias")
        or spelling in {"duplicate-invalid-both", "canonical-invalid"}
    )
    expected_phases = ["C"] if pipeline == "C" else (["A", "B"] if stops_at_b else ["A", "B", "C"])
    failed_phase = "B" if stops_at_b else "C"

    if entry == "orchestrator-ABC":
        monkeypatch.setattr(
            "sys.argv",
            ["suews_yaml_processor", str(path), "--phase", "ABC", "--mode", mode],
        )
        exit_code = orchestrator.main()
        diagnostics = capsys.readouterr().out
    else:
        route = "suews-validate" if entry.startswith("standalone") else "suews"
        executable = Path(sysconfig.get_path("scripts")) / (
            route + (".exe" if os.name == "nt" else "")
        )
        assert executable.is_file(), f"Missing installed command: {executable}"
        command = [str(executable)] + (["validate"] if route == "suews" else [])
        result = subprocess.run(
            command + [str(path), "--pipeline", pipeline, "--mode", mode,
                       "--format", "json"],
            cwd=tmp_path, capture_output=True, text=True, timeout=180,
        )
        exit_code = result.returncode
        diagnostics = result.stdout + result.stderr
        envelope = json.loads(result.stdout)
        assert envelope["data"]["phases_run"] == expected_phases
        assert bool(envelope["errors"]) == rejected

    assert exit_code == int(rejected), diagnostics
    assert "Traceback" not in diagnostics
    assert report_path.is_file(), diagnostics
    report = report_path.read_text(encoding="utf-8")
    sidecar = json.loads(report_path.with_suffix(".json").read_text(encoding="utf-8"))
    phases = sidecar["phases"]
    assert [phase["phase"] for phase in phases] == expected_phases
    assert [phase["phase"] for phase in phases if phase["status"] == "FAILED"] == (
        [failed_phase] if rejected else []
    )
    issues = [issue for phase in phases for issue in phase["issues"]]
    assert not any(
        issue["code"] == "A.RENAMED_PARAM" and "lon" in issue["message"]
        for issue in issues
    )
    assert "sites[0].properties.lon changed to sites[0].properties.lng" not in report
    assert "A.INPUT.AMBIGUOUS_LONGITUDE" not in report
    assert "A.PIPELINE.EXCEPTION" not in report
    if entry != "orchestrator-ABC":
        assert envelope["data"]["validation_report"] == sidecar

    updated = yaml.safe_load(updated_path.read_text(encoding="utf-8"))
    annotated = updated["sites"][0]["properties"]
    for authored_key in ("lon", "lng"):
        if authored_key in props:
            assert annotated[authored_key] == props[authored_key]
    if rejected:
        authored_key = "lng" if spelling == "canonical-invalid" else "lon"
        if stops_at_b:
            assert any(issue["code"] == "GEOGRAPHY.LNG"
                       and issue["severity"] == "ERROR"
                       and issue["yaml_path"] == "lng"
                       for issue in phases[-1]["issues"])
            if authored_key == "lon":
                assert any(issue["code"] == "A.EXTRA_PARAM"
                           and issue["yaml_path"] == "sites[0].properties.lon"
                           for issue in phases[0]["issues"])
                assert "sites[0].properties.lon" in report
            else:
                assert "lng" in report
        else:
            authored_issues = [
                issue for issue in phases[-1]["issues"]
                if f".properties.{authored_key}" in (issue.get("yaml_path") or "")
            ]
            assert authored_issues, phases[-1]
            if authored_key == "lon":
                assert any(issue["code"] == "C.PYDANTIC.EXTRA_FORBIDDEN"
                           for issue in authored_issues)
            assert f"properties.{authored_key}" in report
        # A fallback is evidence, not an accepted alternative configuration.
        with pytest.raises(ValueError):
            SUEWSConfig.from_yaml(str(updated_path))
        if pipeline == "ABC":
            evidence, = tmp_path.glob(".suews-validate-*")
            last_success = "A" if stops_at_b else "B"
            assert updated_path.read_bytes() == (
                evidence / f"updated{last_success}_{path.name}"
            ).read_bytes()
            assert phases[-1]["yaml_out"] is None
    else:
        assert "lon" not in annotated
        config = SUEWSConfig.from_yaml(str(updated_path))
        longitude = config.sites[0].properties.lng
        assert getattr(longitude, "value", longitude) == 0.0
        resolved = yaml.safe_load(config_to_engine_yaml(config))["sites"][0]["properties"]
        assert "lon" not in resolved
        longitude = resolved["lng"]
        assert (longitude["value"] if isinstance(longitude, dict) else longitude) == 0.0
        if pipeline == "ABC":
            assert not list(tmp_path.glob(".suews-validate-*"))
    assert {
        item: (item.read_bytes(), item.stat().st_mtime_ns, item.stat().st_ctime_ns)
        for item in original
    } == original


def _soil_input_warning(key, canonical):
    """The validated model warns when it receives the authored legacy key."""
    if key == canonical:
        return nullcontext()
    message = f"PavedProperties: field '{key}' is deprecated, use '{canonical}' instead."
    return pytest.warns(DeprecationWarning, match=re.escape(message))


@pytest.mark.parametrize("entry", ["cli-ABC", "standalone-ABC", "orchestrator-ABC"])
@pytest.mark.parametrize("mode", ["public", "dev"])
def test_supported_alias_atomic_report_repeats_once_per_section(
    tmp_path, monkeypatch, capsys, entry, mode
):
    """Transfer positive rename/report coverage to supported soildepth, not lon."""
    from supy.cmd.suews_cli import cli
    from supy.cmd.validate_config import cli as standalone_cli
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.validation.pipeline import orchestrator

    path = _copy_sample_data(tmp_path)
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    paved = data["sites"][0]["properties"]["land_cover"]["paved"]
    value = paved.pop("soil_depth")
    paved["soildepth"] = value
    # The input loader accepts the alias and names its replacement.
    with _soil_input_warning("soildepth", "soil_depth"):
        SUEWSConfig.from_dict(data)
    _write_yaml(path, data)
    original = {p: p.read_bytes() for p in tmp_path.iterdir() if p.is_file()}
    snapshots = []
    for _ in range(2):
        if entry == "orchestrator-ABC":
            monkeypatch.setattr("sys.argv", [
                "suews_yaml_processor", str(path), "--phase", "ABC", "--mode", mode,
            ])
            exit_code = orchestrator.main()
            diagnostics = capsys.readouterr().out
        else:
            command = standalone_cli if entry.startswith("standalone") else cli
            args = [] if entry.startswith("standalone") else ["validate"]
            result = CliRunner().invoke(command, args + [
                str(path), "--pipeline", "ABC", "--mode", mode, "--format", "json",
            ])
            exit_code, diagnostics = result.exit_code, result.output
        assert exit_code == 0, diagnostics
        products = {name: (tmp_path / name).read_bytes() for name in (
            f"updated_{path.name}", f"report_{path.stem}.txt", f"report_{path.stem}.json",
        )}
        snapshots.append(products)
        sidecar = json.loads(products[f"report_{path.stem}.json"])
        if entry != "orchestrator-ABC":
            assert json.loads(result.stdout)["data"]["validation_report"] == sidecar
        renamed = [issue for phase in sidecar["phases"] for issue in phase["issues"]
                   if issue["code"] == "A.RENAMED_PARAM"]
        soil_renames = [issue for issue in renamed if issue["yaml_path"] == "soil_depth"]
        assert len(soil_renames) == 1
        assert soil_renames[0]["message"] == "Renamed soildepth -> soil_depth"
        assert soil_renames[0]["applied_fix"] is True
        assert all(issue["phase"] == "A" for issue in renamed)
        report = products[f"report_{path.stem}.txt"].decode("utf-8")
        sections, heading = {}, ""
        for line in report.splitlines():
            if line.startswith("## "):
                heading = line[3:].strip()
            else:
                sections.setdefault(heading, []).append(line.strip())
        rename_note = "soildepth changed to soil_depth"
        headers = {
            "INFO": f"- Updated ({len(renamed)}) renamed parameter(s):",
            "APPLIED UPDATES": f"- Updated ({len(renamed)}) renamed parameter(s) to current standards:",
        }
        assert {heading for heading, lines in sections.items()
                if any(rename_note in line for line in lines)} == set(headers)
        for heading, header in headers.items():
            lines = sections[heading]
            assert lines.count(header) == 1, report
            for issue in renamed:
                old, new = issue["message"].removeprefix("Renamed ").split(" -> ")
                detail = f"-- {old} changed to {new}"
                assert lines.count(detail) == 1, report
                assert lines.index(header) < lines.index(detail), report
        updated_path = tmp_path / f"updated_{path.name}"
        SUEWSConfig.from_yaml(str(updated_path))
        updated = yaml.safe_load(updated_path.read_text(encoding="utf-8"))
        paved = updated["sites"][0]["properties"]["land_cover"]["paved"]
        assert paved["soil_depth"] == value
        assert "soildepth" not in paved
        assert not list(tmp_path.glob(".suews-validate-*"))
        assert {p: p.read_bytes() for p in original} == original
    assert snapshots[0] == snapshots[1]


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("pipeline", ["C", "ABC"])
@pytest.mark.parametrize("spelling", ["canonical", "alias"])
@pytest.mark.parametrize("form", ["valid", "null", "wrapped-null"])
@pytest.mark.parametrize("canonical,alias", [
    ("soil_depth", "soildepth"),
    ("soil_store_capacity", "soilstorecap"),
])
def test_supported_soil_aliases_preserve_required_null_rejection(
    tmp_path, mode, pipeline, spelling, form, canonical, alias
):
    """Documented soil aliases still work; neither spelling hides explicit nulls."""
    from supy.cmd.suews_cli import cli
    from supy.data_model.core.config import SUEWSConfig

    path = _copy_sample_data(tmp_path)
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    paved = data["sites"][0]["properties"]["land_cover"]["paved"]
    key = canonical if spelling == "canonical" else alias
    value = paved.pop(canonical)
    paved[key] = value if form == "valid" else (None if form == "null" else {"value": None})
    rejected = form != "valid"
    required_value = pytest.raises(ValueError, match=canonical) if rejected else nullcontext()
    with _soil_input_warning(key, canonical), required_value:
        SUEWSConfig.from_dict(data)
    _write_yaml(path, data)
    original = {p: p.read_bytes() for p in tmp_path.iterdir() if p.is_file()}
    output_key = canonical if pipeline == "ABC" else key
    # Phase A migrates the alias. Standalone C receives the original spelling.
    with _soil_input_warning(output_key, canonical):
        result = CliRunner().invoke(cli, [
            "validate", str(path), "--pipeline", pipeline, "--mode", mode, "--format", "json",
        ])
    assert result.exit_code == int(rejected), result.output
    assert "Traceback" not in result.output
    envelope = json.loads(result.stdout)
    assert bool(envelope["errors"]) == rejected
    assert envelope["data"]["phases_run"][0] == pipeline[0]
    report_path = tmp_path / f"report_{path.stem}.txt"
    sidecar = json.loads(report_path.with_suffix(".json").read_text(encoding="utf-8"))
    assert envelope["data"]["validation_report"] == sidecar
    updated = tmp_path / f"updated_{path.name}"
    assert all(
        issue["code"] != "C.PIPELINE.EXCEPTION"
        for phase in sidecar["phases"]
        for issue in phase["issues"]
    )
    required_value = pytest.raises(ValueError, match=canonical) if rejected else nullcontext()
    with _soil_input_warning(output_key, canonical), required_value:
        SUEWSConfig.from_yaml(str(updated))
    if rejected:
        # These existing soil completeness diagnostics intentionally use the
        # canonical field name, independently of the longitude unknown-key rule.
        assert canonical in report_path.read_text(encoding="utf-8")
        assert sidecar["overall_status"] == "FAILED"
    else:
        block = yaml.safe_load(updated.read_text(encoding="utf-8"))["sites"][0]["properties"]["land_cover"]["paved"]
        assert block[output_key] == value
    assert {p: p.read_bytes() for p in original} == original


def test_schema_file_longitude_source_compatibility(tmp_path):
    """The schema library path has its own independently valid control."""
    import jsonschema
    from supy.cmd.schema_cli import validate_file_against_schema
    from supy.data_model.configuration.publisher import generate_json_schema

    canonical = _resolved_sample_dict()
    jsonschema.Draft7Validator(generate_json_schema()).validate(canonical)
    path = tmp_path / "schema-longitude.yml"
    for spelling in ["canonical", "alias", "duplicate"]:
        data = copy.deepcopy(canonical)
        props = data["sites"][0]["properties"]
        if spelling == "alias":
            props["lon"] = props.pop("lng")
        elif spelling == "duplicate":
            props["lon"] = copy.deepcopy(props["lng"])
        _write_yaml(path, data)
        original = path.read_bytes()
        valid, errors = validate_file_against_schema(path)
        assert path.read_bytes() == original
        if spelling != "canonical":
            assert not valid
            assert "lon" in str(errors)
        else:
            assert valid, errors


def test_historical_file_stamp_does_not_enable_current_longitude_fold(
    tmp_path, monkeypatch
):
    """Default schema dispatch honours the file stamp; lon stays unknown.

    The publisher currently generates model schemas with version metadata,
    not archived historical shapes. Assert dispatch without inventing a
    historical longitude rejection policy.
    """
    import jsonschema
    from supy.cmd import schema_cli
    from supy.data_model.core.config import SUEWSConfig

    data = _resolved_sample_dict()
    data["schema_version"] = "2026.5.dev5"
    path = tmp_path / "historical.yml"
    generate = schema_cli.generate_json_schema
    versions = []

    def record_version(*, version):
        versions.append(version)
        return generate(version=version)

    monkeypatch.setattr(schema_cli, "generate_json_schema", record_version)
    for alias in [False, True]:
        if alias:
            props = data["sites"][0]["properties"]
            props["lon"] = props.pop("lng")
        _write_yaml(path, data)
        original = path.read_bytes()
        # Default mode still performs the human-facing loader's semantic check.
        if alias:
            with pytest.raises(ValueError, match="lon"):
                SUEWSConfig.from_yaml(str(path))
        else:
            SUEWSConfig.from_yaml(str(path))
        schema_errors = list(jsonschema.Draft7Validator(
            generate(version=data["schema_version"])
        ).iter_errors(data))
        if not alias:
            assert not schema_errors
        valid, errors = schema_cli.validate_file_against_schema(path)
        assert valid == (not schema_errors), errors
        model_errors = [error for error in errors if error.startswith("Model validation:")]
        assert len(model_errors) == int(alias)
        if alias:
            assert "lon" in model_errors[0]
        assert len(errors) == len(schema_errors) + int(alias)
        assert versions[-1] == data["schema_version"]
        assert path.read_bytes() == original


@pytest.mark.parametrize("entry", ["schema", "validate"])
@pytest.mark.parametrize("spelling", ["canonical", "alias", "duplicate"])
def test_explicit_historical_target_skips_longitude_fold(
    tmp_path, monkeypatch, entry, spelling
):
    """Explicit old targets retain schema-only validation for both helpers."""
    import jsonschema
    from supy.cmd import schema_cli, validate_config
    from supy.data_model.configuration.publisher import generate_json_schema
    from supy.data_model.core.config import SUEWSConfig

    data = _resolved_sample_dict()
    target = "2026.5.dev5"
    props = data["sites"][0]["properties"]
    if spelling == "alias":
        props["lon"] = props.pop("lng")
    elif spelling == "duplicate":
        props["lon"] = copy.deepcopy(props["lng"])
    path = tmp_path / "historical-target.yml"
    _write_yaml(path, data)
    original = path.read_bytes()
    schema = generate_json_schema(version=target)
    expected = list(jsonschema.Draft7Validator(schema).iter_errors(data))

    def unexpected_current_validation(*args, **kwargs):
        pytest.fail("Explicit historical target used current model validation")

    monkeypatch.setattr(SUEWSConfig, "from_yaml", unexpected_current_validation)
    if entry == "schema":
        valid, errors = schema_cli.validate_file_against_schema(
            path, schema_version=target
        )
    else:
        valid, errors = validate_config.validate_single_file(
            path, schema, schema_version=target
        )
    assert valid == (not expected), errors
    assert len(errors) == len(expected)
    assert path.read_bytes() == original



def test_validate_sample_config_emits_envelope() -> None:
    """``suews validate <sample> --format json`` emits the canonical envelope."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(
            validate_cli,
            [
                "--pipeline",
                "ABC",
                "--dry-run",
                "--format",
                "json",
                str(sample),
            ],
        )
    assert result.exit_code in {0, 1}, result.output
    envelope = json.loads(result.stdout)
    assert set(envelope.keys()) == {"status", "data", "errors", "warnings", "meta"}
    assert envelope["status"] in {"success", "warning", "error"}
    meta = envelope["meta"]
    for key in (
        "schema_version",
        "suews_version",
        "supy_version",
        "git_commit",
        "command",
        "started_at",
        "ended_at",
    ):
        assert key in meta, f"meta key {key!r} missing from envelope"


def test_validate_pipeline_c_json_accepts_multiple_files() -> None:
    """Pipeline C dry-run JSON mode validates more than one file."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(
            validate_cli,
            [
                "--pipeline",
                "C",
                "--dry-run",
                "--format",
                "json",
                str(sample),
                str(sample),
            ],
        )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"
    assert len(envelope["data"]["files"]) == 2


@pytest.fixture
def public_validator_sample(tmp_path):
    """Keep the sample's relative forcing reference beside its configuration."""
    project = tmp_path / "project"
    project.mkdir()
    path = _copy_sample_data(project)
    payload = yaml.safe_load(path.read_text(encoding="utf-8"))
    forcing_file = payload["model"]["control"]["forcing"]["file"]["value"]
    assert not Path(forcing_file).is_absolute()
    assert (path.parent / forcing_file).is_file()
    return path


def _source_file_snapshot(root):
    """Record complete source bytes and write/change timestamps, including forcing."""
    snapshot = {}
    for path in root.rglob("*"):
        if path.is_file():
            content = path.read_bytes()
            stat = path.stat()
            snapshot[path.relative_to(root)] = (
                content, stat.st_mtime_ns, stat.st_ctime_ns,
            )
    return snapshot


def _run_public_validation(path, pipeline, out_format, cwd, *, dry_run=False):
    executable = Path(sysconfig.get_path("scripts")) / (
        "suews.exe" if os.name == "nt" else "suews"
    )
    assert executable.is_file(), f"Public console script not installed: {executable}"
    command = [str(executable), "validate", str(path), "-p", pipeline]
    if dry_run:
        command.append("--dry-run")
    if out_format == "json":
        command.extend(["--format", "json"])
    # Omitting --format exercises the public command's default table renderer.
    return subprocess.run(
        command, cwd=cwd,
        env={**os.environ, "COLUMNS": "240", "NO_COLOR": "1"},
        capture_output=True, text=True, timeout=180,
    )


@pytest.mark.parametrize("pipeline", ["C", "ABC"])
@pytest.mark.parametrize("out_format", [pytest.param(None, id="default"), "json"])
@pytest.mark.parametrize("case", ["valid", "bad-tstep", "unknown-property"])
def test_public_validate_dry_run_reports_field_reason_without_writes(
    tmp_path, public_validator_sample, pipeline, out_format, case
):
    """Exercise the installed public command, including its ordinary renderer."""
    path = public_validator_sample
    payload = yaml.safe_load(path.read_text(encoding="utf-8"))
    expected_path = expected_reason = None
    if case == "bad-tstep":
        payload["model"]["control"]["tstep"] = "not an integer"
        expected_path = "model -> control -> tstep"
        expected_reason = "'not an integer' is not valid under any of the given schemas"
    elif case == "unknown-property":
        payload["sites"][0]["properties"]["unknown_extra"] = 42
        expected_path = "sites -> 0 -> properties"
        expected_reason = "Additional properties are not allowed ('unknown_extra' was unexpected)"
    if case != "valid":
        _write_yaml(path, payload)
    # Existing generated files must also remain unchanged in read-only mode.
    for name in (
        f"updated_{path.name}", f"report_{path.stem}.txt", f"report_{path.stem}.json",
    ):
        (path.parent / name).write_bytes(b"previous output: do not replace\n")
    before = _source_file_snapshot(tmp_path)
    entries_before = set(tmp_path.rglob("*"))
    result = _run_public_validation(
        path, pipeline, out_format, tmp_path, dry_run=True,
    )
    valid = case == "valid"
    assert result.returncode == (0 if valid else 1), result.stdout + result.stderr
    assert "Traceback" not in result.stderr + result.stdout
    assert "TypeError" not in result.stderr + result.stdout
    assert set(tmp_path.rglob("*")) == entries_before
    assert _source_file_snapshot(tmp_path) == before

    if out_format == "json":
        envelope = json.loads(result.stdout)
        assert envelope["status"] == ("success" if valid else "error")
        assert envelope["data"]["is_valid"] is valid
        record, = envelope["data"]["files"]
        assert record["valid"] is valid
        assert record["error_count"] == len(envelope["errors"])
        if valid:
            assert envelope["errors"] == []
        else:
            # Independent expected field and reason, not just a non-empty error
            # or parity with another output produced by the same implementation.
            assert any(
                issue["schema_path"] == expected_path
                and issue["message"] == expected_reason
                and issue["code_name"] == "INVALID_VALUE"
                and issue["severity"] == "ERROR"
                for issue in envelope["errors"]
            ), envelope["errors"]
    else:
        rendered = re.sub(r"\s+", " ", result.stdout.replace("│", " "))
        assert f"Summary: {1 if valid else 0}/1 files valid" in rendered
        assert ("Valid" if valid else "Invalid") in rendered
        if not valid:
            assert f"{expected_path}: {expected_reason}" in rendered
            assert "'code':" not in rendered
            assert "ValidationError object" not in rendered
            if case == "bad-tstep":
                assert "[type=int_parsing]" in rendered


@pytest.mark.parametrize("pipeline", ["C", "ABC"])
@pytest.mark.parametrize("out_format", [pytest.param(None, id="default"), "json"])
def test_public_validate_normal_sample_preserves_sources(
    tmp_path, public_validator_sample, pipeline, out_format
):
    """The dry-run control must also be valid with real config-relative forcing."""
    path = public_validator_sample
    before = _source_file_snapshot(tmp_path)
    result = _run_public_validation(path, pipeline, out_format, tmp_path)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "Traceback" not in result.stdout + result.stderr
    after = _source_file_snapshot(tmp_path)
    assert all(after.get(name) == content for name, content in before.items())
    report_path = path.parent / f"report_{path.stem}.txt"
    sidecar_path = report_path.with_suffix(".json")
    updated_path = path.parent / f"updated_{path.name}"
    assert "Validation stopped at:" not in report_path.read_text(encoding="utf-8")
    report = json.loads(sidecar_path.read_text(encoding="utf-8"))
    assert [phase["phase"] for phase in report["phases"]] == list(pipeline)
    assert all(phase["status"] != "FAILED" for phase in report["phases"])
    updated = yaml.safe_load(updated_path.read_text(encoding="utf-8"))
    tstep = updated["model"]["control"]["tstep"]
    assert (tstep["value"] if isinstance(tstep, dict) else tstep) == 300
    if out_format == "json":
        envelope = json.loads(result.stdout)
        # Phase B reports the sample's zero-area evergreen surface as an advisory.
        if "B" in pipeline:
            assert envelope["status"] == "warning"
            assert any(
                warning["phase"] == "B"
                and warning["code"] == "LAND_COVER.LAND_COVER_EVETR"
                for warning in envelope["warnings"]
            )
        else:
            assert envelope["status"] == "success"
            assert envelope["warnings"] == []
        assert envelope["errors"] == []
        assert envelope["data"]["phases_run"] == list(pipeline)
        assert envelope["data"]["validation_report"] == report
    else:
        assert "Validation completed" in result.stdout


# ---------------------------------------------------------------------------
# Report consolidation regressions — gh#1466 / gh#1458
# ---------------------------------------------------------------------------


def _copy_sample_data(dst: Path) -> Path:
    """Copy the bundled sample config and its forcing file into ``dst``.

    The forcing file must sit beside the config so Phase B forcing
    validation resolves it and the science checks run end-to-end. Returns
    the path to the copied ``sample_config.yml``.
    """
    sample_dir = files("supy").joinpath("sample_data")
    if not sample_dir.joinpath("sample_config.yml").is_file():
        pytest.skip("Sample data not available")
    for resource in sample_dir.iterdir():
        if resource.is_file():
            (dst / resource.name).write_bytes(resource.read_bytes())
    return dst / "sample_config.yml"


def test_missing_schema_version_keeps_review_and_suggestion_sections(
    tmp_path: Path,
) -> None:
    """A YAML missing ``schema_version`` must still surface the Phase B
    ``REVIEW ADVISED`` / ``SUGGESTED UPDATES`` sections — gh#1466 / gh#1458.

    Root cause: Phase C's INFO consolidation short-circuited to an
    INFO-only report whenever it detected *any* default value. A missing
    ``schema_version`` is the common trigger, so the upstream phase
    content carried in ``no_action_messages`` (Phase A renames, Phase B
    review/suggestion sections) was silently dropped — the report
    collapsed to a single ``## INFO`` line about the missing field.
    """
    from supy.cmd.validate_config import cli as validate_cli

    config_path = _copy_sample_data(tmp_path)

    # Strip the ``schema_version`` line textually so the rest of the
    # bundled config is byte-for-byte unchanged (no YAML round-trip).
    original = config_path.read_text(encoding="utf-8")
    stripped = "".join(
        line
        for line in original.splitlines(keepends=True)
        if not line.startswith("schema_version")
    )
    assert stripped != original, "fixture must contain a schema_version line"
    config_path.write_text(stripped, encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--mode", "dev", str(config_path)])
    assert result.exit_code in {0, 1}, result.output

    report_path = tmp_path / "report_sample_config.txt"
    assert report_path.exists(), result.output
    report = report_path.read_text(encoding="utf-8")

    # The missing-schema_version INFO is expected to be reported ...
    assert "schema_version" in report, report
    # ... but it must NOT have replaced the upstream phase sections.
    assert "## REVIEW ADVISED" in report, report
    assert "## SUGGESTED UPDATES" in report, report


# ---------------------------------------------------------------------------
# Validation edge cases — CLI path
# ---------------------------------------------------------------------------


def _mutate_negative(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, -0.1)


def _mutate_sentinel(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, -999.0)


def _mutate_nan(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, float("nan"))


@pytest.mark.parametrize(
    "case_name, mutator",
    [
        ("negative", _mutate_negative),
        ("sentinel_minus_999", _mutate_sentinel),
        ("nan", _mutate_nan),
    ],
)
def test_validate_invalid_sfr_returns_error_envelope(
    tmp_path: Path,
    case_name: str,
    mutator: Callable[[Dict[str, Any]], None],
) -> None:
    """Each invalid-input pattern must produce a structured error envelope.

    Per ``patterns.md`` (Validation Edge Cases): cover negative, ``-999``
    sentinel, and ``NaN`` — separately, with one regression per pattern.
    """
    from supy.cmd.validate_config import cli as validate_cli

    payload = _load_sample_dict()
    mutator(payload)
    config_path = tmp_path / f"sample_{case_name}.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--dry-run",
            "--format",
            "json",
            str(config_path),
        ],
    )
    # Exit non-zero on invalid; envelope still parsable on stdout.
    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"], "expected non-empty errors array"
    # At least one error must surface a schema path and a hint.
    assert any(
        err.get("schema_path") and err.get("hint") for err in envelope["errors"]
    ), envelope["errors"]


# ---------------------------------------------------------------------------
# Validation edge cases — library function path
# ---------------------------------------------------------------------------


def test_validate_single_file_rejects_negative_sfr(tmp_path: Path) -> None:
    """``validate_single_file`` (the library API) must reject negative ``sfr``.

    This is the second public path the patterns rule asks us to cover —
    if the modern CLI ever stops emitting envelopes, the underlying
    validator still has its own regression here.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema

    payload = _load_sample_dict()
    _mutate_negative(payload)
    config_path = tmp_path / "sample_negative.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    assert not is_valid
    assert errors, "expected at least one error for a negative sfr"


def test_validate_single_file_rejects_storage_value_plus_nested_qf(
    tmp_path: Path,
) -> None:
    """Malformed storage-heat family values must not be partially folded."""
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema

    payload = _load_sample_dict()
    payload["model"]["physics"]["storage_heat"] = {
        "ohm": {"value": 1, "include_qf": False}
    }
    config_path = tmp_path / "bad_storage_heat.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    assert not is_valid
    assert any("storage_heat.ohm" in error.message for error in errors), errors


# ---------------------------------------------------------------------------
# Critical-physics structural-presence — gh#1409
# ---------------------------------------------------------------------------


def _minimal_paved_only_config(schema_version: str) -> Dict[str, Any]:
    """Build a minimal YAML that passes schema and Pydantic but lacks the
    required ``model.physics`` keys.

    Uses ``land_cover.paved.sfr=1.0`` with its required soil parameters so
    the Pydantic ``model_validator`` does not reject the payload via
    site-level critical-null checks. This isolates the structural-presence
    gap closed in gh#1409.
    """
    return {
        "schema_version": schema_version,
        "name": "gh1409-regression",
        "description": "missing model.physics keys",
        "model": {
            "control": {
                "tstep": 300,
                "forcing_file": "forcing.txt",
                "output_file": "out.txt",
                "start_time": "2020-01-01",
                "end_time": "2020-01-31",
            },
            "physics": {},
        },
        "sites": [
            {
                "name": "regression",
                "gridiv": 1,
                "properties": {
                    "lat": {"value": 33.45},
                    "lng": {"value": -112.07},
                    "alt": {"value": 331},
                    "timezone": {"value": -7},
                    "surfacearea": {"value": 1.0},
                    "land_cover": {
                        "paved": {
                            "sfr": {"value": 1.0},
                            "soil_depth": {"value": 350.0},
                            "soil_store_capacity": {"value": 150.0},
                            "saturated_hydraulic_conductivity": {"value": 0.0005},
                        },
                        "bldgs": {"sfr": {"value": 0.0}},
                        "evetr": {"sfr": {"value": 0.0}},
                        "dectr": {"sfr": {"value": 0.0}},
                        "grass": {"sfr": {"value": 0.0}},
                        "bsoil": {"sfr": {"value": 0.0}},
                        "water": {"sfr": {"value": 0.0}},
                    },
                },
            }
        ],
    }


def test_validate_flags_missing_critical_physics_params(tmp_path: Path) -> None:
    """A YAML with ``model.physics: {}`` must fail with one finding per
    missing critical physics field.

    Regression for gh#1409: Pydantic auto-fills ``ModelPhysics`` enum
    defaults so ``model.physics: {}`` previously passed the dry-run
    JSON path with ``status="success"``. The MCP wrapper consumed the
    false-success envelope and stopped iterating. The structural-presence
    check in ``validate_single_file`` now flags every missing
    ``CRITICAL_PHYSICS_PARAMS`` entry as ``MISSING_REQUIRED_FIELD`` so
    the dry-run path agrees with the full pipeline's Phase A on
    user-facing semantics.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION
    from supy.data_model.validation.pipeline.orchestrator import (
        CRITICAL_PHYSICS_PARAMS,
    )
    from supy.data_model.core.field_renames import STEBBS_PHYSICS_LEAF_RENAMES

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    config_path = tmp_path / "missing_physics.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    assert not is_valid

    flagged = {
        getattr(err, "field", None)
        for err in errors
        if getattr(err, "field", "").startswith("model.physics.")
    }
    # gh#1456: STEBBS critical leaves are reported under the nested
    # model.physics.stebbs.* path; the remaining physics params stay flat.
    stebbs_leaf_names = set(STEBBS_PHYSICS_LEAF_RENAMES.values())
    expected = {
        (
            f"model.physics.stebbs.{name}"
            if name in stebbs_leaf_names
            else f"model.physics.{name}"
        )
        for name in CRITICAL_PHYSICS_PARAMS
    }
    assert flagged == expected, (
        f"missing fields not flagged: {expected - flagged}; "
        f"unexpected fields flagged: {flagged - expected}"
    )


def test_validate_flat_stebbs_form_not_flagged_missing(tmp_path: Path) -> None:
    """gh#1456 regression: a current-target YAML in the legacy FLAT form.

    The relocated STEBBS leaves (``capacitance``/``setpoint``/``same_*``)
    may still be written flat under ``model.physics`` alongside a flat
    ``stebbs: {value: 1}`` master toggle. ``SUEWSConfig.from_yaml`` folds
    those flat keys to the nested ``stebbs`` block and accepts them, so
    the dry-run critical-physics check must too. Before the fix it looked
    for the leaves only inside ``physics["stebbs"]`` and false-reported
    them missing.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    # Populate every required family switch plus the relocated STEBBS leaves
    # in the FLAT form (directly under model.physics, with a flat master
    # toggle). This is a still-accepted shape that the loader folds.
    payload["model"]["physics"] = {
        "net_radiation": {"value": 3},
        "emissions": {"value": 2},
        "storage_heat": {"value": 1},
        "ohm_inc_qf": {"value": 0},
        "roughness_length_momentum": {"value": 2},
        "roughness_length_heat": {"value": 2},
        "stability": {"value": 3},
        "soil_moisture_deficit": {"value": 0},
        "water_use": {"value": 0},
        "roughness_sublayer": {"value": 1},
        "frontal_area_index": {"value": 0},
        "roughness_sublayer_level": {"value": 0},
        "surface_conductance": {"value": 1},
        "snow_use": {"value": 0},
        # Flat STEBBS master toggle + relocated leaves (legacy flat form).
        "stebbs": {"value": 1},
        "capacitance": {"value": 1},
        "setpoint": {"value": 0},
        "same_albedo_wall": {"value": 1},
        "same_albedo_roof": {"value": 1},
        "same_emissivity_wall": {"value": 1},
        "same_emissivity_roof": {"value": 1},
    }
    config_path = tmp_path / "flat_stebbs.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    _is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    # The relocated leaves must NOT appear as missing critical physics
    # parameters now that the dry-run path folds the flat form first.
    missing_stebbs = [
        field
        for err in errors
        for field in [getattr(err, "field", "") or ""]
        if field.startswith("model.physics.stebbs.")
        and "missing" in (getattr(err, "message", "") or "").lower()
    ]
    assert not missing_stebbs, (
        f"flat-form STEBBS leaves false-flagged missing: {missing_stebbs}"
    )
    # And none of the required family switches should be missing either.
    missing_flat = [
        field
        for err in errors
        for field in [getattr(err, "field", "") or ""]
        if field.startswith("model.physics.")
        and not field.startswith("model.physics.stebbs.")
        and "required physics parameter is missing"
        in (getattr(err, "message", "") or "")
    ]
    assert not missing_flat, (
        f"flat-form physics switches false-flagged missing: {missing_flat}"
    )


def test_validate_pipeline_cleans_legacy_flat_stebbs_updated_yaml(
    tmp_path: Path,
) -> None:
    """gh#1497: full validate output folds old flat STEBBS switches."""
    from supy.cmd.validate_config import cli as validate_cli

    payload = _load_sample_dict()
    physics = payload["model"]["physics"]
    physics["stebbs"] = {"value": 0}
    physics["outer_cap_fraction"] = {"value": 0}
    physics["setpoint"] = {"value": 1}
    physics["same_albedo_wall"] = {"value": 0}
    physics["same_albedo_roof"] = {"value": 0}
    physics["same_emissivity_wall"] = {"value": 0}
    physics["same_emissivity_roof"] = {"value": 0}

    config_path = tmp_path / "legacy_stebbs.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])

    assert result.exit_code == 0, result.output
    updated_path = tmp_path / "updated_legacy_stebbs.yml"
    assert updated_path.exists(), result.output
    updated = yaml.safe_load(updated_path.read_text(encoding="utf-8"))
    updated_physics = updated["model"]["physics"]
    updated_stebbs = updated_physics["stebbs"]

    assert "value" not in updated_stebbs
    for flat_key in (
        "capacitance",
        "outer_cap_fraction",
        "setpoint",
        "same_albedo_wall",
        "same_albedo_roof",
        "same_emissivity_wall",
        "same_emissivity_roof",
    ):
        assert flat_key not in updated_physics

    assert updated_stebbs["enabled"] == {"value": False}
    assert updated_stebbs["parameters"] == {"value": 1}
    assert updated_stebbs["capacitance"] == {"value": 0}
    assert updated_stebbs["setpoint"] == {"value": 1}
    assert updated_stebbs["same_albedo_wall"] == {"value": 0}
    assert updated_stebbs["same_albedo_roof"] == {"value": 0}
    assert updated_stebbs["same_emissivity_wall"] == {"value": 0}
    assert updated_stebbs["same_emissivity_roof"] == {"value": 0}


def test_validate_full_pipeline_emits_json_envelope(tmp_path: Path) -> None:
    """The non-dry-run pipeline path must honour ``--format json`` and
    emit a canonical envelope on stdout (gh#1409 follow-up).

    Previously only ``--dry-run --format json`` produced JSON; the
    full pipeline branch wrote a report file and printed a status
    banner to console regardless of the format flag, so any consumer
    that expected JSON had to parse the report file or fall back to
    the dry-run path.
    """
    from supy.cmd.validate_config import cli as validate_cli

    # Reuse the paved-only YAML from the gh#1409 reproducer — bad
    # enough to fail Phase A so we exercise both the failure path and
    # the JSON emit.
    payload = _minimal_paved_only_config("2026.5.dev9")
    config_path = tmp_path / "full_pipeline.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--format",
            "json",
            str(config_path),
        ],
    )
    # Bad config → non-zero exit, but stdout must still parse as the
    # canonical envelope.
    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert set(envelope.keys()) == {"status", "data", "errors", "warnings", "meta"}

    data = envelope["data"]
    assert "validation_report" in data, (
        "full-pipeline JSON envelope must carry the structured ValidationReport"
    )
    assert data["validation_report"]["overall_status"] == "FAILED"
    assert "report_file" in data and "updated_yaml" in data
    # phases_run records which phases actually executed; for a Phase-A
    # failure the list contains just "A" because the pipeline bails
    # before B/C.
    assert isinstance(data["phases_run"], list)
    assert "A" in data["phases_run"]


@pytest.mark.parametrize("failed_phase", ["A", "B"])
@pytest.mark.parametrize("mode", ["public", "dev"])
def test_validate_abc_failed_phase_envelope(tmp_path, failed_phase, mode):
    """Real early rejections preserve their phase, diagnosis and consolidated sidecar."""
    from supy.cmd.suews_cli import cli

    _copy_sample_data(tmp_path)
    path = tmp_path / "sample_config.yml"
    payload = _load_sample_dict()
    if failed_phase == "A":
        payload["model"]["physics"].pop("net_radiation")
    else:
        _set_first_paved_sfr(payload, -0.1)
    _write_yaml(path, payload)
    result = CliRunner().invoke(cli, [
        "validate", str(path), "-p", "ABC", "--mode", mode, "--format", "json",
    ])

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"]
    data = envelope["data"]
    assert data["phases_run"] == (["A"] if failed_phase == "A" else ["A", "B"])
    report = data["validation_report"]
    assert report["overall_status"] == "FAILED"
    assert report["phases"][-1]["status"] == "FAILED"
    assert json.loads(Path(data["report_file"]).with_suffix(".json").read_text(encoding="utf-8")) == report
    text = Path(data["report_file"]).read_text(encoding="utf-8")
    assert ("net_radiation" if failed_phase == "A" else "sfr") in text


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("content", ["model: [unterminated\n", "model: null\n"])
def test_validate_abc_malformed_input_envelope(tmp_path, content, mode):
    """ABC retains the genuine startup or Phase A diagnosis for malformed inputs."""
    from supy.cmd.suews_cli import cli

    path = tmp_path / "malformed.yml"
    path.write_text(content, encoding="utf-8")
    result = CliRunner().invoke(cli, [
        "validate", str(path), "-p", "ABC", "--mode", mode, "--format", "json",
    ])

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    if mode == "public" and "unterminated" in content:
        # The public feature gate already has a documented YAML-read envelope.
        assert envelope["errors"][0]["code"] == "yaml_read_failed"
        assert "Error reading YAML" in envelope["errors"][0]["message"]
        assert not (tmp_path / "updated_malformed.yml").exists()
        return
    data = envelope["data"]
    assert data["phases_run"] == ["A"]
    phase = data["validation_report"]["phases"][0]
    assert phase["status"] == "FAILED"
    assert phase["yaml_out"] is None
    text = Path(data["report_file"]).read_text(encoding="utf-8")
    assert "Validation stopped at: Completeness Check" in text
    assert all(issue["message"] in text for issue in phase["issues"])


def test_validate_full_pipeline_experimental_restriction_emits_json_envelope(
    tmp_path: Path,
) -> None:
    """Early public-mode restrictions must still honour ``--format json``.

    The restriction check runs before PhaseReport objects exist, so it
    needs its own envelope path rather than printing Rich text.
    """
    from supy.cmd.validate_config import cli as validate_cli
    from supy.data_model.configuration.version import CURRENT_SCHEMA_VERSION

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    payload["model"]["physics"] = {"stebbs": {"value": 1}}
    config_path = tmp_path / "experimental.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"][0]["code"] == "experimental_features_restricted"
    # gh#1456: master toggle is now model.physics.stebbs.enabled; the
    # restriction message reflects the new nested surface.
    assert "STEBBS is enabled" in envelope["data"]["restrictions"][0]


def test_public_mode_restriction_honours_nested_stebbs_bool_strings(
    tmp_path: Path,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {"model": {"physics": {"stebbs": {"enabled": {"value": "off"}}}}}
    config_path = tmp_path / "public-ok.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is True
    assert restrictions == []
    assert read_error is None

    payload["model"]["physics"]["stebbs"]["enabled"]["value"] = "yes"
    _write_yaml(config_path, payload)
    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "STEBBS is enabled" in restrictions[0]
    assert read_error is None


@pytest.mark.parametrize("legacy_key", ["stebbsmethod", "stebbs_method"])
def test_public_mode_restriction_honours_legacy_stebbs_aliases(
    tmp_path: Path,
    legacy_key: str,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {"model": {"physics": {legacy_key: {"value": 1}}}}
    config_path = tmp_path / "public-legacy.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "STEBBS is enabled" in restrictions[0]
    assert read_error is None

    payload["model"]["physics"][legacy_key]["value"] = 0
    _write_yaml(config_path, payload)
    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is True
    assert restrictions == []
    assert read_error is None


@pytest.mark.parametrize("legacy_key", ["stebbsmethod", "stebbs_method"])
def test_public_mode_restriction_rejects_mixed_stebbs_master_aliases(
    tmp_path: Path,
    legacy_key: str,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {
        "model": {
            "physics": {
                "stebbs": {"enabled": {"value": False}},
                legacy_key: {"value": 1},
            }
        }
    }
    config_path = tmp_path / "public-mixed-master.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "Invalid STEBBS physics configuration" in restrictions[0]
    assert legacy_key in restrictions[0]
    assert read_error is None


def test_public_mode_restriction_rejects_duplicate_legacy_stebbs_aliases(
    tmp_path: Path,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {
        "model": {
            "physics": {
                "stebbsmethod": {"value": 0},
                "stebbs_method": {"value": 1},
            }
        }
    }
    config_path = tmp_path / "public-duplicate-master.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "Invalid STEBBS physics configuration" in restrictions[0]
    assert "stebbsmethod" in restrictions[0]
    assert "stebbs_method" in restrictions[0]
    assert read_error is None


def test_public_mode_restriction_rejects_duplicate_nested_stebbs_aliases(
    tmp_path: Path,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {
        "model": {
            "physics": {
                "stebbs": {
                    "enabled": {"value": False},
                    "capacitance": {"value": 1},
                    "outer_cap_fraction": {"value": 2},
                }
            }
        }
    }
    config_path = tmp_path / "public-duplicate-nested.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "Invalid STEBBS physics configuration" in restrictions[0]
    assert "outer_cap_fraction" in restrictions[0]
    assert "capacitance" in restrictions[0]
    assert read_error is None


def test_emit_pipeline_result_warning_status_matches_inner_report(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Warning-only phase reports should surface as a warning envelope."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_WARNING,
    )

    phase = PhaseReport(
        phase="A",
        issues=[
            Issue(
                phase="A",
                severity=SEVERITY_WARNING,
                code="A.TEST.WARNING",
                message="warning message",
                yaml_path="model.physics",
            )
        ],
    )

    exit_code = _emit_pipeline_result(
        phases=[phase],
        report_path=tmp_path / "report.txt",
        yaml_path=tmp_path / "updated.yml",
        out_format="json",
        command="suews validate --format json",
        started_at="2026-05-15T00:00:00Z",
    )

    assert exit_code == 0
    envelope = json.loads(capsys.readouterr().out)
    assert envelope["status"] == "warning"
    assert envelope["errors"] == []
    assert envelope["warnings"][0]["code"] == "A.TEST.WARNING"
    assert envelope["data"]["validation_report"]["overall_status"] == "WARNING"


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize(
    "pipeline, failed_phase",
    [
        ("A", "A"),
        ("B", "B"),
        ("C", "C"),
        ("AB", "A"),
        ("AB", "B"),
        ("AC", "A"),
        ("AC", "C"),
        ("BC", "B"),
        ("BC", "C"),
        ("ABC", "A"),
        ("ABC", "B"),
        ("ABC", "C"),
    ],
)
def test_final_text_report_matches_failed_structured_phase(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
    mode: str,
    pipeline: str,
    failed_phase: str,
) -> None:
    """Every pipeline selection derives its final header from PhaseReport."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_ERROR,
    )
    from supy.data_model.validation.pipeline.report_writer import (
        VALIDATION_PHASE_NAMES,
    )

    phases = []
    for phase in pipeline:
        issues = []
        if phase == failed_phase:
            issues.append(
                Issue(
                    phase=phase,
                    severity=SEVERITY_ERROR,
                    code=f"{phase}.TEST.ERROR",
                    message="blocking error",
                )
            )
        phases.append(PhaseReport(phase=phase, issues=issues))
        if issues:
            break

    report_path = tmp_path / f"report_{pipeline}_{failed_phase}_{mode}.txt"
    report_path.write_text(
        "# SUEWS Validation Report\n"
        "# ==================================================\n"
        f"# Mode: {'Public' if mode == 'public' else 'Developer'}\n"
        "# ==================================================\n\n"
        "## ACTION NEEDED\n"
        "- Fix the blocking error.\n",
        encoding="utf-8",
    )

    exit_code = _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=tmp_path / "updated.yml",
        out_format="table",
        command=f"suews validate --pipeline {pipeline}",
        started_at="2026-08-19T00:00:00Z",
    )
    capsys.readouterr()

    assert exit_code == 1
    text = report_path.read_text(encoding="utf-8")
    expected_name = VALIDATION_PHASE_NAMES[failed_phase]
    assert f"# Validation stopped at: {expected_name}" in text
    assert text.count("# Validation stopped at:") == 1
    assert f"# Mode: {'Public' if mode == 'public' else 'Developer'}" in text

    payload = json.loads(
        report_path.with_suffix(".json").read_text(encoding="utf-8")
    )
    failed_entries = [
        phase for phase in payload["phases"] if phase["status"] == "FAILED"
    ]
    assert [phase["phase"] for phase in failed_entries] == [failed_phase]
    assert expected_name == VALIDATION_PHASE_NAMES[failed_entries[0]["phase"]]


@pytest.mark.parametrize("pipeline", ["A", "B", "C", "AB", "AC", "BC", "ABC"])
@pytest.mark.parametrize("outcome", ["success", "warning"])
def test_completed_pipeline_reports_omit_stopping_phase(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
    pipeline: str,
    outcome: str,
) -> None:
    """Successful and warning-only single/combined reports do not say stopped."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_WARNING,
    )

    phases = [PhaseReport(phase=phase, issues=[]) for phase in pipeline]
    if outcome == "warning":
        phase = phases[-1].phase
        phases[-1].issues.append(
            Issue(
                phase=phase,
                severity=SEVERITY_WARNING,
                code=f"{phase}.TEST.WARNING",
                message="review this",
            )
        )

    report_path = tmp_path / f"report_{pipeline}_{outcome}.txt"
    report_path.write_text(
        "# SUEWS Validation Report\n"
        "# ==================================================\n"
        "# Mode: Public\n"
        "# ==================================================\n\n"
        "## INFO\n"
        "- Validation completed.\n",
        encoding="utf-8",
    )

    exit_code = _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=tmp_path / "updated.yml",
        out_format="table",
        command=f"suews validate --pipeline {pipeline}",
        started_at="2026-08-19T00:00:00Z",
    )
    capsys.readouterr()

    assert exit_code == 0
    assert "Validation stopped at:" not in report_path.read_text(encoding="utf-8")
    payload = json.loads(
        report_path.with_suffix(".json").read_text(encoding="utf-8")
    )
    assert payload["overall_status"] == (
        "WARNING" if outcome == "warning" else "PASSED"
    )


def test_pre_pipeline_input_error_does_not_claim_stopping_phase(tmp_path: Path) -> None:
    """A Click input-path failure occurs before any A/B/C result exists."""
    from supy.cmd.validate_config import cli as validate_cli

    result = CliRunner().invoke(validate_cli, [str(tmp_path / "missing.yml")])

    assert result.exit_code == 2
    assert "Validation stopped at:" not in result.output


def test_validate_passes_when_critical_physics_present(tmp_path: Path) -> None:
    """The bundled sample config declares every critical physics key, so
    the structural-presence check must not fire.

    Guards against over-eager flagging that would break the happy path.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.configuration.publisher import generate_json_schema

    schema = generate_json_schema()
    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")
        is_valid, errors = validate_single_file(sample, schema, show_details=True)

    physics_findings = [
        err
        for err in errors
        if getattr(err, "field", "").startswith("model.physics.")
    ]
    assert is_valid, f"sample config should be valid; got errors: {errors}"
    assert physics_findings == [], (
        "sample config should not trigger physics structural-presence findings"
    )


def test_validate_sidecar_includes_non_error_info_messages(tmp_path: Path) -> None:
    """gh#1467: the consolidated CLI JSON sidecar carries non-error issues.

    Dropping the genuinely optional ``emission_factor_co2_fuel`` (inactive under
    the sample config's non-biogenic emissions method) makes the run succeed
    (exit 0) while Phase A records a non-error ``A.MISSING_PARAM`` warning.
    That informational message must appear in the JSON sidecar, not only in the
    text report. Anchored on this deterministic signal (the sample config's
    Phase B issues are environment-dependent; gh#2087 stopped Phase A from
    null-filling defaulted non-optional fields such as ``h_std``).
    """
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
        "emission_factor_co2_fuel"
    )

    config_path = tmp_path / "myconfig.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    sidecar = tmp_path / "report_myconfig.json"
    assert sidecar.exists(), "sidecar JSON not written"
    payload = json.loads(sidecar.read_text(encoding="utf-8"))

    # Consolidated sidecar is a multi-phase ValidationReport (gh#1467),
    # not a single Phase-C PhaseReport.
    assert "overall_status" in payload, payload.keys()
    assert isinstance(payload.get("phases"), list)
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}

    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(
        i["code"] == "A.MISSING_PARAM"
        and "emission_factor_co2_fuel" in (i.get("yaml_path") or "")
        and i["severity"] != "ERROR"
        for i in all_issues
    ), f"emission_factor_co2_fuel informational issue missing from sidecar: {all_issues}"

    # gh#1467: the consolidated sidecar must not publish paths to files that
    # no longer exist. In a multi-phase run the intermediate temp reports and
    # updated*_ YAMLs are merged/deleted, so every path the sidecar advertises
    # must resolve, and report paths point at the surviving consolidated pair.
    assert "temp_report" not in sidecar.read_text(encoding="utf-8")
    for p in payload["phases"]:
        assert Path(p["text_report_path"]).name == "report_myconfig.txt"
        assert Path(p["json_report_path"]).name == "report_myconfig.json"
        for key in ("text_report_path", "json_report_path", "yaml_in", "yaml_out"):
            val = p.get(key)
            assert val is None or Path(val).exists(), (
                f"sidecar phase {p['phase']} {key}={val} does not exist"
            )


def test_validate_sidecar_is_validation_report_for_clean_config(tmp_path: Path) -> None:
    """The consolidated CLI sidecar is a multi-phase ValidationReport even
    for the bundled sample config (which already carries Phase B notes)."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        config_path = tmp_path / "clean.yml"
        config_path.write_text(sample.read_text(encoding="utf-8"), encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    payload = json.loads((tmp_path / "report_clean.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] in {"PASSED", "WARNING"}
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}
    # Each phase entry preserves the PhaseReport shape.
    for phase_entry in payload["phases"]:
        assert {"phase", "status", "issues"} <= set(phase_entry)


def test_validate_sidecar_keeps_info_alongside_errors(tmp_path: Path) -> None:
    """A failing config keeps Phase A informational issues beside Phase C
    errors in the same consolidated sidecar."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
        "emission_factor_co2_fuel"
    )  # -> A.MISSING_PARAM (info)
    data["sites"][0]["properties"]["bogus_extra_param"] = 42  # -> C.PYDANTIC error

    config_path = tmp_path / "bad.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code != 0, result.output

    payload = json.loads((tmp_path / "report_bad.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(i["code"].startswith("C.PYDANTIC") for i in all_issues), all_issues
    assert any(i["code"] == "A.MISSING_PARAM" for i in all_issues), all_issues


def test_emit_pipeline_result_writes_consolidated_sidecar(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """gh#1467: `_emit_pipeline_result` writes the consolidated
    ValidationReport sidecar beside the report, in both output modes."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_INFO,
        SEVERITY_ERROR,
    )

    phases = [
        PhaseReport(
            phase="A",
            issues=[Issue(phase="A", severity=SEVERITY_INFO, code="A.INFO.NOTE",
                          message="informational note", yaml_path="model.physics")],
        ),
        PhaseReport(
            phase="C",
            issues=[Issue(phase="C", severity=SEVERITY_ERROR, code="C.PYDANTIC.X",
                          message="bad value", yaml_path="sites.1.properties.lat")],
        ),
    ]
    report_path = tmp_path / "report_x.txt"

    exit_code = _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=tmp_path / "updated_x.yml",
        out_format="table",
        command="suews validate",
        started_at="2026-05-30T00:00:00Z",
    )
    assert exit_code == 1  # has an ERROR

    sidecar = tmp_path / "report_x.json"
    assert sidecar.exists()
    payload = json.loads(sidecar.read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    assert {p["phase"] for p in payload["phases"]} == {"A", "C"}
    info_codes = [i["code"] for p in payload["phases"] for i in p["issues"]]
    assert "A.INFO.NOTE" in info_codes  # non-error info survived
    assert "C.PYDANTIC.X" in info_codes
    capsys.readouterr()  # drain captured table output


def test_emit_pipeline_result_clears_missing_phase_yaml_paths(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """gh#1467: a failed phase's missing yaml_out is cleared, not repointed to
    another phase's surviving file (and intermediate paths never dangle)."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_ERROR,
    )

    existing_yaml = tmp_path / "updated_x.yml"
    existing_yaml.write_text("name: x\n", encoding="utf-8")
    report_path = tmp_path / "report_x.txt"

    phases = [
        # Successful Phase A whose intermediate output was deleted on consolidation.
        PhaseReport(
            phase="A",
            issues=[],
            yaml_in=str(tmp_path / "missing_in.yml"),
            yaml_out=str(tmp_path / "updatedA_x.yml"),
        ),
        # Failed Phase B that never produced an output YAML.
        PhaseReport(
            phase="B",
            issues=[Issue(phase="B", severity=SEVERITY_ERROR, code="B.X", message="boom")],
            yaml_out=str(tmp_path / "updatedB_x.yml"),
        ),
    ]

    _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=existing_yaml,
        out_format="table",
        command="suews validate",
        started_at="2026-05-31T00:00:00Z",
    )
    capsys.readouterr()

    payload = json.loads((tmp_path / "report_x.json").read_text(encoding="utf-8"))
    by_phase = {p["phase"]: p for p in payload["phases"]}
    # Vanished intermediate/failed YAML paths are cleared, not repointed.
    assert by_phase["A"]["yaml_in"] is None
    assert by_phase["A"]["yaml_out"] is None
    assert by_phase["B"]["yaml_out"] is None
    # No phase falsely claims the surviving final YAML as its own output.
    assert all(p["yaml_out"] != str(existing_yaml) for p in payload["phases"])


# ---------------------------------------------------------------------------
# Runtime forcing: explicit null forcing.file (gh#1722)
# ---------------------------------------------------------------------------

_SAMPLE_FORCING_ENTRY = "      file:\n        value: Kc_2012_data_60.txt\n"


def _forcing_issues(tmp_path: Path) -> list:
    payload = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    return [
        issue
        for phase in payload["phases"]
        for issue in phase["issues"]
        if issue["category"] == "FORCING"
    ]


@pytest.mark.parametrize(
    ("replacement", "exit_code", "expected"),
    [
        ("      file: null\n", 0, [("A.FORCING.RUNTIME", "WARNING")]),
        ("", 1, [("A.FORCING", "ERROR")]),
    ],
    ids=["explicit-null", "absent-key"],
)
def test_validate_forcing_file_null_is_a_warning(
    tmp_path: Path, replacement: str, exit_code: int, expected: list
) -> None:
    """``forcing.file: null`` validates with a warning; an absent key fails."""
    from supy.cmd.validate_config import cli as validate_cli

    config_path = _copy_sample_data(tmp_path)
    original = config_path.read_text(encoding="utf-8")
    assert original.count(_SAMPLE_FORCING_ENTRY) == 1, "fixture forcing entry moved"
    config_path.write_text(
        original.replace(_SAMPLE_FORCING_ENTRY, replacement), encoding="utf-8"
    )

    result = CliRunner().invoke(validate_cli, [str(config_path)])

    assert result.exit_code == exit_code, result.output
    found = [(i["code"], i["severity"]) for i in _forcing_issues(tmp_path)]
    assert found == expected, found


# ---------------------------------------------------------------------------
# model.control.start_time / end_time: timestamps and forcing coverage
# ---------------------------------------------------------------------------


def _validate_sample_with_bounds(
    tmp_path: Path, start_time: str, end_time: str
) -> Dict[str, Any]:
    """Run the full pipeline on the sample with new bounds; return the report."""
    from supy.cmd.validate_config import cli as validate_cli

    config_path = _copy_sample_data(tmp_path)
    payload = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    payload["model"]["control"]["start_time"] = start_time
    payload["model"]["control"]["end_time"] = end_time
    _write_yaml(config_path, payload)

    result = CliRunner().invoke(
        validate_cli, ["--pipeline", "ABC", "--format", "json", str(config_path)]
    )
    envelope = json.loads(result.stdout)
    return envelope["data"]["validation_report"]


def _issue_messages(report: Dict[str, Any]) -> list:
    return [issue["message"] for phase in report["phases"] for issue in phase["issues"]]


def test_validate_accepts_timestamp_bounds(tmp_path: Path) -> None:
    """A ``start_time`` with a time component passes Phase B (gh#1852).

    The run accepts timestamp bounds, but Phase B parsed ``start_time`` with
    ``strptime("%Y-%m-%d")`` and failed with "unconverted data remains".
    """
    report = _validate_sample_with_bounds(
        tmp_path, "2012-01-01 01:00", "2012-12-31 23:00"
    )
    messages = _issue_messages(report)
    assert report["overall_status"] != "FAILED", messages
    assert not any("unconverted data" in message for message in messages)


def test_validate_reports_forcing_not_covering_period(tmp_path: Path) -> None:
    """``suews validate`` rejects a period the sample forcing does not cover.

    The sample forcing ends at 2013-01-01 00:00, so an ``end_time`` of
    2013-01-05 (which runs to the row stamped 2013-01-06 00:00) needs rows
    the file does not have; ``run()`` would raise.
    """
    report = _validate_sample_with_bounds(tmp_path, "2012-01-01", "2013-01-05")
    messages = _issue_messages(report)
    assert report["overall_status"] == "FAILED"
    assert any(
        "does not cover model.control.start_time/end_time" in message
        and "2013-01-06 00:00:00" in message
        for message in messages
    ), messages


def test_validate_sample_period_is_covered(tmp_path: Path) -> None:
    """The sample's date-only bounds are covered by its own forcing."""
    report = _validate_sample_with_bounds(tmp_path, "2012-01-01", "2012-12-31")
    messages = _issue_messages(report)
    assert not any("does not cover" in message for message in messages), messages


def test_default_export_survives_disabled_stebbs_science_fixes(tmp_path):
    """The default export keeps the internal occupancy fields loadable."""
    from supy.cmd.validate_config import cli as validate_cli
    from supy.data_model.core import SUEWSConfig

    data = _load_sample_dict()
    data["model"]["physics"]["stebbs"]["enabled"] = False
    config = SUEWSConfig.from_dict(data)
    path = tmp_path / "export.yml"
    config.to_yaml(str(path))

    result = CliRunner().invoke(validate_cli, [
        str(path), "--pipeline", "BC", "--science-fixes", "apply",
        "--forcing", "off", "--mode", "dev",
    ])

    assert result.exit_code == 0, result.output
    reloaded = SUEWSConfig.from_yaml(str(tmp_path / "updated_export.yml"))
    archetype = reloaded.model_dump(mode="json")["sites"][0]["properties"][
        "building_archetype"
    ]
    for name in ("hhs0", "age_0_4", "age_5_11", "age_12_18", "age_19_64", "age_65plus"):
        assert archetype[name] == 0.0


# Fields whose model rejects ``{value: null}`` (gh#2001). Omitting one gives
# its default, so the default pipeline must accept the omission and must not
# write a null placeholder for it into the updated YAML.
_NULL_REJECTING_OMISSIONS = [
    (("properties", "land_cover", "water"), "flowchange"),
    (("initial_states", "evetr"), "sdd_id"),
    (("initial_states",), "snowalb"),
]


@pytest.mark.parametrize(("path", "key"), _NULL_REJECTING_OMISSIONS)
def test_validate_accepts_omitted_null_rejecting_field(
    tmp_path: Path, path: tuple, key: str
) -> None:
    """Omitting a null-rejecting field passes ``suews validate`` (gh#2001)."""
    from supy.cmd.validate_config import cli as validate_cli
    from supy.data_model.core import SUEWSConfig

    data = _load_sample_dict()
    block = data["sites"][0]
    for part in path:
        block = block[part]
    del block[key]
    config_path = tmp_path / "myconfig.yml"
    _write_yaml(config_path, data)

    result = CliRunner().invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    updated = tmp_path / "updated_myconfig.yml"
    assert updated.exists(), result.output
    text = updated.read_text(encoding="utf-8")
    SUEWSConfig.from_yaml(str(updated))
    reloaded = yaml.safe_load(text)["sites"][0]
    for part in path:
        reloaded = reloaded[part]
    assert key not in reloaded, reloaded


def test_missing_block_gets_no_null_for_null_rejecting_field() -> None:
    """A block Phase A fills in skips the null-rejecting fields (gh#2001, gh#2087)."""
    from supy.data_model.validation.pipeline.phase_a import (
        create_clean_missing_param_annotation,
    )

    lines = create_clean_missing_param_annotation(
        "evetr",
        {"alb_id": {"value": 0.1}, "state": {"value": 0.0}, "sdd_id": {"value": 0}},
    )
    assert lines[:2] == ["evetr:", "  alb_id:"], lines
    assert not any("sdd_id" in line for line in lines), lines
    assert not any("state" in line for line in lines), lines


def test_default_export_survives_annual_mean_science_fix(tmp_path, monkeypatch):
    """A default export carries no stale ``deep_soil_temperature`` (gh#2001).

    With STEBBS enabled, ``--science-fixes apply`` rewrites
    ``temperature_air_annual_mean``; the computed deep soil temperature must
    follow it instead of surviving in the exported YAML as an input.
    """
    from supy.cmd.validate_config import cli as validate_cli
    from supy.data_model.core import SUEWSConfig
    from supy.data_model.validation.pipeline import phase_b

    data = _load_sample_dict()
    data["model"]["physics"]["stebbs"]["enabled"] = True
    data["sites"][0]["properties"]["stebbs"]["temperature_air_annual_mean"] = {
        "value": 5.0
    }
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: 13.0
    )
    path = tmp_path / "export.yml"
    SUEWSConfig.from_dict(data).to_yaml(str(path))

    result = CliRunner().invoke(validate_cli, [
        str(path), "--pipeline", "BC", "--science-fixes", "apply",
        "--forcing", "off", "--mode", "dev",
    ])

    assert result.exit_code == 0, result.output
    reloaded = SUEWSConfig.from_yaml(str(tmp_path / "updated_export.yml"))
    stebbs = reloaded.sites[0].properties.stebbs
    assert stebbs.deep_soil_temperature == 13.0


def test_snowalb_drop_survives_revalidation(tmp_path: Path) -> None:
    """An ``apply`` run that drops ``snowalb`` validates again (gh#2001).

    A summer start makes Phase B remove ``snowalb``; validating that output
    a second time must not put a null placeholder back.
    """
    from supy.cmd.validate_config import cli as validate_cli

    data = _load_sample_dict()
    data["model"]["control"]["start_time"] = "2012-07-01"
    data["model"]["control"]["end_time"] = "2012-07-31"
    config_path = tmp_path / "myconfig.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    first = runner.invoke(validate_cli, [
        str(config_path), "--science-fixes", "apply", "--forcing", "off",
    ])
    assert first.exit_code == 0, first.output
    updated = yaml.safe_load(
        (tmp_path / "updated_myconfig.yml").read_text(encoding="utf-8")
    )
    assert "snowalb" not in updated["sites"][0]["initial_states"]

    again = tmp_path / "again.yml"
    _write_yaml(again, updated)
    second = runner.invoke(validate_cli, [str(again), "--forcing", "off"])
    assert second.exit_code == 0, second.output
