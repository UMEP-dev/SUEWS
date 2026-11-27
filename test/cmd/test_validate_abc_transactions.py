"""Exercise ABC validation entry points with real phases and overlapping processes."""

import copy
from dataclasses import dataclass
from functools import lru_cache
from importlib.resources import files
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import sysconfig
import time

import pytest
import yaml

pytestmark = [
    pytest.mark.unit,
    pytest.mark.os_compat,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR", "C-ISOLATION"),
]

_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


@dataclass(frozen=True)
class _CliRunResult:
    """Subprocess-compatible result for in-process standalone Click invocations."""

    returncode: int
    stdout: str
    stderr: str


@lru_cache(maxsize=1)
def _sample_data_files() -> dict[str, bytes]:
    """Cache bundled sample_data file bytes once per process via Traversable API."""
    return {
        item.name: item.read_bytes()
        for item in files("supy").joinpath("sample_data").iterdir()
        if item.is_file()
    }


@lru_cache(maxsize=1)
def _sample_dict_template():
    return yaml.load(
        _sample_data_files()["sample_config.yml"].decode("utf-8"), Loader=_YAML_LOADER
    )


def _load_sample_dict():
    return copy.deepcopy(_sample_dict_template())


@pytest.fixture
def source(tmp_path):
    project = tmp_path / "project"
    project.mkdir()
    for name, content in _sample_data_files().items():
        (project / name).write_bytes(content)
    path = project / "sample_config.yml"
    forcing = _sample_dict_template()["model"]["control"]["forcing"]["file"]["value"]
    assert not Path(forcing).is_absolute()
    assert (project / forcing).is_file()
    return path


def _entry_point(route):
    executable = Path(sysconfig.get_path("scripts")) / (
        route + (".exe" if os.name == "nt" else "")
    )
    assert executable.is_file(), f"Missing installed command: {executable}"
    return [str(executable)] + (["validate"] if route == "suews" else [])


def _command(route, source, science, mode="public", out_format="json"):
    command = _entry_point(route)
    command += [str(source), "-p", "ABC", "--mode", mode, "--science-fixes", science]
    if out_format == "json":
        command += ["--format", "json"]
    return command


def _invoke_standalone_click(
    route,
    args,
    *,
    cwd,
    encoding="utf-8",
    env_overrides=None,
    unset_env=(),
):
    """Run ``suews`` or ``suews-validate`` Click entry point in-process in standalone mode.

    Uses ``io.TextIOWrapper(..., encoding=encoding, errors="strict")`` on
    ``sys.stdout`` and ``sys.stderr`` so strict encoding (e.g. ``cp1252``),
    Rich console width/colour settings, and Click's ``KeyboardInterrupt`` ->
    ``Aborted!`` handling match standalone CLI execution without spawning a
    subprocess.
    """
    from rich.console import Console

    from supy.cmd import validate_config as validate_config_mod
    from supy.cmd.suews_cli import cli as suews_cli

    command = validate_config_mod.cli if route == "suews-validate" else suews_cli
    argv = list(map(str, args))
    if route == "suews":
        argv = ["validate", *argv]

    out_buf = io.BytesIO()
    err_buf = io.BytesIO()
    out_stream = io.TextIOWrapper(
        out_buf, encoding=encoding, errors="strict", write_through=True
    )
    err_stream = io.TextIOWrapper(
        err_buf, encoding=encoding, errors="strict", write_through=True
    )

    old_cwd = Path.cwd()
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    old_argv = sys.argv
    old_console = validate_config_mod.console
    saved_env: dict[str, str | None] = {}
    for key in unset_env:
        saved_env[key] = os.environ.get(key)
        os.environ.pop(key, None)
    for key, value in (env_overrides or {}).items():
        if key not in saved_env:
            saved_env[key] = os.environ.get(key)
        os.environ[key] = value

    returncode = 0
    try:
        os.chdir(cwd)
        sys.stdout = out_stream
        sys.stderr = err_stream
        sys.argv = [route, *argv]
        validate_config_mod.console = Console()
        try:
            command.main(args=argv, prog_name=route, standalone_mode=True)
        except SystemExit as exc:
            if exc.code is None:
                returncode = 0
            elif isinstance(exc.code, int):
                returncode = exc.code
            else:
                returncode = 1
    finally:
        out_stream.flush()
        err_stream.flush()
        validate_config_mod.console = old_console
        sys.stdout = old_stdout
        sys.stderr = old_stderr
        sys.argv = old_argv
        os.chdir(old_cwd)
        for key, prev in saved_env.items():
            if prev is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = prev

    return _CliRunResult(
        returncode=returncode,
        stdout=out_buf.getvalue().decode(encoding),
        stderr=err_buf.getvalue().decode(encoding),
    )


def _run(route, source, science, mode="public", out_format="json", extra=()):
    args = [str(source), "-p", "ABC", "--mode", mode, "--science-fixes", science]
    if out_format == "json":
        args += ["--format", "json"]
    args += list(extra)
    return _invoke_standalone_click(
        route,
        args,
        cwd=source.parent.parent,
        env_overrides={"NO_COLOR": "1", "COLUMNS": "240"},
        unset_env=("FORCE_COLOR", "CLICOLOR_FORCE"),
    )


def _products(source):
    return {
        name: (source.parent / name).read_bytes()
        for name in (
            f"updated_{source.name}", f"report_{source.stem}.txt",
            f"report_{source.stem}.json",
        )
    }


def _snapshot(paths):
    return {
        path: (path.read_bytes(), path.stat().st_mtime_ns, path.stat().st_ctime_ns)
        for path in paths
    }


def _success(result, science):
    assert result.returncode == 0, result.stdout + result.stderr
    envelope = json.loads(result.stdout)  # Reject mixed output, not just its last JSON object.
    assert envelope["status"] == "warning"  # The real sample has Phase B advisories.
    assert envelope["errors"] == []
    assert envelope["data"]["phases_run"] == ["A", "B", "C"]
    report = envelope["data"]["validation_report"]
    assert report["overall_status"] == "WARNING"
    assert all(phase["status"] != "FAILED" for phase in report["phases"])
    issues = report["phases"][1]["issues"]
    assert any(issue["code"] == "LAND_COVER.LAND_COVER_EVETR" for issue in issues)
    assert any(issue["severity"] == "SUGGESTION" for issue in issues) == (science == "suggest")
    assert ".suews-validate-" not in result.stdout
    return report


@pytest.fixture
def hooks(tmp_path):
    """Test-only barriers/faults; the installed commands still run all real phases."""
    directory = tmp_path / "hooks"
    directory.mkdir()
    (directory / "sitecustomize.py").write_text('''
import json
import os
from pathlib import Path
import time
from supy.data_model.validation.pipeline import orchestrator as engine

control = Path(os.environ["SUEWS_TEST_CONTROL"])
role = os.environ["SUEWS_TEST_ROLE"]

def wait_for(name):
    deadline = time.monotonic() + 120
    while not (control / name).exists():
        if time.monotonic() >= deadline:
            raise TimeoutError("Test barrier not released: " + name)
        time.sleep(0.01)

original_b = engine.run_phase_b

def phase_b(*args, **kwargs):
    report = original_b(*args, **kwargs)
    # Existence signals a complete JSON payload, not an open/truncated file.
    # Each role owns its staging name; replacement stays on the same filesystem.
    ready = control / (role + ".ready")
    pending = control / (role + ".ready.tmp")
    pending.write_text(json.dumps({
        "yaml": args[3], "report": args[4],
    }), encoding="utf-8")
    os.replace(pending, ready)
    wait_for("release")
    return report

engine.run_phase_b = phase_b
original_publish = engine._publish_abc_outputs

def publish(*args, **kwargs):
    if os.environ.get("SUEWS_TEST_FAIL") == "1":
        # Ensure the failed invocation snapshots the successful concurrent
        # publication, then fails after replacing YAML and text.
        wait_for("good-published")
    original_publish(*args, **kwargs)
    (control / "good-published").touch()

engine._publish_abc_outputs = publish
original_replace = engine.os.replace

def replace(source, destination):
    if os.environ.get("SUEWS_TEST_FAIL") == "1" and Path(source).name == "publish_2":
        raise OSError("injected final JSON publication failure")
    return original_replace(source, destination)

engine.os.replace = replace
''', encoding="utf-8")
    return directory


def _start_pair(source, routes, mode, hooks, control, fail_off=False):
    processes = {}
    for science, route in zip(("off", "suggest"), routes):
        env = {
            **os.environ,
            "PYTHONPATH": str(hooks) + os.pathsep + os.environ.get("PYTHONPATH", ""),
            "SUEWS_TEST_CONTROL": str(control), "SUEWS_TEST_ROLE": science,
            "SUEWS_TEST_FAIL": "1" if fail_off and science == "off" else "0",
        }
        processes[science] = subprocess.Popen(
            _command(route, source, science, mode), cwd=source.parent.parent,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env,
        )
    return processes


def _release_pair(processes, control, source):
    deadline = time.monotonic() + 120
    for science, process in processes.items():
        while not (control / f"{science}.ready").exists():
            if process.poll() is not None:
                stdout, stderr = process.communicate()
                pytest.fail(f"{science} exited before the barrier: {stdout}\n{stderr}")
            assert time.monotonic() < deadline, "Phase B barrier timed out"
            time.sleep(0.01)
    stages = [json.loads((control / f"{science}.ready").read_text(encoding="utf-8"))
              for science in processes]
    # A real path observation catches the old shared-path branch deterministically,
    # even on a scheduler that happens not to corrupt its files in this run.
    assert stages[0]["yaml"] != stages[1]["yaml"]
    assert stages[0]["report"] != stages[1]["report"]
    for stage in stages:
        assert Path(stage["yaml"]).parent.parent == source.parent
        assert Path(stage["yaml"]).parent.name.startswith(".suews-validate-")
        assert Path(stage["yaml"]).is_file()
    (control / "release").touch()


def _collect(process, command):
    stdout, stderr = process.communicate(timeout=180)
    return subprocess.CompletedProcess(command, process.returncode, stdout, stderr)


def _stop(processes):
    for process in processes.values():
        if process.poll() is None:
            process.kill()
        process.communicate(timeout=30)


@pytest.mark.e2e
@pytest.mark.parametrize("routes", [
    ("suews", "suews"), ("suews-validate", "suews-validate"), ("suews", "suews-validate"),
])
@pytest.mark.parametrize("mode", ["public", "dev"])
def test_public_abc_concurrent_results_match_isolated_controls(source, hooks, tmp_path, routes, mode):
    original = _snapshot(list(source.parent.iterdir()))
    controls = {}
    reports = {}
    for science, route in zip(("off", "suggest"), routes):
        reports[science] = _success(_run(route, source, science, mode), science)
        controls[science] = _products(source)
        assert json.loads(controls[science][f"report_{source.stem}.json"]) == reports[science]
    control = tmp_path / "control"
    control.mkdir()
    processes = _start_pair(source, routes, mode, hooks, control)
    try:
        _release_pair(processes, control, source)
        for science, process in processes.items():
            result = _collect(process, process.args)
            assert _success(result, science) == reports[science]
        final = _products(source)
        assert final in controls.values()  # One complete generation, never a mixed set.
        assert not list(source.parent.glob(".suews-validate-*"))
        assert _snapshot(original) == original
        assert {p.name for p in source.parent.iterdir()} == {
            *(p.name for p in original), *final,
        }
    finally:
        _stop(processes)


@pytest.mark.e2e
@pytest.mark.parametrize("routes", [("suews", "suews-validate"), ("suews-validate", "suews")])
def test_public_abc_concurrent_write_failure_preserves_good_set_and_retries(source, hooks, tmp_path, routes):
    original = _snapshot(list(source.parent.iterdir()))
    _success(_run(routes[0], source, "off"), "off")
    off_control = _products(source)
    _success(_run(routes[1], source, "suggest"), "suggest")
    good_control = _products(source)
    control = tmp_path / "control"
    control.mkdir()
    processes = _start_pair(source, routes, "public", hooks, control, fail_off=True)
    try:
        _release_pair(processes, control, source)
        _success(_collect(processes["suggest"], processes["suggest"].args), "suggest")
        failed = _collect(processes["off"], processes["off"].args)
        assert failed.returncode == 1, failed.stdout + failed.stderr
        envelope = json.loads(failed.stdout)
        assert envelope["status"] == "error"
        assert envelope["errors"][0]["code"] == "output_write_failed"
        assert "injected final JSON publication failure" in envelope["errors"][0]["message"]
        assert _products(source) == good_control
        evidence = Path(envelope["data"]["evidence_dir"])
        for phase in "ABC":
            for suffix in ("txt", "json"):
                assert (evidence / f"temp_report{phase}_{source.stem}.{suffix}").is_file()
        retained = _snapshot([path for path in evidence.iterdir() if path.is_file()])
        _success(_run(routes[0], source, "off"), "off")
        assert _products(source) == off_control
        assert _snapshot(retained) == retained
        assert list(source.parent.glob(".suews-validate-*")) == [evidence]
        assert _snapshot(original) == original
    finally:
        _stop(processes)


@pytest.mark.parametrize("route", ["suews", "suews-validate"])
@pytest.mark.parametrize("out_format", [None, "json"])
@pytest.mark.parametrize("reject", [False, True])
def test_public_abc_table_json_and_rejection_evidence(source, route, out_format, reject):
    if reject:
        payload = _load_sample_dict()
        payload["sites"][0]["properties"]["bogus_extra_param"] = 42
        source.write_text(yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False), encoding="utf-8")
    original = _snapshot(list(source.parent.iterdir()))
    result = _run(route, source, "suggest", out_format=out_format)
    assert result.returncode == (1 if reject else 0), result.stdout + result.stderr
    assert "Traceback" not in result.stdout + result.stderr
    products = _products(source)
    report = json.loads(products[f"report_{source.stem}.json"])
    assert [p["phase"] for p in report["phases"]] == ["A", "B", "C"]
    assert [p["phase"] for p in report["phases"] if p["status"] == "FAILED"] == (["C"] if reject else [])
    if out_format == "json":
        envelope = json.loads(result.stdout)
        assert envelope["data"]["validation_report"] == report
        assert envelope["status"] == ("error" if reject else "warning")
    else:
        assert ("Validation failed" if reject else "Validation completed") in result.stdout
    if reject:
        assert b"bogus_extra_param" in products[f"report_{source.stem}.txt"]
        evidence, = source.parent.glob(".suews-validate-*")
        assert products[f"updated_{source.name}"] == (evidence / f"updatedB_{source.name}").read_bytes()
        assert report["phases"][-1]["yaml_out"] is None
    else:
        assert not list(source.parent.glob(".suews-validate-*"))
    assert _snapshot(original) == original


@pytest.mark.parametrize("route", ["suews", "suews-validate"])
@pytest.mark.parametrize("forcing", ["on", "off"])
def test_public_abc_forcing_option_reaches_shared_phase_a(source, route, forcing):
    payload = _load_sample_dict()
    payload["model"]["control"]["forcing"]["file"]["value"] = "missing-forcing.txt"
    source.write_text(yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False), encoding="utf-8")
    original = _snapshot(list(source.parent.iterdir()))
    result = _run(route, source, "off", extra=("--forcing", forcing))
    envelope = json.loads(result.stdout)
    if forcing == "on":
        assert result.returncode == 1
        assert envelope["data"]["phases_run"] == ["A"]
        assert any(
            issue["code"] == "A.FORCING" and "missing-forcing.txt" in issue["message"]
            for issue in envelope["errors"]
        )
    else:
        _success(result, "off")
    assert _snapshot(original) == original


@pytest.mark.e2e
@pytest.mark.parametrize("route", ["suews", "suews-validate"])
def test_installed_commands_use_working_tree_validator_sources(source, tmp_path, route):
    """An installed CLI must test the current files, including uncommitted edits.

    Comparing HEAD is insufficient: the regression patch may not be committed
    when a reviewer runs it. A subprocess receipt attests the actual loaded
    modules against bytes in the checkout containing these tests.
    """
    import hashlib

    modules = {
        "supy.cmd.validate_config": "src/supy/cmd/validate_config.py",
        "supy.data_model.validation.pipeline.orchestrator":
            "src/supy/data_model/validation/pipeline/orchestrator.py",
        "supy.data_model.validation.engine":
            "src/supy/data_model/validation/engine.py",
        "supy.data_model.validation.pipeline.document":
            "src/supy/data_model/validation/pipeline/document.py",
        "supy.data_model.validation.core.yaml_helpers":
            "src/supy/data_model/validation/core/yaml_helpers.py",
    }
    root = Path(__file__).resolve().parents[2]
    expected = {
        module: hashlib.sha256((root / relative).read_bytes()).hexdigest()
        for module, relative in modules.items()
    }
    probe = tmp_path / "source-probe"
    probe.mkdir()
    receipt = tmp_path / "loaded-sources.json"
    (probe / "sitecustomize.py").write_text('''
import atexit
import hashlib
import inspect
import json
import os
from pathlib import Path
import sys

# Observe after the real command imports its modules. Do not load a different
# copy ourselves or replace any validator behaviour.
def record_sources():
    observed = {}
    for name in json.loads(os.environ["SUEWS_TEST_EXPECTED_SOURCES"]):
        module = sys.modules.get(name)
        path = inspect.getsourcefile(module) if module is not None else None
        observed[name] = {
            "path": path,
            "sha256": hashlib.sha256(Path(path).read_bytes()).hexdigest() if path else None,
        }
    Path(os.environ["SUEWS_TEST_SOURCE_RECEIPT"]).write_text(
        json.dumps(observed), encoding="utf-8",
    )

atexit.register(record_sources)
''', encoding="utf-8")
    env = {
        **os.environ,
        "PYTHONPATH": str(probe) + os.pathsep + os.environ.get("PYTHONPATH", ""),
        "SUEWS_TEST_EXPECTED_SOURCES": json.dumps(expected),
        "SUEWS_TEST_SOURCE_RECEIPT": str(receipt),
    }
    original = _snapshot(list(source.parent.iterdir()))
    result = subprocess.run(
        _command(route, source, "off"), cwd=source.parent.parent,
        capture_output=True, text=True, timeout=180, env=env,
    )
    _success(result, "off")
    observed = json.loads(receipt.read_text(encoding="utf-8"))
    assert {name: record["sha256"] for name, record in observed.items()} == expected, observed
    assert _snapshot(original) == original


def _encoded_validate(source, args, encoding):
    return _invoke_standalone_click(
        "suews",
        args,
        cwd=source.parent,
        encoding=encoding,
        env_overrides={"NO_COLOR": "1", "COLUMNS": "320"},
        unset_env=("FORCE_COLOR", "CLICOLOR_FORCE"),
    )


@pytest.mark.e2e
def test_installed_validator_cp1252_subprocess_smoke(source):
    """Focused subprocess smoke test for strict ``cp1252`` OS stream encoding."""
    path = source.rename(source.with_name("caf\u00e9[red]\u7ad9.yml"))
    env = {
        k: v
        for k, v in os.environ.items()
        if k not in {"FORCE_COLOR", "CLICOLOR_FORCE"}
    }
    env.update(
        {
            "NO_COLOR": "1",
            "PYTHONIOENCODING": "cp1252:strict",
            "COLUMNS": "320",
        }
    )
    result = subprocess.run(
        [*_entry_point("suews"), str(path)],
        cwd=path.parent,
        env=env,
        capture_output=True,
        encoding="cp1252",
        check=False,
        timeout=180,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "[OK] Validation completed" in result.stdout
    assert result.stdout.isascii(), result.stdout


@pytest.mark.parametrize("encoding", ["cp1252", "utf-8"])
def test_installed_validator_text_keeps_literal_product_paths(source, encoding):
    path = source.rename(source.with_name("caf\u00e9[red]\u7ad9.yml"))
    result = _encoded_validate(path, [path], encoding)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "[OK] Validation completed" in result.stdout
    for label, basename in (
        ("Report", f"report_{path.stem}.txt"),
        ("Updated YAML", f"updated_{path.name}"),
    ):
        actual = (path.parent / basename).resolve()
        assert actual.is_file()
        display = str(actual).encode("ascii", "backslashreplace").decode("ascii")
        assert f"{label}: {display}" in result.stdout
    assert result.stdout.isascii(), result.stdout
    structured = _encoded_validate(path, [path, "--format", "json"], encoding)
    assert structured.returncode == 0, structured.stdout + structured.stderr
    assert (
        Path(json.loads(structured.stdout)["data"]["report_file"]).resolve()
        == (path.parent / f"report_{path.stem}.txt").resolve()
    )


@pytest.mark.parametrize("pipeline", ["C", "ABC"])
@pytest.mark.parametrize("encoding", ["cp1252", "utf-8"])
def test_installed_validator_dry_run_literal_issue_and_filename(source, pipeline, encoding):
    rejected = source.with_name("bad[red]\u7ad9.yml")
    payload = _load_sample_dict()
    key = "bad[red]\u7ad9"
    payload["sites"][0]["properties"][key] = 42
    rejected.write_text(yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False), encoding="utf-8")
    inputs = [source, rejected] if pipeline == "C" else [rejected]
    result = _encoded_validate(source, [*inputs, "-p", pipeline, "--dry-run"], encoding)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "Traceback" not in result.stdout + result.stderr
    display = r"bad[red]\u7ad9"
    assert result.stdout.isascii(), result.stdout
    border_lines = [
        line.strip()
        for line in result.stdout.splitlines()
        if line.strip().startswith("+")
    ]
    assert len(border_lines) >= 2, result.stdout
    assert all(
        line.isascii() and set(line) <= set("+-=") for line in border_lines
    ), border_lines
    rows = [line.split("|") for line in result.stdout.splitlines()]
    assert any(
        len(cells) >= 5
        and f"{display}.yml" in cells[1]
        and "[X] Invalid" in cells[2]
        and display in cells[3]
        for cells in rows
    ), result.stdout
    summary = "0/1 files valid" if pipeline == "ABC" else "1/2 files valid"
    assert summary in result.stdout
    assert not (source.parent / f"updated_{rejected.name}").exists()


def test_installed_validator_legacy_stream_usage_and_public_restriction(source):
    usage = _encoded_validate(source, [source, "-p", "B", "--dry-run"], "cp1252")
    assert usage.returncode == 2
    assert "[X] --dry-run is supported for pipeline C or ABC only" in usage.stdout
    payload = _load_sample_dict()
    payload["model"]["physics"]["stebbs"]["enabled"] = {"value": True}
    source.write_text(yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False), encoding="utf-8")
    restricted = _encoded_validate(source, [source], "cp1252")
    assert restricted.returncode == 1
    assert "[X] Configuration contains experimental features restricted in public mode:" in restricted.stdout
    assert "  - STEBBS is enabled" in restricted.stdout
    assert "--mode dev" in restricted.stdout
    assert "Traceback" not in usage.stderr + restricted.stderr


@pytest.mark.parametrize("route", ["suews", "suews-validate"])
@pytest.mark.parametrize("out_format", [None, "json"])
def test_public_abc_interrupt_with_failed_rollback_names_unrestored_outputs(
    source, monkeypatch, route, out_format,
):
    """Ctrl-C during publication must not hide outputs rollback left changed.

    Click turns a bare KeyboardInterrupt into "Aborted!"; the CLI first names
    the unrestored output, its retained previous copy and the evidence directory.
    """
    from supy.data_model.validation.pipeline import orchestrator as engine

    _success(_run(route, source, "suggest"), "suggest")
    before = _products(source)
    report_txt = source.parent / f"report_{source.stem}.txt"
    original_replace = engine.os.replace

    def fail_interrupt_replace(src, dst):
        name = Path(src).name
        if name == "publish_2":
            raise KeyboardInterrupt("injected interrupt during JSON publication")
        if name == "previous_1":
            raise OSError("injected rollback failure on the text report")
        return original_replace(src, dst)

    # A different generation makes the unrestored text report detectable.
    with monkeypatch.context() as patch:
        patch.setattr(engine.os, "replace", fail_interrupt_replace)
        result = _run(route, source, "off", out_format=out_format)
    assert result.returncode != 0, result.stdout + result.stderr
    assert "Traceback" not in result.stdout + result.stderr
    assert "Aborted!" in result.stderr
    evidence, = source.parent.glob(".suews-validate-*")
    if out_format == "json":
        envelope = json.loads(result.stdout)  # Exactly one envelope on stdout.
        assert envelope["status"] == "error"
        error, = envelope["errors"]
        assert error["code"] == "output_publication_interrupted"
        assert str(report_txt) in error["message"]
        assert "injected interrupt during JSON publication" in error["message"]
        assert "injected rollback failure on the text report" in error["message"]
        assert Path(envelope["data"]["evidence_dir"]) == evidence
        unrestored, = envelope["data"]["unrestored_outputs"]
        assert Path(unrestored["path"]) == report_txt
        assert unrestored["error"] == "injected rollback failure on the text report"
        backup = Path(unrestored["backup"])
    else:
        assert "outputs not restored" in result.stdout
        assert f"{report_txt}: injected rollback failure on the text report" in result.stdout
        backup = evidence / "previous_1"
        assert f"previous copy kept at {backup}" in result.stdout
        assert f"Phase evidence: {evidence}" in result.stdout
    assert backup.parent == evidence
    assert backup.read_bytes() == before[report_txt.name]
    after = _products(source)
    assert after[report_txt.name] != before[report_txt.name]  # Left unrestored.
    for name in (f"updated_{source.name}", f"report_{source.stem}.json"):
        assert after[name] == before[name]  # Restored by the rollback.
