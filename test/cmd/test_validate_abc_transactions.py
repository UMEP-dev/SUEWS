"""Exercise installed ABC entry points with real phases and overlapping processes."""

from importlib.resources import files
import json
import os
from pathlib import Path
import subprocess
import sysconfig
import time

import pytest
import yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR", "C-ISOLATION"),
]


@pytest.fixture
def source(tmp_path):
    project = tmp_path / "project"
    project.mkdir()
    for item in files("supy").joinpath("sample_data").iterdir():
        if item.is_file():
            (project / item.name).write_bytes(item.read_bytes())
    path = project / "sample_config.yml"
    payload = yaml.safe_load(path.read_text(encoding="utf-8"))
    forcing = payload["model"]["control"]["forcing"]["file"]["value"]
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


def _run(route, source, science, mode="public", out_format="json", extra=()):
    return subprocess.run(
        _command(route, source, science, mode, out_format) + list(extra),
        cwd=source.parent.parent, capture_output=True, text=True, timeout=180,
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
        payload = yaml.safe_load(source.read_text(encoding="utf-8"))
        payload["sites"][0]["properties"]["bogus_extra_param"] = 42
        source.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
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
    payload = yaml.safe_load(source.read_text(encoding="utf-8"))
    payload["model"]["control"]["forcing"]["file"]["value"] = "missing-forcing.txt"
    source.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
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
        "supy.data_model.validation.core.feedback":
            "src/supy/data_model/validation/core/feedback.py",
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
    env = {
        k: v
        for k, v in os.environ.items()
        if k not in ("FORCE_COLOR", "CLICOLOR_FORCE")
    }
    env.update(
        {
            "NO_COLOR": "1",
            "PYTHONIOENCODING": f"{encoding}:strict",
            "COLUMNS": "320",
        }
    )
    return subprocess.run(
        [*_entry_point("suews"), *map(str, args)],
        cwd=source.parent,
        env=env,
        capture_output=True,
        encoding=encoding,
        check=False,
        timeout=180,
    )


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
    payload = yaml.safe_load(source.read_text(encoding="utf-8"))
    key = "bad[red]\u7ad9"
    payload["sites"][0]["properties"][key] = 42
    rejected.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
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
    payload = yaml.safe_load(source.read_text(encoding="utf-8"))
    payload["model"]["physics"]["stebbs"]["enabled"] = {"value": True}
    source.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")
    restricted = _encoded_validate(source, [source], "cp1252")
    assert restricted.returncode == 1
    assert "[X] Configuration contains experimental features restricted in public mode:" in restricted.stdout
    assert "  - STEBBS is enabled" in restricted.stdout
    assert "--mode dev" in restricted.stdout
    assert "Traceback" not in usage.stderr + restricted.stderr
