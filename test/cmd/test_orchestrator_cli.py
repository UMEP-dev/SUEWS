"""Exercise the module CLI with real phases and scientific transformations."""

import copy
from dataclasses import dataclass
from functools import lru_cache
from importlib.resources import files
import json
import os
import subprocess
import sys

import pytest
import yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]

_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


def _load_yaml(text: str):
    return yaml.load(text, Loader=_YAML_LOADER)


def _dump_yaml(payload) -> str:
    return yaml.dump(payload, Dumper=_YAML_DUMPER, sort_keys=False)


@dataclass(frozen=True)
class _ModuleRunResult:
    """Subprocess-compatible result for in-process ``orchestrator.main()`` calls."""

    returncode: int
    stdout: str
    stderr: str


@lru_cache(maxsize=1)
def _sample_data_files() -> dict[str, bytes]:
    """Cache bundled sample_data file bytes once per process via Traversable API."""
    return {
        resource.name: resource.read_bytes()
        for resource in files("supy").joinpath("sample_data").iterdir()
        if resource.is_file()
    }


@lru_cache(maxsize=1)
def _sample_dict_template():
    return _load_yaml(_sample_data_files()["sample_config.yml"].decode("utf-8"))


def _load_sample_dict():
    return copy.deepcopy(_sample_dict_template())


@pytest.fixture
def sample_config(tmp_path):
    for name, content in _sample_data_files().items():
        (tmp_path / name).write_bytes(content)
    return tmp_path / "sample_config.yml"


def _evidence(tmp_path):
    directories = list(tmp_path.glob(".suews-validate-*"))
    assert len(directories) == 1
    return directories[0]


def _run_module_inprocess(
    monkeypatch, capsys, path, mode="dev", phase="ABC", science_fixes="suggest"
):
    """Invoke ``orchestrator.main()`` directly in-process with argv/cwd isolated."""
    from supy.data_model.validation.pipeline import orchestrator

    monkeypatch.chdir(path.parent)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "orchestrator",
            str(path),
            "--mode",
            mode,
            "--phase",
            phase,
            "--science-fixes",
            science_fixes,
        ],
    )
    capsys.readouterr()
    returncode = orchestrator.main()
    captured = capsys.readouterr()
    return _ModuleRunResult(
        returncode=returncode,
        stdout=captured.out,
        stderr=captured.err,
    )


def _run_module_subprocess(
    path, mode="dev", phase="ABC", science_fixes="suggest", hash_seed=None
):
    """Run ``python -m supy.data_model.validation.pipeline.orchestrator`` in a subprocess."""
    env = os.environ.copy()
    if hash_seed is not None:
        env["PYTHONHASHSEED"] = str(hash_seed)
    return subprocess.run(
        [
            sys.executable, "-m", "supy.data_model.validation.pipeline.orchestrator",
            str(path), "--mode", mode, "--phase", phase,
            "--science-fixes", science_fixes,
        ],
        cwd=path.parent, capture_output=True, text=True, check=False, timeout=180, env=env,
    )


@pytest.mark.parametrize(
    ("valid", "expected_code"),
    [(True, 0), (False, 1)],
    ids=["valid-sample", "malformed-input"],
)
def test_module_cli_subprocess_smoke(tmp_path, sample_config, valid, expected_code):
    """Focused subprocess smoke test for ``python -m ...orchestrator`` entry point."""
    target = sample_config
    if not valid:
        target = tmp_path / "malformed.yml"
        target.write_text("model: null\n", encoding="utf-8")
    result = _run_module_subprocess(target)
    assert result.returncode == expected_code, result.stdout + result.stderr
    assert "Traceback" not in result.stdout + result.stderr
    if valid:
        assert "[OK] Validation completed" in result.stdout
        assert (tmp_path / "updated_sample_config.yml").is_file()
        assert (tmp_path / "report_sample_config.txt").is_file()
        assert (tmp_path / "report_sample_config.json").is_file()
    else:
        assert "[X] Validation failed!" in result.stdout
        assert (tmp_path / "report_malformed.txt").is_file()
        assert not (tmp_path / "updated_malformed.yml").exists()


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("phase", ["AB", "ABC"])
@pytest.mark.parametrize("science_fixes", ["suggest", "apply", "off"])
@pytest.mark.parametrize("warning_only", [False, True])
def test_module_cli_runs_phases_and_preserves_science_mode(
    tmp_path, sample_config, monkeypatch, capsys, mode, phase, science_fixes, warning_only
):
    """Phase B must run and apply corrections only when explicitly requested."""
    path = sample_config
    payload = _load_sample_dict()
    land_cover = payload["sites"][0]["properties"]["land_cover"]
    # A small fraction-sum discrepancy is a real Phase B correction.
    land_cover["paved"]["sfr"]["value"] -= 0.00005
    original_sum = sum(
        surface["sfr"]["value"] for surface in land_cover.values() if "sfr" in surface
    )
    if warning_only:
        payload["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
            "emission_factor_co2_fuel"
        )
    path.write_text(_dump_yaml(payload), encoding="utf-8")

    result = _run_module_inprocess(monkeypatch, capsys, path, mode, phase, science_fixes)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "Unexpected error" not in result.stdout
    assert "Configuration structure check..." in result.stdout
    assert "Physics validation check..." in result.stdout
    if phase == "ABC":
        assert "Configuration consistency check..." in result.stdout
    updated_path = tmp_path / "updated_sample_config.yml"
    assert updated_path.is_file()
    updated = _load_yaml(updated_path.read_text(encoding="utf-8"))
    updated_cover = updated["sites"][0]["properties"]["land_cover"]
    updated_sum = sum(
        surface["sfr"]["value"] for surface in updated_cover.values() if "sfr" in surface
    )
    expected_sum = 1.0 if science_fixes == "apply" else original_sum
    assert updated_sum == pytest.approx(expected_sum, abs=1e-10)
    report_path = tmp_path / "report_sample_config.txt"
    assert report_path.is_file(), result.stdout + result.stderr
    report = report_path.read_text(encoding="utf-8")
    assert "Validation stopped at:" not in report
    if science_fixes == "suggest":
        assert "## SUGGESTED UPDATES" in report
    elif science_fixes == "off":
        assert "## SUGGESTED UPDATES" not in report
    if warning_only:
        assert "emission_factor_co2_fuel" in report

    if phase == "ABC":
        sidecar = json.loads(report_path.with_suffix(".json").read_text(encoding="utf-8"))
        assert [p["phase"] for p in sidecar["phases"]] == ["A", "B", "C"]
        assert all(p["status"] != "FAILED" for p in sidecar["phases"])
        b_issues = sidecar["phases"][1]["issues"]
        if science_fixes == "suggest":
            assert any(i["severity"] == "SUGGESTION" for i in b_issues)
        elif science_fixes == "apply":
            assert "## APPLIED UPDATES" in report
            assert any(i["severity"] == "APPLIED_FIX" for i in b_issues)
        else:
            assert not any(
                i["severity"] in {"SUGGESTION", "APPLIED_FIX"} for i in b_issues
            )


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("science_fixes", ["suggest", "apply", "off"])
def test_module_abc_c_rejection_keeps_diagnosis_and_last_b_yaml(
    tmp_path, sample_config, monkeypatch, capsys, mode, science_fixes
):
    """An extra property passes B but must fail C without losing B's work."""
    payload = _load_sample_dict()
    properties = payload["sites"][0]["properties"]
    properties["bogus_extra_param"] = 42
    properties["land_cover"]["paved"]["sfr"]["value"] -= 0.00005
    properties["anthropogenic_emissions"]["co2"].pop("emission_factor_co2_fuel")
    sample_config.write_text(_dump_yaml(payload), encoding="utf-8")

    result = _run_module_inprocess(
        monkeypatch, capsys, sample_config, mode, science_fixes=science_fixes
    )
    assert result.returncode == 1, result.stdout + result.stderr
    assert "[OK] Phase B completed" in result.stdout
    assert "[OK] Validation completed" not in result.stdout
    report = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    assert "Validation stopped at: Model Compatibility" in report
    assert "## ACTION NEEDED" in report
    assert "bogus_extra_param" in report
    assert "emission_factor_co2_fuel" in report
    if science_fixes == "suggest":
        assert "## SUGGESTED UPDATES" in report
    elif science_fixes == "apply":
        assert "## APPLIED UPDATES" in report
    else:
        assert "## SUGGESTED UPDATES" not in report
    assert (tmp_path / "updated_sample_config.yml").read_bytes() == (
        _evidence(tmp_path) / "updatedB_sample_config.yml"
    ).read_bytes()
    sidecar = json.loads((tmp_path / "report_sample_config.json").read_text(encoding="utf-8"))
    assert [p["phase"] for p in sidecar["phases"]] == ["A", "B", "C"]
    assert [p["phase"] for p in sidecar["phases"] if p["status"] == "FAILED"] == ["C"]
    assert any(
        i["code"].startswith("C.PYDANTIC") and "bogus_extra_param" in i["yaml_path"]
        for i in sidecar["phases"][2]["issues"]
    )


@pytest.mark.parametrize("output", [
    "report_sample_config.txt", "report_sample_config.json", "updated_sample_config.yml",
])
def test_module_abc_output_write_failure_is_not_success(
    tmp_path, sample_config, monkeypatch, capsys, output
):
    """An unwritable final destination must never yield a success exit/banner."""
    (tmp_path / output).mkdir()
    result = _run_module_inprocess(monkeypatch, capsys, sample_config)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "[OK] Validation completed" not in result.stdout
    assert "[X]" in result.stdout
    # Required-output failures must leave B's successful output recoverable.
    assert (_evidence(tmp_path) / "updatedB_sample_config.yml").is_file()
    for phase in ("A", "B"):
        for suffix in ("txt", "json"):
            assert (_evidence(tmp_path) / f"temp_report{phase}_sample_config.{suffix}").is_file()


@pytest.mark.parametrize("failed_phase", ["A", "B"])
def test_module_abc_stops_at_first_failure(
    tmp_path, sample_config, monkeypatch, capsys, failed_phase
):
    """Early failures retain their own diagnosis and never enter Phase C."""
    payload = _load_sample_dict()
    if failed_phase == "A":
        payload["model"]["physics"].pop("net_radiation")
    else:
        payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] = -0.1
    sample_config.write_text(_dump_yaml(payload), encoding="utf-8")

    result = _run_module_inprocess(monkeypatch, capsys, sample_config)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "Configuration consistency check..." not in result.stdout
    assert "[OK] Validation completed" not in result.stdout
    report = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    stage = "Completeness Check" if failed_phase == "A" else "Scientific Validation"
    assert f"Validation stopped at: {stage}" in report
    assert "## ACTION NEEDED" in report
    assert ("net_radiation" if failed_phase == "A" else "sfr") in report
    sidecar = json.loads((tmp_path / "report_sample_config.json").read_text(encoding="utf-8"))
    assert [p["phase"] for p in sidecar["phases"]] == (
        ["A"] if failed_phase == "A" else ["A", "B"]
    )
    assert sidecar["phases"][-1]["status"] == "FAILED"
    assert (tmp_path / "updated_sample_config.yml").read_bytes() == (
        _evidence(tmp_path) / "updatedA_sample_config.yml"
    ).read_bytes()


@pytest.mark.parametrize("mode", ["public", "dev"])
def test_module_abc_keeps_default_info_separate_from_science(
    tmp_path, sample_config, monkeypatch, capsys, mode
):
    """C defaults must not replace B warnings or be labelled scientific fixes."""
    payload = _load_sample_dict()
    payload.pop("schema_version")
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] -= 0.00005
    sample_config.write_text(_dump_yaml(payload), encoding="utf-8")

    result = _run_module_inprocess(monkeypatch, capsys, sample_config, mode)
    assert result.returncode == 0, result.stdout + result.stderr
    report = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    sections = {}
    for block in report.split("\n## ")[1:]:
        heading, _, content = block.partition("\n")
        sections.setdefault(heading, "")
        sections[heading] += content
    assert "schema_version" in sections["INFO"]
    assert sections["REVIEW ADVISED"].strip()
    assert "sfr" in sections["SUGGESTED UPDATES"]
    assert "schema_version" not in sections["SUGGESTED UPDATES"]


@pytest.mark.parametrize("existing_evidence", [False, True])
def test_module_abc_success_file_set_preserves_existing_evidence(
    tmp_path, sample_config, monkeypatch, capsys, existing_evidence
):
    """Remove only this run's redundant reports, including their JSON sidecars."""
    preserved = {
        "temp_reportA_sample_config.txt": b"previous A report",
        "temp_reportB_sample_config.json": b"previous B sidecar",
        "temp_reportC_sample_config.txt": b"previous C report",
        "updatedA_sample_config.yml": b"previous A YAML",
        "updatedB_sample_config.yml": b"previous B YAML",
        "unrelated.txt": b"user notes",
    } if existing_evidence else {}
    for name, content in preserved.items():
        (tmp_path / name).write_bytes(content)
    before = {path.name for path in tmp_path.iterdir()}

    result = _run_module_inprocess(monkeypatch, capsys, sample_config)

    assert result.returncode == 0, result.stdout + result.stderr
    assert {path.name for path in tmp_path.iterdir()} - before == {
        "report_sample_config.txt", "report_sample_config.json",
        "updated_sample_config.yml",
    }
    for name, content in preserved.items():
        assert (tmp_path / name).read_bytes() == content


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("changed_input", [False, True])
def test_module_abc_repeat_run_updates_canonical_outputs(
    tmp_path, sample_config, monkeypatch, capsys, mode, changed_input
):
    """Both invocations validate; the second final YAML reflects its own input."""
    before = {path.name for path in tmp_path.iterdir()}
    payload = _load_sample_dict()
    for run in range(2):
        if run and changed_input:
            payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] -= 0.00005
            sample_config.write_text(_dump_yaml(payload), encoding="utf-8")
        input_bytes = sample_config.read_bytes()
        result = _run_module_inprocess(monkeypatch, capsys, sample_config, mode)
        assert result.returncode == 0, result.stdout + result.stderr
        assert sample_config.read_bytes() == input_bytes
        updated = _load_yaml((tmp_path / "updated_sample_config.yml").read_text(encoding="utf-8"))
        assert updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] == pytest.approx(
            payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"], abs=1e-10
        )
        sidecar = json.loads((tmp_path / "report_sample_config.json").read_text(encoding="utf-8"))
        assert [p["phase"] for p in sidecar["phases"]] == ["A", "B", "C"]
        assert all(p["status"] != "FAILED" for p in sidecar["phases"])
        report = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
        assert "Validation stopped at:" not in report
        if run and changed_input:
            assert "sfr" in report
            assert any(i["severity"] == "SUGGESTION" and "sfr" in i["yaml_path"]
                       for i in sidecar["phases"][1]["issues"])
        assert {path.name for path in tmp_path.iterdir()} - before == {
            "report_sample_config.txt", "report_sample_config.json", "updated_sample_config.yml",
        }


@pytest.mark.parametrize("content", ["model: [unterminated\n", "model: null\n"])
def test_module_abc_malformed_input_retains_diagnostic_without_yaml(
    tmp_path, monkeypatch, capsys, content
):
    """No generated YAML must not hide Phase A's genuine rejection."""
    path = tmp_path / "malformed.yml"
    path.write_text(content, encoding="utf-8")
    result = _run_module_inprocess(monkeypatch, capsys, path)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "File error" not in result.stdout
    assert "Unexpected error" not in result.stdout
    assert "Updated YAML:" not in result.stdout
    sidecar = json.loads((tmp_path / "report_malformed.json").read_text(encoding="utf-8"))
    assert sidecar["overall_status"] == "FAILED"
    assert [phase["phase"] for phase in sidecar["phases"]] == ["A"]
    phase = sidecar["phases"][0]
    assert phase["yaml_out"] is None
    report = (tmp_path / "report_malformed.txt").read_text(encoding="utf-8")
    assert "Validation stopped at: Completeness Check" in report
    assert all(issue["message"] in report for issue in phase["issues"])
    assert not (tmp_path / "updated_malformed.yml").exists()


@pytest.mark.parametrize("output", ["text", "json", "yaml"])
@pytest.mark.parametrize("previous_run", [False, True])
def test_module_abc_final_write_failure_keeps_all_phase_evidence(
    tmp_path, sample_config, monkeypatch, capsys, output, previous_run
):
    """A failure during final writing must leave the completed phases recoverable."""
    from pathlib import Path
    from supy.data_model.validation.pipeline import orchestrator
    from supy.data_model.validation.pipeline.report_schema import JSONReportWriter
    from supy.data_model.validation.pipeline.report_writer import ValidationReportWriter

    monkeypatch.setattr(sys, "argv", ["orchestrator", str(sample_config), "--mode", "dev"])
    names = ["updated_sample_config.yml", "report_sample_config.txt", "report_sample_config.json"]
    if previous_run:
        assert orchestrator.main() == 0
    previous = {name: (tmp_path / name).read_bytes() for name in names} if previous_run else {}
    capsys.readouterr()

    if output == "yaml":
        payload = _load_sample_dict()
        payload["sites"][0]["properties"]["bogus_extra_param"] = 42
        sample_config.write_text(_dump_yaml(payload), encoding="utf-8")
        original_copy = orchestrator.shutil.copy2

        def fail_copy(source, destination, *args, **kwargs):
            if Path(destination).name == "updated_sample_config.yml":
                raise OSError("injected final YAML write failure")
            return original_copy(source, destination, *args, **kwargs)

        monkeypatch.setattr(orchestrator.shutil, "copy2", fail_copy)
    else:
        writer = ValidationReportWriter if output == "text" else JSONReportWriter
        original_write = writer.write
        suffix = "txt" if output == "text" else "json"

        def fail_write(self, filepath, *args, **kwargs):
            if Path(filepath).name == f"report_sample_config.{suffix}":
                raise OSError("injected final report write failure")
            return original_write(self, filepath, *args, **kwargs)

        monkeypatch.setattr(writer, "write", fail_write)
    monkeypatch.setattr(sys, "argv", ["orchestrator", str(sample_config), "--mode", "dev"])

    assert orchestrator.main() == 1
    stdout = capsys.readouterr().out
    assert "injected final" in stdout
    assert "[OK] Validation completed" not in stdout
    for name in names:
        if previous_run:
            assert (tmp_path / name).read_bytes() == previous[name]
        else:
            assert not (tmp_path / name).exists()
    for phase in ("A", "B", "C"):
        for suffix in ("txt", "json"):
            assert (_evidence(tmp_path) / f"temp_report{phase}_sample_config.{suffix}").is_file()
    for phase in ("A", "B"):
        assert (_evidence(tmp_path) / f"updated{phase}_sample_config.yml").is_file()


@pytest.mark.parametrize("position", [0, 1, 2])
@pytest.mark.parametrize("previous_run", [False, True])
def test_module_abc_publication_failure_rolls_back_and_retries(
    tmp_path, sample_config, monkeypatch, capsys, position, previous_run
):
    """A failed replacement cannot leave a mixture of old and new results."""
    from pathlib import Path
    from supy.data_model.validation.pipeline import orchestrator

    monkeypatch.setattr(sys, "argv", ["orchestrator", str(sample_config), "--mode", "dev"])
    names = ["updated_sample_config.yml", "report_sample_config.txt", "report_sample_config.json"]
    if previous_run:
        assert orchestrator.main() == 0
    previous = {name: (tmp_path / name).read_bytes() for name in names} if previous_run else {}
    payload = _load_sample_dict()
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] -= 0.00005
    sample_config.write_text(_dump_yaml(payload), encoding="utf-8")
    capsys.readouterr()
    original_replace = orchestrator.os.replace

    def fail_replace(source, destination):
        if Path(source).name == f"publish_{position}":
            raise OSError("injected publication failure")
        return original_replace(source, destination)

    with monkeypatch.context() as patch:
        patch.setattr(orchestrator.os, "replace", fail_replace)
        assert orchestrator.main() == 1
    assert "[OK] Validation completed" not in capsys.readouterr().out
    for name in names:
        if previous_run:
            assert (tmp_path / name).read_bytes() == previous[name]
        else:
            assert not (tmp_path / name).exists()
    evidence = _evidence(tmp_path)
    assert (evidence / "updatedB_sample_config.yml").is_file()
    retained = {path.name: path.read_bytes() for path in evidence.iterdir() if path.is_file()}
    assert orchestrator.main() == 0
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence]
    assert all((evidence / name).read_bytes() == content for name, content in retained.items())
    updated = _load_yaml((tmp_path / names[0]).read_text(encoding="utf-8"))
    assert updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] == pytest.approx(
        payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"], abs=1e-10
    )


def test_module_abc_replaces_output_symlinks_without_touching_targets(
    tmp_path, sample_config, monkeypatch, capsys
):
    """Canonical output links must not turn a rerun into a write to the input."""
    original = sample_config.read_bytes()
    notes = tmp_path / "notes.txt"
    notes.write_bytes(b"unrelated user notes")
    try:
        (tmp_path / "updated_sample_config.yml").symlink_to(sample_config)
        (tmp_path / "report_sample_config.txt").symlink_to(notes)
    except OSError as exc:
        pytest.skip(f"Symlinks unavailable on this platform: {exc}")
    result = _run_module_inprocess(monkeypatch, capsys, sample_config)
    assert result.returncode == 0, result.stdout + result.stderr
    assert sample_config.read_bytes() == original
    assert notes.read_bytes() == b"unrelated user notes"
    assert not (tmp_path / "updated_sample_config.yml").is_symlink()
    assert not (tmp_path / "report_sample_config.txt").is_symlink()
    assert not list(tmp_path.glob(".suews-validate-*"))


def test_module_abc_malformed_rerun_does_not_advertise_stale_yaml(
    tmp_path, sample_config, monkeypatch, capsys
):
    """A new rejection publishes its diagnosis and removes the old canonical YAML."""
    result = _run_module_inprocess(monkeypatch, capsys, sample_config)
    assert result.returncode == 0, result.stdout + result.stderr
    sample_config.write_text("model: null\n", encoding="utf-8")
    result = _run_module_inprocess(monkeypatch, capsys, sample_config)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "Updated YAML:" not in result.stdout
    assert not (tmp_path / "updated_sample_config.yml").exists()
    sidecar = json.loads((tmp_path / "report_sample_config.json").read_text(encoding="utf-8"))
    assert sidecar["overall_status"] == "FAILED"
    assert len(sidecar["phases"]) == 1
    assert sidecar["phases"][0]["yaml_out"] is None
    assert "Validation stopped at: Completeness Check" in (
        tmp_path / "report_sample_config.txt"
    ).read_text(encoding="utf-8")


_HASH_SEED_COMBOS = (
    ("public", "suggest"),
    ("public", "apply"),
    ("dev", "suggest"),
    ("dev", "apply"),
)
_HASH_SEEDS = (1, 17, 95)


@pytest.fixture(scope="module")
def _hash_seed_snapshots(tmp_path_factory):
    """Run all (mode, science_fixes) combinations once per PYTHONHASHSEED subprocess."""
    root = tmp_path_factory.mktemp("orchestrator_hash_seeds")
    sample_files = _sample_data_files()
    driver = (
        "import os, sys\n"
        "from pathlib import Path\n"
        "from supy.data_model.validation.pipeline import orchestrator\n"
        "root = Path(sys.argv[1])\n"
        "for mode in ('public', 'dev'):\n"
        "    for science in ('suggest', 'apply'):\n"
        "        workdir = root / f'{mode}_{science}'\n"
        "        os.chdir(workdir)\n"
        "        sys.argv = [\n"
        "            'orchestrator', str(workdir / 'sample_config.yml'),\n"
        "            '--mode', mode, '--phase', 'ABC', '--science-fixes', science,\n"
        "        ]\n"
        "        rc = orchestrator.main()\n"
        "        if rc != 0:\n"
        "            sys.exit(rc)\n"
    )
    for mode, science in _HASH_SEED_COMBOS:
        workdir = root / f"{mode}_{science}"
        workdir.mkdir(parents=True)
        for name, content in sample_files.items():
            (workdir / name).write_bytes(content)
    results = {}
    for seed in _HASH_SEEDS:
        env = {**os.environ, "PYTHONHASHSEED": str(seed)}
        proc = subprocess.run(
            [sys.executable, "-c", driver, str(root)],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
            timeout=180,
            env=env,
        )
        assert proc.returncode == 0, proc.stdout + proc.stderr
        for mode, science in _HASH_SEED_COMBOS:
            workdir = root / f"{mode}_{science}"
            outputs = {
                name: (workdir / name).read_bytes()
                for name in (
                    "report_sample_config.txt",
                    "report_sample_config.json",
                    "updated_sample_config.yml",
                )
            }
            leftovers = list(workdir.glob(".suews-validate-*"))
            results.setdefault((mode, science), []).append((outputs, leftovers))
    return results


@pytest.mark.parametrize("mode", ["public", "dev"])
@pytest.mark.parametrize("science_fixes", ["suggest", "apply"])
def test_module_abc_reports_repeat_exactly_across_hash_seeds(
    _hash_seed_snapshots, mode, science_fixes
):
    """Hash randomisation must not reorder the disabled CO2 findings."""
    runs = _hash_seed_snapshots[mode, science_fixes]
    snapshots = []
    for outputs, leftovers in runs:
        snapshots.append(outputs)
        assert b"nullified 14 related parameters" in outputs["report_sample_config.txt"]
        assert not leftovers
    assert snapshots[0] == snapshots[1] == snapshots[2]


def test_nullified_paths_keep_input_order_and_facts():
    """Mapping, profile and list paths retain their facts and defined order."""
    from supy.data_model.validation.core.yaml_helpers import collect_nullified_paths

    before = {
        "z": {"value": 0},
        "profile": {"holiday": 2, "working_day": 1},
        "items": [{"value": 3}, 4],
        "removed": 5,
        "unchanged": 6,
        "empty": None,
    }
    after = {
        "empty": None,
        "unchanged": 6,
        "items": [{"value": None}],
        "profile": {"holiday": None, "working_day": None},
        "z": {"value": None},
        "added": {"value": 7},
    }
    assert collect_nullified_paths(before, after) == [
        "z", "profile.working_day", "profile.holiday", "items[0]", "items[1]", "removed",
    ]


@pytest.mark.parametrize("partial_cleanup", [False, True])
def test_module_abc_cleanup_failure_keeps_published_success(
    tmp_path, sample_config, monkeypatch, capsys, partial_cleanup
):
    """A cleanup error after publication must not claim validation/write failure."""
    from supy.data_model.validation.pipeline import orchestrator

    monkeypatch.setattr(sys, "argv", ["orchestrator", str(sample_config), "--mode", "dev"])
    assert orchestrator.main() == 0
    payload = _load_sample_dict()
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] -= 0.00005
    sample_config.write_text(_dump_yaml(payload), encoding="utf-8")
    capsys.readouterr()

    def fail_cleanup(workspace):
        if partial_cleanup:
            (workspace / "temp_reportA_sample_config.txt").unlink()
        raise OSError("injected staging cleanup failure")

    with monkeypatch.context() as patch:
        patch.setattr(orchestrator.shutil, "rmtree", fail_cleanup)
        assert orchestrator.main() == 0
    stdout = capsys.readouterr().out
    assert "[OK] Validation completed" in stdout
    assert "[WARNING] Outputs published; temporary cleanup failed:" in stdout
    assert "injected staging cleanup failure" in stdout
    assert "[X]" not in stdout
    evidence = _evidence(tmp_path)
    assert f"Remaining temporary directory: {evidence}" in stdout
    updated = _load_yaml((tmp_path / "updated_sample_config.yml").read_text(encoding="utf-8"))
    assert updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] == pytest.approx(
        payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"], abs=1e-10
    )
    report = json.loads((tmp_path / "report_sample_config.json").read_text(encoding="utf-8"))
    assert all(phase["status"] != "FAILED" for phase in report["phases"])
    published = {path.name: path.read_bytes() for path in tmp_path.glob("report_sample_config.*")}
    assert orchestrator.main() == 0
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence]
    assert all((tmp_path / name).read_bytes() == content for name, content in published.items())


def test_module_abc_interrupt_with_failed_rollback_names_recovery_paths(
    tmp_path, sample_config, monkeypatch, capsys
):
    """The module CLI prints unrestored outputs and backups before re-raising."""
    from pathlib import Path
    from supy.data_model.validation.pipeline import orchestrator

    monkeypatch.setattr(sys, "argv", ["orchestrator", str(sample_config), "--mode", "dev"])
    assert orchestrator.main() == 0
    report_txt = tmp_path / "report_sample_config.txt"
    previous = report_txt.read_bytes()
    capsys.readouterr()
    original_replace = orchestrator.os.replace

    def interrupt_and_fail_restore(source, destination):
        if Path(source).name == "publish_2":
            raise KeyboardInterrupt("injected interrupt during JSON publication")
        if Path(source).name == "previous_1":
            raise OSError("injected rollback failure on the text report")
        return original_replace(source, destination)

    monkeypatch.setattr(orchestrator.os, "replace", interrupt_and_fail_restore)
    with pytest.raises(orchestrator.ABCPublicationInterrupted):
        orchestrator.main()
    stdout = capsys.readouterr().out
    evidence = _evidence(tmp_path)
    backup = evidence / "previous_1"
    assert f"{report_txt}: injected rollback failure on the text report" in stdout
    assert f"previous copy kept at {backup}" in stdout
    assert f"Phase evidence: {evidence}" in stdout
    assert backup.read_bytes() == previous
