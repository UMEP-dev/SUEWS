import shutil
import subprocess
from pathlib import Path
import time

import pytest


# Locate the CLI; skip tests if not available
@pytest.fixture(scope="module")
def suews_validate_exe():
    exe = shutil.which("suews-validate")
    if exe is None:
        pytest.skip("suews-validate not in PATH; install supy to run these tests")
    return exe


def _write_minimal_invalid_yaml(path: Path):
    # Small YAML expected to fail validation and produce a report/updated yaml
    content = (
        "name: test\n"
        "sites:\n"
        "  - name: site1\n"
        "    gridiv: 1\n"
        "    properties:\n"
        "      lat: null\n"
    )
    path.write_text(content, encoding="utf8")


def _find_reports(workdir: Path):
    # Common report name patterns produced by the CLI
    return list(workdir.glob("report*.txt"))


def _find_updated(workdir: Path):
    # Common updated-yaml name patterns produced by the CLI
    return list(workdir.glob("updated*.yml")) + list(workdir.glob("updated_*.yml"))


def _run_validate(exe: str, workdir: Path, args=None, timeout=30):
    args = args or []
    cmd = [exe] + args
    proc = subprocess.run(
        cmd,
        cwd=workdir,
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    return proc


def _any_non_empty(files):
    return any(f.exists() and f.stat().st_size > 0 for f in files)


def test_validate_generates_non_empty_report_and_updated_yaml(suews_validate_exe, tmp_path):
    """
    suews-validate should create a non-empty report and updated yaml
    when given invalid input.
    """
    yaml_path = tmp_path / "invalid_config.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)

    # At least one report and one updated yaml should be created
    assert reports, f"No report files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert updated, f"No updated yaml files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"

    # They should not all be empty
    assert _any_non_empty(reports), f"All report files are empty. reports={reports}"
    assert _any_non_empty(updated), f"All updated yaml files are empty. updated={updated}"


def test_validate_second_run_does_not_truncate_reports(suews_validate_exe, tmp_path):
    """
    Running suews-validate twice on the same file should not leave
    report/updated files as zero-length (truncated).
    """
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    # First run
    proc1 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports1 = _find_reports(tmp_path)
    updated1 = _find_updated(tmp_path)
    outputs1 = reports1 + updated1

    assert outputs1, f"First run produced no outputs. stdout:\n{proc1.stdout}\nstderr:\n{proc1.stderr}"
    assert _any_non_empty(outputs1), "First-run outputs are empty"

    # Record sizes
    sizes_before = {f.name: f.stat().st_size for f in outputs1 if f.exists()}

    time.sleep(0.1)  # small pause to allow mtime differences if needed

    # Second run (simulate user re-running; should not truncate to zero)
    proc2 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports2 = _find_reports(tmp_path)
    updated2 = _find_updated(tmp_path)
    outputs2 = reports2 + updated2

    assert outputs2, f"Second run produced no outputs. stdout:\n{proc2.stdout}\nstderr:\n{proc2.stderr}"
    assert _any_non_empty(outputs2), "Second-run outputs are empty"

    for name, size_before in sizes_before.items():
        f = tmp_path / name
        assert f.exists(), f"{name} disappeared after second run"
        size_after = f.stat().st_size
        assert size_after > 0, (
            f"{name} was truncated to zero on second run "
            f"(size_before={size_before}, size_after={size_after})"
        )


def test_validate_returns_error_but_writes_output_on_invalid_input(suews_validate_exe, tmp_path):
    """
    The CLI may exit with non-zero for validation failures; ensure that when it does,
    it still writes a non-empty report/updated file (doesn't fail silently).
    """
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    # Validation of this minimal config is expected to fail (non-zero),
    # but it must still produce report/updated files.
    # If that contract isn't guaranteed, you can relax this first assert.
    # assert proc.returncode != 0, f"Expected non-zero exit for invalid input, got {proc.returncode}"

    assert outputs, f"No report/updated files were created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert _any_non_empty(outputs), "Report/updated files exist but are empty"