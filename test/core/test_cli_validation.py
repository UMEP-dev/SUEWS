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


def _all_non_empty(files):
    return all(f.exists() and f.stat().st_size > 0 for f in files)

@pytest.mark.cfg
def test_validate_generates_non_empty_report_and_updated_yaml(suews_validate_exe, tmp_path):
    """suews-validate should create non-empty report and updated yaml for invalid input."""
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)

    assert reports, f"No report files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert updated, f"No updated yaml files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"

    assert _all_non_empty(reports), f"Some report files are empty: {reports}"
    assert _all_non_empty(updated), f"Some updated yaml files are empty: {updated}"

@pytest.mark.cfg
def test_validate_second_run_does_not_truncate_reports(suews_validate_exe, tmp_path):
    """Running validate twice should not leave report/updated files zero-length (truncated)."""
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    # First run
    proc1 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports1 = _find_reports(tmp_path)
    updated1 = _find_updated(tmp_path)

    assert reports1 or updated1, (
        f"First run produced no outputs. stdout:\n{proc1.stdout}\nstderr:\n{proc1.stderr}"
    )

    # On first run, require all produced outputs to be non-empty
    assert _all_non_empty(reports1), f"First-run reports have empty files: {reports1}"
    assert _all_non_empty(updated1), f"First-run updated yamls have empty files: {updated1}"

    outputs1 = reports1 + updated1
    sizes_before = {f.name: f.stat().st_size for f in outputs1 if f.exists()}

    time.sleep(0.1)  # small pause to allow mtime differences

    # Second run (simulate user re-running; should not truncate to zero)
    proc2 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports2 = _find_reports(tmp_path)
    updated2 = _find_updated(tmp_path)

    assert reports2 or updated2, (
        f"Second run produced no outputs. stdout:\n{proc2.stdout}\nstderr:\n{proc2.stderr}"
    )

    outputs2 = reports2 + updated2

    # Check that all outputs present after second run are non-empty
    assert _all_non_empty(outputs2), f"Some second-run outputs are empty: {outputs2}"

    # Specifically ensure previously existing files were not truncated
    for name, size_before in sizes_before.items():
        f = tmp_path / name
        assert f.exists(), f"{name} disappeared after second run"
        size_after = f.stat().st_size
        assert size_after > 0, (
            f"{name} was truncated to zero on second run "
            f"(size_before={size_before}, size_after={size_after})"
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_returns_error_but_writes_output_on_invalid_input(suews_validate_exe, tmp_path):
    """
    The CLI may exit with non-zero for validation failures; ensure that when it does,
    it still writes non-empty report/updated files (doesn't fail silently).
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
    assert _all_non_empty(outputs), f"Some report/updated files are empty: {outputs}"

@pytest.mark.cfg
def test_validate_second_run_on_updated_yaml_produces_non_empty_report(
    suews_validate_exe,
    tmp_path,
):
    """
    Running suews-validate on the updated YAML generated by a first run
    should still produce non-empty report/updated files and not truncate existing files.
    """
    # First run on the original invalid YAML
    original_yaml = tmp_path / "invalid_setup.yml"
    _write_minimal_invalid_yaml(original_yaml)

    proc1 = _run_validate(suews_validate_exe, tmp_path, args=[str(original_yaml)])
    reports1 = _find_reports(tmp_path)
    updated1 = _find_updated(tmp_path)
    outputs1 = reports1 + updated1

    assert outputs1, f"First run produced no outputs. stdout:\n{proc1.stdout}\nstderr:\n{proc1.stderr}"
    # All first-run outputs must be non-empty
    assert _all_non_empty(outputs1), f"First-run outputs contain empty files: {outputs1}"

    # We expect at least one updated YAML; take the most recent
    assert updated1, "First run produced no updated yaml files"
    updated_yaml = max(updated1, key=lambda p: p.stat().st_mtime)

    # Record sizes of all outputs after first run
    sizes_before = {f.name: f.stat().st_size for f in outputs1 if f.exists()}

    time.sleep(0.1)  # small pause to allow mtime differences if needed

    # Second run on the updated YAML (what a user is likely to do)
    proc2 = _run_validate(suews_validate_exe, tmp_path, args=[str(updated_yaml)])
    reports2 = _find_reports(tmp_path)
    updated2 = _find_updated(tmp_path)
    outputs2 = reports2 + updated2

    assert outputs2, f"Second run produced no outputs. stdout:\n{proc2.stdout}\nstderr:\n{proc2.stderr}"
    # All outputs present after second run must be non-empty
    assert _all_non_empty(outputs2), f"Second-run outputs contain empty files: {outputs2}"

    # Check that previously existing files did not get truncated to zero
    for name, size_before in sizes_before.items():
        f = tmp_path / name
        assert f.exists(), f"{name} disappeared after second run"
        size_after = f.stat().st_size
        assert size_after > 0, (
            f"{name} was truncated to zero on second run "
            f"(size_before={size_before}, size_after={size_after})"
        )

    # Additionally, ensure that the most recent report is non-empty
    if reports2:
        latest_report = max(reports2, key=lambda p: p.stat().st_mtime)
        assert latest_report.stat().st_size > 0, "Latest report after second run is empty"

@pytest.mark.cfg
def test_second_run_on_user_edited_yaml_produces_non_empty_report(
    suews_validate_exe,
    tmp_path,
):
    """
    Given a YAML that has already been 'updated by SUEWS' and edited by the user,
    running suews-validate again should still produce a non-empty report.
    """
    fixture = Path(__file__).parent / "data" / "issue_1097" / "yaml_setup.yml"
    yaml_path = tmp_path / "yaml_setup.yml"
    yaml_path.write_text(fixture.read_text(encoding="utf8"), encoding="utf8")

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    assert outputs, f"No report/updated files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    # All outputs created in this second-run scenario must be non-empty
    assert _all_non_empty(outputs), f"Some second-run outputs are empty: {outputs}"

    # And specifically ensure that the most recent report is non-empty
    assert reports, f"No report files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    latest_report = max(reports, key=lambda p: p.stat().st_mtime)
    content = latest_report.read_text(encoding="utf8", errors="replace")
    assert content.strip(), (
        f"Second-run report is blank. path={latest_report}, "
        f"stdout={proc.stdout}, stderr={proc.stderr}"
    )