import shutil
import subprocess
from pathlib import Path
import time
import sys

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


def _has_meaningful_content(file_path: Path) -> bool:
    """Check if report file has meaningful validation content, not just whitespace."""
    if not file_path.exists() or file_path.stat().st_size == 0:
        return False

    content = file_path.read_text(encoding="utf8", errors="replace").strip()
    if not content:
        return False

    # Check for expected validation report content markers
    meaningful_indicators = [
        "SUEWS",  # Product name should appear
        "Validation",  # Core functionality
        "report",  # Report-specific content
        "YAML",  # Configuration type
        "Phase",  # Validation phases
        "Error",  # Error reporting
        "Warning",  # Warning reporting
        "parameter",  # Parameter validation
        "configuration",  # Config validation
    ]

    # Require at least 2 meaningful indicators and minimum content length
    indicators_found = sum(
        1 for indicator in meaningful_indicators if indicator.lower() in content.lower()
    )
    return indicators_found >= 2 and len(content) > 50


def _write_windows_path_yaml(path: Path):
    """Create YAML with Windows-style paths that could trigger path handling bugs."""
    content = f'''name: Windows Path Test
description: Test with mixed Windows path separators
model:
  control:
    tstep: 300
    # Mixed separators like in issue #1097
    forcing_file:
      value: C:/Users\\TestUser/Desktop/suews_data/forcing.txt
    output_file: C:\\Users\\TestUser\\Desktop/outputs\\test_output.txt
    diagnose: 0
    start_time: '2021-01-01'
    end_time: '2021-12-31'
  physics:
    netradiationmethod:
      value: 1
sites:
  - name: test_site
    gridiv: 1
    properties:
      lat: null  # Invalid to trigger validation
      lng: null  # Invalid to trigger validation
'''
    path.write_text(content, encoding="utf8")


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_produces_meaningful_output(suews_validate_exe, tmp_path):
    """Invalid YAML should produce non-empty, meaningful reports and updated YAML."""
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)

    assert reports, f"No report files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert updated, f"No updated yaml files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert _all_non_empty(reports + updated), (
        f"Some report/updated files are empty: {reports + updated}"
    )

    for report in reports:
        content = report.read_text(encoding="utf8", errors="replace")
        assert content.strip(), f"Report contains only whitespace: {report}"
        assert _has_meaningful_content(report), (
            f"Report lacks expected validation content: {report}\n"
            f"Content preview: {content[:300]}..."
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_second_run_not_truncated(suews_validate_exe, tmp_path):
    """Repeated validation runs should not truncate reports or updated YAML files."""
    yaml_path = tmp_path / "yaml_setup.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc1 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports1 = _find_reports(tmp_path)
    updated1 = _find_updated(tmp_path)
    outputs1 = reports1 + updated1

    assert outputs1, (
        f"First run produced no outputs. stdout:\n{proc1.stdout}\nstderr:\n{proc1.stderr}"
    )
    assert _all_non_empty(outputs1), f"First-run outputs contain empty files: {outputs1}"
    assert updated1, "First run produced no updated yaml files"
    sizes_before = {f.name: f.stat().st_size for f in outputs1 if f.exists()}

    time.sleep(0.1)
    proc2 = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])
    reports2 = _find_reports(tmp_path)
    updated2 = _find_updated(tmp_path)
    outputs2 = reports2 + updated2

    assert outputs2, (
        f"Second run produced no outputs. stdout:\n{proc2.stdout}\nstderr:\n{proc2.stderr}"
    )
    assert _all_non_empty(outputs2), f"Some second-run outputs are empty: {outputs2}"

    for name, size_before in sizes_before.items():
        f = tmp_path / name
        assert f.exists(), f"{name} disappeared after second run"
        size_after = f.stat().st_size
        assert size_after > 0, (
            f"{name} was truncated to zero on second run "
            f"(size_before={size_before}, size_after={size_after})"
        )

    updated_yaml = max(updated1, key=lambda p: p.stat().st_mtime)
    sizes_before_third_run = {f.name: f.stat().st_size for f in outputs2 if f.exists()}

    time.sleep(0.1)
    proc3 = _run_validate(suews_validate_exe, tmp_path, args=[str(updated_yaml)])
    reports3 = _find_reports(tmp_path)
    updated3 = _find_updated(tmp_path)
    outputs3 = reports3 + updated3

    assert outputs3, (
        f"Third run on updated YAML produced no outputs. stdout:\n{proc3.stdout}\n"
        f"stderr:\n{proc3.stderr}"
    )
    assert _all_non_empty(outputs3), f"Third-run outputs contain empty files: {outputs3}"

    for name, size_before in sizes_before_third_run.items():
        f = tmp_path / name
        assert f.exists(), f"{name} disappeared after third run"
        size_after = f.stat().st_size
        assert size_after > 0, (
            f"{name} was truncated to zero on third run "
            f"(size_before={size_before}, size_after={size_after})"
        )

    assert reports3, "Third run on updated YAML produced no report files"
    latest_report = max(reports3, key=lambda p: p.stat().st_mtime)
    assert latest_report.stat().st_size > 0, "Latest report after third run is empty"


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_issue_1097_fixture(suews_validate_exe, tmp_path):
    """The issue #1097 fixture should still yield non-empty, meaningful outputs."""
    fixture = Path(__file__).parent / "data" / "issue_1097" / "yaml_setup.yml"
    if not fixture.exists():
        pytest.skip("Issue 1097 fixture data not available")

    yaml_path = tmp_path / "yaml_setup.yml"
    yaml_path.write_text(fixture.read_text(encoding="utf8"), encoding="utf8")

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    assert outputs, f"No report/updated files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert _all_non_empty(outputs), f"Some fixture outputs are empty: {outputs}"

    assert reports, f"No report files created. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    latest_report = max(reports, key=lambda p: p.stat().st_mtime)
    content = latest_report.read_text(encoding="utf8", errors="replace")
    assert content.strip(), (
        f"Fixture report is blank. path={latest_report}, "
        f"stdout={proc.stdout}, stderr={proc.stderr}"
    )

    for report in reports:
        assert _has_meaningful_content(report), (
            f"Fixture report lacks meaningful content: {report}\n"
            f"Content preview: {report.read_text(encoding='utf8', errors='replace')[:300]}..."
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_windows_paths(suews_validate_exe, tmp_path):
    """Windows-style paths should produce meaningful reports on every platform."""
    yaml_path = tmp_path / "windows_paths.yml"
    if sys.platform == "win32":
        content = f'''name: Windows Native Test
model:
  control:
    forcing_file:
      value: {tmp_path.as_posix()}/test_forcing.txt
    output_file: {str(tmp_path)}\\output.txt
    start_time: '2021-01-01'
    end_time: '2021-01-02'
sites:
  - name: win_site
    gridiv: 1
    properties:
      lat: null  # Force validation error
'''
        yaml_path.write_text(content, encoding="utf8")
    else:
        _write_windows_path_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    assert outputs, f"No outputs created with Windows paths. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert reports, f"No reports created with Windows paths. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    assert _all_non_empty(outputs), f"Some Windows path outputs are empty: {outputs}"

    for report in reports:
        assert _has_meaningful_content(report), (
            f"Windows path report lacks meaningful content: {report}\n"
            f"Content preview: {report.read_text(encoding='utf8', errors='replace')[:200]}..."
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_create_consolidated_report_writes_utf8(tmp_path):
    """create_consolidated_report should write UTF-8 reports (Windows locale safe)."""
    from supy.data_model.validation.pipeline.orchestrator import create_consolidated_report

    report_file = tmp_path / "report_unicode.txt"
    create_consolidated_report(
        phases_run=["B"],
        no_action_messages=["- Updated (1) parameter(s):", "-- x: 1 → 2 (example)"],
        final_report_file=str(report_file),
        mode="public",
    )

    decoded = report_file.read_bytes().decode("utf-8")
    assert "→" in decoded
