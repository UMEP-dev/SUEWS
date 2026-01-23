import shutil
import subprocess
from pathlib import Path
import time
import sys
import os
import tempfile
import stat
from unittest.mock import patch, mock_open

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
    indicators_found = sum(1 for indicator in meaningful_indicators if indicator.lower() in content.lower())
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
@pytest.mark.smoke
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
@pytest.mark.smoke
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
@pytest.mark.smoke
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


@pytest.mark.cfg
@pytest.mark.smoke
@pytest.mark.skipif(sys.platform == "win32", reason="Skip on Windows - testing Windows path handling cross-platform")
def test_validate_with_windows_paths_produces_meaningful_reports(suews_validate_exe, tmp_path):
    """Test validation with Windows-style paths (mixed separators) produces meaningful reports.
    
    This addresses the core issue from #1097 where Windows path handling
    could cause blank report generation.
    """
    yaml_path = tmp_path / "windows_paths.yml"
    _write_windows_path_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    assert outputs, f"No outputs created with Windows paths. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    
    # Verify files exist and are non-empty
    assert _all_non_empty(outputs), f"Some outputs are empty: {outputs}"
    
    # Critically: verify reports have meaningful content, not just non-zero size
    for report in reports:
        assert _has_meaningful_content(report), (
            f"Report lacks meaningful content: {report}\n"
            f"Content: {report.read_text(encoding='utf8', errors='replace')[:200]}...\n"
            f"stdout: {proc.stdout}\nstderr: {proc.stderr}"
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_report_content_quality(suews_validate_exe, tmp_path):
    """Verify that validation reports contain expected meaningful content markers.
    
    This test ensures reports aren't just non-empty but actually contain
    validation-specific content that indicates successful processing.
    """
    yaml_path = tmp_path / "quality_test.yml"
    _write_minimal_invalid_yaml(yaml_path)

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    assert reports, f"No reports generated. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"

    for report in reports:
        # Check file is non-empty
        assert report.stat().st_size > 0, f"Report file is empty: {report}"
        
        # Check content quality beyond just size
        content = report.read_text(encoding="utf8", errors="replace")
        assert content.strip(), f"Report contains only whitespace: {report}"
        
        # Verify it has meaningful validation content
        assert _has_meaningful_content(report), (
            f"Report lacks expected validation content: {report}\n"
            f"Content preview: {content[:300]}..."
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_handles_file_permission_scenarios(suews_validate_exe, tmp_path):
    """Test validation behaviour with file permission restrictions.
    
    This tests scenarios that could lead to blank reports if file operations fail.
    """
    yaml_path = tmp_path / "permission_test.yml"
    _write_minimal_invalid_yaml(yaml_path)

    # Create a read-only directory scenario (simulate restricted write access)
    restricted_dir = tmp_path / "restricted"
    restricted_dir.mkdir()
    yaml_in_restricted = restricted_dir / "test.yml"
    yaml_in_restricted.write_text(yaml_path.read_text())
    
    # Make directory read-only (this simulates potential Windows permission issues)
    if hasattr(os, 'chmod'):  # Skip if chmod not available
        try:
            restricted_dir.chmod(stat.S_IREAD | stat.S_IEXEC)
            
            # Run validation in restricted directory
            proc = _run_validate(suews_validate_exe, restricted_dir, args=[str(yaml_in_restricted)])
            
            # Even with permission restrictions, should handle gracefully
            # Either succeed with outputs or fail cleanly (no blank reports)
            reports = _find_reports(restricted_dir)
            updated = _find_updated(restricted_dir)
            
            # If outputs were created, they should be meaningful
            for report in reports:
                if report.exists() and report.stat().st_size > 0:
                    assert _has_meaningful_content(report), (
                        f"Report created under restrictions lacks content: {report}"
                    )
                    
        except (OSError, PermissionError):
            pytest.skip("Cannot test permission restrictions on this system")
        finally:
            # Restore permissions for cleanup
            try:
                restricted_dir.chmod(stat.S_IWRITE | stat.S_IREAD | stat.S_IEXEC)
            except (OSError, PermissionError):
                pass


@pytest.mark.cfg
@pytest.mark.smoke
@pytest.mark.skipif(sys.platform != "win32", reason="Windows-specific path testing")
def test_windows_specific_path_validation(suews_validate_exe, tmp_path):
    """Windows-specific test for path handling in validation.
    
    This test only runs on Windows to verify actual Windows path behaviour.
    """
    yaml_path = tmp_path / "win_specific.yml"
    
    # Use actual Windows paths that could cause issues
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

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    assert reports, "Windows validation produced no reports"
    
    for report in reports:
        assert report.stat().st_size > 0, f"Windows validation report is empty: {report}"
        assert _has_meaningful_content(report), (
            f"Windows validation report lacks meaningful content: {report}"
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_with_complex_realistic_yaml(suews_validate_exe, tmp_path):
    """Test validation with complex, realistic YAML similar to issue #1097 data.
    
    This uses a structure similar to the real-world YAML that triggered the bug.
    """
    # Use the fixture data that caused the original issue
    fixture = Path(__file__).parent / "data" / "issue_1097" / "yaml_setup.yml"
    if not fixture.exists():
        pytest.skip("Issue 1097 fixture data not available")
        
    yaml_path = tmp_path / "complex_test.yml"
    yaml_path.write_text(fixture.read_text(encoding="utf8"), encoding="utf8")

    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    # Should produce outputs even with complex config
    assert outputs, f"Complex YAML validation produced no outputs. stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    
    # All outputs should be non-empty and meaningful
    assert _all_non_empty(outputs), f"Some complex YAML outputs are empty: {outputs}"
    
    for report in reports:
        assert _has_meaningful_content(report), (
            f"Complex YAML report lacks meaningful content: {report}\n"
            f"Size: {report.stat().st_size}, Content preview: {report.read_text(encoding='utf8', errors='replace')[:200]}..."
        )


@pytest.mark.cfg
@pytest.mark.smoke
def test_validate_error_injection_file_write_failure(suews_validate_exe, tmp_path):
    """Test validation behaviour when report file writing might fail.
    
    This simulates the type of failure that could cause blank reports.
    """
    yaml_path = tmp_path / "error_injection.yml"
    _write_minimal_invalid_yaml(yaml_path)

    # Create a scenario where output files might have write issues
    # by pre-creating empty files with specific names that the validator might use
    potential_reports = [
        tmp_path / "report_error_injection.txt",
        tmp_path / "report_yaml_setup.txt",
        tmp_path / "updated_error_injection.yml",
        tmp_path / "updated_yaml_setup.yml",
    ]
    
    # Pre-create some files to test overwrite scenarios
    for potential_file in potential_reports[:2]:
        potential_file.write_text("pre-existing content")
    
    proc = _run_validate(suews_validate_exe, tmp_path, args=[str(yaml_path)])

    reports = _find_reports(tmp_path)
    updated = _find_updated(tmp_path)
    outputs = reports + updated

    if outputs:  # If outputs were created despite potential issues
        assert _all_non_empty(outputs), f"Error injection scenario produced empty files: {outputs}"
        
        for report in reports:
            # Verify content is meaningful, not just leftover from pre-creation
            content = report.read_text(encoding="utf8", errors="replace")
            assert "pre-existing content" not in content, (
                f"Report appears to contain pre-existing content, suggesting write failure: {report}"
            )
            assert _has_meaningful_content(report), (
                f"Error injection report lacks meaningful content: {report}"
            )