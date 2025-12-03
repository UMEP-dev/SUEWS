"""pytest configuration for SUEWS test suite."""

import subprocess

import pytest
import supy
from click.testing import CliRunner


# =============================================================================
# Shared CLI Testing Utilities
# =============================================================================


class CliResultAdapter:
    """Adapter to make Click's CliRunner results compatible with subprocess.CompletedProcess.

    This adapter provides a consistent interface for CLI test results, allowing tests
    to be written in a subprocess-like style while actually running in-process via
    Click's CliRunner (which avoids the 10-20s ninja rebuild overhead in editable installs).

    Attributes
    ----------
    returncode : int
        Exit code (0 for success, non-zero for failure)
    stdout : str
        Standard output content
    stderr : str
        Standard error content (heuristically extracted from combined output)

    Notes
    -----
    Click's CliRunner combines stdout/stderr by default. This adapter uses keyword
    matching to heuristically split them, which is a pragmatic workaround. For more
    precise control, ensure CLI code uses `click.echo(..., err=True)` consistently.
    """

    # Keywords that typically indicate stderr content
    STDERR_KEYWORDS = ("DEPRECAT", "ERROR", "WARNING", "=====", "TRACEBACK")

    def __init__(self, click_result):
        """Initialise from a Click Result object.

        Parameters
        ----------
        click_result : click.testing.Result
            Result from CliRunner.invoke()
        """
        self._result = click_result
        self.returncode = click_result.exit_code
        self.stdout = click_result.output or ""
        self.stderr = self._extract_stderr(click_result.output)

    def _extract_stderr(self, output):
        """Heuristically extract stderr-like content from combined output.

        Parameters
        ----------
        output : str or None
            Combined output from CliRunner

        Returns
        -------
        str
            Lines that appear to be stderr content
        """
        if not output:
            return ""

        stderr_lines = []
        for line in output.split("\n"):
            line_upper = line.upper()
            if any(kw in line_upper for kw in self.STDERR_KEYWORDS):
                stderr_lines.append(line)

        return "\n".join(stderr_lines) if stderr_lines else ""


@pytest.fixture
def cli_runner():
    """Create a Click CliRunner for in-process CLI testing.

    This fixture provides a CliRunner instance that can be used to test
    Click-based CLI commands without subprocess overhead.

    Returns
    -------
    CliRunner
        Click test runner instance
    """
    return CliRunner()


def run_cli_command(runner, command, args, *, check=False):
    """Run a Click CLI command in-process and return a subprocess-compatible result.

    Parameters
    ----------
    runner : CliRunner
        Click test runner
    command : click.Command
        Click command to invoke
    args : tuple or list
        Command-line arguments
    check : bool, optional
        If True, raise CalledProcessError on non-zero exit code

    Returns
    -------
    CliResultAdapter
        Result with returncode, stdout, stderr attributes

    Raises
    ------
    subprocess.CalledProcessError
        If check=True and command returns non-zero exit code
    """
    click_result = runner.invoke(command, args, catch_exceptions=False)
    result = CliResultAdapter(click_result)

    if check and result.returncode != 0:
        raise subprocess.CalledProcessError(
            result.returncode,
            [command.name, *args],
            result.stdout,
            result.stderr,
        )

    return result
from supy._supy_module import (
    _init_config,
    _init_supy,
    _load_forcing_grid,
    _load_sample_data,
    _run_supy,
    _run_supy_sample,
    _save_supy,
)


def pytest_configure():
    """Monkey patch legacy public functions to private implementations for tests."""
    supy._deprecated_init_supy = supy.init_supy
    supy._deprecated_load_forcing_grid = supy.load_forcing_grid
    supy._deprecated_run_supy = supy.run_supy
    supy._deprecated_run_supy_sample = supy.run_supy_sample
    supy._deprecated_save_supy = supy.save_supy
    supy._deprecated_load_sample_data = supy.load_sample_data
    supy._deprecated_init_config = supy.init_config

    supy.init_supy = _init_supy
    supy.load_forcing_grid = _load_forcing_grid
    supy.load_sample_data = _load_sample_data
    supy.run_supy = _run_supy
    supy.run_supy_sample = _run_supy_sample
    supy.save_supy = _save_supy
    supy.init_config = _init_config


def pytest_collection_modifyitems(items):
    """Ensure test_sample_output runs first, then API equivalence tests, to avoid Fortran state interference.

    The sample output validation test must run FIRST before any other tests because:
    - The Fortran model maintains internal state between test runs
    - Even API equivalence tests can pollute the Fortran state
    - This causes small numerical differences that accumulate over simulations
    - Uninitialized memory from previous runs can pollute output arrays

    Test ordering:
    1. test_sample_output.py tests (highest priority - must be completely clean)
    2. API equivalence tests (need clean state, but less critical than sample output)
    3. All other tests
    """
    # Separate into three priority groups
    sample_output_tests = []
    api_equivalence_tests = []
    other_tests = []

    for item in items:
        # Priority 1: test_sample_output.py must run first
        if "test_sample_output" in str(item.fspath) or "core/test_sample_output" in str(
            item.fspath
        ):
            sample_output_tests.append(item)
        # Priority 2: API equivalence tests need clean state but less critical
        elif (
            "test_functional_matches_oop" in item.nodeid
            or "TestPublicAPIEquivalence" in item.nodeid
        ):
            api_equivalence_tests.append(item)
        # Priority 3: All other tests
        else:
            other_tests.append(item)

    # Run in priority order: sample_output → API equivalence → others
    items[:] = sample_output_tests + api_equivalence_tests + other_tests
