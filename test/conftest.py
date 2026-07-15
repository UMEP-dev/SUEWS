"""pytest configuration for SUEWS test suite."""

import subprocess
import sys
import warnings
from pathlib import Path

# Make the standalone suews_mcp package importable when it is not pip-installed
# (e.g. wheel CI installs only the supy wheel). Done at the root conftest rather
# than test/mcp/conftest.py because pytest's default `prepend` import mode gives
# every conftest.py the bare module name `conftest`, and a per-subdirectory
# conftest.py shadows this one — breaking `from conftest import TIMESTEPS_PER_DAY`
# in test/physics and test/umep during collection (gh#1384 follow-up).
_MCP_SRC = Path(__file__).resolve().parents[1] / "mcp" / "src"
if _MCP_SRC.is_dir() and str(_MCP_SRC) not in sys.path:
    sys.path.insert(0, str(_MCP_SRC))

import pytest
import supy
from click.testing import CliRunner


# =============================================================================
# Debug Decorators - Centralised to avoid duplication in test files
# =============================================================================

try:
    from debug_utils import (
        debug_on_ci,
        debug_dataframe_output,
        debug_water_balance,
        capture_test_artifacts,
        analyze_dailystate_nan,
    )
except ImportError:
    # No-op fallbacks when debug_utils is not available
    def debug_on_ci(func):
        return func

    def debug_dataframe_output(func):
        return func

    def debug_water_balance(func):
        return func

    def capture_test_artifacts(name):
        return lambda func: func

    def analyze_dailystate_nan(func):
        return func


# =============================================================================
# Global Test Configuration
# =============================================================================

# Named constants for test clarity
TIMESTEPS_PER_DAY = 288  # 24*60/5 = 288 five-minute intervals
SHORT_RUN_STEPS = 24  # Two hours; enough for lifecycle/output checks


@pytest.fixture
def cru_data_available():
    """Skip the test when CRU climatology data is not available.

    Several phase-B validation helpers (`get_mean_annual_air_temperature`,
    `get_mean_monthly_air_temperature`, `get_monthly_air_temperature_diffmax`)
    read from a CRU NetCDF that is not vendored with the repository. CI
    runners and contributor environments without the file should treat the
    dependent tests as skipped, not failed.

    The probe uses a known-good mid-latitude land cell (45 N, 10 E).
    """
    from supy.data_model.validation.pipeline.phase_b import (
        get_mean_annual_air_temperature,
    )

    if get_mean_annual_air_temperature(45.0, 10.0) is None:
        pytest.skip("CRU climatology data not available")


@pytest.fixture(autouse=True)
def suppress_import_warnings():
    """Suppress ImportWarning globally for all tests.

    This centralises the warning suppression that was previously
    duplicated in setUp methods across multiple test files.
    """
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=ImportWarning)
        yield


@pytest.fixture(autouse=True)
def _clear_mcp_caches():
    """Clear the per-process ``suews_mcp`` caches around every test.

    ``schema_search._SCHEMA_CACHE`` and ``readiness._SAMPLE_CACHE`` cache
    *successful* CLI envelopes for the process lifetime (correct in production),
    so a test that monkeypatches ``run_suews_cli`` with a fake success would
    poison them for later tests. Defined here, not in ``test/mcp/conftest.py``,
    for the same reason the MCP ``sys.path`` insertion lives at the top of this
    file: a per-subdirectory ``conftest.py`` shadows the bare ``conftest`` module
    name and breaks ``from conftest import ...`` in other test directories.
    No-op when ``suews_mcp`` is not importable (e.g. the supy-only wheel CI).
    """
    try:
        from suews_mcp.tools import readiness, schema_search

        caches = (schema_search._SCHEMA_CACHE, readiness._SAMPLE_CACHE)
    except ImportError:
        caches = ()
    for cache in caches:
        cache.clear()
    yield
    for cache in caches:
        cache.clear()


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
    """Run the import-surface probe first and preserve all other test order.

    Native simulation calls own fresh state at the Rust/Fortran boundary.  Tests
    that need continuation pass the previous state explicitly, so reference and
    API-equivalence tests no longer need collection-order protection.

    The removed workaround moved every ``test_sample_output.py`` item plus
    ``TestPublicAPIEquivalence`` and ``test_functional_matches_oop`` nodes.  The
    bounded real-node set has an executable collection regression in
    ``test_api_surface.py``.
    """
    api_surface_tests = []
    other_tests = []

    for item in items:
        if "test_api_surface" in str(item.fspath):
            api_surface_tests.append(item)
        else:
            other_tests.append(item)

    items[:] = api_surface_tests + other_tests


# =============================================================================
# gh#1300 — enforce the physics/api nature axis on full-tree runs.
# =============================================================================


def _invocation_targets_full_test_tree(config):
    """True when the pytest invocation covers the whole `test/` tree.

    The lint below should fire on CI-style invocations (`pytest test`,
    `pytest` from rootdir, or with marker-only filters) and stay out of the
    way during local iteration on a single file or subdirectory.
    """
    args = list(config.invocation_params.args or ())
    path_args = [a for a in args if not a.startswith("-")]
    if not path_args:
        return True
    test_dir = (Path(str(config.rootpath)) / "test").resolve()
    for arg in path_args:
        try:
            resolved = Path(arg).resolve()
        except (OSError, ValueError):
            return False
        if resolved != test_dir:
            return False
    return True


def pytest_collection_finish(session):
    """Enforce that every collected test carries `physics` or `api` (gh#1300).

    Rationale: the two-axis taxonomy only works if every file declares which
    axis it belongs to. A missing nature marker would silently drop a file
    out of CI once the matrix is split in a follow-up PR. Fail loudly at
    collection time on full-tree runs so the gap is caught before CI.
    """
    if not _invocation_targets_full_test_tree(session.config):
        return

    missing_files = []
    for item in session.items:
        marker_names = {m.name for m in item.iter_markers()}
        if "physics" in marker_names or "api" in marker_names:
            continue
        try:
            rel = Path(str(item.fspath)).resolve().relative_to(
                Path(str(session.config.rootpath)).resolve()
            )
        except ValueError:
            rel = Path(str(item.fspath))
        rel_str = str(rel)
        if rel_str not in missing_files:
            missing_files.append(rel_str)

    if missing_files:
        bullet = "\n  - ".join(missing_files)
        raise pytest.UsageError(
            "gh#1300 marker lint: these test files are missing both "
            "`physics` and `api` markers. Every test file must carry at "
            "least one of the two nature markers (use `pytestmark = "
            "pytest.mark.api` or `pytest.mark.physics` at module level, or "
            "auto-apply via `pytest_collection_modifyitems`):\n  - " + bullet
        )


# =============================================================================
# Session-scoped sample simulation fixtures
# =============================================================================
#
# The bundled sample simulation (KCL, 2011-2013, 105408 five-minute
# timesteps) costs on the order of 5-10 s locally and more on CI Windows to
# run. Many tests only need to READ a completed run's config, forcing, or
# output; they do not need to run the model themselves. These fixtures share
# a single run per test session for that read-only majority. Tests that
# exercise running, mutation, or lifecycle behaviour (reset, checkpoint
# continuation, chunking, parallelism, before/after-run state) must keep
# constructing and running their own `SUEWSSimulation` instance - do not
# retrofit them onto these fixtures.
#
# `sample_run_cached` extends the same idea to the functional API
# (`supy.run_supy`), memoising by forcing window rather than always the full
# range - see its own docstring for the copy contract.
#
# All five fixtures are lazy (nothing runs at import or collection time) and
# session-scoped, so each underlying sample run happens at most once per
# `pytest` invocation, however many tests request it.


@pytest.fixture(scope="session")
def sample_yaml_path() -> Path:
    """Path to the bundled SUEWS sample YAML configuration.

    Session-scoped since the path is fixed for the process lifetime.
    """
    return Path(supy.__file__).parent / "sample_data" / "sample_config.yml"


@pytest.fixture(scope="session")
def sample_data_loaded():
    """The bundled sample ``(df_state_init, df_forcing)``, loaded once.

    READ-ONLY: consumers MUST ``.copy()`` any frame they intend to mutate.
    Loading alone (no physics run) is cheap, but sharing it still avoids
    repeated file I/O and parsing across every test that only needs to read
    the sample state or forcing.
    """
    return supy.load_sample_data()


@pytest.fixture(scope="session")
def completed_sample_sim(sample_yaml_path):
    """A short ``SUEWSSimulation`` with ``.run()`` called once.

    READ-ONLY, shared across the whole test session: consumers must not call
    ``.reset()``, any ``update_*`` method, or ``.run()`` again on this
    instance, and must not mutate its frames (``config``, ``forcing``,
    ``state_init``, ``state_final``, the output DataFrame, ...) in place. A
    test that needs to mutate or re-run the simulation must construct its own
    instance instead, e.g. via ``SUEWSSimulation.from_sample_data()``.

    The consumers only require a completed lifecycle plus non-empty output;
    none validates the full sample period. Limit the shared run to 24 steps so
    it does not retain the 105,408-row sample output for the whole session.
    """
    sim = supy.SUEWSSimulation(str(sample_yaml_path))
    sim.update_forcing(sim.forcing.df.iloc[:SHORT_RUN_STEPS].copy())
    sim.run()
    return sim


@pytest.fixture(scope="session")
def sample_run_output(completed_sample_sim):
    """Convenience accessor for ``completed_sample_sim``'s ``SUEWSOutput``.

    Same READ-ONLY contract as ``completed_sample_sim``.
    """
    return completed_sample_sim.output


@pytest.fixture(scope="session")
def sample_run_cached(sample_data_loaded):
    """Session-scoped factory memoising functional-API sample runs by window.

    Returns a callable ``_run(n_steps=None)`` that runs
    ``supy.run_supy(df_forcing.iloc[:n_steps], df_state_init)`` on the
    bundled sample data (full range when ``n_steps`` is ``None``) and
    computes it at most once per distinct window for the session.

    CONTRACT:
    - Every call returns ``.copy()`` of both ``(df_output, df_state_final)``
      so no consumer can poison the cache by mutating what it gets back.
    - ``df_state_init`` is copied before being passed into ``run_supy``,
      since the run may mutate it.
    - Identical windows share one run across the whole session.
    - CAVEAT: the cache key is ``n_steps`` ONLY - any other run parameter
      (extra ``run_supy`` kwargs such as ``debug_mode``, or a mutated
      ``df_state_init``) is invisible to the cache, so a test that varies
      one of those must NOT go through this factory.
    - Use this only when a test consumes the OUTPUT of a run and does not
      itself test the act of running (deprecation warnings, kwargs like
      ``debug_mode``, multi-grid state, checkpoint continuation, ...).
    """
    df_state_init, df_forcing = sample_data_loaded
    cache: dict = {}

    def _run(n_steps=None):
        if n_steps not in cache:
            df_forcing_window = (
                df_forcing if n_steps is None else df_forcing.iloc[:n_steps]
            )
            cache[n_steps] = supy.run_supy(df_forcing_window, df_state_init.copy())
        df_output, df_state_final = cache[n_steps]
        return df_output.copy(), df_state_final.copy()

    return _run
