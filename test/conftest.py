"""pytest configuration for SUEWS test suite."""

from collections.abc import Iterator
import copy
from functools import lru_cache
import gzip
import importlib.metadata
from importlib.resources import as_file
from inspect import signature
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
from typing import NoReturn
import urllib.parse
import urllib.request
import warnings

# Make the standalone suews_mcp package importable when it is not pip-installed
# (e.g. wheel CI installs only the supy wheel). Done at the root conftest rather
# than test/mcp/conftest.py because pytest's default `prepend` import mode gives
# every conftest.py the bare module name `conftest`, and a per-subdirectory
# conftest.py shadows this one — breaking `from conftest import TIMESTEPS_PER_DAY`
# in test/physics and test/umep during collection (gh#1384 follow-up).
_MCP_SRC = Path(__file__).resolve().parents[1] / "mcp" / "src"
if _MCP_SRC.is_dir() and str(_MCP_SRC) not in sys.path:
    sys.path.insert(0, str(_MCP_SRC))

from click.testing import CliRunner
import pandas as pd
import pytest

import supy
from supy._env import trv_supy_module

# =============================================================================
# Debug Decorators - Centralised to avoid duplication in test files
# =============================================================================

try:
    from debug_utils import (
        analyze_dailystate_nan,
        capture_test_artifacts,
        debug_dataframe_output,
        debug_on_ci,
        debug_water_balance,
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

# Opt-in reference-run construction counter/logger.
# Set SUEWS_LOG_REFERENCE_RUNS=1 (or "true"/"stderr") to emit one ASCII log
# line per cached reference-run build to stderr, or set it to a file path to
# append one JSON line per build (safe across xdist workers).
REFERENCE_RUN_LOG_ENV = "SUEWS_LOG_REFERENCE_RUNS"
_REFERENCE_RUN_COUNTS: dict[tuple[str, str], int] = {}


def _record_reference_run(fixture_name: str, key: str) -> None:
    """Record one session-scoped reference-run construction (cache miss)."""
    count_key = (fixture_name, key)
    _REFERENCE_RUN_COUNTS[count_key] = _REFERENCE_RUN_COUNTS.get(count_key, 0) + 1

    target = os.environ.get(REFERENCE_RUN_LOG_ENV, "").strip()
    if not target or target.lower() in {"0", "false", "no", "off"}:
        return

    worker = os.environ.get("PYTEST_XDIST_WORKER", "master")
    pid = os.getpid()
    if target.lower() in {"1", "true", "yes", "stderr"}:
        sys.stderr.write(
            f"[ref-run] worker={worker} pid={pid} "
            f"fixture={fixture_name} key={key}\n"
        )
        sys.stderr.flush()
        return

    record = {
        "worker": worker,
        "pid": pid,
        "fixture": fixture_name,
        "key": key,
    }
    path_log = Path(target)
    path_log.parent.mkdir(parents=True, exist_ok=True)
    with open(path_log, "a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, ensure_ascii=True) + "\n")


def get_reference_run_counts() -> dict[tuple[str, str], int]:
    """Return a snapshot of in-process cached reference-run build counts."""
    return dict(_REFERENCE_RUN_COUNTS)


def reset_reference_run_counts() -> None:
    """Clear the in-process reference-run build counter (for unit tests)."""
    _REFERENCE_RUN_COUNTS.clear()


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


@pytest.fixture(autouse=True, scope="session")
def _silence_platform_support_notice():
    """Keep the once-per-process platform notice out of the test suite.

    On macOS Intel the first simulation in each process warns that the
    platform has reduced test coverage. Which test runs first in a worker is
    arbitrary, so a test asserting on the warnings a run emits would fail on
    that platform alone. Marking the notice as shown keeps every platform
    alike; ``test/core/test_platform_notice.py`` tests the notice itself.
    """
    import supy._env as env

    shown = env._platform_notice_shown
    env._platform_notice_shown = True
    yield
    env._platform_notice_shown = shown


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
def _authorise_mcp_test_roots(request, monkeypatch):
    """Explicit launcher authorisation for temporary MCP test projects."""
    if request.path.parent.name != "mcp":
        return
    import json

    tmp_path = request.getfixturevalue("tmp_path")
    monkeypatch.delenv("SUEWS_MCP_PROJECT_ROOT", raising=False)
    monkeypatch.setenv(
        "SUEWS_MCP_ALLOWED_ROOTS",
        json.dumps([str(tmp_path), str(_MCP_SRC.parents[1])]),
    )


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
    """Expose Click's separate captured streams with subprocess-style status.

    Preserve the actual streams, including whitespace and continuation lines.
    Diagnostic keywords cannot identify which stream a command wrote to.
    """

    def __init__(self, click_result):
        self.returncode = click_result.exit_code
        self.stdout = click_result.stdout
        self.stderr = click_result.stderr


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
    # Click <8.2 requires opting out of mixed stderr; newer versions always
    # capture both streams separately and have removed this argument.
    if "mix_stderr" in signature(CliRunner).parameters:
        return CliRunner(mix_stderr=False)
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


@lru_cache(maxsize=1)
def _parse_sample_frames():
    """Parse the bundled sample once per process and cache the frames.

    Constructing a ``SUEWSSimulation`` from the shipped sample YAML reads
    8784 hourly forcing rows and disaggregates them to 105408 five-minute
    rows. That parse is identical every time and dominates the wall clock of
    the wrapper test files, so it is memoised for the process (which for
    pytest means the session, matching the session-scoped fixtures below).

    PRIVATE and READ-ONLY: the frames returned here are the shared cache
    itself. Go through ``load_sample_frames`` or ``sample_forcing_parsed``,
    both of which copy on hand-out.
    """
    simulation = supy.SUEWSSimulation.from_sample_data()
    return simulation.state_init, simulation.forcing.df


def load_sample_frames():
    """Return copies of the bundled sample state and forcing frames.

    Copy-on-return, so callers may mutate freely. The underlying parse is
    shared across the session via ``_parse_sample_frames``.
    """
    df_state_init, df_forcing = _parse_sample_frames()
    return df_state_init.copy(), df_forcing.copy()


def run_simulation(
    df_forcing,
    df_state_init,
    *,
    chunk_day=3660,
    serial_mode=False,
    validate_forcing=True,
):
    """Run DataFrame fixtures through the supported OOP interface."""
    simulation = supy.SUEWSSimulation.from_state(df_state_init)
    if validate_forcing:
        simulation.update_forcing(df_forcing)
    else:
        simulation._df_forcing = df_forcing
    output = simulation.run(
        chunk_day=chunk_day,
        n_jobs=1 if serial_mode else -1,
        _validate_forcing=validate_forcing,
    )
    return output.df, output.state_final


def pytest_collection_modifyitems(items):
    """Run the import-surface probe first and preserve all other test order.

    Native simulation calls own fresh state at the Rust/Fortran boundary.  Tests
    that need continuation pass the previous state explicitly, so reference and
    output-reference tests no longer need collection-order protection.

    The removed workaround moved every ``test_sample_output.py`` item. The
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
    `pytest` from rootdir, or with option filters such as
    `pytest -m physics test/`) and stay out of the way during local
    iteration on a single file or subdirectory.

    Uses pytest's parsed positional `config.args` rather than raw
    `invocation_params.args` so option values (e.g. `physics` in
    `-m physics test/`) are never mistaken for path arguments.
    """
    rootpath = Path(str(config.rootpath)).resolve()
    test_dir = (rootpath / "test").resolve()
    invocation_dir = Path(str(config.invocation_params.dir)).resolve()

    path_args = list(getattr(config, "args", ()) or ())
    if not path_args:
        return True

    full_tree_targets = {rootpath, test_dir}
    for arg in path_args:
        if "::" in str(arg):
            return False
        raw = Path(arg)
        candidate = raw if raw.is_absolute() else invocation_dir / raw
        try:
            resolved = candidate.resolve()
        except (OSError, ValueError):
            return False
        if resolved not in full_tree_targets:
            return False
    return True


def pytest_deselected(items):
    """Record items deselected by ``-m``/``-k`` so full-tree marker lint sees them.

    Built-in mark/keyword filtering runs in ``pytest_collection_modifyitems``
    before ``pytest_collection_finish``, removing unmarked tests from
    ``session.items`` on invocations such as ``pytest -m physics test/``.
    Capturing deselected items here lets ``pytest_collection_finish`` check
    the full collected set after all ``pytest_collection_modifyitems`` hooks
    (including directory-level auto-markers such as ``test/umep/conftest.py``)
    have run (gh#2092).
    """
    for item in items:
        bucket = getattr(item.session, "_gh1300_deselected_items", None)
        if bucket is None:
            bucket = []
            item.session._gh1300_deselected_items = bucket
        bucket.append(item)


def pytest_collection_finish(session):
    """Enforce that every collected test carries `physics` or `api` (gh#1300).

    Rationale: the composed marker taxonomy only works if every file declares
    its nature. A missing nature marker would silently drop a file
    out of CI once the matrix is split in a follow-up PR. Fail loudly at
    collection time on full-tree runs so the gap is caught before CI.
    """
    if not _invocation_targets_full_test_tree(session.config):
        return

    all_items = list(session.items)
    all_items.extend(getattr(session, "_gh1300_deselected_items", ()))

    missing_files = []
    for item in all_items:
        marker_names = {m.name for m in item.iter_markers()}
        if "physics" in marker_names or "api" in marker_names:
            continue
        try:
            rel = (
                Path(str(item.fspath))
                .resolve()
                .relative_to(Path(str(session.config.rootpath)).resolve())
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
# `sample_run_cached` extends the same idea to the OOP simulation runtime,
# memoising by forcing window rather than always the full
# range - see its own docstring for the copy contract.
#
# `sample_forcing_parsed` shares the same parse copy-on-hand-out, for tests
# that need to mutate the frames; `short_sample_yaml_path` and
# `short_sample_sim` sidestep the full-year parse altogether, for tests that
# need a working simulation rather than a year of climate.
# `sample_dailystate_full_year` is the same idea for the one window that is
# too wide to memoise whole: it runs the full year once and keeps only the
# DailyState group.
#
# Every fixture here is lazy (nothing runs at import or collection time) and
# session-scoped, so each underlying sample parse or run happens at most once
# per `pytest` invocation, however many tests request it.


@pytest.fixture(scope="session")
def sample_yaml_path() -> Iterator[Path]:
    """Path to the bundled SUEWS sample YAML configuration.

    Session-scoped since the path is fixed for the process lifetime.
    ``as_file`` materialises the packaged resource as a real filesystem
    path, which is what the consumers below hand to ``str()``-taking APIs.

    The **directory** is materialised, not just the config: the config names
    its forcing file as a bare sibling (``Kc_2012_data_60.txt``), so a
    file-only ``as_file`` would hand back a config whose forcing cannot
    resolve under any non-filesystem loader.
    """
    with as_file(trv_supy_module / "sample_data") as path_sample_dir:
        yield path_sample_dir / "sample_config.yml"


@pytest.fixture(scope="session")
def suews_engine_bin() -> Iterator[Path]:
    """Path to the bundled or locally built ``suews-engine`` CLI binary.

    Probes ``Traversable.is_file()`` before entering ``as_file`` so an absent
    non-filesystem resource falls through cleanly to the local release build or
    documented skip, keeps the materialisation context open for the session
    lifetime, and ensures owner-execute permission on POSIX if extracted to a
    temporary file.
    """
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = trv_supy_module / "bin" / binary_name
    if bundled.is_file():
        with as_file(bundled) as bundled_path:
            path = Path(bundled_path)
            if os.name != "nt" and not os.access(path, os.X_OK):
                path.chmod(path.stat().st_mode | stat.S_IXUSR)
            yield path
            return

    dev_binary = (
        Path(__file__).resolve().parents[1]
        / "src"
        / "suews_bridge"
        / "target"
        / "release"
        / binary_name
    )
    if dev_binary.exists():
        yield dev_binary
        return
    pytest.skip("Rust CLI binary not found; build with: make bridge")


# =============================================================================
# Native Fortran Probe Availability Policy (gh#2349 W1-1)
# =============================================================================

_REPO_ROOT = Path(__file__).resolve().parents[1]


def _is_editable_source_install(repo_root: Path = _REPO_ROOT) -> bool:
    """Return True when ``supy`` is installed editably from *repo_root*.

    Reads PEP 610 ``direct_url.json`` from distribution metadata rather than
    inspecting ``supy.__file__`` (forbidden under ``test/`` by
    ``scripts/lint/check_packaged_data_paths.py``). Returns False for
    installed wheels (PyPI or ``wheelhouse/*.whl``) and for editable installs
    pointing at another checkout.
    """
    try:
        dist = importlib.metadata.distribution("supy")
    except importlib.metadata.PackageNotFoundError:
        return False

    direct_entry = next(
        (entry for entry in (dist.files or ()) if entry.name == "direct_url.json"),
        None,
    )
    dist_path = getattr(dist, "_path", None)
    direct_path = (
        Path(dist.locate_file(direct_entry))
        if direct_entry is not None
        else (Path(dist_path) / "direct_url.json" if dist_path is not None else None)
    )
    if direct_path is None or not direct_path.is_file():
        return False

    try:
        payload = json.loads(direct_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return False

    dir_info = payload.get("dir_info")
    url = payload.get("url")
    if (
        not isinstance(dir_info, dict)
        or not dir_info.get("editable")
        or not isinstance(url, str)
        or not url.startswith("file://")
    ):
        return False

    parsed = urllib.parse.urlparse(url)
    raw_path = (
        f"//{parsed.netloc}{parsed.path}"
        if parsed.netloc and parsed.netloc.lower() != "localhost"
        else parsed.path
    )
    install_path = Path(urllib.request.url2pathname(raw_path))
    try:
        return install_path.resolve() == repo_root.resolve()
    except OSError:
        return False


def _has_source_build_artifacts(repo_root: Path = _REPO_ROOT) -> bool:
    """Return True when *repo_root* contains compiled Fortran build artefacts.

    Distinguishes a partial or non-editable source build (which has compiled
    archives, module files, object files, or a build fingerprint under
    ``src/suews/``) from a clean repository checkout testing an installed wheel
    (which has only tracked source files and no compiled artefacts).
    """
    suews_dir = repo_root / "src" / "suews"
    lib_dir = suews_dir / "lib"
    mod_dir = suews_dir / "mod"
    src_dir = suews_dir / "src"
    ext_lib_dir = suews_dir / "ext_lib"
    return (
        (suews_dir / ".suews_build_fingerprint").is_file()
        or (lib_dir.is_dir() and any(lib_dir.glob("*.a")))
        or (mod_dir.is_dir() and any(mod_dir.glob("*.mod")))
        or (src_dir.is_dir() and any(src_dir.glob("*.o")))
        or (ext_lib_dir.is_dir() and any(ext_lib_dir.rglob("*.o")))
    )


def _fortran_probes_required_by_env() -> bool:
    """Return True when CI environment variables require Fortran probes."""
    return (
        os.environ.get("SUEWS_REQUIRE_FORTRAN_PROBES") == "1"
        or os.environ.get("SUEWS_FORTRAN_PROBES_PREBUILT") == "1"
    )


def fortran_probes_required(repo_root: Path = _REPO_ROOT) -> bool:
    """Return True when missing Fortran probe prerequisites must fail loudly.

    Probes are mandatory in CI reference and prebuilt-probe lanes
    (``SUEWS_REQUIRE_FORTRAN_PROBES=1`` or
    ``SUEWS_FORTRAN_PROBES_PREBUILT=1``), in editable source builds of
    *repo_root*, and whenever *repo_root* already holds compiled Fortran
    source-build artefacts under ``src/suews/``. Only clean installed-wheel
    runs without a source build may skip probe tests.
    """
    return (
        _fortran_probes_required_by_env()
        or _is_editable_source_install(repo_root)
        or _has_source_build_artifacts(repo_root)
    )


_RECOGNISED_NON_GNU_COMPILER_TOKENS = (
    "ifort",
    "ifx",
    "intel",
    "flang",
    "nvfortran",
    "pgfortran",
    "nagfor",
)
_STATIC_ARCHIVE_MAGICS = (b"!<arch>\n", b"!<thin>\n")
_MIN_NON_GNU_MOD_HEADER_BYTES = 16


def _is_non_gnu_fortran_build(suews_dir: Path) -> bool:
    """Return True when generated build metadata positively names a non-GNU compiler.

    Reads ``src/suews/src/suews_ctrl_ver.f95`` (written by ``src/suews/Makefile``
    from ``$(FC) --version``) rather than ambient environment variables, so an
    unrelated shell ``PROFILE`` cannot override a GNU build or mask corruption.
    """
    ver_file = suews_dir / "src" / "suews_ctrl_ver.f95"
    if not ver_file.is_file():
        return False
    try:
        text = ver_file.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return False
    match = re.search(r"compiler_ver\s*=\s*'([^']*)'", text)
    if not match:
        return False
    compiler_ver = match.group(1).strip().lower()
    if not compiler_ver or "gnu fortran" in compiler_ver or "gfortran" in compiler_ver:
        return False
    return any(token in compiler_ver for token in _RECOGNISED_NON_GNU_COMPILER_TOKENS)


def _is_valid_static_archive(archive_path: Path) -> bool:
    """Return True when *archive_path* is a readable Unix/MinGW ``ar`` archive."""
    try:
        with archive_path.open("rb") as stream:
            magic = stream.read(8)
    except OSError:
        return False
    return magic in _STATIC_ARCHIVE_MAGICS


def _classify_fortran_module(mod_path: Path, suews_dir: Path) -> str:
    """Classify a Fortran ``.mod`` file as missing, gnu, non_gnu, or corrupt."""
    if not mod_path.is_file():
        return "missing"
    try:
        with mod_path.open("rb") as stream:
            header = stream.read(_MIN_NON_GNU_MOD_HEADER_BYTES)
    except OSError:
        header = b""
    if not header:
        return "corrupt"
    if header.startswith(b"\x1f\x8b"):
        try:
            with gzip.open(mod_path, "rb") as gz_stream:
                is_gnu = gz_stream.readline().startswith(b"GFORTRAN")
        except (gzip.BadGzipFile, EOFError, OSError):
            is_gnu = False
        return "gnu" if is_gnu else "corrupt"
    if header.startswith(b"GFORTRAN"):
        return "gnu"
    if header.startswith((b"\xef\xbb\xbf!mod$", b"!mod$")):
        return "non_gnu"
    is_non_gnu = len(
        header
    ) >= _MIN_NON_GNU_MOD_HEADER_BYTES and _is_non_gnu_fortran_build(suews_dir)
    return "non_gnu" if is_non_gnu else "corrupt"


def fortran_probe_unavailable(
    reason: str,
    *,
    repo_root: Path = _REPO_ROOT,
    require_env_only: bool = False,
) -> NoReturn:
    """Fail when Fortran probes are required, otherwise skip with *reason*."""
    required = (
        _fortran_probes_required_by_env()
        if require_env_only
        else fortran_probes_required(repo_root)
    )
    if required:
        pytest.fail(reason)
    pytest.skip(reason)


def require_fortran_probe_toolchain(
    description: str,
    *,
    libraries: tuple[str, ...] = ("libsuewsphys.a",),
    modules: tuple[str, ...] = (),
    source_dir: Path | None = None,
    repo_root: Path = _REPO_ROOT,
) -> tuple[str, Path]:
    """Return ``(gfortran_path, suews_dir)`` or fail/skip per probe policy.

    Parameters
    ----------
    description : str
        Human-readable probe label used in failure/skip diagnostics.
    libraries : tuple of str, optional
        Static archive filenames required under ``src/suews/lib/``. Pass ``()``
        for probes that compile directly from Fortran source files.
    modules : tuple of str, optional
        GNU Fortran ``.mod`` filenames required under ``src/suews/mod/``. Each
        must exist and carry a valid ``GFORTRAN`` header.
    source_dir : Path or None, optional
        Optional Fortran source directory that must exist for direct-source
        probes.
    repo_root : Path, optional
        Repository root directory; defaults to the enclosing checkout.
    """
    suews_dir = repo_root / "src" / "suews"

    if source_dir is not None and not source_dir.is_dir():
        fortran_probe_unavailable(
            f"{description} requires Fortran sources at {source_dir}; "
            "model/driver tests cover installed wheels",
            repo_root=repo_root,
        )

    missing_libs = [lib for lib in libraries if not (suews_dir / "lib" / lib).is_file()]
    if missing_libs:
        fortran_probe_unavailable(
            f"{description} requires source-build libraries "
            f"({', '.join(missing_libs)} missing under {suews_dir / 'lib'}); "
            "model/driver tests cover installed wheels",
            repo_root=repo_root,
        )

    corrupt_libs = [
        lib
        for lib in libraries
        if not _is_valid_static_archive(suews_dir / "lib" / lib)
    ]
    if corrupt_libs:
        fortran_probe_unavailable(
            f"{description} found unreadable or corrupt static archive(s) "
            f"{', '.join(corrupt_libs)} under {suews_dir / 'lib'}",
            repo_root=repo_root,
        )

    missing_mods: list[str] = []
    corrupt_mods: list[str] = []
    non_gnu_mods: list[str] = []
    for mod_name in modules:
        status = _classify_fortran_module(suews_dir / "mod" / mod_name, suews_dir)
        if status == "missing":
            missing_mods.append(mod_name)
        elif status == "corrupt":
            corrupt_mods.append(mod_name)
        elif status == "non_gnu":
            non_gnu_mods.append(mod_name)

    if missing_mods:
        fortran_probe_unavailable(
            f"{description} requires GNU Fortran module file(s) "
            f"{', '.join(missing_mods)} under {suews_dir / 'mod'}; "
            "model/driver tests cover installed wheels",
            repo_root=repo_root,
        )
    if corrupt_mods:
        fortran_probe_unavailable(
            f"{description} found unreadable or corrupt Fortran module file(s) "
            f"{', '.join(corrupt_mods)} under {suews_dir / 'mod'}",
            repo_root=repo_root,
        )
    if non_gnu_mods:
        # A recognised non-GNU local build (e.g. Intel Fortran or Flang)
        # cannot be read by gfortran; fail in CI probe lanes, skip locally.
        fortran_probe_unavailable(
            f"{description} requires GNU Fortran module files "
            f"({', '.join(non_gnu_mods)} from a non-GNU Fortran build)",
            repo_root=repo_root,
            require_env_only=True,
        )

    compiler = shutil.which("gfortran")
    if compiler is None:
        if _is_non_gnu_fortran_build(suews_dir):
            fortran_probe_unavailable(
                f"{description} requires gfortran on PATH "
                "(active build uses a non-GNU Fortran compiler)",
                repo_root=repo_root,
                require_env_only=True,
            )
        fortran_probe_unavailable(
            f"{description} requires gfortran on PATH; "
            "model/driver tests cover installed wheels",
            repo_root=repo_root,
        )

    return compiler, suews_dir


@pytest.fixture(scope="session")
def sample_ohm_48h_qs(sample_config_loaded, sample_data_loaded, tmp_path_factory):
    """Factory returning defensive copies of a 48-hour public-path OHM QS reference.

    Runs the sample configuration with ``storage_heat=1`` (ordinary OHM) once
    per session through the public YAML -> ``SUEWSSimulation`` path over the
    first 48 hours (576 five-minute steps) and hands each caller a fresh deep
    copy of ``SUEWS`` ``QS`` and ``QS_Bldgs`` so shared session state cannot be
    mutated across tests.
    """
    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.model.physics.storage_heat = 1
    forcing = sample_data_loaded[1].iloc[: 2 * TIMESTEPS_PER_DAY].copy()
    path = tmp_path_factory.mktemp("ohm_48h_control") / "case.yml"
    cfg.to_yaml(path)
    sim = supy.SUEWSSimulation()
    sim.update_config(path, auto_load_forcing=False)
    sim.update_forcing(forcing)
    output = sim.run(
        start_date=forcing.index[0],
        end_date=forcing.index[-1],
        n_jobs=1,
        chunk_day=3660,
    ).df
    reference = output["SUEWS"][["QS", "QS_Bldgs"]].copy(deep=True)

    def _copy() -> pd.DataFrame:
        return reference.copy(deep=True)

    return _copy


@pytest.fixture(scope="session")
def continuation_reference_cache(tmp_path_factory):
    """Session-scoped cache of uninterrupted continuation reference runs.

    Runs each uninterrupted 48-hour reference simulation and its persisted
    Parquet representation at most once per session (keyed by mode name),
    releases the ``SUEWSSimulation`` instance immediately, and hands every
    caller fresh deep copies so shared session references remain isolated
    across ``chunk``, ``checkpoint``, and ``stream`` mechanisms.
    """
    cache_root = tmp_path_factory.mktemp("continuation_references")
    cache: dict[str, tuple[pd.DataFrame, pd.DataFrame]] = {}

    def _get(
        key: str,
        yaml_path: Path,
        forcing: pd.DataFrame,
    ) -> tuple[pd.DataFrame, pd.DataFrame]:
        if key not in cache:
            sim = supy.SUEWSSimulation()
            sim.update_config(yaml_path, auto_load_forcing=False)
            sim.update_forcing(forcing.copy())
            native = sim.run(
                start_date=forcing.index[0],
                end_date=forcing.index[-1],
                n_jobs=1,
                chunk_day=3660,
            ).df
            assert native.index.is_unique, "Duplicate grid/timestamp output rows"
            pd.testing.assert_index_equal(
                native.index.get_level_values("datetime"),
                forcing.index,
                check_names=False,
            )
            groups = [
                group
                for group in native.columns.get_level_values(0).unique()
                if group != "BL"
            ]
            sim.config.model.control.output.freq = 300
            sim.config.model.control.output.groups = groups
            out_dir = cache_root / key
            sim.save(out_dir, format="parquet", include_provisional=True)
            (saved,) = out_dir.glob("*SUEWS_output*.parquet")
            persisted = pd.read_parquet(saved).sort_index()
            cache[key] = (native.copy(deep=True), persisted.copy(deep=True))
        native_ref, persisted_ref = cache[key]
        return native_ref.copy(deep=True), persisted_ref.copy(deep=True)

    return _get


@pytest.fixture(scope="session")
def sample_config_loaded(sample_yaml_path):
    """The validated sample config, without loading its full forcing file.

    READ-ONLY: consumers that mutate the config MUST take a deep model copy.
    This supports wrapper tests whose execution backend is mocked: they need
    a structurally real config, but repeatedly parsing 105,408 forcing rows
    would add no coverage to their argument-forwarding contract.
    """
    from supy.data_model import SUEWSConfig

    return SUEWSConfig.from_yaml(str(sample_yaml_path))


@pytest.fixture(scope="session")
def sample_data_loaded():
    """The bundled sample ``(df_state_init, df_forcing)``, loaded once.

    READ-ONLY: consumers MUST ``.copy()`` any frame they intend to mutate.
    Loading alone (no physics run) is cheap, but sharing it still avoids
    repeated file I/O and parsing across every test that only needs to read
    the sample state or forcing.

    Backed by the same process-wide parse as ``load_sample_frames`` and
    ``sample_forcing_parsed``, so the sample is parsed at most once however a
    test reaches for it.
    """
    return _parse_sample_frames()


@pytest.fixture(scope="session")
def sample_forcing_parsed():
    """Factory handing out private copies of the parsed sample frames.

    Returns a callable ``_frames(n_steps=None)`` yielding
    ``(df_state_init, df_forcing)`` copies -- the whole 105408-row forcing
    when ``n_steps`` is ``None``, otherwise its first ``n_steps`` rows. The
    underlying parse happens at most once per session; each call copies, so a
    consumer may mutate what it gets back and cannot poison the cache. This is
    the copy-on-hand-out counterpart to ``sample_data_loaded``, which shares
    the frames read-only.

    Use it to build a simulation without re-parsing the sample file::

        df_state, df_forcing = sample_forcing_parsed(TIMESTEPS_PER_DAY)
        sim = SUEWSSimulation.from_state(df_state)
        sim.update_forcing(df_forcing)

    ``from_state`` gives no config, so ``run()`` has no configured period and
    runs exactly the forcing loaded. A test that needs a real config as well
    wants ``short_sample_yaml_path`` instead, whose config asks only for the
    month its forcing covers.
    """

    def _frames(n_steps=None):
        df_state_init, df_forcing = _parse_sample_frames()
        window = df_forcing if n_steps is None else df_forcing.iloc[:n_steps]
        return df_state_init.copy(), window.copy()

    return _frames


@pytest.fixture(scope="session")
def short_sample_yaml_path() -> Path:
    """Path to the one-month (January 2012) short sample configuration.

    A committed fixture under ``test/fixtures/sample_short/``: the shipped
    sample config with only its name, description, ``end_time`` and forcing
    file changed, pointing at a 744-row hourly forcing file carved from the
    shipped year. Constructing a ``SUEWSSimulation`` from it costs roughly a
    twelfth of the full sample, so lifecycle tests that need a working
    simulation -- config, forcing, a runnable period -- do not pay for a year
    of climate they never look at.

    Because the config's ``end_time`` matches its forcing, a test that asks
    for a period beyond January fails loudly at ``run()``
    (``supy._run_period._check_coverage`` raises unless
    ``clip_to_forcing=True``) rather than silently running on less data.

    Both artefacts are regenerated by
    ``test/fixtures/sample_short/make_sample_short.py``; the parity test in
    ``test/core/test_sample_short_fixture.py`` fails if they drift from the
    shipped sample.
    """
    return (
        Path(__file__).parent / "fixtures" / "sample_short" / "sample_config_short.yml"
    )


@pytest.fixture(scope="session")
def short_sample_sim(short_sample_yaml_path):
    """Factory building a fresh simulation from the one-month short sample.

    Returns a callable ``_sim()`` handing back a brand-new, un-run
    ``SUEWSSimulation`` with the short sample's config and its 8928
    five-minute forcing rows loaded. Each call constructs a separate
    instance, so unlike ``completed_sample_sim`` the result may be mutated,
    reset, re-run and continued from freely -- which is what the lifecycle
    tests need.

    Use it wherever a test previously called
    ``SUEWSSimulation.from_sample_data()`` only to get *a working
    simulation*. Keep the full sample where the loader itself is the
    contract under test.
    """

    def _sim():
        return supy.SUEWSSimulation(str(short_sample_yaml_path))

    return _sim


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
    df_forcing_short = sim.forcing.df.iloc[:SHORT_RUN_STEPS].copy()
    sim.update_forcing(df_forcing_short)
    # The YAML requests the full sample year; bound the run explicitly so the
    # truncated forcing is a covered request rather than a silent clip.
    _record_reference_run("completed_sample_sim", f"n_steps={SHORT_RUN_STEPS}")
    sim.run(end_date=df_forcing_short.index[-1])
    return sim


@pytest.fixture(scope="session")
def sample_run_output(completed_sample_sim):
    """Convenience accessor for ``completed_sample_sim``'s ``SUEWSOutput``.

    Same READ-ONLY contract as ``completed_sample_sim``.
    """
    return completed_sample_sim.output


@pytest.fixture(scope="session")
def sample_dailystate_full_year(sample_data_loaded):
    """The DailyState group of one full-year sample run, kept without the rest.

    READ-ONLY, shared across the whole test session: consumers must not mutate
    the returned frame.

    Rows are the day boundaries at which DailyState is written; the all-NaN
    timesteps in between are already dropped, which is what every consumer did
    for itself.

    Why this is not ``sample_run_cached()``: that factory memoises the whole
    output frame for the session and hands each caller a ``.copy()`` of it. The
    full-year window is 105,408 rows across roughly 1,295 columns, about 1.1 GB
    per frame, so five DailyState tests going through the factory kept one full
    frame alive for the session and built a second on every call while reading
    47 DailyState columns on 366 day-boundary rows. Projecting the group here
    lets the wide frame be released as soon as the run returns.
    """
    import gc

    _record_reference_run("sample_dailystate_full_year", "full_year")
    df_state_init, df_forcing = sample_data_loaded
    simulation = supy.SUEWSSimulation.from_state(df_state_init.copy())
    simulation.update_forcing(df_forcing)
    output = simulation.run()
    df_dailystate = output.df.loc[:, "DailyState"].dropna(how="all").copy()
    del output, simulation
    gc.collect()
    return df_dailystate


@pytest.fixture(scope="session")
def sample_reference() -> pd.DataFrame:
    """The one-year sample-output reference frame, parsed once per session.

    Reconstructed from the twelve monthly plain-CSV shards under
    ``test/fixtures/data_test/`` by ``sample_output_io.load_sample_output``.
    Parsing them costs a few seconds of pure CSV work, and every reader wants
    the same immutable frame, so it is session-scoped rather than paid per
    test.

    READ-ONLY, shared across the whole session: consumers must not mutate the
    frame in place. Slice it (``.iloc[...]``) or ``.copy()`` before changing
    anything.

    Under pandas copy-on-write the frame is largely protected by construction:
    the column arrays it hands out are read-only
    (``df[col].values.flags.writeable`` is ``False``), so an in-place write
    through a column raises rather than corrupting the shared frame. What is
    still not protected is column assignment on the frame itself
    (``df[col] = ...``, ``df.drop(..., inplace=True)``), which would leak to
    every later consumer -- hence the contract above. No per-test copy is made
    deliberately: copying 105,408 rows x 113 columns per test would cost what
    the session-scoped parse saves.
    """
    import sys

    data_dir = Path(__file__).parent / "fixtures" / "data_test"
    # Same path insertion the fixture's own readers use: the module lives
    # beside the shards so the split/combine convention has one home.
    if str(data_dir) not in sys.path:
        sys.path.insert(0, str(data_dir))
    from sample_output_io import load_sample_output

    return load_sample_output(data_dir)


@pytest.fixture(scope="session")
def sample_reference_dir() -> Path:
    """Directory holding the sample-output shards and their provenance sidecar."""
    return Path(__file__).parent / "fixtures" / "data_test"


@pytest.fixture(scope="session")
def sample_run_cached(sample_data_loaded):
    """Session-scoped factory memoising OOP sample runs by window.

    Returns a callable ``_run(n_steps=None)`` that runs
    ``SUEWSSimulation.run()`` on the
    bundled sample data (full range when ``n_steps`` is ``None``) and
    computes it at most once per distinct window for the session.

    CONTRACT:
    - Every call returns ``.copy()`` of both ``(df_output, df_state_final)``
      so no consumer can poison the cache by mutating what it gets back.
    - ``df_state_init`` is copied before creating the simulation.
    - Identical windows share one run across the whole session.
    - CAVEAT: the cache key is ``n_steps`` ONLY - any other run parameter
      (extra run options or a mutated
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
            _record_reference_run("sample_run_cached", f"n_steps={n_steps}")
            df_forcing_window = (
                df_forcing if n_steps is None else df_forcing.iloc[:n_steps]
            )
            simulation = supy.SUEWSSimulation.from_state(df_state_init.copy())
            simulation.update_forcing(df_forcing_window)
            output = simulation.run()
            cache[n_steps] = output.df, output.state_final
        df_output, df_state_final = cache[n_steps]
        return df_output.copy(), df_state_final.copy()

    return _run


def _normalise_period_key(value):
    """Build a hashable cache key preserving date-only strings vs Timestamps.

    ``SUEWSSimulation.run`` treats a date-only string such as ``"2012-01-01"``
    differently from ``pd.Timestamp("2012-01-01")``, so the key includes the
    runtime type name alongside the string value.
    """
    if value is None:
        return None
    return (type(value).__qualname__, str(value))


def _clone_suews_output(output):
    """Return an independent deep copy of a ``SUEWSOutput`` instance.

    Every mutable component (``_df_output`` and its ``attrs``,
    ``_df_state_final``, ``_config``, ``_checkpoint``, ``_metadata``, and
    ``_kernel_warnings``) is copied so a test cannot poison a session-scoped
    cache entry by mutating any attribute or DataFrame in place.
    """
    df_output = output._df_output.copy(deep=True)
    df_output.attrs = copy.deepcopy(output._df_output.attrs)
    df_state_final = (
        output._df_state_final.copy(deep=True)
        if output._df_state_final is not None
        else None
    )
    config = (
        output._config.model_copy(deep=True)
        if output._config is not None and hasattr(output._config, "model_copy")
        else copy.deepcopy(output._config)
    )
    checkpoint = copy.deepcopy(output._checkpoint)
    metadata = copy.deepcopy(output._metadata)
    kernel_warnings = (
        output._kernel_warnings.copy(deep=True)
        if output._kernel_warnings is not None
        else None
    )
    return supy.SUEWSOutput(
        df_output=df_output,
        df_state_final=df_state_final,
        config=config,
        metadata=metadata,
        checkpoint=checkpoint,
        kernel_warnings=kernel_warnings,
        streamed_paths=list(output._streamed_paths)
        if output._streamed_paths is not None
        else None,
    )


@pytest.fixture(scope="session")
def sample_config_run_cached():
    """Session-scoped factory memoising ``SUEWSSimulation.from_sample_data().run(...)``.

    Unlike ``sample_run_cached`` (which runs ``SUEWSSimulation.from_state``
    without a ``SUEWSConfig``), this factory constructs the simulation via
    ``SUEWSSimulation.from_sample_data()`` and runs ``run(start_date=...,
    end_date=...)`` once per distinct period per session, returning a complete
    ``SUEWSOutput`` (including file-based config and forcing provenance
    metadata, ``config``, ``checkpoint``, ``state_final``, and ``attrs``).

    CONTRACT:
    - Every call returns an independent deep copy via ``_clone_suews_output``
      so mutating the returned output, its DataFrames, ``attrs``,
      ``metadata``, or ``config`` cannot poison the session cache.
    """
    cache: dict = {}

    def _run(start_date=None, end_date=None):
        key = (_normalise_period_key(start_date), _normalise_period_key(end_date))
        if key not in cache:
            _record_reference_run(
                "sample_config_run_cached",
                f"start={start_date!r},end={end_date!r}",
            )
            simulation = supy.SUEWSSimulation.from_sample_data()
            cache[key] = simulation.run(start_date=start_date, end_date=end_date)
        return _clone_suews_output(cache[key])

    return _run


_STREAM_CHUNK_DEFAULT_GROUPS = ("SUEWS", "DailyState", "RSL", "debug")
_STREAM_CHUNK_CLOCK_FOLLOW = ("local_standard_time", "follow")


def _with_replicated_sites(config, list_gridiv):
    """Return a deep copy of *config* with one site per *list_gridiv* entry.

    Each copy is 10 degrees further north than the one before, so the grids'
    outputs differ and a block of rows carrying the wrong grid's label would
    not match.
    """
    config = config.model_copy(deep=True)
    site0 = config.sites[0]
    list_site = []
    for pos, gridiv in enumerate(list_gridiv):
        site = site0.model_copy(deep=True)
        site.gridiv = gridiv
        site.name = f"grid{gridiv}"
        site.properties.lat.value += 10.0 * pos
        list_site.append(site)
    config.sites = list_site
    return config


def _with_site_clock(config, timezone, startdls, enddls):
    """Return a deep copy of *config* with site time zone and DLS window."""
    config = config.model_copy(deep=True)
    for site in config.sites:
        site.properties.timezone.value = timezone
        site.properties.anthropogenic_emissions.startdls.value = startdls
        site.properties.anthropogenic_emissions.enddls.value = enddls
    return config


def _year_crossing_forcing(sim, end=None, start=None):
    """Relabel the January short sample forcing to run across 1 January.

    With *start* or *end*, the forcing starts or stops at that row.
    """
    forcing = sim.forcing.df.copy()
    forcing.index -= pd.Timedelta(days=16)
    forcing.index.freq = forcing.index[1] - forcing.index[0]
    if start is not None:
        forcing = forcing.loc[pd.Timestamp(start) :]
    if end is not None:
        forcing = forcing.loc[: pd.Timestamp(end)]
    for col, values in (
        ("iy", forcing.index.year),
        ("id", forcing.index.day_of_year),
        ("it", forcing.index.hour),
        ("imin", forcing.index.minute),
    ):
        if col in forcing.columns:
            forcing[col] = values
    return forcing


def build_year_crossing_sim(
    short_sample_sim,
    path_out,
    freq_s,
    stream_chunk,
    list_gridiv=None,
    groups=_STREAM_CHUNK_DEFAULT_GROUPS,
    clock=_STREAM_CHUNK_CLOCK_FOLLOW,
    end=None,
    start=None,
    site_clock=None,
    fmt="txt",
):
    """Build a one-month year-crossing simulation saving *groups* to *path_out*.

    *site_clock* is ``(timezone, startdls, enddls)`` for every site; *fmt* is
    the output format.
    """
    from supy.data_model.output import PROVISIONAL_OUTPUT_GROUPS

    list_groups = list(groups)
    sim = short_sample_sim()
    forcing = _year_crossing_forcing(sim, end=end, start=start)
    config = sim.config
    if list_gridiv is not None:
        config = _with_replicated_sites(config, list_gridiv)
    if site_clock is not None:
        config = _with_site_clock(config, *site_clock)
    output = {
        "format": fmt,
        "freq": freq_s,
        "dir": str(path_out),
        "groups": list_groups,
        "include_provisional": bool(set(list_groups) & PROVISIONAL_OUTPUT_GROUPS),
        "timestamp_reference": clock[1],
    }
    if stream_chunk is not None:
        output["stream_chunk"] = stream_chunk
    update = {
        "model": {
            "control": {
                "start_time": str(forcing.index[0]),
                "end_time": str(forcing.index[-1]),
                "forcing": {"timestamp_reference": clock[0]},
                "output": output,
            }
        }
    }
    if config is not sim.config:
        # Changed sites travel in the same update, which replaces the sites
        # list wholesale, so the state frame is built from the configuration
        # once rather than once per change.
        update["sites"] = [
            site.model_dump(exclude_unset=True, round_trip=True, mode="json")
            for site in config.sites
        ]
    sim.update_config(update, auto_load_forcing=False)
    sim.update_forcing(forcing)
    return sim


@pytest.fixture(scope="session")
def stream_chunk_reference_dir(short_sample_sim, tmp_path_factory):
    """Session-scoped factory of year-crossing reference output folders.

    Computes each distinct settings tuple once per pytest session (once per
    worker under xdist):

    - ``stream_chunk=None`` (default): runs ``sim.run()`` followed by
      ``sim.save()`` (the unstreamed reference folder).
    - ``stream_chunk=<int>``: runs ``sim.run()`` with ``output.stream_chunk``
      set (the uninterrupted streamed reference folder).

    READ-ONLY by default: consumers must not write into or delete files from
    the returned directory. Pass ``copy_dir=True`` when a test needs a private
    writable copy of the reference folder.
    """
    dict_dir: dict = {}

    def _ref_dir(
        freq_s,
        list_gridiv=None,
        groups=_STREAM_CHUNK_DEFAULT_GROUPS,
        clock=_STREAM_CHUNK_CLOCK_FOLLOW,
        end=None,
        start=None,
        site_clock=None,
        fmt="txt",
        *,
        stream_chunk=None,
        copy_dir: bool = False,
    ) -> Path:
        key = (
            freq_s,
            tuple(list_gridiv or ()),
            tuple(groups),
            clock,
            end,
            start,
            site_clock,
            fmt,
            stream_chunk,
        )
        if key not in dict_dir:
            _record_reference_run("stream_chunk_reference_dir", repr(key))
            prefix = "whole" if stream_chunk is None else "streamed"
            path = tmp_path_factory.mktemp(prefix)
            sim = build_year_crossing_sim(
                short_sample_sim,
                path,
                freq_s,
                stream_chunk,
                list_gridiv=list_gridiv,
                groups=groups,
                clock=clock,
                end=end,
                start=start,
                site_clock=site_clock,
                fmt=fmt,
            )
            sim.run()
            if stream_chunk is None:
                sim.save()
            dict_dir[key] = path
        path_cached = dict_dir[key]
        if copy_dir:
            path_copy = tmp_path_factory.mktemp("ref_copy")
            shutil.copytree(path_cached, path_copy, dirs_exist_ok=True)
            return path_copy
        return path_cached

    return _ref_dir


@pytest.fixture(scope="session")
def stream_chunk_reference_output(short_sample_sim, tmp_path_factory):
    """Session-scoped factory of unstreamed year-crossing ``SUEWSOutput``.

    Computes each distinct settings tuple once per pytest session and hands
    back an independent deep copy via ``_clone_suews_output`` on every call so
    callers cannot poison the cache.
    """
    dict_output: dict = {}

    def _ref_output(
        freq_s,
        list_gridiv=None,
        groups=_STREAM_CHUNK_DEFAULT_GROUPS,
        clock=_STREAM_CHUNK_CLOCK_FOLLOW,
        end=None,
        start=None,
        site_clock=None,
        fmt="txt",
    ):
        key = (
            freq_s,
            tuple(list_gridiv or ()),
            tuple(groups),
            clock,
            end,
            start,
            site_clock,
            fmt,
        )
        if key not in dict_output:
            _record_reference_run("stream_chunk_reference_output", repr(key))
            path = tmp_path_factory.mktemp("whole_output")
            sim = build_year_crossing_sim(
                short_sample_sim,
                path,
                freq_s,
                None,
                list_gridiv=list_gridiv,
                groups=groups,
                clock=clock,
                end=end,
                start=start,
                site_clock=site_clock,
                fmt=fmt,
            )
            dict_output[key] = sim.run()
        return _clone_suews_output(dict_output[key])

    return _ref_output
