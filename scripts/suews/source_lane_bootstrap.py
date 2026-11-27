#!/usr/bin/env python3
"""Bootstrap and compiled-extension guards for wheel-free source test lanes.

Activated by ``test/conftest.py`` when ``SUEWS_SOURCE_LANE_ONLY=1`` is set.
Enables running the ``tooling`` and ``unit`` pytest axes directly from a clean
source checkout (without building or installing a compiled ``supy`` wheel),
while actively blocking accidental imports of compiled Rust/Fortran extensions
and refusing to execute ``native`` or ``physics`` tests in source lanes.

When ``SUEWS_SOURCE_LANE_ONLY`` is unset or falsy, this module performs no
process mutations so normal wheel and editable test runs stay unchanged.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
import importlib.abc
import importlib.machinery
import importlib.util
import os
from pathlib import Path
import sys
import sysconfig
from typing import Any

SOURCE_LANE_ENV_VAR = "SUEWS_SOURCE_LANE_ONLY"
_TRUTHY_VALUES: frozenset[str] = frozenset({"1", "true", "yes", "on"})
_FALSY_VALUES: frozenset[str] = frozenset({"0", "false", "no", "off"})

FORBIDDEN_SOURCE_LANE_AXES: frozenset[str] = frozenset({"native", "physics"})
ALLOWED_SOURCE_LANE_AXES: frozenset[str] = frozenset({"tooling", "unit"})

# Compiled Rust/Fortran extension module names (current and historical) that
# must never load in a wheel-free source lane.
BLOCKED_COMPILED_MODULES: frozenset[str] = frozenset({
    "suews_bridge",
    "supy.suews_bridge",
    "_suews_bridge",
    "supy._suews_bridge",
    "_supy_driver",
    "supy._supy_driver",
    "_suews_driver",
    "supy._suews_driver",
    "supy_driver",
    "supy.supy_driver",
})

# Generated build-time version modules that are gitignored in a clean source
# checkout and only materialised by ``get_ver_git.py`` during builds.
_FALLBACK_VERSION_MODULES: frozenset[str] = frozenset({
    "supy._version_scm",
    "suews_mcp._version_scm",
})

# Pure-Python CLI routes declared in ``pyproject.toml`` ``[project.scripts]``.
_CLI_ENTRY_POINTS: dict[str, tuple[str, str]] = {
    "suews": ("supy.cmd.suews_cli", "main"),
    "suews-run": ("supy.cmd.suews_cli", "run_alias"),
    "suews-convert": ("supy.cmd.suews_cli", "convert_alias"),
    "suews-validate": ("supy.cmd.suews_cli", "validate_alias"),
    "suews-schema": ("supy.cmd.suews_cli", "schema_alias"),
    "suews-inspect": ("supy.cmd.suews_cli", "inspect_alias"),
}

_STATE: dict[str, Path | None] = {"repo_root": None}


class SourceLaneViolationError(RuntimeError):
    """Raised when a source-only lane attempts to load or run compiled code.

    Deliberately subclasses ``RuntimeError`` rather than ``ImportError`` so
    ``pytest.importorskip`` or ``except ImportError`` handlers cannot silently
    turn an accidental compiled dependency in a ``tooling`` or ``unit`` test
    into a skip.
    """


def is_source_lane_only(env: Mapping[str, str] | None = None) -> bool:
    """Return True when ``SUEWS_SOURCE_LANE_ONLY`` enables source-only mode.

    Parameters
    ----------
    env : Mapping[str, str] or None, optional
        Environment mapping to inspect; defaults to ``os.environ``.

    Returns
    -------
    bool
        ``True`` when enabled, ``False`` when unset, empty, or falsy.

    Raises
    ------
    ValueError
        If ``SUEWS_SOURCE_LANE_ONLY`` is set to an unrecognised value.
    """
    source = os.environ if env is None else env
    raw = source.get(SOURCE_LANE_ENV_VAR)
    if raw is None:
        return False
    normalised = raw.strip().lower()
    if not normalised or normalised in _FALSY_VALUES:
        return False
    if normalised in _TRUTHY_VALUES:
        return True
    raise ValueError(
        f"Invalid {SOURCE_LANE_ENV_VAR}={raw!r}; expected one of "
        f"{sorted(_TRUTHY_VALUES)} or {sorted(_FALSY_VALUES)}."
    )


def is_blocked_compiled_module(fullname: str) -> bool:
    """Return True when ``fullname`` names a forbidden compiled extension."""
    if fullname in BLOCKED_COMPILED_MODULES:
        return True
    return any(fullname.startswith(f"{name}.") for name in BLOCKED_COMPILED_MODULES)


class CompiledExtensionBlocker(importlib.abc.MetaPathFinder):
    """Meta-path finder that blocks loading compiled SUEWS extensions."""

    def find_spec(
        self,
        fullname: str,
        path: Sequence[str] | None,
        target: Any = None,
    ) -> None:
        """Raise ``SourceLaneViolationError`` for any blocked compiled module."""
        del path, target
        if is_blocked_compiled_module(fullname):
            raise SourceLaneViolationError(
                f"Compiled extension {fullname!r} cannot be imported when "
                f"{SOURCE_LANE_ENV_VAR}=1 (wheel-free source lane). "
                "Reclassify tests that require compiled Rust/Fortran code to "
                "the 'native' or 'physics' dependency axis."
            )


class VersionScmFallbackFinder(importlib.abc.MetaPathFinder, importlib.abc.Loader):
    """Fallback finder for gitignored ``_version_scm`` modules in clean checkouts.

    Appended at the end of ``sys.meta_path`` so a real generated
    ``_version_scm.py`` file on disk always takes precedence when present.
    """

    def find_spec(
        self,
        fullname: str,
        path: Sequence[str] | None,
        target: Any = None,
    ) -> importlib.machinery.ModuleSpec | None:
        """Return a synthetic module spec when ``_version_scm`` is absent on disk."""
        del path, target
        if fullname in _FALLBACK_VERSION_MODULES:
            return importlib.machinery.ModuleSpec(fullname, self)
        return None

    def create_module(self, spec: importlib.machinery.ModuleSpec) -> None:
        """Use Python's default module creation semantics."""
        del spec

    def exec_module(self, module: Any) -> None:
        """Populate fallback version and commit attributes matching build output."""
        module.__version__ = "0+unknown"
        module.version = "0+unknown"
        module.__version_tuple__ = ()
        module.version_tuple = ()
        module.__commit_hash__ = "unknown"
        module.commit_hash = "unknown"


def configure_source_paths(repo_root: Path) -> tuple[Path, Path]:
    """Ensure ``src`` and ``mcp/src`` are absolute entries on ``sys.path`` and ``PYTHONPATH``.

    Normalising relative ``PYTHONPATH`` entries (such as ``PYTHONPATH=src:mcp/src``)
    to absolute paths ensures child subprocesses launched with ``cwd=tmp_path``
    continue resolving the source checkout cleanly.

    Parameters
    ----------
    repo_root : Path
        Repository root directory containing ``src/`` and ``mcp/src/``.

    Returns
    -------
    tuple[Path, Path]
        Resolved ``(src_dir, mcp_src_dir)`` paths.
    """
    resolved_root = repo_root.resolve()
    src_dir = resolved_root / "src"
    mcp_src_dir = resolved_root / "mcp" / "src"
    source_dirs = [path for path in (src_dir, mcp_src_dir) if path.is_dir()]

    for path in reversed(source_dirs):
        path_str = str(path)
        if path_str not in sys.path:
            sys.path.insert(0, path_str)

    cwd = Path.cwd()
    raw_pythonpath = os.environ.get("PYTHONPATH", "")
    existing_entries: list[str] = []
    for entry in raw_pythonpath.split(os.pathsep):
        if not entry:
            continue
        candidate = Path(entry)
        resolved_entry = str(
            candidate.resolve()
            if candidate.is_absolute()
            else (cwd / candidate).resolve()
        )
        if resolved_entry not in existing_entries:
            existing_entries.append(resolved_entry)

    combined: list[str] = []
    for path in source_dirs:
        path_str = str(path)
        if path_str not in combined:
            combined.append(path_str)
    for entry in existing_entries:
        if entry not in combined:
            combined.append(entry)

    if combined:
        os.environ["PYTHONPATH"] = os.pathsep.join(combined)

    return src_dir, mcp_src_dir


def _assert_no_loaded_compiled_modules() -> None:
    """Fail loudly if a blocked compiled module is already loaded in ``sys.modules``."""
    loaded = sorted(
        name
        for name, module in sys.modules.items()
        if module is not None and is_blocked_compiled_module(name)
    )
    if loaded:
        raise SourceLaneViolationError(
            f"Compiled extension module(s) already loaded in sys.modules under "
            f"{SOURCE_LANE_ENV_VAR}=1: {', '.join(loaded)}"
        )


def install_guards() -> None:
    """Install the compiled-extension blocker and ``_version_scm`` fallback finder."""
    _assert_no_loaded_compiled_modules()
    if not any(
        isinstance(finder, CompiledExtensionBlocker) for finder in sys.meta_path
    ):
        sys.meta_path.insert(0, CompiledExtensionBlocker())
    if not any(
        isinstance(finder, VersionScmFallbackFinder) for finder in sys.meta_path
    ):
        sys.meta_path.append(VersionScmFallbackFinder())


def uninstall_guards() -> None:
    """Remove source-lane meta-path finders installed by :func:`install_guards`."""
    sys.meta_path[:] = [
        finder
        for finder in sys.meta_path
        if not isinstance(finder, (CompiledExtensionBlocker, VersionScmFallbackFinder))
    ]


def activate(repo_root: Path | None = None) -> Path:
    """Activate source-lane path bootstrap and compiled-extension guards.

    Parameters
    ----------
    repo_root : Path or None, optional
        Repository root directory; defaults to the repository enclosing this script.

    Returns
    -------
    Path
        Resolved repository root directory.
    """
    resolved_root = (
        repo_root.resolve()
        if repo_root is not None
        else Path(__file__).resolve().parents[2]
    )
    configure_source_paths(resolved_root)
    install_guards()
    _STATE["repo_root"] = resolved_root
    return resolved_root


def activate_if_enabled(
    repo_root: Path | None = None,
    env: Mapping[str, str] | None = None,
) -> bool:
    """Activate source-lane bootstrap only when ``SUEWS_SOURCE_LANE_ONLY`` is truthy.

    Parameters
    ----------
    repo_root : Path or None, optional
        Repository root directory.
    env : Mapping[str, str] or None, optional
        Environment mapping to inspect; defaults to ``os.environ``.

    Returns
    -------
    bool
        ``True`` if source-lane mode was activated, ``False`` otherwise.
    """
    if not is_source_lane_only(env):
        return False
    activate(repo_root)
    return True


def check_selected_items_for_source_lane(items: Sequence[Any]) -> list[str]:
    """Return descriptions of any selected pytest items carrying forbidden axes.

    Parameters
    ----------
    items : Sequence[Any]
        Pytest test items selected for execution.

    Returns
    -------
    list[str]
        Sorted list of ``"<nodeid> (<axes>)"`` entries for items carrying
        ``native`` or ``physics``.
    """
    offending: list[str] = []
    for item in items:
        if not hasattr(item, "iter_markers"):
            continue
        forbidden = sorted(
            {marker.name for marker in item.iter_markers()} & FORBIDDEN_SOURCE_LANE_AXES
        )
        if forbidden:
            offending.append(f"{item.nodeid} ({', '.join(forbidden)})")
    return offending


def require_compiled_runtime(
    feature: str,
    env: Mapping[str, str] | None = None,
) -> None:
    """Raise ``SourceLaneViolationError`` if ``feature`` is invoked in source-only mode."""
    if is_source_lane_only(env):
        raise SourceLaneViolationError(
            f"{feature} requires the compiled SUEWS extension or binary and is "
            f"forbidden when {SOURCE_LANE_ENV_VAR}=1. Reclassify the calling "
            "test to the 'native' or 'physics' dependency axis."
        )


def resolve_suews_cli_command(
    route: str = "suews",
    *,
    repo_root: Path | None = None,
    env: Mapping[str, str] | None = None,
) -> list[str]:
    """Return the subprocess argv prefix for a SUEWS CLI console route.

    In normal wheel or editable installs (``SUEWS_SOURCE_LANE_ONLY`` unset),
    returns the installed console script under ``sysconfig.get_path("scripts")``
    and raises ``AssertionError`` if absent (preserving existing test contracts).

    In wheel-free source lanes (``SUEWS_SOURCE_LANE_ONLY=1``), returns a
    ``sys.executable -c ...`` command that activates source-lane guards and
    dispatches directly to the matching pure-Python entry point in
    ``supy.cmd.suews_cli``.

    Parameters
    ----------
    route : str, optional
        Console-script name (``"suews"``, ``"suews-validate"``, etc.).
    repo_root : Path or None, optional
        Repository root directory for source-lane bootstrap; defaults to the
        active or enclosing repository root.
    env : Mapping[str, str] or None, optional
        Environment mapping to inspect; defaults to ``os.environ``.

    Returns
    -------
    list[str]
        Command prefix suitable for ``subprocess.run`` / ``subprocess.Popen``.
    """
    if route not in _CLI_ENTRY_POINTS:
        raise ValueError(
            f"Unknown SUEWS CLI route {route!r}; expected one of "
            f"{sorted(_CLI_ENTRY_POINTS)}."
        )

    if not is_source_lane_only(env):
        executable = Path(sysconfig.get_path("scripts")) / (
            route + (".exe" if os.name == "nt" else "")
        )
        assert executable.is_file(), f"Missing installed command: {executable}"
        return [str(executable)]

    resolved_root = (
        repo_root.resolve()
        if repo_root is not None
        else (_STATE["repo_root"] or Path(__file__).resolve().parents[2])
    )
    module_name, func_name = _CLI_ENTRY_POINTS[route]
    scripts_dir = str((resolved_root / "scripts" / "suews").resolve())
    snippet = (
        "import importlib, pathlib, sys; "
        f"sys.path.insert(0, {scripts_dir!r}); "
        "import source_lane_bootstrap as _slb; "
        f"_slb.activate_if_enabled(pathlib.Path({str(resolved_root)!r})); "
        f"sys.argv[0] = {route!r}; "
        f"getattr(importlib.import_module({module_name!r}), {func_name!r})()"
    )
    return [sys.executable, "-c", snippet]
