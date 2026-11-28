#!/usr/bin/env python3
"""Bootstrap and compiled-extension guards for wheel-free source test lanes.

Activated by ``test/conftest.py`` when ``SUEWS_SOURCE_LANE_ONLY=1`` is set.
Enables running the ``tooling`` and ``unit`` pytest axes directly from a clean
source checkout (without building or installing a compiled ``supy`` wheel),
while actively blocking accidental imports of compiled Rust/Fortran extensions,
resolving ``supy`` and ``suews_mcp`` authoritatively from the requested
checkout (superseding installed wheels and Meson editable loaders), propagating
startup guards to ordinary child Python subprocesses, and refusing to execute
``native`` or ``physics`` tests in source lanes.

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
SOURCE_LANE_ROOT_ENV_VAR = "SUEWS_SOURCE_LANE_ROOT"
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

_SITE_BOOTSTRAP_DIR = Path(__file__).resolve().parent / "source_lane_site"

_STATE: dict[str, Any] = {
    "repo_root": None,
    "disabled_finders": [],
}


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


class SourceCheckoutFinder(importlib.abc.MetaPathFinder):
    """Resolve ``supy`` and ``suews_mcp`` authoritatively from one source checkout.

    Installed right after :class:`CompiledExtensionBlocker` (ahead of Meson
    editable finders and ``PathFinder``) so source-lane imports always bind to
    the requested checkout and never trigger editable rebuilds or load an
    installed wheel from another path.
    """

    def __init__(self, repo_root: Path) -> None:
        """Record the checkout's ``src/`` and ``mcp/src/`` roots."""
        self.repo_root = repo_root.resolve()
        self._package_roots: dict[str, Path] = {}
        src_dir = self.repo_root / "src"
        mcp_src_dir = self.repo_root / "mcp" / "src"
        if (src_dir / "supy").is_dir():
            self._package_roots["supy"] = src_dir
        if (mcp_src_dir / "suews_mcp").is_dir():
            self._package_roots["suews_mcp"] = mcp_src_dir
        self._version_fallback = VersionScmFallbackFinder()

    def find_spec(
        self,
        fullname: str,
        path: Sequence[str] | None,
        target: Any = None,
    ) -> importlib.machinery.ModuleSpec | None:
        """Resolve ``supy`` and ``suews_mcp`` modules from ``self.repo_root``."""
        top = fullname.split(".", 1)[0]
        pkg_root = self._package_roots.get(top)
        if pkg_root is None:
            return None

        parts = fullname.split(".")
        if len(parts) == 1:
            search_paths = [str(pkg_root)]
        else:
            parent_dir = pkg_root.joinpath(*parts[:-1])
            search_paths = [str(parent_dir)] if parent_dir.is_dir() else []

        spec = (
            importlib.machinery.PathFinder.find_spec(fullname, search_paths, target)
            if search_paths
            else None
        )
        if spec is not None:
            return spec
        if fullname in _FALLBACK_VERSION_MODULES:
            return importlib.machinery.ModuleSpec(fullname, self._version_fallback)
        del path
        return None


def _is_editable_loader_finder(finder: Any) -> bool:
    """Return True when ``finder`` is a Meson/editable meta-path finder for ``supy``."""
    if isinstance(
        finder,
        (CompiledExtensionBlocker, SourceCheckoutFinder, VersionScmFallbackFinder),
    ):
        return False
    cls = finder if isinstance(finder, type) else type(finder)
    mod_name = getattr(cls, "__module__", "") or ""
    cls_name = getattr(cls, "__name__", "") or ""
    if "editable_loader" in mod_name and ("supy" in mod_name or "suews" in mod_name):
        return True
    if "mesonpy" in mod_name.lower() or "editablefinder" in cls_name.lower():
        project = str(
            getattr(finder, "_project", "")
            or getattr(finder, "_name", "")
            or getattr(finder, "name", "")
            or mod_name
        ).lower()
        if "supy" in project or "suews" in project:
            return True
    return False


def _owned_package_roots(repo_root: Path) -> dict[str, Path]:
    """Return ``{pkg_name: pkg_dir}`` for packages present under ``repo_root``."""
    resolved_root = repo_root.resolve()
    roots: dict[str, Path] = {}
    supy_dir = resolved_root / "src" / "supy"
    mcp_dir = resolved_root / "mcp" / "src" / "suews_mcp"
    if supy_dir.is_dir():
        roots["supy"] = supy_dir
    if mcp_dir.is_dir():
        roots["suews_mcp"] = mcp_dir
    return roots


def _module_is_within(module: Any, expected_dir: Path) -> bool:
    """Return True if ``module`` originates under ``expected_dir`` or is synthetic."""
    spec = getattr(module, "__spec__", None)
    if spec is not None and isinstance(spec.loader, VersionScmFallbackFinder):
        return True

    raw_file = getattr(module, "__file__", None)
    raw_path = getattr(module, "__path__", None)
    if raw_file:
        candidates = [raw_file]
    elif raw_path is not None:
        candidates = list(raw_path)
        if not candidates:
            return False
    else:
        return True

    try:
        return all(Path(p).resolve().is_relative_to(expected_dir) for p in candidates)
    except OSError:
        return False


def _assert_no_conflicting_loaded_packages(repo_root: Path | None) -> None:
    """Reject already-cached ``supy``/``suews_mcp`` modules from outside ``repo_root``."""
    if repo_root is None:
        return
    expected_roots = _owned_package_roots(repo_root)
    if not expected_roots:
        return

    conflicts: list[str] = []
    for name, module in list(sys.modules.items()):
        if module is None:
            continue
        top = name.split(".", 1)[0]
        expected_dir = expected_roots.get(top)
        if expected_dir is None:
            continue
        if not _module_is_within(module, expected_dir):
            origin = getattr(module, "__file__", None) or getattr(
                module, "__path__", "<unknown>"
            )
            conflicts.append(f"{name} ({origin})")
    if conflicts:
        raise SourceLaneViolationError(
            f"Conflicting cached package module(s) loaded outside requested "
            f"source checkout {repo_root.resolve()} under "
            f"{SOURCE_LANE_ENV_VAR}=1: {', '.join(sorted(conflicts))}"
        )


def configure_source_paths(repo_root: Path) -> tuple[Path, Path]:
    """Ensure ``src``, ``mcp/src``, and startup hook are absolute on ``sys.path``/``PYTHONPATH``.

    Normalising relative ``PYTHONPATH`` entries (such as ``PYTHONPATH=src:mcp/src``)
    to absolute paths and exporting ``source_lane_site`` ensures child
    subprocesses launched with ``cwd=tmp_path`` resolve the source checkout
    cleanly and inherit the compiled-extension guards at interpreter startup.

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
        if path_str in sys.path:
            sys.path.remove(path_str)
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
    if _SITE_BOOTSTRAP_DIR.is_dir():
        site_dir_str = str(_SITE_BOOTSTRAP_DIR.resolve())
        if site_dir_str not in combined:
            combined.append(site_dir_str)
    for entry in existing_entries:
        if entry not in combined:
            combined.append(entry)

    if combined:
        os.environ["PYTHONPATH"] = os.pathsep.join(combined)
    os.environ[SOURCE_LANE_ROOT_ENV_VAR] = str(resolved_root)

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


def install_guards(repo_root: Path | None = None) -> None:
    """Install the compiled-extension blocker, checkout finder, and version fallback."""
    _assert_no_loaded_compiled_modules()
    _assert_no_conflicting_loaded_packages(repo_root)

    # Temporarily disable any installed Meson editable meta-path finder for supy
    # so it cannot trigger compilation or resolve another checkout in source mode.
    kept_finders: list[Any] = []
    for finder in sys.meta_path:
        if _is_editable_loader_finder(finder):
            if finder not in _STATE["disabled_finders"]:
                _STATE["disabled_finders"].append(finder)
            continue
        if isinstance(finder, SourceCheckoutFinder):
            continue
        kept_finders.append(finder)
    sys.meta_path[:] = kept_finders

    if not any(
        isinstance(finder, CompiledExtensionBlocker) for finder in sys.meta_path
    ):
        sys.meta_path.insert(0, CompiledExtensionBlocker())

    if repo_root is not None:
        blocker_idx = next(
            idx
            for idx, finder in enumerate(sys.meta_path)
            if isinstance(finder, CompiledExtensionBlocker)
        )
        sys.meta_path.insert(blocker_idx + 1, SourceCheckoutFinder(repo_root))

    if not any(
        isinstance(finder, VersionScmFallbackFinder) for finder in sys.meta_path
    ):
        sys.meta_path.append(VersionScmFallbackFinder())


def uninstall_guards() -> None:
    """Remove source-lane meta-path finders and restore any disabled editable finders."""
    sys.meta_path[:] = [
        finder
        for finder in sys.meta_path
        if not isinstance(
            finder,
            (CompiledExtensionBlocker, SourceCheckoutFinder, VersionScmFallbackFinder),
        )
    ]
    for finder in reversed(_STATE["disabled_finders"]):
        if finder not in sys.meta_path:
            sys.meta_path.insert(0, finder)
    _STATE["disabled_finders"].clear()


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
    install_guards(resolved_root)
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
    ``sys.executable -P -c ...`` command (omitting the current working directory
    from ``sys.path`` to prevent CWD shadowing while retaining ``PYTHONPATH``)
    that activates source-lane guards deterministically and dispatches directly
    to the matching pure-Python entry point in ``supy.cmd.suews_cli``.

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
        "import importlib, os, pathlib, sys; "
        f"os.environ.setdefault({SOURCE_LANE_ENV_VAR!r}, '1'); "
        f"sys.path.insert(0, {scripts_dir!r}); "
        "import source_lane_bootstrap as _slb; "
        f"_slb.activate(pathlib.Path({str(resolved_root)!r})); "
        f"sys.argv[0] = {route!r}; "
        f"getattr(importlib.import_module({module_name!r}), {func_name!r})()"
    )
    return [sys.executable, "-P", "-c", snippet]
