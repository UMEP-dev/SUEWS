"""Startup hook for inherited source-lane guards in child Python subprocesses.

Added to ``PYTHONPATH`` by ``scripts/suews/source_lane_bootstrap.py`` only when
``SUEWS_SOURCE_LANE_ONLY=1`` is active. When an ordinary child interpreter
(``python -c ...``, ``python -m ...``, etc.) starts under source-lane mode,
Python's ``site`` module imports this file before user code runs, activating
the compiled-extension blocker, authoritative source-checkout finder, and
``_version_scm`` fallback.

Because Python's ``site.execsitecustomize()`` swallows plain ``Exception``
subclasses and continues running the child payload, any activation failure here
prints a diagnostic to ``stderr`` and terminates startup with ``SystemExit(1)``.

If another ``sitecustomize.py`` also exists further along ``sys.path``, this
hook chains to it so downstream customisations are not shadowed.
"""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import sys
from types import ModuleType

_THIS_FILE = Path(__file__).resolve()
_THIS_DIR = _THIS_FILE.parent
_BOOTSTRAP_PATH = (_THIS_DIR.parent / "source_lane_bootstrap.py").resolve()
_BOOTSTRAP_MOD_NAME = "_suews_source_lane_bootstrap"
_FALSY_VALUES: frozenset[str] = frozenset({"", "0", "false", "no", "off"})


def _load_bootstrap_module() -> ModuleType | None:
    """Load or reuse ``scripts/suews/source_lane_bootstrap.py``."""
    if not _BOOTSTRAP_PATH.is_file():
        raw = os.environ.get("SUEWS_SOURCE_LANE_ONLY", "").strip().lower()
        if raw not in _FALSY_VALUES:
            raise RuntimeError(
                f"Missing source-lane bootstrap module: {_BOOTSTRAP_PATH}"
            )
        return None

    existing = sys.modules.get(_BOOTSTRAP_MOD_NAME)
    existing_file = (
        getattr(existing, "__file__", None) if existing is not None else None
    )
    if existing_file:
        try:
            if Path(existing_file).resolve() == _BOOTSTRAP_PATH:
                return existing
        except OSError:
            pass

    spec = importlib.util.spec_from_file_location(_BOOTSTRAP_MOD_NAME, _BOOTSTRAP_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(
            f"Unable to load source-lane bootstrap module from {_BOOTSTRAP_PATH}"
        )
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _activate_source_lane() -> None:
    """Activate source-lane guards, aborting startup via ``SystemExit`` on failure."""
    try:
        module = _load_bootstrap_module()
        if module is None:
            return
        raw_root = os.environ.get(module.SOURCE_LANE_ROOT_ENV_VAR, "").strip()
        repo_root = Path(raw_root) if raw_root else _THIS_DIR.parents[2]
        module.activate_if_enabled(repo_root)
    except Exception as exc:
        sys.stderr.write(
            f"[source-lane] Fatal startup guard failure ({type(exc).__name__}): {exc}\n"
        )
        sys.stderr.flush()
        raise SystemExit(1) from exc


def _chain_other_sitecustomize() -> None:
    """Execute the next ``sitecustomize.py`` on ``sys.path``, if one exists."""
    for entry in list(sys.path):
        if not entry:
            continue
        try:
            candidate = (Path(entry) / "sitecustomize.py").resolve()
        except OSError:
            continue
        if candidate == _THIS_FILE or not candidate.is_file():
            continue
        spec = importlib.util.spec_from_file_location(
            "_suews_chained_sitecustomize", candidate
        )
        if spec is None or spec.loader is None:
            continue
        chained = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(chained)
        break


_activate_source_lane()
_chain_other_sitecustomize()
