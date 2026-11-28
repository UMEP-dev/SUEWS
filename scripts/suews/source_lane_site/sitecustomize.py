"""Startup hook for inherited source-lane guards in child Python subprocesses.

Added to ``PYTHONPATH`` by ``scripts/suews/source_lane_bootstrap.py`` only when
``SUEWS_SOURCE_LANE_ONLY=1`` is active. When an ordinary child interpreter
(``python -c ...``, ``python -m ...``, etc.) starts under source-lane mode,
Python's ``site`` module imports this file before user code runs, activating
the compiled-extension blocker, authoritative source-checkout finder, and
``_version_scm`` fallback.

If another ``sitecustomize.py`` also exists further along ``sys.path``, this
hook chains to it so downstream customisations are not shadowed.
"""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import sys

_THIS_FILE = Path(__file__).resolve()
_THIS_DIR = _THIS_FILE.parent
_BOOTSTRAP_PATH = _THIS_DIR.parent / "source_lane_bootstrap.py"


def _activate_source_lane() -> None:
    """Activate source-lane guards if ``SUEWS_SOURCE_LANE_ONLY`` is enabled."""
    if not _BOOTSTRAP_PATH.is_file():
        return
    spec = importlib.util.spec_from_file_location(
        "_suews_source_lane_bootstrap", _BOOTSTRAP_PATH
    )
    if spec is None or spec.loader is None:
        return
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)

    raw_root = os.environ.get(module.SOURCE_LANE_ROOT_ENV_VAR, "").strip()
    repo_root = Path(raw_root) if raw_root else _THIS_DIR.parents[2]
    module.activate_if_enabled(repo_root)


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
