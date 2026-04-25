"""``inspect_config`` MCP tool — explain a config without running it."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def inspect_config(
    config_path: str,
    project_root: Optional[str] = None,
    view: str = "all",
) -> dict[str, Any]:
    """Inspect a YAML config and return the standard envelope.

    Parameters
    ----------
    config_path
        Path to the YAML config.
    project_root
        Override the per-session project root for this call.
    view
        One of ``all``, ``inputs``, ``surface-cover``, ``forcing``, ``derived``.
        Forwarded to ``suews inspect`` as ``--view`` when not ``all``.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(config_path)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews inspect {config_path}")

    args = [str(path)]
    if view and view != "all":
        args += ["--view", view]

    try:
        return run_suews_cli("inspect", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews inspect {path}")
