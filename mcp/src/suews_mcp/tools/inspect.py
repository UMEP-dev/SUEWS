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
) -> dict[str, Any]:
    """**Use this after `init_case` or after Write, before going back to
    Write** — reports the active fields, surface fractions, and
    physics methods so you do not have to re-derive them from
    `query_knowledge`. Cheaper than `query_knowledge` for "what field
    do I edit"; pair with `validate_config` after each edit cycle
    (gh#1407).

    Parameters
    ----------
    config_path
        Path to the YAML config.
    project_root
        Override the per-session project root for this call.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(config_path)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews inspect {config_path}")

    try:
        return run_suews_cli("inspect", [str(path)], project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews inspect {path}")
