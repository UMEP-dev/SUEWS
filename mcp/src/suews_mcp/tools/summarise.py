"""``summarise_run`` MCP tool — per-variable statistics for a SUEWS run."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def summarise_run(
    run_dir: str,
    project_root: Optional[str] = None,
    variables: Optional[str] = None,
) -> dict[str, Any]:
    """Summarise a SUEWS run output and return the standard envelope.

    Parameters
    ----------
    run_dir
        Path to the SUEWS run directory (relative to the project root or
        absolute).
    project_root
        Override the per-session project root for this call. Defaults to
        the value resolved from ``SUEWS_MCP_PROJECT_ROOT``.
    variables
        Comma-separated variable names to restrict the summary. ``None`` or
        an empty string means every numeric column is summarised.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(run_dir)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews summarise {run_dir}")

    args = [str(path)]
    if variables:
        args += ["--variables", variables]

    try:
        return run_suews_cli("summarise", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews summarise {path}")
