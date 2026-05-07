"""``diagnose_run`` MCP tool — Phase-1 health checks on a finished run."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def diagnose_run(
    run_dir: str,
    project_root: Optional[str] = None,
) -> dict[str, Any]:
    """Diagnose a finished SUEWS run directory.

    Runs ``suews diagnose <run_dir> --format json`` which checks for
    provenance, expected output files, NaN proportions in QH/QE/QN, and
    energy-balance closure.

    Parameters
    ----------
    run_dir
        Path to the SUEWS run directory.
    project_root
        Override the per-session project root for this call.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(run_dir)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews diagnose {run_dir}")

    try:
        return run_suews_cli("diagnose", [str(path)], project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews diagnose {path}")
