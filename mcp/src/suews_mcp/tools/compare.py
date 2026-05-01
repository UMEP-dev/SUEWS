"""``compare_runs`` MCP tool — RMSE/bias/r between two runs or run-vs-obs."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def compare_runs(
    run_a: str,
    run_b_or_obs: str,
    project_root: Optional[str] = None,
    metrics: Optional[str] = None,
    variables: Optional[str] = None,
) -> dict[str, Any]:
    """Compare two SUEWS runs (or a run and an observations CSV).

    Parameters
    ----------
    run_a
        First SUEWS run directory (relative or absolute).
    run_b_or_obs
        Second SUEWS run directory, or an observations CSV file. The CLI
        treats the argument as a directory if it is one and as an
        observations file otherwise.
    project_root
        Override the per-session project root for this call.
    metrics
        Comma-separated subset of ``{rmse,bias,r}``. ``None`` keeps the CLI
        default ``rmse,bias,r``.
    variables
        Comma-separated variable names to compare. ``None`` keeps the CLI
        default ``QH,QE,QN``.
    """
    root = ProjectRoot(project_root)
    try:
        path_a = root.resolve(run_a)
        path_b = root.resolve(run_b_or_obs)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(
            str(exc), command=f"suews compare {run_a} {run_b_or_obs}"
        )

    args = [str(path_a), str(path_b)]
    if metrics:
        args += ["--metrics", metrics]
    if variables:
        args += ["--variables", variables]

    try:
        return run_suews_cli("compare", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews compare {path_a} {path_b}"
        )
