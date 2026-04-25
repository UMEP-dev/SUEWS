"""``validate_config`` MCP tool.

Thin wrapper around ``suews validate --format json``. Sandboxes the input
path against the per-session project root.
"""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)


def validate_config(
    config_path: str,
    project_root: Optional[str] = None,
    explain: bool = True,
) -> dict[str, Any]:
    """Validate a SUEWS YAML config and return the standard envelope.

    Parameters
    ----------
    config_path
        Path to the YAML config (relative to the project root or absolute).
    project_root
        Override the per-session project root for this call. Defaults to
        the value resolved from ``SUEWS_MCP_PROJECT_ROOT``.
    explain
        Reserved for future use. Currently the CLI always emits the full
        envelope; the flag is accepted so MCP clients can plan for the
        upcoming behaviour.

    Returns
    -------
    dict
        The CLI's JSON envelope verbatim, or an error envelope generated
        locally when sandboxing or process invocation fails.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(config_path)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews validate {config_path}")

    try:
        return run_suews_cli(
            "validate",
            [str(path)],
            project_root=root.root,
        )
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews validate {path}")


def _error_envelope(message: str, command: str) -> dict[str, Any]:
    """Build a tool-side error envelope when the subprocess never ran.

    We import :mod:`supy.cmd.json_envelope` lazily so this module remains
    importable even in a context where supy isn't installed (for unit
    testing the sandbox guard).
    """
    try:
        from supy.cmd.json_envelope import Envelope

        return Envelope.error(errors=[message], command=command).to_dict()
    except ImportError:
        return {
            "status": "error",
            "data": {},
            "errors": [{"message": message}],
            "warnings": [],
            "meta": {"command": command},
        }
