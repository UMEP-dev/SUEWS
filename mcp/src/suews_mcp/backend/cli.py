"""Subprocess wrapper around the unified ``suews`` CLI.

Every MCP tool that needs to invoke a SUEWS subcommand goes through
``run_suews_cli`` here. The wrapper enforces three things:

1. The subcommand must be in ``ALLOWED_SUBCOMMANDS``. No arbitrary shell.
2. The subprocess is bounded by a timeout (env-configurable).
3. ``stdout`` is parsed as the standard SUEWS JSON envelope; both success
   and error envelopes are returned to the caller.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Any, Optional, Sequence

from ..constants import ALLOWED_SUBCOMMANDS, DEFAULT_TIMEOUT_SECONDS


class SUEWSMCPError(RuntimeError):
    """Raised when the CLI wrapper cannot return a valid envelope."""


def run_suews_cli(
    subcmd: str,
    args: Sequence[str],
    project_root: Optional[Path] = None,
    timeout: int = DEFAULT_TIMEOUT_SECONDS,
    suews_executable: str = "suews",
) -> dict[str, Any]:
    """Invoke ``suews <subcmd> <args>`` and return the parsed JSON envelope.

    Parameters
    ----------
    subcmd
        Subcommand name. Must be in :data:`ALLOWED_SUBCOMMANDS`.
    args
        Command-line arguments forwarded to the subprocess. ``--format json``
        is appended automatically when not already present.
    project_root
        Working directory for the subprocess. ``None`` keeps the inherited cwd.
    timeout
        Subprocess timeout in seconds.
    suews_executable
        Override the executable name (used for testing).

    Returns
    -------
    dict
        The parsed JSON envelope ``{status, data, errors, warnings, meta}``.
        Both success and error envelopes are returned without raising; only
        truly malformed output (non-JSON stdout, missing executable, timeout)
        raises :class:`SUEWSMCPError`.
    """
    if subcmd not in ALLOWED_SUBCOMMANDS:
        raise SUEWSMCPError(
            f"Subcommand {subcmd!r} is not in the MCP allow-list "
            f"({sorted(ALLOWED_SUBCOMMANDS)})."
        )

    args_list = list(args)
    if "--format" not in args_list:
        args_list = [*args_list, "--format", "json"]

    cmd = [suews_executable, subcmd, *args_list]

    try:
        completed = subprocess.run(
            cmd,
            cwd=str(project_root) if project_root else None,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except FileNotFoundError as exc:
        raise SUEWSMCPError(
            f"Executable {suews_executable!r} not found on PATH; "
            "is supy installed in the active environment?"
        ) from exc
    except subprocess.TimeoutExpired as exc:
        raise SUEWSMCPError(
            f"`{' '.join(cmd)}` exceeded {timeout}s timeout"
        ) from exc

    stdout = completed.stdout or ""
    if not stdout.strip():
        raise SUEWSMCPError(
            f"`{' '.join(cmd)}` produced no stdout (exit {completed.returncode}); "
            f"stderr: {completed.stderr.strip()[:500]!r}"
        )

    try:
        # The CLI may emit progress lines on stderr; we ignore those and
        # parse stdout as a single JSON envelope.
        envelope = json.loads(stdout)
    except json.JSONDecodeError as exc:
        raise SUEWSMCPError(
            f"`{' '.join(cmd)}` stdout is not valid JSON: "
            f"{stdout[:500]!r} (stderr: {completed.stderr.strip()[:200]!r})"
        ) from exc

    if not isinstance(envelope, dict) or "status" not in envelope:
        raise SUEWSMCPError(
            f"`{' '.join(cmd)}` stdout did not match the SUEWS envelope shape; "
            f"received: {envelope!r}"
        )

    return envelope
