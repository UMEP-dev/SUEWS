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
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Optional, Sequence

from ..constants import ALLOWED_SUBCOMMANDS, DEFAULT_TIMEOUT_SECONDS


class SUEWSMCPError(RuntimeError):
    """Raised when the CLI wrapper cannot return a valid envelope."""


def _resolve_from_dir(name: str, directory: Path) -> str | None:
    """Resolve ``name`` in ``directory``.

    ``shutil.which`` handles PATHEXT on Windows for real console scripts such
    as ``suews.exe``. The direct file check keeps the resolver testable with
    extensionless fixture scripts on every platform.
    """
    direct = directory / name
    if direct.is_file() and os.access(direct, os.X_OK):
        return str(direct)
    return shutil.which(name, path=str(directory))


def _resolve_suews_executable(name: str = "suews") -> str:
    """Locate the ``suews`` console script for the current interpreter (gh#1400).

    MCP plugin hosts (Claude Code, Codex, Claude Desktop, Cursor) launch
    MCP servers as subprocesses **without** sourcing a venv. The host's
    PATH is the user's login PATH, which usually does not include the
    venv's ``bin/`` directory. The result: every CLI-backed MCP tool
    fails with "Executable 'suews' not found on PATH" even though
    ``suews`` is installed in the same venv as ``suews-mcp``.

    The MCP server's own Python interpreter (``sys.executable``) is the
    right anchor: ``pip install`` placed ``suews`` as a sibling console
    script in the same directory. We resolve in this order:

    1. ``shutil.which(name, path=Path(sys.executable).parent)`` — looks
       for a sibling console script. ``shutil.which`` honours
       ``PATHEXT`` on Windows so ``suews.exe`` is found correctly.
    2. ``shutil.which(name)`` — falls back to the user's PATH for
       system-wide installs (e.g. ``pipx``).
    3. Return ``name`` unchanged so ``subprocess.run`` still raises a
       useful ``FileNotFoundError`` when nothing is reachable.
    """
    sibling_dir = Path(sys.executable).parent
    sibling = _resolve_from_dir(name, sibling_dir)
    if sibling:
        return sibling

    for entry in os.environ.get("PATH", "").split(os.pathsep):
        if not entry:
            continue
        on_path = _resolve_from_dir(name, Path(entry))
        if on_path:
            return on_path

    on_path = shutil.which(name)
    if on_path:
        return on_path
    return name


def run_suews_cli(
    subcmd: str,
    args: Sequence[str],
    project_root: Optional[Path] = None,
    timeout: int = DEFAULT_TIMEOUT_SECONDS,
    suews_executable: Optional[str] = None,
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
        Override the executable path (used for testing). When ``None``
        (the default), :func:`_resolve_suews_executable` anchors lookup
        to ``sys.executable`` so MCP plugin hosts that launch the server
        without sourcing a venv still reach the sibling ``suews`` script
        (gh#1400).

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

    resolved_executable = (
        suews_executable
        if suews_executable is not None
        else _resolve_suews_executable()
    )
    cmd = [resolved_executable, subcmd, *args_list]

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
            f"Executable {resolved_executable!r} not found via "
            f"`sys.executable` sibling lookup or PATH; is supy installed "
            f"in the same environment as suews-mcp?"
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
