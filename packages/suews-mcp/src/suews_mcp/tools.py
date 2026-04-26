"""Tool helpers for the SUEWS MCP server."""

from __future__ import annotations

import json
import logging
from pathlib import Path
import shutil
import subprocess
import time
from typing import Any

ALLOWED_COMMANDS = (
    "suews-validate",
    "suews-schema",
    "suews",
    "suews-run",
    "suews-convert",
)

DEFAULT_TIMEOUT_S = 30
MAX_TIMEOUT_S = 120


class PathOutsideRootError(ValueError):
    """Raised when a user-provided path escapes the configured project root."""


class CommandNotAllowedError(ValueError):
    """Raised when code attempts to invoke a non-allowlisted command."""


def resolve_root(root: Path) -> Path:
    """Resolve and validate the MCP project root."""
    path_root = Path(root).expanduser().resolve(strict=True)
    if not path_root.is_dir():
        raise NotADirectoryError(f"root is not a directory: {path_root}")
    return path_root


def resolve_under_root(root: Path, value: str) -> Path:
    """Resolve a path argument and ensure it remains inside root."""
    path_root = resolve_root(root)
    path_input = Path(value).expanduser()
    if not path_input.is_absolute():
        path_input = path_root / path_input

    path_resolved = path_input.resolve(strict=False)
    try:
        path_resolved.relative_to(path_root)
    except ValueError as exc:
        raise PathOutsideRootError(
            f"path escapes configured root: {value}"
        ) from exc
    path_resolved.resolve(strict=True)
    return path_resolved


def relative_to_root(root: Path, path: Path) -> str:
    """Return a path relative to root when possible."""
    try:
        return str(path.resolve(strict=False).relative_to(resolve_root(root)))
    except ValueError:
        return str(path)


def clamp_timeout(timeout_s: float | None) -> float:
    """Normalise timeout values for subprocess execution."""
    if timeout_s is None:
        return float(DEFAULT_TIMEOUT_S)
    timeout = float(timeout_s)
    if timeout <= 0:
        raise ValueError("timeout_s must be positive")
    return min(timeout, float(MAX_TIMEOUT_S))


def ensure_command_available(command: str) -> str:
    """Return the executable path for an allowlisted command."""
    if command not in ALLOWED_COMMANDS:
        raise CommandNotAllowedError(f"command is not allowlisted: {command}")
    executable = shutil.which(command)
    if executable is None:
        raise FileNotFoundError(f"required SUEWS executable not found: {command}")
    return executable


def run_allowlisted_command(
    command: str,
    args: list[str],
    *,
    cwd: Path,
    timeout_s: float,
) -> subprocess.CompletedProcess[str]:
    """Run an allowlisted SUEWS command without invoking a shell."""
    executable = ensure_command_available(command)
    return subprocess.run(
        [executable, *args],
        cwd=str(cwd),
        capture_output=True,
        text=True,
        timeout=timeout_s,
        check=False,
    )


def _json_preview(text: str, limit: int = 1000) -> str:
    """Return a compact output preview for diagnostics."""
    compact = text.strip()
    if len(compact) <= limit:
        return compact
    return compact[:limit] + "...[truncated]"


def validate_config(
    root: Path,
    config_path: str,
    *,
    schema_version: str | None = None,
    timeout_s: float | None = DEFAULT_TIMEOUT_S,
    logger: logging.Logger | None = None,
) -> dict[str, Any]:
    """Validate a SUEWS YAML configuration through the canonical CLI."""
    path_root = resolve_root(root)
    path_config = resolve_under_root(path_root, config_path)
    timeout = clamp_timeout(timeout_s)
    args = ["--dry-run", "--format", "json"]
    if schema_version:
        args.extend(["--schema-version", schema_version])
    args.append(str(path_config))

    log = logger or logging.getLogger(__name__)
    started = time.monotonic()
    timed_out = False
    return_code: int | None = None
    try:
        proc = run_allowlisted_command(
            "suews-validate",
            args,
            cwd=path_root,
            timeout_s=timeout,
        )
        return_code = proc.returncode
    except subprocess.TimeoutExpired as exc:
        timed_out = True
        duration_s = time.monotonic() - started
        log.info(
            "tool=validate_config root=%s path=%s exit_code=%s timeout=%s duration=%.3f",
            path_root,
            relative_to_root(path_root, path_config),
            return_code,
            timed_out,
            duration_s,
        )
        return {
            "status": "error",
            "isError": True,
            "error_code": "TIMEOUT",
            "message": f"suews-validate exceeded {timeout:g} seconds",
            "command": {
                "name": "suews-validate",
                "args": args,
                "exit_code": None,
                "duration_s": duration_s,
                "timeout_s": timeout,
                "timed_out": True,
            },
            "path": relative_to_root(path_root, path_config),
            "stdout": _json_preview(exc.stdout or ""),
            "stderr": _json_preview(exc.stderr or ""),
        }

    duration_s = time.monotonic() - started
    log.info(
        "tool=validate_config root=%s path=%s exit_code=%s timeout=%s duration=%.3f",
        path_root,
        relative_to_root(path_root, path_config),
        return_code,
        timed_out,
        duration_s,
    )

    try:
        parsed = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        return {
            "status": "error",
            "isError": True,
            "error_code": "INVALID_JSON",
            "message": f"suews-validate did not return valid JSON: {exc}",
            "command": {
                "name": "suews-validate",
                "args": args,
                "exit_code": proc.returncode,
                "duration_s": duration_s,
                "timeout_s": timeout,
                "timed_out": False,
            },
            "path": relative_to_root(path_root, path_config),
            "stdout": _json_preview(proc.stdout),
            "stderr": _json_preview(proc.stderr),
        }

    is_error = proc.returncode != 0 or parsed.get("status") not in {"success", "ok"}
    status = "error" if is_error else "success"
    return {
        "status": status,
        "isError": is_error,
        "command": {
            "name": "suews-validate",
            "args": args,
            "exit_code": proc.returncode,
            "duration_s": duration_s,
            "timeout_s": timeout,
            "timed_out": False,
        },
        "path": relative_to_root(path_root, path_config),
        "schema_version": schema_version,
        "result": parsed,
        "stderr": _json_preview(proc.stderr),
    }
