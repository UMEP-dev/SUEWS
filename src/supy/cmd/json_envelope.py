"""Standard JSON envelope for SUEWS unified CLI output.

Every ``suews`` subcommand emits this envelope. Every MCP tool consumes it.
The shape is fixed::

    {
        "status": "success" | "warning" | "error",
        "data": <command-specific payload>,
        "errors": [{"message": str, ...}, ...],
        "warnings": [str | {...}, ...],
        "meta": {
            "schema_version": str,
            "suews_version": str,
            "supy_version": str,
            "git_commit": str | null,
            "command": str,
            "started_at": str,  # ISO 8601 with trailing Z
            "ended_at": str,
        }
    }

Status rules:
- ``"success"`` if no errors and no warnings.
- ``"warning"`` if no errors but warnings present.
- ``"error"`` if any errors present.

This module is the single contract; never branch the shape per subcommand.
"""

from __future__ import annotations

import json
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, IO, Optional, Sequence, Union

EXIT_OK = 0
EXIT_USER_ERROR = 1
EXIT_SCHEMA_ERROR = 2
EXIT_RUN_FAILURE = 3
EXIT_ENV_ERROR = 4


def _now_iso() -> str:
    """Return current UTC time as ISO 8601 with trailing ``Z``."""
    return (
        datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
    )


def _git_commit() -> Optional[str]:
    """Return the current git commit short hash, or ``None`` if unavailable.

    Looks up the repository the supy package was installed from. Returns
    ``None`` for wheel installs without a ``.git`` directory.
    """
    try:
        import supy

        path_pkg = Path(supy.__file__).resolve().parent
    except Exception:
        return None

    for candidate in (path_pkg, *path_pkg.parents):
        if (candidate / ".git").exists():
            try:
                out = subprocess.run(
                    ["git", "-C", str(candidate), "rev-parse", "--short", "HEAD"],
                    check=False,
                    capture_output=True,
                    text=True,
                    timeout=2,
                )
            except (FileNotFoundError, subprocess.SubprocessError):
                return None
            if out.returncode == 0:
                return out.stdout.strip() or None
            return None
    return None


def _coerce_messages(items: Optional[Sequence[Any]]) -> list:
    """Coerce string entries to ``{"message": str}``; pass dicts through."""
    if not items:
        return []
    out: list = []
    for entry in items:
        if isinstance(entry, str):
            out.append({"message": entry})
        else:
            out.append(entry)
    return out


def _build_meta(command: str, started_at: Optional[str]) -> dict[str, Any]:
    from supy import __version__ as supy_version
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

    started = started_at or _now_iso()
    return {
        "schema_version": CURRENT_SCHEMA_VERSION,
        "suews_version": supy_version,
        "supy_version": supy_version,
        "git_commit": _git_commit(),
        "command": command,
        "started_at": started,
        "ended_at": _now_iso(),
    }


class Envelope:
    """Canonical JSON envelope for ``suews`` CLI output."""

    __slots__ = ("status", "data", "errors", "warnings", "meta")

    def __init__(
        self,
        status: str,
        data: dict[str, Any],
        errors: list,
        warnings: list,
        meta: dict[str, Any],
    ) -> None:
        self.status = status
        self.data = data
        self.errors = errors
        self.warnings = warnings
        self.meta = meta

    @classmethod
    def success(
        cls,
        data: Optional[dict[str, Any]] = None,
        command: str = "",
        warnings: Optional[Sequence[Any]] = None,
        started_at: Optional[str] = None,
    ) -> "Envelope":
        """Build a success/warning envelope (no errors)."""
        coerced_warnings = _coerce_warnings(warnings)
        status = "warning" if coerced_warnings else "success"
        return cls(
            status=status,
            data=dict(data) if data else {},
            errors=[],
            warnings=coerced_warnings,
            meta=_build_meta(command, started_at),
        )

    @classmethod
    def error(
        cls,
        errors: Sequence[Any],
        command: str = "",
        data: Optional[dict[str, Any]] = None,
        warnings: Optional[Sequence[Any]] = None,
        started_at: Optional[str] = None,
    ) -> "Envelope":
        """Build an error envelope."""
        return cls(
            status="error",
            data=dict(data) if data else {},
            errors=_coerce_messages(errors),
            warnings=_coerce_warnings(warnings),
            meta=_build_meta(command, started_at),
        )

    def to_dict(self) -> dict[str, Any]:
        return {
            "status": self.status,
            "data": self.data,
            "errors": self.errors,
            "warnings": self.warnings,
            "meta": self.meta,
        }

    def to_json(self, indent: int = 2) -> str:
        return json.dumps(self.to_dict(), indent=indent, ensure_ascii=False)

    def emit(self, stream: Optional[IO[str]] = None) -> None:
        """Write the envelope as JSON to ``stream`` (default stdout)."""
        text = self.to_json()
        target = stream if stream is not None else sys.stdout
        target.write(text)
        target.write("\n")
        target.flush()


def _coerce_warnings(items: Optional[Sequence[Any]]) -> list:
    """Warnings keep strings as strings (more lightweight than errors)."""
    if not items:
        return []
    return list(items)
