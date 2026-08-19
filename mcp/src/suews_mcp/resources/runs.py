"""``suews://runs/{run_id}/{provenance|diagnostics}`` resource."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Optional

from ..backend import ProjectRoot, SUEWSMCPSandboxError
from ..backend.provenance import read_diagnostics, read_provenance


def read_run_resource(
    run_id: str,
    kind: str,
    project_root: Optional[str] = None,
) -> dict[str, Any]:
    """Read ``provenance.json`` or ``diagnostics.json`` from a run dir.

    ``run_id`` is interpreted as a directory path under the project root.
    """
    if kind not in {"provenance", "diagnostics"}:
        return _err(
            f"Unsupported run resource kind {kind!r}; "
            "expected 'provenance' or 'diagnostics'.",
            run_id,
            kind,
        )

    root = ProjectRoot(project_root)
    try:
        run_dir = root.resolve(run_id)
    except SUEWSMCPSandboxError as exc:
        return _err(str(exc), run_id, kind)

    try:
        if kind == "provenance":
            payload = read_provenance(run_dir)
        else:
            payload = read_diagnostics(run_dir)
    except FileNotFoundError as exc:
        return _err(str(exc), run_id, kind)

    return {
        "status": "success",
        "data": {"run_id": run_id, "kind": kind, "payload": payload},
        "errors": [],
        "warnings": [],
        "meta": {"command": f"read_run_resource {run_id} {kind}"},
    }


def _err(message: str, run_id: str, kind: str) -> dict[str, Any]:
    return {
        "status": "error",
        "data": {"run_id": run_id, "kind": kind},
        "errors": [{"message": message}],
        "warnings": [],
        "meta": {"command": f"read_run_resource {run_id} {kind}"},
    }
