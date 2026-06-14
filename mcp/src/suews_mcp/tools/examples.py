"""``list_examples`` and ``read_example`` MCP tools.

Examples are sourced from the installed supy's ``sample_data`` directory
plus an optional ``mcp/examples/`` directory shipped alongside this server.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Optional

from .validate import _error_envelope


# Envelope size policy (gh#1403). The full sample_config.yml is ~1.2 MB
# of YAML — well past every host's per-tool token cap. We default to a
# bounded summary and require an explicit ``mode="file"`` opt-in to
# return full file content. The summary-mode preview cap is small
# enough (80 lines * ~80 chars) to fit comfortably under any realistic
# 16k–32k token budget across files in the catalogue.
_SUMMARY_PREVIEW_LINES = 80
_FILE_MODE_BYTES_CAP = 64_000  # ~64 KB per single-file response.


def _supy_sample_data_dir() -> Path:
    """Locate the bundled ``supy/sample_data/`` directory.

    Resolves via ``supy.__file__`` rather than ``importlib.resources.files``
    because meson-python's editable loader returns a ``Traversable`` that is
    not directly convertible to ``pathlib.Path``.
    """
    import supy

    return Path(supy.__file__).resolve().parent / "sample_data"


_BUILTIN_EXAMPLES: dict[str, dict[str, str]] = {
    "simple-urban": {
        "description": (
            "Single-grid urban case based on the bundled sample_config.yml. "
            "Suitable as a starting point and for teaching demonstrations."
        ),
        "config": "sample_config.yml",
        "forcing": "Kc_2012_data_60.txt",
    },
}


def list_examples() -> dict[str, Any]:
    """**Use this before `read_example`** to discover which sample
    configurations the installed package ships. Cheap (no file IO
    beyond stat) and the right starting point when the user has not
    named a specific example (gh#1407).
    """
    sample_dir = _supy_sample_data_dir()
    examples: list[dict[str, Any]] = []

    for name, meta in _BUILTIN_EXAMPLES.items():
        cfg = sample_dir / meta["config"]
        forcing = sample_dir / meta["forcing"]
        examples.append({
            "name": name,
            "description": meta["description"],
            "files": [
                {"role": "config", "path": str(cfg), "exists": cfg.exists()},
                {"role": "forcing", "path": str(forcing), "exists": forcing.exists()},
            ],
        })

    try:
        from supy.cmd.json_envelope import Envelope

        return Envelope.success(
            data={"examples": examples},
            command="list_examples",
        ).to_dict()
    except ImportError:
        return {
            "status": "success",
            "data": {"examples": examples},
            "errors": [],
            "warnings": [],
            "meta": {"command": "list_examples"},
        }


def _file_entry(path: Path, role: str) -> dict[str, Any]:
    """Build the common file metadata block (no content)."""
    return {
        "role": role,
        "path": str(path),
        "size_bytes": path.stat().st_size if path.exists() else 0,
        "exists": path.exists(),
    }


def _summary_preview(path: Path) -> dict[str, Any]:
    """Return the first ``_SUMMARY_PREVIEW_LINES`` of ``path``.

    Truncated previews carry ``truncated=True`` and the total line count
    so the agent knows there is more to fetch via ``mode="file"``.
    """
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return {"preview": None, "truncated": False, "total_lines": None}
    lines = text.splitlines()
    truncated = len(lines) > _SUMMARY_PREVIEW_LINES
    preview = "\n".join(lines[:_SUMMARY_PREVIEW_LINES])
    return {
        "preview": preview,
        "truncated": truncated,
        "total_lines": len(lines),
    }


def read_example(
    name: str,
    mode: str = "summary",
    path: Optional[str] = None,
) -> dict[str, Any]:
    """**Use this after `init_case` if the user wants to study a
    specific bundled example** (rather than scaffold a new case).
    Default `mode="summary"` gives you a bounded preview; opt in to
    `mode="file"` only when you need the raw content of one specific
    file. Do not call this when you already have a scaffolded case
    from `init_case` — the file is already on disk (gh#1407).

    What this tool returns depends on ``mode`` (gh#1403)
    --------------------------------------------------------------------

    - ``mode="summary"`` (default): file list with sizes plus a
      first-``_SUMMARY_PREVIEW_LINES``-line preview per file. Bounded
      so the response always fits in an agent token budget.
    - ``mode="manifest"``: file list with sizes only, no content.
      Cheapest mode; useful when the agent only needs to know what's
      inside the example.
    - ``mode="file"``: return the full text of a single file specified
      by ``path`` (the role-anchored path emitted by ``mode="summary"``
      or ``mode="manifest"``), capped at ``_FILE_MODE_BYTES_CAP``.

    The previous unbounded behaviour returned the full sample bundle —
    1.2 MB of YAML — in one envelope, which every MCP host
    (Claude Code, Codex, Claude Desktop) rejected as "result exceeds
    maximum allowed tokens". Agents either lost the content entirely or
    fell into a loop of duplicate `query_knowledge` calls trying to
    rebuild the example piecewise.

    Parameters
    ----------
    name
        Example name (must be in :data:`_BUILTIN_EXAMPLES`).
    mode
        Output mode. One of ``"summary"`` (default), ``"manifest"``,
        ``"file"``.
    path
        Required when ``mode="file"``. The path of the single file to
        return; should match a ``path`` value emitted by an earlier
        ``mode="summary"`` or ``mode="manifest"`` call.
    """
    if name not in _BUILTIN_EXAMPLES:
        return _error_envelope(
            f"Example {name!r} is not in the built-in catalogue. "
            f"Available: {sorted(_BUILTIN_EXAMPLES)}",
            command=f"read_example {name}",
        )

    valid_modes = {"summary", "manifest", "file"}
    if mode not in valid_modes:
        return _error_envelope(
            f"Unknown mode {mode!r}. Expected one of {sorted(valid_modes)}.",
            command=f"read_example {name} mode={mode}",
        )

    meta = _BUILTIN_EXAMPLES[name]
    sample_dir = _supy_sample_data_dir()
    file_paths = {role: sample_dir / meta[role] for role in ("config", "forcing")}

    if mode == "file":
        if not path:
            return _error_envelope(
                "mode='file' requires the `path` argument; pass one of the "
                "paths reported by an earlier mode='summary' or "
                "mode='manifest' call.",
                command=f"read_example {name} mode=file",
            )
        target = next(
            (p for p in file_paths.values() if str(p) == path or p.name == path),
            None,
        )
        if target is None or not target.exists():
            return _error_envelope(
                f"Path {path!r} is not part of example {name!r}; expected "
                f"one of: {[str(p) for p in file_paths.values()]}.",
                command=f"read_example {name} mode=file path={path}",
            )
        text = target.read_text(encoding="utf-8")
        truncated = len(text.encode("utf-8")) > _FILE_MODE_BYTES_CAP
        if truncated:
            text = text.encode("utf-8")[:_FILE_MODE_BYTES_CAP].decode(
                "utf-8", errors="ignore"
            )
        files = [
            {
                "role": next(role for role, p in file_paths.items() if p == target),
                "path": str(target),
                "size_bytes": target.stat().st_size,
                "content": text,
                "truncated": truncated,
                "byte_cap": _FILE_MODE_BYTES_CAP,
            }
        ]
    elif mode == "manifest":
        files = [_file_entry(p, role) for role, p in file_paths.items()]
    else:  # summary
        files = []
        for role, p in file_paths.items():
            entry = _file_entry(p, role)
            if p.exists():
                entry.update(_summary_preview(p))
            files.append(entry)

    data = {
        "name": name,
        "mode": mode,
        "files": files,
        "next_steps": [
            f"call read_example with mode='file' and path=<one of the file paths above> "
            f"to fetch a single file's full content (capped at {_FILE_MODE_BYTES_CAP} bytes)",
        ] if mode in {"summary", "manifest"} else [],
    }

    try:
        from supy.cmd.json_envelope import Envelope

        return Envelope.success(
            data=data,
            command=f"read_example {name} mode={mode}",
        ).to_dict()
    except ImportError:
        return {
            "status": "success",
            "data": data,
            "errors": [],
            "warnings": [],
            "meta": {"command": f"read_example {name} mode={mode}"},
        }
