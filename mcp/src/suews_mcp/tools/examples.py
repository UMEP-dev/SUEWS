"""``list_examples`` and ``read_example`` MCP tools.

Examples are sourced from the installed supy's ``sample_data`` directory
plus an optional ``mcp/examples/`` directory shipped alongside this server.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from .validate import _error_envelope


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
    """Return the catalogue of available example configurations."""
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


def read_example(name: str) -> dict[str, Any]:
    """Return the raw text content of an example's files."""
    if name not in _BUILTIN_EXAMPLES:
        return _error_envelope(
            f"Example {name!r} is not in the built-in catalogue. "
            f"Available: {sorted(_BUILTIN_EXAMPLES)}",
            command=f"read_example {name}",
        )

    meta = _BUILTIN_EXAMPLES[name]
    sample_dir = _supy_sample_data_dir()
    files: list[dict[str, Any]] = []
    for role in ("config", "forcing"):
        path = sample_dir / meta[role]
        if path.exists():
            files.append({
                "role": role,
                "path": str(path),
                "content": path.read_text(encoding="utf-8"),
            })
        else:
            files.append({
                "role": role,
                "path": str(path),
                "content": None,
                "missing": True,
            })

    try:
        from supy.cmd.json_envelope import Envelope

        return Envelope.success(
            data={"name": name, "files": files},
            command=f"read_example {name}",
        ).to_dict()
    except ImportError:
        return {
            "status": "success",
            "data": {"name": name, "files": files},
            "errors": [],
            "warnings": [],
            "meta": {"command": f"read_example {name}"},
        }
