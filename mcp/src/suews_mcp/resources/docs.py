"""``suews://docs/{slug}`` resource — exposes selected doc fragments."""

from __future__ import annotations

from pathlib import Path
from typing import Any

# Slug -> relative file path (under the suews repo's docs/source/ tree).
# The MCP server resolves these against the *checked-out* repo, not against
# the installed supy package, because docs are not bundled with the wheel.
_DOC_SLUGS: dict[str, list[str]] = {
    "configuration/yaml": [
        "docs/source/inputs/yaml/index.rst",
        "docs/source/inputs/yaml_input.rst",
    ],
    "forcing-data": [
        "docs/source/inputs/met_input.rst",
        "docs/source/inputs/forcing.rst",
    ],
    "output-variables": [
        "docs/source/output_files/output_files.rst",
        "docs/source/output_files/index.rst",
    ],
}


def _repo_root() -> Path:
    """Best-effort guess at the suews repo root.

    1. ``SUEWS_REPO_ROOT`` env var, if set.
    2. The parent of the installed supy package (works for editable installs).
    3. CWD.
    """
    import os

    env_root = os.environ.get("SUEWS_REPO_ROOT")
    if env_root:
        return Path(env_root).resolve()

    try:
        import supy

        supy_path = Path(supy.__file__).resolve().parent
        # In an editable install, supy lives at <repo>/src/supy.
        if supy_path.parent.name == "src":
            return supy_path.parent.parent.resolve()
    except ModuleNotFoundError:
        pass

    return Path.cwd().resolve()


def read_doc(slug: str) -> dict[str, Any]:
    """Return the raw text content of a doc fragment by slug."""
    if slug not in _DOC_SLUGS:
        return {
            "status": "error",
            "data": {},
            "errors": [
                {
                    "message": f"Unknown doc slug {slug!r}. "
                    f"Available: {sorted(_DOC_SLUGS)}"
                }
            ],
            "warnings": [],
            "meta": {"command": f"read_doc {slug}"},
        }

    root = _repo_root()
    snippets: list[dict[str, Any]] = []
    for relpath in _DOC_SLUGS[slug]:
        path = root / relpath
        if path.exists():
            snippets.append({
                "path": str(path),
                "content": path.read_text(encoding="utf-8", errors="replace"),
            })
        else:
            snippets.append({"path": str(path), "content": None, "missing": True})

    return {
        "status": "success" if snippets else "warning",
        "data": {"slug": slug, "snippets": snippets},
        "errors": [],
        "warnings": [
            f"Doc fragment not found at {s['path']}"
            for s in snippets
            if s.get("missing")
        ],
        "meta": {"command": f"read_doc {slug}"},
    }
