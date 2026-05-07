"""``query_knowledge`` and ``read_knowledge_manifest`` MCP tools.

Wrap ``suews knowledge query`` and ``suews knowledge manifest`` so that
agents can retrieve cited source evidence from the versioned SUEWS pack.
Each match carries a ``git_sha``, ``github_url``, ``repo_path`` and
``line_start``/``line_end`` so downstream answers can cite exactly where
each piece of evidence comes from.
"""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def query_knowledge(
    question: str,
    project_root: Optional[str] = None,
    pack: Optional[str] = None,
    limit: int = 5,
    scope: Optional[str] = None,
) -> dict[str, Any]:
    """Retrieve cited source evidence from the SUEWS knowledge pack.

    Parameters
    ----------
    question
        Free-text query (e.g. ``"STEBBS heating demand"``).
    project_root
        Override the per-session project root. Used to sandbox the
        ``--pack`` directory when supplied.
    pack
        Optional path to a knowledge pack directory. ``None`` uses the
        installed pack shipped with supy.
    limit
        Maximum number of matches to return.
    scope
        Restrict to a content type (``fortran``, ``rust``, ``python_api``,
        ``schema``).
    """
    if not question or not question.strip():
        return _error_envelope(
            "`question` is required and must be non-empty.",
            command="suews knowledge query",
        )

    args = [question, "--limit", str(int(limit))]

    pack_path: Optional[str] = None
    if pack:
        root = ProjectRoot(project_root)
        try:
            pack_path = str(root.resolve(pack))
        except SUEWSMCPSandboxError as exc:
            return _error_envelope(
                str(exc), command=f"suews knowledge query --pack {pack}"
            )
        args += ["--pack", pack_path]

    if scope:
        args += ["--scope", scope]

    try:
        return run_suews_cli("knowledge", ["query", *args])
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews knowledge query {question!r}"
        )


def read_knowledge_manifest(
    project_root: Optional[str] = None,
    pack: Optional[str] = None,
) -> dict[str, Any]:
    """Return the knowledge-pack manifest (version, git SHA, citation rules).

    Parameters
    ----------
    project_root
        Override the per-session project root. Used to sandbox the
        ``--pack`` directory when supplied.
    pack
        Optional path to a knowledge pack directory. ``None`` uses the
        installed pack shipped with supy.
    """
    args: list[str] = []
    if pack:
        root = ProjectRoot(project_root)
        try:
            pack_path = str(root.resolve(pack))
        except SUEWSMCPSandboxError as exc:
            return _error_envelope(
                str(exc), command=f"suews knowledge manifest --pack {pack}"
            )
        args += ["--pack", pack_path]

    try:
        return run_suews_cli("knowledge", ["manifest", *args])
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command="suews knowledge manifest")
