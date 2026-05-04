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


# Envelope size policy (gh#1403). The CLI ``suews knowledge query``
# returns one match per chunk with the chunk's full text inline. A
# single Fortran-module match can exceed 8 KB, and a default
# ``limit=5`` query routinely exceeds 50 KB — past every host's
# per-tool token cap, so the envelope was silently persisted to disk
# and the agent saw only a 2 KB preview. Three knobs together keep
# the default response bounded:
#
#   - smaller default limit (3 instead of 5)
#   - per-match text cap in the default ``"snippet"`` mode
#   - explicit ``"summary"`` mode that drops text entirely
#
# Hosts that need full chunk content opt in with ``mode="full"``.
_DEFAULT_LIMIT = 3
_SNIPPET_BYTES_CAP = 2_000


def _trim_match(match: dict[str, Any], mode: str) -> dict[str, Any]:
    """Apply the size policy to a single match envelope entry."""
    if mode == "full":
        return match
    if mode == "summary":
        # Drop ``text`` entirely; keep all the citation fields so the
        # agent can still link the user to the source.
        trimmed = {k: v for k, v in match.items() if k != "text"}
        trimmed["text"] = None
        trimmed["text_truncated"] = True
        return trimmed
    # mode == "snippet"
    text = match.get("text")
    if not isinstance(text, str):
        return match
    encoded = text.encode("utf-8")
    if len(encoded) <= _SNIPPET_BYTES_CAP:
        return match
    capped = encoded[:_SNIPPET_BYTES_CAP].decode("utf-8", errors="ignore")
    trimmed = dict(match)
    trimmed["text"] = capped
    trimmed["text_truncated"] = True
    trimmed["text_full_bytes"] = len(encoded)
    trimmed["snippet_byte_cap"] = _SNIPPET_BYTES_CAP
    return trimmed


def _apply_size_policy(envelope: dict[str, Any], mode: str) -> dict[str, Any]:
    """Trim per-match text in the envelope according to ``mode``."""
    data = envelope.get("data") or {}
    matches = data.get("matches")
    if isinstance(matches, list):
        data = {**data, "matches": [_trim_match(m, mode) for m in matches]}
        data["mode"] = mode
        envelope = {**envelope, "data": data}
    return envelope


def query_knowledge(
    question: str,
    project_root: Optional[str] = None,
    pack: Optional[str] = None,
    limit: int = _DEFAULT_LIMIT,
    scope: Optional[str] = None,
    mode: str = "snippet",
) -> dict[str, Any]:
    """**Use only when you need citable evidence** for a claim about
    SUEWS physics, schema history, or source-level behaviour. For
    "what is the field name?" prefer `inspect_config` or
    `search_schema` first — they cost less and are not subject to the
    knowledge-pack staleness window (gh#1407).

    Envelope size policy (gh#1403)
    ------------------------------

    A typical knowledge match is one source chunk (Fortran module,
    Pydantic class, docs page); chunks routinely exceed the agent's
    per-tool-result token budget. ``mode`` keeps the default response
    bounded; opt-in ``mode="full"`` returns the original CLI envelope.

    - ``mode="snippet"`` (default): each match's ``text`` is capped at
      ``_SNIPPET_BYTES_CAP`` bytes; ``text_truncated`` and
      ``text_full_bytes`` are added so the agent knows there is more.
    - ``mode="summary"``: each match's ``text`` is dropped entirely;
      citation metadata (``git_sha``, ``github_url``, ``repo_path``,
      ``line_start`` / ``line_end``, ``content_type``, ``score``) is
      preserved so the agent can still point the user at the source.
    - ``mode="full"``: pass-through; no trimming.

    The default ``limit`` is also lowered from 5 to 3 because a single
    snippet-mode match is ~2 KB and three matches comfortably fit in
    any realistic budget.

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
        Maximum number of matches to return. Default 3.
    scope
        Restrict to a content type (``fortran``, ``rust``, ``python_api``,
        ``schema``).
    mode
        Output mode. One of ``"snippet"`` (default), ``"summary"``,
        ``"full"``.
    """
    if not question or not question.strip():
        return _error_envelope(
            "`question` is required and must be non-empty.",
            command="suews knowledge query",
        )

    valid_modes = {"snippet", "summary", "full"}
    if mode not in valid_modes:
        return _error_envelope(
            f"Unknown mode {mode!r}. Expected one of {sorted(valid_modes)}.",
            command=f"suews knowledge query {question!r} mode={mode}",
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
        envelope = run_suews_cli("knowledge", ["query", *args])
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews knowledge query {question!r}"
        )

    return _apply_size_policy(envelope, mode)


def read_knowledge_manifest(
    project_root: Optional[str] = None,
    pack: Optional[str] = None,
) -> dict[str, Any]:
    """**Call this when the user asks "which version of the knowledge
    pack am I querying?"** or before any answer that needs to cite a
    `git_sha`. Cheap (no chunk retrieval) and the right preamble for
    a session that will rely on `query_knowledge` for evidence
    (gh#1407).

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
