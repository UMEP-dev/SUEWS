"""``query_knowledge`` and ``read_knowledge_manifest`` MCP tools.

Wrap ``suews knowledge query`` and ``suews knowledge manifest`` so that
agents can retrieve cited source evidence from the versioned SUEWS pack.
Each match carries a ``git_sha``, ``github_url``, ``repo_path`` and
``line_start``/``line_end`` so downstream answers can cite exactly where
each piece of evidence comes from.
"""

from __future__ import annotations

import re
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


# Audience classification for knowledge-pack chunks (gh#1402).
#
# `query_knowledge` previously returned chunks drawn from the full
# source-evidence pack — Fortran sources, Pydantic data models, *and*
# internal validation pipeline docs — without telling the consumer
# which audience each chunk belonged to. An assistant that called
# `query_knowledge` first (instead of `search_schema`) would happily
# quote internal Fortran names like ``stebbsmethod`` /
# ``netradiationmethod`` back to the user as if they were YAML fields,
# producing configuration advice that fails validation.
#
# The audience tag is derived from the chunk's ``repo_path``. The
# patterns are deliberately broad — when in doubt fall back to
# ``developer_doc`` rather than ``user_yaml``.
_AUDIENCE_PATTERNS: tuple[tuple[str, str], ...] = (
    # Pydantic data models (the YAML schema's source of truth).
    ("user_yaml", "src/supy/data_model/"),
    # Fortran kernel and Rust bridge — runtime, not user-facing YAML.
    ("internal_runtime", "src/suews/src/"),
    ("internal_runtime", "src/suews_bridge/"),
    ("internal_runtime", "src/supy_driver/"),
    # Generated schema artefacts ship under docs/ but describe the
    # YAML surface.
    ("user_yaml", "docs/source/inputs/"),
    ("user_yaml", "docs/source/assets/schema"),
    # Everything else under docs/ is developer-facing prose.
    ("developer_doc", "docs/"),
    # Pipeline / orchestrator code is internal.
    ("internal_runtime", "src/supy/data_model/validation/pipeline/"),
)


def _classify_audience(repo_path: Optional[str]) -> str:
    if not repo_path:
        return "developer_doc"
    for audience, prefix in _AUDIENCE_PATTERNS:
        if repo_path.startswith(prefix):
            return audience
    return "developer_doc"


def _legacy_names_in_text(text: Optional[str]) -> list[dict[str, str]]:
    """Return ``[{legacy: ..., current: ...}]`` for legacy field names
    that appear as whole tokens in ``text`` (gh#1402).

    Backed by ``ALL_FIELD_RENAMES`` in supy's data-model layer. When
    the function cannot import the rename registry (older supy install)
    it returns an empty list — the audience tag alone is still
    actionable.
    """
    if not text:
        return []
    try:
        from supy.data_model.core.field_renames import ALL_FIELD_RENAMES
    except Exception:
        return []
    hits: list[dict[str, str]] = []
    seen: set[str] = set()
    for legacy, current in ALL_FIELD_RENAMES.items():
        if legacy in seen:
            continue
        # Whole-word match so partial substrings (e.g. ``method`` inside
        # ``methodology``) do not trigger a false positive.
        pattern = rf"(?<![A-Za-z0-9_]){re.escape(legacy)}(?![A-Za-z0-9_])"
        if re.search(pattern, text):
            hits.append({"legacy": legacy, "current": current})
            seen.add(legacy)
    return hits


def _annotate_match(match: dict[str, Any]) -> dict[str, Any]:
    """Attach ``audience`` and ``legacy_name_for`` annotations (gh#1402)."""
    annotated = dict(match)
    annotated["audience"] = _classify_audience(match.get("repo_path"))
    legacy_hits = _legacy_names_in_text(match.get("text"))
    if legacy_hits:
        annotated["legacy_name_for"] = legacy_hits
    return annotated


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
    """Annotate then trim per-match text according to ``mode``.

    Annotation runs before trimming so the legacy-name detector sees
    the full chunk text rather than the snippet-mode 2 KB prefix
    (gh#1402, gh#1403).
    """
    data = envelope.get("data") or {}
    matches = data.get("matches")
    if isinstance(matches, list):
        annotated = [_annotate_match(m) for m in matches]
        data = {**data, "matches": [_trim_match(m, mode) for m in annotated]}
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

    Audience annotations (gh#1402)
    ------------------------------

    Each match carries an ``audience`` tag derived from its
    ``repo_path``:

    - ``user_yaml`` — Pydantic data-model code or generated schema
      artefacts; safe to quote as the YAML field shape.
    - ``internal_runtime`` — Fortran kernel, Rust bridge, or
      validation pipeline; field names here are NOT necessarily the
      ones the user writes in YAML.
    - ``developer_doc`` — everything else (release notes, design
      docs, tutorials).

    When a match's text contains one or more legacy field names
    (taken from ``ALL_FIELD_RENAMES``), an additional
    ``legacy_name_for`` list is attached — e.g.
    ``[{"legacy": "stebbsmethod", "current": "stebbs"}]``. Use the
    ``current`` form when generating user-facing YAML.

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
