"""``suews://knowledge/{...}`` resource family.

Two slugs are supported:

- ``suews://knowledge/manifest`` — pack manifest (version, git SHA, etc.).
- ``suews://knowledge/query/{question}`` — top-N cited evidence for a
  question. The question is the URI-encoded free-text query.
"""

from __future__ import annotations

from typing import Any

from ..tools.knowledge import query_knowledge, read_knowledge_manifest


def read_knowledge_manifest_resource() -> dict[str, Any]:
    """Resource alias for :func:`read_knowledge_manifest`."""
    return read_knowledge_manifest()


def read_knowledge_query_resource(question: str, limit: int = 5) -> dict[str, Any]:
    """Resource alias for :func:`query_knowledge` with default scope."""
    return query_knowledge(question=question, limit=limit)
