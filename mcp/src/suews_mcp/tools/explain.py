"""explain() MCP tool handler."""

from __future__ import annotations

import re
from typing import Any

from ..backend.base import SUEWSBackend


def _truncate(text: str, max_chars: int) -> tuple[str, bool]:
    if len(text) <= max_chars:
        return text, False
    trimmed = text[:max_chars]
    if " " in trimmed:
        trimmed = trimmed.rsplit(" ", 1)[0]
    return f"{trimmed}...", True


def _extract_variable_hint(topic: str) -> str | None:
    known = ["LAI", "QH", "QE", "QS", "Tsurf", "snow", "runoff", "albedo"]
    lowered = topic.lower()
    for name in known:
        if name.lower() in lowered:
            return name
    upper_tokens = re.findall(r"\b[A-Z]{2,}\b", topic)
    if upper_tokens:
        return upper_tokens[0]
    return None


def _extract_symptom_hint(topic: str) -> str | None:
    from ..backend.local import symptom_from_text

    return symptom_from_text(topic)


def _extract_subroutine_hint(topic: str, match: dict[str, Any]) -> str | None:
    lowered = topic.lower()
    for routine in match.get("subroutines", []):
        if not isinstance(routine, dict):
            continue
        name = str(routine.get("name", ""))
        if name and name.lower() in lowered:
            return name
    return None


async def handle_explain(
    backend: SUEWSBackend,
    topic: str,
    *,
    show_source: bool = False,
    context: dict[str, Any] | None = None,
    max_chars: int = 7000,
) -> dict[str, Any]:
    """Return concept, source, or diagnostic guidance for a topic."""
    if context is not None:
        variable_hint = _extract_variable_hint(topic) or topic
        symptom_hint = _extract_symptom_hint(topic)
        diagnostic = await backend.get_diagnostic(
            variable=variable_hint,
            symptom=symptom_hint,
            context=context,
        )
        return {
            "mode": "diagnostic",
            "topic": topic,
            "requested_variable": variable_hint,
            "detected_symptom": diagnostic.get("detected_symptom"),
            **diagnostic,
        }

    source_index = await backend.get_source_index(query=topic)
    if show_source:
        best_match = source_index.get("best_match")
        file_name = topic if best_match is None else str(best_match.get("file_name", topic))
        subroutine = None
        if isinstance(best_match, dict):
            subroutine = _extract_subroutine_hint(topic, best_match)
        source = await backend.get_source_excerpt(file_name=file_name, subroutine=subroutine)
        excerpt, truncated = _truncate(str(source["source"]), max_chars=max_chars)
        return {
            "mode": "source",
            "topic": topic,
            "show_source": True,
            "query_result": source_index,
            **source,
            "source": excerpt,
            "truncated": truncated,
        }

    if source_index.get("has_matches"):
        return {
            "mode": "source",
            "topic": topic,
            "show_source": False,
            **source_index,
        }

    result = await backend.get_knowledge(topic=topic)
    content, truncated = _truncate(str(result["content"]), max_chars=max_chars)

    return {
        "mode": "concept",
        "topic": topic,
        "matched_topic": result["matched_topic"],
        "score": result["score"],
        "content": content,
        "truncated": truncated,
        "available_topics": result["available_topics"],
    }
