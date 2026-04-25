"""search() MCP tool handler."""

from __future__ import annotations

import asyncio
from typing import Any

from ..backend.base import SUEWSBackend

VALID_DETAIL_LEVELS = {"index", "schema", "sample"}


async def handle_search(
    backend: SUEWSBackend,
    query: str,
    type_name: str | None = None,
    detail_level: str = "index",
) -> dict[str, Any]:
    """Search index or return type-specific schema/sample details.

    When type_name is omitted, searches both the type index and
    the data catalogue, returning merged results.
    """
    detail = detail_level.strip().lower()
    if detail not in VALID_DETAIL_LEVELS:
        raise ValueError(
            f"Invalid detail_level '{detail_level}'. "
            "Use one of: index, schema, sample."
        )

    if type_name is not None and type_name.strip():
        return await backend.get_schema(type_name=type_name, detail_level=detail)

    # No type_name: search both type index and data catalogue concurrently.
    type_result, catalogue_result = await asyncio.gather(
        backend.list_types(query=query),
        backend.search_catalogue(query=query),
    )

    # Compact the type index: keep only type_name + category + description
    # to stay within token budget.
    compact_categories: dict[str, list[dict[str, str]]] | None = None
    categories = type_result.get("categories")
    if isinstance(categories, dict):
        compact_categories = {}
        for cat_name, entries in categories.items():
            if not isinstance(entries, list):
                continue
            compact_categories[cat_name] = [
                {
                    "type_name": entry.get("type_name", ""),
                    "description": entry.get("description", ""),
                }
                for entry in entries
                if isinstance(entry, dict)
            ]

    return {
        "query": type_result.get("query", query),
        "types": {
            "total": type_result.get("total_types", 0),
            "category_counts": type_result.get("category_counts"),
            "categories": compact_categories,
        },
        "data_catalogue": {
            "matched_sections": catalogue_result.get("matched_sections", []),
            "available_sections": catalogue_result.get("available_sections", []),
        },
        "tips": [
            "Use type_name to inspect one type in detail, e.g. type_name='ohm-state'.",
            "Use detail_level='schema' for machine-readable fields.",
            "Use detail_level='sample' to request example values.",
        ],
    }
