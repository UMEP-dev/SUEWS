"""``search_schema`` MCP tool — query the SUEWS YAML schema dump."""

from __future__ import annotations

from typing import Any

from ..backend import SUEWSMCPError, run_suews_cli
from .validate import _error_envelope


def search_schema(query: str = "", version: str = "current") -> dict[str, Any]:
    """Return matching schema entries for ``query`` at ``version``.

    Phase-1 implementation: shells to ``suews schema export --format json``
    and filters the dumped schema dict for keys / descriptions matching the
    query string. The CLI itself does not yet implement filtering — we
    perform it here on the parsed result.
    """
    try:
        envelope = run_suews_cli("schema", ["export"])
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command="suews schema export")

    if envelope.get("status") == "error":
        return envelope

    raw = envelope.get("data") or {}

    if not query:
        # No filter: return the full envelope unchanged.
        return envelope

    needle = query.lower()
    matches: list[dict[str, Any]] = []

    def _walk(node: Any, path: str = "") -> None:
        if isinstance(node, dict):
            for key, value in node.items():
                child_path = f"{path}.{key}" if path else key
                if needle in key.lower():
                    matches.append({"path": child_path, "value": _shallow(value)})
                _walk(value, child_path)
        elif isinstance(node, list):
            for idx, item in enumerate(node):
                _walk(item, f"{path}[{idx}]")
        else:
            if needle in str(node).lower() and len(str(node)) <= 200:
                matches.append({"path": path, "value": node})

    _walk(raw)

    envelope["data"] = {"matches": matches[:200], "n_matches": len(matches), "version": version}
    return envelope


def _shallow(value: Any) -> Any:
    """Trim deep substructures so search results stay small."""
    if isinstance(value, (dict, list)):
        return f"<{type(value).__name__}>"
    return value
