"""``search_schema`` MCP tool — query the SUEWS YAML schema dump."""

from __future__ import annotations

from typing import Any

from ..backend import SUEWSMCPError, run_suews_cli
from .validate import _error_envelope


def search_schema(query: str = "", version: str = "current") -> dict[str, Any]:
    """**Use this when the user asks "what is the YAML field for X?"**
    Cheaper than `query_knowledge` for field-name lookups, and the
    result reflects the *current* schema rather than the
    knowledge-pack's snapshot — so it never returns a legacy /
    deprecated name (gh#1407).

    Phase-1 implementation: shells to the no-subcommand envelope path
    ``suews schema --version <v> --format json`` and filters the dumped
    schema dict for keys / descriptions matching the query string. The
    CLI itself does not yet implement filtering — we perform it here on
    the parsed result.
    """
    args: list[str] = []
    if version and version != "current":
        args += ["--version", version]
    try:
        envelope = run_suews_cli("schema", args)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc),
            command=("suews schema " + " ".join(args)).strip(),
        )

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


def _shallow(value: Any, depth: int = 0) -> Any:
    """Trim deep substructures so search results stay small.

    A matched schema field is itself a dict (``{type, description,
    default, units, ...}``) — replacing the whole dict with the literal
    ``"<dict>"`` strips exactly the metadata the caller wanted. So at
    ``depth=0`` keep scalar entries verbatim and only stub the nested
    structures; at ``depth>=1`` collapse dicts/lists to their type name.

    Special case at ``depth=0``: a JSON-Schema object node has a
    ``properties`` dict whose keys are the field names the user actually
    wants to see (e.g. ``EvetrProperties.properties`` lists every field
    of the evergreen-tree config block). Replace such a dict with the
    sorted list of its keys so callers can discover field names without
    a follow-up call, while still keeping the response bounded.

    Long string scalars (>200 chars) are truncated to keep envelopes
    bounded.
    """
    if isinstance(value, dict):
        if depth >= 1:
            return f"<{type(value).__name__}>"
        result: dict[str, Any] = {}
        for k, v in value.items():
            if k == "properties" and isinstance(v, dict):
                result[k] = sorted(v.keys())
            else:
                result[k] = _shallow(v, depth + 1)
        return result
    if isinstance(value, list):
        if depth >= 1:
            return f"<{type(value).__name__}>"
        return [_shallow(item, depth + 1) for item in value[:5]]
    if isinstance(value, str) and len(value) > 200:
        return value[:200] + "..."
    return value
