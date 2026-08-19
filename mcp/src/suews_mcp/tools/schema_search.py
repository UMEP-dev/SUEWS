"""``search_schema`` MCP tool — query the SUEWS YAML schema dump.

The query is tokenised and expanded through a small domain-alias map so
that natural-language questions ("leaf area index", "green
infrastructure vegetation", "frontal area index for roughness") reach
the relevant schema fields, rather than requiring the caller to know the
exact field token. Each schema node is scored by how many distinct query
needles appear in its key path, description, or display name; matches are
returned highest-score-first.
"""

from __future__ import annotations

import copy
import re
from typing import Any

from ..backend import SUEWSMCPError, run_suews_cli
from .validate import _error_envelope

# Tokens that carry no schema-discriminating signal. Dropped before matching.
_STOPWORDS: frozenset[str] = frozenset({
    "the", "and", "for", "with", "this", "that", "these", "those", "are", "was",
    "want", "need", "use", "used", "using", "get", "from", "into", "via", "per",
    "model", "suews", "data", "value", "values", "set", "field", "fields",
    "parameter", "parameters", "param", "params", "option", "options", "config",
    "configuration", "what", "which", "how", "where", "when", "why", "you",
    "your", "can", "should", "would", "does", "about", "most", "important",
    "describe", "describing", "area", "areas", "assign", "appropriate", "region",
})

# Natural-language term -> extra schema tokens to also search for. The token
# itself is always searched; these are supplements drawn from SUEWS field
# naming so a user does not have to know the abbreviation.
_ALIASES: dict[str, list[str]] = {
    "leaf": ["lai"],
    "lai": ["lai"],
    "vegetation": ["evetr", "dectr", "grass", "veg"],
    "veg": ["evetr", "dectr", "grass"],
    "green": ["evetr", "dectr", "grass"],
    "greenery": ["evetr", "dectr", "grass"],
    "greenspace": ["evetr", "dectr", "grass"],
    "infrastructure": ["evetr", "dectr", "grass"],
    "tree": ["evetr", "dectr"],
    "trees": ["evetr", "dectr"],
    "evergreen": ["evetr"],
    "deciduous": ["dectr"],
    "grass": ["grass"],
    "lawn": ["grass"],
    "irrigation": ["irrigation", "wateruse", "water_use"],
    "irrigated": ["irrigation", "wateruse"],
    "watering": ["irrigation", "wateruse"],
    "albedo": ["alb"],
    "reflectance": ["alb"],
    "emissivity": ["emis"],
    "conductance": ["conductance", "maxconductance", "g_", "gs"],
    "stomatal": ["conductance", "g_"],
    "roughness": ["z0", "zd", "roughness", "fai", "frontal"],
    "frontal": ["frontal", "fai"],
    "fai": ["fai", "frontal"],
    "building": ["bldgs", "building"],
    "buildings": ["bldgs", "building"],
    "morphology": ["height", "fai", "building_scale", "building_frac", "z0", "zd"],
    "morphometric": ["height", "fai", "building_scale", "building_frac"],
    "height": ["height"],
    "paved": ["paved"],
    "pavement": ["paved"],
    "road": ["paved"],
    "impervious": ["paved", "bldgs"],
    "soil": ["bsoil", "soil"],
    "bare": ["bsoil"],
    "water": ["water"],
    "waterbody": ["water"],
    "snow": ["snow"],
    "forcing": ["forcing", "met"],
    "meteorology": ["forcing", "met"],
    "meteorological": ["forcing", "met"],
    "met": ["forcing"],
    "temperature": ["temp", "t2", "tair", "tsfc"],
    "humidity": ["rh", "humid"],
    "anthropogenic": ["anthropogenic", "qf", "ah"],
    "heat": ["heat", "qf"],
    "storage": ["storage", "ohm"],
    "radiation": ["radiation", "narp", "kdown", "ldown"],
    "porosity": ["porosity", "poros"],
    "cover": ["land_cover", "sfr"],
    "fraction": ["frac", "sfr", "fraction"],
}

_TOKEN_RE = re.compile(r"[a-z0-9_]+")
_MIN_TOKEN_LEN = 3

# The full schema dump is static per version, but `suews schema --format json`
# costs ~10s per call. The MCP server is long-lived, so cache the parsed
# envelope per version — first query pays the cost, the rest are instant.
# Only successful dumps are cached (errors fall through and are retried).
_SCHEMA_CACHE: dict[str, dict[str, Any]] = {}


def _load_schema(version: str) -> dict[str, Any]:
    """Return the `suews schema` envelope for `version`, cached per process.

    Raises ``SUEWSMCPError`` (propagated from ``run_suews_cli``) on a CLI
    failure; error envelopes are not cached so a transient failure does
    not poison the cache.
    """
    key = version or "current"
    cached = _SCHEMA_CACHE.get(key)
    if cached is not None:
        return cached
    args: list[str] = []
    if key != "current":
        args += ["--version", key]
    envelope = run_suews_cli("schema", args)
    if envelope.get("status") != "error":
        _SCHEMA_CACHE[key] = envelope
    return envelope


def _needles(query: str) -> set[str]:
    """Tokenise the query and expand through the alias map.

    Tokens shorter than ``_MIN_TOKEN_LEN`` are normally dropped (they
    are too generic to score usefully across a large schema). For an
    exact short-field lookup like ``"z"`` (measurement height), every
    token would be dropped and the search would return nothing — a
    regression vs the previous literal search. So if filtering leaves
    no needles, fall back to the raw non-stopword tokens to preserve
    exact short-field lookups.
    """
    raw = _TOKEN_RE.findall(query.lower())
    needles: set[str] = set()
    for tok in raw:
        if len(tok) < _MIN_TOKEN_LEN or tok in _STOPWORDS:
            continue
        needles.add(tok)
        needles.update(_ALIASES.get(tok, ()))
    if not needles:
        needles = {tok for tok in raw if tok and tok not in _STOPWORDS}
    return needles


def search_schema(query: str = "", version: str = "current") -> dict[str, Any]:
    """**Use this when the user asks "what is the YAML field for X?"**
    Cheaper than `query_knowledge` for field-name lookups, and the
    result reflects the *current* schema rather than the
    knowledge-pack's snapshot — so it never returns a legacy /
    deprecated name (gh#1407).

    Natural-language queries are supported: the query is tokenised and
    expanded through a domain-alias map (e.g. "leaf area index" -> `lai`,
    "green infrastructure" -> `evetr`/`dectr`/`grass`, "frontal area" ->
    `fai`/`frontal`), so the caller need not know the exact field token.
    Schema nodes are scored by how many distinct needles appear in the
    key path, description, or display name, and returned highest-score
    first.
    """
    try:
        envelope = _load_schema(version)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc),
            command="suews schema",
        )

    if envelope.get("status") == "error":
        return envelope

    raw = envelope.get("data") or {}

    if not query:
        # No filter: return the full (cached) envelope. Deep-copy so a caller
        # mutating the result (including nested `data`) cannot corrupt the
        # per-process cache. This is the rare cold/full-dump path.
        return copy.deepcopy(envelope)

    # Build a fresh result envelope; never mutate the cached `envelope`.
    result = dict(envelope)

    needles = _needles(query)
    if not needles:
        result["data"] = {"matches": [], "n_matches": 0, "version": version}
        return result

    # path -> (score, value). Keep the highest score seen per path.
    scored: dict[str, tuple[int, Any]] = {}

    def _record(path: str, value: Any, text: str) -> None:
        score = sum(1 for n in needles if n in text)
        if score:
            prev = scored.get(path)
            if prev is None or score > prev[0]:
                # Store the raw value; `_shallow` runs only on the survivors below.
                scored[path] = (score, value)

    def _walk(node: Any, path: str = "") -> None:
        if isinstance(node, dict):
            for key, value in node.items():
                child_path = f"{path}.{key}" if path else key
                # Build the searchable text for this node: its key, plus
                # human-readable metadata when the value is a field dict.
                text = key.lower()
                if isinstance(value, dict):
                    for attr in ("description", "display_name", "title"):
                        meta = value.get(attr)
                        if isinstance(meta, str):
                            text += " " + meta.lower()
                _record(child_path, value, text)
                _walk(value, child_path)
        elif isinstance(node, list):
            for idx, item in enumerate(node):
                _walk(item, f"{path}[{idx}]")

    _walk(raw)

    ranked = sorted(scored.items(), key=lambda kv: (-kv[1][0], kv[0]))
    matches = [
        {"path": path, "score": score, "value": _shallow(value)}
        for path, (score, value) in ranked[:200]
    ]

    result["data"] = {
        "matches": matches,
        "n_matches": len(scored),
        "version": version,
        "needles": sorted(needles),
    }
    return result


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
