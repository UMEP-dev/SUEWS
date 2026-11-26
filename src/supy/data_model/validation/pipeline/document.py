"""Unified YAML document model, RefValue helpers, and patch primitives.

Every validation phase (A, B, C) reads and modifies the same SUEWS YAML
configuration shape, where leaves may be authored as bare scalars/lists
(``sfr: 0.4``), wrapped ``RefValue`` mappings (``sfr: {value: 0.4, ref: ...}``),
or value-less ref-only placeholders (``ie_start: {ref: ...}``), and where
``ValidationContext`` freezes mappings into ``MappingProxyType`` and lists into
tuples.

Centralising value access, path resolution, site GRIDID lookup, and
style-preserving patch application here ensures no phase or rule duplicates
fragile ``isinstance(..., dict)`` / ``.get("value")`` handling.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from copy import deepcopy
from dataclasses import dataclass
import io
from pathlib import Path
import re
from typing import Any, Literal

from ...core._yaml_io import safe_dump_yaml, safe_load_yaml
from ...core.field_renames import normalise_yaml_renames
from ...core.physics_families import flatten_physics_in_config
from ...core.profile import normalise_hourly_profile_keys_in_raw_config

_MISSING = object()
_PATH_TOKEN_RE = re.compile(r"([^\.\[\]]+)|\[(\d+)\]")
_VALID_PATH_RE = re.compile(
    r"^(?:[^\.\[\]]+(?:\[\d+\])*)(?:\.[^\.\[\]]+(?:\[\d+\])*)*$"
)

PatchAction = Literal["set", "delete", "nullify_tree"]
_VALID_PATCH_ACTIONS = frozenset({"set", "delete", "nullify_tree"})


def thaw_copy(value: Any) -> Any:
    """Recursively copy *value*, thawing frozen mappings and tuple sequences."""
    if isinstance(value, Mapping):
        return {k: thaw_copy(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [thaw_copy(item) for item in value]
    if isinstance(value, set):
        return {thaw_copy(item) for item in value}
    if isinstance(value, frozenset):
        return frozenset(thaw_copy(item) for item in value)
    return deepcopy(value)


def is_ref_only(entry: Any) -> bool:
    """Return True for a ref-only ``RefValue`` placeholder such as ``{"ref": {...}}``.

    Such an entry carries no value (the model and Rust bridge treat it as
    null), so readers return their default and nullifiers leave it untouched
    rather than descending into ``ref`` metadata.
    """
    return (
        isinstance(entry, Mapping)
        and "value" not in entry
        and bool(entry)
        and all(key == "ref" for key in entry)
    )


def is_ref_wrapper(entry: Any) -> bool:
    """Return True for a ``RefValue`` mapping carrying ``"value"`` (and optional ``"ref"``)."""
    return (
        isinstance(entry, Mapping)
        and "value" in entry
        and set(entry) <= {"value", "ref"}
    )


def unwrap_entry(entry: Any, default: Any = None) -> Any:
    """Extract the inner value from a ``RefValue`` mapping or return a bare value.

    Returns ``default`` when *entry* is an empty mapping or a ref-only mapping
    without ``"value"``. Non-``RefValue`` mappings (such as sub-model sections)
    are returned as-is.
    """
    if isinstance(entry, Mapping):
        if is_ref_wrapper(entry):
            return entry["value"]
        if not entry or is_ref_only(entry):
            return default
        return entry
    return entry


def read_scalar(container: Any, key: str, default: Any = None) -> Any:
    """Read ``container[key]`` whether stored as ``{"value": x, ...}`` or bare ``x``.

    Returns ``default`` when ``container`` is not a ``Mapping``, ``key`` is
    absent, or the entry is a mapping without a ``"value"`` key (such as ``{}``
    or a ref-only dict).
    """
    if not isinstance(container, Mapping) or key not in container:
        return default
    entry = container[key]
    if isinstance(entry, Mapping):
        if is_ref_wrapper(entry):
            return entry["value"]
        return default
    return entry


def _update_existing_entry(existing: Any, new_value: Any) -> Any:
    """Update an existing bare, ``RefValue``, or ref-only entry preserving metadata."""
    copied = thaw_copy(new_value)
    if isinstance(existing, dict) and (is_ref_wrapper(existing) or is_ref_only(existing)):
        existing["value"] = copied
        return existing
    if isinstance(existing, Mapping) and (is_ref_wrapper(existing) or is_ref_only(existing)):
        return {**thaw_copy(existing), "value": copied}
    if isinstance(existing, dict) and "value" in existing:
        existing["value"] = copied
        return existing
    if isinstance(existing, Mapping) and "value" in existing:
        return {**thaw_copy(existing), "value": copied}
    return copied


def write_scalar(
    container: dict[str, Any],
    key: str,
    new_value: Any,
    *,
    default_wrapped: bool = True,
) -> None:
    """Update ``container[key]`` while preserving ``RefValue`` metadata or bare style.

    - Existing ``RefValue`` or ref-only ``dict`` entries (e.g.
      ``{"value": old, "ref": ...}`` or ``{"ref": ...}``) update ``"value"``
      in place so ``"ref"`` metadata is preserved.
    - Existing read-only ``Mapping`` entries are replaced with a thawed dict
      copy carrying the updated ``"value"``.
    - Existing non-mapping entries (bare scalars, lists, or ``None``) stay bare.
    - Absent keys are inserted as ``{"value": new_value}`` when
      ``default_wrapped=True``, or bare ``new_value`` otherwise.
    """
    if key in container:
        existing = container[key]
        updated = _update_existing_entry(existing, new_value)
        if updated is not existing:
            container[key] = updated
    elif default_wrapped:
        container[key] = {"value": thaw_copy(new_value)}
    else:
        container[key] = thaw_copy(new_value)


def site_gridid(site: Any, fallback: Any = None) -> Any:
    """Extract ``gridiv`` from a site mapping, handling bare and ``RefValue`` forms."""
    if not isinstance(site, Mapping):
        return fallback
    val = read_scalar(site, "gridiv", _MISSING)
    if val is _MISSING or val is None:
        return fallback
    return val


def site_gridid_map(data: Any) -> dict[int, Any]:
    """Build a mapping from site list index to site GRIDID (or index fallback)."""
    mapping: dict[int, Any] = {}
    if not isinstance(data, Mapping):
        return mapping
    sites = data.get("sites")
    if not isinstance(sites, (list, tuple)):
        return mapping
    for idx, site in enumerate(sites):
        mapping[idx] = site_gridid(site, fallback=idx)
    return mapping


def format_pydantic_loc(
    loc_tuple: Sequence[Any],
    gridid_map: Mapping[int, Any] | None = None,
) -> str:
    """Convert a Pydantic error location tuple to a GRIDID-labelled dotted path.

    Example: ``("sites", 0, "properties", "lat")`` with ``{0: 123}`` becomes
    ``"sites.123.properties.lat"``.
    """
    if not loc_tuple:
        return ""
    gridids = gridid_map or {}
    parts: list[str] = []
    idx = 0
    while idx < len(loc_tuple):
        part = loc_tuple[idx]
        if (
            part == "sites"
            and idx + 1 < len(loc_tuple)
            and isinstance(loc_tuple[idx + 1], int)
        ):
            site_idx = loc_tuple[idx + 1]
            parts.append(f"sites.{gridids.get(site_idx, site_idx)}")
            idx += 2
            continue
        parts.append(str(part))
        idx += 1
    return ".".join(parts)


def parse_path_tokens(path: str) -> list[str | int]:
    """Parse a dotted/indexed YAML path into mapping keys and list indices.

    Example: ``"sites[0].properties.land_cover.paved.sfr"`` becomes
    ``["sites", 0, "properties", "land_cover", "paved", "sfr"]``.
    """
    if not isinstance(path, str) or not _VALID_PATH_RE.fullmatch(path):
        raise ValueError(f"Malformed YAML path: {path!r}")
    tokens: list[str | int] = []
    for match in _PATH_TOKEN_RE.finditer(path):
        key, index = match.groups()
        if key is not None:
            tokens.append(key)
        else:
            tokens.append(int(index))
    return tokens


def _step_read(current: Any, token: str | int) -> Any:
    if isinstance(token, int):
        if isinstance(current, (list, tuple)) and 0 <= token < len(current):
            return current[token]
        return _MISSING
    if isinstance(current, Mapping) and token in current:
        return current[token]
    return _MISSING


def read_path(data: Any, path: str, default: Any = None) -> Any:
    """Resolve a dotted/indexed *path* inside *data*, unwrapping a leaf ``RefValue``."""
    try:
        tokens = parse_path_tokens(path)
    except ValueError:
        return default
    current: Any = data
    for token in tokens:
        current = _step_read(current, token)
        if current is _MISSING:
            return default
    return unwrap_entry(current, default)


def _can_resolve_parent(
    data: dict[str, Any], tokens: Sequence[str | int], *, create_missing: bool
) -> bool:
    probe: Any = data
    creating = False
    for pos, token in enumerate(tokens[:-1]):
        next_is_int = isinstance(tokens[pos + 1], int)
        if creating:
            if isinstance(token, int) or next_is_int:
                return False
            continue
        if isinstance(token, int):
            if not isinstance(probe, list) or not (0 <= token < len(probe)):
                return False
            probe = probe[token]
            continue
        if not isinstance(probe, dict):
            return False
        if token not in probe or probe[token] is None:
            if not create_missing or next_is_int:
                return False
            creating = True
        else:
            probe = probe[token]
    return not (creating and isinstance(tokens[-1], int))


def _ensure_parent_container(
    data: dict[str, Any], tokens: Sequence[str | int], *, create_missing: bool
) -> tuple[Any, str | int] | None:
    """Walk *data* to the parent container of *tokens[-1]* without partial mutations."""
    if not tokens or not _can_resolve_parent(
        data, tokens, create_missing=create_missing
    ):
        return None
    current: Any = data
    for token in tokens[:-1]:
        if isinstance(token, int):
            current = current[token]
        else:
            if token not in current or current[token] is None:
                current[token] = {}
            current = current[token]
    return current, tokens[-1]


def write_path(
    data: dict[str, Any],
    path: str,
    new_value: Any,
    *,
    default_wrapped: bool = True,
    create_missing: bool = True,
) -> bool:
    """Write *new_value* at *path* in *data*, preserving bare vs ``RefValue`` style."""
    tokens = parse_path_tokens(path)
    located = _ensure_parent_container(data, tokens, create_missing=create_missing)
    if located is None:
        return False
    parent, leaf = located
    if isinstance(leaf, int):
        if not isinstance(parent, list) or not (0 <= leaf < len(parent)):
            return False
        existing = parent[leaf]
        updated = _update_existing_entry(existing, new_value)
        if updated is not existing:
            parent[leaf] = updated
        return True
    if not isinstance(parent, dict):
        return False
    write_scalar(parent, leaf, new_value, default_wrapped=default_wrapped)
    return True


def delete_path(data: dict[str, Any], path: str) -> bool:
    """Delete the leaf key at *path* in *data* if present."""
    tokens = parse_path_tokens(path)
    located = _ensure_parent_container(data, tokens, create_missing=False)
    if located is None:
        return False
    parent, leaf = located
    if isinstance(leaf, str) and isinstance(parent, dict) and leaf in parent:
        del parent[leaf]
        return True
    return False


def nullify_tree_in_place(
    node: Any,
    *,
    skip_keys: frozenset[str] = frozenset(),
    skip_at_root_only: bool = True,
    _at_root: bool = True,
) -> list[str]:
    """Nullify scalar and list leaves in *node* in place, preserving ref metadata.

    Returns the relative paths of leaves that changed from non-null to null in
    deterministic input traversal order. Ref-only placeholders (``{"ref": ...}``)
    and sibling ``"ref"`` metadata blocks are left untouched.
    """
    changed: list[str] = []

    def _nullify_wrapper(cur: dict[str, Any], prefix: str) -> None:
        inner = cur["value"]
        if isinstance(inner, list):
            if any(isinstance(item, (dict, list)) for item in inner):
                _walk(inner, prefix, False)
            elif any(item is not None for item in inner):
                cur["value"] = [None] * len(inner)
                changed.append(prefix)
        elif isinstance(inner, dict):
            _walk(inner, prefix, False)
        elif inner is not None:
            cur["value"] = None
            changed.append(prefix)

    def _walk(cur: Any, prefix: str, is_root: bool) -> None:
        if isinstance(cur, dict):
            if is_ref_only(cur):
                return
            if is_ref_wrapper(cur):
                _nullify_wrapper(cur, prefix)
                return
            for key, val in cur.items():
                if key == "ref" or (key in skip_keys and (is_root or not skip_at_root_only)):
                    continue
                child_path = f"{prefix}.{key}" if prefix else str(key)
                if isinstance(val, (dict, list)):
                    _walk(val, child_path, False)
                elif val is not None:
                    cur[key] = None
                    changed.append(child_path)
        elif isinstance(cur, list):
            for idx, item in enumerate(cur):
                child_path = f"{prefix}[{idx}]" if prefix else f"[{idx}]"
                if isinstance(item, (dict, list)):
                    _walk(item, child_path, False)
                elif item is not None:
                    cur[idx] = None
                    changed.append(child_path)

    _walk(node, "", _at_root)
    return changed


def apply_nullify_tree_at_path(
    target: dict[str, Any],
    path: str,
    *,
    skip_keys: frozenset[str] = frozenset(),
) -> bool:
    """Apply ``nullify_tree_in_place`` at *path* (supporting dict keys and list indices)."""
    tokens = parse_path_tokens(path)
    located = _ensure_parent_container(target, tokens, create_missing=False)
    if located is None:
        return False
    parent, leaf = located
    if isinstance(leaf, str):
        if not isinstance(parent, dict) or leaf not in parent:
            return False
        node = parent[leaf]
        if isinstance(node, (dict, list)):
            nullify_tree_in_place(node, skip_keys=skip_keys)
        elif node is not None:
            parent[leaf] = None
        return True
    if not isinstance(parent, list) or not (0 <= leaf < len(parent)):
        return False
    node = parent[leaf]
    if isinstance(node, (dict, list)):
        nullify_tree_in_place(node, skip_keys=skip_keys)
    elif node is not None:
        parent[leaf] = None
    return True


@dataclass(frozen=True)
class ConfigPatch:
    """Declarative mutation planned against a :class:`ConfigDocument`.

    Pure planners (Phase B scientific suggestions, Phase A normalisations)
    return ``list[ConfigPatch]`` so suggest mode never needs to deep-copy and
    discard mutated trees, and apply mode applies the exact same patch list.
    """

    path: str
    action: PatchAction
    new_value: Any = None
    old_value: Any = None
    reason: str = ""
    parameter: str | None = None
    site_index: int | None = None
    site_gridid: int | None = None
    source: str = ""
    skip_keys: tuple[str, ...] = ()
    default_wrapped: bool = True


class ConfigDocument:
    """Canonical in-memory representation of a SUEWS YAML configuration.

    Holds the parsed mapping alongside optional original text and source path,
    providing style-preserving reads, GRIDID maps, immutable patch application,
    and deterministic YAML serialisation.
    """

    def __init__(
        self,
        data: dict[str, Any],
        *,
        source_path: str | Path | None = None,
        raw_text: str | None = None,
    ) -> None:
        self.data: dict[str, Any] = data
        self.source_path: Path | None = (
            Path(source_path) if source_path is not None else None
        )
        self.raw_text: str | None = raw_text

    @classmethod
    def from_file(
        cls,
        path: str | Path,
        *,
        normalise: bool = True,
        flatten_physics: bool = False,
    ) -> ConfigDocument:
        """Load a YAML file into a :class:`ConfigDocument`."""
        file_path = Path(path)
        raw_text = file_path.read_text(encoding="utf-8")
        loaded = safe_load_yaml(raw_text)
        if not isinstance(loaded, dict):
            raise ValueError("YAML root is not a mapping")
        return cls.from_dict(
            loaded,
            source_path=file_path,
            raw_text=raw_text,
            normalise=normalise,
            flatten_physics=flatten_physics,
        )

    @classmethod
    def from_dict(
        cls,
        data: Mapping[str, Any],
        *,
        source_path: str | Path | None = None,
        raw_text: str | None = None,
        normalise: bool = True,
        flatten_physics: bool = False,
    ) -> ConfigDocument:
        """Construct a :class:`ConfigDocument` from an in-memory mapping."""
        working = thaw_copy(data)
        if normalise:
            working = normalise_yaml_renames(working)
            working, _ = normalise_hourly_profile_keys_in_raw_config(working)
        if flatten_physics:
            flatten_physics_in_config(working)
        return cls(working, source_path=source_path, raw_text=raw_text)

    def clone(self) -> ConfigDocument:
        """Return an independent deep copy of this document."""
        return ConfigDocument(
            thaw_copy(self.data),
            source_path=self.source_path,
            raw_text=self.raw_text,
        )

    def gridid_map(self) -> dict[int, Any]:
        """Return ``{site_index: gridiv}`` for every site in the document."""
        return site_gridid_map(self.data)

    def read(self, path: str, default: Any = None) -> Any:
        """Read the scalar or subtree at *path*, unwrapping leaf ``RefValue``."""
        return read_path(self.data, path, default=default)

    def apply_patches(self, patches: Sequence[ConfigPatch]) -> ConfigDocument:
        """Return a new :class:`ConfigDocument` with *patches* applied in order."""
        target = self.clone()
        for patch in patches:
            if patch.action not in _VALID_PATCH_ACTIONS:
                raise ValueError(
                    f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
                )
            if patch.action == "set":
                applied = write_path(
                    target.data,
                    patch.path,
                    patch.new_value,
                    default_wrapped=patch.default_wrapped,
                )
                if not applied:
                    raise ValueError(
                        f"Failed to apply 'set' patch at path {patch.path!r}"
                    )
            elif patch.action == "delete":
                tokens = parse_path_tokens(patch.path)
                located = _ensure_parent_container(
                    target.data, tokens, create_missing=False
                )
                if located is None:
                    raise ValueError(
                        f"Failed to apply 'delete' patch at path {patch.path!r}: "
                        "parent container not found"
                    )
                delete_path(target.data, patch.path)
            elif patch.action == "nullify_tree":
                applied = apply_nullify_tree_at_path(
                    target.data,
                    patch.path,
                    skip_keys=frozenset(patch.skip_keys),
                )
                if not applied:
                    raise ValueError(
                        f"Failed to apply 'nullify_tree' patch at path {patch.path!r}"
                    )
        target.raw_text = None
        return target

    def dump_yaml(self, *, header: str | None = None) -> str:
        """Serialise the document mapping to YAML text with an optional header."""
        stream = io.StringIO()
        if header:
            stream.write(header)
        safe_dump_yaml(
            self.data,
            stream,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
        )
        return stream.getvalue()
