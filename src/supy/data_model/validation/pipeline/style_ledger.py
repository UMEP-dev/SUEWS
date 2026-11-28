"""YAML authoring-style ledger and style-preserving YAML serialiser.

Records how a SUEWS YAML configuration was originally authored -- bare scalar
vs ``RefValue`` mapping vs ref-only placeholder, ``ref`` provenance metadata,
PyYAML flow/block container styles, scalar quoting styles, key order, and
source line numbers -- keyed by canonical dotted/indexed path.

Because the ledger is immutable and carried alongside :class:`ConfigDocument`,
normalisation, deletion/re-insertion, and patch application can preserve the
author's representation instead of silently rewriting bare values into
``RefValue`` wrappers or losing inline flow formatting.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
import io
from types import MappingProxyType
from typing import Any, Literal

import yaml

from ...core._yaml_io import safe_dump_yaml

WrapperStyle = Literal["bare", "ref_value", "ref_only"]

_EMPTY_MAPPING: Mapping[str, Any] = MappingProxyType({})
_ALLOWED_STR_SCALAR_STYLES = frozenset({None, "", "'", '"', "|", ">"})
_YAML_STR_TAG = "tag:yaml.org,2002:str"
_YAML_MERGE_TAG = "tag:yaml.org,2002:merge"


def _freeze_value(value: Any) -> Any:
    """Recursively freeze ledger metadata using the shared ``deep_freeze`` helper."""
    from .document import deep_freeze  # noqa: PLC0415

    return deep_freeze(value)


def _is_ref_only_mapping(entry: Any) -> bool:
    return (
        isinstance(entry, Mapping)
        and "value" not in entry
        and bool(entry)
        and all(key == "ref" for key in entry)
    )


def _is_ref_wrapper_mapping(entry: Any) -> bool:
    return (
        isinstance(entry, Mapping)
        and "value" in entry
        and set(entry) <= {"value", "ref"}
    )


def _join_key_path(prefix: str, key: Any) -> str:
    """Format a mapping key onto *prefix* preserving non-string and punctuated keys."""
    if isinstance(key, str) and key and not any(ch in key for ch in ".[]"):
        return f"{prefix}.{key}" if prefix else key
    seg = f"[{key!r}]"
    return f"{prefix}{seg}" if prefix else seg


def _join_index_path(prefix: str, index: int) -> str:
    return f"{prefix}[{index}]" if prefix else f"[{index}]"


def _extract_leading_comments(raw_text: str) -> tuple[str, ...]:
    """Extract contiguous leading comment and blank lines before the first YAML key."""
    lines: list[str] = []
    for line in raw_text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            lines.append(line)
        else:
            break
    if not any(line.strip().startswith("#") for line in lines):
        return ()
    return tuple(lines)


_NON_SCALAR_KEY = object()


def _typed_key_id(key: Any) -> tuple[type[Any], Any]:
    """Return a type-preserving hashable identity distinguishing ``1``, ``"1"``, and ``True``."""
    return (type(key), key)


def _node_typed_key(node: yaml.Node, loader: yaml.SafeLoader | None = None) -> Any:
    """Recover the typed Python key using PyYAML's SafeLoader scalar constructors."""
    if not isinstance(node, yaml.ScalarNode):
        return _NON_SCALAR_KEY
    if loader is not None:
        return loader.construct_object(node, deep=True)
    helper = yaml.SafeLoader("")
    try:
        return helper.construct_object(node, deep=True)
    finally:
        helper.dispose()


def _construct_node_value(loader: yaml.SafeLoader, node: yaml.Node) -> Any:
    """Safely construct a plain Python value from a PyYAML AST node."""
    return loader.construct_object(node, deep=True)


def _classify_mapping_node(
    node: yaml.MappingNode,
    loader: yaml.SafeLoader | None = None,
) -> tuple[WrapperStyle | None, yaml.Node | None, yaml.Node | None, tuple[Any, ...]]:
    """Classify a PyYAML mapping node as ``ref_value``, ``ref_only``, or plain section."""
    if loader is not None and any(
        isinstance(key_node, yaml.ScalarNode) and key_node.tag == _YAML_MERGE_TAG
        for key_node, _ in node.value
    ):
        loader.flatten_mapping(node)
    keys: list[Any] = []
    value_node: yaml.Node | None = None
    ref_node: yaml.Node | None = None
    for key_node, val_node in node.value:
        typed_key = _node_typed_key(key_node, loader)
        if typed_key is _NON_SCALAR_KEY:
            continue
        keys.append(typed_key)
        if isinstance(typed_key, str):
            if typed_key == "value":
                value_node = val_node
            elif typed_key == "ref":
                ref_node = val_node

    key_ids = {_typed_key_id(k) for k in keys}
    if value_node is not None and key_ids <= {(str, "value"), (str, "ref")}:
        return "ref_value", value_node, ref_node, tuple(keys)
    if value_node is None and keys and key_ids == {(str, "ref")}:
        return "ref_only", None, ref_node, tuple(keys)
    return None, None, None, tuple(keys)


def _infer_dominant_wrapper_style(
    entries: Mapping[str, PathStyle],
) -> Literal["bare", "ref_value"]:
    bare_count = 0
    wrapped_count = 0
    for style in entries.values():
        if style.is_container:
            continue
        if style.wrapper_style == "bare":
            bare_count += 1
        elif style.wrapper_style in {"ref_value", "ref_only"}:
            wrapped_count += 1
    if bare_count > wrapped_count:
        return "bare"
    return "ref_value"


def _walk_mapping_value(
    node: Mapping[str, Any], prefix: str, entries: dict[str, PathStyle]
) -> None:
    """Record styles for a mapping node in :meth:`StyleLedger.from_mapping`."""
    if _is_ref_only_mapping(node):
        if prefix:
            ref_val = node["ref"]
            entries[prefix] = PathStyle(
                wrapper_style="ref_only",
                ref=_freeze_value(ref_val) if isinstance(ref_val, Mapping) else None,
                has_ref=True,
                key_order=tuple(_freeze_value(k) for k in node),
            )
        return
    if _is_ref_wrapper_mapping(node):
        if prefix:
            ref_val = node.get("ref")
            entries[prefix] = PathStyle(
                wrapper_style="ref_value",
                ref=_freeze_value(ref_val) if isinstance(ref_val, Mapping) else None,
                has_ref="ref" in node,
                key_order=tuple(_freeze_value(k) for k in node),
            )
        inner = node["value"]
        inner_prefix = _join_key_path(prefix, "value")
        if (isinstance(inner, Mapping) and not _is_ref_wrapper_mapping(inner)) or (
            isinstance(inner, (list, tuple))
            and any(isinstance(item, (Mapping, list, tuple)) for item in inner)
        ):
            _walk_python_data(inner, inner_prefix, entries)
        return

    keys = tuple(_freeze_value(k) for k in node)
    entries[prefix] = PathStyle(
        wrapper_style="bare" if prefix else None,
        key_order=keys,
        is_container=bool(prefix),
    )
    child_paths = [
        (_join_key_path(prefix, key), val)
        for key, val in node.items()
        if not (isinstance(key, str) and key == "ref")
    ]
    path_counts: dict[str, int] = {}
    for child_path, _val in child_paths:
        path_counts[child_path] = path_counts.get(child_path, 0) + 1
    for child_path, val in child_paths:
        if path_counts[child_path] > 1:
            continue
        if isinstance(val, (Mapping, list, tuple)):
            _walk_python_data(val, child_path, entries)
        else:
            entries[child_path] = PathStyle(wrapper_style="bare")


def _walk_python_data(node: Any, prefix: str, entries: dict[str, PathStyle]) -> None:
    """Recursively populate *entries* from a plain Python mapping/sequence tree."""
    if isinstance(node, Mapping):
        _walk_mapping_value(node, prefix, entries)
        return
    if isinstance(node, (list, tuple)):
        if not any(isinstance(item, (Mapping, list, tuple)) for item in node):
            if prefix and prefix not in entries:
                entries[prefix] = PathStyle(wrapper_style="bare")
            return
        if prefix and prefix not in entries:
            entries[prefix] = PathStyle(wrapper_style="bare", is_container=True)
        for idx, item in enumerate(node):
            child_path = _join_index_path(prefix, idx)
            if isinstance(item, (Mapping, list, tuple)):
                _walk_python_data(item, child_path, entries)
            else:
                entries[child_path] = PathStyle(wrapper_style="bare")


def _parse_yaml_entries(raw_text: str) -> dict[str, PathStyle] | None:
    """Parse *raw_text* into per-path styles, returning ``None`` for non-mapping roots."""
    loader = yaml.SafeLoader(raw_text)
    try:
        root_node = loader.get_single_node()
        if not isinstance(root_node, yaml.MappingNode):
            return None
        entries: dict[str, PathStyle] = {}
        _walk_yaml_node(root_node, "", entries, loader)
        return entries
    finally:
        loader.dispose()


@dataclass(frozen=True, slots=True)
class PathStyle:
    """Recorded authoring style for one canonical YAML path."""

    wrapper_style: WrapperStyle | None = None
    ref: Mapping[str, Any] | None = None
    flow_style: bool | None = None
    scalar_style: str | None = None
    value_flow_style: bool | None = None
    value_scalar_style: str | None = None
    key_order: tuple[Any, ...] = ()
    source_line: int | None = None
    has_ref: bool = False
    is_container: bool = False

    def __post_init__(self) -> None:
        """Freeze ``ref`` metadata and deeply freeze ``key_order``."""
        if self.ref is not None:
            object.__setattr__(self, "ref", _freeze_value(self.ref))
            if not self.has_ref:
                object.__setattr__(self, "has_ref", True)
        if self.key_order:
            object.__setattr__(self, "key_order", _freeze_value(tuple(self.key_order)))
        elif not isinstance(self.key_order, tuple):
            object.__setattr__(self, "key_order", ())


@dataclass(frozen=True, slots=True)
class StyleLedger:
    """Immutable ledger of per-path YAML authoring style and provenance."""

    entries: Mapping[str, PathStyle] = _EMPTY_MAPPING
    leading_comments: tuple[str, ...] = ()
    dominant_wrapper_style: Literal["bare", "ref_value"] = "ref_value"

    def __post_init__(self) -> None:
        """Freeze ``entries`` and normalise ``leading_comments`` to a tuple."""
        if self.entries is not _EMPTY_MAPPING:
            object.__setattr__(self, "entries", MappingProxyType(dict(self.entries)))
        if not isinstance(self.leading_comments, tuple):
            object.__setattr__(self, "leading_comments", tuple(self.leading_comments))

    @classmethod
    def empty(cls) -> StyleLedger:
        """Return an empty :class:`StyleLedger`."""
        return cls()

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> StyleLedger:
        """Capture bare vs ``RefValue`` style, ``ref`` metadata, and key order from *data*."""
        entries: dict[str, PathStyle] = {}
        if isinstance(data, Mapping):
            _walk_python_data(data, "", entries)
        return cls(
            entries=entries,
            dominant_wrapper_style=_infer_dominant_wrapper_style(entries),
        )

    @classmethod
    def from_yaml_text(
        cls,
        raw_text: str,
        *,
        data: Mapping[str, Any] | None = None,
    ) -> StyleLedger:
        """Capture full node-level YAML formatting and wrapper style from *raw_text*.

        Uses :func:`yaml.compose` via ``SafeLoader`` so flow/block layout,
        scalar quoting, key order, leading comments, and 1-based line numbers
        are preserved without adding any external YAML parser dependency.
        """
        leading_comments = _extract_leading_comments(raw_text)
        try:
            entries = _parse_yaml_entries(raw_text)
        except yaml.YAMLError:
            if data is None:
                raise
            entries = None

        if entries is None:
            if data is None:
                return cls(leading_comments=leading_comments)
            fallback = cls.from_mapping(data)
            return cls(
                entries=fallback.entries,
                leading_comments=leading_comments,
                dominant_wrapper_style=fallback.dominant_wrapper_style,
            )

        return cls(
            entries=entries,
            leading_comments=leading_comments,
            dominant_wrapper_style=_infer_dominant_wrapper_style(entries),
        )

    def __len__(self) -> int:
        """Return the number of recorded entries in the ledger."""
        return len(self.entries)

    def __contains__(self, path: object) -> bool:
        """Return True when *path* has a recorded style entry."""
        return path in self.entries

    def get(self, path: str) -> PathStyle | None:
        """Return the recorded :class:`PathStyle` for *path*, or ``None``."""
        return self.entries.get(path)

    def wrapper_style(self, path: str) -> WrapperStyle | None:
        """Return ``"bare"``, ``"ref_value"``, ``"ref_only"``, or ``None`` for *path*."""
        entry = self.entries.get(path)
        return entry.wrapper_style if entry is not None else None

    def ref_metadata(self, path: str) -> Mapping[str, Any] | None:
        """Return the frozen ``ref`` mapping recorded for *path*, or ``None``."""
        entry = self.entries.get(path)
        return entry.ref if entry is not None else None

    def has_ref_key(self, path: str) -> bool:
        """Return True when *path* explicitly carried a ``"ref"`` key (even if null)."""
        entry = self.entries.get(path)
        return entry.has_ref if entry is not None else False

    def source_line(self, path: str) -> int | None:
        """Return the 1-based YAML source line recorded for *path*, or ``None``."""
        entry = self.entries.get(path)
        return entry.source_line if entry is not None else None

    def paths(self) -> tuple[str, ...]:
        """Return all recorded non-root paths in deterministic insertion order."""
        return tuple(p for p in self.entries if p)

    def with_entry(self, path: str, style: PathStyle) -> StyleLedger:
        """Return a new :class:`StyleLedger` with *path* set to *style*."""
        updated = dict(self.entries)
        updated[path] = style
        return StyleLedger(
            entries=updated,
            leading_comments=self.leading_comments,
            dominant_wrapper_style=_infer_dominant_wrapper_style(updated),
        )

    def remap_paths(
        self,
        renames: Mapping[str, str],
        *,
        parent_key_renames: Mapping[str, Mapping[tuple[type[Any], Any], Any]]
        | None = None,
    ) -> StyleLedger:
        """Return a new :class:`StyleLedger` copying styles across renamed path prefixes."""
        if not renames and not parent_key_renames:
            return self
        updated = dict(self.entries)
        for old_prefix, new_prefix in renames.items():
            if not old_prefix or not new_prefix or old_prefix == new_prefix:
                continue
            for path, style in self.entries.items():
                if path == old_prefix:
                    updated.setdefault(new_prefix, style)
                elif path.startswith(f"{old_prefix}.") or path.startswith(
                    f"{old_prefix}["
                ):
                    remapped = new_prefix + path[len(old_prefix) :]
                    updated.setdefault(remapped, style)
        if parent_key_renames:
            for parent_path, key_map in parent_key_renames.items():
                style = updated.get(parent_path)
                if style is None or not style.key_order:
                    continue
                new_order = tuple(
                    key_map.get(_typed_key_id(k), k) for k in style.key_order
                )
                if tuple(_typed_key_id(k) for k in new_order) != tuple(
                    _typed_key_id(k) for k in style.key_order
                ):
                    updated[parent_path] = PathStyle(
                        wrapper_style=style.wrapper_style,
                        ref=style.ref,
                        flow_style=style.flow_style,
                        scalar_style=style.scalar_style,
                        value_flow_style=style.value_flow_style,
                        value_scalar_style=style.value_scalar_style,
                        key_order=new_order,
                        source_line=style.source_line,
                        has_ref=style.has_ref,
                        is_container=style.is_container,
                    )
        return StyleLedger(
            entries=updated,
            leading_comments=self.leading_comments,
            dominant_wrapper_style=_infer_dominant_wrapper_style(updated),
        )

    def remap_leaf_keys(self, key_renames: Mapping[str, str]) -> StyleLedger:
        """Return a new :class:`StyleLedger` carrying styles across key-name renames.

        Preserves original entries while also indexing renamed canonical paths
        and updating container ``key_order`` tuples when an old key is renamed
        to its canonical spelling.
        """
        if not key_renames:
            return self
        updated: dict[str, PathStyle] = {}
        for path, style in self.entries.items():
            new_order = tuple(
                key_renames.get(k, k) if isinstance(k, str) else k
                for k in style.key_order
            )
            order_changed = tuple(_typed_key_id(k) for k in new_order) != tuple(
                _typed_key_id(k) for k in style.key_order
            )
            remapped_style = (
                PathStyle(
                    wrapper_style=style.wrapper_style,
                    ref=style.ref,
                    flow_style=style.flow_style,
                    scalar_style=style.scalar_style,
                    value_flow_style=style.value_flow_style,
                    value_scalar_style=style.value_scalar_style,
                    key_order=new_order,
                    source_line=style.source_line,
                    has_ref=style.has_ref,
                    is_container=style.is_container,
                )
                if order_changed
                else style
            )
            updated[path] = remapped_style
            remapped_path = _remap_path_segments(path, key_renames)
            if remapped_path is not None:
                updated.setdefault(remapped_path, remapped_style)

        return StyleLedger(
            entries=updated,
            leading_comments=self.leading_comments,
            dominant_wrapper_style=_infer_dominant_wrapper_style(updated),
        )


def _remap_path_segments(path: str, key_renames: Mapping[str, str]) -> str | None:
    if not path:
        return None
    parts = path.split(".")
    changed = False
    new_parts: list[str] = []
    for part in parts:
        if "[" in part:
            head, bracket_rest = part.split("[", 1)
            if head in key_renames:
                new_parts.append(f"{key_renames[head]}[{bracket_rest}")
                changed = True
            else:
                new_parts.append(part)
        elif part in key_renames:
            new_parts.append(key_renames[part])
            changed = True
        else:
            new_parts.append(part)
    return ".".join(new_parts) if changed else None


def _has_nested_yaml_children(seq_node: yaml.SequenceNode) -> bool:
    return any(
        isinstance(child, (yaml.MappingNode, yaml.SequenceNode))
        for child in seq_node.value
    )


def _walk_yaml_ref_value(
    node: yaml.MappingNode,
    val_node: yaml.Node | None,
    ref_node: yaml.Node | None,
    key_order: tuple[Any, ...],
    prefix: str,
    line: int | None,
    entries: dict[str, PathStyle],
    val_loader: yaml.SafeLoader,
) -> None:
    ref_val = (
        _construct_node_value(val_loader, ref_node) if ref_node is not None else None
    )
    val_flow = (
        val_node.flow_style
        if isinstance(val_node, (yaml.MappingNode, yaml.SequenceNode))
        else None
    )
    val_scalar = (
        val_node.style
        if isinstance(val_node, yaml.ScalarNode) and val_node.tag == _YAML_STR_TAG
        else None
    )
    if prefix:
        entries[prefix] = PathStyle(
            wrapper_style="ref_value",
            ref=_freeze_value(ref_val) if isinstance(ref_val, Mapping) else None,
            flow_style=node.flow_style,
            value_flow_style=val_flow,
            value_scalar_style=val_scalar,
            key_order=key_order,
            source_line=line,
            has_ref=ref_node is not None,
        )
    inner_prefix = _join_key_path(prefix, "value")
    if isinstance(val_node, yaml.MappingNode):
        sub_kind, _, _, _ = _classify_mapping_node(val_node, val_loader)
        if sub_kind is None:
            _walk_yaml_node(val_node, inner_prefix, entries, val_loader)
    elif isinstance(val_node, yaml.SequenceNode):
        if _has_nested_yaml_children(val_node):
            _walk_yaml_node(val_node, inner_prefix, entries, val_loader)
        else:
            _record_scalar_sequence_elements(val_node, inner_prefix, entries)


def _record_scalar_sequence_elements(
    seq_node: yaml.SequenceNode,
    prefix: str,
    entries: dict[str, PathStyle],
) -> None:
    """Record per-element scalar quoting styles without altering container wrapper counts."""
    for idx, child_node in enumerate(seq_node.value):
        if not isinstance(child_node, yaml.ScalarNode):
            continue
        scalar_style = child_node.style if child_node.tag == _YAML_STR_TAG else None
        if scalar_style is None:
            continue
        child_line = (
            child_node.start_mark.line + 1
            if child_node.start_mark is not None
            else None
        )
        entries[_join_index_path(prefix, idx)] = PathStyle(
            scalar_style=scalar_style,
            source_line=child_line,
        )


def _walk_yaml_mapping(
    node: yaml.MappingNode,
    prefix: str,
    line: int | None,
    entries: dict[str, PathStyle],
    val_loader: yaml.SafeLoader,
) -> None:
    wrapper_kind, val_node, ref_node, key_order = _classify_mapping_node(
        node, val_loader
    )
    if wrapper_kind == "ref_only":
        ref_val = (
            _construct_node_value(val_loader, ref_node)
            if ref_node is not None
            else None
        )
        if prefix:
            entries[prefix] = PathStyle(
                wrapper_style="ref_only",
                ref=_freeze_value(ref_val) if isinstance(ref_val, Mapping) else None,
                flow_style=node.flow_style,
                key_order=key_order,
                source_line=line,
                has_ref=True,
            )
        return

    if wrapper_kind == "ref_value":
        _walk_yaml_ref_value(
            node, val_node, ref_node, key_order, prefix, line, entries, val_loader
        )
        return

    entries[prefix] = PathStyle(
        wrapper_style="bare" if prefix else None,
        flow_style=node.flow_style,
        key_order=key_order,
        source_line=line,
        is_container=bool(prefix),
    )
    yaml_children: list[tuple[str, yaml.Node]] = []
    for key_node, child_node in node.value:
        typed_key = _node_typed_key(key_node, val_loader)
        if typed_key is _NON_SCALAR_KEY or (
            isinstance(typed_key, str) and typed_key == "ref"
        ):
            continue
        yaml_children.append((_join_key_path(prefix, typed_key), child_node))
    yaml_counts: dict[str, int] = {}
    for child_path, _child_node in yaml_children:
        yaml_counts[child_path] = yaml_counts.get(child_path, 0) + 1
    for child_path, child_node in yaml_children:
        if yaml_counts[child_path] > 1:
            continue
        _walk_yaml_node(child_node, child_path, entries, val_loader)


def _walk_yaml_node(
    node: yaml.Node,
    prefix: str,
    entries: dict[str, PathStyle],
    val_loader: yaml.SafeLoader,
) -> None:
    """Populate *entries* by walking a PyYAML composed AST."""
    line = node.start_mark.line + 1 if node.start_mark is not None else None
    if isinstance(node, yaml.MappingNode):
        _walk_yaml_mapping(node, prefix, line, entries, val_loader)
        return
    if isinstance(node, yaml.SequenceNode):
        if not _has_nested_yaml_children(node):
            if prefix and prefix not in entries:
                entries[prefix] = PathStyle(
                    wrapper_style="bare",
                    flow_style=node.flow_style,
                    source_line=line,
                )
            _record_scalar_sequence_elements(node, prefix, entries)
            return
        if prefix and prefix not in entries:
            entries[prefix] = PathStyle(
                wrapper_style="bare",
                flow_style=node.flow_style,
                source_line=line,
                is_container=True,
            )
        for idx, child_node in enumerate(node.value):
            _walk_yaml_node(
                child_node, _join_index_path(prefix, idx), entries, val_loader
            )
        return
    if isinstance(node, yaml.ScalarNode) and prefix:
        scalar_style = node.style if node.tag == _YAML_STR_TAG else None
        entries[prefix] = PathStyle(
            wrapper_style="bare",
            scalar_style=scalar_style,
            source_line=line,
        )


def _safe_scalar_style(node: yaml.ScalarNode, recorded: str | None) -> str | None:
    """Return *recorded* only when explicit, safe, and round-trip preserving for *node*."""
    if not recorded or recorded not in _ALLOWED_STR_SCALAR_STYLES:
        return node.style
    if node.tag != _YAML_STR_TAG:
        return node.style
    if recorded in {"|", ">"} and any(
        (ord(ch) < 0x20 and ch not in {"\n", "\t"}) or (0x7F <= ord(ch) <= 0x9F)
        for ch in node.value
    ):
        return '"'
    return recorded


def _reorder_mapping_node(
    node: yaml.MappingNode,
    key_order: tuple[Any, ...],
    loader: yaml.SafeLoader | None = None,
) -> None:
    """Reorder a PyYAML MappingNode's pairs to match *key_order* where keys exist."""
    if not key_order or len(node.value) <= 1:
        return
    rank: dict[tuple[type[Any], Any], int] = {}
    for idx, key in enumerate(key_order):
        try:
            rank.setdefault(_typed_key_id(key), idx)
        except TypeError:
            continue
    fallback_rank = len(key_order)
    indexed = list(enumerate(node.value))

    def _sort_key(item: tuple[int, tuple[yaml.Node, yaml.Node]]) -> tuple[int, int]:
        orig_idx, (key_node, _val_node) = item
        typed_key = _node_typed_key(key_node, loader)
        if typed_key is _NON_SCALAR_KEY:
            return (fallback_rank, orig_idx)
        try:
            key_rank = rank.get(_typed_key_id(typed_key), fallback_rank)
        except TypeError:
            key_rank = fallback_rank
        return (key_rank, orig_idx)

    indexed.sort(key=_sort_key)
    node.value = [pair for _, pair in indexed]


def _apply_style_to_mapping_node(
    node: yaml.MappingNode,
    prefix: str,
    style: PathStyle | None,
    ledger: StyleLedger,
    loader: yaml.SafeLoader | None = None,
) -> None:
    wrapper_kind, val_node, _ref_node, _keys = _classify_mapping_node(node, loader)
    if style is not None:
        if style.flow_style is not None:
            node.flow_style = style.flow_style
        if style.key_order:
            _reorder_mapping_node(node, style.key_order, loader)
    if wrapper_kind == "ref_only":
        return
    if wrapper_kind == "ref_value":
        if val_node is not None and style is not None:
            if (
                isinstance(val_node, (yaml.MappingNode, yaml.SequenceNode))
                and style.value_flow_style is not None
            ):
                val_node.flow_style = style.value_flow_style
            elif isinstance(val_node, yaml.ScalarNode):
                val_node.style = _safe_scalar_style(val_node, style.value_scalar_style)
        inner_prefix = _join_key_path(prefix, "value")
        if isinstance(val_node, yaml.MappingNode):
            sub_kind, _, _, _ = _classify_mapping_node(val_node, loader)
            if sub_kind is None:
                _apply_style_to_yaml_node(val_node, inner_prefix, ledger, loader)
        elif isinstance(val_node, yaml.SequenceNode):
            for idx, child_node in enumerate(val_node.value):
                _apply_style_to_yaml_node(
                    child_node, _join_index_path(inner_prefix, idx), ledger, loader
                )
        return

    for key_node, child_node in node.value:
        typed_key = _node_typed_key(key_node, loader)
        if typed_key is _NON_SCALAR_KEY or (
            isinstance(typed_key, str) and typed_key == "ref"
        ):
            continue
        _apply_style_to_yaml_node(
            child_node, _join_key_path(prefix, typed_key), ledger, loader
        )


def _apply_style_to_yaml_node(
    node: yaml.Node,
    prefix: str,
    ledger: StyleLedger,
    loader: yaml.SafeLoader | None = None,
) -> None:
    """Apply recorded flow, scalar, and key-order styles to a PyYAML node tree."""
    style = ledger.get(prefix)
    if isinstance(node, yaml.MappingNode):
        _apply_style_to_mapping_node(node, prefix, style, ledger, loader)
        return
    if isinstance(node, yaml.SequenceNode):
        if style is not None and style.flow_style is not None:
            node.flow_style = style.flow_style
        for idx, child_node in enumerate(node.value):
            _apply_style_to_yaml_node(
                child_node, _join_index_path(prefix, idx), ledger, loader
            )
        return
    if isinstance(node, yaml.ScalarNode) and style is not None:
        node.style = _safe_scalar_style(node, style.scalar_style)


def dump_yaml_with_style(
    data: Any,
    ledger: StyleLedger | None,
    *,
    header: str | None = None,
    preserve_leading_comments: bool = False,
) -> str:
    """Serialise *data* to YAML while preserving styles recorded in *ledger*.

    Falls back to :func:`safe_dump_yaml` when *ledger* is ``None`` or empty.
    """
    stream = io.StringIO()
    if header:
        stream.write(header)
    elif preserve_leading_comments and ledger is not None and ledger.leading_comments:
        stream.write("\n".join(ledger.leading_comments) + "\n")

    if ledger is None or len(ledger) == 0:
        safe_dump_yaml(
            data,
            stream,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
        )
        return stream.getvalue()

    representer = yaml.SafeDumper(
        io.StringIO(),
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
    )
    root_node = representer.represent_data(data)
    key_loader = yaml.SafeLoader("")
    try:
        _apply_style_to_yaml_node(root_node, "", ledger, key_loader)
    finally:
        key_loader.dispose()

    yaml.serialize(
        root_node,
        stream,
        Dumper=yaml.SafeDumper,
        allow_unicode=True,
    )
    return stream.getvalue()
