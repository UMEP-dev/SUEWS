"""Patch normalisation carrying original values and exact patch inversion.

Normalises a sequence of declarative :class:`ConfigPatch` records against a
target :class:`ConfigDocument` or raw mapping without mutating the target:

1. Validates patch actions and canonicalises YAML paths (preserving exact
   mapping-key identity for non-string keys and keys containing punctuation).
2. Expands coarse ``nullify_tree`` patches into deterministic leaf ``set``
   patches (or populates original-value snapshots when expansion is disabled).
3. Captures the true pre-patch ``old_value``, ``had_original`` existence flag,
   ``original_style`` (bare vs ``RefValue`` vs ref-only), frozen
   ``original_ref`` metadata, exact ``original_raw`` entry, and any implicitly
   created ancestor for every mutation.
4. Coalesces sequential writes on identical paths across non-interfering
   barriers (never reordering across ancestor/descendant mutations and never
   changing the authored/inserted wrapper representation) and drops type-safe
   net no-ops.
5. Supports lossless inversion via :func:`invert_patches` so applied patches
   can be undone cleanly — restoring bare vs ``RefValue`` representations,
   explicit ``ref: None`` entries, and removing implicitly created ancestors.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from decimal import Decimal
from fractions import Fraction
from typing import Any

import numpy as np

from .document import (
    _MISSING,
    _UNSET,
    _VALID_PATCH_ACTIONS,
    ConfigDocument,
    ConfigPatch,
    MappingKey,
    PathToken,
    _build_new_entry,
    _copy_payload,
    _ensure_parent_container,
    _is_list_index_token,
    _mapping_key_from_token,
    _prepare_working_dict,
    apply_nullify_tree_at_tokens,
    canonicalise_token,
    deep_freeze,
    find_first_created_ancestor,
    format_path_tokens,
    is_ref_only,
    is_ref_wrapper,
    make_key_token,
    parse_path_tokens,
    read_raw_path,
    read_raw_tokens,
    thaw_copy,
    unwrap_entry,
    write_tokens,
)
from .style_ledger import StyleLedger, WrapperStyle

_CANNOT_MERGE = object()
_STRICT_NUMERIC_TYPES = (bool, int, float, complex, Decimal, Fraction, np.generic)


def _float_strict_equal(left: Any, right: Any) -> bool:
    """Compare floating-point scalars by exact type, NaN equality, value, and zero sign."""
    if type(left) is not type(right):
        return False
    if bool(np.isnan(left)) and bool(np.isnan(right)):
        return True
    if left != right:
        return False
    return bool(np.signbit(left)) == bool(np.signbit(right))


def _complex_strict_equal(left: Any, right: Any) -> bool:
    """Compare complex scalars by exact type, real/imag NaN equality, value, and zero sign."""
    return (
        type(left) is type(right)
        and _float_strict_equal(left.real, right.real)
        and _float_strict_equal(left.imag, right.imag)
    )


def _real_float_arrays_strict_equal(
    left: np.ndarray[Any, Any], right: np.ndarray[Any, Any]
) -> bool:
    """Compare two real floating arrays strictly by NaN positions, values, and zero sign."""
    if not bool(np.array_equal(left, right, equal_nan=True)):
        return False
    non_nan = ~np.isnan(left)
    return bool(np.array_equal(np.signbit(left)[non_nan], np.signbit(right)[non_nan]))


def _dtype_strict_equal(left: np.dtype[Any], right: np.dtype[Any]) -> bool:
    """Compare NumPy dtypes strictly including metadata on top-level, field, and subarray dtypes."""
    if left != right or (left.metadata is None) != (right.metadata is None):
        return False
    if left.metadata is not None and not _mapping_strict_equal(
        left.metadata, right.metadata
    ):
        return False
    if (left.fields is None) != (right.fields is None):
        return False
    if (
        left.fields is not None
        and right.fields is not None
        and (
            left.fields.keys() != right.fields.keys()
            or any(
                not _dtype_strict_equal(left.fields[name][0], right.fields[name][0])
                for name in left.fields
            )
        )
    ):
        return False
    if (left.subdtype is None) != (right.subdtype is None):
        return False
    return (
        left.subdtype is None
        or right.subdtype is None
        or (
            left.subdtype[1] == right.subdtype[1]
            and _dtype_strict_equal(left.subdtype[0], right.subdtype[0])
        )
    )


def _arrays_strict_equal(
    left: np.ndarray[Any, Any], right: np.ndarray[Any, Any]
) -> bool:
    """Compare two NumPy arrays strictly by exact type, shape, dtype, NaN-equality, sign, and elements."""
    if (
        type(left) is not type(right)
        or left.shape != right.shape
        or not _dtype_strict_equal(left.dtype, right.dtype)
    ):
        return False
    if left.dtype.hasobject:
        return all(_strict_equal(a, b) for a, b in zip(left.flat, right.flat))
    if left.dtype.names:
        return all(
            _arrays_strict_equal(left[name], right[name]) for name in left.dtype.names
        )
    if np.issubdtype(left.dtype, np.floating):
        return _real_float_arrays_strict_equal(left, right)
    if np.issubdtype(left.dtype, np.complexfloating):
        return _real_float_arrays_strict_equal(
            left.real, right.real
        ) and _real_float_arrays_strict_equal(left.imag, right.imag)
    return bool(np.array_equal(left, right))


def _numpy_strict_equal(left: Any, right: Any) -> bool:
    """Compare NumPy ``ndarray`` or ``np.void`` values strictly."""
    if isinstance(left, np.ndarray) or isinstance(right, np.ndarray):
        return (
            isinstance(left, np.ndarray)
            and isinstance(right, np.ndarray)
            and _arrays_strict_equal(left, right)
        )
    if not (isinstance(left, np.void) and isinstance(right, np.void)):
        return False
    if not _dtype_strict_equal(left.dtype, right.dtype):
        return False
    if left.dtype.names:
        return all(_strict_equal(left[name], right[name]) for name in left.dtype.names)
    return bytes(left) == bytes(right)


def _strict_hashable_key(value: Any) -> Any:
    """Build a hashable key preserving exact type, NaN equality, and zero sign."""
    val_type = type(value)
    result: Any
    if isinstance(value, (float, np.floating)):
        result = (
            (val_type, "nan")
            if bool(np.isnan(value))
            else (val_type, bool(np.signbit(value)), value)
        )
    elif isinstance(value, (complex, np.complexfloating)):
        result = (
            val_type,
            _strict_hashable_key(value.real),
            _strict_hashable_key(value.imag),
        )
    elif isinstance(value, Decimal):
        result = (
            (val_type, "snan" if value.is_snan() else "nan")
            if value.is_nan()
            else (val_type, value.is_signed(), value)
        )
    elif val_type is tuple:
        result = (tuple, tuple(_strict_hashable_key(item) for item in value))
    elif val_type is frozenset:
        result = (frozenset, frozenset(_strict_hashable_key(item) for item in value))
    else:
        result = (val_type, value)
    return result


def _mapping_strict_equal(left: Mapping[Any, Any], right: Mapping[Any, Any]) -> bool:
    """Compare two mappings strictly while distinguishing key types and signed zeros."""
    if len(left) != len(right):
        return False
    left_typed = {_strict_hashable_key(k): v for k, v in left.items()}
    right_typed = {_strict_hashable_key(k): v for k, v in right.items()}
    if len(left_typed) != len(left) or len(right_typed) != len(right):
        return False
    return left_typed.keys() == right_typed.keys() and all(
        _strict_equal(left_typed[k], right_typed[k]) for k in left_typed
    )


def _container_strict_equal(left: Any, right: Any) -> bool:
    """Compare sequences, mappings, or sets strictly while keeping element types and zero sign distinct."""
    if isinstance(left, (list, tuple)) and isinstance(right, (list, tuple)):
        return (
            type(left) is type(right)
            and len(left) == len(right)
            and all(_strict_equal(a, b) for a, b in zip(left, right))
        )
    if isinstance(left, Mapping) and isinstance(right, Mapping):
        return _mapping_strict_equal(left, right)
    if isinstance(left, (set, frozenset)) and isinstance(right, (set, frozenset)):
        if type(left) is not type(right) or len(left) != len(right):
            return False
        left_keys = {_strict_hashable_key(item) for item in left}
        right_keys = {_strict_hashable_key(item) for item in right}
        if len(left_keys) != len(left) or len(right_keys) != len(right):
            return False
        return left_keys == right_keys
    return False


def _numeric_strict_equal(left: Any, right: Any) -> bool:
    """Compare numeric scalars strictly by exact type, value, NaN-equality, and zero sign."""
    if type(left) is not type(right):
        return False
    if isinstance(left, (float, np.floating)):
        return _float_strict_equal(left, right)
    if isinstance(left, (complex, np.complexfloating)):
        return _complex_strict_equal(left, right)
    if isinstance(left, Decimal):
        if left.is_nan() or right.is_nan():
            return (
                left.is_nan() and right.is_nan() and left.is_snan() == right.is_snan()
            )
        return bool(left == right) and left.is_signed() == right.is_signed()
    return bool(left == right)


def _strict_equal(left: Any, right: Any) -> bool:
    """Compare two config values strictly, keeping scalar types and zero sign distinct."""
    if isinstance(left, (np.ndarray, np.void)) or isinstance(
        right, (np.ndarray, np.void)
    ):
        return _numpy_strict_equal(left, right)
    if isinstance(left, _STRICT_NUMERIC_TYPES) or isinstance(
        right, _STRICT_NUMERIC_TYPES
    ):
        return _numeric_strict_equal(left, right)
    if isinstance(left, (list, tuple, Mapping, set, frozenset)) or isinstance(
        right, (list, tuple, Mapping, set, frozenset)
    ):
        return _container_strict_equal(left, right)
    if type(left) is not type(right):
        return False
    return bool(left == right)


def _classify_raw_entry(
    raw_entry: Any,
) -> tuple[bool, Any, WrapperStyle | None, Mapping[str, Any] | None]:
    """Extract ``(had_original, unwrapped_old_value, original_style, original_ref)``."""
    if raw_entry is _MISSING:
        return False, None, None, None
    if is_ref_only(raw_entry):
        ref_val = raw_entry.get("ref")
        return (
            True,
            None,
            "ref_only",
            deep_freeze(ref_val) if isinstance(ref_val, Mapping) else None,
        )
    if is_ref_wrapper(raw_entry):
        ref_val = raw_entry.get("ref")
        return (
            True,
            _copy_payload(raw_entry["value"]),
            "ref_value",
            deep_freeze(ref_val) if isinstance(ref_val, Mapping) else None,
        )
    return True, _copy_payload(raw_entry), "bare", None


def _token_dict_aliases(left: PathToken, right: PathToken) -> bool:
    """Return True when two path tokens address the same container slot at runtime."""
    left_is_idx = _is_list_index_token(left)
    right_is_idx = _is_list_index_token(right)
    if left_is_idx or right_is_idx:
        return left_is_idx and right_is_idx and left == right
    left_key = _mapping_key_from_token(left)
    right_key = _mapping_key_from_token(right)
    return left_key is right_key or left_key == right_key


def _tokens_overlap(left: Sequence[PathToken], right: Sequence[PathToken]) -> bool:
    """Return True when *left* and *right* alias the same slot or an ancestor/descendant."""
    min_len = min(len(left), len(right))
    return all(
        _token_dict_aliases(canonicalise_token(a), canonicalise_token(b))
        for a, b in zip(left[:min_len], right[:min_len])
    )


def _resolve_patch_tokens(patch: ConfigPatch) -> tuple[PathToken, ...]:
    if patch.path_tokens is not None:
        return tuple(canonicalise_token(t) for t in patch.path_tokens)
    return tuple(parse_path_tokens(patch.path))


def _format_relative_tokens(tokens: tuple[PathToken, ...]) -> str:
    if not tokens:
        return ""
    return format_path_tokens(tokens)


def _collect_nullifiable_token_entries(
    node: Any,
    *,
    prefix_tokens: tuple[PathToken, ...] = (),
    skip_keys: frozenset[str] = frozenset(),
    skip_at_root_only: bool = True,
) -> list[tuple[tuple[PathToken, ...], str, Any, Any]]:
    """Return ``(tokens, display_path, old_unwrapped_value, new_null_value)`` for every nullified leaf."""
    entries: list[tuple[tuple[PathToken, ...], str, Any, Any]] = []

    def _collect_wrapper(
        cur: Mapping[str, Any], cur_tokens: tuple[PathToken, ...]
    ) -> None:
        inner = cur["value"]
        inner_tokens = (*cur_tokens, "value")
        if isinstance(inner, list):
            if any(isinstance(item, (Mapping, list)) for item in inner):
                _walk(inner, inner_tokens, False)
            elif any(item is not None for item in inner):
                entries.append((
                    cur_tokens,
                    _format_relative_tokens(cur_tokens),
                    _copy_payload(inner),
                    [None] * len(inner),
                ))
        elif isinstance(inner, Mapping):
            _walk(inner, inner_tokens, False)
        elif inner is not None:
            entries.append((
                cur_tokens,
                _format_relative_tokens(cur_tokens),
                _copy_payload(inner),
                None,
            ))

    def _walk(cur: Any, cur_tokens: tuple[PathToken, ...], is_root: bool) -> None:
        if isinstance(cur, Mapping):
            if is_ref_only(cur):
                return
            if is_ref_wrapper(cur):
                _collect_wrapper(cur, cur_tokens)
                return
            for key, val in cur.items():
                if key == "ref" or (
                    key in skip_keys and (is_root or not skip_at_root_only)
                ):
                    continue
                child_tokens = (*cur_tokens, make_key_token(key))
                if isinstance(val, (Mapping, list)):
                    _walk(val, child_tokens, False)
                elif val is not None:
                    entries.append((
                        child_tokens,
                        _format_relative_tokens(child_tokens),
                        _copy_payload(val),
                        None,
                    ))
        elif isinstance(cur, list):
            for idx, item in enumerate(cur):
                child_tokens = (*cur_tokens, idx)
                if isinstance(item, (Mapping, list)):
                    _walk(item, child_tokens, False)
                elif item is not None:
                    entries.append((
                        child_tokens,
                        _format_relative_tokens(child_tokens),
                        _copy_payload(item),
                        None,
                    ))
        elif cur is not None:
            entries.append((
                cur_tokens,
                _format_relative_tokens(cur_tokens),
                _copy_payload(cur),
                None,
            ))

    _walk(node, prefix_tokens, True)
    return entries


def collect_nullifiable_entries(
    node: Any,
    *,
    prefix: str = "",
    skip_keys: frozenset[str] = frozenset(),
    skip_at_root_only: bool = True,
) -> list[tuple[str, Any, Any]]:
    """Return ``(leaf_path, old_unwrapped_value, new_null_value)`` for every nullified leaf.

    Matches :func:`nullify_tree_in_place` traversal order and semantics:
    scalar leaves map to ``None``; scalar lists map to ``[None] * len(list)``;
    ref-only placeholders and sibling ``"ref"`` metadata blocks are untouched.
    """
    prefix_tokens: tuple[PathToken, ...] = (
        tuple(parse_path_tokens(prefix)) if prefix else ()
    )
    return [
        (display_path, old_val, new_val)
        for _tokens, display_path, old_val, new_val in _collect_nullifiable_token_entries(
            node,
            prefix_tokens=prefix_tokens,
            skip_keys=skip_keys,
            skip_at_root_only=skip_at_root_only,
        )
    ]


def collect_nullifiable_values(
    node: Any,
    *,
    prefix: str = "",
    skip_keys: frozenset[str] = frozenset(),
    skip_at_root_only: bool = True,
) -> dict[str, Any]:
    """Return ``{path: original_value}`` for every leaf ``nullify_tree`` would clear."""
    return {
        path: old_val
        for path, old_val, _ in collect_nullifiable_entries(
            node,
            prefix=prefix,
            skip_keys=skip_keys,
            skip_at_root_only=skip_at_root_only,
        )
    }


def _find_existing_dict_key(mapping: Mapping[Any, Any], key: Any) -> tuple[bool, Any]:
    """Return ``(True, existing_key)`` preserving exact key type when *key* matches."""
    if key not in mapping:
        return False, key
    for existing_key in mapping:
        if existing_key == key:
            return True, existing_key
    return False, key


def _resolve_existing_tokens(
    data: Any, tokens: tuple[PathToken, ...]
) -> tuple[PathToken, ...]:
    """Replace aliased mapping-key tokens with the exact existing key types in *data*."""
    resolved: list[PathToken] = []
    current: Any = data
    for idx, token in enumerate(tokens):
        if _is_list_index_token(token):
            assert isinstance(token, int)
            if isinstance(current, (list, tuple)) and 0 <= token < len(current):
                resolved.append(token)
                current = current[token]
                continue
            resolved.extend(tokens[idx:])
            break
        if isinstance(current, Mapping):
            raw_key = _mapping_key_from_token(token)
            found, existing_key = _find_existing_dict_key(current, raw_key)
            if found:
                resolved.append(make_key_token(existing_key))
                current = current[existing_key]
                continue
        resolved.extend(tokens[idx:])
        break
    return tuple(resolved)


def _apply_atomic_set(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
    style_ledger: StyleLedger | None,
) -> tuple[ConfigPatch, Any]:
    raw_before = read_raw_tokens(working, tokens, default=_MISSING)
    had_original, old_val, orig_style, orig_ref = _classify_raw_entry(raw_before)
    raw_snapshot = _copy_payload(raw_before) if had_original else _UNSET
    created_ancestor = find_first_created_ancestor(working, tokens)
    if (
        patch.write_style is not None
        or patch.write_ref is not None
        or patch.write_has_ref
    ):
        forward_style = patch.write_style
        forward_ref = patch.write_ref
        forward_has_ref: bool | None = patch.write_has_ref or forward_ref is not None
    else:
        forward_style = patch.original_style
        forward_ref = patch.original_ref
        forward_has_ref = True if forward_ref is not None else None
    applied = write_tokens(
        working,
        tokens,
        patch.new_value,
        default_wrapped=patch.default_wrapped,
        style_ledger=style_ledger,
        original_style=forward_style,
        original_ref=forward_ref,
        has_ref=forward_has_ref,
        replace_raw=patch.replace_raw,
    )
    if not applied:
        raise ValueError(f"Failed to apply 'set' patch at path {patch.path!r}")
    raw_after = read_raw_tokens(working, tokens, default=_MISSING)
    _, _, written_style, written_ref = _classify_raw_entry(raw_after)
    written_has_ref = (
        written_style in {"ref_value", "ref_only"}
        and isinstance(raw_after, Mapping)
        and "ref" in raw_after
    )
    recorded_replace_raw = (
        patch.replace_raw
        or is_ref_wrapper(patch.new_value)
        or is_ref_only(patch.new_value)
    )
    recorded_new_value = (
        _copy_payload(raw_after) if recorded_replace_raw else patch.new_value
    )
    recorded = ConfigPatch(
        path=canon_path,
        action="set",
        new_value=recorded_new_value,
        old_value=old_val,
        reason=patch.reason,
        parameter=patch.parameter,
        site_index=patch.site_index,
        site_gridid=patch.site_gridid,
        source=patch.source,
        skip_keys=patch.skip_keys,
        default_wrapped=written_style != "bare"
        if written_style is not None
        else patch.default_wrapped,
        had_original=had_original,
        original_style=orig_style,
        original_ref=orig_ref,
        path_tokens=tokens,
        original_raw=raw_snapshot,
        replace_raw=recorded_replace_raw,
        created_ancestor=created_ancestor,
        write_style=written_style,
        write_ref=written_ref,
        write_has_ref=written_has_ref,
    )
    return recorded, _copy_payload(raw_after)


def _apply_atomic_delete(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
) -> tuple[ConfigPatch, Any]:
    located = _ensure_parent_container(working, tokens, create_missing=False)
    if located is None:
        raise ValueError(
            f"Failed to apply 'delete' patch at path {patch.path!r}: "
            "parent container not found"
        )
    parent, leaf = located
    if not isinstance(parent, dict) or _is_list_index_token(leaf):
        raise ValueError(
            f"Failed to apply 'delete' patch at path {patch.path!r}: "
            "target must be a mapping key"
        )
    raw_key = _mapping_key_from_token(leaf)
    raw_before = parent.get(raw_key, _MISSING)
    had_original, old_val, orig_style, orig_ref = _classify_raw_entry(raw_before)
    raw_snapshot = _copy_payload(raw_before) if had_original else _UNSET
    parent.pop(raw_key, None)
    recorded = ConfigPatch(
        path=canon_path,
        action="delete",
        new_value=None,
        old_value=old_val,
        reason=patch.reason,
        parameter=patch.parameter,
        site_index=patch.site_index,
        site_gridid=patch.site_gridid,
        source=patch.source,
        skip_keys=patch.skip_keys,
        default_wrapped=patch.default_wrapped,
        had_original=had_original,
        original_style=orig_style,
        original_ref=orig_ref,
        path_tokens=tokens,
        original_raw=raw_snapshot,
    )
    return recorded, _MISSING


def _apply_atomic_nullify(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
    style_ledger: StyleLedger | None,
    *,
    expand_nullify_tree: bool,
) -> list[tuple[ConfigPatch, Any]]:
    raw_before = read_raw_tokens(working, tokens, default=_MISSING)
    if raw_before is _MISSING:
        raise ValueError(f"Failed to apply 'nullify_tree' patch at path {patch.path!r}")

    skip_set = frozenset(patch.skip_keys)
    if expand_nullify_tree:
        token_entries = _collect_nullifiable_token_entries(
            raw_before,
            prefix_tokens=tokens,
            skip_keys=skip_set,
            skip_at_root_only=True,
        )
        expanded: list[tuple[ConfigPatch, Any]] = []
        for leaf_tokens, leaf_path, _old_val, new_null_val in token_entries:
            cur_leaf = read_raw_tokens(working, leaf_tokens, default=_MISSING)
            _, _, cur_style, cur_ref = _classify_raw_entry(cur_leaf)
            cur_has_ref = isinstance(cur_leaf, Mapping) and "ref" in cur_leaf
            leaf_patch = ConfigPatch(
                path=leaf_path,
                action="set",
                new_value=new_null_val,
                reason=patch.reason,
                parameter=patch.parameter,
                site_index=patch.site_index,
                site_gridid=patch.site_gridid,
                source=patch.source,
                default_wrapped=patch.default_wrapped,
                path_tokens=leaf_tokens,
                write_style=cur_style,
                write_ref=cur_ref,
                write_has_ref=cur_has_ref,
            )
            expanded.append(
                _apply_atomic_set(working, leaf_patch, leaf_tokens, leaf_path, None)
            )
        return expanded

    raw_snapshot = _copy_payload(raw_before)
    rel_entries = _collect_nullifiable_token_entries(
        raw_before,
        prefix_tokens=(),
        skip_keys=skip_set,
        skip_at_root_only=True,
    )
    rel_snapshot = {display: old_val for _toks, display, old_val, _new in rel_entries}
    applied = apply_nullify_tree_at_tokens(working, tokens, skip_keys=skip_set)
    if not applied:
        raise ValueError(f"Failed to apply 'nullify_tree' patch at path {patch.path!r}")
    if not rel_snapshot:
        return []
    return [
        (
            ConfigPatch(
                path=canon_path,
                action="nullify_tree",
                new_value=None,
                old_value=rel_snapshot,
                reason=patch.reason,
                parameter=patch.parameter,
                site_index=patch.site_index,
                site_gridid=patch.site_gridid,
                source=patch.source,
                skip_keys=patch.skip_keys,
                default_wrapped=patch.default_wrapped,
                had_original=True,
                path_tokens=tokens,
                original_raw=raw_snapshot,
            ),
            _UNSET,
        )
    ]


def _is_noop_set_patch(patch: ConfigPatch, *, raw_after: Any = _UNSET) -> bool:
    if patch.created_ancestor is not None or patch.had_original is not True:
        return False
    if raw_after is not _UNSET and patch.original_raw is not _UNSET:
        return _strict_equal(patch.original_raw, raw_after)
    if patch.replace_raw:
        return patch.original_raw is not _UNSET and _strict_equal(
            patch.original_raw, patch.new_value
        )
    if patch.write_style is not None and patch.original_style != patch.write_style:
        return False
    if (patch.original_ref is not None or patch.write_ref is not None) and (
        patch.original_ref != patch.write_ref
    ):
        return False
    return _strict_equal(patch.old_value, patch.new_value)


def _is_noop_patch(patch: ConfigPatch, *, raw_after: Any = _UNSET) -> bool:
    if patch.action == "delete":
        return patch.had_original is False
    if patch.action == "set":
        return _is_noop_set_patch(patch, raw_after=raw_after)
    if patch.action == "nullify_tree":
        return isinstance(patch.old_value, Mapping) and not patch.old_value
    return False


def _writes_explicit_wrapper(patch: ConfigPatch) -> bool:
    return (
        patch.replace_raw
        or is_ref_wrapper(patch.new_value)
        or is_ref_only(patch.new_value)
        or (isinstance(patch.new_value, Mapping) and "value" in patch.new_value)
    )


def _merge_with_delete(
    first: ConfigPatch, second: ConfigPatch
) -> ConfigPatch | object | None:
    if first.had_original is False:
        if first.created_ancestor is not None:
            return _CANNOT_MERGE
        return None
    return ConfigPatch(
        path=first.path,
        action="delete",
        new_value=None,
        old_value=first.old_value,
        reason=second.reason or first.reason,
        parameter=second.parameter or first.parameter,
        site_index=second.site_index
        if second.site_index is not None
        else first.site_index,
        site_gridid=second.site_gridid
        if second.site_gridid is not None
        else first.site_gridid,
        source=second.source or first.source,
        skip_keys=second.skip_keys,
        default_wrapped=first.default_wrapped,
        had_original=first.had_original,
        original_style=first.original_style,
        original_ref=first.original_ref,
        path_tokens=first.path_tokens,
        original_raw=first.original_raw,
    )


def _delete_then_set_preserves_style(
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger | None
) -> bool:
    explicit_write = (
        second.write_style is not None
        or second.write_ref is not None
        or second.write_has_ref
    )
    ledger_style = (
        style_ledger.wrapper_style(second.path)
        if style_ledger is not None and not explicit_write
        else None
    )
    eff_style = second.write_style or second.original_style or ledger_style
    if eff_style is None:
        eff_style = "ref_value" if second.default_wrapped else "bare"
    if explicit_write:
        eff_ref = second.write_ref
        eff_has_ref = second.write_has_ref or eff_ref is not None
    elif second.original_ref is not None:
        eff_ref = second.original_ref
        eff_has_ref = True
    elif style_ledger is not None:
        eff_ref = style_ledger.ref_metadata(second.path)
        eff_has_ref = style_ledger.has_ref_key(second.path)
    else:
        eff_ref = None
        eff_has_ref = False
    if eff_style != first.original_style or eff_ref != first.original_ref:
        return False
    # Non-RefValue mappings (e.g. {"value": 1, "unit": "m"}) cannot merge delete+set into an update.
    if (
        isinstance(first.original_raw, Mapping)
        and not is_ref_wrapper(first.original_raw)
        and not is_ref_only(first.original_raw)
    ):
        return False
    orig_has_ref = (
        isinstance(first.original_raw, Mapping) and "ref" in first.original_raw
    )
    return eff_has_ref == orig_has_ref


def _merge_with_set(
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger | None
) -> ConfigPatch | object | None:
    if _writes_explicit_wrapper(first) or _writes_explicit_wrapper(second):
        return _CANNOT_MERGE
    first_orig_has_ref = (
        isinstance(first.original_raw, Mapping) and "ref" in first.original_raw
    )
    if (
        first.action == "set"
        and first.had_original is True
        and (
            first.write_style != first.original_style
            or first.write_ref != first.original_ref
            or first.write_has_ref != first_orig_has_ref
        )
    ):
        return _CANNOT_MERGE
    if first.action == "delete":
        if first.had_original is False:
            return second
        if not _delete_then_set_preserves_style(first, second, style_ledger):
            return _CANNOT_MERGE
    return ConfigPatch(
        path=first.path,
        action="set",
        new_value=second.new_value,
        old_value=first.old_value,
        reason=second.reason or first.reason,
        parameter=second.parameter or first.parameter,
        site_index=second.site_index
        if second.site_index is not None
        else first.site_index,
        site_gridid=second.site_gridid
        if second.site_gridid is not None
        else first.site_gridid,
        source=second.source or first.source,
        skip_keys=second.skip_keys,
        default_wrapped=second.default_wrapped,
        had_original=first.had_original,
        original_style=first.original_style,
        original_ref=first.original_ref,
        path_tokens=first.path_tokens,
        original_raw=first.original_raw,
        created_ancestor=first.created_ancestor,
        write_style=second.write_style,
        write_ref=second.write_ref,
        write_has_ref=second.write_has_ref,
    )


def _merge_same_path(
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger | None
) -> ConfigPatch | object | None:
    """Coalesce two non-interfering patches on the exact same canonical path."""
    if second.action == "delete":
        return _merge_with_delete(first, second)
    if second.action == "set":
        return _merge_with_set(first, second, style_ledger)
    return _CANNOT_MERGE


def _assert_no_aliased_containers(value: Any) -> None:
    """Reject targets containing shared/aliased or cyclic containers."""
    seen: set[int] = set()

    def _walk(node: Any) -> None:
        if node is None or type(node) in {bool, int, float, str, bytes}:
            return
        if isinstance(node, (dict, list)) or (isinstance(node, Mapping) and bool(node)):
            obj_id = id(node)
            if obj_id in seen:
                raise ValueError(
                    "Cannot normalise patches against a target with aliased or cyclic "
                    "containers; construct a detached copy (e.g. ConfigDocument.from_dict "
                    "or thaw_copy) first"
                )
            seen.add(obj_id)
            children = node.values() if isinstance(node, Mapping) else node
            for child in children:
                _walk(child)
            return
        if isinstance(node, (tuple, set, frozenset)):
            for item in node:
                _walk(item)

    _walk(value)


def _coalesce_owned_patches(
    owned_patches: list[tuple[int | None, ConfigPatch, Any]],
    style_ledger: StyleLedger | None,
) -> list[tuple[int | None, ConfigPatch, Any]]:
    """Coalesce sequential same-path patches without crossing ancestor/descendant barriers."""
    result: list[tuple[int | None, ConfigPatch, Any]] = []
    result_tokens: list[tuple[PathToken, ...]] = []

    for owner_idx, patch, raw_after in owned_patches:
        tokens = _resolve_patch_tokens(patch)
        if patch.action == "nullify_tree":
            result.append((owner_idx, patch, raw_after))
            result_tokens.append(tokens)
            continue

        merge_idx: int | None = None
        for idx in range(len(result) - 1, -1, -1):
            _prev_owner, prev, _prev_raw_after = result[idx]
            prev_tokens = result_tokens[idx]
            if prev_tokens == tokens and prev.action != "nullify_tree":
                merge_idx = idx
                break
            if _tokens_overlap(prev_tokens, tokens):
                break

        if merge_idx is None:
            result.append((owner_idx, patch, raw_after))
            result_tokens.append(tokens)
            continue

        prev_owner, prev_patch, _prev_raw_after = result[merge_idx]
        merged = _merge_same_path(prev_patch, patch, style_ledger)
        if merged is _CANNOT_MERGE:
            result.append((owner_idx, patch, raw_after))
            result_tokens.append(tokens)
            continue

        if isinstance(merged, ConfigPatch):
            # Replace in place so any created_ancestor stays ordered before sibling writes.
            merged_owner = owner_idx if owner_idx is not None else prev_owner
            result[merge_idx] = (merged_owner, merged, raw_after)
        else:
            result.pop(merge_idx)
            result_tokens.pop(merge_idx)

    return result


def normalise_owned_patches(
    owned_patches: Sequence[tuple[int | None, ConfigPatch]],
    target: ConfigDocument | Mapping[str, Any],
    *,
    expand_nullify_tree: bool = True,
    drop_noops: bool = True,
    coalesce: bool = True,
) -> list[tuple[int | None, ConfigPatch]]:
    """Normalise ``(owner_id, patch)`` pairs while reconciling ownership across coalescing."""
    if isinstance(target, ConfigDocument):
        base_data: Mapping[str, Any] = target.data
        style_ledger: StyleLedger | None = target.style_ledger
    else:
        base_data = target
        style_ledger = None

    _assert_no_aliased_containers(base_data)
    working = _prepare_working_dict(target)
    recorded: list[tuple[int | None, ConfigPatch, Any]] = []

    for owner_idx, patch in owned_patches:
        if patch.action not in _VALID_PATCH_ACTIONS:
            raise ValueError(
                f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
            )
        tokens = _resolve_existing_tokens(working, _resolve_patch_tokens(patch))
        canon_path = format_path_tokens(tokens)
        if patch.action == "set":
            rec_patch, raw_after = _apply_atomic_set(
                working, patch, tokens, canon_path, style_ledger
            )
            recorded.append((owner_idx, rec_patch, raw_after))
        elif patch.action == "delete":
            rec_patch, raw_after = _apply_atomic_delete(
                working, patch, tokens, canon_path
            )
            recorded.append((owner_idx, rec_patch, raw_after))
        elif patch.action == "nullify_tree":
            expanded = _apply_atomic_nullify(
                working,
                patch,
                tokens,
                canon_path,
                style_ledger,
                expand_nullify_tree=expand_nullify_tree,
            )
            recorded.extend(
                (owner_idx, item, raw_after) for item, raw_after in expanded
            )

    if coalesce:
        recorded = _coalesce_owned_patches(recorded, style_ledger)
    if drop_noops:
        recorded = [
            (owner, p, raw_after)
            for owner, p, raw_after in recorded
            if not _is_noop_patch(p, raw_after=raw_after)
        ]
    return [(owner, p) for owner, p, _raw_after in recorded]


def normalise_patches(
    patches: Sequence[ConfigPatch],
    target: ConfigDocument | Mapping[str, Any],
    *,
    expand_nullify_tree: bool = True,
    drop_noops: bool = True,
    coalesce: bool = True,
) -> list[ConfigPatch]:
    """Normalise *patches* against *target* so every patch carries its original value.

    Parameters
    ----------
    patches : Sequence[ConfigPatch]
        Raw declarative patches to normalise.
    target : ConfigDocument or Mapping[str, Any]
        Baseline configuration (never mutated).
    expand_nullify_tree : bool, default True
        When ``True``, expand ``nullify_tree`` patches into atomic leaf ``set``
        patches carrying each leaf's original value. When ``False``, retain
        ``nullify_tree`` with ``old_value`` set to ``{rel_path: old_value}``.
    drop_noops : bool, default True
        Drop mutations that leave the target value unchanged.
    coalesce : bool, default True
        Merge sequential mutations on the same canonical path across
        non-interfering barriers while preserving the initial ``old_value``
        and authored representation.

    Returns
    -------
    list[ConfigPatch]
        Normalised patches carrying ``old_value``, ``had_original``,
        ``original_style``, ``original_ref``, ``original_raw``, and
        ``created_ancestor``.
    """
    owned = normalise_owned_patches(
        [(None, patch) for patch in patches],
        target,
        expand_nullify_tree=expand_nullify_tree,
        drop_noops=drop_noops,
        coalesce=coalesce,
    )
    return [patch for _, patch in owned]


def _restore_payload_for_inverse(patch: ConfigPatch, default_wrapped: bool) -> Any:
    if patch.original_raw is not _UNSET:
        return patch.original_raw
    return _build_new_entry(
        patch.old_value,
        default_wrapped=default_wrapped,
        wrapper_style=patch.original_style,
        ref_metadata=patch.original_ref,
    )


def _invert_created_ancestor(patch: ConfigPatch, reason: str) -> ConfigPatch | None:
    if patch.created_ancestor is None:
        return None
    anc_tokens, was_none = patch.created_ancestor
    anc_path = format_path_tokens(anc_tokens)
    if was_none:
        return ConfigPatch(
            path=anc_path,
            action="set",
            new_value=None,
            old_value={},
            reason=reason,
            parameter=patch.parameter,
            site_index=patch.site_index,
            site_gridid=patch.site_gridid,
            source=patch.source,
            default_wrapped=False,
            had_original=True,
            original_style="bare",
            path_tokens=anc_tokens,
            replace_raw=True,
        )
    return ConfigPatch(
        path=anc_path,
        action="delete",
        new_value=None,
        old_value={},
        reason=reason,
        parameter=patch.parameter,
        site_index=patch.site_index,
        site_gridid=patch.site_gridid,
        source=patch.source,
        default_wrapped=False,
        had_original=True,
        path_tokens=anc_tokens,
    )


def _invert_single_patch(patch: ConfigPatch) -> list[ConfigPatch]:
    if patch.action == "nullify_tree":
        raise ValueError(
            f"Cannot invert unexpanded 'nullify_tree' patch at {patch.path!r}; "
            "normalise with expand_nullify_tree=True first"
        )
    if patch.had_original is None:
        raise ValueError(
            f"Cannot invert unnormalised patch at {patch.path!r}: "
            "had_original is unknown; call normalise_patches() first"
        )
    default_wrapped = (
        patch.original_style != "bare"
        if patch.original_style is not None
        else patch.default_wrapped
    )
    reason = f"Revert: {patch.reason}" if patch.reason else "Revert"

    if patch.action == "set":
        if patch.had_original:
            return [
                ConfigPatch(
                    path=patch.path,
                    action="set",
                    new_value=_restore_payload_for_inverse(patch, default_wrapped),
                    old_value=patch.new_value,
                    reason=reason,
                    parameter=patch.parameter,
                    site_index=patch.site_index,
                    site_gridid=patch.site_gridid,
                    source=patch.source,
                    default_wrapped=default_wrapped,
                    had_original=True,
                    original_style=patch.original_style,
                    original_ref=patch.original_ref,
                    path_tokens=patch.path_tokens,
                    replace_raw=True,
                )
            ]
        steps: list[ConfigPatch] = [
            ConfigPatch(
                path=patch.path,
                action="delete",
                new_value=None,
                old_value=patch.new_value,
                reason=reason,
                parameter=patch.parameter,
                site_index=patch.site_index,
                site_gridid=patch.site_gridid,
                source=patch.source,
                default_wrapped=default_wrapped,
                had_original=True,
                path_tokens=patch.path_tokens,
            )
        ]
        anc_step = _invert_created_ancestor(patch, reason)
        if anc_step is not None:
            steps.append(anc_step)
        return steps

    if patch.action == "delete":
        if not patch.had_original:
            return []
        return [
            ConfigPatch(
                path=patch.path,
                action="set",
                new_value=_restore_payload_for_inverse(patch, default_wrapped),
                old_value=None,
                reason=reason,
                parameter=patch.parameter,
                site_index=patch.site_index,
                site_gridid=patch.site_gridid,
                source=patch.source,
                default_wrapped=default_wrapped,
                had_original=False,
                original_style=patch.original_style,
                original_ref=patch.original_ref,
                path_tokens=patch.path_tokens,
                replace_raw=True,
            )
        ]

    raise ValueError(
        f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
    )


def invert_patches(patches: Sequence[ConfigPatch]) -> list[ConfigPatch]:
    """Construct the inverse patch sequence that undoes *patches*.

    Requires normalised atomic patches (``had_original`` populated and no
    unexpanded ``nullify_tree`` actions). Applying ``patches`` followed by
    ``invert_patches(patches)`` restores the original document values,
    ``RefValue`` / bare / ref-only representations, explicit ``ref: None``
    entries, and removes any implicitly created ancestor containers.
    """
    inverse: list[ConfigPatch] = []
    for patch in reversed(patches):
        inverse.extend(_invert_single_patch(patch))
    return inverse


def read_original_unwrapped(
    target: ConfigDocument | Mapping[str, Any], path: str
) -> tuple[bool, Any]:
    """Return ``(exists, unwrapped_value)`` for *path* in *target*."""
    data = target.data if isinstance(target, ConfigDocument) else target
    raw = read_raw_path(data, path, default=_MISSING)
    if raw is _MISSING:
        return False, None
    return True, thaw_copy(unwrap_entry(raw))


__all__ = [
    "MappingKey",
    "collect_nullifiable_entries",
    "collect_nullifiable_values",
    "invert_patches",
    "normalise_owned_patches",
    "normalise_patches",
    "read_original_unwrapped",
]
