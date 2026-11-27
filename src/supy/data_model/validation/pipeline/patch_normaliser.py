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
import math
from typing import Any

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
    apply_nullify_tree_at_tokens,
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


def _strict_equal(left: Any, right: Any) -> bool:
    """Compare two config values strictly, keeping ``bool`` distinct from ``int``/``float``."""
    if isinstance(left, bool) != isinstance(right, bool):
        return False
    if isinstance(left, float) and isinstance(right, float):
        return (math.isnan(left) and math.isnan(right)) or left == right
    if isinstance(left, (list, tuple)) and isinstance(right, (list, tuple)):
        return len(left) == len(right) and all(
            _strict_equal(a, b) for a, b in zip(left, right)
        )
    if isinstance(left, Mapping) and isinstance(right, Mapping):
        return left.keys() == right.keys() and all(
            _strict_equal(left[k], right[k]) for k in left
        )
    return left == right


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
            thaw_copy(raw_entry["value"]),
            "ref_value",
            deep_freeze(ref_val) if isinstance(ref_val, Mapping) else None,
        )
    return True, thaw_copy(raw_entry), "bare", None


def _tokens_overlap(left: Sequence[PathToken], right: Sequence[PathToken]) -> bool:
    """Return True when *left* and *right* are equal or one is an ancestor of the other."""
    min_len = min(len(left), len(right))
    return tuple(left[:min_len]) == tuple(right[:min_len])


def _resolve_patch_tokens(patch: ConfigPatch) -> tuple[PathToken, ...]:
    if patch.path_tokens is not None:
        return tuple(patch.path_tokens)
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
        if isinstance(inner, (list, tuple)):
            if any(isinstance(item, (Mapping, list, tuple)) for item in inner):
                _walk(inner, inner_tokens, False)
            elif any(item is not None for item in inner):
                entries.append((
                    cur_tokens,
                    _format_relative_tokens(cur_tokens),
                    thaw_copy(inner),
                    [None] * len(inner),
                ))
        elif isinstance(inner, Mapping):
            _walk(inner, inner_tokens, False)
        elif inner is not None:
            entries.append((
                cur_tokens,
                _format_relative_tokens(cur_tokens),
                thaw_copy(inner),
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
                if isinstance(val, (Mapping, list, tuple)):
                    _walk(val, child_tokens, False)
                elif val is not None:
                    entries.append((
                        child_tokens,
                        _format_relative_tokens(child_tokens),
                        thaw_copy(val),
                        None,
                    ))
        elif isinstance(cur, (list, tuple)):
            for idx, item in enumerate(cur):
                child_tokens = (*cur_tokens, idx)
                if isinstance(item, (Mapping, list, tuple)):
                    _walk(item, child_tokens, False)
                elif item is not None:
                    entries.append((
                        child_tokens,
                        _format_relative_tokens(child_tokens),
                        thaw_copy(item),
                        None,
                    ))
        elif cur is not None:
            entries.append((
                cur_tokens,
                _format_relative_tokens(cur_tokens),
                thaw_copy(cur),
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


def _apply_atomic_set(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
    style_ledger: StyleLedger,
) -> ConfigPatch:
    raw_before = read_raw_tokens(working, tokens, default=_MISSING)
    had_original, old_val, orig_style, orig_ref = _classify_raw_entry(raw_before)
    raw_snapshot = _copy_payload(raw_before) if had_original else _UNSET
    created_ancestor = find_first_created_ancestor(working, tokens)
    applied = write_tokens(
        working,
        tokens,
        patch.new_value,
        default_wrapped=patch.default_wrapped,
        style_ledger=style_ledger,
        original_style=patch.original_style,
        original_ref=patch.original_ref,
        replace_raw=patch.replace_raw,
    )
    if not applied:
        raise ValueError(f"Failed to apply 'set' patch at path {patch.path!r}")
    return ConfigPatch(
        path=canon_path,
        action="set",
        new_value=patch.new_value,
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
        replace_raw=patch.replace_raw,
        created_ancestor=created_ancestor,
    )


def _apply_atomic_delete(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
) -> ConfigPatch:
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
    return ConfigPatch(
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


def _apply_atomic_nullify(
    working: dict[str, Any],
    patch: ConfigPatch,
    tokens: tuple[PathToken, ...],
    canon_path: str,
    style_ledger: StyleLedger,
    *,
    expand_nullify_tree: bool,
) -> list[ConfigPatch]:
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
        expanded: list[ConfigPatch] = []
        for leaf_tokens, leaf_path, _old_val, new_null_val in token_entries:
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
            )
            expanded.append(
                _apply_atomic_set(
                    working, leaf_patch, leaf_tokens, leaf_path, style_ledger
                )
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
        )
    ]


def _is_noop_set_patch(patch: ConfigPatch) -> bool:
    if patch.created_ancestor is not None or patch.had_original is not True:
        return False
    if patch.replace_raw:
        return patch.original_raw is not _UNSET and _strict_equal(
            patch.original_raw, patch.new_value
        )
    if patch.original_style == "ref_only" and patch.new_value is None:
        return True
    return _strict_equal(patch.old_value, patch.new_value)


def _is_noop_patch(patch: ConfigPatch) -> bool:
    if patch.action == "delete":
        return patch.had_original is False
    if patch.action == "set":
        return _is_noop_set_patch(patch)
    if patch.action == "nullify_tree":
        return isinstance(patch.old_value, Mapping) and not patch.old_value
    return False


def _writes_explicit_wrapper(patch: ConfigPatch) -> bool:
    return (
        patch.replace_raw
        or is_ref_wrapper(patch.new_value)
        or is_ref_only(patch.new_value)
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
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger
) -> bool:
    eff_style = second.original_style or style_ledger.wrapper_style(second.path)
    if eff_style is None:
        eff_style = "ref_value" if second.default_wrapped else "bare"
    eff_ref = (
        second.original_ref
        if second.original_ref is not None
        else style_ledger.ref_metadata(second.path)
    )
    if eff_style != first.original_style or eff_ref != first.original_ref:
        return False
    # Explicit null ref key in original_raw (e.g. {"value": 1, "ref": None}) would be dropped.
    return not (
        isinstance(first.original_raw, Mapping)
        and "ref" in first.original_raw
        and first.original_raw["ref"] is None
    )


def _merge_with_set(
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger
) -> ConfigPatch | object | None:
    if _writes_explicit_wrapper(first) or _writes_explicit_wrapper(second):
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
        default_wrapped=first.default_wrapped,
        had_original=first.had_original,
        original_style=first.original_style,
        original_ref=first.original_ref,
        path_tokens=first.path_tokens,
        original_raw=first.original_raw,
        created_ancestor=first.created_ancestor,
    )


def _merge_same_path(
    first: ConfigPatch, second: ConfigPatch, style_ledger: StyleLedger
) -> ConfigPatch | object | None:
    """Coalesce two non-interfering patches on the exact same canonical path."""
    if second.action == "delete":
        return _merge_with_delete(first, second)
    if second.action == "set":
        return _merge_with_set(first, second, style_ledger)
    return _CANNOT_MERGE


def _coalesce_owned_patches(
    owned_patches: list[tuple[int | None, ConfigPatch]],
    style_ledger: StyleLedger,
) -> list[tuple[int | None, ConfigPatch]]:
    """Coalesce sequential same-path patches without crossing ancestor/descendant barriers."""
    result: list[tuple[int | None, ConfigPatch]] = []
    result_tokens: list[tuple[PathToken, ...]] = []

    for owner_idx, patch in owned_patches:
        tokens = _resolve_patch_tokens(patch)
        if patch.action == "nullify_tree":
            result.append((owner_idx, patch))
            result_tokens.append(tokens)
            continue

        merge_idx: int | None = None
        for idx in range(len(result) - 1, -1, -1):
            _prev_owner, prev = result[idx]
            prev_tokens = result_tokens[idx]
            if prev_tokens == tokens and prev.action != "nullify_tree":
                merge_idx = idx
                break
            if _tokens_overlap(prev_tokens, tokens):
                break

        if merge_idx is None:
            result.append((owner_idx, patch))
            result_tokens.append(tokens)
            continue

        prev_owner, prev_patch = result[merge_idx]
        merged = _merge_same_path(prev_patch, patch, style_ledger)
        if merged is _CANNOT_MERGE:
            result.append((owner_idx, patch))
            result_tokens.append(tokens)
            continue

        result.pop(merge_idx)
        result_tokens.pop(merge_idx)
        if isinstance(merged, ConfigPatch):
            merged_owner = owner_idx if owner_idx is not None else prev_owner
            result.append((merged_owner, merged))
            result_tokens.append(tokens)

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
        style_ledger = target.style_ledger
    else:
        base_data = target
        style_ledger = StyleLedger.from_mapping(target)

    working = thaw_copy(base_data)
    recorded: list[tuple[int | None, ConfigPatch]] = []

    for owner_idx, patch in owned_patches:
        if patch.action not in _VALID_PATCH_ACTIONS:
            raise ValueError(
                f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
            )
        tokens = _resolve_patch_tokens(patch)
        canon_path = format_path_tokens(tokens)
        if patch.action == "set":
            recorded.append((
                owner_idx,
                _apply_atomic_set(working, patch, tokens, canon_path, style_ledger),
            ))
        elif patch.action == "delete":
            recorded.append((
                owner_idx,
                _apply_atomic_delete(working, patch, tokens, canon_path),
            ))
        elif patch.action == "nullify_tree":
            expanded = _apply_atomic_nullify(
                working,
                patch,
                tokens,
                canon_path,
                style_ledger,
                expand_nullify_tree=expand_nullify_tree,
            )
            recorded.extend((owner_idx, item) for item in expanded)

    if coalesce:
        recorded = _coalesce_owned_patches(recorded, style_ledger)
    if drop_noops:
        recorded = [(owner, p) for owner, p in recorded if not _is_noop_patch(p)]
    return recorded


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
