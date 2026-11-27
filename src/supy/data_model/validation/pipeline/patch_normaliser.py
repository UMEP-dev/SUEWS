"""Patch normalisation carrying original values and exact patch inversion.

Normalises a sequence of declarative :class:`ConfigPatch` records against a
target :class:`ConfigDocument` or raw mapping without mutating the target:

1. Validates patch actions and canonicalises YAML paths.
2. Expands coarse ``nullify_tree`` patches into deterministic leaf ``set``
   patches (or populates original-value snapshots when expansion is disabled).
3. Captures the true pre-patch ``old_value``, ``had_original`` existence flag,
   ``original_style`` (bare vs ``RefValue`` vs ref-only), and frozen
   ``original_ref`` metadata for every mutation.
4. Coalesces sequential writes on identical paths across non-interfering
   barriers (never reordering across ancestor/descendant mutations) and drops
   type-safe net no-ops.
5. Supports lossless inversion via :func:`invert_patches` so applied patches
   can be undone cleanly for rollback or oscillation recovery.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
import math
from typing import Any

from .document import (
    _MISSING,
    _VALID_PATCH_ACTIONS,
    ConfigDocument,
    ConfigPatch,
    _ensure_parent_container,
    apply_nullify_tree_at_path,
    canonicalise_path,
    deep_freeze,
    is_ref_only,
    is_ref_wrapper,
    parse_path_tokens,
    read_raw_path,
    thaw_copy,
    unwrap_entry,
    write_path,
)
from .style_ledger import StyleLedger, WrapperStyle


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


def _tokens_overlap(left: Sequence[str | int], right: Sequence[str | int]) -> bool:
    """Return True when *left* and *right* are equal or one is an ancestor of the other."""
    min_len = min(len(left), len(right))
    return tuple(left[:min_len]) == tuple(right[:min_len])


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
    entries: list[tuple[str, Any, Any]] = []

    def _collect_wrapper(cur: Mapping[str, Any], cur_path: str) -> None:
        inner = cur["value"]
        inner_path = f"{cur_path}.value" if cur_path else "value"
        if isinstance(inner, (list, tuple)):
            if any(isinstance(item, (Mapping, list, tuple)) for item in inner):
                _walk(inner, inner_path, False)
            elif any(item is not None for item in inner):
                entries.append((cur_path, thaw_copy(inner), [None] * len(inner)))
        elif isinstance(inner, Mapping):
            _walk(inner, inner_path, False)
        elif inner is not None:
            entries.append((cur_path, thaw_copy(inner), None))

    def _walk(cur: Any, cur_path: str, is_root: bool) -> None:
        if isinstance(cur, Mapping):
            if is_ref_only(cur):
                return
            if is_ref_wrapper(cur):
                _collect_wrapper(cur, cur_path)
                return
            for key, val in cur.items():
                if key == "ref" or (
                    key in skip_keys and (is_root or not skip_at_root_only)
                ):
                    continue
                child_path = f"{cur_path}.{key}" if cur_path else str(key)
                if isinstance(val, (Mapping, list, tuple)):
                    _walk(val, child_path, False)
                elif val is not None:
                    entries.append((child_path, thaw_copy(val), None))
        elif isinstance(cur, (list, tuple)):
            if not any(isinstance(item, (Mapping, list, tuple)) for item in cur):
                for idx, item in enumerate(cur):
                    if item is not None:
                        child_path = f"{cur_path}[{idx}]" if cur_path else f"[{idx}]"
                        entries.append((child_path, thaw_copy(item), None))
                return
            for idx, item in enumerate(cur):
                child_path = f"{cur_path}[{idx}]" if cur_path else f"[{idx}]"
                if isinstance(item, (Mapping, list, tuple)):
                    _walk(item, child_path, False)
                elif item is not None:
                    entries.append((child_path, thaw_copy(item), None))
        elif cur is not None and cur_path:
            entries.append((cur_path, thaw_copy(cur), None))

    _walk(node, prefix, True)
    return entries


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
    canon_path: str,
    style_ledger: StyleLedger,
) -> ConfigPatch:
    raw_before = read_raw_path(working, canon_path, default=_MISSING)
    had_original, old_val, orig_style, orig_ref = _classify_raw_entry(raw_before)
    applied = write_path(
        working,
        canon_path,
        patch.new_value,
        default_wrapped=patch.default_wrapped,
        style_ledger=style_ledger,
        original_style=patch.original_style,
        original_ref=patch.original_ref,
    )
    if not applied:
        raise ValueError(f"Failed to apply 'set' patch at path {patch.path!r}")
    return ConfigPatch(
        path=canon_path,
        action="set",
        new_value=thaw_copy(patch.new_value),
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
    )


def _apply_atomic_delete(
    working: dict[str, Any],
    patch: ConfigPatch,
    canon_path: str,
) -> ConfigPatch:
    tokens = parse_path_tokens(canon_path)
    located = _ensure_parent_container(working, tokens, create_missing=False)
    if located is None:
        raise ValueError(
            f"Failed to apply 'delete' patch at path {patch.path!r}: "
            "parent container not found"
        )
    parent, leaf = located
    if not isinstance(parent, dict) or not isinstance(leaf, str):
        raise ValueError(
            f"Failed to apply 'delete' patch at path {patch.path!r}: "
            "target must be a mapping key"
        )
    raw_before = parent.get(leaf, _MISSING)
    had_original, old_val, orig_style, orig_ref = _classify_raw_entry(raw_before)
    parent.pop(leaf, None)
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
    )


def _apply_atomic_nullify(
    working: dict[str, Any],
    patch: ConfigPatch,
    canon_path: str,
    style_ledger: StyleLedger,
    *,
    expand_nullify_tree: bool,
) -> list[ConfigPatch]:
    raw_before = read_raw_path(working, canon_path, default=_MISSING)
    if raw_before is _MISSING:
        raise ValueError(f"Failed to apply 'nullify_tree' patch at path {patch.path!r}")

    skip_set = frozenset(patch.skip_keys)
    entries = collect_nullifiable_entries(
        raw_before,
        prefix=canon_path,
        skip_keys=skip_set,
        skip_at_root_only=True,
    )
    if expand_nullify_tree:
        expanded: list[ConfigPatch] = []
        for leaf_path, _old_val, new_null_val in entries:
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
            )
            expanded.append(
                _apply_atomic_set(working, leaf_patch, leaf_path, style_ledger)
            )
        return expanded

    rel_snapshot = collect_nullifiable_values(
        raw_before,
        prefix="",
        skip_keys=skip_set,
        skip_at_root_only=True,
    )
    applied = apply_nullify_tree_at_path(working, canon_path, skip_keys=skip_set)
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
        )
    ]


def _is_noop_patch(patch: ConfigPatch) -> bool:
    if patch.action == "delete":
        return patch.had_original is False
    if patch.action == "set":
        if patch.had_original is not True:
            return False
        if patch.original_style == "ref_only" and patch.new_value is None:
            return True
        return _strict_equal(patch.old_value, patch.new_value)
    if patch.action == "nullify_tree":
        return isinstance(patch.old_value, Mapping) and not patch.old_value
    return False


def _merge_same_path(first: ConfigPatch, second: ConfigPatch) -> ConfigPatch | None:
    """Coalesce two non-interfering patches on the exact same canonical path."""
    if second.action == "delete":
        if first.had_original is False:
            return None
        return ConfigPatch(
            path=first.path,
            action="delete",
            new_value=None,
            old_value=thaw_copy(first.old_value),
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
        )

    if second.action == "set":
        return ConfigPatch(
            path=first.path,
            action="set",
            new_value=thaw_copy(second.new_value),
            old_value=thaw_copy(first.old_value),
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
        )

    return None


def _coalesce_patches(patches: list[ConfigPatch]) -> list[ConfigPatch]:
    """Coalesce sequential same-path patches without crossing ancestor/descendant barriers."""
    result: list[ConfigPatch] = []
    result_tokens: list[tuple[str | int, ...]] = []

    for patch in patches:
        tokens = tuple(parse_path_tokens(patch.path))
        if patch.action == "nullify_tree":
            result.append(patch)
            result_tokens.append(tokens)
            continue

        merge_idx: int | None = None
        for idx in range(len(result) - 1, -1, -1):
            prev = result[idx]
            prev_tokens = result_tokens[idx]
            if prev_tokens == tokens and prev.action != "nullify_tree":
                merge_idx = idx
                break
            if _tokens_overlap(prev_tokens, tokens):
                # Ancestor or descendant barrier: do not merge past this point.
                break

        if merge_idx is None:
            result.append(patch)
            result_tokens.append(tokens)
            continue

        merged = _merge_same_path(result[merge_idx], patch)
        result.pop(merge_idx)
        result_tokens.pop(merge_idx)
        if merged is not None:
            result.append(merged)
            result_tokens.append(tokens)

    return result


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
        non-interfering barriers while preserving the initial ``old_value``.

    Returns
    -------
    list[ConfigPatch]
        Normalised patches carrying ``old_value``, ``had_original``,
        ``original_style``, and ``original_ref``.
    """
    if isinstance(target, ConfigDocument):
        base_data: Mapping[str, Any] = target.data
        style_ledger = target.style_ledger
    else:
        base_data = target
        style_ledger = StyleLedger.from_mapping(target)

    working = thaw_copy(base_data)
    recorded: list[ConfigPatch] = []

    for patch in patches:
        if patch.action not in _VALID_PATCH_ACTIONS:
            raise ValueError(
                f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
            )
        canon_path = canonicalise_path(patch.path)
        if patch.action == "set":
            recorded.append(_apply_atomic_set(working, patch, canon_path, style_ledger))
        elif patch.action == "delete":
            recorded.append(_apply_atomic_delete(working, patch, canon_path))
        elif patch.action == "nullify_tree":
            recorded.extend(
                _apply_atomic_nullify(
                    working,
                    patch,
                    canon_path,
                    style_ledger,
                    expand_nullify_tree=expand_nullify_tree,
                )
            )

    if coalesce:
        recorded = _coalesce_patches(recorded)
    if drop_noops:
        recorded = [patch for patch in recorded if not _is_noop_patch(patch)]
    return recorded


def invert_patches(patches: Sequence[ConfigPatch]) -> list[ConfigPatch]:
    """Construct the inverse patch sequence that undoes *patches*.

    Requires normalised atomic patches (``had_original`` populated and no
    unexpanded ``nullify_tree`` actions). Applying ``patches`` followed by
    ``invert_patches(patches)`` restores the original document values and
    ``RefValue`` / bare / ref-only styles.
    """
    inverse: list[ConfigPatch] = []
    for patch in reversed(patches):
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
        if patch.action == "set":
            if patch.had_original:
                inverse.append(
                    ConfigPatch(
                        path=patch.path,
                        action="set",
                        new_value=thaw_copy(patch.old_value),
                        old_value=thaw_copy(patch.new_value),
                        reason=f"Revert: {patch.reason}" if patch.reason else "Revert",
                        parameter=patch.parameter,
                        site_index=patch.site_index,
                        site_gridid=patch.site_gridid,
                        source=patch.source,
                        default_wrapped=default_wrapped,
                        had_original=True,
                        original_style=patch.original_style,
                        original_ref=patch.original_ref,
                    )
                )
            else:
                inverse.append(
                    ConfigPatch(
                        path=patch.path,
                        action="delete",
                        new_value=None,
                        old_value=thaw_copy(patch.new_value),
                        reason=f"Revert: {patch.reason}" if patch.reason else "Revert",
                        parameter=patch.parameter,
                        site_index=patch.site_index,
                        site_gridid=patch.site_gridid,
                        source=patch.source,
                        default_wrapped=default_wrapped,
                        had_original=True,
                    )
                )
        elif patch.action == "delete":
            if patch.had_original:
                inverse.append(
                    ConfigPatch(
                        path=patch.path,
                        action="set",
                        new_value=thaw_copy(patch.old_value),
                        old_value=None,
                        reason=f"Revert: {patch.reason}" if patch.reason else "Revert",
                        parameter=patch.parameter,
                        site_index=patch.site_index,
                        site_gridid=patch.site_gridid,
                        source=patch.source,
                        default_wrapped=default_wrapped,
                        had_original=False,
                        original_style=patch.original_style,
                        original_ref=patch.original_ref,
                    )
                )
        else:
            raise ValueError(
                f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
            )
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
