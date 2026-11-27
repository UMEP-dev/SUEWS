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

from collections import deque
from collections.abc import Mapping, Sequence, Set as AbstractSet
from copy import deepcopy
from dataclasses import dataclass, is_dataclass
import datetime
from decimal import Decimal
from enum import Enum
from fractions import Fraction
from pathlib import Path, PosixPath, PurePosixPath, PureWindowsPath, WindowsPath
import re
from types import MappingProxyType
from typing import TYPE_CHECKING, Any, Literal
from uuid import UUID
import weakref
from zoneinfo import ZoneInfo

import numpy as np
from pydantic import BaseModel

from ...core._yaml_io import safe_load_yaml
from ...core.field_renames import (
    RAW_YAML_FIELD_RENAMES,
    STEBBS_PHYSICS_LEAF_RENAMES,
    normalise_yaml_renames,
)
from ...core.physics_families import flatten_physics_in_config
from ...core.profile import (
    normalise_hourly_profile_key,
    normalise_hourly_profile_keys_in_raw_config,
)
from .style_ledger import (
    PathStyle,
    StyleLedger,
    WrapperStyle,
    _join_key_path,
    dump_yaml_with_style,
)

if TYPE_CHECKING:
    from ..rules.typed_view import TypedConfigView

_MISSING = object()


class _UnsetType:
    """Sentinel distinguishing an omitted raw payload from an explicit ``None``."""

    def __repr__(self) -> str:
        """Return a readable sentinel representation."""
        return "<UNSET>"

    def __deepcopy__(self, _memo: dict[int, Any]) -> _UnsetType:
        """Preserve sentinel identity across deep copies."""
        return self


_UNSET = _UnsetType()
_PATH_TOKEN_RE = re.compile(r"([^\.\[\]]+)|\[(\d+)\]")
_VALID_PATH_RE = re.compile(
    r"^(?:[^\.\[\]]+(?:\[\d+\])*)(?:\.[^\.\[\]]+(?:\[\d+\])*)*$"
)


@dataclass(frozen=True)
class MappingKey:
    """Explicit mapping-key path token preserving non-string keys and punctuation."""

    key: Any

    def __eq__(self, other: object) -> bool:
        """Compare key identity while keeping distinct Python key types separate."""
        if not isinstance(other, MappingKey):
            return NotImplemented
        if type(self.key) is not type(other.key):
            return False
        return self.key == other.key

    def __hash__(self) -> int:
        """Hash key identity consistently with :meth:`__eq__`."""
        return hash((type(self.key), self.key))


PathToken = str | int | MappingKey

PatchAction = Literal["set", "delete", "nullify_tree"]
_VALID_PATCH_ACTIONS = frozenset({"set", "delete", "nullify_tree"})


def make_key_token(key: Any) -> str | MappingKey:
    """Return a plain string token when unambiguous, or a :class:`MappingKey`."""
    if isinstance(key, str) and key and not any(ch in key for ch in ".[]"):
        return key
    return MappingKey(key)


def canonicalise_token(token: PathToken) -> PathToken:
    """Canonicalise equivalent mapping-key tokens so ``MappingKey('a') == 'a'``."""
    if isinstance(token, MappingKey):
        return make_key_token(token.key)
    return token


_EXACT_IMMUTABLE_TYPES: frozenset[type[Any]] = frozenset({
    type(None),
    bool,
    int,
    float,
    complex,
    str,
    bytes,
    range,
    Decimal,
    Fraction,
    UUID,
    PurePosixPath,
    PureWindowsPath,
    PosixPath,
    WindowsPath,
    datetime.datetime,
    datetime.date,
    datetime.time,
    datetime.timedelta,
    datetime.timezone,
    ZoneInfo,
    re.Pattern,
})

_NUMPY_IMMUTABLE_SCALARS = (
    np.number,
    np.bool_,
    np.character,
    np.datetime64,
    np.timedelta64,
)

_ENUM_INTERNAL_ATTRS = frozenset({
    "_name_",
    "_value_",
    "__objclass__",
    "_sort_order_",
    "_inverted_",
})

_FROZEN_ARRAY_STATE: dict[int, tuple[bytes, np.dtype[Any], tuple[int, ...]]] = {}
_FROZEN_SLICE_STATE: dict[int, tuple[Any, Any, Any]] = {}
_FROZEN_MAP_STATE: dict[int, dict[Any, Any]] = {}


def _is_exact_immutable_scalar(value: Any) -> bool:
    """Return True only for exact immutable primitive/stdlib/NumPy scalar types."""
    value_type = type(value)
    if value_type in _EXACT_IMMUTABLE_TYPES:
        return True
    return (
        isinstance(value, _NUMPY_IMMUTABLE_SCALARS) and value_type.__module__ == "numpy"
    )


def _enum_extra_attrs_immutable(value: Enum, seen: set[int]) -> bool:
    """Return True when all custom instance/slot attributes of *value* are immutable."""
    if hasattr(value, "__dict__") and any(
        not _is_deeply_immutable_payload(attr_val, seen)
        for attr_name, attr_val in vars(value).items()
        if attr_name not in _ENUM_INTERNAL_ATTRS
    ):
        return False
    for cls in type(value).__mro__:
        slots = cls.__dict__.get("__slots__")
        if not slots:
            continue
        slot_names = (slots,) if isinstance(slots, str) else slots
        for slot_name in slot_names:
            if slot_name in _ENUM_INTERNAL_ATTRS or slot_name in {
                "__dict__",
                "__weakref__",
            }:
                continue
            try:
                slot_val = getattr(value, slot_name)
            except AttributeError:
                continue
            if not _is_deeply_immutable_payload(slot_val, seen):
                return False
    return True


def _is_deeply_immutable_payload(value: Any, seen: set[int]) -> bool:
    """Return True when *value* contains no reachable mutable objects."""
    if isinstance(value, Enum):
        obj_id = id(value)
        if obj_id in seen:
            return True
        seen.add(obj_id)
        return _is_deeply_immutable_payload(
            value.value, seen
        ) and _enum_extra_attrs_immutable(value, seen)
    if _is_exact_immutable_scalar(value):
        return True
    if type(value) is slice:
        return all(
            _is_deeply_immutable_payload(bound, seen)
            for bound in (value.start, value.stop, value.step)
        )
    if type(value) in {tuple, frozenset}:
        obj_id = id(value)
        if obj_id in seen:
            return True
        seen.add(obj_id)
        return all(_is_deeply_immutable_payload(item, seen) for item in value)
    return False


def _is_immutable_enum(member: Enum) -> bool:
    """Return True when an ``Enum`` member carries only deeply immutable state."""
    return _is_deeply_immutable_payload(member, set())


class _FrozenArraySpec:
    """Immutable byte-backed specification materialised as fresh read-only views."""

    __slots__ = ("__weakref__",)

    def __init__(self, array: np.ndarray[Any, Any]) -> None:
        key = id(self)
        _FROZEN_ARRAY_STATE[key] = (
            array.tobytes(),
            deepcopy(array.dtype),
            tuple(array.shape),
        )
        weakref.finalize(self, _FROZEN_ARRAY_STATE.pop, key, None)

    def materialise(self) -> np.ndarray[Any, Any]:
        """Return a fresh zero-copy read-only NumPy view over immutable bytes."""
        buf, dtype, shape = _FROZEN_ARRAY_STATE[id(self)]
        return np.frombuffer(buf, dtype=deepcopy(dtype)).reshape(shape)

    def __copy__(self) -> _FrozenArraySpec:
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> _FrozenArraySpec:
        return self

    def __eq__(self, other: object) -> bool:
        if isinstance(other, _FrozenArraySpec):
            return _FROZEN_ARRAY_STATE[id(self)] == _FROZEN_ARRAY_STATE[id(other)]
        return NotImplemented

    def __hash__(self) -> int:
        return hash(_FROZEN_ARRAY_STATE[id(self)])

    def __setattr__(self, name: str, value: Any) -> None:
        raise TypeError("Cannot mutate _FrozenArraySpec")

    def __delattr__(self, name: str) -> None:
        raise TypeError("Cannot mutate _FrozenArraySpec")


class _FrozenSliceSpec:
    """Immutable specification for a ``slice`` holding lazy array or slice specs."""

    __slots__ = ("__weakref__",)

    def __init__(self, start: Any, stop: Any, step: Any) -> None:
        key = id(self)
        _FROZEN_SLICE_STATE[key] = (start, stop, step)
        weakref.finalize(self, _FROZEN_SLICE_STATE.pop, key, None)

    def materialise(self) -> slice:
        """Return a fresh ``slice`` with materialised read-only bounds."""
        start, stop, step = _FROZEN_SLICE_STATE[id(self)]
        return slice(
            _materialise_frozen(start),
            _materialise_frozen(stop),
            _materialise_frozen(step),
        )

    def __copy__(self) -> _FrozenSliceSpec:
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> _FrozenSliceSpec:
        return self

    def __eq__(self, other: object) -> bool:
        if isinstance(other, _FrozenSliceSpec):
            return _FROZEN_SLICE_STATE[id(self)] == _FROZEN_SLICE_STATE[id(other)]
        return NotImplemented

    def __hash__(self) -> int:
        return hash(_FROZEN_SLICE_STATE[id(self)])

    def __setattr__(self, name: str, value: Any) -> None:
        raise TypeError("Cannot mutate _FrozenSliceSpec")

    def __delattr__(self, name: str) -> None:
        raise TypeError("Cannot mutate _FrozenSliceSpec")


_LAZY_FROZEN_SPECS = (_FrozenArraySpec, _FrozenSliceSpec)


def _materialise_frozen(value: Any) -> Any:
    """Materialise a lazy frozen spec into a fresh read-only view."""
    if isinstance(value, _LAZY_FROZEN_SPECS):
        return value.materialise()
    return value


def _freeze_slice_value(value: slice, freeze_fn: Any) -> slice | _FrozenSliceSpec:
    """Recursively freeze the bounds of *value* using *freeze_fn*."""
    start = freeze_fn(value.start)
    stop = freeze_fn(value.stop)
    step = freeze_fn(value.step)
    if any(isinstance(bound, _LAZY_FROZEN_SPECS) for bound in (start, stop, step)):
        return _FrozenSliceSpec(start, stop, step)
    if start is value.start and stop is value.stop and step is value.step:
        return value
    return slice(start, stop, step)


class _ReadOnlyMaterialisingMapping(Mapping[Any, Any]):
    """Read-only mapping that materialises fresh NumPy views on every lookup."""

    __slots__ = ("__weakref__",)
    __hash__ = None  # type: ignore[assignment]

    def __init__(self, entries: dict[Any, Any]) -> None:
        key = id(self)
        _FROZEN_MAP_STATE[key] = entries
        weakref.finalize(self, _FROZEN_MAP_STATE.pop, key, None)

    def __getitem__(self, key: Any) -> Any:
        return _materialise_frozen(_FROZEN_MAP_STATE[id(self)][key])

    def __iter__(self) -> Any:
        return iter(_FROZEN_MAP_STATE[id(self)])

    def __len__(self) -> int:
        return len(_FROZEN_MAP_STATE[id(self)])

    def __contains__(self, key: object) -> bool:
        return key in _FROZEN_MAP_STATE[id(self)]

    def __repr__(self) -> str:
        return repr({
            k: _materialise_frozen(v) for k, v in _FROZEN_MAP_STATE[id(self)].items()
        })

    def __eq__(self, other: object) -> bool:
        if isinstance(other, _ReadOnlyMaterialisingMapping):
            return _FROZEN_MAP_STATE[id(self)] == _FROZEN_MAP_STATE[id(other)]
        return super().__eq__(other)

    def __copy__(self) -> _ReadOnlyMaterialisingMapping:
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> _ReadOnlyMaterialisingMapping:
        return self

    def __setattr__(self, name: str, value: Any) -> None:
        raise TypeError(f"Cannot mutate read-only mapping attribute {name!r}")

    def __delattr__(self, name: str) -> None:
        raise TypeError(f"Cannot delete read-only mapping attribute {name!r}")


class _ReadOnlyMaterialisingTuple(tuple):  # type: ignore[type-arg]
    """Tuple subclass that materialises fresh NumPy views on every access."""

    __slots__ = ()
    __hash__ = None  # type: ignore[assignment]

    def __getitem__(self, index: Any) -> Any:
        if isinstance(index, slice):
            sliced = super().__getitem__(index)
            if any(isinstance(item, _LAZY_FROZEN_SPECS) for item in sliced):
                return _ReadOnlyMaterialisingTuple(sliced)
            return sliced
        return _materialise_frozen(super().__getitem__(index))

    def __iter__(self) -> Any:
        for item in super().__iter__():
            yield _materialise_frozen(item)

    def __reversed__(self) -> Any:
        for idx in range(len(self) - 1, -1, -1):
            yield _materialise_frozen(super().__getitem__(idx))

    def __copy__(self) -> _ReadOnlyMaterialisingTuple:
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> _ReadOnlyMaterialisingTuple:
        return self

    def __contains__(self, value: object) -> bool:
        return any(item == value for item in self)

    def __repr__(self) -> str:
        return repr(tuple(self))

    def __eq__(self, other: object) -> bool:
        if isinstance(other, _ReadOnlyMaterialisingTuple):
            return super().__eq__(other)
        if not isinstance(other, tuple):
            return NotImplemented
        if len(self) != len(other):
            return False
        return all(a == b for a, b in zip(self, other))

    def __ne__(self, other: object) -> bool:
        eq = self.__eq__(other)
        return NotImplemented if eq is NotImplemented else not eq

    def __add__(self, other: Any) -> Any:
        if not isinstance(other, tuple):
            return NotImplemented
        return tuple(self) + tuple(other)

    def __radd__(self, other: Any) -> Any:
        if not isinstance(other, tuple):
            return NotImplemented
        return tuple(other) + tuple(self)

    def __mul__(self, count: Any) -> Any:
        if not isinstance(count, int):
            return NotImplemented
        return tuple(self) * count

    def __rmul__(self, count: Any) -> Any:
        return self.__mul__(count)

    def index(self, value: Any, start: int = 0, stop: int | None = None) -> int:
        materialised = tuple(self)
        return (
            materialised.index(value, start)
            if stop is None
            else materialised.index(value, start, stop)
        )

    def count(self, value: Any) -> int:
        return tuple(self).count(value)

    def __setattr__(self, name: str, value: Any) -> None:
        raise TypeError(f"Cannot mutate read-only sequence attribute {name!r}")

    def __delattr__(self, name: str) -> None:
        raise TypeError(f"Cannot delete read-only sequence attribute {name!r}")


def _copy_payload(value: Any) -> Any:
    """Defensively copy *value* while preserving tuples and thawing read-only mappings."""
    if value is None or type(value) in {bool, int, float, str, bytes}:
        return value
    if isinstance(value, Mapping):
        return {_copy_payload(k): _copy_payload(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_copy_payload(item) for item in value]
    if isinstance(value, tuple):
        return tuple(_copy_payload(item) for item in value)
    if isinstance(value, set):
        return {_copy_payload(item) for item in value}
    if isinstance(value, frozenset):
        return frozenset(_copy_payload(item) for item in value)
    return deepcopy(value)


def _freeze_mapping_entries(entries: dict[Any, Any]) -> Mapping[Any, Any]:
    if any(isinstance(v, _LAZY_FROZEN_SPECS) for v in entries.values()):
        return _ReadOnlyMaterialisingMapping(entries)
    return MappingProxyType(entries)


def _freeze_sequence_items(items: list[Any]) -> tuple[Any, ...]:
    if any(isinstance(item, _LAZY_FROZEN_SPECS) for item in items):
        return _ReadOnlyMaterialisingTuple(items)
    return tuple(items)


def _freeze_numpy_value(value: np.ndarray[Any, Any] | np.void) -> Any:
    if isinstance(value, np.void):
        if value.dtype.names:
            return _freeze_mapping_entries({
                name: _deep_freeze_internal(value[name]) for name in value.dtype.names
            })
        return bytes(value)
    if value.dtype.hasobject:
        if value.ndim == 0:
            return _deep_freeze_internal(deepcopy(value.item()))
        return _deep_freeze_internal(deepcopy(value.tolist()))
    return _FrozenArraySpec(value)


def _freeze_model_or_object(value: Any) -> Any:
    """Freeze byte buffers, models, dataclasses, or already-frozen proxies, or reject."""
    if type(value) in {bytearray, memoryview}:
        return bytes(value)
    from ..rules.readonly_model import (  # noqa: PLC0415
        ReadOnlyDataclassProxy,
        ReadOnlyEnumProxy,
        ReadOnlyModelProxy,
        freeze_model,
    )

    if isinstance(
        value, (ReadOnlyModelProxy, ReadOnlyDataclassProxy, ReadOnlyEnumProxy)
    ):
        return value
    if isinstance(value, BaseModel):
        return freeze_model(value)
    if is_dataclass(value) and not isinstance(value, type):
        return ReadOnlyDataclassProxy(value)
    raise TypeError(
        f"deep_freeze cannot safely freeze mutable object of type {type(value).__name__!r}"
    )


def _freeze_container_value(value: Any) -> Any:
    """Freeze containers, NumPy values, models, dataclasses, or reject unsupported types."""
    if isinstance(value, Mapping):
        return _freeze_mapping_entries({
            _deep_freeze_internal(k): _deep_freeze_internal(v) for k, v in value.items()
        })
    if isinstance(value, (list, tuple, deque, Sequence)) and not isinstance(
        value, (str, bytes, bytearray, memoryview)
    ):
        return _freeze_sequence_items([_deep_freeze_internal(item) for item in value])
    if isinstance(value, (set, frozenset, AbstractSet)):
        return frozenset(_deep_freeze_internal(item) for item in value)
    if isinstance(value, (np.ndarray, np.void)):
        return _freeze_numpy_value(value)
    return _freeze_model_or_object(value)


def _deep_freeze_internal(value: Any) -> Any:
    """Recursively freeze values, keeping NumPy arrays as lazy :class:`_FrozenArraySpec`."""
    if isinstance(value, Enum):
        from ..rules.readonly_model import ReadOnlyEnumProxy  # noqa: PLC0415

        return ReadOnlyEnumProxy(value)
    if type(value) is slice:
        return _freeze_slice_value(value, _deep_freeze_internal)
    if _is_exact_immutable_scalar(value) or isinstance(
        value,
        (
            *_LAZY_FROZEN_SPECS,
            _ReadOnlyMaterialisingMapping,
            _ReadOnlyMaterialisingTuple,
        ),
    ):
        return value
    return _freeze_container_value(value)


def deep_freeze(value: Any) -> Any:
    """Recursively freeze mappings, sequences, sets, arrays, and models into read-only objects."""
    return _materialise_frozen(_deep_freeze_internal(value))


def _thaw_non_container(value: Any) -> Any:
    """Thaw slices, lazy frozen specs, read-only proxies, or fall back to ``deepcopy``."""
    if isinstance(value, _LAZY_FROZEN_SPECS):
        return thaw_copy(value.materialise())
    if type(value) is slice:
        return slice(
            thaw_copy(value.start),
            thaw_copy(value.stop),
            thaw_copy(value.step),
        )
    from ..rules.readonly_model import (  # noqa: PLC0415
        ReadOnlyDataclassProxy,
        ReadOnlyEnumProxy,
        ReadOnlyModelProxy,
    )

    if isinstance(
        value, (ReadOnlyModelProxy, ReadOnlyDataclassProxy, ReadOnlyEnumProxy)
    ):
        return value.unwrap_copy()
    return deepcopy(value)


def thaw_copy(value: Any) -> Any:
    """Recursively copy *value*, thawing frozen mappings, sequences, and model/dataclass proxies."""
    if value is None or type(value) in {bool, int, float, str, bytes}:
        return value
    if isinstance(value, Mapping):
        return {k: thaw_copy(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [thaw_copy(item) for item in value]
    if isinstance(value, set):
        return {thaw_copy(item) for item in value}
    if isinstance(value, frozenset):
        return frozenset(thaw_copy(item) for item in value)
    return _thaw_non_container(value)


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


def _build_new_entry(
    new_value: Any,
    *,
    default_wrapped: bool,
    wrapper_style: WrapperStyle | None = None,
    ref_metadata: Mapping[str, Any] | None = None,
    has_ref: bool = False,
) -> Any:
    """Construct a new YAML entry honouring recorded authoring style and ``ref`` metadata."""
    copied = thaw_copy(new_value)
    if wrapper_style == "bare":
        return copied
    if (
        wrapper_style == "ref_only"
        and copied is None
        and (ref_metadata is not None or has_ref)
    ):
        return {"ref": thaw_copy(ref_metadata) if ref_metadata is not None else None}
    if wrapper_style in {"ref_value", "ref_only"}:
        entry: dict[str, Any] = {"value": copied}
        if ref_metadata is not None:
            entry["ref"] = thaw_copy(ref_metadata)
        elif has_ref:
            entry["ref"] = None
        return entry
    if default_wrapped:
        return {"value": copied}
    return copied


def _update_existing_entry(
    existing: Any,
    new_value: Any,
    *,
    original_style: WrapperStyle | None = None,
    original_ref: Mapping[str, Any] | None = None,
    has_ref: bool = False,
) -> Any:
    """Update an existing bare, ``RefValue``, or ref-only entry preserving metadata."""
    copied = thaw_copy(new_value)
    if copied is None and (
        original_style == "ref_only"
        or (original_style is None and is_ref_only(existing))
    ):
        if isinstance(existing, Mapping) and "ref" in existing:
            return {"ref": thaw_copy(existing["ref"])}
        if original_ref is not None or has_ref:
            return {
                "ref": thaw_copy(original_ref) if original_ref is not None else None
            }
    if isinstance(existing, dict) and (
        is_ref_wrapper(existing) or is_ref_only(existing)
    ):
        existing["value"] = copied
        return existing
    if isinstance(existing, Mapping) and (
        is_ref_wrapper(existing) or is_ref_only(existing)
    ):
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
    wrapper_style: WrapperStyle | None = None,
    ref_metadata: Mapping[str, Any] | None = None,
    has_ref: bool = False,
) -> None:
    """Update ``container[key]`` while preserving ``RefValue`` metadata or bare style.

    - Existing ``RefValue`` or ref-only ``dict`` entries (e.g.
      ``{"value": old, "ref": ...}`` or ``{"ref": ...}``) update ``"value"``
      in place so ``"ref"`` metadata is preserved.
    - Existing read-only ``Mapping`` entries are replaced with a thawed dict
      copy carrying the updated ``"value"``.
    - Existing non-mapping entries (bare scalars, lists, or ``None``) stay bare.
    - Absent keys consult *wrapper_style* and *ref_metadata* (from a
      :class:`StyleLedger` or normalised patch) when provided, otherwise falling
      back to ``{"value": new_value}`` when ``default_wrapped=True`` or bare
      ``new_value``.
    """
    if key in container:
        existing = container[key]
        updated = _update_existing_entry(
            existing,
            new_value,
            original_style=wrapper_style,
            original_ref=ref_metadata,
            has_ref=has_ref,
        )
        if updated is not existing:
            container[key] = updated
    else:
        container[key] = _build_new_entry(
            new_value,
            default_wrapped=default_wrapped,
            wrapper_style=wrapper_style,
            ref_metadata=ref_metadata,
            has_ref=has_ref,
        )


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


def format_path_tokens(tokens: Sequence[PathToken]) -> str:
    """Format parsed path tokens back into a canonical dotted/indexed path string."""
    if not tokens:
        raise ValueError("Path tokens cannot be empty")
    parts: list[str] = []
    for token in tokens:
        if isinstance(token, MappingKey):
            seg = f"[{token.key!r}]"
            if not parts:
                parts.append(seg)
            else:
                parts[-1] = f"{parts[-1]}{seg}"
        elif isinstance(token, int) and not isinstance(token, bool):
            if not parts:
                parts.append(f"[{token}]")
            else:
                parts[-1] = f"{parts[-1]}[{token}]"
        elif isinstance(token, str) and token:
            parts.append(token)
        else:
            raise ValueError(f"Invalid path token: {token!r}")
    return ".".join(parts)


def canonicalise_path(path: str) -> str:
    """Validate and return the canonical string form of *path*."""
    return format_path_tokens(parse_path_tokens(path))


def _step_read(current: Any, token: PathToken) -> Any:
    if isinstance(token, MappingKey):
        if isinstance(current, Mapping) and token.key in current:
            return current[token.key]
        return _MISSING
    if isinstance(token, int):
        if isinstance(current, (list, tuple)) and 0 <= token < len(current):
            return current[token]
        return _MISSING
    if isinstance(current, Mapping) and token in current:
        return current[token]
    return _MISSING


def read_raw_tokens(
    data: Any, tokens: Sequence[PathToken], default: Any = _MISSING
) -> Any:
    """Resolve *tokens* inside *data* and return the raw entry without unwrapping."""
    current: Any = data
    for token in tokens:
        current = _step_read(current, token)
        if current is _MISSING:
            return default
    return current


def read_raw_path(data: Any, path: str, default: Any = _MISSING) -> Any:
    """Resolve *path* inside *data* and return the raw entry without unwrapping ``RefValue``."""
    try:
        tokens = parse_path_tokens(path)
    except ValueError:
        return default
    return read_raw_tokens(data, tokens, default=default)


def path_exists(data: Any, path: str) -> bool:
    """Return True when *path* resolves to an existing entry in *data* (even if ``None``)."""
    return read_raw_path(data, path, default=_MISSING) is not _MISSING


def read_path(data: Any, path: str, default: Any = None) -> Any:
    """Resolve a dotted/indexed *path* inside *data*, unwrapping a leaf ``RefValue``."""
    raw = read_raw_path(data, path, default=_MISSING)
    if raw is _MISSING:
        return default
    return unwrap_entry(raw, default)


def _is_list_index_token(token: PathToken) -> bool:
    return isinstance(token, int) and not isinstance(token, bool)


def _mapping_key_from_token(token: PathToken) -> Any:
    return token.key if isinstance(token, MappingKey) else token


def _can_resolve_parent(
    data: dict[str, Any], tokens: Sequence[PathToken], *, create_missing: bool
) -> bool:
    probe: Any = data
    creating = False
    for pos, token in enumerate(tokens[:-1]):
        next_is_list_idx = _is_list_index_token(tokens[pos + 1])
        if creating:
            if _is_list_index_token(token) or next_is_list_idx:
                return False
            continue
        if _is_list_index_token(token):
            assert isinstance(token, int)
            if not isinstance(probe, list) or not (0 <= token < len(probe)):
                return False
            probe = probe[token]
            continue
        if not isinstance(probe, dict):
            return False
        raw_key = _mapping_key_from_token(token)
        if raw_key not in probe or probe[raw_key] is None:
            if not create_missing or next_is_list_idx:
                return False
            creating = True
        else:
            probe = probe[raw_key]
    return not (creating and _is_list_index_token(tokens[-1]))


def find_first_created_ancestor(
    data: Mapping[str, Any], tokens: Sequence[PathToken]
) -> tuple[tuple[PathToken, ...], bool] | None:
    """Return ``(ancestor_tokens, was_none)`` for the shallowest missing/null ancestor.

    Returns ``None`` when all parent containers already exist and are non-null.
    """
    current: Any = data
    for pos, token in enumerate(tokens[:-1]):
        if _is_list_index_token(token):
            assert isinstance(token, int)
            if not isinstance(current, (list, tuple)) or not (
                0 <= token < len(current)
            ):
                return None
            current = current[token]
            continue
        if not isinstance(current, Mapping):
            return None
        raw_key = _mapping_key_from_token(token)
        if raw_key not in current:
            return tuple(tokens[: pos + 1]), False
        if current[raw_key] is None:
            return tuple(tokens[: pos + 1]), True
        current = current[raw_key]
    return None


def _ensure_parent_container(
    data: dict[str, Any], tokens: Sequence[PathToken], *, create_missing: bool
) -> tuple[Any, PathToken] | None:
    """Walk *data* to the parent container of *tokens[-1]* without partial mutations."""
    if not tokens or not _can_resolve_parent(
        data, tokens, create_missing=create_missing
    ):
        return None
    current: Any = data
    for token in tokens[:-1]:
        if _is_list_index_token(token):
            assert isinstance(token, int)
            current = current[token]
        else:
            raw_key = _mapping_key_from_token(token)
            if raw_key not in current or current[raw_key] is None:
                current[raw_key] = {}
            current = current[raw_key]
    return current, tokens[-1]


def write_tokens(
    data: dict[str, Any],
    tokens: Sequence[PathToken],
    new_value: Any,
    *,
    default_wrapped: bool = True,
    create_missing: bool = True,
    style_ledger: StyleLedger | None = None,
    original_style: WrapperStyle | None = None,
    original_ref: Mapping[str, Any] | None = None,
    has_ref: bool = False,
    replace_raw: bool = False,
) -> bool:
    """Write *new_value* at *tokens* in *data*, preserving style or replacing raw."""
    canon_path = format_path_tokens(tokens)
    located = _ensure_parent_container(data, tokens, create_missing=create_missing)
    if located is None:
        return False
    parent, leaf = located
    if replace_raw:
        if _is_list_index_token(leaf):
            assert isinstance(leaf, int)
            if not isinstance(parent, list) or not (0 <= leaf < len(parent)):
                return False
            parent[leaf] = _copy_payload(new_value)
            return True
        if not isinstance(parent, dict):
            return False
        parent[_mapping_key_from_token(leaf)] = _copy_payload(new_value)
        return True

    effective_style = original_style
    effective_ref = original_ref
    effective_has_ref = has_ref or effective_ref is not None
    if style_ledger is not None:
        if effective_style is None:
            effective_style = style_ledger.wrapper_style(canon_path)
        if effective_ref is None:
            effective_ref = style_ledger.ref_metadata(canon_path)
        if not effective_has_ref:
            effective_has_ref = style_ledger.has_ref_key(canon_path)
    if _is_list_index_token(leaf):
        assert isinstance(leaf, int)
        if not isinstance(parent, list) or not (0 <= leaf < len(parent)):
            return False
        existing = parent[leaf]
        updated = _update_existing_entry(
            existing,
            new_value,
            original_style=effective_style,
            original_ref=effective_ref,
            has_ref=effective_has_ref,
        )
        if updated is not existing:
            parent[leaf] = updated
        return True
    if not isinstance(parent, dict):
        return False
    write_scalar(
        parent,
        _mapping_key_from_token(leaf),
        new_value,
        default_wrapped=default_wrapped,
        wrapper_style=effective_style,
        ref_metadata=effective_ref,
        has_ref=effective_has_ref,
    )
    return True


def write_path(
    data: dict[str, Any],
    path: str,
    new_value: Any,
    *,
    default_wrapped: bool = True,
    create_missing: bool = True,
    style_ledger: StyleLedger | None = None,
    original_style: WrapperStyle | None = None,
    original_ref: Mapping[str, Any] | None = None,
    replace_raw: bool = False,
) -> bool:
    """Write *new_value* at *path* in *data*, preserving bare vs ``RefValue`` style."""
    tokens = parse_path_tokens(path)
    return write_tokens(
        data,
        tokens,
        new_value,
        default_wrapped=default_wrapped,
        create_missing=create_missing,
        style_ledger=style_ledger,
        original_style=original_style,
        original_ref=original_ref,
        replace_raw=replace_raw,
    )


def delete_path(data: dict[str, Any], path: str) -> bool:
    """Delete the leaf key at *path* in *data* if present."""
    tokens = parse_path_tokens(path)
    located = _ensure_parent_container(data, tokens, create_missing=False)
    if located is None:
        return False
    parent, leaf = located
    if not _is_list_index_token(leaf) and isinstance(parent, dict):
        raw_key = _mapping_key_from_token(leaf)
        if raw_key in parent:
            del parent[raw_key]
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
                if key == "ref" or (
                    key in skip_keys and (is_root or not skip_at_root_only)
                ):
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


def apply_nullify_tree_at_tokens(
    target: dict[str, Any],
    tokens: Sequence[PathToken],
    *,
    skip_keys: frozenset[str] = frozenset(),
) -> bool:
    """Apply ``nullify_tree_in_place`` at *tokens* (supporting dict keys and list indices)."""
    located = _ensure_parent_container(target, tokens, create_missing=False)
    if located is None:
        return False
    parent, leaf = located
    if not _is_list_index_token(leaf):
        raw_key = _mapping_key_from_token(leaf)
        if not isinstance(parent, dict) or raw_key not in parent:
            return False
        node = parent[raw_key]
        if isinstance(node, (dict, list)):
            nullify_tree_in_place(node, skip_keys=skip_keys)
        elif node is not None:
            parent[raw_key] = None
        return True
    assert isinstance(leaf, int)
    if not isinstance(parent, list) or not (0 <= leaf < len(parent)):
        return False
    node = parent[leaf]
    if isinstance(node, (dict, list)):
        nullify_tree_in_place(node, skip_keys=skip_keys)
    elif node is not None:
        parent[leaf] = None
    return True


def apply_nullify_tree_at_path(
    target: dict[str, Any],
    path: str,
    *,
    skip_keys: frozenset[str] = frozenset(),
) -> bool:
    """Apply ``nullify_tree_in_place`` at *path* (supporting dict keys and list indices)."""
    return apply_nullify_tree_at_tokens(
        target, parse_path_tokens(path), skip_keys=skip_keys
    )


@dataclass(frozen=True)
class ConfigPatch:
    """Declarative mutation planned against a :class:`ConfigDocument`.

    Pure planners (Phase B scientific suggestions, Phase A normalisations)
    return ``list[ConfigPatch]`` so suggest mode never needs to deep-copy and
    discard mutated trees, and apply mode applies the exact same patch list.
    When normalised via :func:`normalise_patches`, every patch carries its
    original value (``old_value``), existence flag (``had_original``),
    authoring wrapper style (``original_style``), frozen ``original_ref``
    metadata, exact raw pre-patch entry (``original_raw``), and any implicitly
    created ancestor (``created_ancestor``) so conflicts, oscillation checks,
    and exact rollbacks are well-defined.
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
    had_original: bool | None = None
    original_style: WrapperStyle | None = None
    original_ref: Mapping[str, Any] | None = None
    path_tokens: tuple[PathToken, ...] | None = None
    original_raw: Any = _UNSET
    replace_raw: bool = False
    created_ancestor: tuple[tuple[PathToken, ...], bool] | None = None
    write_style: WrapperStyle | None = None
    write_ref: Mapping[str, Any] | None = None
    write_has_ref: bool = False

    def __post_init__(self) -> None:
        """Defensively copy mutable payloads and freeze ``original_ref`` metadata."""
        object.__setattr__(self, "new_value", _copy_payload(self.new_value))
        object.__setattr__(self, "old_value", _copy_payload(self.old_value))
        if self.original_ref is not None:
            object.__setattr__(self, "original_ref", deep_freeze(self.original_ref))
        if self.write_ref is not None:
            object.__setattr__(self, "write_ref", deep_freeze(self.write_ref))
            if not self.write_has_ref:
                object.__setattr__(self, "write_has_ref", True)
        if self.path_tokens is not None:
            object.__setattr__(
                self,
                "path_tokens",
                tuple(canonicalise_token(tok) for tok in self.path_tokens),
            )
        if self.original_raw is not _UNSET:
            object.__setattr__(self, "original_raw", _copy_payload(self.original_raw))


def apply_patches_in_place(
    target: dict[str, Any],
    patches: Sequence[ConfigPatch],
    *,
    style_ledger: StyleLedger | None = None,
) -> dict[str, Any]:
    """Apply a sequence of :class:`ConfigPatch` mutations to *target* in place.

    Shared by :meth:`ConfigDocument.apply_patches`, :meth:`PatchPlan.apply_to_dict`,
    and selective suggestion application so all entry points enforce identical
    validation semantics:

    - ``set`` raises :class:`ValueError` if the path cannot be written.
    - ``delete`` raises :class:`ValueError` if the parent container does not
      exist or is not a mapping / leaf is not a mapping key; deleting a missing
      key from an existing mapping parent is an idempotent no-op.
    - ``nullify_tree`` raises :class:`ValueError` if the path cannot be resolved.
    """
    for patch in patches:
        if patch.action not in _VALID_PATCH_ACTIONS:
            raise ValueError(
                f"Unsupported patch action {patch.action!r} at path {patch.path!r}"
            )
        tokens: tuple[PathToken, ...] = (
            patch.path_tokens
            if patch.path_tokens is not None
            else tuple(parse_path_tokens(patch.path))
        )
        if patch.action == "set":
            if (
                patch.write_style is not None
                or patch.write_ref is not None
                or patch.write_has_ref
            ):
                eff_style = patch.write_style
                eff_ref = patch.write_ref
                eff_has_ref = patch.write_has_ref or eff_ref is not None
            elif patch.had_original is None:
                eff_style = patch.original_style
                eff_ref = patch.original_ref
                eff_has_ref = eff_ref is not None
            else:
                eff_style = None
                eff_ref = None
                eff_has_ref = False
            applied = write_tokens(
                target,
                tokens,
                patch.new_value,
                default_wrapped=patch.default_wrapped,
                style_ledger=style_ledger,
                original_style=eff_style,
                original_ref=eff_ref,
                has_ref=eff_has_ref,
                replace_raw=patch.replace_raw,
            )
            if not applied:
                raise ValueError(f"Failed to apply 'set' patch at path {patch.path!r}")
        elif patch.action == "delete":
            located = _ensure_parent_container(target, tokens, create_missing=False)
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
            parent.pop(_mapping_key_from_token(leaf), None)
        elif patch.action == "nullify_tree":
            applied = apply_nullify_tree_at_tokens(
                target,
                tokens,
                skip_keys=frozenset(patch.skip_keys),
            )
            if not applied:
                raise ValueError(
                    f"Failed to apply 'nullify_tree' patch at path {patch.path!r}"
                )
    return target


class ConfigDocument:
    """Canonical in-memory representation of a SUEWS YAML configuration.

    Holds the parsed mapping alongside its immutable :class:`StyleLedger`,
    optional original text, and source path, providing style-preserving reads,
    GRIDID maps, patch normalisation and application, read-only typed views,
    and style-preserving YAML serialisation.
    """

    def __init__(
        self,
        data: dict[str, Any],
        *,
        source_path: str | Path | None = None,
        raw_text: str | None = None,
        style_ledger: StyleLedger | None = None,
    ) -> None:
        self.data: dict[str, Any] = data
        self.source_path: Path | None = (
            Path(source_path) if source_path is not None else None
        )
        self.raw_text: str | None = raw_text
        if style_ledger is not None:
            self.style_ledger: StyleLedger = style_ledger
        elif raw_text is not None:
            self.style_ledger = StyleLedger.from_yaml_text(raw_text, data=data)
        else:
            self.style_ledger = StyleLedger.from_mapping(data)

    @classmethod
    def from_text(
        cls,
        raw_text: str,
        *,
        source_path: str | Path | None = None,
        normalise: bool = True,
        flatten_physics: bool = False,
    ) -> ConfigDocument:
        """Load YAML text into a :class:`ConfigDocument`, capturing its :class:`StyleLedger`."""
        loaded = safe_load_yaml(raw_text)
        if not isinstance(loaded, dict):
            raise ValueError("YAML root is not a mapping")
        ledger = StyleLedger.from_yaml_text(raw_text, data=loaded)
        return cls.from_dict(
            loaded,
            source_path=source_path,
            raw_text=raw_text,
            normalise=normalise,
            flatten_physics=flatten_physics,
            style_ledger=ledger,
        )

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
        return cls.from_text(
            raw_text,
            source_path=file_path,
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
        style_ledger: StyleLedger | None = None,
    ) -> ConfigDocument:
        """Construct a :class:`ConfigDocument` from an in-memory mapping."""
        working = thaw_copy(data)
        if style_ledger is not None:
            ledger = style_ledger
        elif raw_text is not None:
            ledger = StyleLedger.from_yaml_text(raw_text, data=working)
        else:
            ledger = StyleLedger.from_mapping(working)

        if normalise:
            working = normalise_yaml_renames(working)
            ledger = ledger.remap_leaf_keys(RAW_YAML_FIELD_RENAMES)
            stebbs_relocations = _collect_stebbs_physics_relocations(working, ledger)
            if stebbs_relocations:
                ledger = ledger.remap_paths(stebbs_relocations)
            pre_profile = thaw_copy(working)
            working, profile_changed = normalise_hourly_profile_keys_in_raw_config(
                working
            )
            if profile_changed:
                profile_renames, parent_key_renames = (
                    _collect_mapping_key_renames_with_orders(pre_profile, working)
                )
                if profile_renames or parent_key_renames:
                    ledger = ledger.remap_paths(
                        profile_renames, parent_key_renames=parent_key_renames
                    )
        if flatten_physics:
            flatten_physics_in_config(working)
        return cls(
            working,
            source_path=source_path,
            raw_text=raw_text,
            style_ledger=ledger,
        )

    def clone(self) -> ConfigDocument:
        """Return an independent deep copy of this document preserving its style ledger."""
        return ConfigDocument(
            thaw_copy(self.data),
            source_path=self.source_path,
            raw_text=self.raw_text,
            style_ledger=self.style_ledger,
        )

    def gridid_map(self) -> dict[int, Any]:
        """Return ``{site_index: gridiv}`` for every site in the document."""
        return site_gridid_map(self.data)

    def read(self, path: str, default: Any = None) -> Any:
        """Read the scalar or subtree at *path*, unwrapping leaf ``RefValue``."""
        return read_path(self.data, path, default=default)

    def frozen_data(self) -> Mapping[str, Any]:
        """Return a deeply immutable ``MappingProxyType`` snapshot of ``self.data``."""
        return deep_freeze(self.data)

    def typed_view(self) -> TypedConfigView:
        """Return a read-only :class:`TypedConfigView` over this document."""
        from ..rules.typed_view import TypedConfigView  # noqa: PLC0415

        return TypedConfigView.from_document(self)

    def normalise_patches(
        self,
        patches: Sequence[ConfigPatch],
        *,
        expand_nullify_tree: bool = True,
        drop_noops: bool = True,
        coalesce: bool = True,
    ) -> list[ConfigPatch]:
        """Normalise *patches* against this document so every patch carries original values."""
        from .patch_normaliser import normalise_patches  # noqa: PLC0415

        return normalise_patches(
            patches,
            self,
            expand_nullify_tree=expand_nullify_tree,
            drop_noops=drop_noops,
            coalesce=coalesce,
        )

    def apply_patches(
        self,
        patches: Sequence[ConfigPatch],
        *,
        normalise: bool = False,
    ) -> ConfigDocument:
        """Return a new :class:`ConfigDocument` with *patches* applied in order."""
        effective_patches = self.normalise_patches(patches) if normalise else patches
        target = self.clone()
        apply_patches_in_place(
            target.data, effective_patches, style_ledger=self.style_ledger
        )
        target.raw_text = None
        return target

    def dump_yaml(
        self,
        *,
        header: str | None = None,
        preserve_style: bool = True,
        preserve_leading_comments: bool = False,
    ) -> str:
        """Serialise the document mapping to YAML text with optional style preservation."""
        if preserve_style:
            return dump_yaml_with_style(
                self.data,
                self.style_ledger,
                header=header,
                preserve_leading_comments=preserve_leading_comments,
            )
        return dump_yaml_with_style(
            self.data,
            None,
            header=header,
            preserve_leading_comments=False,
        )


def _collect_stebbs_physics_relocations(
    working: Mapping[str, Any], ledger: StyleLedger
) -> dict[str, str]:
    """Map folded STEBBS and orthogonal physics paths onto their canonical paths."""
    model = working.get("model")
    if not isinstance(model, Mapping):
        return {}
    physics = model.get("physics")
    if not isinstance(physics, Mapping):
        return {}
    relocations: dict[str, str] = {}
    stebbs = physics.get("stebbs")
    if isinstance(stebbs, Mapping):
        for flat_key, target_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
            if target_leaf not in stebbs:
                continue
            old_path = f"model.physics.{flat_key}"
            new_path = f"model.physics.stebbs.{target_leaf}"
            if old_path in ledger and new_path not in ledger:
                relocations[old_path] = new_path
    orthogonal_moves = (
        (
            "model.physics.kdown_split_method.constant.sw_dn_direct_frac",
            "sw_dn_direct_frac",
        ),
        ("model.physics.storage_heat.ohm.include_qf", "ohm_inc_qf"),
        ("model.physics.storage_heat.ohm_inc_qf", "ohm_inc_qf"),
        ("model.physics.storage_heat.ohmincqf", "ohm_inc_qf"),
    )
    for old_path, flat_leaf in orthogonal_moves:
        new_path = f"model.physics.{flat_leaf}"
        if flat_leaf in physics and old_path in ledger and new_path not in ledger:
            relocations[old_path] = new_path
    return relocations


def _typed_mapping_key(key: Any) -> tuple[type[Any], Any]:
    return (type(key), key)


def _match_profile_key_renames(
    before: Mapping[Any, Any],
    after_by_typed: dict[tuple[type[Any], Any], Any],
) -> list[tuple[Any, str]]:
    """Pair raw profile keys in *before* with their canonical string keys in *after*."""
    matched: list[tuple[Any, str]] = []
    seen_new: set[str] = set()
    for old_k in before:
        try:
            new_k = normalise_hourly_profile_key(old_k)
        except ValueError:
            return []
        if _typed_mapping_key(new_k) not in after_by_typed or new_k in seen_new:
            return []
        seen_new.add(new_k)
        matched.append((old_k, new_k))
    return matched


def _collect_mapping_key_renames_with_orders(
    before: Any, after: Any, prefix: str = ""
) -> tuple[dict[str, str], dict[str, dict[tuple[type[Any], Any], Any]]]:
    """Collect path and parent-container key-order renames produced by profile normalisation."""
    renames: dict[str, str] = {}
    parent_orders: dict[str, dict[tuple[type[Any], Any], Any]] = {}
    if isinstance(before, Mapping) and isinstance(after, Mapping):
        if len(before) == len(after):
            before_by_typed = {_typed_mapping_key(k): k for k in before}
            after_by_typed = {_typed_mapping_key(k): k for k in after}
            if before_by_typed.keys() == after_by_typed.keys():
                for typed_k, old_k in before_by_typed.items():
                    new_k = after_by_typed[typed_k]
                    child_path = _join_key_path(prefix, new_k)
                    sub_ren, sub_ord = _collect_mapping_key_renames_with_orders(
                        before[old_k], after[new_k], child_path
                    )
                    renames.update(sub_ren)
                    parent_orders.update(sub_ord)
            else:
                for old_k, new_k in _match_profile_key_renames(before, after_by_typed):
                    old_path = _join_key_path(prefix, old_k)
                    new_path = _join_key_path(prefix, new_k)
                    if old_path != new_path:
                        renames[old_path] = new_path
                    if type(old_k) is not type(new_k) or old_k != new_k:
                        parent_orders.setdefault(prefix, {})[
                            _typed_mapping_key(old_k)
                        ] = new_k
                    sub_ren, sub_ord = _collect_mapping_key_renames_with_orders(
                        before[old_k], after[new_k], new_path
                    )
                    renames.update(sub_ren)
                    parent_orders.update(sub_ord)
    elif (
        isinstance(before, list)
        and isinstance(after, list)
        and len(before) == len(after)
    ):
        for idx, (b_item, a_item) in enumerate(zip(before, after)):
            child_prefix = f"{prefix}[{idx}]" if prefix else f"[{idx}]"
            sub_ren, sub_ord = _collect_mapping_key_renames_with_orders(
                b_item, a_item, child_prefix
            )
            renames.update(sub_ren)
            parent_orders.update(sub_ord)
    return renames, parent_orders


def _collect_mapping_key_renames(
    before: Any, after: Any, prefix: str = ""
) -> dict[str, str]:
    """Collect path renames produced by profile-key normalisation (e.g. ``1``/``"01"`` -> ``"1"``)."""
    renames, _orders = _collect_mapping_key_renames_with_orders(before, after, prefix)
    return renames


__all__ = [
    "ConfigDocument",
    "ConfigPatch",
    "MappingKey",
    "PatchAction",
    "PathStyle",
    "PathToken",
    "StyleLedger",
    "WrapperStyle",
    "apply_nullify_tree_at_path",
    "apply_nullify_tree_at_tokens",
    "apply_patches_in_place",
    "canonicalise_path",
    "canonicalise_token",
    "deep_freeze",
    "delete_path",
    "find_first_created_ancestor",
    "format_path_tokens",
    "format_pydantic_loc",
    "is_ref_only",
    "is_ref_wrapper",
    "make_key_token",
    "nullify_tree_in_place",
    "parse_path_tokens",
    "path_exists",
    "read_path",
    "read_raw_path",
    "read_raw_tokens",
    "read_scalar",
    "site_gridid",
    "site_gridid_map",
    "thaw_copy",
    "unwrap_entry",
    "write_path",
    "write_scalar",
    "write_tokens",
]
