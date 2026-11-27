"""Read-only proxy over validated Pydantic configuration models.

Wraps a defensive snapshot of any Pydantic ``BaseModel`` (such as
``SUEWSConfig`` and its nested sub-models) so validation rules can inspect
typed attributes, ``model_fields_set``, and computed properties without being
able to mutate the configuration or observe post-freeze mutations made by the
caller.
"""

from __future__ import annotations

from collections import deque
from collections.abc import Mapping, Sequence, Set as AbstractSet
from copy import deepcopy
from dataclasses import dataclass, fields as dc_fields, is_dataclass
from enum import Enum
from types import MappingProxyType
from typing import Any
import weakref

import numpy as np
from pydantic import BaseModel
from pydantic_core import PydanticUndefinedType

from ..pipeline.document import (
    _ENUM_INTERNAL_ATTRS,
    _freeze_mapping_entries,
    _freeze_sequence_items,
    _freeze_slice_value,
    _FrozenArraySpec,
    _FrozenSliceSpec,
    _is_exact_immutable_scalar,
    _is_immutable_enum,
    _materialise_frozen,
    _ReadOnlyMaterialisingMapping,
    _ReadOnlyMaterialisingTuple,
)

_ALLOWED_READONLY_METHODS = frozenset({"model_dump"})
_PROXY_STATE: dict[
    int,
    tuple[
        BaseModel,
        dict[str, Any],
        dict[int, ReadOnlyEnumProxy],
        dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]],
    ],
] = {}
_DATACLASS_STATE: dict[
    int,
    tuple[
        Any,
        dict[str, Any],
        dict[int, ReadOnlyEnumProxy],
        dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]],
    ],
] = {}
_ENUM_STATE: dict[int, tuple[Enum, Any, Mapping[str, Any]]] = {}

_MODEL_EXTRA_IMMUTABLE_TYPES: frozenset[type[Any]] = frozenset({
    PydanticUndefinedType,
})


def _detach_mapping_proxy_tree(value: Any, memo: dict[int, Any]) -> Any:
    """Fallback deep copy that detaches ``MappingProxyType`` inside models and dataclasses."""
    if value is None or _is_exact_immutable_scalar(value):
        return value
    obj_id = id(value)
    if obj_id in memo:
        return memo[obj_id]
    if isinstance(value, MappingProxyType):
        out_map: dict[Any, Any] = {}
        memo[obj_id] = out_map
        for k, v in value.items():
            out_map[_detach_mapping_proxy_tree(k, memo)] = _detach_mapping_proxy_tree(
                v, memo
            )
        return out_map
    if isinstance(value, BaseModel):
        model_cls = type(value)
        copied_fields = {
            name: _detach_mapping_proxy_tree(getattr(value, name), memo)
            for name in model_cls.model_fields
        }
        extra = getattr(value, "model_extra", None)
        if isinstance(extra, Mapping):
            for k, v in extra.items():
                copied_fields[k] = _detach_mapping_proxy_tree(v, memo)
        cloned = model_cls.model_construct(
            _fields_set=set(value.model_fields_set), **copied_fields
        )
        memo[obj_id] = cloned
        return cloned
    if is_dataclass(value) and not isinstance(value, type):
        dc_cls = type(value)
        cloned_dc = object.__new__(dc_cls)
        memo[obj_id] = cloned_dc
        for dc_field in dc_fields(dc_cls):
            object.__setattr__(
                cloned_dc,
                dc_field.name,
                _detach_mapping_proxy_tree(getattr(value, dc_field.name), memo),
            )
        return cloned_dc
    return _detach_container_fallback(value, memo)


def _detach_container_fallback(value: Any, memo: dict[int, Any]) -> Any:
    """Detach standard Python containers in the ``MappingProxyType`` fallback path."""
    obj_id = id(value)
    result: Any
    if isinstance(value, dict):
        out_dict: dict[Any, Any] = {}
        memo[obj_id] = out_dict
        for k, v in value.items():
            out_dict[_detach_mapping_proxy_tree(k, memo)] = _detach_mapping_proxy_tree(
                v, memo
            )
        result = out_dict
    elif isinstance(value, list):
        out_list: list[Any] = []
        memo[obj_id] = out_list
        out_list.extend(_detach_mapping_proxy_tree(item, memo) for item in value)
        result = out_list
    elif isinstance(value, (tuple, set, frozenset)):
        result = type(value)(_detach_mapping_proxy_tree(item, memo) for item in value)
    elif isinstance(value, deque):
        result = deque(
            (_detach_mapping_proxy_tree(item, memo) for item in value),
            maxlen=value.maxlen,
        )
    elif type(value) is slice:
        result = slice(
            _detach_mapping_proxy_tree(value.start, memo),
            _detach_mapping_proxy_tree(value.stop, memo),
            _detach_mapping_proxy_tree(value.step, memo),
        )
    else:
        result = deepcopy(value)
    return result


def _safe_snapshot_copy(value: Any) -> Any:
    """Deep-copy *value*, falling back cleanly if it contains ``MappingProxyType``."""
    try:
        return deepcopy(value)
    except TypeError:
        return _detach_mapping_proxy_tree(value, {})


def _snapshot_enum_extra_attrs(member: Enum) -> Mapping[str, Any]:
    """Snapshot and freeze non-private custom attributes on *member*."""
    attrs: dict[str, Any] = {}
    if hasattr(member, "__dict__"):
        for attr_name, attr_val in vars(member).items():
            if attr_name.startswith("_") or attr_name in _ENUM_INTERNAL_ATTRS:
                continue
            attrs[attr_name] = _wrap_readonly(attr_val)
    for cls in type(member).__mro__:
        slots = cls.__dict__.get("__slots__")
        if not slots:
            continue
        slot_names = (slots,) if isinstance(slots, str) else slots
        for slot_name in slot_names:
            if (
                slot_name.startswith("_")
                or slot_name in _ENUM_INTERNAL_ATTRS
                or slot_name in attrs
            ):
                continue
            try:
                slot_val = getattr(member, slot_name)
            except AttributeError:
                continue
            attrs[slot_name] = _wrap_readonly(slot_val)
    return _freeze_mapping_entries(attrs)


class ReadOnlyEnumProxy:
    """Deeply immutable proxy over an ``Enum`` member blocking attribute reassignment."""

    __slots__ = ("__weakref__",)

    def __init__(self, member: Enum) -> None:
        if not isinstance(member, Enum):
            raise TypeError(
                f"ReadOnlyEnumProxy requires an Enum member, got {type(member).__name__}"
            )
        if not _is_immutable_enum(member):
            raise TypeError(
                f"Cannot safely freeze Enum member {type(member).__name__}.{member.name} "
                "with mutable payload"
            )
        frozen_value = _wrap_readonly(member.value)
        extra_attrs = _snapshot_enum_extra_attrs(member)
        key = id(self)
        _ENUM_STATE[key] = (member, frozen_value, extra_attrs)
        weakref.finalize(self, _ENUM_STATE.pop, key, None)

    @property
    def enum_type(self) -> type[Enum]:
        """Underlying ``Enum`` class."""
        return type(_ENUM_STATE[id(self)][0])

    @property
    def name(self) -> str:
        """Name of the wrapped ``Enum`` member."""
        return _ENUM_STATE[id(self)][0].name

    @property
    def value(self) -> Any:
        """Frozen value of the wrapped ``Enum`` member."""
        return _materialise_readonly(_ENUM_STATE[id(self)][1])

    def unwrap_copy(self) -> Enum:
        """Return the underlying ``Enum`` member."""
        return _ENUM_STATE[id(self)][0]

    def __getattr__(self, name: str) -> Any:
        """Resolve snapshotted public attributes on the wrapped ``Enum`` member."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only enum view {self.enum_type.__name__!r} "
                f"does not expose private attribute {name!r}"
            )
        extra_attrs = _ENUM_STATE[id(self)][2]
        if name in extra_attrs:
            return extra_attrs[name]
        raise AttributeError(
            f"Read-only enum view {self.enum_type.__name__!r} has no readable attribute {name!r}"
        )

    def __setattr__(self, name: str, value: Any) -> None:
        """Reject every attribute assignment on the read-only enum proxy."""
        raise TypeError(
            f"Cannot mutate attribute {name!r} on read-only enum view "
            f"{self.enum_type.__name__!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only enum proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only enum view "
            f"{self.enum_type.__name__!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare underlying enum equality against another proxy, member, or scalar."""
        member = _ENUM_STATE[id(self)][0]
        if isinstance(other, ReadOnlyEnumProxy):
            return bool(member == _ENUM_STATE[id(other)][0])
        return bool(member == other)

    def __hash__(self) -> int:
        """Delegate hashing to the underlying ``Enum`` member."""
        return hash(_ENUM_STATE[id(self)][0])

    def __bool__(self) -> bool:
        """Delegate truthiness to the underlying ``Enum`` member."""
        return bool(_ENUM_STATE[id(self)][0])

    def __int__(self) -> int:
        """Support ``int()`` conversion for ``IntEnum`` / ``IntFlag`` proxies."""
        member = _ENUM_STATE[id(self)][0]
        if isinstance(member, int):
            return int(member)
        raise TypeError(
            f"int() argument must be an integer Enum proxy, not {type(member).__name__!r}"
        )

    def __index__(self) -> int:
        """Support integer indexing for ``IntEnum`` / ``IntFlag`` proxies."""
        return self.__int__()

    def __float__(self) -> float:
        """Support ``float()`` conversion for numeric ``Enum`` proxies (e.g. ``TimezoneOffset``)."""
        member = _ENUM_STATE[id(self)][0]
        if isinstance(member, (int, float)) and not isinstance(member, bool):
            return float(member)
        raise TypeError(
            f"float() argument must be a numeric Enum proxy, not {type(member).__name__!r}"
        )

    def __str__(self) -> str:
        """Return the string representation of the underlying ``Enum`` member."""
        return str(_ENUM_STATE[id(self)][0])

    def __repr__(self) -> str:
        """Return a readable representation of the wrapped ``Enum`` member."""
        return f"ReadOnlyEnumProxy({_ENUM_STATE[id(self)][0]!r})"

    def __copy__(self) -> ReadOnlyEnumProxy:
        """Return ``self`` because ``ReadOnlyEnumProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyEnumProxy:
        """Return ``self`` because ``ReadOnlyEnumProxy`` is deeply immutable."""
        return self


_FIELD_INFO_ATTRS = (
    "name",
    "annotation",
    "default",
    "is_required",
    "alias",
    "title",
    "description",
    "deprecated",
    "metadata",
    "json_schema_extra",
)


@dataclass(frozen=True)
class ReadOnlyFieldInfo:
    """Deeply immutable snapshot of a Pydantic ``FieldInfo`` declaration."""

    name: str
    annotation: Any
    default: Any
    is_required: bool
    alias: str | None
    title: str | None
    description: str | None
    deprecated: Any
    metadata: tuple[Any, ...]
    json_schema_extra: Any

    def __getattribute__(self, name: str) -> Any:
        """Materialise lazy frozen specs into fresh views on every attribute read."""
        return _materialise_readonly(super().__getattribute__(name))

    def __eq__(self, other: object) -> bool:
        """Compare stored field metadata without triggering ambiguous NumPy comparisons."""
        if other is self:
            return True
        if not isinstance(other, ReadOnlyFieldInfo):
            return NotImplemented
        return all(
            object.__getattribute__(self, attr) == object.__getattribute__(other, attr)
            for attr in _FIELD_INFO_ATTRS
        )

    def __hash__(self) -> int:
        """Hash stored field metadata consistently with :meth:`__eq__`."""
        return hash(
            tuple(object.__getattribute__(self, attr) for attr in _FIELD_INFO_ATTRS)
        )

    def __copy__(self) -> ReadOnlyFieldInfo:
        """Return ``self`` because ``ReadOnlyFieldInfo`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyFieldInfo:
        """Return ``self`` because ``ReadOnlyFieldInfo`` is deeply immutable."""
        return self


def _collect_proxy_snapshots(
    root: Any,
) -> tuple[
    dict[int, ReadOnlyEnumProxy],
    dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]],
]:
    """Eagerly snapshot reachable immutable ``Enum`` members and ``BaseModel`` field declarations."""
    enum_snapshots: dict[int, ReadOnlyEnumProxy] = {}
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]] = {}
    visited: set[int] = set()

    def _walk_children(obj: Any) -> None:
        if isinstance(obj, BaseModel):
            model_cls = type(obj)
            if model_cls not in field_snapshots:
                field_snapshots[model_cls] = _freeze_model_fields(model_cls)
            for field_name in model_cls.model_fields:
                _walk(getattr(obj, field_name))
            extra = getattr(obj, "model_extra", None)
            if isinstance(extra, Mapping):
                _walk(extra)
        elif is_dataclass(obj) and not isinstance(obj, type):
            for dc_field in dc_fields(type(obj)):
                _walk(getattr(obj, dc_field.name))
        elif isinstance(obj, Mapping):
            for k, v in obj.items():
                _walk(k)
                _walk(v)
        elif isinstance(obj, (list, tuple, deque, set, frozenset)):
            for item in obj:
                _walk(item)
        elif type(obj) is slice:
            _walk(obj.start)
            _walk(obj.stop)
            _walk(obj.step)
        elif isinstance(obj, np.ndarray) and obj.dtype.hasobject:
            for item in obj.flat:
                _walk(item)
        elif isinstance(obj, np.void) and obj.dtype.names:
            for field_name in obj.dtype.names:
                _walk(obj[field_name])

    def _walk(obj: Any) -> None:
        if obj is None or _is_exact_immutable_scalar(obj):
            return
        if isinstance(obj, Enum):
            obj_id = id(obj)
            if obj_id not in enum_snapshots and _is_immutable_enum(obj):
                enum_snapshots[obj_id] = ReadOnlyEnumProxy(obj)
            return
        obj_id = id(obj)
        if obj_id in visited:
            return
        visited.add(obj_id)
        _walk_children(obj)

    _walk(root)
    return enum_snapshots, field_snapshots


class ReadOnlyDataclassProxy:
    """Deeply immutable attribute-access proxy over a Python dataclass instance."""

    __slots__ = ("__weakref__",)

    def __init__(
        self,
        instance: Any,
        *,
        _detached: bool = False,
        _enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
        _field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
        | None = None,
    ) -> None:
        if not is_dataclass(instance) or isinstance(instance, type):
            raise TypeError(
                f"ReadOnlyDataclassProxy requires a dataclass instance, got {type(instance).__name__}"
            )
        snapshot = instance if _detached else _safe_snapshot_copy(instance)
        if _enum_snapshots is None or _field_snapshots is None:
            collected_enums, collected_fields = _collect_proxy_snapshots(snapshot)
            enum_snapshots = (
                _enum_snapshots if _enum_snapshots is not None else collected_enums
            )
            field_snapshots = (
                _field_snapshots if _field_snapshots is not None else collected_fields
            )
        else:
            enum_snapshots = _enum_snapshots
            field_snapshots = _field_snapshots
        key = id(self)
        _DATACLASS_STATE[key] = (snapshot, {}, enum_snapshots, field_snapshots)
        weakref.finalize(self, _DATACLASS_STATE.pop, key, None)

    @classmethod
    def _from_detached(
        cls,
        detached_instance: Any,
        *,
        enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
        field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
        | None = None,
    ) -> ReadOnlyDataclassProxy:
        """Wrap an already-detached dataclass snapshot without re-copying."""
        return cls(
            detached_instance,
            _detached=True,
            _enum_snapshots=enum_snapshots,
            _field_snapshots=field_snapshots,
        )

    @property
    def dataclass_type(self) -> type[Any]:
        """Underlying dataclass type."""
        return type(_DATACLASS_STATE[id(self)][0])

    def __getattr__(self, name: str) -> Any:
        """Resolve declared dataclass fields and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only dataclass view {self.dataclass_type.__name__!r} "
                f"does not expose private attribute {name!r}"
            )
        target, cache, enum_snapshots, field_snapshots = _DATACLASS_STATE[id(self)]
        if name in cache:
            return _materialise_readonly(cache[name])

        dc_type = type(target)
        field_names = {field.name for field in dc_fields(dc_type)}
        if name in field_names:
            wrapped = _wrap_readonly(
                getattr(target, name), enum_snapshots, field_snapshots
            )
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        for cls_in_mro in dc_type.__mro__:
            if name in cls_in_mro.__dict__:
                if isinstance(cls_in_mro.__dict__[name], property):
                    wrapped = _wrap_readonly(
                        getattr(_safe_snapshot_copy(target), name),
                        enum_snapshots,
                        field_snapshots,
                    )
                    cache[name] = wrapped
                    return _materialise_readonly(wrapped)
                break

        raise AttributeError(
            f"Read-only dataclass view {dc_type.__name__!r} has no readable attribute {name!r}"
        )

    def __setattr__(self, name: str, value: Any) -> None:
        """Reject every attribute assignment on the read-only dataclass proxy."""
        raise TypeError(
            f"Cannot mutate attribute {name!r} on read-only dataclass view "
            f"{self.dataclass_type.__name__!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only dataclass proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only dataclass view "
            f"{self.dataclass_type.__name__!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare underlying dataclass equality against another proxy or instance."""
        target = _DATACLASS_STATE[id(self)][0]
        if isinstance(other, ReadOnlyDataclassProxy):
            return bool(target == _DATACLASS_STATE[id(other)][0])
        if is_dataclass(other) and not isinstance(other, type):
            return bool(target == other)
        return NotImplemented

    def __hash__(self) -> int:
        """Delegate hashing to the underlying immutable dataclass snapshot when hashable."""
        return hash(_DATACLASS_STATE[id(self)][0])

    def unwrap_copy(self) -> Any:
        """Return an independent mutable deep copy of the underlying dataclass."""
        return _safe_snapshot_copy(_DATACLASS_STATE[id(self)][0])

    def __copy__(self) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __repr__(self) -> str:
        """Return a readable representation of the wrapped dataclass."""
        return f"ReadOnlyDataclassProxy({_DATACLASS_STATE[id(self)][0]!r})"


def _freeze_ndarray_spec(
    value: np.ndarray[Any, Any] | np.void,
    enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
    | None = None,
) -> Any:
    """Wrap *value* using immutable backing storage or frozen nested containers."""
    if isinstance(value, np.void):
        if value.dtype.names:
            return _freeze_mapping_entries({
                name: _wrap_readonly(
                    _safe_snapshot_copy(value[name]), enum_snapshots, field_snapshots
                )
                for name in value.dtype.names
            })
        return bytes(value)
    if value.dtype.hasobject:
        if value.ndim == 0:
            return _wrap_readonly(
                _safe_snapshot_copy(value.item()), enum_snapshots, field_snapshots
            )
        return _wrap_readonly(
            _safe_snapshot_copy(value.tolist()), enum_snapshots, field_snapshots
        )
    return _FrozenArraySpec(value)


def _materialise_readonly(value: Any) -> Any:
    """Materialise any top-level lazy frozen spec into a fresh read-only view."""
    return _materialise_frozen(value)


def _wrap_container_or_object(
    value: Any,
    enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
    | None = None,
) -> Any:
    """Wrap containers, dataclasses, NumPy arrays, or reject unsupported mutable objects."""
    if isinstance(value, Mapping):
        return _freeze_mapping_entries({
            _wrap_readonly(k, enum_snapshots, field_snapshots): _wrap_readonly(
                v, enum_snapshots, field_snapshots
            )
            for k, v in value.items()
        })
    if isinstance(value, (list, tuple, deque, Sequence)) and not isinstance(
        value, (str, bytes, bytearray, memoryview)
    ):
        return _freeze_sequence_items([
            _wrap_readonly(item, enum_snapshots, field_snapshots) for item in value
        ])
    if isinstance(value, (set, frozenset, AbstractSet)):
        return frozenset(
            _wrap_readonly(item, enum_snapshots, field_snapshots) for item in value
        )
    if isinstance(value, (np.ndarray, np.void)):
        return _freeze_ndarray_spec(value, enum_snapshots, field_snapshots)
    if is_dataclass(value) and not isinstance(value, type):
        return ReadOnlyDataclassProxy._from_detached(
            value,
            enum_snapshots=enum_snapshots,
            field_snapshots=field_snapshots,
        )
    raise TypeError(
        f"ReadOnlyModelProxy cannot safely freeze mutable object of type {type(value).__name__!r}"
    )


def _wrap_readonly(
    value: Any,
    enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
    | None = None,
) -> Any:
    """Recursively wrap a detached Pydantic/Python value as read-only."""
    if isinstance(value, Enum):
        return (
            enum_snapshots[id(value)]
            if enum_snapshots is not None and id(value) in enum_snapshots
            else ReadOnlyEnumProxy(value)
        )
    if type(value) is slice:
        return _freeze_slice_value(
            value, lambda bound: _wrap_readonly(bound, enum_snapshots, field_snapshots)
        )
    if (
        _is_exact_immutable_scalar(value)
        or type(value) in _MODEL_EXTRA_IMMUTABLE_TYPES
        or isinstance(
            value,
            (
                ReadOnlyModelProxy,
                ReadOnlyDataclassProxy,
                ReadOnlyEnumProxy,
                _FrozenArraySpec,
                _FrozenSliceSpec,
                _ReadOnlyMaterialisingMapping,
                _ReadOnlyMaterialisingTuple,
            ),
        )
    ):
        return value
    if type(value) in {bytearray, memoryview}:
        return bytes(value)
    if isinstance(value, BaseModel):
        return ReadOnlyModelProxy._from_detached(
            value,
            enum_snapshots=enum_snapshots,
            field_snapshots=field_snapshots,
        )
    return _wrap_container_or_object(value, enum_snapshots, field_snapshots)


def _freeze_field_metadata_value(value: Any) -> Any:
    """Safely snapshot and freeze arbitrary Pydantic field metadata values."""
    if _is_exact_immutable_scalar(value) or type(value) in _MODEL_EXTRA_IMMUTABLE_TYPES:
        return value
    try:
        copied = deepcopy(value)
    except (TypeError, ValueError):
        copied = value
    try:
        return _wrap_readonly(copied)
    except TypeError:
        return repr(value)


def _freeze_model_fields(model_cls: type[BaseModel]) -> Mapping[str, ReadOnlyFieldInfo]:
    """Build a detached read-only snapshot of ``model_cls.model_fields``."""
    frozen: dict[str, ReadOnlyFieldInfo] = {}
    for field_name, info in model_cls.model_fields.items():
        frozen[field_name] = ReadOnlyFieldInfo(
            name=field_name,
            annotation=info.annotation,
            default=_freeze_field_metadata_value(info.default),
            is_required=bool(info.is_required()),
            alias=info.alias,
            title=info.title,
            description=info.description,
            deprecated=_freeze_field_metadata_value(info.deprecated),
            metadata=_freeze_sequence_items([
                _freeze_field_metadata_value(m) for m in info.metadata
            ]),
            json_schema_extra=_freeze_field_metadata_value(info.json_schema_extra),
        )
    return MappingProxyType(frozen)


class ReadOnlyModelProxy:
    """Deeply immutable attribute-access proxy over a Pydantic ``BaseModel``.

    All attribute writes and deletions raise :class:`TypeError`. Nested
    ``BaseModel`` attributes are lazily wrapped in :class:`ReadOnlyModelProxy`,
    dataclasses in :class:`ReadOnlyDataclassProxy`, mappings in
    :class:`~types.MappingProxyType`, sequences (including ``list`` and
    ``deque``) as tuples, sets as frozensets, and NumPy arrays as fresh
    zero-copy views backed by immutable ``bytes``. Unsupported mutable objects
    are rejected explicitly with :class:`TypeError`. Backing state is held
    outside the proxy instance so private attribute access cannot leak the
    underlying mutable model.
    """

    __slots__ = ("__weakref__",)

    def __init__(
        self,
        model: BaseModel,
        *,
        _detached: bool = False,
        _enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
        _field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
        | None = None,
    ) -> None:
        if not isinstance(model, BaseModel):
            raise TypeError(
                f"ReadOnlyModelProxy requires a Pydantic BaseModel, got {type(model).__name__}"
            )
        snapshot = model if _detached else _safe_snapshot_copy(model)
        if _enum_snapshots is None or _field_snapshots is None:
            collected_enums, collected_fields = _collect_proxy_snapshots(snapshot)
            enum_snapshots = (
                _enum_snapshots if _enum_snapshots is not None else collected_enums
            )
            field_snapshots = (
                _field_snapshots if _field_snapshots is not None else collected_fields
            )
        else:
            enum_snapshots = _enum_snapshots
            field_snapshots = _field_snapshots
        model_cls = type(snapshot)
        if model_cls not in field_snapshots:
            field_snapshots[model_cls] = _freeze_model_fields(model_cls)
        key = id(self)
        _PROXY_STATE[key] = (snapshot, {}, enum_snapshots, field_snapshots)
        weakref.finalize(self, _PROXY_STATE.pop, key, None)

    @classmethod
    def _from_detached(
        cls,
        detached_model: BaseModel,
        *,
        enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
        field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
        | None = None,
    ) -> ReadOnlyModelProxy:
        """Construct a proxy around an already-detached snapshot without re-copying."""
        return cls(
            detached_model,
            _detached=True,
            _enum_snapshots=enum_snapshots,
            _field_snapshots=field_snapshots,
        )

    @property
    def model_type(self) -> type[BaseModel]:
        """Underlying Pydantic ``BaseModel`` class."""
        return type(_PROXY_STATE[id(self)][0])

    @property
    def model_fields(self) -> Mapping[str, ReadOnlyFieldInfo]:
        """Detached, deeply immutable view of the model's declared Pydantic fields."""
        _target, _cache, _enums, field_snapshots = _PROXY_STATE[id(self)]
        return field_snapshots[self.model_type]

    @property
    def model_fields_set(self) -> frozenset[str]:
        """Frozen set of field names explicitly set on the model."""
        return frozenset(_PROXY_STATE[id(self)][0].model_fields_set)

    def model_dump(self, *args: Any, **kwargs: Any) -> dict[str, Any]:
        """Return a fresh detached dictionary serialisation of a working model copy."""
        working_copy = _safe_snapshot_copy(_PROXY_STATE[id(self)][0])
        return _safe_snapshot_copy(working_copy.model_dump(*args, **kwargs))

    def unwrap_copy(self) -> BaseModel:
        """Return an independent mutable deep copy of the underlying model."""
        return _safe_snapshot_copy(_PROXY_STATE[id(self)][0])

    def to_typed_view(self) -> Any:
        """Convert this read-only model into a deeply frozen :class:`TypedConfigView`."""
        from .typed_view import TypedConfigView  # noqa: PLC0415

        return TypedConfigView.from_config(
            _safe_snapshot_copy(_PROXY_STATE[id(self)][0])
        )

    def __getattr__(self, name: str) -> Any:
        """Resolve declared fields, computed fields, and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only model view {self.model_type.__name__!r} does not expose private attribute {name!r}"
            )
        target, cache, enum_snapshots, field_snapshots = _PROXY_STATE[id(self)]
        if name in cache:
            return _materialise_readonly(cache[name])

        model_cls = type(target)

        if name in model_cls.model_fields:
            wrapped = _wrap_readonly(
                getattr(target, name), enum_snapshots, field_snapshots
            )
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        if name in model_cls.model_computed_fields:
            wrapped = _wrap_readonly(
                getattr(_safe_snapshot_copy(target), name),
                enum_snapshots,
                field_snapshots,
            )
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        for cls_in_mro in model_cls.__mro__:
            if name in cls_in_mro.__dict__:
                descriptor = cls_in_mro.__dict__[name]
                if isinstance(descriptor, property):
                    wrapped = _wrap_readonly(
                        getattr(_safe_snapshot_copy(target), name),
                        enum_snapshots,
                        field_snapshots,
                    )
                    cache[name] = wrapped
                    return _materialise_readonly(wrapped)
                break

        extra = getattr(target, "model_extra", None)
        if isinstance(extra, Mapping) and name in extra:
            wrapped = _wrap_readonly(extra[name], enum_snapshots, field_snapshots)
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        if name in _ALLOWED_READONLY_METHODS:
            return getattr(self, name)

        raise AttributeError(
            f"Read-only model view {model_cls.__name__!r} has no readable attribute {name!r}"
        )

    def __setattr__(self, name: str, value: Any) -> None:
        """Reject every attribute assignment on the read-only proxy."""
        raise TypeError(
            f"Cannot mutate attribute {name!r} on read-only model view "
            f"{self.model_type.__name__!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only model view "
            f"{self.model_type.__name__!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare underlying model equality against another proxy or ``BaseModel``."""
        target = _PROXY_STATE[id(self)][0]
        if isinstance(other, ReadOnlyModelProxy):
            return target == _PROXY_STATE[id(other)][0]
        if isinstance(other, BaseModel):
            return target == other
        return NotImplemented

    def __hash__(self) -> int:
        """Delegate hashing to the underlying immutable model snapshot when hashable."""
        return hash(_PROXY_STATE[id(self)][0])

    def __copy__(self) -> ReadOnlyModelProxy:
        """Return ``self`` because ``ReadOnlyModelProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyModelProxy:
        """Return ``self`` because ``ReadOnlyModelProxy`` is deeply immutable."""
        return self

    def __repr__(self) -> str:
        """Return a readable representation of the wrapped model."""
        return f"ReadOnlyModelProxy({_PROXY_STATE[id(self)][0]!r})"

    def __dir__(self) -> list[str]:
        """List readable fields and inspection helpers exposed by the proxy."""
        model_cls = self.model_type
        names = set(model_cls.model_fields) | set(model_cls.model_computed_fields)
        names.update({
            "model_type",
            "model_fields",
            "model_fields_set",
            "model_dump",
            "unwrap_copy",
            "to_typed_view",
        })
        return sorted(names)


def freeze_model(model: BaseModel | ReadOnlyModelProxy) -> ReadOnlyModelProxy:
    """Return a deeply immutable :class:`ReadOnlyModelProxy` for *model*.

    If *model* is already a :class:`ReadOnlyModelProxy`, returns it directly.
    Otherwise snapshots *model* via ``deepcopy`` so subsequent caller mutations
    cannot leak into validation rules.
    """
    if isinstance(model, ReadOnlyModelProxy):
        return model
    return ReadOnlyModelProxy(model)
