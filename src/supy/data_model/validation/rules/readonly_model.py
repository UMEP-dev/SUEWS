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
import datetime
from decimal import Decimal
from enum import Enum
from fractions import Fraction
import ipaddress
from pathlib import PurePath
import re
from types import (
    BuiltinFunctionType,
    FunctionType,
    MappingProxyType,
    MethodDescriptorType,
)
from typing import Any
from uuid import UUID
import weakref

import numpy as np
from pydantic import BaseModel
from pydantic_core import PydanticUndefinedType

from ..pipeline.document import (
    _freeze_mapping_entries,
    _freeze_sequence_items,
    _freeze_slice_value,
    _FrozenArraySpec,
    _FrozenSliceSpec,
    _is_immutable_enum,
    _materialise_frozen,
    _ReadOnlyMaterialisingMapping,
    _ReadOnlyMaterialisingTuple,
)

_ALLOWED_READONLY_METHODS = frozenset({"model_dump"})
_PROXY_STATE: dict[int, tuple[BaseModel, dict[str, Any]]] = {}
_DATACLASS_STATE: dict[int, tuple[Any, dict[str, Any]]] = {}

_IMMUTABLE_TYPES = (
    type(None),
    bool,
    int,
    float,
    complex,
    str,
    bytes,
    range,
    type,
    Decimal,
    Fraction,
    UUID,
    PurePath,
    datetime.datetime,
    datetime.date,
    datetime.time,
    datetime.timedelta,
    datetime.tzinfo,
    re.Pattern,
    ipaddress.IPv4Address,
    ipaddress.IPv6Address,
    ipaddress.IPv4Network,
    ipaddress.IPv6Network,
    ipaddress.IPv4Interface,
    ipaddress.IPv6Interface,
    PydanticUndefinedType,
    np.number,
    np.bool_,
    np.character,
    np.datetime64,
    np.timedelta64,
    FunctionType,
    BuiltinFunctionType,
    MethodDescriptorType,
)

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


class ReadOnlyDataclassProxy:
    """Deeply immutable attribute-access proxy over a Python dataclass instance."""

    __slots__ = ("__weakref__",)

    def __init__(self, instance: Any, *, _detached: bool = False) -> None:
        if not is_dataclass(instance) or isinstance(instance, type):
            raise TypeError(
                f"ReadOnlyDataclassProxy requires a dataclass instance, got {type(instance).__name__}"
            )
        snapshot = instance if _detached else deepcopy(instance)
        key = id(self)
        _DATACLASS_STATE[key] = (snapshot, {})
        weakref.finalize(self, _DATACLASS_STATE.pop, key, None)

    @classmethod
    def _from_detached(cls, detached_instance: Any) -> ReadOnlyDataclassProxy:
        """Wrap an already-detached dataclass snapshot without re-copying."""
        return cls(detached_instance, _detached=True)

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
        target, cache = _DATACLASS_STATE[id(self)]
        if name in cache:
            return _materialise_readonly(cache[name])

        dc_type = type(target)
        field_names = {field.name for field in dc_fields(dc_type)}
        if name in field_names:
            wrapped = _wrap_readonly(getattr(target, name))
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        for cls_in_mro in dc_type.__mro__:
            if name in cls_in_mro.__dict__:
                if isinstance(cls_in_mro.__dict__[name], property):
                    wrapped = _wrap_readonly(getattr(target, name))
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
        return deepcopy(_DATACLASS_STATE[id(self)][0])

    def __copy__(self) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __repr__(self) -> str:
        """Return a readable representation of the wrapped dataclass."""
        return f"ReadOnlyDataclassProxy({_DATACLASS_STATE[id(self)][0]!r})"


def _freeze_ndarray_spec(value: np.ndarray[Any, Any] | np.void) -> Any:
    """Wrap *value* using immutable backing storage or frozen nested containers."""
    if isinstance(value, np.void):
        if value.dtype.names:
            return _freeze_mapping_entries({
                name: _wrap_readonly(deepcopy(value[name]))
                for name in value.dtype.names
            })
        return bytes(value)
    if value.dtype.hasobject:
        if value.ndim == 0:
            return _wrap_readonly(deepcopy(value.item()))
        return _wrap_readonly(deepcopy(value.tolist()))
    return _FrozenArraySpec(value)


def _materialise_readonly(value: Any) -> Any:
    """Materialise any top-level lazy frozen spec into a fresh read-only view."""
    return _materialise_frozen(value)


def _wrap_container_or_object(value: Any) -> Any:
    """Wrap containers, dataclasses, NumPy arrays, or reject unsupported mutable objects."""
    if isinstance(value, Mapping):
        return _freeze_mapping_entries({k: _wrap_readonly(v) for k, v in value.items()})
    if isinstance(value, (list, tuple, deque, Sequence)):
        return _freeze_sequence_items([_wrap_readonly(item) for item in value])
    if isinstance(value, (set, frozenset, AbstractSet)):
        return frozenset(_wrap_readonly(item) for item in value)
    if isinstance(value, (np.ndarray, np.void)):
        return _freeze_ndarray_spec(value)
    if is_dataclass(value) and not isinstance(value, type):
        return ReadOnlyDataclassProxy._from_detached(value)
    raise TypeError(
        f"ReadOnlyModelProxy cannot safely freeze mutable object of type {type(value).__name__!r}"
    )


def _wrap_readonly(value: Any) -> Any:
    """Recursively wrap a detached Pydantic/Python value as read-only."""
    if isinstance(value, Enum):
        if _is_immutable_enum(value):
            return value
        raise TypeError(
            f"ReadOnlyModelProxy cannot safely freeze Enum member "
            f"{type(value).__name__}.{value.name} with mutable payload"
        )
    if isinstance(value, slice):
        return _freeze_slice_value(value, _wrap_readonly)
    if isinstance(
        value,
        (
            ReadOnlyModelProxy,
            ReadOnlyDataclassProxy,
            _FrozenArraySpec,
            _FrozenSliceSpec,
            _ReadOnlyMaterialisingMapping,
            _ReadOnlyMaterialisingTuple,
            *_IMMUTABLE_TYPES,
        ),
    ):
        return value
    if isinstance(value, (bytearray, memoryview)):
        return bytes(value)
    if isinstance(value, BaseModel):
        return ReadOnlyModelProxy._from_detached(value)
    return _wrap_container_or_object(value)


def _freeze_field_metadata_value(value: Any) -> Any:
    """Safely snapshot and freeze arbitrary Pydantic field metadata values."""
    if not isinstance(value, Enum) and isinstance(value, _IMMUTABLE_TYPES):
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

    def __init__(self, model: BaseModel, *, _detached: bool = False) -> None:
        if not isinstance(model, BaseModel):
            raise TypeError(
                f"ReadOnlyModelProxy requires a Pydantic BaseModel, got {type(model).__name__}"
            )
        snapshot = model if _detached else deepcopy(model)
        key = id(self)
        _PROXY_STATE[key] = (snapshot, {})
        weakref.finalize(self, _PROXY_STATE.pop, key, None)

    @classmethod
    def _from_detached(cls, detached_model: BaseModel) -> ReadOnlyModelProxy:
        """Construct a proxy around an already-detached snapshot without re-copying."""
        return cls(detached_model, _detached=True)

    @property
    def model_type(self) -> type[BaseModel]:
        """Underlying Pydantic ``BaseModel`` class."""
        return type(_PROXY_STATE[id(self)][0])

    @property
    def model_fields(self) -> Mapping[str, ReadOnlyFieldInfo]:
        """Detached, deeply immutable view of the model's declared Pydantic fields."""
        _target, cache = _PROXY_STATE[id(self)]
        cached = cache.get("__model_fields__")
        if cached is None:
            cached = _freeze_model_fields(self.model_type)
            cache["__model_fields__"] = cached
        return cached

    @property
    def model_fields_set(self) -> frozenset[str]:
        """Frozen set of field names explicitly set on the model."""
        return frozenset(_PROXY_STATE[id(self)][0].model_fields_set)

    def model_dump(self, *args: Any, **kwargs: Any) -> dict[str, Any]:
        """Return a fresh detached dictionary serialisation of the underlying model."""
        return deepcopy(_PROXY_STATE[id(self)][0].model_dump(*args, **kwargs))

    def unwrap_copy(self) -> BaseModel:
        """Return an independent mutable deep copy of the underlying model."""
        return deepcopy(_PROXY_STATE[id(self)][0])

    def to_typed_view(self) -> Any:
        """Convert this read-only model into a deeply frozen :class:`TypedConfigView`."""
        from .typed_view import TypedConfigView  # noqa: PLC0415

        return TypedConfigView.from_config(_PROXY_STATE[id(self)][0])

    def __getattr__(self, name: str) -> Any:
        """Resolve declared fields, computed fields, and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only model view {self.model_type.__name__!r} does not expose private attribute {name!r}"
            )
        target, cache = _PROXY_STATE[id(self)]
        if name in cache:
            return _materialise_readonly(cache[name])

        model_cls = type(target)

        if name in model_cls.model_fields or name in model_cls.model_computed_fields:
            wrapped = _wrap_readonly(getattr(target, name))
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        for cls_in_mro in model_cls.__mro__:
            if name in cls_in_mro.__dict__:
                descriptor = cls_in_mro.__dict__[name]
                if isinstance(descriptor, property):
                    wrapped = _wrap_readonly(getattr(target, name))
                    cache[name] = wrapped
                    return _materialise_readonly(wrapped)
                break

        extra = getattr(target, "model_extra", None)
        if isinstance(extra, Mapping) and name in extra:
            wrapped = _wrap_readonly(extra[name])
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
