"""Read-only proxy over validated Pydantic configuration models.

Wraps a defensive snapshot of any Pydantic ``BaseModel`` (such as
``SUEWSConfig`` and its nested sub-models) so validation rules can inspect
typed attributes, ``model_fields_set``, and computed properties without being
able to mutate the configuration or observe post-freeze mutations made by the
caller.
"""

from __future__ import annotations

from collections import Counter, OrderedDict, defaultdict, deque
from collections.abc import Mapping, Sequence, Set as AbstractSet
from copy import deepcopy
from dataclasses import dataclass, fields as dc_fields, is_dataclass
import datetime
from enum import Enum, Flag
import functools
import math
from types import (
    BuiltinMethodType,
    FunctionType,
    MappingProxyType,
    MethodType,
    MethodWrapperType,
    ModuleType,
)
from typing import Any
import weakref

import numpy as np
from pydantic import BaseModel
from pydantic_core import PydanticUndefinedType

from ..pipeline.document import (
    _ENUM_INTERNAL_ATTRS,
    _LAZY_FROZEN_SPECS,
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
    _validate_safe_numpy_dtype,
)

_ALLOWED_READONLY_METHODS = frozenset({"model_dump"})
_PARTIAL_IN_PROGRESS = object()
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
_ENUM_STATE: dict[int, tuple[Enum, str, Any, int | None, Mapping[str, Any]]] = {}

_MODEL_EXTRA_IMMUTABLE_TYPES: frozenset[type[Any]] = frozenset({
    PydanticUndefinedType,
})


_PYDANTIC_INTERNAL_SLOTS: frozenset[str] = frozenset({
    "__dict__",
    "__weakref__",
    "__pydantic_fields_set__",
    "__pydantic_extra__",
    "__pydantic_private__",
})


def _iter_custom_slots(cls: type[Any], skip: frozenset[str]) -> tuple[str, ...]:
    """Return unique custom ``__slots__`` attribute names across *cls* MRO (with name mangling)."""
    names: list[str] = []
    seen: set[str] = set()
    for base in cls.__mro__:
        slots = base.__dict__.get("__slots__")
        if not slots:
            continue
        slot_iter = (slots,) if isinstance(slots, str) else slots
        for slot_name in slot_iter:
            if slot_name in skip:
                continue
            resolved_name = slot_name
            if slot_name.startswith("__") and not slot_name.endswith("__"):
                stripped = base.__name__.lstrip("_")
                if stripped:
                    resolved_name = f"_{stripped}{slot_name}"
            if resolved_name in skip or resolved_name in seen:
                continue
            seen.add(resolved_name)
            names.append(resolved_name)
    return tuple(names)


def _copy_slots(
    src: Any,
    dst: Any,
    memo: dict[int, Any],
    *,
    skip: frozenset[str],
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> None:
    """Copy non-skipped ``__slots__`` values from *src* to *dst* via detached traversal."""
    for slot_name in _iter_custom_slots(type(src), skip):
        try:
            slot_val = object.__getattribute__(src, slot_name)
        except AttributeError:
            continue
        object.__setattr__(
            dst,
            slot_name,
            _detach_snapshot_tree(
                slot_val,
                memo,
                reject_enums=reject_enums,
                wrap_enums=wrap_enums,
            ),
        )


def _clone_base_model(
    value: BaseModel,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> BaseModel:
    """Clone a Pydantic ``BaseModel`` preserving ``PrivateAttr``, extras, and cycles."""
    model_cls = type(value)
    cloned = model_cls.__new__(model_cls)
    memo[id(value)] = cloned
    src_dict = object.__getattribute__(value, "__dict__")
    cloned_dict = {
        k: _detach_snapshot_tree(
            v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
        for k, v in src_dict.items()
    }
    object.__setattr__(cloned, "__dict__", cloned_dict)
    fields_set = getattr(value, "__pydantic_fields_set__", None)
    if fields_set is not None:
        object.__setattr__(cloned, "__pydantic_fields_set__", set(fields_set))
    extra = getattr(value, "__pydantic_extra__", None)
    if extra is None:
        object.__setattr__(cloned, "__pydantic_extra__", None)
    else:
        object.__setattr__(
            cloned,
            "__pydantic_extra__",
            _detach_snapshot_tree(
                extra, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            ),
        )
    private = getattr(value, "__pydantic_private__", None)
    if private is None:
        object.__setattr__(cloned, "__pydantic_private__", None)
    else:
        object.__setattr__(
            cloned,
            "__pydantic_private__",
            {
                k: _detach_snapshot_tree(
                    v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
                for k, v in private.items()
            },
        )
    _copy_slots(
        value,
        cloned,
        memo,
        skip=_PYDANTIC_INTERNAL_SLOTS,
        reject_enums=reject_enums,
        wrap_enums=wrap_enums,
    )
    return cloned


def _clone_dataclass(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> Any:
    """Clone a dataclass instance preserving declared fields, extra state, and cycles."""
    dc_cls = type(value)
    cloned_dc = object.__new__(dc_cls)
    memo[id(value)] = cloned_dc
    if hasattr(value, "__dict__"):
        src_dict = object.__getattribute__(value, "__dict__")
        object.__setattr__(
            cloned_dc,
            "__dict__",
            {
                k: _detach_snapshot_tree(
                    v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
                for k, v in src_dict.items()
            },
        )
    _copy_slots(
        value,
        cloned_dc,
        memo,
        skip=frozenset({"__dict__", "__weakref__"}),
        reject_enums=reject_enums,
        wrap_enums=wrap_enums,
    )
    return cloned_dc


def _detach_callable(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach bound methods/partials or reject callables that cannot be safely isolated."""
    obj_id = id(value)
    if isinstance(value, FunctionType):
        raise TypeError(
            f"Cannot safely detach function/closure {value.__qualname__!r} without "
            "sharing closure, default, or global state"
        )
    if isinstance(value, (BuiltinMethodType, MethodWrapperType)):
        bound_self = getattr(value, "__self__", None)
        if bound_self is None or isinstance(bound_self, ModuleType):
            return True, value
        if isinstance(bound_self, type):
            if bound_self.__module__ == "builtins":
                return True, value
            raise TypeError(
                f"Cannot safely detach class-bound builtin method on {bound_self.__name__!r}"
            )
        copied_self = _detach_snapshot_tree(
            bound_self, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
        rebound = getattr(copied_self, value.__name__)
        memo[obj_id] = rebound
        return True, rebound
    if isinstance(value, MethodType):
        raise TypeError(
            f"Cannot safely detach Python bound method "
            f"{value.__func__.__qualname__!r} without sharing global state"
        )
    if type(value) is functools.partial:
        memo[obj_id] = _PARTIAL_IN_PROGRESS
        try:
            out_partial = functools.partial(
                _detach_snapshot_tree(
                    value.func, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                ),
                *(
                    _detach_snapshot_tree(
                        arg, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                    )
                    for arg in value.args
                ),
                **{
                    k: _detach_snapshot_tree(
                        v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                    )
                    for k, v in value.keywords.items()
                },
            )
            extra_dict = getattr(value, "__dict__", None)
            if extra_dict:
                for attr_name, attr_val in extra_dict.items():
                    setattr(
                        out_partial,
                        attr_name,
                        _detach_snapshot_tree(
                            attr_val,
                            memo,
                            reject_enums=reject_enums,
                            wrap_enums=wrap_enums,
                        ),
                    )
        except Exception:
            memo.pop(obj_id, None)
            raise
        memo[obj_id] = out_partial
        return True, out_partial
    return False, None


def _detach_memoryview(value: memoryview) -> memoryview:
    """Return a detached read-only memoryview preserving C-contiguous format and shape."""
    if not value.c_contiguous:
        raise TypeError(
            "Cannot safely detach non-contiguous memoryview without altering layout"
        )
    raw_view = memoryview(value.tobytes())
    if value.format == "B" and value.ndim == 1:
        return raw_view
    try:
        return raw_view.cast(value.format, shape=value.shape)
    except (TypeError, ValueError) as exc:
        raise TypeError(
            f"Cannot safely detach memoryview with format {value.format!r} "
            f"and shape {value.shape!r}"
        ) from exc


def _detach_numpy_or_buffer(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach exact NumPy arrays, structured scalars, and byte buffers."""
    obj_id = id(value)
    result: Any = None
    handled = True
    if type(value) is np.ndarray:
        detached_dtype = _validate_safe_numpy_dtype(value.dtype)
        out_arr: np.ndarray[Any, Any] = np.empty(value.shape, dtype=detached_dtype)
        memo[obj_id] = out_arr
        if value.dtype.hasobject:
            for idx, item in enumerate(value.flat):
                out_arr.flat[idx] = _detach_snapshot_tree(
                    item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
        else:
            np.copyto(out_arr, value, casting="unsafe")
        return True, out_arr
    if type(value) is np.void:
        detached_dtype = _validate_safe_numpy_dtype(value.dtype)
        if value.dtype.names and value.dtype.hasobject:
            detached_fields = tuple(
                _detach_snapshot_tree(
                    value[name], memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
                for name in value.dtype.names
            )
            result = np.array([detached_fields], dtype=detached_dtype)[0]
        else:
            arr_tmp: np.ndarray[Any, Any] = np.empty((), dtype=detached_dtype)
            arr_tmp[()] = value
            result = arr_tmp[()]
    elif type(value) is bytearray:
        result = bytearray(value)
    elif type(value) is memoryview:
        result = _detach_memoryview(value)
    else:
        handled = False
    if handled:
        memo[obj_id] = result
    return handled, result


def _detach_sequence_or_set(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach list, deque, set, tuple, frozenset, and slice containers."""
    obj_id = id(value)
    if type(value) is list:
        out_list: list[Any] = []
        memo[obj_id] = out_list
        out_list.extend(
            _detach_snapshot_tree(
                item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
            for item in value
        )
        return True, out_list
    if type(value) is deque:
        out_deque: deque[Any] = deque(maxlen=value.maxlen)
        memo[obj_id] = out_deque
        out_deque.extend(
            _detach_snapshot_tree(
                item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
            for item in value
        )
        return True, out_deque
    if type(value) is set:
        out_set: set[Any] = set()
        memo[obj_id] = out_set
        for item in value:
            out_set.add(
                _detach_snapshot_tree(
                    item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
            )
        return True, out_set
    result: Any = None
    handled = True
    if type(value) is tuple:
        items = [
            _detach_snapshot_tree(
                item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
            for item in value
        ]
        result = memo[obj_id] if obj_id in memo else tuple(items)
    elif (
        isinstance(value, tuple)
        and hasattr(type(value), "_fields")
        and not hasattr(value, "__dict__")
        and not _iter_custom_slots(type(value), frozenset({"__dict__", "__weakref__"}))
    ):
        items = [
            _detach_snapshot_tree(
                item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
            for item in value
        ]
        result = memo[obj_id] if obj_id in memo else type(value)(*items)
    elif type(value) is frozenset:
        result = frozenset(
            _detach_snapshot_tree(
                item, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
            for item in value
        )
    elif type(value) is slice:
        result = slice(
            _detach_snapshot_tree(
                value.start, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            ),
            _detach_snapshot_tree(
                value.stop, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            ),
            _detach_snapshot_tree(
                value.step, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            ),
        )
    else:
        handled = False
    if handled:
        memo[obj_id] = result
    return handled, result


def _detach_plain_instance(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach plain Python class instances preserving ``__dict__``, ``__slots__``, and cycles."""
    skip = frozenset({"__dict__", "__weakref__"})
    if (
        isinstance(value, type)
        or type(value).__new__ is not object.__new__
        or hasattr(value, "__deepcopy__")
        or hasattr(value, "__copy__")
        or type(value).__reduce_ex__ is not object.__reduce_ex__
        or not (
            hasattr(value, "__dict__") or bool(_iter_custom_slots(type(value), skip))
        )
    ):
        return False, None
    cloned_obj = object.__new__(type(value))
    memo[id(value)] = cloned_obj
    if hasattr(value, "__dict__"):
        src_dict = object.__getattribute__(value, "__dict__")
        object.__setattr__(
            cloned_obj,
            "__dict__",
            {
                k: _detach_snapshot_tree(
                    v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                )
                for k, v in src_dict.items()
            },
        )
    _copy_slots(
        value,
        cloned_obj,
        memo,
        skip=skip,
        reject_enums=reject_enums,
        wrap_enums=wrap_enums,
    )
    return True, cloned_obj


_PRIMITIVE_SUBCLASS_BASES: tuple[type[Any], ...] = (str, bytes, int, float, complex)


def _detach_primitive_subclass(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach subclasses of immutable primitive types (e.g. ``class Label(str)``)."""
    if (
        isinstance(value, type)
        or hasattr(value, "__deepcopy__")
        or hasattr(value, "__copy__")
        or not isinstance(value, _PRIMITIVE_SUBCLASS_BASES)
    ):
        return False, None
    skip = frozenset({"__dict__", "__weakref__"})
    val_cls = type(value)
    for base in _PRIMITIVE_SUBCLASS_BASES:
        if issubclass(val_cls, base) and val_cls is not base:
            if val_cls.__new__ is not base.__new__:
                return False, None
            cloned_sub = base.__new__(val_cls, value)
            memo[id(value)] = cloned_sub
            if hasattr(value, "__dict__"):
                src_dict = object.__getattribute__(value, "__dict__")
                object.__setattr__(
                    cloned_sub,
                    "__dict__",
                    {
                        k: _detach_snapshot_tree(
                            v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
                        )
                        for k, v in src_dict.items()
                    },
                )
            _copy_slots(
                value,
                cloned_sub,
                memo,
                skip=skip,
                reject_enums=reject_enums,
                wrap_enums=wrap_enums,
            )
            return True, cloned_sub
    return False, None


def _detach_suews_internal_ref(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach SUEWS internal weak-config helpers onto copied targets in *memo*."""
    del reject_enums, wrap_enums
    from ...core.type import _SchemeActiveGuard, _WeakConfigRef  # noqa: PLC0415

    if type(value) in {_WeakConfigRef, _SchemeActiveGuard}:
        copied = value.__deepcopy__(memo)
        memo[id(value)] = copied
        return True, copied
    return False, None


_SAFE_DEFAULTDICT_FACTORIES: frozenset[Any] = frozenset({
    None,
    bool,
    int,
    float,
    complex,
    str,
    bytes,
    bytearray,
    list,
    tuple,
    dict,
    set,
    frozenset,
    deque,
    OrderedDict,
    Counter,
})


def _detach_mapping_value(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> tuple[bool, Any]:
    """Detach supported mapping types while preserving ``defaultdict``/``OrderedDict``/``Counter``."""
    val_type = type(value)
    out_map: dict[Any, Any]
    if val_type in {dict, MappingProxyType}:
        out_map = {}
    elif val_type is OrderedDict:
        out_map = OrderedDict()
    elif val_type is Counter:
        out_map = Counter()
    elif val_type is defaultdict:
        factory = value.default_factory
        safe_factory = (
            factory
            if factory in _SAFE_DEFAULTDICT_FACTORIES
            else _detach_snapshot_tree(
                factory, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
        )
        out_map = defaultdict(safe_factory)
    else:
        return False, None
    memo[id(value)] = out_map
    for k, v in value.items():
        out_map[
            _detach_snapshot_tree(
                k, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
            )
        ] = _detach_snapshot_tree(
            v, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
    return True, out_map


def _detach_container_or_object(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> Any:
    """Detach Python containers, callables, NumPy values, and plain Python objects."""
    if value is Ellipsis or value is NotImplemented:
        return value
    if isinstance(value, np.dtype):
        copied_dt = _validate_safe_numpy_dtype(value)
        if value.names is None and value.metadata is None:
            return copied_dt
        memo[id(value)] = copied_dt
        return copied_dt
    if type(value) in {datetime.datetime, datetime.time} and value.tzinfo is not None:
        detached_tz = _detach_snapshot_tree(
            value.tzinfo, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
        replaced = value.replace(tzinfo=detached_tz)
        memo[id(value)] = replaced
        return replaced
    for handler in (
        _detach_mapping_value,
        _detach_suews_internal_ref,
        _detach_sequence_or_set,
        _detach_callable,
        _detach_numpy_or_buffer,
        _detach_plain_instance,
        _detach_primitive_subclass,
    ):
        handled, result = handler(
            value, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
        if handled:
            return result
    raise TypeError(
        f"Cannot safely detach object of type {type(value).__name__!r} with "
        "opaque or custom copy semantics"
    )


def _detach_enum_value(
    value: Enum,
    *,
    reject_enums: bool,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None,
) -> Any:
    """Apply export policy to an ``Enum`` member during snapshot detachment."""
    if reject_enums:
        raise TypeError(
            f"Cannot produce an independent copy containing Enum singleton "
            f"{type(value).__name__}.{value.name}"
        )
    if wrap_enums is not None:
        return (
            wrap_enums[id(value)]
            if id(value) in wrap_enums
            else ReadOnlyEnumProxy(value)
        )
    return value


def _detach_snapshot_tree(
    value: Any,
    memo: dict[int, Any],
    *,
    reject_enums: bool = False,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None = None,
) -> Any:
    """Recursively detach *value*, isolating ``MappingProxyType``, bound methods, and enums."""
    if (
        value is None
        or _is_exact_immutable_scalar(value)
        or type(value) in _MODEL_EXTRA_IMMUTABLE_TYPES
        or isinstance(
            value,
            (
                ReadOnlyModelProxy,
                ReadOnlyDataclassProxy,
                ReadOnlyEnumProxy,
                ReadOnlyFieldInfo,
                ReadOnlyTypeInfo,
                _FrozenArraySpec,
                _FrozenSliceSpec,
                _ReadOnlyMaterialisingMapping,
                _ReadOnlyMaterialisingTuple,
            ),
        )
    ):
        return value
    if isinstance(value, Enum):
        return _detach_enum_value(
            value, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
    obj_id = id(value)
    if obj_id in memo:
        cached = memo[obj_id]
        if cached is _PARTIAL_IN_PROGRESS:
            raise TypeError(
                "Cannot safely detach cyclic functools.partial without sharing mutable state"
            )
        return cached
    if isinstance(value, BaseModel):
        return _clone_base_model(
            value, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
    if is_dataclass(value) and not isinstance(value, type):
        return _clone_dataclass(
            value, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
        )
    return _detach_container_or_object(
        value, memo, reject_enums=reject_enums, wrap_enums=wrap_enums
    )


def _safe_snapshot_copy(
    value: Any,
    *,
    reject_enums: bool = False,
    wrap_enums: Mapping[int, ReadOnlyEnumProxy] | None = None,
) -> Any:
    """Return a detached snapshot copy of *value* isolating bound methods and read-only views."""
    return _detach_snapshot_tree(
        value, {}, reject_enums=reject_enums, wrap_enums=wrap_enums
    )


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


_STD_ENUM_EQ_METHODS = frozenset({object.__eq__, int.__eq__, float.__eq__, str.__eq__})
_STD_ENUM_HASH_METHODS = frozenset({
    Enum.__hash__,
    object.__hash__,
    int.__hash__,
    float.__hash__,
    str.__hash__,
})


def _compare_enum_members(
    left: Enum, left_val: Any, right: Enum, right_val: Any
) -> bool:
    """Compare two validated ``Enum`` members purely without custom ``__eq__`` hooks."""
    left_eq = type(left).__eq__
    right_eq = type(right).__eq__
    if right_eq not in _STD_ENUM_EQ_METHODS:
        raise TypeError(
            f"Cannot safely compare Enum proxy with custom __eq__ on {type(right).__name__!r}"
        )
    if left is right:
        return True
    if left_eq is object.__eq__ or right_eq is object.__eq__:
        return False
    return bool(left_val == right_val)


def _enum_proxy_equals(member: Enum, frozen_value: Any, other: object) -> bool | Any:
    """Compare an enum proxy safely without passing live singletons to reflected hooks."""
    member_eq = type(member).__eq__
    if member_eq not in _STD_ENUM_EQ_METHODS:
        raise TypeError(
            f"Cannot safely compare Enum proxy with custom __eq__ on {type(member).__name__!r}"
        )
    if isinstance(other, ReadOnlyEnumProxy):
        other_member, _other_name, other_val, _other_hash, _ = _ENUM_STATE[id(other)]
        return _compare_enum_members(member, frozen_value, other_member, other_val)
    if isinstance(other, Enum):
        return _compare_enum_members(member, frozen_value, other, other.value)
    if _is_exact_immutable_scalar(other):
        return False if member_eq is object.__eq__ else bool(frozen_value == other)
    return NotImplemented


def _compute_enum_snapshot_hash(
    member: Enum, frozen_name: str, frozen_value: Any
) -> int | None:
    """Compute a snapshotted hash for *member* when its ``__hash__`` is standard."""
    member_hash = type(member).__hash__
    if member_hash not in _STD_ENUM_HASH_METHODS:
        return None
    if member_hash is object.__hash__:
        return object.__hash__(member)
    if member_hash is Enum.__hash__:
        return hash(frozen_name)
    return hash(frozen_value)


_STD_ENUM_BOOL_METHODS = frozenset({
    None,
    int.__bool__,
    float.__bool__,
    bool.__bool__,
    Flag.__bool__,
})
_STD_ENUM_LEN_METHODS = frozenset({
    None,
    str.__len__,
    bytes.__len__,
    tuple.__len__,
})


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
        frozen_name = str(member.name)
        frozen_value = _wrap_readonly(member.value)
        frozen_hash = _compute_enum_snapshot_hash(member, frozen_name, frozen_value)
        extra_attrs = _snapshot_enum_extra_attrs(member)
        key = id(self)
        _ENUM_STATE[key] = (
            member,
            frozen_name,
            frozen_value,
            frozen_hash,
            extra_attrs,
        )
        weakref.finalize(self, _ENUM_STATE.pop, key, None)

    @property
    def enum_type(self) -> ReadOnlyTypeInfo:
        """Immutable type descriptor for the underlying ``Enum`` class."""
        return _freeze_type_info(type(_ENUM_STATE[id(self)][0]))

    @property
    def name(self) -> str:
        """Snapshotted name of the wrapped ``Enum`` member."""
        return _ENUM_STATE[id(self)][1]

    @property
    def value(self) -> Any:
        """Frozen value of the wrapped ``Enum`` member."""
        return _materialise_readonly(_ENUM_STATE[id(self)][2])

    def unwrap_copy(self) -> Enum:
        """Reject unwrapping because ``Enum`` members are shared process-wide singletons."""
        member, frozen_name, _val, _hash, _extra = _ENUM_STATE[id(self)]
        raise TypeError(
            f"Cannot produce an independent copy of Enum singleton "
            f"{type(member).__name__}.{frozen_name}; inspect the ReadOnlyEnumProxy "
            "or read .value instead"
        )

    def __getattr__(self, name: str) -> Any:
        """Resolve snapshotted public attributes on the wrapped ``Enum`` member."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only enum view {self.enum_type.name!r} "
                f"does not expose private attribute {name!r}"
            )
        extra_attrs = _ENUM_STATE[id(self)][4]
        if name in extra_attrs:
            return extra_attrs[name]
        raise AttributeError(
            f"Read-only enum view {self.enum_type.name!r} has no readable attribute {name!r}"
        )

    def __setattr__(self, name: str, value: Any) -> None:
        """Reject every attribute assignment on the read-only enum proxy."""
        raise TypeError(
            f"Cannot mutate attribute {name!r} on read-only enum view "
            f"{self.enum_type.name!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only enum proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only enum view "
            f"{self.enum_type.name!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare snapshotted enum equality without leaking the live singleton."""
        member, _name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        return _enum_proxy_equals(member, frozen_value, other)

    def __hash__(self) -> int:
        """Return the snapshotted hash of the wrapped ``Enum`` member without user hooks."""
        member, _name, _value, frozen_hash, _extra = _ENUM_STATE[id(self)]
        if frozen_hash is None:
            raise TypeError(
                f"Cannot safely hash Enum proxy with custom __hash__ on "
                f"{type(member).__name__!r}"
            )
        return frozen_hash

    def __bool__(self) -> bool:
        """Compute truthiness purely from snapshotted state without calling live enum hooks."""
        member, _name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        cls = type(member)
        cls_bool = getattr(cls, "__bool__", None)
        if cls_bool not in _STD_ENUM_BOOL_METHODS:
            raise TypeError(
                f"Cannot safely evaluate custom __bool__ on Enum proxy {cls.__name__!r}"
            )
        if cls_bool is None:
            cls_len = getattr(cls, "__len__", None)
            if cls_len not in _STD_ENUM_LEN_METHODS:
                raise TypeError(
                    f"Cannot safely evaluate custom __len__ on Enum proxy {cls.__name__!r}"
                )
            return bool(frozen_value) if cls_len is not None else True
        return bool(frozen_value)

    def __int__(self) -> int:
        """Support ``int()`` conversion purely from snapshotted state."""
        member, _name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        cls = type(member)
        cls_int = getattr(cls, "__int__", None)
        if isinstance(member, int) and not isinstance(member, bool):
            if cls_int is not int.__int__:
                raise TypeError(
                    f"Cannot safely evaluate custom __int__ on Enum proxy {cls.__name__!r}"
                )
            return int(frozen_value)
        if (
            cls_int is not None
            and cls.__module__.startswith("supy.data_model.")
            and isinstance(frozen_value, int)
            and not isinstance(frozen_value, bool)
        ):
            return int(frozen_value)
        if cls_int is not None:
            raise TypeError(
                f"Cannot safely evaluate custom __int__ on Enum proxy {cls.__name__!r}"
            )
        raise TypeError(
            f"int() argument must be an integer Enum proxy, not {cls.__name__!r}"
        )

    def __index__(self) -> int:
        """Support integer indexing for ``IntEnum`` / ``IntFlag`` proxies purely from snapshot."""
        member, _name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        cls = type(member)
        if isinstance(member, int) and not isinstance(member, bool):
            if getattr(cls, "__index__", None) is not int.__index__:
                raise TypeError(
                    f"Cannot safely evaluate custom __index__ on Enum proxy {cls.__name__!r}"
                )
            return int(frozen_value)
        raise TypeError(f"'{cls.__name__}' object cannot be interpreted as an integer")

    def __float__(self) -> float:
        """Support ``float()`` conversion purely from snapshotted state."""
        member, _name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        cls = type(member)
        cls_float = getattr(cls, "__float__", None)
        if isinstance(member, (int, float)) and not isinstance(member, bool):
            if cls_float not in {float.__float__, int.__float__}:
                raise TypeError(
                    f"Cannot safely evaluate custom __float__ on Enum proxy {cls.__name__!r}"
                )
            return float(frozen_value)
        if cls_float is not None:
            raise TypeError(
                f"Cannot safely evaluate custom __float__ on Enum proxy {cls.__name__!r}"
            )
        raise TypeError(
            f"float() argument must be a numeric Enum proxy, not {cls.__name__!r}"
        )

    def __str__(self) -> str:
        """Return a pure string representation built from snapshotted enum state."""
        member, frozen_name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        if isinstance(member, str):
            return str(frozen_value)
        if isinstance(member, int) and not isinstance(member, bool):
            return str(int(frozen_value))
        return f"{type(member).__name__}.{frozen_name}"

    def __repr__(self) -> str:
        """Return a pure representation built from snapshotted enum metadata."""
        member, frozen_name, frozen_value, _hash, _extra = _ENUM_STATE[id(self)]
        return (
            f"ReadOnlyEnumProxy(<{type(member).__name__}.{frozen_name}: "
            f"{_safe_repr_value(frozen_value, set())}>)"
        )

    def __copy__(self) -> ReadOnlyEnumProxy:
        """Return ``self`` because ``ReadOnlyEnumProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyEnumProxy:
        """Return ``self`` because ``ReadOnlyEnumProxy`` is deeply immutable."""
        return self


@dataclass(frozen=True, slots=True)
class ReadOnlyTypeInfo:
    """Deeply immutable type descriptor exposing class identity without the live class object."""

    name: str
    qualname: str
    module: str

    def __str__(self) -> str:
        """Return the qualified name of the described type."""
        return self.qualname


def _freeze_type_info(cls: type[Any]) -> ReadOnlyTypeInfo:
    """Build an immutable :class:`ReadOnlyTypeInfo` for *cls*."""
    return ReadOnlyTypeInfo(
        name=cls.__name__,
        qualname=cls.__qualname__,
        module=cls.__module__,
    )


def _freeze_annotation(annotation: Any) -> ReadOnlyTypeInfo | str | None:
    """Snapshot a Pydantic field annotation without exposing live class objects."""
    if annotation is None:
        return None
    if isinstance(annotation, type):
        return _freeze_type_info(annotation)
    return repr(annotation)


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


@dataclass(frozen=True, slots=True)
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
        return _materialise_readonly(object.__getattribute__(self, name))

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


def _iter_instance_state_children(obj: Any, skip: frozenset[str]) -> list[Any]:
    """Return values from ``__dict__`` and custom ``__slots__`` on *obj*."""
    out: list[Any] = []
    if hasattr(obj, "__dict__"):
        out.extend(object.__getattribute__(obj, "__dict__").values())
    for slot_name in _iter_custom_slots(type(obj), skip):
        try:
            out.append(object.__getattribute__(obj, slot_name))
        except AttributeError:
            continue
    return out


def _iter_dtype_metadata_children(dtype: np.dtype[Any]) -> list[Any]:
    """Return metadata mappings reachable from *dtype* and its nested field/subarray dtypes."""
    out: list[Any] = []
    if dtype.metadata is not None:
        out.append(dtype.metadata)
    if dtype.fields is not None:
        for field_info in dtype.fields.values():
            out.extend(_iter_dtype_metadata_children(field_info[0]))
    if dtype.subdtype is not None:
        out.extend(_iter_dtype_metadata_children(dtype.subdtype[0]))
    return out


def _iter_non_container_children(obj: Any) -> list[Any]:
    """Return child objects for non-container types traversed by ``_detach_snapshot_tree``."""
    children: list[Any] = []
    if type(obj) in {datetime.datetime, datetime.time} and obj.tzinfo is not None:
        children.append(obj.tzinfo)
    elif isinstance(obj, np.dtype):
        children.extend(_iter_dtype_metadata_children(obj))
    elif isinstance(obj, np.ndarray):
        children.extend(_iter_dtype_metadata_children(obj.dtype))
        if obj.dtype.hasobject:
            children.extend(obj.flat)
    elif isinstance(obj, np.void):
        children.extend(_iter_dtype_metadata_children(obj.dtype))
        if obj.dtype.names:
            children.extend(obj[name] for name in obj.dtype.names)
    elif isinstance(obj, (BuiltinMethodType, MethodWrapperType, MethodType)):
        bound_self = getattr(obj, "__self__", None)
        if bound_self is not None and not isinstance(bound_self, (ModuleType, type)):
            children.append(bound_self)
    elif isinstance(obj, functools.partial):
        children.extend((obj.func, obj.args, obj.keywords))
        extra_dict = getattr(obj, "__dict__", None)
        if extra_dict:
            children.append(extra_dict)
    elif not isinstance(obj, (type, bytearray, memoryview)):
        children.extend(
            _iter_instance_state_children(obj, frozenset({"__dict__", "__weakref__"}))
        )
    return children


def _iter_snapshot_children(
    obj: Any,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]] | None,
) -> list[Any]:
    """Return all child objects reachable from *obj* matching ``_detach_snapshot_tree``."""
    children: list[Any] = []
    if isinstance(obj, BaseModel):
        model_cls = type(obj)
        if field_snapshots is not None and model_cls not in field_snapshots:
            field_snapshots[model_cls] = _freeze_model_fields(model_cls)
        children.extend(_iter_instance_state_children(obj, _PYDANTIC_INTERNAL_SLOTS))
        for attr in ("model_extra", "__pydantic_private__"):
            mapping = getattr(obj, attr, None)
            if isinstance(mapping, Mapping):
                children.append(mapping)
    elif is_dataclass(obj) and not isinstance(obj, type):
        children.extend(
            _iter_instance_state_children(obj, frozenset({"__dict__", "__weakref__"}))
        )
    elif isinstance(obj, Mapping):
        for k, v in obj.items():
            children.extend((k, v))
    elif isinstance(obj, (list, tuple, deque, set, frozenset)):
        children.extend(obj)
    elif type(obj) is slice:
        children.extend((obj.start, obj.stop, obj.step))
    else:
        children.extend(_iter_non_container_children(obj))
    return children


def _is_standard_dataclass_hook(cls: type[Any], hook_name: str) -> bool:
    """Return True when *cls* uses the compiler-generated dataclass ``__eq__``/``__hash__``."""
    hook = getattr(cls, hook_name, None)
    if hook is None:
        return True
    if hook is getattr(object, hook_name, None):
        return True
    code = getattr(hook, "__code__", None)
    return code is not None and code.co_filename == "<string>"


_STD_EQ_METHODS = frozenset({
    object.__eq__,
    dict.__eq__,
    OrderedDict.__eq__,
    Counter.__eq__,
    MappingProxyType.__eq__,
    list.__eq__,
    tuple.__eq__,
    deque.__eq__,
    set.__eq__,
    frozenset.__eq__,
    slice.__eq__,
    np.ndarray.__eq__,
    np.void.__eq__,
    np.dtype.__eq__,
    bytearray.__eq__,
    memoryview.__eq__,
    BuiltinMethodType.__eq__,
    MethodWrapperType.__eq__,
    MethodType.__eq__,
    functools.partial.__eq__,
    weakref.ReferenceType.__eq__,
    str.__eq__,
    bytes.__eq__,
    int.__eq__,
    float.__eq__,
    complex.__eq__,
    datetime.datetime.__eq__,
    datetime.time.__eq__,
    datetime.date.__eq__,
    datetime.timedelta.__eq__,
})
_STD_HASH_METHODS = frozenset({
    None,
    object.__hash__,
    dict.__hash__,
    MappingProxyType.__hash__,
    list.__hash__,
    tuple.__hash__,
    deque.__hash__,
    set.__hash__,
    frozenset.__hash__,
    slice.__hash__,
    np.ndarray.__hash__,
    np.void.__hash__,
    np.dtype.__hash__,
    bytearray.__hash__,
    memoryview.__hash__,
    BuiltinMethodType.__hash__,
    MethodWrapperType.__hash__,
    MethodType.__hash__,
    functools.partial.__hash__,
    weakref.ReferenceType.__hash__,
    str.__hash__,
    bytes.__hash__,
    int.__hash__,
    float.__hash__,
    complex.__hash__,
    datetime.datetime.__hash__,
    datetime.time.__hash__,
    datetime.date.__hash__,
    datetime.timedelta.__hash__,
})


def _has_custom_dunder(root: Any, dunder: str) -> bool:
    """Return True if any reachable object in *root* defines a non-standard ``__eq__``/``__hash__``."""
    from ...core.type import RefValue  # noqa: PLC0415

    std_model_hook = getattr(BaseModel, dunder, None)
    std_ref_hook = getattr(RefValue, dunder, None) if dunder == "__eq__" else None
    std_enum_hooks = (
        _STD_ENUM_EQ_METHODS if dunder == "__eq__" else _STD_ENUM_HASH_METHODS
    )
    std_generic_hooks = _STD_EQ_METHODS if dunder == "__eq__" else _STD_HASH_METHODS
    visited: set[int] = set()
    stack: list[Any] = [root]
    while stack:
        obj = stack.pop()
        if obj is None or _is_exact_immutable_scalar(obj):
            continue
        if isinstance(obj, Enum):
            if getattr(type(obj), dunder, None) not in std_enum_hooks:
                return True
            continue
        obj_id = id(obj)
        if obj_id in visited:
            continue
        visited.add(obj_id)
        cls = type(obj)
        if isinstance(obj, BaseModel):
            hook = getattr(cls, dunder, None)
            is_pydantic_hash = (
                dunder == "__hash__"
                and getattr(hook, "__module__", None)
                == "pydantic._internal._model_construction"
                and getattr(hook, "__qualname__", "")
                == "make_hash_func.<locals>.hash_func"
            )
            if (
                hook is not std_model_hook
                and hook is not std_ref_hook
                and not is_pydantic_hash
            ):
                return True
        elif is_dataclass(obj) and not isinstance(obj, type):
            if not _is_standard_dataclass_hook(cls, dunder):
                return True
        elif getattr(cls, dunder, None) not in std_generic_hooks:
            return True
        stack.extend(_iter_snapshot_children(obj, None))
    return False


def _get_vetted_suews_hooks() -> tuple[
    frozenset[Any],
    frozenset[tuple[type[Any], str]],
    frozenset[tuple[type[Any], str]],
]:
    """Return exact vetted SUEWS core serializers, computed fields, and properties."""
    from ...core.human_activity import IrrigationParams  # noqa: PLC0415
    from ...core.hydro import StorageDrainParams  # noqa: PLC0415
    from ...core.model import ModelControl, OutputControl  # noqa: PLC0415
    from ...core.site import (  # noqa: PLC0415
        SiteProperties,
        SnowParams,
        SoilObservationConfig,
        StebbsProperties,
    )
    from ...core.type import RefValue  # noqa: PLC0415

    funcs = (
        RefValue._serialize_with_value_key,
        SiteProperties._serialise_optional_snow,
        IrrigationParams._serialise_irrigation_doy,
        SnowParams._serialise_snow_scalar,
        SnowParams._serialise_snow_profile,
        SoilObservationConfig._serialise_soil_observation_field,
        StorageDrainParams._dump_storage,
    )
    return (
        frozenset(getattr(fn, "__func__", fn) for fn in funcs),
        frozenset({(StebbsProperties, "deep_soil_temperature")}),
        frozenset({
            (ModelControl, "forcing_file"),
            (ModelControl, "output_file"),
            (OutputControl, "path"),
        }),
    )


def _schema_has_disallowed_hook(
    schema: Any,
    allowed_sers: frozenset[Any],
    allowed_computed: frozenset[tuple[type[Any], str]],
) -> bool:
    """Return True if *schema* contains any custom serializer or computed field outside allowlists."""
    seen: set[int] = set()
    stack: list[tuple[Any, type[Any] | None]] = [(schema, None)]
    while stack:
        node, owner_cls = stack.pop()
        if isinstance(node, dict):
            node_id = id(node)
            if node_id in seen:
                continue
            seen.add(node_id)
            next_owner = (
                node.get("cls", owner_cls)
                if node.get("type") in {"model", "dataclass"}
                else owner_cls
            )
            ser = node.get("serialization")
            if isinstance(ser, dict) and ser.get("type") in {
                "function-plain",
                "function-wrap",
            }:
                fn = ser.get("function")
                if getattr(fn, "__func__", fn) not in allowed_sers:
                    return True
            if (
                node.get("type") == "computed-field"
                and (next_owner, node.get("property_name")) not in allowed_computed
            ):
                return True
            stack.extend((v, next_owner) for v in node.values())
        elif isinstance(node, (list, tuple)):
            node_id = id(node)
            if node_id in seen:
                continue
            seen.add(node_id)
            stack.extend((item, owner_cls) for item in node)
    return False


def _pydantic_type_has_disallowed_hooks(
    cls: type[Any],
    allowed_sers: frozenset[Any],
    allowed_computed: frozenset[tuple[type[Any], str]],
) -> bool:
    """Return True if a Pydantic model or dataclass defines disallowed serializers, dump overrides, or computed fields."""
    from ...core.type import RefValue  # noqa: PLC0415

    if issubclass(cls, BaseModel) and cls.model_dump not in {
        BaseModel.model_dump,
        RefValue.model_dump,
    }:
        return True
    decorators = getattr(cls, "__pydantic_decorators__", None)
    if decorators is not None:
        if any(
            getattr(dec.func, "__func__", dec.func) not in allowed_sers
            for dec in decorators.field_serializers.values()
        ) or any(
            getattr(dec.func, "__func__", dec.func) not in allowed_sers
            for dec in decorators.model_serializers.values()
        ):
            return True
        if any(
            (cls, name) not in allowed_computed
            for name in getattr(decorators, "computed_fields", {})
        ):
            return True
    computed_fields = getattr(cls, "model_computed_fields", None)
    if isinstance(computed_fields, Mapping) and any(
        (cls, name) not in allowed_computed for name in computed_fields
    ):
        return True
    return _schema_has_disallowed_hook(
        getattr(cls, "__pydantic_core_schema__", None),
        allowed_sers,
        allowed_computed,
    )


def _validate_enum_dump_isolation(root: BaseModel, *, has_user_fallback: bool) -> None:
    """Enforce vetted-hook isolation on ``model_dump`` independently of Enum presence."""
    if has_user_fallback:
        raise TypeError(
            "Cannot safely run unvetted fallback serializer on a read-only model proxy "
            "(unvetted hooks can access shared class/global state or Enum singletons)"
        )
    allowed_sers, allowed_computed, _allowed_props = _get_vetted_suews_hooks()
    visited: set[int] = set()
    stack: list[Any] = [root]
    while stack:
        obj = stack.pop()
        if obj is None or _is_exact_immutable_scalar(obj):
            continue
        if isinstance(obj, Enum):
            if not _is_immutable_enum(obj):
                raise TypeError(
                    f"Cannot safely serialize Enum member "
                    f"{type(obj).__name__}.{obj.name} with mutable payload"
                )
            continue
        obj_id = id(obj)
        if obj_id in visited:
            continue
        visited.add(obj_id)
        if (
            isinstance(obj, BaseModel)
            or (is_dataclass(obj) and not isinstance(obj, type))
        ) and _pydantic_type_has_disallowed_hooks(
            type(obj), allowed_sers, allowed_computed
        ):
            raise TypeError(
                f"Cannot safely run unvetted custom serializer, dump override, or "
                f"computed field on {type(obj).__name__!r} (unvetted hooks can "
                "access shared class/global state or Enum singletons)"
            )
        stack.extend(_iter_snapshot_children(obj, None))


def _safe_repr_container(value: Any, seen: set[int]) -> str | None:
    """Format frozen mapping, tuple, or frozenset containers with cycle protection."""
    obj_id = id(value)
    if obj_id in seen:
        return "..."
    if isinstance(value, Mapping):
        seen.add(obj_id)
        try:
            items = ", ".join(
                f"{_safe_repr_value(k, seen)}: {_safe_repr_value(v, seen)}"
                for k, v in value.items()
            )
            return f"{{{items}}}"
        finally:
            seen.discard(obj_id)
    if isinstance(value, tuple):
        seen.add(obj_id)
        try:
            items = ", ".join(_safe_repr_value(v, seen) for v in value)
            suffix = "," if len(value) == 1 else ""
            return f"({items}{suffix})"
        finally:
            seen.discard(obj_id)
    if isinstance(value, frozenset):
        seen.add(obj_id)
        try:
            items = ", ".join(_safe_repr_value(v, seen) for v in value)
            return f"frozenset({{{items}}})"
        finally:
            seen.discard(obj_id)
    return None


def _safe_repr_value(value: Any, seen: set[int]) -> str:
    """Format a frozen value without invoking user-defined ``__repr__`` or properties."""
    if (
        value is None
        or _is_exact_immutable_scalar(value)
        or isinstance(value, (ReadOnlyEnumProxy, np.ndarray))
    ):
        return repr(value)
    if isinstance(value, _LAZY_FROZEN_SPECS):
        return repr(value.materialise())
    if isinstance(value, ReadOnlyModelProxy):
        return _format_model_proxy_repr(value, seen)
    if isinstance(value, ReadOnlyDataclassProxy):
        return _format_dataclass_proxy_repr(value, seen)
    container_repr = _safe_repr_container(value, seen)
    return container_repr if container_repr is not None else f"<{type(value).__name__}>"


def _format_raw_field_repr(
    raw_value: Any,
    enum_snapshots: dict[int, ReadOnlyEnumProxy],
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]],
    seen: set[int],
) -> str:
    """Safely format one declared field value for proxy ``__repr__``."""
    try:
        frozen = _wrap_readonly(raw_value, enum_snapshots, field_snapshots)
    except TypeError:
        return f"<unfreezable {type(raw_value).__name__}>"
    return _safe_repr_value(frozen, seen)


def _format_dataclass_proxy_repr(proxy: ReadOnlyDataclassProxy, seen: set[int]) -> str:
    """Build ``ReadOnlyDataclassProxy.__repr__`` strictly from declared fields."""
    target, _cache, enum_snapshots, field_snapshots = _DATACLASS_STATE[id(proxy)]
    target_id = id(target)
    if target_id in seen:
        return f"ReadOnlyDataclassProxy({type(target).__name__}(...))"
    seen.add(target_id)
    try:
        parts = [
            f"{dc_field.name}="
            f"{_format_raw_field_repr(getattr(target, dc_field.name), enum_snapshots, field_snapshots, seen)}"
            for dc_field in dc_fields(type(target))
        ]
        return f"ReadOnlyDataclassProxy({type(target).__name__}({', '.join(parts)}))"
    finally:
        seen.discard(target_id)


def _format_model_proxy_repr(proxy: ReadOnlyModelProxy, seen: set[int]) -> str:
    """Build ``ReadOnlyModelProxy.__repr__`` strictly from declared fields without computed fields."""
    target, _cache, enum_snapshots, field_snapshots = _PROXY_STATE[id(proxy)]
    target_id = id(target)
    if target_id in seen:
        return f"ReadOnlyModelProxy({type(target).__name__}(...))"
    seen.add(target_id)
    try:
        model_cls = type(target)
        parts = [
            f"{name}="
            f"{_format_raw_field_repr(getattr(target, name), enum_snapshots, field_snapshots, seen)}"
            for name in model_cls.model_fields
        ]
        extra = getattr(target, "model_extra", None)
        if isinstance(extra, Mapping):
            parts.extend(
                f"{k}={_format_raw_field_repr(v, enum_snapshots, field_snapshots, seen)}"
                for k, v in extra.items()
            )
        return f"ReadOnlyModelProxy({model_cls.__name__}({', '.join(parts)}))"
    finally:
        seen.discard(target_id)


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
    stack: list[Any] = [root]
    while stack:
        obj = stack.pop()
        if obj is None or _is_exact_immutable_scalar(obj):
            continue
        if isinstance(obj, Enum):
            obj_id = id(obj)
            if obj_id not in enum_snapshots and _is_immutable_enum(obj):
                enum_snapshots[obj_id] = ReadOnlyEnumProxy(obj)
            continue
        obj_id = id(obj)
        if obj_id in visited:
            continue
        visited.add(obj_id)
        stack.extend(_iter_snapshot_children(obj, field_snapshots))
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
    def dataclass_type(self) -> ReadOnlyTypeInfo:
        """Immutable type descriptor for the underlying dataclass."""
        return _freeze_type_info(type(_DATACLASS_STATE[id(self)][0]))

    def __getattr__(self, name: str) -> Any:
        """Resolve declared dataclass fields and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only dataclass view {self.dataclass_type.name!r} "
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
                    raise TypeError(
                        f"Cannot safely evaluate unvetted property {name!r} on "
                        f"{dc_type.__name__!r} (unvetted hooks can access shared "
                        "class/global state or Enum singletons); inspect declared "
                        "fields directly"
                    )
                break

        raise AttributeError(
            f"Read-only dataclass view {dc_type.__name__!r} has no readable attribute {name!r}"
        )

    def __setattr__(self, name: str, value: Any) -> None:
        """Reject every attribute assignment on the read-only dataclass proxy."""
        raise TypeError(
            f"Cannot mutate attribute {name!r} on read-only dataclass view "
            f"{self.dataclass_type.name!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only dataclass proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only dataclass view "
            f"{self.dataclass_type.name!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare underlying dataclass equality on isolated copies preserving shared identities."""
        target = _DATACLASS_STATE[id(self)][0]
        if isinstance(other, ReadOnlyDataclassProxy):
            other_target = _DATACLASS_STATE[id(other)][0]
        elif is_dataclass(other) and not isinstance(other, type):
            other_target = other
        else:
            return NotImplemented
        if type(target).__eq__ is object.__eq__:
            return self is other or target is other_target
        if _has_custom_dunder(target, "__eq__") or _has_custom_dunder(
            other_target, "__eq__"
        ):
            raise TypeError(
                f"Cannot safely compare dataclasses with unvetted custom __eq__ "
                f"on {type(target).__name__!r} (may access shared state or Enum singletons)"
            )
        shared_memo: dict[int, Any] = {}
        left_copy = _detach_snapshot_tree(target, shared_memo)
        right_copy = _detach_snapshot_tree(other_target, shared_memo)
        return bool(left_copy == right_copy)

    def __hash__(self) -> int:
        """Hash an isolated copy of the underlying dataclass without unvetted user hooks."""
        target = _DATACLASS_STATE[id(self)][0]
        if type(target).__hash__ is object.__hash__:
            return object.__hash__(target)
        if _has_custom_dunder(target, "__hash__"):
            raise TypeError(
                f"Cannot safely hash dataclass with unvetted custom __hash__ "
                f"on {type(target).__name__!r} (may access shared state or Enum singletons)"
            )
        return hash(_safe_snapshot_copy(target))

    def unwrap_copy(self) -> Any:
        """Return an independent mutable deep copy of the underlying dataclass."""
        return _safe_snapshot_copy(_DATACLASS_STATE[id(self)][0], reject_enums=True)

    def __copy__(self) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyDataclassProxy:
        """Return ``self`` because ``ReadOnlyDataclassProxy`` is deeply immutable."""
        return self

    def __repr__(self) -> str:
        """Return a pure representation of the wrapped dataclass without user hooks."""
        return _format_dataclass_proxy_repr(self, set())


def _freeze_ndarray_spec(
    value: np.ndarray[Any, Any] | np.void,
    enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
    | None = None,
    active: set[int] | None = None,
) -> Any:
    """Wrap exact ``ndarray``/``np.void`` values using immutable storage or frozen containers."""
    if type(value) not in {np.ndarray, np.void}:
        raise TypeError(
            f"ReadOnlyModelProxy cannot safely freeze NumPy subclass {type(value).__name__!r}"
        )
    if active is None:
        active = set()
    if type(value) is np.void:
        if value.dtype.names:
            return _freeze_mapping_entries({
                name: _wrap_readonly(
                    value[name],
                    enum_snapshots,
                    field_snapshots,
                    active,
                )
                for name in value.dtype.names
            })
        return bytes(value)
    if value.dtype.hasobject:
        obj_id = id(value)
        if obj_id in active:
            raise TypeError("ReadOnlyModelProxy cannot safely freeze cyclic container")
        active.add(obj_id)
        try:
            if value.ndim == 0:
                return _wrap_readonly(
                    value.item(),
                    enum_snapshots,
                    field_snapshots,
                    active,
                )
            return _wrap_readonly(
                value.tolist(),
                enum_snapshots,
                field_snapshots,
                active,
            )
        finally:
            active.discard(obj_id)
    return _FrozenArraySpec(value)


def _materialise_readonly(value: Any) -> Any:
    """Materialise any top-level lazy frozen spec into a fresh read-only view."""
    return _materialise_frozen(value)


def _wrap_container_or_object(
    value: Any,
    enum_snapshots: dict[int, ReadOnlyEnumProxy] | None = None,
    field_snapshots: dict[type[BaseModel], Mapping[str, ReadOnlyFieldInfo]]
    | None = None,
    active: set[int] | None = None,
) -> Any:
    """Wrap containers, dataclasses, NumPy arrays, or reject unsupported mutable objects."""
    if active is None:
        active = set()
    if isinstance(value, Mapping):
        obj_id = id(value)
        if obj_id in active:
            raise TypeError("ReadOnlyModelProxy cannot safely freeze cyclic container")
        active.add(obj_id)
        try:
            return _freeze_mapping_entries({
                _wrap_readonly(
                    k, enum_snapshots, field_snapshots, active
                ): _wrap_readonly(v, enum_snapshots, field_snapshots, active)
                for k, v in value.items()
            })
        finally:
            active.discard(obj_id)
    if isinstance(value, (list, tuple, deque, Sequence)) and not isinstance(
        value, (str, bytes, bytearray, memoryview)
    ):
        obj_id = id(value)
        if obj_id in active:
            raise TypeError("ReadOnlyModelProxy cannot safely freeze cyclic container")
        active.add(obj_id)
        try:
            return _freeze_sequence_items([
                _wrap_readonly(item, enum_snapshots, field_snapshots, active)
                for item in value
            ])
        finally:
            active.discard(obj_id)
    if isinstance(value, (set, frozenset, AbstractSet)):
        obj_id = id(value)
        if obj_id in active:
            raise TypeError("ReadOnlyModelProxy cannot safely freeze cyclic container")
        active.add(obj_id)
        try:
            return frozenset(
                _wrap_readonly(item, enum_snapshots, field_snapshots, active)
                for item in value
            )
        finally:
            active.discard(obj_id)
    if isinstance(value, (np.ndarray, np.void)):
        return _freeze_ndarray_spec(value, enum_snapshots, field_snapshots, active)
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
    active: set[int] | None = None,
) -> Any:
    """Recursively wrap a detached Pydantic/Python value as read-only."""
    if active is None:
        active = set()
    if isinstance(value, Enum):
        return (
            enum_snapshots[id(value)]
            if enum_snapshots is not None and id(value) in enum_snapshots
            else ReadOnlyEnumProxy(value)
        )
    if type(value) is slice:
        return _freeze_slice_value(
            value,
            lambda bound: _wrap_readonly(
                bound, enum_snapshots, field_snapshots, active
            ),
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
                ReadOnlyTypeInfo,
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
    return _wrap_container_or_object(value, enum_snapshots, field_snapshots, active)


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
            annotation=_freeze_annotation(info.annotation),
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
    def model_type(self) -> ReadOnlyTypeInfo:
        """Immutable type descriptor for the underlying Pydantic ``BaseModel`` class."""
        return _freeze_type_info(type(_PROXY_STATE[id(self)][0]))

    @property
    def model_fields(self) -> Mapping[str, ReadOnlyFieldInfo]:
        """Detached, deeply immutable view of the model's declared Pydantic fields."""
        target, _cache, _enums, field_snapshots = _PROXY_STATE[id(self)]
        return field_snapshots[type(target)]

    @property
    def model_fields_set(self) -> frozenset[str]:
        """Frozen set of field names explicitly set on the model."""
        return frozenset(_PROXY_STATE[id(self)][0].model_fields_set)

    def model_dump(self, *args: Any, **kwargs: Any) -> dict[str, Any]:
        """Return a fresh detached dictionary serialisation of a working model copy."""
        target, _cache, enum_snapshots, _fields = _PROXY_STATE[id(self)]
        _validate_enum_dump_isolation(
            target, has_user_fallback=kwargs.get("fallback") is not None
        )
        if enum_snapshots and _enum_snapshots_drifted(enum_snapshots):
            raise TypeError(
                "Cannot safely serialise frozen model after live Enum singleton "
                "_name_ or _value_ was mutated externally"
            )
        working_copy = _safe_snapshot_copy(target)
        dumped = working_copy.model_dump(*args, **kwargs)
        return _safe_snapshot_copy(dumped, wrap_enums=enum_snapshots)

    def unwrap_copy(self) -> BaseModel:
        """Return an independent mutable deep copy of the underlying model."""
        return _safe_snapshot_copy(_PROXY_STATE[id(self)][0], reject_enums=True)

    def to_typed_view(self) -> Any:
        """Convert this read-only model into a deeply frozen :class:`TypedConfigView`."""
        from .typed_view import TypedConfigView  # noqa: PLC0415

        return TypedConfigView.from_config(self)

    def __getattr__(self, name: str) -> Any:
        """Resolve declared fields, computed fields, and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only model view {self.model_type.name!r} does not expose private attribute {name!r}"
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
            _allowed_sers, allowed_computed, _allowed_props = _get_vetted_suews_hooks()
            if (model_cls, name) not in allowed_computed:
                raise TypeError(
                    f"Cannot safely evaluate unvetted computed field {name!r} on "
                    f"{model_cls.__name__!r} (unvetted hooks can access shared "
                    "class/global state or Enum singletons)"
                )
            detached_result = _safe_snapshot_copy(
                getattr(_safe_snapshot_copy(target), name)
            )
            wrapped = _wrap_readonly(detached_result, enum_snapshots, field_snapshots)
            cache[name] = wrapped
            return _materialise_readonly(wrapped)

        for cls_in_mro in model_cls.__mro__:
            if name in cls_in_mro.__dict__:
                descriptor = cls_in_mro.__dict__[name]
                if isinstance(descriptor, property):
                    _allowed_sers, _allowed_comp, allowed_props = (
                        _get_vetted_suews_hooks()
                    )
                    if (cls_in_mro, name) not in allowed_props:
                        raise TypeError(
                            f"Cannot safely evaluate unvetted property {name!r} on "
                            f"{model_cls.__name__!r} (unvetted hooks can access shared "
                            "class/global state or Enum singletons); inspect declared "
                            "fields directly"
                        )
                    detached_result = _safe_snapshot_copy(
                        getattr(_safe_snapshot_copy(target), name)
                    )
                    wrapped = _wrap_readonly(
                        detached_result, enum_snapshots, field_snapshots
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
            f"{self.model_type.name!r}"
        )

    def __delattr__(self, name: str) -> None:
        """Reject every attribute deletion on the read-only proxy."""
        raise TypeError(
            f"Cannot delete attribute {name!r} on read-only model view "
            f"{self.model_type.name!r}"
        )

    def __eq__(self, other: object) -> bool:
        """Compare underlying model equality on isolated copies without mutating snapshot."""
        from ...core.type import RefValue  # noqa: PLC0415

        target = _PROXY_STATE[id(self)][0]
        if isinstance(other, ReadOnlyModelProxy):
            other_target = _PROXY_STATE[id(other)][0]
        elif isinstance(other, BaseModel):
            other_target = other
        elif isinstance(target, RefValue) and (
            _is_exact_immutable_scalar(other)
            or isinstance(other, (Enum, ReadOnlyEnumProxy))
        ):
            if isinstance(target.value, Enum):
                return bool(self.value == other)
            other_target = (
                _ENUM_STATE[id(other)][0]
                if isinstance(other, ReadOnlyEnumProxy)
                else other
            )
        else:
            return NotImplemented
        if type(target).__eq__ is object.__eq__:
            return self is other or target is other_target
        if _has_custom_dunder(target, "__eq__") or _has_custom_dunder(
            other_target, "__eq__"
        ):
            raise TypeError(
                f"Cannot safely compare models with unvetted custom __eq__ "
                f"on {type(target).__name__!r} (may access shared state or Enum singletons)"
            )
        shared_memo: dict[int, Any] = {}
        left_copy = _detach_snapshot_tree(target, shared_memo)
        right_copy = _detach_snapshot_tree(other_target, shared_memo)
        return bool(left_copy == right_copy)

    def __hash__(self) -> int:
        """Hash an isolated copy of the underlying model without unvetted user hooks."""
        target = _PROXY_STATE[id(self)][0]
        if type(target).__hash__ is object.__hash__:
            return object.__hash__(target)
        if _has_custom_dunder(target, "__hash__"):
            raise TypeError(
                f"Cannot safely hash model with unvetted custom __hash__ "
                f"on {type(target).__name__!r} (may access shared state or Enum singletons)"
            )
        return hash(_safe_snapshot_copy(target))

    def __copy__(self) -> ReadOnlyModelProxy:
        """Return ``self`` because ``ReadOnlyModelProxy`` is deeply immutable."""
        return self

    def __deepcopy__(self, _memo: dict[int, Any]) -> ReadOnlyModelProxy:
        """Return ``self`` because ``ReadOnlyModelProxy`` is deeply immutable."""
        return self

    def __repr__(self) -> str:
        """Return a pure representation of the wrapped model without computed fields."""
        return _format_model_proxy_repr(self, set())

    def __dir__(self) -> list[str]:
        """List readable fields and inspection helpers exposed by the proxy."""
        model_cls = type(_PROXY_STATE[id(self)][0])
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


def _enum_snapshots_drifted(
    enum_snapshots: Mapping[int, ReadOnlyEnumProxy],
) -> bool:
    """Return True if any snapshotted Enum singleton had its live ``_name_`` or ``_value_`` mutated."""
    for proxy in enum_snapshots.values():
        member, frozen_name, frozen_value, _frozen_hash, _extra = _ENUM_STATE[id(proxy)]
        if member.name != frozen_name:
            return True
        live_val = member.value
        if live_val is frozen_value:
            continue
        if (
            isinstance(live_val, float)
            and isinstance(frozen_value, float)
            and math.isnan(live_val)
            and math.isnan(frozen_value)
        ):
            continue
        if live_val != frozen_value:
            return True
    return False


def _unwrap_model_for_validation(proxy: ReadOnlyModelProxy) -> BaseModel:
    """Return a detached working model copy for the validation engine after isolation and drift checks."""
    target, _cache, enum_snapshots, _fields = _PROXY_STATE[id(proxy)]
    _validate_enum_dump_isolation(target, has_user_fallback=False)
    if enum_snapshots and _enum_snapshots_drifted(enum_snapshots):
        raise TypeError(
            "Cannot safely validate frozen model after live Enum singleton "
            "_name_ or _value_ was mutated externally"
        )
    return _safe_snapshot_copy(target)


def freeze_model(model: BaseModel | ReadOnlyModelProxy) -> ReadOnlyModelProxy:
    """Return a deeply immutable :class:`ReadOnlyModelProxy` for *model*.

    If *model* is already a :class:`ReadOnlyModelProxy`, returns it directly.
    Otherwise snapshots *model* via ``deepcopy`` so subsequent caller mutations
    cannot leak into validation rules.
    """
    if isinstance(model, ReadOnlyModelProxy):
        return model
    return ReadOnlyModelProxy(model)
