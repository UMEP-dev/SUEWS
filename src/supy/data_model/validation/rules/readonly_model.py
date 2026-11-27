"""Read-only proxy over validated Pydantic configuration models.

Wraps a defensive snapshot of any Pydantic ``BaseModel`` (such as
``SUEWSConfig`` and its nested sub-models) so validation rules can inspect
typed attributes, ``model_fields_set``, and computed properties without being
able to mutate the configuration or observe post-freeze mutations made by the
caller.
"""

from __future__ import annotations

from collections.abc import Mapping
from copy import deepcopy
from types import MappingProxyType
from typing import Any
import weakref

import numpy as np
from pydantic import BaseModel

_ALLOWED_READONLY_METHODS = frozenset({"model_dump"})
_PROXY_STATE: dict[int, tuple[BaseModel, dict[str, Any]]] = {}


def _wrap_readonly(value: Any) -> Any:
    """Recursively wrap a detached Pydantic/Python value as read-only."""
    if isinstance(value, BaseModel):
        return ReadOnlyModelProxy._from_detached(value)
    if isinstance(value, Mapping):
        return MappingProxyType({k: _wrap_readonly(v) for k, v in value.items()})
    if isinstance(value, (list, tuple)):
        return tuple(_wrap_readonly(item) for item in value)
    if isinstance(value, (set, frozenset)):
        return frozenset(_wrap_readonly(item) for item in value)
    if isinstance(value, np.ndarray):
        arr = value.copy()
        arr.setflags(write=False)
        return arr
    return value


class ReadOnlyModelProxy:
    """Deeply immutable attribute-access proxy over a Pydantic ``BaseModel``.

    All attribute writes and deletions raise :class:`TypeError`. Nested
    ``BaseModel`` attributes are lazily wrapped in :class:`ReadOnlyModelProxy`,
    mappings are exposed as :class:`~types.MappingProxyType`, lists as tuples,
    sets as frozensets, and NumPy arrays as non-writeable views. Backing state
    is held outside the proxy instance so private attribute access cannot leak
    the underlying mutable model.
    """

    __slots__ = ("__weakref__",)
    __hash__ = None  # type: ignore[assignment]

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
    def model_fields(self) -> Mapping[str, Any]:
        """Read-only view of the model's declared Pydantic fields."""
        return MappingProxyType(self.model_type.model_fields)

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
        from .typed_view import TypedConfigView  # ruff: ignore[import-outside-top-level]

        return TypedConfigView.from_config(_PROXY_STATE[id(self)][0])

    def __getattr__(self, name: str) -> Any:
        """Resolve declared fields, computed fields, and read-only properties."""
        if name.startswith("_"):
            raise AttributeError(
                f"Read-only model view {self.model_type.__name__!r} does not expose private attribute {name!r}"
            )
        target, cache = _PROXY_STATE[id(self)]
        if name in cache:
            return cache[name]

        model_cls = type(target)

        if name in model_cls.model_fields or name in model_cls.model_computed_fields:
            wrapped = _wrap_readonly(getattr(target, name))
            cache[name] = wrapped
            return wrapped

        for cls_in_mro in model_cls.__mro__:
            if name in cls_in_mro.__dict__:
                descriptor = cls_in_mro.__dict__[name]
                if isinstance(descriptor, property):
                    wrapped = _wrap_readonly(getattr(target, name))
                    cache[name] = wrapped
                    return wrapped
                break

        extra = getattr(target, "model_extra", None)
        if isinstance(extra, Mapping) and name in extra:
            wrapped = _wrap_readonly(extra[name])
            cache[name] = wrapped
            return wrapped

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
