"""Forcing data contract definitions."""

# Lazy package exports keep ``forcing.version`` independent of Pydantic registry
# construction. The names are resolved by ``__getattr__`` below.
# ruff: disable[undefined-export, non-empty-init-module]

import importlib as _importlib

__all__ = [
    "FORCING_REGISTRY",
    "ForcingRegistry",
    "ForcingVariable",
    "RequirementRule",
]

_LAZY_ATTRS = {
    "FORCING_REGISTRY": ("variables", "FORCING_REGISTRY"),
    "ForcingRegistry": ("registry", "ForcingRegistry"),
    "ForcingVariable": ("registry", "ForcingVariable"),
    "RequirementRule": ("registry", "RequirementRule"),
}


def __getattr__(name: str):
    """Load the forcing registry only when its API is requested."""
    try:
        module_name, attribute_name = _LAZY_ATTRS[name]
    except KeyError as exc:
        raise AttributeError(f"module {__name__!r} has no attribute {name!r}") from exc

    value = getattr(
        _importlib.import_module(f"{__name__}.{module_name}"), attribute_name
    )
    globals()[name] = value
    return value


# ruff: enable[undefined-export, non-empty-init-module]
