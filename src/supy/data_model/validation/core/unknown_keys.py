"""Detect YAML keys that the data model does not recognise.

Pydantic's default ``extra="ignore"`` drops an unrecognised key without a
word, so a user who mistypes a field name -- or carries a field name from an
older schema -- gets the default value with nothing to indicate it (gh#1647).

This module walks the raw YAML mapping against the Pydantic model tree and
reports every key the model would silently discard, together with a
suggestion where one can be made:

- an exact legacy name from the rename registry resolves to the current
  field name;
- a case-insensitive legacy match catches the historical CamelCase
  spellings (``WaterUseMethod``), which the rename tables hold only in
  their lowercase form;
- otherwise a close-match search over the sibling field names catches an
  ordinary typo.

An unrecognised key means the model never applied something the user asked
for, so the caller rejects the config rather than warning -- the same
treatment an unrecognised option *value* has always had. This module only
finds and describes the keys; raising is the loader's decision.
"""

import difflib
from dataclasses import dataclass
from functools import lru_cache
from typing import (
    Any,
    Dict,
    FrozenSet,
    List,
    Mapping,
    Optional,
    Set,
    Tuple,
    Type,
    Union,
)

from pydantic import BaseModel
from typing_extensions import get_args, get_origin

from ...core.field_renames import (
    RAW_YAML_FIELD_RENAMES,
    STEBBS_PHYSICS_LEAF_RENAMES,
)
from ...core.physics_families import (
    PHYSICS_PUBLIC_KEY_ALIASES,
    STEBBS_PUBLIC_KEY_ALIASES,
)

__all__ = [
    "UnknownKey",
    "collect_unknown_keys",
    "format_unknown_keys_report",
]

# Minimum similarity for a close-match suggestion. difflib's ratio is
# generous at lower cutoffs, and a wrong "did you mean" is worse than none.
_FUZZY_CUTOFF = 0.75

# Every ``{accepted YAML key: field name}`` mapping a ``mode="before"``
# validator may apply. Beyond the rename registry this covers the public
# key aliases, where the documented YAML spelling differs from the field
# name (``leaf_area_index`` -> ``laimethod``, ``snow`` -> ``snow_use``,
# ``parameter_source`` -> ``parameters``).
_ACCEPTED_ALIASES: Dict[str, str] = {
    **RAW_YAML_FIELD_RENAMES,
    **PHYSICS_PUBLIC_KEY_ALIASES,
    **STEBBS_PUBLIC_KEY_ALIASES,
}

# Keys a ``mode="before"`` validator consumes without them ever being a
# field of that model, so the generic "alias whose target is a sibling
# field" rule cannot find them. Each entry is a legacy spelling the model
# still accepts and warns about itself, which is why reporting it here
# would be both wrong and duplicate.
#
# Extend this when a validator is added that swallows a raw key whose
# target is not a sibling field -- otherwise that key starts being
# reported as unrecognised even though the model honours it.
_VALIDATOR_CONSUMED_KEYS: Dict[str, Set[str]] = {
    # Lifted into the nested `forcing` / `output` objects.
    "ModelControl": {"forcing_file", "output_file"},
    # The flat STEBBS switches, folded down into `model.physics.stebbs`.
    "ModelPhysics": set(STEBBS_PHYSICS_LEAF_RENAMES),
}

# Legacy names indexed case-insensitively, so the CamelCase spellings that
# every historical RunControl.nml used (``WaterUseMethod``) resolve to the
# current field even though the registry holds only ``waterusemethod``.
_LEGACY_BY_LOWER: Dict[str, str] = {
    old.lower(): new for old, new in _ACCEPTED_ALIASES.items()
}


@dataclass(frozen=True)
class UnknownKey:
    """One YAML key the data model does not recognise.

    Attributes
    ----------
    path : str
        Dotted path to the key, e.g. ``model.physics.WaterUseMethod``.
    key : str
        The unrecognised key itself.
    suggestion : str, optional
        Current field name this key most likely meant, when one is found.
    reason : str
        Why the suggestion was made: ``"legacy"`` for a rename-registry hit,
        ``"typo"`` for a close match against a sibling field, or ``""`` when
        no suggestion could be made.
    """

    path: str
    key: str
    suggestion: Optional[str] = None
    reason: str = ""

    def describe(self) -> str:
        """Render a single-line, user-facing description of this key."""
        base = f"{self.path} is not a recognised field"
        if self.suggestion is None:
            return f"{base}."
        if self.reason == "legacy":
            return (
                f"{base} in the current schema; it was renamed to '{self.suggestion}'."
            )
        return f"{base}; did you mean '{self.suggestion}'?"


def _unwrap_annotation(annotation: Any) -> List[type]:
    """Return the BaseModel types reachable from a field annotation.

    Unions, ``Optional``, ``List`` and ``Dict`` value types are all unwrapped,
    so a field declared ``Union[RefValue[int], int]`` yields ``RefValue`` and a
    field declared ``List[Site]`` yields ``Site``.
    """
    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return [annotation]

    origin = get_origin(annotation)
    if origin is None:
        return []

    found: List[type] = []
    for arg in get_args(annotation):
        found.extend(_unwrap_annotation(arg))
    return found


def _has_non_model_arm(annotation: Any) -> bool:
    """Report whether *annotation* admits a value that is not a nested model.

    A field declared ``Union[RefValue[NetRadiationMethod], NetRadiationMethod]``
    has an enum arm, so a mapping supplied for it is not necessarily a
    ``RefValue`` -- it may be a scheme mapping that a ``mode="before"``
    validator folds down. Detecting that keeps the walk from descending into
    a shape the model never sees as a nested object.
    """
    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return False

    origin = get_origin(annotation)
    if origin is None:
        # A bare scalar, enum, or unparameterised annotation.
        return True

    args = [arg for arg in get_args(annotation) if arg is not type(None)]
    if not args:
        return True
    return any(_has_non_model_arm(arg) for arg in args)


def _is_mapping_field(annotation: Any) -> bool:
    """Report whether a field is a keyed mapping rather than a nested model.

    A mapping field (``Dict[str, HourlyProfile]``) carries user-chosen keys
    such as hours or surface names. Those keys are data, not field names, so
    the walk must not test them against ``model_fields``.
    """
    origin = get_origin(annotation)
    if origin in (dict, Dict) or (
        isinstance(origin, type) and issubclass(origin, Mapping)
    ):
        return True
    if origin is Union:
        return any(_is_mapping_field(arg) for arg in get_args(annotation))
    return False


@lru_cache(maxsize=None)
def _accepted_keys(model_cls: Type[BaseModel]) -> FrozenSet[str]:
    """Return every key *model_cls* accepts from raw YAML.

    This is the declared field names, plus any validation/serialisation
    aliases, plus the legacy names that this model's ``mode="before"``
    validators rewrite into one of its fields. The legacy set is derived
    generically -- a legacy name is accepted here when its current name is a
    field of this model -- so a new rename table is covered the moment it is
    registered, without this module having to know which model applies it.

    Cached per class: a configuration with many sites revisits the same
    handful of models thousands of times, and the answer depends only on the
    class and the module-level registries.
    """
    accepted: Set[str] = set()
    for name, field in model_cls.model_fields.items():
        accepted.add(name)
        if field.alias:
            accepted.add(field.alias)
        if isinstance(field.validation_alias, str):
            accepted.add(field.validation_alias)

    field_names = set(model_cls.model_fields)
    accepted.update(old for old, new in _ACCEPTED_ALIASES.items() if new in field_names)
    accepted.update(_VALIDATOR_CONSUMED_KEYS.get(model_cls.__name__, ()))
    return frozenset(accepted)


def _suggest(key: str, field_names: Set[str]) -> Tuple[Optional[str], str]:
    """Propose the field *key* most likely meant, with the reason.

    Tried in order of confidence: an exact legacy name, a case-insensitive
    legacy name, then a close match against this model's own field names.
    """
    legacy = _ACCEPTED_ALIASES.get(key)
    if legacy is None:
        legacy = _LEGACY_BY_LOWER.get(key.lower())
    if legacy is not None and legacy in field_names:
        return legacy, "legacy"

    # A case-insensitive hit on a current field name is a capitalisation
    # slip rather than a rename, so report it as a typo.
    lowered = {name.lower(): name for name in field_names}
    if key.lower() in lowered:
        return lowered[key.lower()], "typo"

    close = difflib.get_close_matches(
        key, sorted(field_names), n=1, cutoff=_FUZZY_CUTOFF
    )
    if close:
        return close[0], "typo"

    return None, ""


def _walk(
    data: Any,
    model_cls: Optional[Type[BaseModel]],
    path: str,
    found: List[UnknownKey],
) -> None:
    """Recursively compare *data* against *model_cls*, appending unknown keys."""
    if isinstance(data, list):
        for index, item in enumerate(data):
            _walk(item, model_cls, f"{path}[{index}]", found)
        return

    if not isinstance(data, dict) or model_cls is None:
        return

    accepted = _accepted_keys(model_cls)
    field_names = set(model_cls.model_fields)

    for key, value in data.items():
        key_str = str(key)
        # Underscore-prefixed keys are the reserved internal bookkeeping
        # namespace (``_yaml_path``, ``_yaml_raw``, ...) and round-trip freely.
        if key_str.startswith("_"):
            continue

        child_path = f"{path}.{key_str}" if path else key_str

        if key_str not in accepted:
            suggestion, reason = _suggest(key_str, field_names)
            found.append(UnknownKey(child_path, key_str, suggestion, reason))
            continue

        field = model_cls.model_fields.get(key_str)
        if field is None:
            # An accepted legacy name: it is rewritten into a current field
            # by a before-validator, so there is nothing further to check
            # here. The rename itself already warns.
            continue

        if _is_mapping_field(field.annotation):
            nested = _unwrap_annotation(field.annotation)
            if nested and isinstance(value, dict):
                for map_key, map_value in value.items():
                    _walk(
                        map_value,
                        nested[0],
                        f"{child_path}.{map_key}",
                        found,
                    )
            continue

        nested = _unwrap_annotation(field.annotation)
        if not nested:
            continue

        # An unambiguous nested model: the value must conform to it.
        if len(nested) == 1 and not _has_non_model_arm(field.annotation):
            _walk(value, nested[0], child_path, found)
            continue

        # Otherwise the annotation is ambiguous -- either a union of several
        # models, or a union mixing a model with a scalar/enum arm. The
        # physics families are the latter: `net_radiation` is declared
        # `Union[RefValue[NetRadiationMethod], NetRadiationMethod]` yet
        # accepts a scheme mapping (`{narp: {...}}`) that a before-validator
        # folds down. Descend only when exactly one model candidate accepts
        # every key present, so a validator-consumed shape is left alone
        # rather than reported as a wrong key.
        if isinstance(value, dict):
            keys = {str(k) for k in value if not str(k).startswith("_")}
            matches = [
                candidate for candidate in nested if keys <= _accepted_keys(candidate)
            ]
            if len(matches) == 1:
                _walk(value, matches[0], child_path, found)


def collect_unknown_keys(
    data: Mapping[str, Any], model_cls: Type[BaseModel]
) -> List[UnknownKey]:
    """Find every key in *data* that *model_cls* would silently discard.

    Parameters
    ----------
    data : Mapping
        Raw YAML-shaped configuration data, before Pydantic validation.
    model_cls : type
        Root model class to compare against, normally ``SUEWSConfig``.

    Returns
    -------
    list of UnknownKey
        One entry per unrecognised key, ordered by path.

    Notes
    -----
    Detection is best-effort and deliberately conservative: where the model
    tree is ambiguous (an unresolvable union, a mapping of user-chosen keys)
    the walk declines to descend rather than risk a false report.
    """
    found: List[UnknownKey] = []
    _walk(dict(data), model_cls, "", found)
    return sorted(found, key=lambda item: item.path)


def format_unknown_keys_report(unknown: List[UnknownKey]) -> str:
    """Render *unknown* as a multi-line warning message.

    Returns an empty string when there is nothing to report, so callers can
    use the result as the truth test.
    """
    if not unknown:
        return ""

    count = len(unknown)
    noun = "key" if count == 1 else "keys"
    lines = [f"{count} unrecognised configuration {noun}:"]
    lines.extend(f"  - {item.describe()}" for item in unknown)
    lines.append(
        "The model has no field of this name, so the value would have no "
        "effect on the run. Correct the name, or remove the key if it is "
        "not needed."
    )
    return "\n".join(lines)
