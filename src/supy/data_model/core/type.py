from collections.abc import Iterable, Mapping, Sequence
from contextvars import ContextVar
from enum import Enum
import math
import types
from typing import (
    Annotated,
    Any,
    Callable,
    ClassVar,
    Generic,
    Literal,
    Optional,
    TypeVar,
    Union,
    get_args,
    get_origin,
)
import weakref

import numpy as np
import pandas as pd
from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    PrivateAttr,
    ValidationInfo,
    field_validator,
    model_serializer,
)

_REF_VALUE_UNSET = object()


class SurfaceType(str, Enum):
    PAVED = "paved"
    BLDGS = "bldgs"
    EVETR = "evetr"
    DECTR = "dectr"
    GRASS = "grass"
    BSOIL = "bsoil"
    WATER = "water"


T = TypeVar("T")


class Reference(BaseModel):
    """Evidence supporting parameter choices (literature, data sources, expert knowledge, etc.)."""

    model_config = ConfigDict(title="Reference Info")

    desc: Optional[str] = Field(
        default=None, description="Description of the reference source"
    )
    ID: Optional[str] = Field(
        default=None, description="Identifier for the reference (e.g., citation key)"
    )
    DOI: Optional[str] = Field(
        default=None, description="Digital Object Identifier for the reference"
    )


class RefValue(BaseModel, Generic[T]):
    """A class that wraps a value with an optional reference.

    This class allows storing a value along with its reference information (e.g. DOI).
    It handles numeric type conversion and implements comparison operators.

    When used in Field definitions for physical quantities, units should be specified.

    **Unit Conventions:**

    - Use SI base units: m, kg, s, K, W, etc.
    - Use ASCII notation for compounds: m^2, kg m^-3, W m^-1 K^-1
    - Use degC for temperatures, K for temperature differences
    - Use "dimensionless" for ratios, fractions, albedos, etc.

    Attributes:
        value (T): The wrapped value of generic type T
        ref (Optional[Reference]): Optional reference information for the value
    """

    value: Optional[T]
    ref: Optional[Reference] = None
    _non_null_field_name: Optional[str] = PrivateAttr(default=None)
    _non_null_expected_kind: str = PrivateAttr(default="value")
    _reject_bool_field_name: Optional[str] = PrivateAttr(default=None)
    _value_validator: Optional[Callable[[Any], Any]] = PrivateAttr(default=None)

    model_config = ConfigDict(
        # Configure serialization to handle the value field properly
        ser_json_inf_nan="constants",
        # Reject unknown keys as ValidationError so Pydantic's union fall-through
        # can try the other branch instead of bubbling up a TypeError (gh#1303).
        extra="forbid",
        # Validate attribute assignment so `rv.ref = {"DOI": ...}` is coerced
        # into a Reference instead of being stored as a raw dict, which made
        # every later dump emit PydanticSerializationUnexpectedValue (gh#1095).
        validate_assignment=True,
    )

    def __new__(cls, value=_REF_VALUE_UNSET, ref=None, **data):
        """Auto-parametrise the bare generic so ``RefValue(x)`` is ``RefValue[type(x)]``.

        The data model declares fields as ``FlexibleRefValue(T)`` =
        ``Union[RefValue[T], T]``. Pydantic's union serializer only routes a
        value cleanly to the ``RefValue[T]`` branch when the instance carries
        the matching generic parameter; a bare, unparametrised ``RefValue``
        (from ``RefValue(x)``) matches neither branch's schema and triggers a
        ``PydanticSerializationUnexpectedValue`` warning per field on dump --
        hundreds for a config rebuilt from ``df_state`` (gh#1569). Validation
        from YAML already yields ``RefValue[T]``; this makes the dozens of
        direct ``RefValue(value)`` reconstruction sites consistent with it.

        Only the bare base class with a concrete, non-``None`` scalar is
        redirected; already-parametrised subclasses, the keyword-only / unset
        call shape used during validation and ``model_construct``, and ``None``
        values (no inferable type) are left untouched.
        """
        if cls is RefValue:
            concrete = value if value is not _REF_VALUE_UNSET else data.get(
                "value", _REF_VALUE_UNSET
            )
            # Resolve numpy scalars to their Python native, so we parametrise on
            # the Python type (RefValue[float], not RefValue[numpy.float64]) and
            # never ask Pydantic to build a schema for a numpy type it cannot.
            if isinstance(concrete, np.generic):
                concrete = concrete.item()
            # Only parametrise on the plain types FlexibleRefValue actually uses
            # and that Pydantic can build a RefValue[T] schema for. Anything else
            # (None, pandas Timestamps, containers, ...) stays the bare generic.
            if type(concrete) in (bool, int, float, str) or isinstance(
                concrete, Enum
            ):
                cls = RefValue[type(concrete)]
        return super().__new__(cls)

    def __init__(self, value=_REF_VALUE_UNSET, ref=None, **data):
        # Two call shapes are supported:
        # 1. Direct construction: `RefValue(3.0)` or `RefValue(value=3.0, ref=...)`.
        # 2. Pydantic-driven validation: `RefValue(**dict_from_yaml)`, which may
        #    carry profile-shaped keys when a union falls through. In that case
        #    let the base class's validators run so extras become a
        #    ValidationError that the union can catch, rather than a TypeError
        #    that aborts validation.
        if value is _REF_VALUE_UNSET:
            super().__init__(**data)
            return
        data["value"] = value
        data["ref"] = ref
        super().__init__(**data)

    def _bind_non_optional(self, field_name: str, expected_kind: str) -> None:
        """Bind this wrapper to a non-optional field so ``.value = None`` fails."""
        self._non_null_field_name = field_name
        self._non_null_expected_kind = expected_kind

    def _bind_reject_bool(self, field_name: str) -> None:
        """Bind this wrapper so ``.value = True`` / ``np.bool_`` fails before coercion."""
        self._reject_bool_field_name = field_name

    def _bind_value_validator(self, validator: Callable[[Any], Any]) -> None:
        """Bind a scalar validator enforced before assignment to ``.value``."""
        self._value_validator = validator

    def __setattr__(self, name: str, value: Any) -> None:
        if name == "value":
            if value is None:
                field_name = getattr(self, "_non_null_field_name", None)
                if field_name is not None:
                    reject_null_value(
                        None,
                        field_name,
                        expected_kind=getattr(
                            self, "_non_null_expected_kind", "value"
                        ),
                    )
            else:
                bool_field = getattr(self, "_reject_bool_field_name", None)
                if bool_field is not None:
                    reject_bool_input(value, bool_field)
                validator = getattr(self, "_value_validator", None)
                if validator is not None:
                    value = validator(value)
        super().__setattr__(name, value)

    @field_validator("value", mode="before")
    @classmethod
    def _convert_numpy_scalar(cls, v):
        """Convert numpy scalar types to Python natives before validation."""
        if isinstance(v, (np.float64, np.float32)):
            return float(v)
        if isinstance(v, (np.int64, np.int32)):
            return int(v)
        if isinstance(v, np.bool_):
            return bool(v)
        if isinstance(v, np.str_):
            return str(v)
        return v

    def model_dump(self, **kwargs):
        """Override model_dump to handle JSON mode properly."""
        if kwargs.get("mode") == "json":
            # In JSON mode, return a simplified representation
            if self.ref:
                return {"value": self.value, "ref": self.ref}
            else:
                # If no reference, just return the value directly
                return self.value
        # For other modes, use default behavior
        return super().model_dump(**kwargs)

    @model_serializer(mode="wrap")
    def _serialize_with_value_key(self, handler, info):
        """Keep ``value: null`` in nested dumps for schema-valid YAML."""
        if info.mode == "json" and self.value is None:
            data = {"value": None}
            if self.ref is not None or not info.exclude_none:
                data["ref"] = self.ref
            return data

        return handler(self)

    @classmethod
    def wrap(cls, value: Union[T, "RefValue[T]"]) -> "RefValue[T]":
        """Auto-wrap simple values in RefValue, return RefValue unchanged"""
        if isinstance(value, cls):
            return value
        return cls(value)

    def __str__(self):
        """String representation showing just the value"""
        return f"{self.value}"

    def __repr__(self):
        """Representation showing just the value"""
        return f"{self.value}"

    def __iter__(self):
        """Allow iteration over lists"""
        return iter(self.value)

    def __eq__(self, other):
        """Equal comparison operator"""
        if isinstance(other, RefValue):
            return self.value == other.value
        return self.value == other

    def __ne__(self, other):
        """Not equal comparison operator"""
        if isinstance(other, RefValue):
            return self.value != other.value
        return self.value != other

    def __ge__(self, other):
        """Greater than or equal comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value >= other.value
        return self.value >= other

    def __gt__(self, other):
        """Greater than comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value > other.value
        return self.value > other

    def __le__(self, other):
        """Less than or equal comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value <= other.value
        return self.value <= other

    def __lt__(self, other):
        """Less than comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value < other.value
        return self.value < other


def FlexibleRefValue(type_param):
    """Create a Union type that accepts both RefValue and simple values"""
    return Union[RefValue[type_param], type_param]


def reject_null_value(value, field_name: str, *, expected_kind: str = "number"):
    """Reject ``{value: null}`` on a field that does not accept ``None``.

    ``RefValue.value`` is ``Optional`` so that genuinely optional fields can
    carry ``null``. On a non-optional field a ``null`` would reach the Rust
    bridge as a missing key and be replaced by the bridge's own fallback, so
    it is rejected here instead (gh#2001, gh#2087).

    Parameters
    ----------
    value : RefValue or scalar
        The validated field value.
    field_name : str
        Name used in the error message.
    expected_kind : str, optional
        Noun phrase describing the expected type (``"number"`` or ``"value"``).

    Returns
    -------
    RefValue or scalar
        ``value`` unchanged when it carries a value.

    Raises
    ------
    ValueError
        If the wrapped value is ``None``.
    """
    inner = value.value if isinstance(value, RefValue) else value
    if inner is None:
        raise ValueError(
            f"{field_name} needs a {expected_kind}; null is not accepted "
            "(omit the field to use its default)"
        )
    return value


# Names of defaulted non-optional RefValue fields that reject ``{value: null}``,
# populated automatically by ``DataModelBase.__pydantic_init_subclass__`` (and
# ``null_rejecting_validator``) as models are defined. The validation pipeline
# reads it so Phase A never writes a null placeholder over a field whose
# omission already supplies a model default (gh#2001, gh#2087).
NULL_REJECTED_FIELDS: set[str] = set()


def reject_bool_input(value, field_name: str):
    """Reject booleans in a numeric field before Pydantic coerces them.

    In lax mode Pydantic turns ``True``/``False`` into ``1.0``/``0.0`` for a
    ``float`` field, so a check that runs after validation can no longer see
    that the input was a boolean. Used as a ``mode="before"`` validator on
    numeric inputs that have no boolean meaning; accepts a bare scalar, a list,
    a ``{value: ...}`` mapping or a ``RefValue``.

    Raises
    ------
    ValueError
        If the value, or any element of a list value, is a boolean.
    """
    inner = value.value if isinstance(value, RefValue) else value
    if isinstance(inner, dict) and "value" in inner:
        inner = inner["value"]
    items = inner if isinstance(inner, (list, tuple)) else [inner]
    for item in items:
        if isinstance(item, (bool, np.bool_)):
            raise ValueError(
                f"{field_name} must be numeric; boolean {item!r} is not accepted"
            )
    return value


def _unwrap_annotated(annotation: Any) -> Any:
    while get_origin(annotation) is Annotated:
        annotation = get_args(annotation)[0]
    return annotation


def _is_numeric_type(tp: Any) -> bool:
    tp = _unwrap_annotated(tp)
    if tp in {int, float}:
        return True
    origin = get_origin(tp)
    if origin in {Union, types.UnionType}:
        return all(_is_numeric_type(arg) for arg in get_args(tp))
    if origin is Literal:
        args = get_args(tp)
        return bool(args) and all(
            isinstance(a, (int, float)) and not isinstance(a, bool) for a in args
        )
    if isinstance(tp, type) and issubclass(tp, Enum):
        return not issubclass(tp, str) and all(
            isinstance(m.value, (int, float)) and not isinstance(m.value, bool)
            for m in tp
        )
    return False


def _classify_non_optional_ref_field(annotation: Any) -> str | None:
    """Return ``'number'`` or ``'value'`` for non-Optional RefValue fields, else ``None``."""
    ann = _unwrap_annotated(annotation)
    origin = get_origin(ann)
    if origin in {Union, types.UnionType}:
        args = tuple(_unwrap_annotated(a) for a in get_args(ann))
    else:
        args = (ann,)
    if type(None) in args:
        return None
    has_ref = False
    non_ref_args = []
    for arg in args:
        arg_origin = get_origin(arg)
        if (
            arg is RefValue
            or arg_origin is RefValue
            or (isinstance(arg, type) and issubclass(arg, RefValue))
        ):
            has_ref = True
        else:
            non_ref_args.append(arg)
    if not has_ref:
        return None
    if non_ref_args and all(_is_numeric_type(a) for a in non_ref_args):
        return "number"
    return "value"


class DataModelBase(BaseModel):
    """Base class for SUEWS configuration models enforcing non-null ``RefValue`` fields.

    ``RefValue.value`` stays ``Optional[T]`` so genuinely ``Optional[FlexibleRefValue(T)]``
    fields can still carry ``{value: null}`` (for inactive physics blocks or
    round-trips). Any field typed as non-optional ``FlexibleRefValue(T)`` rejects
    ``{value: null}`` automatically at validation time, and defaulted ones are
    registered in ``NULL_REJECTED_FIELDS`` so ``suews validate`` Phase A never
    overwrites omitted defaults with null placeholders (gh#2087).
    """

    _non_optional_ref_fields: ClassVar[dict[str, str]] = {}

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs: Any) -> None:  # noqa: PLW3201
        """Record non-optional RefValue fields and register defaulted ones."""
        super().__pydantic_init_subclass__(**kwargs)
        non_opt: dict[str, str] = {}
        for field_name, field_info in cls.model_fields.items():
            kind = _classify_non_optional_ref_field(field_info.annotation)
            if kind is None:
                continue
            non_opt[field_name] = kind
            if not field_info.is_required():
                NULL_REJECTED_FIELDS.add(field_name)
        cls._non_optional_ref_fields = non_opt

    @field_validator("*", mode="after")
    @classmethod
    def _reject_null_on_non_optional_ref_field(cls, v: Any, info: ValidationInfo) -> Any:
        if isinstance(v, RefValue):
            field_name = info.field_name
            if field_name is not None and field_name in cls._non_optional_ref_fields:
                expected_kind = cls._non_optional_ref_fields[field_name]
                if v.value is None:
                    return reject_null_value(
                        v,
                        field_name,
                        expected_kind=expected_kind,
                    )
                v._bind_non_optional(field_name, expected_kind)
        return v

    def __setattr__(self, name: str, value: Any) -> None:
        expected_kind = type(self)._non_optional_ref_fields.get(name)
        if expected_kind is not None:
            if value is None:
                reject_null_value(value, name, expected_kind=expected_kind)
            elif isinstance(value, RefValue):
                if value.value is None:
                    reject_null_value(value, name, expected_kind=expected_kind)
                value._bind_non_optional(name, expected_kind)
            elif (
                isinstance(value, dict)
                and "value" in value
                and value["value"] is None
            ):
                reject_null_value(None, name, expected_kind=expected_kind)
        super().__setattr__(name, value)

    def validate_non_null_ref_values(self, _path: str = "") -> None:
        """Recursively reject null values on non-optional ``RefValue`` fields.

        Defence in depth at public execution and export boundaries (such as
        ``SUEWSSimulation`` and ``config_to_engine_yaml``) in case a live
        model was mutated via ``object.__setattr__`` or ``model_construct``
        (gh#2087).
        """
        non_opt = type(self)._non_optional_ref_fields
        for field_name in type(self).model_fields:
            val = getattr(self, field_name, None)
            field_path = f"{_path}.{field_name}" if _path else field_name
            expected_kind = non_opt.get(field_name)
            if expected_kind is not None:
                if val is None or (isinstance(val, RefValue) and val.value is None):
                    reject_null_value(None, field_path, expected_kind=expected_kind)
                elif (
                    isinstance(val, dict)
                    and "value" in val
                    and val["value"] is None
                ):
                    reject_null_value(None, field_path, expected_kind=expected_kind)
            _validate_non_null_ref_container(val, field_path)


def _validate_non_null_ref_container(value: Any, path: str) -> None:
    if isinstance(value, DataModelBase):
        value.validate_non_null_ref_values(path)
    elif isinstance(value, (list, tuple)):
        for idx, item in enumerate(value):
            _validate_non_null_ref_container(item, f"{path}[{idx}]")
    elif isinstance(value, dict):
        for key, item in value.items():
            _validate_non_null_ref_container(item, f"{path}.{key}")


def null_rejecting_validator(field_name: str):
    """Return a validator that rejects ``{value: null}`` on ``field_name``.

    Assign the result in the model body, e.g.
    ``_sdd_id_not_null = null_rejecting_validator("sdd_id")``. The field is
    recorded in ``NULL_REJECTED_FIELDS``.
    """
    NULL_REJECTED_FIELDS.add(field_name)

    def _reject(cls, v):
        return reject_null_value(v, field_name)

    _reject.__name__ = f"_{field_name}_not_null"
    return field_validator(field_name, mode="after")(_reject)


# pandas 2.x sorts unioned columns in DataFrame.combine_first(), while pandas 3.x
# preserves left-then-right column order. Probe the active runtime once so
# combine_df_states() matches DataFrame.combine_first() across both major versions.
_COMBINE_FIRST_COLUMN_SORT: Literal[False] | None = (
    None
    if pd.DataFrame({"z": [1]}).combine_first(pd.DataFrame({"a": [2]})).columns[0]
    == "a"
    else False
)


def combine_df_states(left: pd.DataFrame, right: pd.DataFrame) -> pd.DataFrame:
    """Fill missing state values without aligning every disjoint profile column.

    State components normally share an index and only the grid identifier.
    Keep pandas' null and dtype resolution for shared columns; columns present
    on just one side need no element-wise combination. Unusual axes retain the
    general pandas operation. No input frame is mutated or cached.
    """
    if (
        not left.index.equals(right.index)
        or not left.columns.is_unique
        or not right.columns.is_unique
        or left.empty
        or right.empty
    ):
        return left.combine_first(right)
    sort = _COMBINE_FIRST_COLUMN_SORT
    shared = left.columns.intersection(right.columns, sort=sort)
    left_only = left.columns.difference(right.columns, sort=sort)
    right_only = right.columns.difference(left.columns, sort=sort)
    parts = [left.loc[:, left_only]]
    if len(shared):
        parts.append(left.loc[:, shared].combine_first(right.loc[:, shared]))
    parts.append(right.loc[:, right_only])
    return pd.concat(parts, axis=1).reindex(
        columns=left.columns.union(right.columns, sort=sort)
    )


def merge_df_state_cols(
    target: dict, source: dict, *, fill_na: bool = False
) -> None:
    """Merge single-grid column entries into *target* without intermediate frames.

    With ``fill_na=False`` (default), preserves first-seen values matching
    ``pd.concat(..., axis=1)`` followed by ``~columns.duplicated(keep="first")``.
    With ``fill_na=True``, fills missing or NA values matching
    ``combine_df_states`` on aligned single-row frames.
    """
    if not fill_na:
        for k, v in source.items():
            target.setdefault(k, v)
        return
    for k, v in source.items():
        if k not in target:
            target[k] = v
        else:
            cur = target[k]
            if pd.api.types.is_scalar(cur) and pd.isna(cur):
                target[k] = v


def merge_df_state_col_items(
    items: Iterable[tuple[tuple[str, str], Any]],
) -> dict[tuple[str, str], Any]:
    """Deduplicate ordered ``(col_key, value)`` pairs preserving first occurrence."""
    cols: dict[tuple[str, str], Any] = {}
    for k, v in items:
        cols.setdefault(k, v)
    return cols


def df_from_col_items(
    items: Sequence[tuple[tuple[str, str], Any]],
    index: pd.Index,
    *,
    column_names: Sequence[str | None] | None = ("var", "ind_dim"),
) -> pd.DataFrame:
    """Build a single-row state DataFrame from ordered ``(col_key, value)`` items.

    Preserves exact column order, per-column dtypes, and duplicate-column
    multiplicity matching ``pd.concat(..., axis=1)`` without allocating
    intermediate DataFrames.
    """
    col_keys = [k for k, _ in items]
    col_data = {pos: [v] for pos, (_, v) in enumerate(items)}
    df_state = pd.DataFrame(col_data, index=index)
    df_state.columns = pd.MultiIndex.from_tuples(col_keys, names=column_names)
    return df_state


def df_from_cols(
    cols: dict,
    index: pd.Index,
    *,
    column_names: Sequence[str | None] | None = ("var", "ind_dim"),
) -> pd.DataFrame:
    df_state = pd.DataFrame(cols, index=index)
    df_state.columns = pd.MultiIndex.from_tuples(
        df_state.columns, names=column_names
    )
    return df_state


def init_df_state(grid_id: int) -> pd.DataFrame:
    idx = pd.Index([grid_id], name="grid")
    col = pd.MultiIndex.from_tuples([("gridiv", "0")], names=["var", "ind_dim"])
    df_state = pd.DataFrame(index=idx, columns=col)
    df_state.loc[grid_id, ("gridiv", "0")] = grid_id
    return df_state


# Scheme-gating validation contexts propagated from SUEWSConfig during nested
# construction and df_state reconstruction:
# - None: standalone model construction (no parent model.physics; stay strict).
# - True: owning physical scheme is active (or selector unresolved).
# - False: owning physical scheme is cleanly declared inactive.
_SNOW_ACTIVE_CONTEXT: ContextVar[bool | None] = ContextVar(
    "_SNOW_ACTIVE_CONTEXT", default=None
)
_SOIL_OBSERVED_CONTEXT: ContextVar[bool | None] = ContextVar(
    "_SOIL_OBSERVED_CONTEXT", default=None
)

_SNOW_USE_NAME_CODES: dict[str, int] = {
    "disabled": 0,
    "enabled": 1,
}
_SOIL_MOISTURE_NAME_CODES: dict[str, int] = {
    "modelled": 0,
    "modeled": 0,
    "observed": 1,
    "observed_volumetric": 1,
    "observed_gravimetric": 2,
}


def _unwrap_selector_raw(value: Any) -> Any:
    """Unwrap RefValue, single-key ``{"value": ...}`` mappings, Enum, and NumPy scalars."""
    while True:
        if isinstance(value, RefValue):
            value = value.value
            continue
        if isinstance(value, Mapping) and "value" in value:
            value = value["value"]
            continue
        break
    if isinstance(value, Enum):
        value = value.value
    if isinstance(value, np.generic):
        value = value.item()
    return value


def resolve_snow_use_code(value: Any) -> int | None:
    """Resolve a raw ``snow_use`` selector to 0 or 1; ``None`` if unknown or malformed."""
    raw = _unwrap_selector_raw(value)
    if isinstance(raw, bool):
        return None
    if isinstance(raw, str):
        text = raw.strip().lower()
        try:
            num = float(text)
        except ValueError:
            return _SNOW_USE_NAME_CODES.get(text.rsplit(".", 1)[-1])
        if math.isfinite(num) and num.is_integer() and int(num) in (0, 1):
            return int(num)
        return None
    if isinstance(raw, (int, float)) and math.isfinite(float(raw)):
        fval = float(raw)
        if fval.is_integer() and int(fval) in (0, 1):
            return int(fval)
    return None


def resolve_soil_moisture_code(value: Any) -> int | None:
    """Resolve a raw ``soil_moisture`` selector to 0, 1 or 2; ``None`` if unknown."""
    raw = _unwrap_selector_raw(value)
    if isinstance(raw, bool):
        return None
    if isinstance(raw, str):
        text = raw.strip().lower()
        try:
            num = float(text)
        except ValueError:
            return _SOIL_MOISTURE_NAME_CODES.get(text.rsplit(".", 1)[-1])
        if math.isfinite(num) and num.is_integer() and int(num) in (0, 1, 2):
            return int(num)
        return None
    if isinstance(raw, (int, float)) and math.isfinite(float(raw)):
        fval = float(raw)
        if fval.is_integer() and int(fval) in (0, 1, 2):
            return int(fval)
    return None


def is_finite_real_number(value: Any) -> bool:
    """Return True iff *value* is a non-bool finite int/float/NumPy real scalar."""
    if isinstance(value, (bool, np.bool_)):
        return False
    if not isinstance(value, (int, float, np.number)):
        return False
    try:
        return math.isfinite(float(value))
    except (TypeError, ValueError, OverflowError):
        return False


def _coerce_inactive_float_scalar(
    inner: Any, *, allow_null: bool, allow_non_finite: bool
) -> tuple[bool, Any]:
    """Coerce a bare scalar inside an inactive float field without enforcing active bounds."""
    if isinstance(inner, np.generic):
        inner = inner.item()
    if inner is None:
        return (True, None) if allow_null else (False, inner)
    if isinstance(inner, bool) or not isinstance(inner, (int, float)):
        return (False, inner)
    try:
        fval = float(inner)
    except OverflowError:
        return (False, inner)
    if not allow_non_finite and not math.isfinite(fval):
        return (False, inner)
    return (True, fval)


def normalise_inactive_float_value(
    raw: Any,
    *,
    allow_null: bool,
    allow_non_finite: bool,
) -> tuple[bool, Any]:
    """Normalise an inactive ``FlexibleRefValue(float)`` input without enforcing active bounds.

    Returns ``(True, normalised_value)`` when *raw* is an acceptable inactive value
    (preserving ``RefValue`` wrappers and optional ``ref`` metadata), or
    ``(False, raw)`` when *raw* should fall through to strict Pydantic validation
    (e.g. booleans, malformed strings, containers, or disallowed ``None`` / non-finite).
    """
    if isinstance(raw, np.generic):
        raw = raw.item()

    if isinstance(raw, RefValue):
        ok, inner = _coerce_inactive_float_scalar(
            raw.value, allow_null=allow_null, allow_non_finite=allow_non_finite
        )
        if not ok:
            return (False, raw)
        rv = (
            RefValue[float].model_construct(value=None, ref=raw.ref)
            if inner is None
            else RefValue[float](value=inner, ref=raw.ref)
        )
        return (True, rv)

    if isinstance(raw, Mapping) and "value" in raw and set(raw.keys()) <= {"value", "ref"}:
        ok, inner = _coerce_inactive_float_scalar(
            raw["value"], allow_null=allow_null, allow_non_finite=allow_non_finite
        )
        ref_raw = raw.get("ref")
        if not ok or (
            ref_raw is not None and not isinstance(ref_raw, (Mapping, Reference))
        ):
            return (False, raw)
        ref_obj = (
            Reference.model_validate(ref_raw)
            if isinstance(ref_raw, Mapping)
            else ref_raw
        )
        rv = (
            RefValue[float].model_construct(value=None, ref=ref_obj)
            if inner is None
            else RefValue[float](value=inner, ref=ref_obj)
        )
        return (True, rv)

    return _coerce_inactive_float_scalar(
        raw, allow_null=allow_null, allow_non_finite=allow_non_finite
    )


def serialise_relaxed_ref_float(value: Any, handler: Any) -> Any:
    """Serialise relaxed inactive ``FlexibleRefValue(float)`` values without Pydantic warnings."""
    if value is None:
        return None
    if isinstance(value, RefValue):
        if value.value is None:
            out: dict[str, Any] = {"value": None}
            if value.ref is not None:
                out["ref"] = (
                    value.ref.model_dump(mode="json", exclude_none=True)
                    if hasattr(value.ref, "model_dump")
                    else value.ref
                )
            return out
        return handler(value)
    return handler(value)


class _WeakConfigRef:
    """Picklable, copy-aware weak reference to an owning ``SUEWSConfig``."""

    __slots__ = ("_ref",)

    def __init__(self, config: Any | None = None) -> None:
        self._ref = weakref.ref(config) if config is not None else None

    def get(self) -> Any | None:
        """Return the owning ``SUEWSConfig`` if still alive, else ``None``."""
        return self._ref() if self._ref is not None else None

    def __getstate__(self) -> None:
        return None

    def __setstate__(self, state: Any) -> None:
        self._ref = None

    def __deepcopy__(self, memo: dict[int, Any]) -> "_WeakConfigRef":
        target = self.get()
        if target is None:
            return _WeakConfigRef(None)
        copied_target = memo.get(id(target))
        if copied_target is not None:
            return _WeakConfigRef(copied_target)
        return _WeakConfigRef(None)
