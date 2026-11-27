from collections.abc import Mapping
from contextvars import ContextVar
from dataclasses import dataclass
import math
from typing import Any, Optional, Union
import warnings
import weakref

import numpy as np
import pandas as pd
from pydantic import (
    ConfigDict,
    Field,
    PrivateAttr,
    ValidationInfo,
    field_serializer,
    field_validator,
    model_validator,
)
from pydantic_core import PydanticUndefined

from ..validation.core.utils import (
    check_missing_params,
    validate_only_when_complete,
    warn_missing_params,
)
from .field_renames import CO2PARAMS_RENAMES, apply_field_renames
from .profile import DayProfile, HourlyProfile, WeeklyProfile
from .type import (
    DataModelBase,
    FlexibleRefValue,
    Reference,
    RefValue,
    df_from_cols,
    merge_df_state_cols,
)
from .unit_metadata import (
    ConditionalUnitCase,
    ConditionalUnitSpec,
    conditional_unit_extra,
)


def coerce_integer_doy_scalar(raw: Any, field_name: str) -> int:
    """Validate and coerce a scalar day-of-year value to ``int`` (without range clamping).

    Accepts Python/NumPy integers, exact whole-number finite floats (such as
    ``86.0`` from legacy YAML or ``df_state`` float columns), and canonical
    base-10 integer strings (such as ``"86"``). Rejects booleans, fractional
    floats (such as ``20.5``), non-finite floats (``NaN``/``Inf``), malformed
    strings, and non-numeric objects with an actionable message pointing the
    caller to integer days of year (1..366).
    """
    if isinstance(raw, np.generic):
        raw = raw.item()
    if isinstance(raw, bool):
        raise ValueError(
            f"{field_name} must be an integer day of year (1..366), got boolean "
            f"{raw!r}; specify a whole integer day of year between 1 and 366."
        )
    if isinstance(raw, int):
        return raw
    if isinstance(raw, float):
        if not math.isfinite(raw):
            raise ValueError(
                f"{field_name} must be a finite integer day of year (1..366), "
                f"got {raw!r}; specify a whole integer day of year between 1 and 366."
            )
        if not raw.is_integer():
            raise ValueError(
                f"{field_name} must be an integer day of year (1..366), got "
                f"fractional value {raw!r}; replace decimal day-of-year values "
                "with an integer day of year between 1 and 366."
            )
        return int(raw)
    if isinstance(raw, str):
        stripped = raw.strip()
        if stripped and (
            stripped.isdigit()
            or (stripped.startswith("-") and len(stripped) > 1 and stripped[1:].isdigit())
        ):
            return int(stripped)
        raise ValueError(
            f"{field_name} must be an integer day of year (1..366), got string "
            f"{raw!r}; specify a whole integer day of year between 1 and 366."
        )
    raise ValueError(
        f"{field_name} must be an integer day of year (1..366), got "
        f"{type(raw).__name__}; specify a whole integer day of year between 1 and 366."
    )


def _is_disabled_doy_mapping(value: Any) -> bool:
    """Return True for empty (``{}``) or ref-only (``{"ref": ...}``) DOY mappings."""
    return (
        isinstance(value, Mapping)
        and "value" not in value
        and all(k == "ref" for k in value)
    )


def _validate_integer_doy_input(value: Any, field_name: str) -> Any:
    """Pre-validator for ``Optional[FlexibleRefValue(int)]`` day-of-year fields."""
    if value is None:
        return None
    if isinstance(value, RefValue):
        if value.value is None:
            return RefValue[int](value=None, ref=value.ref)
        coerced = coerce_integer_doy_scalar(value.value, field_name)
        return RefValue[int](value=coerced, ref=value.ref)
    if isinstance(value, Mapping):
        if "value" in value:
            inner = value["value"]
            if inner is None:
                return dict(value)
            coerced = coerce_integer_doy_scalar(inner, field_name)
            return {**value, "value": coerced}
        if _is_disabled_doy_mapping(value):
            if not value:
                return None
            return {"value": None, "ref": value["ref"]}
        return dict(value)
    return coerce_integer_doy_scalar(value, field_name)


@dataclass(frozen=True)
class _IntegerDOYValueValidator:
    """Picklable bound validator enforced on ``RefValue.value`` assignment."""

    field_name: str

    def __call__(self, raw: Any) -> int:
        val = coerce_integer_doy_scalar(raw, self.field_name)
        if not (1 <= val <= 366):
            raise ValueError(
                f"{self.field_name} must be an integer day of year between 1 "
                "and 366 (greater than or equal to 1 and less than or equal to "
                f"366), got {val}."
            )
        return val


def _bind_doy_ref_validator(value: Any, field_name: str) -> Any:
    """Attach strict integer-DOY assignment validation to a ``RefValue`` wrapper."""
    if isinstance(value, RefValue):
        value._bind_value_validator(_IntegerDOYValueValidator(field_name))
    return value


def _validate_and_bind_doy_assignment(value: Any, field_name: str) -> Any:
    """Validate and bind live attribute assignment on an integer-DOY field."""
    if value is None:
        return None
    validator = _IntegerDOYValueValidator(field_name)
    if isinstance(value, RefValue):
        inner = None if value.value is None else validator(value.value)
        bound = RefValue[int](value=inner, ref=value.ref)
        bound._bind_value_validator(validator)
        return bound
    if isinstance(value, Mapping):
        if "value" in value:
            inner = None if value["value"] is None else validator(value["value"])
            bound = RefValue[int](**{**value, "value": inner})
            bound._bind_value_validator(validator)
            return bound
        if _is_disabled_doy_mapping(value):
            if not value:
                return None
            bound = RefValue[int](value=None, ref=value["ref"])
            bound._bind_value_validator(validator)
            return bound
    return validator(value)


def _is_zero_doy_sentinel(val: Any) -> bool:
    """Return True when ``val`` is numeric zero (excluding booleans)."""
    if isinstance(val, np.generic):
        val = val.item()
    return not isinstance(val, bool) and val == 0


# Validation context propagated from SUEWSConfig during nested construction:
# - None: standalone IrrigationParams construction (no parent model.physics).
# - True: inside SUEWSConfig where model.physics.water_use is MODELLED (0) or unresolved.
# - False: inside SUEWSConfig where model.physics.water_use is OBSERVED (1).
_IRRIGATION_MODELLED_CONTEXT: ContextVar[bool | None] = ContextVar(
    "_IRRIGATION_MODELLED_CONTEXT", default=None
)


def _build_inactive_doy_ref(
    inner: Any, ref_obj: Any, field_name: str
) -> RefValue[int]:
    """Build a ``RefValue[int]`` preserving raw inactive DOY data while binding strict validation."""
    if isinstance(inner, np.generic):
        inner = inner.item()
    if inner is None:
        rv = RefValue[int](value=None, ref=ref_obj)
    else:
        try:
            coerced = coerce_integer_doy_scalar(inner, field_name)
        except ValueError:
            rv = RefValue[int].model_construct(value=inner, ref=ref_obj)
        else:
            rv = (
                RefValue[int](value=coerced, ref=ref_obj)
                if 1 <= coerced <= 366
                else RefValue[int].model_construct(value=inner, ref=ref_obj)
            )
    return _bind_doy_ref_validator(rv, field_name)


def _normalise_inactive_irrigation_doy(raw: Any, field_name: str) -> Any:
    """Normalise a valid/disabled DOY when irrigation is inactive; preserve invalid values.

    Every returned ``RefValue`` keeps the strict integer-DOY assignment validator
    bound so subsequent ``.value = ...`` mutations cannot silently coerce
    booleans or out-of-range integers once irrigation becomes active.
    """
    if isinstance(raw, np.generic):
        raw = raw.item()
    if raw is None:
        return None
    if _is_disabled_doy_mapping(raw):
        if not raw:
            return None
        ref_raw = raw["ref"]
        ref_obj = (
            Reference.model_validate(ref_raw) if isinstance(ref_raw, Mapping) else ref_raw
        )
        return _build_inactive_doy_ref(None, ref_obj, field_name)
    if isinstance(raw, RefValue):
        return _build_inactive_doy_ref(raw.value, raw.ref, field_name)
    if isinstance(raw, Mapping) and "value" in raw and set(raw.keys()) <= {"value", "ref"}:
        ref_raw = raw.get("ref")
        ref_obj = (
            Reference.model_validate(ref_raw) if isinstance(ref_raw, Mapping) else ref_raw
        )
        return _build_inactive_doy_ref(raw["value"], ref_obj, field_name)
    try:
        coerced = coerce_integer_doy_scalar(raw, field_name)
        if 1 <= coerced <= 366:
            return coerced
    except ValueError:
        pass
    return raw


def _df_state_doy_scalar(val: Any) -> int | float:
    """Project an irrigation DOY field to a scalar for ``df_state`` export.

    Valid numeric scalars (including inactive float/int placeholders) are
    preserved as-is; ``None``, booleans, non-finite numbers, oversized
    integers that overflow float conversion, strings, and container
    placeholders accepted when modelled irrigation is inactive are
    projected to neutral ``0`` (disabled), matching the Rust bridge.
    """
    raw = val.value if isinstance(val, RefValue) else val
    if isinstance(raw, np.generic):
        raw = raw.item()
    if raw is None or isinstance(raw, bool):
        return 0
    if isinstance(raw, (int, float)):
        try:
            return raw if math.isfinite(float(raw)) else 0
        except OverflowError:
            return 0
    return 0


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


class IrrigationParams(
    DataModelBase
):  # TODO: May need to add RefValue to the profiles here
    """Parameters for irrigation and water use management."""

    model_config = ConfigDict(title="Irrigation")

    _doy_validation_active: bool = PrivateAttr(default=True)
    _water_use_modelled: bool | None = PrivateAttr(default=None)
    _parent_config_ref: _WeakConfigRef | None = PrivateAttr(default=None)

    def _is_doy_validation_active(self) -> bool:
        """Return True when live ``ie_start``/``ie_end`` mutations enforce strict DOY bounds."""
        parent_ref = getattr(self, "_parent_config_ref", None)
        parent_cfg = parent_ref.get() if parent_ref is not None else None
        if parent_cfg is not None:
            from ..validation.rules.hard_invariants import (
                is_modelled_irrigation_active,
                resolve_water_use_code,
            )

            model = getattr(parent_cfg, "model", None)
            physics = getattr(model, "physics", None) if model is not None else None
            water_use = getattr(physics, "water_use", None) if physics is not None else None
            code = resolve_water_use_code(water_use)
            if code is not None and code != 0:
                return False
            return is_modelled_irrigation_active(0, self)
        water_use_modelled = getattr(self, "_water_use_modelled", None)
        if water_use_modelled is None:
            return bool(getattr(self, "_doy_validation_active", True))
        if not water_use_modelled:
            return False
        from ..validation.rules.hard_invariants import is_modelled_irrigation_active

        return is_modelled_irrigation_active(0, self)

    def _sync_doy_validation_state(
        self,
        *,
        active: bool,
        water_use_modelled: bool | None = None,
        config: Any | None = None,
    ) -> None:
        """Synchronise DOY validation state and ensure ``RefValue`` wrappers bind strict validation."""
        if water_use_modelled is not None:
            object.__setattr__(self, "_water_use_modelled", water_use_modelled)
        if config is not None:
            object.__setattr__(self, "_parent_config_ref", _WeakConfigRef(config))
        object.__setattr__(self, "_doy_validation_active", active)
        for field_name in ("ie_start", "ie_end"):
            current = getattr(self, field_name, None)
            if isinstance(current, RefValue):
                _bind_doy_ref_validator(current, field_name)

    h_maintain: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Water depth to maintain through irrigation",
        json_schema_extra={"unit": "kg m-2", "display_name": "Maintain Height"},
    )
    faut: FlexibleRefValue(float) = Field(
        default=0.0,
        description="Fraction of automatic irrigation",
        json_schema_extra={
            "unit": "1",
            "display_name": "Automatic Fraction",
        },
        ge=0.0,
        le=1.0,
    )
    ie_start: Optional[FlexibleRefValue(int)] = Field(
        default=None,
        ge=1,
        le=366,
        description="Integer day of year (1-366) when irrigation starts",
        json_schema_extra={"unit": "1", "display_name": "Irrigation Start Day"},
    )
    ie_end: Optional[FlexibleRefValue(int)] = Field(
        default=None,
        ge=1,
        le=366,
        description="Integer day of year (1-366) when irrigation ends",
        json_schema_extra={"unit": "1", "display_name": "Irrigation End Day"},
    )

    @model_validator(mode="wrap")
    @classmethod
    def _contextualise_irrigation_doys(cls, data: Any, handler: Any) -> "IrrigationParams":
        """Skip strict ie_start/ie_end bounds during SUEWSConfig load when irrigation is inactive."""
        if isinstance(data, IrrigationParams):
            return handler(data)
        water_use_ctx = _IRRIGATION_MODELLED_CONTEXT.get()
        if water_use_ctx is None or not isinstance(data, Mapping):
            return handler(data)

        from ..validation.rules.hard_invariants import (
            is_modelled_irrigation_active,
        )

        active = bool(water_use_ctx) and is_modelled_irrigation_active(0, data)
        if active:
            inst = handler(data)
            object.__setattr__(inst, "_water_use_modelled", bool(water_use_ctx))
            object.__setattr__(inst, "_doy_validation_active", True)
            return inst

        stashed: dict[str, Any] = {}
        stripped: dict[str, Any] = {}
        for key, val in data.items():
            if key in {"ie_start", "ie_end"}:
                stashed[key] = val
            else:
                stripped[key] = val
        inst = handler(stripped)
        object.__setattr__(inst, "_water_use_modelled", bool(water_use_ctx))
        object.__setattr__(inst, "_doy_validation_active", False)
        for field_name, raw_val in stashed.items():
            normalised = _normalise_inactive_irrigation_doy(raw_val, field_name)
            object.__setattr__(inst, field_name, normalised)
            inst.model_fields_set.add(field_name)
        return inst

    @field_validator("ie_start", "ie_end", mode="before")
    @classmethod
    def _validate_irrigation_doy(cls, v: Any, info: ValidationInfo) -> Any:
        return _validate_integer_doy_input(v, info.field_name or "ie_start/ie_end")

    @field_validator("ie_start", "ie_end", mode="after")
    @classmethod
    def _bind_irrigation_doy_ref(cls, v: Any, info: ValidationInfo) -> Any:
        return _bind_doy_ref_validator(v, info.field_name or "ie_start/ie_end")

    @field_serializer("ie_start", "ie_end", mode="wrap")
    def _serialise_irrigation_doy(self, value: Any, handler: Any) -> Any:
        """Serialise inactive non-integer DOY placeholders without Pydantic warnings."""
        if value is None:
            return None
        if isinstance(value, RefValue):
            inner = value.value
            if inner is None or (isinstance(inner, int) and not isinstance(inner, bool)):
                return handler(value)
            out: dict[str, Any] = {"value": inner}
            if value.ref is not None:
                out["ref"] = (
                    value.ref.model_dump(mode="json", exclude_none=True)
                    if hasattr(value.ref, "model_dump")
                    else value.ref
                )
            return out
        if isinstance(value, int) and not isinstance(value, bool):
            return handler(value)
        return value

    def __setattr__(self, name: str, value: Any) -> None:
        if name in {"ie_start", "ie_end"}:
            if self._is_doy_validation_active():
                value = _validate_and_bind_doy_assignment(value, name)
            else:
                value = _normalise_inactive_irrigation_doy(value, name)
        super().__setattr__(name, value)
    internalwateruse_h: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Internal water use rate",
        json_schema_extra={
            "unit": "mm h-1",
            "display_name": "Internal Water Use Rate",
        },
    )
    daywatper: WeeklyProfile = Field(
        default_factory=WeeklyProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Weekly Water Use Fraction",
        },
    )
    daywat: WeeklyProfile = Field(
        default_factory=WeeklyProfile,
        json_schema_extra={"unit": None, "display_name": "Weekly Water Use"},
    )
    wuprofa_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile,
        description="24-hour profile of automatic irrigation water use",
        json_schema_extra={
            "unit": "1",
            "display_name": "Water Use Profile Automatic (24hr)",
        },
    )
    wuprofm_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile,
        description="24-hour profile of manual irrigation water use",
        json_schema_extra={
            "unit": "1",
            "display_name": "Water Use Profile Manual (24hr)",
        },
    )

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = {
            ("gridiv", "0"): grid_id,
            ("h_maintain", "0"): (
                self.h_maintain.value
                if isinstance(self.h_maintain, RefValue)
                else self.h_maintain
                if self.h_maintain is not None
                else 0.0
            ),
            ("faut", "0"): (
                self.faut.value if isinstance(self.faut, RefValue) else self.faut
            ),
            ("ie_start", "0"): _df_state_doy_scalar(self.ie_start),
            ("ie_end", "0"): _df_state_doy_scalar(self.ie_end),
            ("internalwateruse_h", "0"): (
                self.internalwateruse_h.value
                if isinstance(self.internalwateruse_h, RefValue)
                else self.internalwateruse_h
                if self.internalwateruse_h is not None
                else 0.0
            ),
        }
        merge_df_state_cols(
            cols, self.daywatper._to_df_state_cols(grid_id, "daywatper"), fill_na=True
        )
        merge_df_state_cols(
            cols, self.daywat._to_df_state_cols(grid_id, "daywat"), fill_na=True
        )
        merge_df_state_cols(
            cols, self.wuprofa_24hr._to_df_state_cols(grid_id, "wuprofa_24hr"), fill_na=True
        )
        merge_df_state_cols(
            cols, self.wuprofm_24hr._to_df_state_cols(grid_id, "wuprofm_24hr"), fill_na=True
        )
        return dict(sorted(cols.items()))

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert irrigation parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing irrigation parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "IrrigationParams":
        """
        Reconstruct IrrigationParams from a DataFrame state format.

        Args:
            df: DataFrame containing irrigation parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            IrrigationParams: Instance of IrrigationParams
        """
        # Extract scalar attributes
        h_maintain = df.loc[grid_id, ("h_maintain", "0")]
        faut = df.loc[grid_id, ("faut", "0")]
        ie_start_val = df.loc[grid_id, ("ie_start", "0")]
        ie_end_val = df.loc[grid_id, ("ie_end", "0")]
        internalwateruse_h = df.loc[grid_id, ("internalwateruse_h", "0")]

        # Convert to RefValue (treat 0 / 0.0 as None since to_df_state writes None as 0;
        # preserve NaN so active irrigation validation rejects non-finite DOYs loudly).
        h_maintain = RefValue(h_maintain)
        faut = RefValue(faut)
        ie_start = (
            None if _is_zero_doy_sentinel(ie_start_val) else RefValue(ie_start_val)
        )
        ie_end = None if _is_zero_doy_sentinel(ie_end_val) else RefValue(ie_end_val)
        internalwateruse_h = RefValue(internalwateruse_h)

        # Extract WeeklyProfile attributes
        daywatper = WeeklyProfile.from_df_state(df, grid_id, "daywatper")
        daywat = WeeklyProfile.from_df_state(df, grid_id, "daywat")

        # Extract HourlyProfile attributes
        wuprofa_24hr = HourlyProfile.from_df_state(df, grid_id, "wuprofa_24hr")
        wuprofm_24hr = HourlyProfile.from_df_state(df, grid_id, "wuprofm_24hr")

        # Construct and return the IrrigationParams instance
        return cls(
            h_maintain=h_maintain,
            faut=faut,
            ie_start=ie_start,
            ie_end=ie_end,
            internalwateruse_h=internalwateruse_h,
            daywatper=daywatper,
            daywat=daywat,
            wuprofa_24hr=wuprofa_24hr,
            wuprofm_24hr=wuprofm_24hr,
        )


class AnthropogenicHeat(
    DataModelBase
):  # TODO: May need to add the RefValue to the profiles here
    """Anthropogenic heat flux parameters and profiles."""

    model_config = ConfigDict(title="Anthropogenic Heat")

    # Sample/range metadata values below are derived from:
    # - test/fixtures/benchmark1/benchmark1.yml (Ward et al. 2016 benchmark setup)
    # - docs/source/inputs/tables/SUEWS_SiteInfo/sample-table/SUEWS_AnthropogenicEmission.txt
    qf0_beu: DayProfile = Field(
        description=(
            "Fraction of the temperature-independent (base) anthropogenic heat flux "
            "that comes from building energy use. Used to separate the building "
            "share of heat and CO2 emissions. In the methods with codes ending in "
            "1, 2 or 3 the rest of the base flux, less human metabolism, is "
            "attributed to traffic."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Building Fraction of Base QF",
        },
    )
    qf_a: DayProfile = Field(
        description=(
            "Temperature-independent (base) anthropogenic heat flux per unit "
            "population density, that is per person, in the J11 heat basis (method "
            "codes ending in 2 or 5)."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2 ha",
            "display_name": "QF Coefficient A",
            "range_description": "Sample range from benchmark/sample tables: 0.10-0.37 (working day); calibrate by site.",
        },
    )
    qf_b: DayProfile = Field(
        description=(
            "Cooling coefficient in the J11 heat basis (method codes ending in 2 "
            "or 5): anthropogenic heat flux per cooling degree day and per unit "
            "population density, that is per person."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2 K-1 ha",
            "display_name": "QF Coefficient B",
            "range_description": "Sample range from benchmark/sample tables: 0.0-0.01; calibrate by site.",
        },
    )
    qf_c: DayProfile = Field(
        description=(
            "Heating coefficient in the J11 heat basis (method codes ending in 2 "
            "or 5): anthropogenic heat flux per heating degree day and per unit "
            "population density, that is per person."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2 K-1 ha",
            "display_name": "QF Coefficient C",
            "range_description": "Sample range from benchmark/sample tables: 0.0037-0.0400 (benchmark1 is ~0.007); calibrate by site.",
        },
    )
    baset_cooling: DayProfile = Field(
        description=(
            "Base temperature for cooling degree days. In the L11_UPDATED heat "
            "basis (method codes ending in 3 or 6) it is also the critical "
            "temperature above which the cooling term applies, compared with a "
            "running mean of daily mean air temperature over about five days."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "degC",
            "display_name": "Base Temperature Cooling",
            "default_description": "Sample benchmark value: 18.2 degC (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    baset_heating: DayProfile = Field(
        description=(
            "Base temperature for heating degree days. In the L11 and L11_UPDATED "
            "heat bases (method codes ending in 1, 3, 4 or 6) it is also the "
            "critical air temperature below which the heating term applies."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "degC",
            "display_name": "Base Temperature Heating",
            "default_description": "Sample benchmark value: 18.2 degC (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    ah_min: DayProfile = Field(
        description=(
            "Temperature-independent (base) anthropogenic heat flux in the L11 and "
            "L11_UPDATED heat bases (method codes ending in 1, 3, 4 or 6), as a "
            "daily mean and not a lower bound: at each timestep it is multiplied by "
            "ahprof_24hr normalised to a mean of 1, so the modelled base term is "
            "above this value at some hours and below it at others. In the detailed "
            "methods (codes ending in 4 or 6) the building heat flux is rescaled by "
            "the population profile instead. Not used by the J11 heat basis (codes "
            "ending in 2 or 5)."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2",
            "display_name": "Base Anthropogenic Heat",
            "default_description": "Sample benchmark value: 15.0 W m^-2 (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    ah_slope_cooling: DayProfile = Field(
        description=(
            "Increase in anthropogenic heat flux per degree by which a running mean "
            "of daily mean air temperature (about five days) exceeds baset_cooling. "
            "Used only by the L11_UPDATED heat basis (method codes ending in 3 or "
            "6); scaled by the normalised ahprof_24hr."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2 K-1",
            "display_name": "Anthropogenic Heat Slope (Cooling)",
            "default_description": "Sample benchmark value: 2.7 W m^-2 K^-1 (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    ah_slope_heating: DayProfile = Field(
        description=(
            "Increase in anthropogenic heat flux per degree of air temperature below "
            "baset_heating: instantaneous air temperature in the L11 heat basis "
            "(method codes ending in 1 or 4), a running mean of daily mean air "
            "temperature over about five days in L11_UPDATED (codes ending in 3 or "
            "6). Scaled by the normalised ahprof_24hr."
        ),
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "W m-2 K-1",
            "display_name": "Anthropogenic Heat Slope (Heating)",
            "default_description": "Sample benchmark value: 2.7 W m^-2 K^-1 (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    ahprof_24hr: HourlyProfile = Field(
        description="24-hour profile of anthropogenic heat flux",
        default_factory=HourlyProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Anthropogenic Heat Profile (24hr)",
        },
    )
    popdensdaytime: DayProfile = Field(
        description="Daytime population density",
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "ha-1",
            "display_name": "Daytime Population Density",
        },
    )
    popdensnighttime: float = Field(
        default=10.0,
        description="Nighttime population density",
        json_schema_extra={
            "unit": "ha-1",
            "display_name": "Nighttime Population Density",
        },
        ge=0.0,
    )
    popprof_24hr: HourlyProfile = Field(
        description="24-hour profile of population density",
        default_factory=HourlyProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Population Profile (24hr)",
        },
    )

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = {
            ("gridiv", "0"): grid_id,
            ("popdensnighttime", "0"): self.popdensnighttime,
        }
        day_profiles = {
            "qf0_beu": self.qf0_beu,
            "qf_a": self.qf_a,
            "qf_b": self.qf_b,
            "qf_c": self.qf_c,
            "baset_cooling": self.baset_cooling,
            "baset_heating": self.baset_heating,
            "ah_min": self.ah_min,
            "ah_slope_cooling": self.ah_slope_cooling,
            "ah_slope_heating": self.ah_slope_heating,
            "popdensdaytime": self.popdensdaytime,
        }
        for param_name, profile in day_profiles.items():
            merge_df_state_cols(
                cols, profile._to_df_state_cols(grid_id, param_name), fill_na=True
            )
        hourly_profiles = {
            "ahprof_24hr": self.ahprof_24hr,
            "popprof_24hr": self.popprof_24hr,
        }
        for param_name, profile in hourly_profiles.items():
            merge_df_state_cols(
                cols, profile._to_df_state_cols(grid_id, param_name), fill_na=True
            )
        return dict(sorted(cols.items()))

    # DayProfile coulmns need to be fixed
    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert anthropogenic heat parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing anthropogenic heat parameters.
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "AnthropogenicHeat":
        """
        Reconstruct AnthropogenicHeat from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing anthropogenic heat parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            AnthropogenicHeat: Instance of AnthropogenicHeat.
        """

        # Extract DayProfile attributes
        day_profiles = {
            "qf0_beu": DayProfile.from_df_state(df, grid_id, "qf0_beu"),
            "qf_a": DayProfile.from_df_state(df, grid_id, "qf_a"),
            "qf_b": DayProfile.from_df_state(df, grid_id, "qf_b"),
            "qf_c": DayProfile.from_df_state(df, grid_id, "qf_c"),
            "baset_cooling": DayProfile.from_df_state(df, grid_id, "baset_cooling"),
            "baset_heating": DayProfile.from_df_state(df, grid_id, "baset_heating"),
            "ah_min": DayProfile.from_df_state(df, grid_id, "ah_min"),
            "ah_slope_cooling": DayProfile.from_df_state(
                df, grid_id, "ah_slope_cooling"
            ),
            "ah_slope_heating": DayProfile.from_df_state(
                df, grid_id, "ah_slope_heating"
            ),
            "popdensdaytime": DayProfile.from_df_state(df, grid_id, "popdensdaytime"),
        }

        # Extract HourlyProfile attributes
        hourly_profiles = {
            "ahprof_24hr": HourlyProfile.from_df_state(df, grid_id, "ahprof_24hr"),
            "popprof_24hr": HourlyProfile.from_df_state(df, grid_id, "popprof_24hr"),
        }

        # Extract scalar attribute
        popdensnighttime = df.loc[grid_id, ("popdensnighttime", "0")]

        # Construct and return AnthropogenicHeat instance
        return cls(
            **day_profiles,
            **hourly_profiles,
            popdensnighttime=popdensnighttime,
        )


class CO2Params(DataModelBase):  # TODO: May need to add the RefValue to the profiles here
    """CO2 emission parameters and profiles."""

    model_config = ConfigDict(title="CO2 Emissions")

    @model_validator(mode="before")
    @classmethod
    def _rename_co2_fields(cls, values):
        if isinstance(values, dict):
            return apply_field_renames(values, CO2PARAMS_RENAMES, cls.__name__)
        return values

    # Sample/range metadata values below are derived from:
    # - test/fixtures/benchmark1/benchmark1.yml (Ward et al. 2016 benchmark setup)
    # - docs/source/inputs/tables/SUEWS_SiteInfo/sample-table/SUEWS_AnthropogenicEmission.txt
    emission_co2_point_source: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Whole-grid CO2 point-source emission expressed as carbon mass",
        json_schema_extra={"unit": "kg d-1", "display_name": "CO2 Point Source"},
    )
    emission_factor_co2_fuel: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="CO2 emission factor per unit of fuel energy",
        json_schema_extra={
            "unit": "umol J-1",
            "display_name": "Emission Factor (umol CO2/J)",
            "default_description": "Sample benchmark value: 1.159 umol J^-1 (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    emission_factor_energy_vehicle: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Vehicle energy consumption per unit distance",
        json_schema_extra={
            "unit": "J km-1",
            "display_name": "Energy Emission Factor Vehicles",
            "range_description": "Sample range from benchmark/sample tables: 3.97e6-4.11e6 J km^-1; calibrate by site.",
        },
    )
    emission_factor_co2_vehicle: Optional[DayProfile] = Field(
        description="Vehicle CO2 emission factor per unit distance",
        default_factory=DayProfile,
        json_schema_extra={
            "unit": "kg km-1",
            "display_name": "Fuel Carbon Emission Factor Vehicles",
            "default_description": "Sample benchmark value: 0.285 kg CO2 km^-1 (Ward et al. 2016, central London); calibrate by site.",
        },
    )
    # Field is Optional[DayProfile] but has default_factory=DayProfile,
    # so it will never actually be None unless explicitly set during
    # validation preprocessing.

    fraction_fossil_fuel_heating: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Fraction of heating energy from fossil fuels",
        json_schema_extra={
            "unit": "1",
            "display_name": "Fossil Fuel Fraction Heating",
            "range_description": "Sample range from benchmark/sample tables: 0.05-0.7 (depends on local energy mix).",
        },
        ge=0.0,
        le=1.0,
    )
    fraction_fossil_fuel_non_heating: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Fraction of non-heating energy from fossil fuels",
        json_schema_extra={
            "unit": "1",
            "display_name": "Fossil Fuel Fraction Non-Heating",
            "range_description": "Sample range from benchmark/sample tables: 0.0-0.7 (depends on local energy mix).",
        },
        ge=0.0,
        le=1.0,
    )
    emission_co2_metabolism_max: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Maximum daytime metabolic CO2 emission per capita",
        json_schema_extra={
            "unit": "umol s-1",
            "display_name": "Maximum Metabolic CO2 Flux",
            "default_description": "Sample value from benchmark/sample tables: 280.0 umol cap^-1 s^-1; calibrate by site.",
        },
    )
    emission_heat_metabolism_max: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Maximum daytime metabolic heat emission per capita",
        json_schema_extra={
            "unit": "W",
            "display_name": "Maximum Metabolic Heat Flux",
            "default_description": "Sample value from benchmark/sample tables: 175.0 W cap^-1; calibrate by site.",
        },
    )
    emission_co2_metabolism_min: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Minimum night-time metabolic CO2 emission per capita",
        json_schema_extra={
            "unit": "umol s-1",
            "display_name": "Minimum Metabolic CO2 Flux",
            "default_description": "Sample value from benchmark/sample tables: 120.0 umol cap^-1 s^-1; calibrate by site.",
        },
    )
    emission_heat_metabolism_min: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Minimum night-time metabolic heat emission per capita",
        json_schema_extra={
            "unit": "W",
            "display_name": "Minimum Metabolic Heat Flux",
            "default_description": "Sample value from benchmark/sample tables: 75.0 W cap^-1; calibrate by site.",
        },
    )
    traffic_rate: Optional[DayProfile] = Field(
        description="Mean traffic activity; basis selected by type_traffic_rate",
        default_factory=DayProfile,
        json_schema_extra={
            **conditional_unit_extra(
                ConditionalUnitSpec(
                    selector="type_traffic_rate",
                    selector_scope="sibling",
                    selector_model="CO2Params",
                    selector_field="type_traffic_rate",
                    cases={
                        "1": ConditionalUnitCase(
                            label="Per area",
                            kind="cf",
                            unit="km m-2 d-1",
                            formula=(
                                "QF_traff = traffic_rate / 86400 * "
                                "emission_factor_energy_vehicle * profile_traffic; "
                                "Fc_traff = traffic_rate / 86400 * "
                                "emission_factor_co2_vehicle * 1e9 / 44 * "
                                "profile_traffic"
                            ),
                            note=(
                                "Daily vehicle-km per m2 of grid area (counted "
                                "vehicles leave the CF unit string; 86400 s d-1 "
                                "converts per-day activity to per-second fluxes)."
                            ),
                        ),
                        "2": ConditionalUnitCase(
                            label="Per capita",
                            kind="cf",
                            unit="km d-1",
                            formula=(
                                "QF_traff = traffic_rate / 86400 * "
                                "emission_factor_energy_vehicle * profile_traffic * "
                                "(pop_density / 10000); "
                                "Fc_traff = traffic_rate / 86400 * "
                                "emission_factor_co2_vehicle * 1e9 / 44 * "
                                "profile_traffic * (pop_density / 10000)"
                            ),
                            note=(
                                "Daily vehicle-km per person (counted vehicles and "
                                "people leave the CF unit string; pop_density in "
                                "ha-1 is divided by 10000 m2 ha-1 to convert "
                                "per-capita traffic to grid area)."
                            ),
                        ),
                    },
                    activation_note=(
                        "Used only when model.physics.emissions selects a detailed "
                        "anthropogenic branch (codes 4-6, 14-16, 24-26, 34-36, "
                        "44-46); codes 4-6 still include traffic heat QF_traff "
                        "while zeroing Fc_anthro in the driver."
                    ),
                    inactive_note=(
                        "Ignored when model.physics.emissions is 0 or ends in 1-3; "
                        "inside detailed emissions branches, any type_traffic_rate "
                        "other than 1 or 2 (including unset/0) raises Fortran "
                        "ErrorHint(75)."
                    ),
                )
            ),
            "display_name": "Traffic Rate",
        },
    )
    # Field is Optional[DayProfile] but has default_factory=DayProfile,
    # so it will never actually be None unless explicitly set during
    # validation preprocessing.

    type_traffic_rate: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Traffic-rate basis: 1 for per area, 2 for per capita",
        json_schema_extra={
            "unit": None,
            "display_name": "Traffic Rate Type",
        },
    )
    profile_traffic_24hr: Optional[HourlyProfile] = Field(
        description="24-hour profile of traffic rate",
        default_factory=HourlyProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Traffic Profile (24hr)",
        },
    )
    # Field is Optional[HourlyProfile] but has default_factory=HourlyProfile,
    # so it will never actually be None unless explicitly set during
    # validation preprocessing.

    profile_human_activity_24hr: Optional[HourlyProfile] = Field(
        description="24-hour profile of human activity",
        default_factory=HourlyProfile,
        json_schema_extra={
            "unit": "1",
            "display_name": "Human Activity Profile (24hr)",
        },
    )
    # Field is Optional[HourlyProfile] but has default_factory=HourlyProfile,
    # so it will never actually be None unless explicitly set during
    # validation preprocessing.

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int) -> dict:
        scalar_params = {
            "co2pointsource": self.emission_co2_point_source.value
            if isinstance(self.emission_co2_point_source, RefValue)
            else self.emission_co2_point_source
            if self.emission_co2_point_source is not None
            else 0.0,
            "ef_umolco2perj": self.emission_factor_co2_fuel.value
            if isinstance(self.emission_factor_co2_fuel, RefValue)
            else self.emission_factor_co2_fuel
            if self.emission_factor_co2_fuel is not None
            else 0.0,
            "enef_v_jkm": self.emission_factor_energy_vehicle.value
            if isinstance(self.emission_factor_energy_vehicle, RefValue)
            else self.emission_factor_energy_vehicle
            if self.emission_factor_energy_vehicle is not None
            else 0.0,
            "frfossilfuel_heat": self.fraction_fossil_fuel_heating.value
            if isinstance(self.fraction_fossil_fuel_heating, RefValue)
            else self.fraction_fossil_fuel_heating
            if self.fraction_fossil_fuel_heating is not None
            else 0.0,
            "frfossilfuel_nonheat": self.fraction_fossil_fuel_non_heating.value
            if isinstance(self.fraction_fossil_fuel_non_heating, RefValue)
            else self.fraction_fossil_fuel_non_heating
            if self.fraction_fossil_fuel_non_heating is not None
            else 0.0,
            "maxfcmetab": self.emission_co2_metabolism_max.value
            if isinstance(self.emission_co2_metabolism_max, RefValue)
            else self.emission_co2_metabolism_max
            if self.emission_co2_metabolism_max is not None
            else 0.0,
            "maxqfmetab": self.emission_heat_metabolism_max.value
            if isinstance(self.emission_heat_metabolism_max, RefValue)
            else self.emission_heat_metabolism_max
            if self.emission_heat_metabolism_max is not None
            else 0.0,
            "minfcmetab": self.emission_co2_metabolism_min.value
            if isinstance(self.emission_co2_metabolism_min, RefValue)
            else self.emission_co2_metabolism_min
            if self.emission_co2_metabolism_min is not None
            else 0.0,
            "minqfmetab": self.emission_heat_metabolism_min.value
            if isinstance(self.emission_heat_metabolism_min, RefValue)
            else self.emission_heat_metabolism_min
            if self.emission_heat_metabolism_min is not None
            else 0.0,
            "trafficunits": self.type_traffic_rate.value
            if isinstance(self.type_traffic_rate, RefValue)
            else self.type_traffic_rate
            if self.type_traffic_rate is not None
            else 0.0,
        }
        cols = {("gridiv", "0"): grid_id}
        for param_name, value in scalar_params.items():
            cols[(param_name, "0")] = value

        day_profiles = {
            "fcef_v_kgkm": self.emission_factor_co2_vehicle,
            "trafficrate": self.traffic_rate,
        }
        for param_name, profile in day_profiles.items():
            merge_df_state_cols(
                cols, profile._to_df_state_cols(grid_id, param_name), fill_na=True
            )

        hourly_profiles = {
            "traffprof_24hr": self.profile_traffic_24hr,
            "humactivity_24hr": self.profile_human_activity_24hr,
        }
        for param_name, profile in hourly_profiles.items():
            merge_df_state_cols(
                cols, profile._to_df_state_cols(grid_id, param_name), fill_na=True
            )

        return dict(sorted(cols.items()))

    # DayProfile coulmns need to be fixed
    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert CO2 parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing CO2 parameters.
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "CO2Params":
        """
        Reconstruct CO2Params from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing CO2 parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            CO2Params: Instance of CO2Params.
        """

        # Extract scalar attributes
        scalar_params = {
            "emission_co2_point_source": df.loc[grid_id, ("co2pointsource", "0")],
            "emission_factor_co2_fuel": df.loc[grid_id, ("ef_umolco2perj", "0")],
            "emission_factor_energy_vehicle": df.loc[grid_id, ("enef_v_jkm", "0")],
            "fraction_fossil_fuel_heating": df.loc[grid_id, ("frfossilfuel_heat", "0")],
            "fraction_fossil_fuel_non_heating": df.loc[
                grid_id, ("frfossilfuel_nonheat", "0")
            ],
            "emission_co2_metabolism_max": df.loc[grid_id, ("maxfcmetab", "0")],
            "emission_heat_metabolism_max": df.loc[grid_id, ("maxqfmetab", "0")],
            "emission_co2_metabolism_min": df.loc[grid_id, ("minfcmetab", "0")],
            "emission_heat_metabolism_min": df.loc[grid_id, ("minqfmetab", "0")],
            "type_traffic_rate": df.loc[grid_id, ("trafficunits", "0")],
        }

        # Convert scalar attributes to RefValue
        scalar_params = {key: RefValue(value) for key, value in scalar_params.items()}

        # Extract DayProfile attributes
        day_profiles = {
            "emission_factor_co2_vehicle": DayProfile.from_df_state(
                df, grid_id, "fcef_v_kgkm"
            ),
            "traffic_rate": DayProfile.from_df_state(df, grid_id, "trafficrate"),
        }

        # Extract HourlyProfile attributes
        hourly_profiles = {
            "profile_traffic_24hr": HourlyProfile.from_df_state(
                df, grid_id, "traffprof_24hr"
            ),
            "profile_human_activity_24hr": HourlyProfile.from_df_state(
                df, grid_id, "humactivity_24hr"
            ),
        }

        # Construct and return CO2Params instance
        return cls(
            **scalar_params,
            **day_profiles,
            **hourly_profiles,
        )


class AnthropogenicEmissions(DataModelBase):
    model_config = ConfigDict(title="Anthropogenic Emissions")

    startdls: Optional[FlexibleRefValue(int)] = Field(
        default=None,
        ge=1,
        le=366,
        description="Start of daylight savings time in integer day of year (1-366)",
        json_schema_extra={"unit": "1", "display_name": "Daylight Saving Start"},
    )
    enddls: Optional[FlexibleRefValue(int)] = Field(
        default=None,
        ge=1,
        le=366,
        description="End of daylight savings time in integer day of year (1-366)",
        json_schema_extra={"unit": "1", "display_name": "Daylight Saving End"},
    )

    @field_validator("startdls", "enddls", mode="before")
    @classmethod
    def _validate_dls_doy(cls, v: Any, info: ValidationInfo) -> Any:
        return _validate_integer_doy_input(v, info.field_name or "startdls/enddls")

    @field_validator("startdls", "enddls", mode="after")
    @classmethod
    def _bind_dls_doy_ref(cls, v: Any, info: ValidationInfo) -> Any:
        return _bind_doy_ref_validator(v, info.field_name or "startdls/enddls")

    def __setattr__(self, name: str, value: Any) -> None:
        if name in {"startdls", "enddls"}:
            value = _validate_and_bind_doy_assignment(value, name)
        super().__setattr__(name, value)
    heat: AnthropogenicHeat = Field(
        description="Anthropogenic heat emission parameters",
        default_factory=AnthropogenicHeat,
        json_schema_extra={"display_name": "Heat"},
    )
    co2: CO2Params = Field(
        description="CO2 emission parameters",
        default_factory=CO2Params,
        json_schema_extra={"display_name": "CO2"},
    )

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = {
            ("gridiv", "0"): grid_id,
            ("startdls", "0"): (
                self.startdls.value
                if isinstance(self.startdls, RefValue)
                and self.startdls.value is not None
                else self.startdls
                if self.startdls is not None
                and not isinstance(self.startdls, RefValue)
                else 0
            ),
            ("enddls", "0"): (
                self.enddls.value
                if isinstance(self.enddls, RefValue)
                and self.enddls.value is not None
                else self.enddls
                if self.enddls is not None
                and not isinstance(self.enddls, RefValue)
                else 0
            ),
        }
        merge_df_state_cols(cols, self.heat._to_df_state_cols(grid_id))
        merge_df_state_cols(cols, self.co2._to_df_state_cols(grid_id))
        return cols

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert anthropogenic emissions parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing anthropogenic emissions parameters.
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "AnthropogenicEmissions":
        """
        Reconstruct AnthropogenicEmissions from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing anthropogenic emissions parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            AnthropogenicEmissions: Instance of AnthropogenicEmissions.
        """
        # Treat 0 / 0.0 as None (since to_df_state writes None as 0)
        startdls_val = df.loc[grid_id, ("startdls", "0")]
        enddls_val = df.loc[grid_id, ("enddls", "0")]

        startdls = (
            None if _is_zero_doy_sentinel(startdls_val) else RefValue(startdls_val)
        )
        enddls = None if _is_zero_doy_sentinel(enddls_val) else RefValue(enddls_val)

        # Reconstruct heat parameters
        heat = AnthropogenicHeat.from_df_state(df, grid_id)

        # Reconstruct CO2 parameters
        co2 = CO2Params.from_df_state(df, grid_id)

        return cls(startdls=startdls, enddls=enddls, heat=heat, co2=co2)
