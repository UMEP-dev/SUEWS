from collections.abc import Callable, Mapping
from copy import deepcopy
import math
from typing import Annotated, Any, Dict, Optional

import numpy as np
import pandas as pd
from pydantic import (
    ConfigDict,
    Field,
    SerializationInfo,
    field_serializer,
    field_validator,
    model_validator,
)

from .field_renames import RAW_YAML_FIELD_RENAMES
from .type import (
    _STEBBS_ACTIVE_CONTEXT,
    DataModelBase,
    Reference,
    _coerce_inactive_stebbs_scalar,
    df_from_cols,
    is_finite_real_number,
)

CANONICAL_HOUR_KEYS: tuple[str, ...] = tuple(str(hour) for hour in range(1, 25))
_CANONICAL_HOUR_KEY_SET: frozenset[str] = frozenset(CANONICAL_HOUR_KEYS)
DAY_NIGHT_PROFILE_MIN: float = 1.0
DAY_NIGHT_PROFILE_MAX: float = 2.0
DAY_NIGHT_PROFILE_DEFAULT: float = 1.0
DAY_NIGHT_HOURLY_PROFILE_NAMES: frozenset[str] = frozenset({
    "anthropogenic_emissions.heat.popprof_24hr",
    "anthropogenic_emissions.co2.profile_human_activity_24hr",
})
CANONICAL_TEN_MINUTE_KEYS: tuple[str, ...] = tuple(str(step) for step in range(1, 145))
_CANONICAL_TEN_MINUTE_KEY_SET: frozenset[str] = frozenset(CANONICAL_TEN_MINUTE_KEYS)

_HOURLY_PROFILE_PATHS: tuple[tuple[str, ...], ...] = (
    ("snow", "snow_profile_24hr"),
    ("snow", "snowprof_24hr"),
    ("irrigation", "wuprofa_24hr"),
    ("irrigation", "wuprofm_24hr"),
    ("anthropogenic_emissions", "heat", "ahprof_24hr"),
    ("anthropogenic_emissions", "heat", "popprof_24hr"),
    ("anthropogenic_emissions", "co2", "profile_traffic_24hr"),
    ("anthropogenic_emissions", "co2", "traffprof_24hr"),
    ("anthropogenic_emissions", "co2", "profile_human_activity_24hr"),
    ("anthropogenic_emissions", "co2", "humactivity_24hr"),
)
HOURLY_PROFILE_FIELD_NAMES: frozenset[str] = frozenset(
    path[-1] for path in _HOURLY_PROFILE_PATHS
)

TEN_MINUTE_PROFILE_PATHS: tuple[tuple[str, str], ...] = (
    ("building_archetype", "profile_metabolism"),
    ("building_archetype", "profile_setpoint_temperature_heating_air"),
    ("building_archetype", "profile_setpoint_temperature_cooling_air"),
    ("stebbs", "profile_flow_hot_water"),
    ("stebbs", "profile_appliance"),
)


def _build_ten_minute_profile_alias_groups() -> tuple[
    tuple[str, str, tuple[str, ...]], ...
]:
    """Derive ``(section, canonical_field, (canonical, *aliases))`` from ``RAW_YAML_FIELD_RENAMES``."""
    groups: list[tuple[str, str, tuple[str, ...]]] = []
    for section_name, canonical_field in TEN_MINUTE_PROFILE_PATHS:
        aliases = tuple(
            sorted(
                old_name
                for old_name, target in RAW_YAML_FIELD_RENAMES.items()
                if target == canonical_field and old_name != canonical_field
            )
        )
        groups.append((section_name, canonical_field, (canonical_field, *aliases)))
    return tuple(groups)


TEN_MINUTE_PROFILE_ALIAS_GROUPS: tuple[
    tuple[str, str, tuple[str, ...]], ...
] = _build_ten_minute_profile_alias_groups()

_TEN_MINUTE_PROFILE_RAW_PATHS: tuple[tuple[str, str], ...] = tuple(
    (section_name, field_name)
    for section_name, _canonical, names in TEN_MINUTE_PROFILE_ALIAS_GROUPS
    for field_name in names
)
TEN_MINUTE_PROFILE_FIELD_NAMES: frozenset[str] = frozenset(
    field_name for _, field_name in _TEN_MINUTE_PROFILE_RAW_PATHS
)


def _normalise_profile_step_key(
    key: object, *, max_step: int, profile_label: str, unit: str
) -> str:
    """Normalise a 1-based integer or ASCII-digit profile key in ``1..max_step``."""
    if isinstance(key, bool):
        raise ValueError(
            f"Invalid {profile_label} key {key!r}: booleans are not allowed; "
            f"expected an integer 1..{max_step} or ASCII digit string '1'..'{max_step}'."
        )
    if isinstance(key, int):
        step = key
    elif isinstance(key, str):
        if not key or not all("0" <= ch <= "9" for ch in key):
            raise ValueError(
                f"Invalid {profile_label} key {key!r}: expected ASCII digits '1'..'{max_step}' "
                "(optional leading zeros allowed; no signs, whitespace, decimals, "
                "underscores, or non-ASCII digits)."
            )
        stripped = key.lstrip("0")
        if not stripped:
            raise ValueError(
                f"Invalid {profile_label} key {key!r}: {unit} must be between 1 and {max_step}."
            )
        step = int(stripped)
    else:
        raise ValueError(
            f"Invalid {profile_label} key {key!r} ({type(key).__name__} is not allowed): "
            f"expected an integer 1..{max_step} or ASCII digit string '1'..'{max_step}'."
        )

    if not 1 <= step <= max_step:
        raise ValueError(
            f"Invalid {profile_label} key {key!r}: {unit} {step} is outside valid range 1..{max_step}."
        )
    return str(step)


def _normalise_profile_step_mapping(
    mapping: dict[object, object], *, max_step: int, profile_label: str, unit: str
) -> dict[str, object]:
    """Normalise keys of a 1-based profile mapping and reject canonical collisions."""
    normalised: dict[str, object] = {}
    seen_raw: dict[str, object] = {}
    for raw_key, value in mapping.items():
        canonical = _normalise_profile_step_key(
            raw_key, max_step=max_step, profile_label=profile_label, unit=unit
        )
        if canonical in seen_raw:
            raise ValueError(
                f"Duplicate {profile_label} key {canonical!r} "
                f"(colliding keys: {seen_raw[canonical]!r} and {raw_key!r})."
            )
        seen_raw[canonical] = raw_key
        normalised[canonical] = value
    return {key: normalised[key] for key in sorted(normalised, key=int)}


def normalise_hourly_profile_key(key: object) -> str:
    """Normalise a 24-hour profile key to canonical ``"1"``..``"24"``.

    Accepts plain integers ``1``..``24`` (excluding ``bool``) and pure
    ASCII-digit strings representing ``1``..``24``, including leading-zero
    forms such as ``"01"``..``"09"``, ``"001"``, and ``"024"``.

    Parameters
    ----------
    key : object
        Raw mapping key from a 24-hour profile day-type dictionary.

    Returns
    -------
    str
        Canonical hour string in ``"1"``..``"24"``.

    Raises
    ------
    ValueError
        If *key* is a boolean, float, signed string, whitespace-padded string,
        underscore-separated string, non-ASCII digit string, non-numeric, or
        outside the inclusive range ``1``..``24``.
    """
    return _normalise_profile_step_key(
        key, max_step=24, profile_label="24-hour profile", unit="hour"
    )


def normalise_hourly_profile_mapping(
    mapping: dict[object, object],
) -> dict[str, object]:
    """Normalise keys of a 24-hour profile mapping to canonical ``"1"``..``"24"``.

    Rejects malformed or out-of-range keys and rejects canonical collisions
    (for example ``{"1": 0.1, "01": 0.2}``) instead of silently overwriting.

    Parameters
    ----------
    mapping : dict[object, object]
        Raw day-type mapping from a 24-hour profile.

    Returns
    -------
    dict[str, object]
        Mapping with canonical ``"1"``..``"24"`` string keys ordered numerically.

    Raises
    ------
    ValueError
        If any key is malformed, out of range, or collides with another key
        after normalisation.
    """
    return _normalise_profile_step_mapping(
        mapping, max_step=24, profile_label="24-hour profile", unit="hour"
    )


def normalise_ten_minute_profile_key(key: object) -> str:
    """Normalise a 10-minute profile key to canonical ``"1"``..``"144"``.

    Accepts plain integers ``1``..``144`` (excluding ``bool``) and pure
    ASCII-digit strings representing ``1``..``144``, including leading-zero
    forms such as ``"01"``..``"09"``, ``"001"``, and ``"0144"``.
    """
    return _normalise_profile_step_key(
        key, max_step=144, profile_label="10-minute profile", unit="step"
    )


def normalise_ten_minute_profile_mapping(
    mapping: dict[object, object],
) -> dict[str, object]:
    """Normalise keys of a 10-minute profile mapping to canonical ``"1"``..``"144"``.

    Rejects malformed or out-of-range keys and rejects canonical collisions
    (for example ``{"1": 0.1, "01": 0.2}``) instead of silently overwriting.
    """
    return _normalise_profile_step_mapping(
        mapping, max_step=144, profile_label="10-minute profile", unit="step"
    )


def _normalise_day_mapping_if_unambiguous(
    day_map: object,
    *,
    normalise_mapping: Callable[[dict[object, object]], dict[str, object]],
    normalise_key: Callable[[object], str],
) -> dict[str, object] | None:
    """Return a canonical copy of *day_map* when valid, non-empty, and non-canonical."""
    if not isinstance(day_map, dict) or not day_map:
        return None
    try:
        canonical_map = normalise_mapping(day_map)
    except ValueError:
        # Preserve malformed or colliding mappings for strict rejection.
        return None
    has_non_canonical = any(
        not isinstance(raw_k, str) or normalise_key(raw_k) != raw_k for raw_k in day_map
    )
    return deepcopy(canonical_map) if has_non_canonical else None


def _resolve_nested_dict(root: dict, rel_path: tuple[str, ...]) -> dict | None:
    """Return nested dictionary at *rel_path*, or ``None`` if absent or non-dict."""
    node: object = root
    for part in rel_path:
        if not isinstance(node, dict) or part not in node:
            return None
        node = node[part]
    return node if isinstance(node, dict) else None


def _normalise_profile_keys_in_raw_config(
    config_data: object,
    *,
    paths: tuple[tuple[str, ...], ...],
    normalise_mapping: Callable[[dict[object, object]], dict[str, object]],
    normalise_key: Callable[[object], str],
) -> tuple[object, bool]:
    """Normalise unambiguous non-canonical profile keys along *paths* in raw YAML."""
    if not isinstance(config_data, dict):
        return config_data, False
    sites = config_data.get("sites")
    if not isinstance(sites, list):
        return config_data, False

    normalised_root: dict | None = None
    changed = False

    for site_idx, site in enumerate(sites):
        props = site.get("properties") if isinstance(site, dict) else None
        if not isinstance(props, dict):
            continue
        for rel_path in paths:
            node = _resolve_nested_dict(props, rel_path)
            if node is None:
                continue
            for day_type in ("working_day", "holiday"):
                canonical_map = _normalise_day_mapping_if_unambiguous(
                    node.get(day_type),
                    normalise_mapping=normalise_mapping,
                    normalise_key=normalise_key,
                )
                if canonical_map is None:
                    continue
                if normalised_root is None:
                    normalised_root = deepcopy(config_data)
                target = _resolve_nested_dict(
                    normalised_root["sites"][site_idx]["properties"], rel_path
                )
                assert target is not None
                target[day_type] = canonical_map
                changed = True

    if normalised_root is None:
        return config_data, False
    return normalised_root, changed


def normalise_hourly_profile_keys_in_raw_config(
    config_data: object,
) -> tuple[object, bool]:
    """Normalise unambiguous zero-padded or integer 24-hour profile keys in raw YAML.

    Walks the seven 24-hour profile fields (both current and legacy spellings)
    under each site's ``properties`` block. Day-type mappings whose keys are
    all valid and collision-free are rewritten to canonical ``"1"``..``"24"``
    strings when at least one key was non-canonical. Any mapping containing a
    malformed, out-of-range, or colliding key is left untouched so downstream
    strict validation rejects it explicitly.

    Parameters
    ----------
    config_data : object
        Parsed raw YAML configuration object.

    Returns
    -------
    tuple[object, bool]
        ``(normalised_config, changed)`` where *changed* is ``True`` only when
        at least one non-canonical key was rewritten.
    """
    return _normalise_profile_keys_in_raw_config(
        config_data,
        paths=_HOURLY_PROFILE_PATHS,
        normalise_mapping=normalise_hourly_profile_mapping,
        normalise_key=normalise_hourly_profile_key,
    )


def normalise_ten_minute_profile_keys_in_raw_config(
    config_data: object,
) -> tuple[object, bool]:
    """Normalise unambiguous zero-padded or integer 10-minute profile keys in raw YAML.

    Walks the five 10-minute STEBBS profile fields (both current and legacy
    spellings) under each site's ``properties`` block. Day-type mappings whose
    keys are all valid and collision-free are rewritten to canonical
    ``"1"``..``"144"`` strings when at least one key was non-canonical. Any
    mapping containing a malformed, out-of-range, or colliding key is left
    untouched so downstream strict validation rejects it explicitly.

    Parameters
    ----------
    config_data : object
        Parsed raw YAML configuration object.

    Returns
    -------
    tuple[object, bool]
        ``(normalised_config, changed)`` where *changed* is ``True`` only when
        at least one non-canonical key was rewritten.
    """
    return _normalise_profile_keys_in_raw_config(
        config_data,
        paths=_TEN_MINUTE_PROFILE_RAW_PATHS,
        normalise_mapping=normalise_ten_minute_profile_mapping,
        normalise_key=normalise_ten_minute_profile_key,
    )


class DayProfile(DataModelBase):
    """Scalar values keyed by ``working_day`` and ``holiday``.

    The field defaults (``working_day=1.0``, ``holiday=0.0``) are asymmetric
    compatibility placeholders, not calibrated site parameters. Omitting a
    ``DayProfile`` field or omitting either key falls back to these values;
    under active modelled emissions (``model.physics.emissions != 0``),
    ``SUEWSConfig.from_yaml`` and ``SUEWSConfig.from_dict`` emit a
    ``UserWarning`` when active ``AnthropogenicHeat`` fields rely on them.

    See :doc:`/inputs/yaml/temporal-profiles` for day selection and field uses.
    """

    model_config = ConfigDict(title="Daily Profile")

    working_day: Optional[float] = Field(
        default=1.0,
        description=(
            "Scalar value applied on working days (Monday to Friday). "
            "The default 1.0 is a compatibility placeholder, not a calibrated "
            "site parameter."
        ),
        json_schema_extra={
            "display_name": "Working Day",
            "default_description": (
                "Compatibility placeholder default: 1.0 (not a calibrated site value); "
                "specify explicitly for active parameters."
            ),
        },
    )
    holiday: Optional[float] = Field(
        default=0.0,
        description=(
            "Scalar value applied on holidays/weekends (Saturday and Sunday). "
            "The default 0.0 is an asymmetric compatibility placeholder, not a "
            "calibrated site parameter."
        ),
        json_schema_extra={
            "display_name": "Holiday",
            "default_description": (
                "Compatibility placeholder default: 0.0 (asymmetric with working_day=1.0; "
                "not a calibrated site value); specify explicitly for active parameters."
            ),
        },
    )

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int, param_name: str) -> dict:
        day_map = {
            "working_day": 0,
            "holiday": 1,
        }
        cols = {("gridiv", "0"): grid_id}
        for day, idx in day_map.items():
            cols[(param_name, f"({idx},)")] = getattr(self, day)
        return cols

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """
        Convert day profile to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.
            param_name (str): Name of the parameter this profile belongs to.

        Returns:
            pd.DataFrame: DataFrame containing day profile parameters.
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id, param_name),
            index=pd.Index([grid_id], name="grid"),
        )

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "DayProfile":
        """
        Reconstruct DayProfile from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing day profile parameters.
            grid_id (int): Grid ID for the DataFrame index.
            param_name (str): Name of the parameter this profile belongs to.

        Returns:
            DayProfile: Instance of DayProfile.
        """

        day_map = {
            "working_day": 0,
            "holiday": 1,
        }

        # Extract values for working day and holiday from the DataFrame
        params = {}
        for day, idx in day_map.items():
            col = (param_name, f"({idx},)")
            if col in df.columns:
                params[day] = df.loc[grid_id, col]
            else:
                raise KeyError(f"Column {col} not found in DataFrame")

        return cls(**params)


_WEEKLY_DF_STATE_INDEX: dict[str, int] = {
    "sunday": 0,
    "monday": 1,
    "tuesday": 2,
    "wednesday": 3,
    "thursday": 4,
    "friday": 5,
    "saturday": 6,
}


class WeeklyProfile(DataModelBase):
    """Seven daily values keyed by lowercase weekday names, Monday to Sunday.

    Legacy ``df_state`` arrays and ``SUEWS_Irrigation.txt`` columns
    (``DayWat(1)``..``DayWat(7)``) store the seven days Sunday first
    (``0=sunday`` .. ``6=saturday``). See :doc:`/inputs/yaml/temporal-profiles`
    for storage order and field uses.
    """

    model_config = ConfigDict(title="Weekly Profile")

    monday: Optional[float] = 0.0
    tuesday: Optional[float] = 0.0
    wednesday: Optional[float] = 0.0
    thursday: Optional[float] = 0.0
    friday: Optional[float] = 0.0
    saturday: Optional[float] = 0.0
    sunday: Optional[float] = 0.0

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int, param_name: str) -> dict:
        cols = {("gridiv", "0"): grid_id}
        for day, idx in _WEEKLY_DF_STATE_INDEX.items():
            cols[param_name, f"({idx},)"] = getattr(self, day)
        return cols

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """Convert weekly profile to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter this profile belongs to

        Returns:
            pd.DataFrame: DataFrame containing weekly profile parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id, param_name),
            index=pd.Index([grid_id], name="grid"),
        )

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "WeeklyProfile":
        """Reconstruct WeeklyProfile from a DataFrame state format.

        Args:
            df: DataFrame containing weekly profile parameters
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter to extract values from

        Returns:
            WeeklyProfile: Instance of WeeklyProfile
        """
        params = {
            day: df.loc[grid_id, (param_name, f"({idx},)")]
            for day, idx in _WEEKLY_DF_STATE_INDEX.items()
        }

        return cls(**params)


class HourlyProfile(DataModelBase):
    """24-hour profile for diurnal variations.

    Holds two sets of 24 hourly values, one for working days
    (``working_day``) and one for holidays (``holiday``). Each set is a
    mapping keyed by the hour, ``"1"`` to ``"24"``, where ``"1"`` is the
    first hour of the day, starting at 00:00; all 24 keys must be present.
    The two sets are given together. A generic profile left out of the
    configuration takes a uniform 1/24 in both sets; ``popprof_24hr`` and
    ``profile_human_activity_24hr`` instead default to a uniform 1.0 (night
    level) on their ``[1.0, 2.0]`` transition scale.

    Used by the snow, irrigation, anthropogenic-heat, population, traffic
    and human-activity profiles: ``snow_profile_24hr``, ``wuprofa_24hr``,
    ``wuprofm_24hr``, ``ahprof_24hr``, ``popprof_24hr``,
    ``profile_traffic_24hr`` and ``profile_human_activity_24hr``.
    The 10-minute STEBBS profiles use ``TenMinuteProfile`` and its setpoint
    subclasses instead.

    See :doc:`/inputs/yaml/temporal-profiles` for timing and value treatment.
    """

    model_config = ConfigDict(title="Hourly Profile")

    working_day: Optional[Dict[str, Optional[float]]]
    holiday: Optional[Dict[str, Optional[float]]]

    ref: Optional[Reference] = None

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        """Generate default values for hourly profiles.

        Returns:
            Dict containing default working_day and holiday profiles with uniform distribution
        """
        # Create uniform distribution (1/24) for each hour
        uniform_value = 1.0 / 24.0

        # Generate hour keys 1-24 with uniform values
        hourly_values = {str(hour): uniform_value for hour in range(1, 25)}

        return {"working_day": hourly_values.copy(), "holiday": hourly_values.copy()}

    def __init__(self, **data):
        # If no values provided, use defaults
        if not data:
            defaults = self.__init_default_values__()
            data = defaults
        super().__init__(**data)

    @field_validator("working_day", "holiday", mode="before")
    def convert_keys_to_str(cls, v: Dict) -> Dict[str, Optional[float]]:
        if not isinstance(v, dict):
            return v

        normalised = normalise_hourly_profile_mapping(v)
        out: Dict[str, Optional[float]] = {}
        for k, val in normalised.items():
            if val is None:
                out[k] = None
            elif isinstance(val, (bool, np.bool_)):
                raise ValueError(
                    f"Hourly profile value for hour {k!r} must be a number, "
                    f"not bool ({val!r})."
                )
            else:
                out[k] = float(val)
        return out

    def _to_df_state_cols(self, grid_id: int, param_name: str) -> dict:
        cols = {("gridiv", "0"): grid_id}
        for hour, value in self.working_day.items():
            cols[(param_name, f"({int(hour) - 1}, 0)")] = value
        for hour, value in self.holiday.items():
            cols[(param_name, f"({int(hour) - 1}, 1)")] = value
        return cols

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """Convert hourly profile to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter this profile belongs to

        Returns:
            pd.DataFrame: DataFrame containing hourly profile parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id, param_name),
            index=pd.Index([grid_id], name="grid"),
        )

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "HourlyProfile":
        """Reconstruct HourlyProfile from a DataFrame state format.

        Args:
            df: DataFrame containing hourly profile parameters
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter to extract values from

        Returns:
            HourlyProfile: Instance of HourlyProfile
        """
        # Extract working day values (index 0)
        working_day = {
            str(hour + 1): df.loc[grid_id, (param_name, f"({hour}, 0)")]
            for hour in range(24)
        }

        # Extract holiday values (index 1)
        holiday = {
            str(hour + 1): df.loc[grid_id, (param_name, f"({hour}, 1)")]
            for hour in range(24)
        }

        # Create an instance of HourlyProfile
        return cls(working_day=working_day, holiday=holiday)


DayNightHourValue = Optional[
    Annotated[
        float,
        Field(ge=DAY_NIGHT_PROFILE_MIN, le=DAY_NIGHT_PROFILE_MAX),
    ]
]


class DayNightHourlyProfile(HourlyProfile):
    """24-hour day/night transition profile bounded to ``[1.0, 2.0]``.

    Used by ``popprof_24hr`` and ``profile_human_activity_24hr``, which the
    anthropogenic emissions kernel consumes raw without normalisation
    (``1.0`` = night, ``2.0`` = day, values in between = transition).

    See :doc:`/inputs/yaml/temporal-profiles` for timing and value treatment.
    """

    model_config = ConfigDict(title="Day/Night Hourly Profile")

    working_day: Optional[Dict[str, DayNightHourValue]]
    holiday: Optional[Dict[str, DayNightHourValue]]

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        hourly_values = {
            str(hour): DAY_NIGHT_PROFILE_DEFAULT for hour in range(1, 25)
        }
        return {"working_day": hourly_values.copy(), "holiday": hourly_values.copy()}

    @field_validator("working_day", "holiday", mode="before")
    @classmethod
    def _validate_day_night_bounds_before(cls, v: object) -> object:
        if not isinstance(v, dict):
            return v
        coerced = HourlyProfile.convert_keys_to_str(v)
        bad_entries: list[str] = []
        for hour, val in coerced.items():
            if val is None:
                continue
            if not math.isfinite(val) or not (
                DAY_NIGHT_PROFILE_MIN <= val <= DAY_NIGHT_PROFILE_MAX
            ):
                bad_entries.append(f"{hour}={val!r}")
        if bad_entries:
            preview = ", ".join(bad_entries[:6])
            if len(bad_entries) > 6:
                preview += f", ... ({len(bad_entries)} total)"
            raise ValueError(
                f"Day/night hourly profile values must be finite numbers in "
                f"[{DAY_NIGHT_PROFILE_MIN}, {DAY_NIGHT_PROFILE_MAX}] "
                "(1.0 = night, 2.0 = day, values in between = transition; "
                f"read raw without normalisation). Out-of-range entries: {preview}."
            )
        return coerced

    @classmethod
    def from_df_state(
        cls,
        df: pd.DataFrame,
        grid_id: int,
        param_name: str,
        *,
        allow_inactive_sentinels: bool = False,
    ) -> "DayNightHourlyProfile":
        """Reconstruct a day/night profile from ``df_state``.

        When *allow_inactive_sentinels* is ``True`` (anthropogenic emissions
        disabled via ``emissionsmethod == 0``), exact ``NaN`` and legacy
        ``-999`` sentinels are normalised to ``None`` (conditional absence)
        while preserving rejection of ``+/-inf`` and finite out-of-range values.
        """
        from ...util._missing import SUEWS_MISSING  # noqa: PLC0415

        def _normalise_cell(raw_val: object) -> object:
            if not allow_inactive_sentinels or raw_val is None:
                return raw_val
            if isinstance(raw_val, (int, float, np.number)) and not isinstance(
                raw_val, bool
            ):
                fval = float(raw_val)
                if math.isnan(fval) or fval == SUEWS_MISSING:
                    return None
            return raw_val

        working_day = {
            str(hour + 1): _normalise_cell(
                df.loc[grid_id, (param_name, f"({hour}, 0)")]
            )
            for hour in range(24)
        }
        holiday = {
            str(hour + 1): _normalise_cell(
                df.loc[grid_id, (param_name, f"({hour}, 1)")]
            )
            for hour in range(24)
        }
        return cls(working_day=working_day, holiday=holiday)


def default_day_night_hourly_profile() -> DayNightHourlyProfile:
    """Return a uniform 1.0 (night-level) 24-hour profile on the ``[1.0, 2.0]`` scale."""
    hourly_values = {str(hour): DAY_NIGHT_PROFILE_DEFAULT for hour in range(1, 25)}
    return DayNightHourlyProfile(
        working_day=hourly_values.copy(),
        holiday=hourly_values.copy(),
    )


def validate_day_night_hourly_profile(
    profile: Optional[HourlyProfile], field_name: str
) -> Optional[HourlyProfile]:
    """Validate that non-null hourly values lie on the kernel's ``[1.0, 2.0]`` scale.

    The anthropogenic emissions kernel reads ``popprof_24hr`` and
    ``profile_human_activity_24hr`` raw without normalisation, weighting night
    terms by ``(2 - x)`` and day terms by ``(x - 1)``. Values below ``1.0``
    produce negative daytime weights and values above ``2.0`` produce negative
    night-time weights.
    """
    if profile is None:
        return None
    bad_entries: list[str] = []
    for day_type in ("working_day", "holiday"):
        day_map = getattr(profile, day_type, None)
        if not isinstance(day_map, dict):
            continue
        for hour, val in day_map.items():
            if val is None:
                continue
            if not is_finite_real_number(val) or not (
                DAY_NIGHT_PROFILE_MIN <= float(val) <= DAY_NIGHT_PROFILE_MAX
            ):
                bad_entries.append(f"{day_type}.{hour}={val!r}")
    if bad_entries:
        preview = ", ".join(bad_entries[:6])
        if len(bad_entries) > 6:
            preview += f", ... ({len(bad_entries)} total)"
        raise ValueError(
            f"{field_name} values must be finite numbers in "
            f"[{DAY_NIGHT_PROFILE_MIN}, {DAY_NIGHT_PROFILE_MAX}] "
            "(1.0 = night, 2.0 = day, values in between = transition; "
            f"read raw without normalisation). Out-of-range entries: {preview}."
        )
    return profile


class TenMinuteProfile(DataModelBase):
    """Two 10-minute mappings, ``working_day`` and ``holiday``, keyed ``"1"`` to ``"144"``.

    See :doc:`/inputs/yaml/temporal-profiles` for slot timing and field uses.
    """

    model_config = ConfigDict(title="Ten Minute Profile")

    working_day: Optional[Dict[str, Optional[float]]]
    holiday: Optional[Dict[str, Optional[float]]]

    ref: Optional[Reference] = None

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        """Generate default values for 10-minute profiles.

        Returns:
            Dict containing default working_day and holiday profiles with uniform distribution
        """
        # Create uniform distribution (1/144) for each 10-minute bin
        uniform_value = 1.0 / 144.0

        # Generate bin keys 1-144 with uniform values
        ten_min_values = {str(i): uniform_value for i in range(1, 145)}

        return {"working_day": ten_min_values.copy(), "holiday": ten_min_values.copy()}

    def __init__(self, **data):
        # If no values provided, use defaults
        if not data:
            data = self.__init_default_values__()
        super().__init__(**data)

    @model_validator(mode="wrap")
    @classmethod
    def _contextualise_ten_minute_profile(
        cls, data: Any, handler: Any
    ) -> "TenMinuteProfile":
        """Relax non-numeric placeholder slot values when STEBBS is inactive inside SUEWSConfig.

        Key validation (``1``..``144``), ``extra="forbid"``, boolean rejection,
        and non-finite (``NaN``/``Inf``) rejection stay active even when STEBBS
        is disabled.
        """
        if isinstance(data, TenMinuteProfile):
            return handler(data)
        if _STEBBS_ACTIVE_CONTEXT.get() is not False or not isinstance(data, Mapping):
            return handler(data)
        unknown_keys = set(data.keys()) - set(cls.model_fields.keys())
        if unknown_keys:
            return handler(data)

        fields_set: set[str] = set()
        normalised_days: dict[str, Any] = {}
        for day_key in ("working_day", "holiday"):
            if day_key not in data:
                return handler(data)
            fields_set.add(day_key)
            raw_day = data[day_key]
            if raw_day is None:
                normalised_days[day_key] = None
                continue
            if not isinstance(raw_day, dict):
                return handler(data)
            try:
                normalised = normalise_ten_minute_profile_mapping(raw_day)
            except ValueError:
                return handler(data)
            out_day: dict[str, Any] = {}
            for step_key, raw_val in normalised.items():
                if raw_val is None:
                    out_day[step_key] = None
                    continue
                if isinstance(raw_val, bool):
                    return handler(data)
                ok, coerced = _coerce_inactive_stebbs_scalar(
                    raw_val,
                    allow_null=True,
                    allow_string=True,
                    int_preferred=False,
                )
                if not ok:
                    raise ValueError(
                        f"Ten-minute profile value for step {step_key!r} must be a finite "
                        f"number or placeholder string, got {raw_val!r}."
                    )
                out_day[step_key] = coerced
            normalised_days[day_key] = out_day

        stripped: dict[str, Any] = {"working_day": None, "holiday": None}
        if "ref" in data:
            stripped["ref"] = data["ref"]
            fields_set.add("ref")

        inst = handler(stripped)
        object.__setattr__(inst, "working_day", normalised_days["working_day"])
        object.__setattr__(inst, "holiday", normalised_days["holiday"])
        inst.model_fields_set.update(fields_set)
        return inst

    @field_validator("working_day", "holiday", mode="before")
    def convert_keys_to_str(cls, v: Dict) -> Dict[str, Optional[float]]:
        if not isinstance(v, dict):
            return v

        normalised = normalise_ten_minute_profile_mapping(v)
        out: Dict[str, Optional[float]] = {}
        for k, val in normalised.items():
            if val is None:
                out[k] = None
            elif isinstance(val, bool):
                raise ValueError(
                    f"Ten-minute profile value for step {k!r} must be a number, "
                    f"not bool ({val!r})."
                )
            else:
                out[k] = float(val)
        return out

    @field_serializer("working_day", "holiday", mode="wrap")
    def _serialise_ten_minute_profile_day(
        self, value: Any, handler: Any, info: SerializationInfo
    ) -> Any:
        """Emit inactive string slot placeholders without Pydantic type warnings."""
        if isinstance(value, dict) and any(isinstance(v, str) for v in value.values()):
            return dict(value)
        return handler(value)

    def _to_df_state_cols(self, grid_id: int, param_name: str) -> dict:
        cols = {("gridiv", "0"): grid_id}
        for day_idx, day_type in enumerate(("working_day", "holiday")):
            day_map = getattr(self, day_type)
            if not isinstance(day_map, dict):
                raise ValueError(
                    f"TenMinuteProfile.{day_type} must be a mapping keyed by "
                    f"steps '1'..'144', got {type(day_map).__name__}."
                )
            normalised = normalise_ten_minute_profile_mapping(day_map)
            for bin_id, value in normalised.items():
                cols[param_name, f"({int(bin_id) - 1}, {day_idx})"] = value
        return cols

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """Convert 10-minute profile to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter this profile belongs to

        Returns:
            pd.DataFrame: DataFrame containing 10-minute profile parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id, param_name),
            index=pd.Index([grid_id], name="grid"),
        )

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "TenMinuteProfile":
        """Reconstruct TenMinuteProfile from a DataFrame state format.

        Args:
            df: DataFrame containing 10-minute profile parameters
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter to extract values from

        Returns:
            TenMinuteProfile: Instance of TenMinuteProfile
        """
        allow_inactive_nulls = _STEBBS_ACTIVE_CONTEXT.get() is False

        def _normalise_cell(raw_val: Any) -> Any:
            if allow_inactive_nulls and raw_val is not None:
                if isinstance(raw_val, (float, np.floating)) and math.isnan(
                    float(raw_val)
                ):
                    return None
                if raw_val is pd.NA:
                    return None
            return raw_val

        # Extract working day values (index 0)
        working_day = {
            str(i + 1): _normalise_cell(df.loc[grid_id, (param_name, f"({i}, 0)")])
            for i in range(144)
        }

        # Extract holiday values (index 1)
        holiday = {
            str(i + 1): _normalise_cell(df.loc[grid_id, (param_name, f"({i}, 1)")])
            for i in range(144)
        }

        return cls.model_validate({"working_day": working_day, "holiday": holiday})


class HeatingSetpointProfile(TenMinuteProfile):
    """Heating setpoints in two day-type mappings with 10-minute keys ``"1"`` to ``"144"``.

    See :doc:`/inputs/yaml/temporal-profiles` for timing and the default values.
    """

    model_config = ConfigDict(title="Heating Setpoint Profile")

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        off_value = 0.0
        ten_min_values = {str(i): off_value for i in range(1, 145)}
        return {"working_day": ten_min_values.copy(), "holiday": ten_min_values.copy()}


class CoolingSetpointProfile(TenMinuteProfile):
    """Cooling setpoints in two day-type mappings with 10-minute keys ``"1"`` to ``"144"``.

    See :doc:`/inputs/yaml/temporal-profiles` for timing and the default values.
    """

    model_config = ConfigDict(title="Cooling Setpoint Profile")

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        off_value = 100.0
        ten_min_values = {str(i): off_value for i in range(1, 145)}
        return {"working_day": ten_min_values.copy(), "holiday": ten_min_values.copy()}
