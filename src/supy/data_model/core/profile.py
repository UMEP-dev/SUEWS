from copy import deepcopy
from pydantic import ConfigDict, Field, field_validator, model_validator
from typing import Optional, Dict
import pandas as pd
from .type import DataModelBase, Reference, df_from_cols

CANONICAL_HOUR_KEYS: tuple[str, ...] = tuple(str(hour) for hour in range(1, 25))
_CANONICAL_HOUR_KEY_SET: frozenset[str] = frozenset(CANONICAL_HOUR_KEYS)

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
    if isinstance(key, bool):
        raise ValueError(
            f"Invalid 24-hour profile key {key!r}: booleans are not allowed; "
            "expected an integer 1..24 or ASCII digit string '1'..'24'."
        )
    if isinstance(key, int):
        hour = key
    elif isinstance(key, str):
        if not key or not all("0" <= ch <= "9" for ch in key):
            raise ValueError(
                f"Invalid 24-hour profile key {key!r}: expected ASCII digits '1'..'24' "
                "(optional leading zeros allowed; no signs, whitespace, decimals, "
                "underscores, or non-ASCII digits)."
            )
        stripped = key.lstrip("0")
        if not stripped:
            raise ValueError(
                f"Invalid 24-hour profile key {key!r}: hour must be between 1 and 24."
            )
        hour = int(stripped)
    else:
        raise ValueError(
            f"Invalid 24-hour profile key {key!r} ({type(key).__name__} is not allowed): "
            "expected an integer 1..24 or ASCII digit string '1'..'24'."
        )

    if not 1 <= hour <= 24:
        raise ValueError(
            f"Invalid 24-hour profile key {key!r}: hour {hour} is outside valid range 1..24."
        )
    return str(hour)


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
    normalised: dict[str, object] = {}
    seen_raw: dict[str, object] = {}
    for raw_key, value in mapping.items():
        canonical = normalise_hourly_profile_key(raw_key)
        if canonical in seen_raw:
            raise ValueError(
                f"Duplicate 24-hour profile key {canonical!r} "
                f"(colliding keys: {seen_raw[canonical]!r} and {raw_key!r})."
            )
        seen_raw[canonical] = raw_key
        normalised[canonical] = value
    return {key: normalised[key] for key in sorted(normalised, key=int)}


def _normalise_day_mapping_if_unambiguous(
    day_map: object,
) -> dict[str, object] | None:
    """Return a canonical copy of *day_map* when valid, non-empty, and non-canonical."""
    if not isinstance(day_map, dict) or not day_map:
        return None
    try:
        canonical_map = normalise_hourly_profile_mapping(day_map)
    except ValueError:
        # Preserve malformed or colliding mappings for strict rejection.
        return None
    has_non_canonical = any(
        not isinstance(raw_k, str) or normalise_hourly_profile_key(raw_k) != raw_k
        for raw_k in day_map
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
        for rel_path in _HOURLY_PROFILE_PATHS:
            node = _resolve_nested_dict(props, rel_path)
            if node is None:
                continue
            for day_type in ("working_day", "holiday"):
                canonical_map = _normalise_day_mapping_if_unambiguous(
                    node.get(day_type)
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


class DayProfile(DataModelBase):
    """Scalar values keyed by ``working_day`` and ``holiday``.

    See :doc:`/inputs/yaml/temporal-profiles` for day selection and field uses.
    """

    model_config = ConfigDict(title="Daily Profile")

    working_day: Optional[float] = Field(
        default=1.0, json_schema_extra={"display_name": "Working Day"}
    )
    holiday: Optional[float] = Field(
        default=0.0, json_schema_extra={"display_name": "Holiday"}
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
    The two sets are given together. A profile left out of the
    configuration takes a uniform 1/24 in both sets.

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

    @field_validator("working_day", "holiday", mode="before")
    def convert_keys_to_str(cls, v: Dict) -> Dict[str, Optional[float]]:
        if not isinstance(v, dict):
            return v

        out: Dict[str, Optional[float]] = {}
        for k, val in v.items():
            if val is None:
                out[str(k)] = None
            else:
                out[str(k)] = float(val)
        return out

    def _to_df_state_cols(self, grid_id: int, param_name: str) -> dict:
        cols = {("gridiv", "0"): grid_id}
        for bin_id, value in self.working_day.items():
            cols[(param_name, f"({int(bin_id) - 1}, 0)")] = value
        for bin_id, value in self.holiday.items():
            cols[(param_name, f"({int(bin_id) - 1}, 1)")] = value
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
        # Extract working day values (index 0)
        working_day = {
            str(i + 1): df.loc[grid_id, (param_name, f"({i}, 0)")]
            for i in range(144)
        }

        # Extract holiday values (index 1)
        holiday = {
            str(i + 1): df.loc[grid_id, (param_name, f"({i}, 1)")]
            for i in range(144)
        }

        return cls(working_day=working_day, holiday=holiday)


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
