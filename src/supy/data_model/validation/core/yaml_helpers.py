from dataclasses import dataclass
from typing import (
    Any,
    List,
    Optional,
    Tuple,
)
from pydantic import BaseModel
from collections.abc import Mapping
import pandas as pd
import yaml
from datetime import datetime, timedelta
import pytz
from ...core.field_renames import (
    RAW_YAML_FIELD_RENAMES,
    STEBBS_PHYSICS_LEAF_RENAMES,
    normalise_yaml_renames,
    read_renamed_key,
)
from ...core.physics_families import coerce_nested_to_flat
from ...core.physics_orthogonal import coerce_orthogonal_to_flat
from ...core.profile import (
    normalise_hourly_profile_keys_in_raw_config,
    normalise_ten_minute_profile_keys_in_raw_config,
)

# Use tzfpy instead of timezonefinder for Windows compatibility
# tzfpy has pre-built Windows wheels and provides similar functionality
try:
    from tzfpy import get_tz

    HAS_TIMEZONE_FINDER = True

    # Create a compatibility wrapper for timezonefinder API
    class TimezoneFinder:
        def timezone_at(self, lat, lng):
            """Wrapper to match timezonefinder API."""
            # tzfpy uses (longitude, latitude) order
            return get_tz(lng, lat)

except ImportError:
    # Fallback to original timezonefinder if tzfpy not available
    try:
        from timezonefinder import TimezoneFinder

        HAS_TIMEZONE_FINDER = True
    except ImportError:
        HAS_TIMEZONE_FINDER = False
        import warnings

        warnings.warn(
            "Neither tzfpy nor timezonefinder available. DST calculations will be skipped.",
            UserWarning,
        )


def _fallback_timezone_name(lat: float, lng: float) -> Optional[str]:
    """Small deterministic timezone fallback for simple UTC and London cases."""
    if 49.0 <= lat <= 61.0 and -8.5 <= lng <= 2.5:
        return "Europe/London"
    if -1.0 <= lat <= 1.0 and -1.0 <= lng <= 1.0:
        return "Etc/GMT"
    return None


# Optional import - use standalone if supy not available
try:
    from ...._env import logger_supy, trv_supy_module
except ImportError:
    import logging
    from pathlib import Path

    logger_supy = logging.getLogger("supy.data_model")
    if not logger_supy.handlers:
        handler = logging.StreamHandler()
        handler.setFormatter(
            logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
        )
        logger_supy.addHandler(handler)
        logger_supy.setLevel(logging.INFO)

    # Mock traversable for standalone mode
    class MockTraversable:
        def __init__(self):
            # Go up to the supy module root from yaml_helpers.py location
            # yaml_helpers.py is in src/supy/data_model/validation/core/
            # So we need to go up 4 levels to get to src/supy/
            self.base = Path(__file__).parent.parent.parent.parent

        def __truediv__(self, other):
            return self.base / other

        def exists(self):
            return False

    trv_supy_module = MockTraversable()


def load_user_yaml_normalised(path: str) -> Any:
    """Load a user YAML file and normalise legacy field spellings to current names.

    The single load chokepoint for the standalone validation pipeline (gh#1457).
    Routing every raw user-YAML read through this helper means a new load site
    cannot silently forget to normalise: ``SUEWSConfig.from_yaml`` normalises
    before validation, but the standalone Phase B/C path does not, so rules that
    look up the current name would otherwise miss a legacy spelling.

    Args:
        path: Path to the user YAML file.

    Returns:
        The parsed YAML with legacy field spellings rewritten to current names
        (or untouched if both a legacy and current spelling are present; see
        ``normalise_yaml_renames``).
    """
    with open(path, "r", encoding="utf-8") as f:
        data = normalise_yaml_renames(yaml.safe_load(f))
    data, _ = normalise_hourly_profile_keys_in_raw_config(data)
    data, _ = normalise_ten_minute_profile_keys_in_raw_config(data)
    return data


def get_value_safe(param_dict, param_key, default=None):
    """Safely extract value from RefValue or plain format.

    Args:
        param_dict: Dictionary containing the parameter
        param_key: Key to look up
        default: Default value if key not found

    Returns:
        The parameter value, handling both RefValue {"value": X} and plain X formats
    """
    param = read_renamed_key(
        param_dict,
        param_key,
        renames=RAW_YAML_FIELD_RENAMES,
        default=default,
    )
    param = coerce_orthogonal_to_flat(param_key, param)
    param = coerce_nested_to_flat(param_key, param)
    if isinstance(param, Mapping) and "value" in param:
        return param["value"]  # RefValue format: {"value": 1}
    else:
        return param  # Plain format: 1


def get_ref_or_bare(container, key: str, default=None):
    """Read ``container[key]`` whether stored as ``{"value": x, ...}`` or bare ``x``.

    Returns ``default`` when ``container`` is not a mapping, ``key`` is absent,
    or the entry is a mapping without a ``"value"`` key (such as ``{}`` or a
    ref-only dict).
    """
    if not isinstance(container, Mapping) or key not in container:
        return default
    entry = container[key]
    if isinstance(entry, Mapping):
        return entry.get("value", default)
    return entry


def is_ref_only(entry) -> bool:
    """Return True for a ref-only RefValue placeholder such as ``{"ref": {...}}``.

    Such an entry carries no value (the model and the Rust bridge read it as
    null), so nullifiers must leave it alone rather than descend into ``ref``.
    """
    return (
        isinstance(entry, Mapping)
        and "value" not in entry
        and bool(entry)
        and all(key == "ref" for key in entry)
    )


def set_ref_or_bare(container: dict, key: str, new_value) -> None:
    """Update ``container[key]`` while preserving RefValue metadata or bare format.

    - Existing ``dict`` entries (e.g. ``{"value": old, "ref": ...}``) have their
      ``"value"`` field updated in place so ``"ref"`` metadata is preserved.
    - Existing non-mapping entries (bare scalars, lists, or ``None``) remain bare.
    - Absent keys are inserted as ``{"value": new_value}``.
    """
    if key in container:
        existing = container[key]
        if isinstance(existing, dict):
            existing["value"] = new_value
        elif isinstance(existing, Mapping):
            container[key] = {**existing, "value": new_value}
        else:
            container[key] = new_value
    else:
        container[key] = {"value": new_value}


_STEBBS_MISSING = object()
_STEBBS_REFVALUE_KEYS = frozenset({"value", "ref"})
_STEBBS_NESTED_KEYS = frozenset(
    {
        "enabled",
        "parameters",
        *STEBBS_PHYSICS_LEAF_RENAMES.keys(),
        *STEBBS_PHYSICS_LEAF_RENAMES.values(),
    }
)
_STEBBS_BOOL_TRUE = frozenset({"1", "true", "t", "yes", "y", "on"})
_STEBBS_BOOL_FALSE = frozenset({"0", "false", "f", "no", "n", "off"})


def _stebbs_master_aliases(physics):
    """Return legacy master-toggle aliases present beside ``stebbs``."""
    if not isinstance(physics, Mapping):
        return []
    return sorted(
        old_key
        for old_key, new_key in RAW_YAML_FIELD_RENAMES.items()
        if old_key != "stebbs" and new_key == "stebbs" and old_key in physics
    )


def _stebbs_leaf_alias_conflicts(physics):
    """Return flat STEBBS alias groups that would collide after folding."""
    if not isinstance(physics, Mapping):
        return []
    present_by_leaf = {}
    for old_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if old_key in physics:
            present_by_leaf.setdefault(nested_leaf, []).append(old_key)
    return [
        (leaf, sorted(keys))
        for leaf, keys in sorted(present_by_leaf.items())
        if len(keys) > 1
    ]


def _format_stebbs_leaf_alias_conflicts(conflicts):
    """Format colliding flat STEBBS aliases for validation errors."""
    return "; ".join(
        f"{', '.join(keys)} -> stebbs.{leaf}" for leaf, keys in conflicts
    )


def _stebbs_flat_leaf_siblings(physics):
    """Return flat STEBBS leaf keys present beside a nested ``stebbs`` block."""
    if not isinstance(physics, Mapping):
        return []
    return sorted({key for key in STEBBS_PHYSICS_LEAF_RENAMES if key in physics})


def _read_raw_stebbs_entry(physics):
    """Read ``stebbs`` while rejecting ambiguous master-toggle aliases."""
    aliases = _stebbs_master_aliases(physics)
    if len(aliases) > 1:
        raise ValueError(
            "Multiple legacy STEBBS master aliases "
            f"({', '.join(aliases)}) are present. Use only one spelling."
        )
    if isinstance(physics, Mapping) and "stebbs" in physics and aliases:
        raise ValueError(
            "Both 'stebbs' and legacy STEBBS master aliases "
            f"({', '.join(aliases)}) are present. Use only 'stebbs'."
        )
    return read_renamed_key(
        physics,
        "stebbs",
        renames=RAW_YAML_FIELD_RENAMES,
        default=_STEBBS_MISSING,
    )


def _is_stebbs_refvalue_scalar(entry):
    """Return True for a RefValue-style legacy ``stebbs`` master toggle."""
    return (
        isinstance(entry, Mapping)
        and "value" in entry
        and set(entry) <= _STEBBS_REFVALUE_KEYS
    )


def _is_stebbs_nested_block(entry):
    """Return True when ``entry`` is the nested STEBBS physics object."""
    return (
        isinstance(entry, Mapping)
        and not _is_stebbs_refvalue_scalar(entry)
        and any(key in entry for key in _STEBBS_NESTED_KEYS)
    )


def _legacy_stebbs_master_value(entry):
    """Compose the legacy tri-state ``stebbsmethod`` from a raw scalar."""
    entry = coerce_nested_to_flat("stebbs", entry)
    if isinstance(entry, Mapping) and "value" not in entry:
        raise ValueError(
            "Legacy 'stebbs' mappings must use a 'value' key; use "
            "'stebbs.enabled' / 'stebbs.parameters' for the nested form."
        )
    raw = entry["value"] if isinstance(entry, Mapping) and "value" in entry else entry
    if isinstance(raw, bool):
        code = int(raw)
    elif isinstance(raw, int):
        code = raw
    elif isinstance(raw, float) and raw.is_integer():
        code = int(raw)
    else:
        raise ValueError(
            "Legacy 'stebbs' master toggle expects integer 0, 1, or 2."
        )
    if code == 0:
        return 0
    if code == 1:
        return 1
    if code == 2:
        return 2
    raise ValueError("Legacy 'stebbs' master toggle expects integer 0, 1, or 2.")


def _parse_stebbs_enabled_value(raw):
    """Parse ``stebbs.enabled`` with Pydantic-compatible bool coercion."""
    if isinstance(raw, bool):
        return raw
    if isinstance(raw, int):
        if raw in (0, 1):
            return bool(raw)
    elif isinstance(raw, float) and raw in (0.0, 1.0):
        return bool(raw)
    elif isinstance(raw, str):
        lowered = raw.lower()
        if lowered in _STEBBS_BOOL_TRUE:
            return True
        if lowered in _STEBBS_BOOL_FALSE:
            return False
    raise ValueError("model.physics.stebbs.enabled expects a boolean value.")


def _parse_stebbs_parameters_value(raw):
    """Parse ``stebbs.parameters`` with Pydantic-compatible enum coercion."""
    if isinstance(raw, bool):
        if raw:
            return 1
    elif isinstance(raw, int):
        if raw in (1, 2):
            return raw
    elif isinstance(raw, float) and raw in (1.0, 2.0):
        return int(raw)
    raise ValueError("model.physics.stebbs.parameters expects integer 1 or 2.")


def get_stebbs_block(physics):
    """Return a folded ``model.physics.stebbs`` view (or {} if absent).

    gh#1456: the STEBBS switches moved under a nested ``stebbs`` object. This
    helper isolates that container so callers can read its leaves
    (``capacitance``, ``setpoint``, ``same_*``) uniformly against the
    YAML-dict representation. For raw Phase B callers that have not gone through
    Pydantic, it also folds still-accepted flat sibling switches into a transient
    nested view.

    Parameters
    ----------
    physics : Mapping
        The ``model.physics`` dict (or anything mapping-like).

    Returns
    -------
    Mapping
        The nested ``stebbs`` sub-dict, including any accepted flat sibling
        switches, or ``{}`` when it is absent.
    """
    if not isinstance(physics, Mapping):
        return {}
    block = _read_raw_stebbs_entry(physics)
    folded = dict(block) if _is_stebbs_nested_block(block) else {}

    if folded:
        nested_conflicts = _stebbs_leaf_alias_conflicts(folded)
        if nested_conflicts:
            raise ValueError(
                "Multiple nested STEBBS physics switches map to the same nested "
                f"leaf ({_format_stebbs_leaf_alias_conflicts(nested_conflicts)}). "
                "Use only one spelling."
            )

        flat_conflicts = _stebbs_flat_leaf_siblings(physics)
        if flat_conflicts:
            raise ValueError(
                "Both nested 'stebbs' and flat STEBBS physics switches "
                f"({', '.join(flat_conflicts)}) are present. Use only the "
                "nested 'stebbs' form."
            )

    leaf_conflicts = _stebbs_leaf_alias_conflicts(physics)
    if leaf_conflicts:
        raise ValueError(
            "Multiple flat STEBBS physics switches map to the same nested leaf "
            f"({_format_stebbs_leaf_alias_conflicts(leaf_conflicts)}). Use only "
            "one spelling."
        )

    for old_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if old_key == nested_leaf or old_key not in folded:
            continue
        value = folded.pop(old_key)
        if nested_leaf not in folded:
            folded[nested_leaf] = value

    for _flat_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if nested_leaf in folded:
            continue
        value = read_renamed_key(
            physics,
            nested_leaf,
            renames=RAW_YAML_FIELD_RENAMES,
            default=_STEBBS_MISSING,
        )
        if value is not _STEBBS_MISSING:
            folded[nested_leaf] = value

    return folded


def get_stebbsmethod_value(physics):
    """Return the composed legacy ``stebbsmethod`` integer from physics.

    Accepts both the new nested shape (``model.physics.stebbs.enabled`` +
    ``.parameters``, composing to ``0`` / ``int(parameters)``) and the legacy
    flat ``stebbs`` tri-state integer. Returns ``None`` when neither is
    present.

    Parameters
    ----------
    physics : Mapping
        The ``model.physics`` dict.

    Returns
    -------
    int or None
        The composed ``stebbsmethod`` code (0/1/2), or ``None`` when the
        master toggle is absent.
    """
    if not isinstance(physics, Mapping):
        return None

    raw_stebbs = _read_raw_stebbs_entry(physics)
    stebbs_block = get_stebbs_block(physics)

    if raw_stebbs is not _STEBBS_MISSING and not _is_stebbs_nested_block(raw_stebbs):
        return _legacy_stebbs_master_value(raw_stebbs)

    if stebbs_block:
        # Validate a present parameters value even when disabled, matching the
        # Pydantic field contract and the Rust bridge flattening pre-pass.
        raw_parameters = get_value_safe(
            stebbs_block,
            "parameters",
            _STEBBS_MISSING,
        )
        parameters = (
            1
            if raw_parameters is _STEBBS_MISSING
            else _parse_stebbs_parameters_value(raw_parameters)
        )
        enabled = _parse_stebbs_enabled_value(
            get_value_safe(stebbs_block, "enabled", False)
        )
        if not enabled:
            return 0
        return parameters

    return None


def unwrap_value(val):
    """
    Unwrap RefValue and Enum values consistently.

    This helper ensures consistent handling of RefValue wrappers and Enum values
    throughout the validation logic.

    Args:
        val: The value to unwrap (could be RefValue, Enum, or raw value)

    Returns:
        The unwrapped raw value
    """
    # Handle RefValue wrapper
    if (
        hasattr(val, "value")
        and hasattr(val, "__class__")
        and "RefValue" in val.__class__.__name__
    ):
        val = val.value

    # Handle Enum values (which also have .value attribute)
    if (
        hasattr(val, "value")
        and hasattr(val, "__class__")
        and "Enum" in str(val.__class__.__bases__)
    ):
        val = val.value

    return val


def unwrap_nested_value(x: Any) -> Any:
    """Unwrap RefValue-like objects or {"value": ...} dicts recursively.

    Handles dicts with a "value" key nested within other dicts or objects
    that have a "value" attribute, recursively unwrapping up to 10 levels deep.

    Args:
        x (Any): The value to unwrap. Can be a dict with a "value" key,
            an object with a "value" attribute, or any other type.

    Returns:
        Any: The unwrapped value, or None if input is None.
    """
    cur = x
    for _ in range(10):
        if cur is None:
            return None
        # dict YAML form
        if isinstance(cur, Mapping) and "value" in cur:
            cur = cur["value"]
            continue
        # RefValue-like form
        if hasattr(cur, "value"):
            cur = cur.value
            continue
        break
    return cur


@dataclass(frozen=True)
class DSTSchedule:
    """Resolved yearly daylight saving transition schedule for one site."""

    timezone: str | None = None
    utc_offset_hours: int | float | None = None
    starts: tuple[int, ...] = ()
    ends: tuple[int, ...] = ()
    scan_complete: bool = True
    offset_step_hours: float | None = None
    distinct_offset_count: int = 0

    @property
    def has_multiple_periods(self) -> bool:
        """Return True when the year has more than one DST start or end transition."""
        return self.scan_complete and (
            len(self.starts) > 1 or len(self.ends) > 1 or self.distinct_offset_count > 2
        )

    @property
    def has_non_hourly_dst(self) -> bool:
        """Return True when the yearly offset step is non-zero and not +1 hour."""
        return (
            self.scan_complete
            and self.offset_step_hours is not None
            and abs(self.offset_step_hours - 1.0) > 1e-9
        )

    @property
    def has_no_dst(self) -> bool:
        """Return True when the timezone resolved cleanly with zero DST transitions."""
        return (
            self.scan_complete
            and self.timezone is not None
            and not self.starts
            and not self.ends
        )

    @property
    def start(self) -> int | None:
        """Single forward-transition DOY when representable, else ``None``."""
        if not self.scan_complete or self.has_multiple_periods or self.has_non_hourly_dst:
            return None
        return self.starts[0] if len(self.starts) == 1 else None

    @property
    def end(self) -> int | None:
        """Single backward-transition DOY when representable, else ``None``."""
        if not self.scan_complete or self.has_multiple_periods or self.has_non_hourly_dst:
            return None
        return self.ends[0] if len(self.ends) == 1 else None

    def describe_multiple_periods(self, year: int) -> str:
        """Return an ASCII diagnostic phrase listing all transitions found."""
        tz_label = f"timezone '{self.timezone}'" if self.timezone else "this site"
        parts: list[str] = []
        if self.starts:
            parts.append(f"starts on DOY {', '.join(str(d) for d in self.starts)}")
        if self.ends:
            parts.append(f"ends on DOY {', '.join(str(d) for d in self.ends)}")
        detail = f" ({'; '.join(parts)})" if parts else ""
        return f"{tz_label} has multiple daylight saving periods in {year}{detail}"

    def describe_non_hourly_dst(self, year: int) -> str:
        """Return an ASCII diagnostic phrase for a non-1-hour DST offset shift."""
        tz_label = f"timezone '{self.timezone}'" if self.timezone else "this site"
        step = self.offset_step_hours
        step_label = f"{step:g} h" if step is not None else "non-1 h"
        return (
            f"{tz_label} uses a non-hourly daylight saving shift ({step_label}) in {year}"
        )


def resolve_dst_schedule(dls_obj: Any) -> DSTSchedule:
    """Return a :class:`DSTSchedule` from a ``DLSCheck`` or compatible stub."""
    inspect_fn = getattr(dls_obj, "inspect_dst_schedule", None)
    if callable(inspect_fn):
        return inspect_fn()
    start, end, utc_offset = dls_obj.compute_dst_transitions()
    return DSTSchedule(
        timezone="<resolved>" if utc_offset is not None else None,
        utc_offset_hours=utc_offset,
        starts=(start,) if start is not None else (),
        ends=(end,) if end is not None else (),
        scan_complete=utc_offset is not None or start is not None or end is not None,
    )


class DLSCheck(BaseModel):
    lat: float
    lng: float
    year: int
    startdls: Optional[int] = None
    enddls: Optional[int] = None

    def inspect_dst_schedule(self) -> DSTSchedule:
        """Collect all DST transitions for ``self.year`` and derive base UTC offset together."""
        try:
            if not HAS_TIMEZONE_FINDER:
                logger_supy.debug(
                    "[DLS] No timezone finder available, using limited fallback."
                )
                tz_name = _fallback_timezone_name(self.lat, self.lng)
            else:
                tf = TimezoneFinder()
                tz_name = tf.timezone_at(lat=self.lat, lng=self.lng)
        except Exception as e:
            logger_supy.debug(
                f"[DLS] Timezone finder failed for lat={self.lat}, lng={self.lng}: {e}"
            )
            return DSTSchedule(
                timezone=None,
                utc_offset_hours=None,
                starts=(),
                ends=(),
                scan_complete=False,
            )

        if not tz_name:
            logger_supy.debug(
                f"[DLS] Cannot determine timezone for lat={self.lat}, lng={self.lng}"
            )
            return DSTSchedule(
                timezone=None,
                utc_offset_hours=None,
                starts=(),
                ends=(),
                scan_complete=False,
            )

        logger_supy.debug(f"[DLS] Timezone identified as '{tz_name}'")
        try:
            tz = pytz.timezone(tz_name)
        except Exception as e:
            logger_supy.debug(f"[DLS] Failed to load timezone '{tz_name}': {e}")
            return DSTSchedule(
                timezone=None,
                utc_offset_hours=None,
                starts=(),
                ends=(),
                scan_complete=False,
            )

        # Scan every day of the target year (comparing against Dec 31 of the
        # preceding year so 1st-of-month transitions such as April 1 in
        # Australia/Sydney 2012, mid-year Ramadan windows such as
        # Africa/Casablanca 2019, and multi-period years such as
        # Africa/Casablanca 2012 are all captured consistently). Any daily
        # localisation failure marks the scan incomplete so callers never act
        # on partial or shifted transition observations.
        starts: list[int] = []
        ends: list[int] = []
        transition_steps: list[float] = []
        daily_offsets: list[timedelta] = []
        try:
            prev_offset = tz.localize(
                datetime(self.year - 1, 12, 31, 12), is_dst=None
            ).utcoffset()
            if prev_offset is None:
                raise ValueError("Preceding year-end UTC offset is None")

            curr_date = datetime(self.year, 1, 1, 12)
            year_end = datetime(self.year, 12, 31, 12)
            while curr_date <= year_end:
                curr_offset = tz.localize(curr_date, is_dst=None).utcoffset()
                if curr_offset is None:
                    raise ValueError(f"UTC offset is None on {curr_date.date()}")
                daily_offsets.append(curr_offset)
                if curr_offset != prev_offset:
                    doy = curr_date.timetuple().tm_yday
                    transition_steps.append(
                        abs((curr_offset - prev_offset).total_seconds()) / 3600
                    )
                    if curr_offset > prev_offset:
                        starts.append(doy)
                    elif curr_offset < prev_offset:
                        ends.append(doy)
                prev_offset = curr_offset
                curr_date += timedelta(days=1)
        except Exception as e:
            logger_supy.debug(f"[DLS] Failed to scan DST transitions: {e}")
            return DSTSchedule(
                timezone=tz_name,
                utc_offset_hours=None,
                starts=(),
                ends=(),
                scan_complete=False,
            )

        # Derive standard (base) UTC offset from the complete yearly scan
        # rather than only Jan/Jul samples: SUEWS adds +1 h during DST, so the
        # minimum yearly offset is always the consistent base offset (covering
        # northern hemisphere, southern hemisphere, Irish winter, fractional
        # offsets, and mid-year drop windows such as Africa/Casablanca 2019).
        # Record transition magnitudes separately (including any Jan 1 boundary
        # shift such as Asia/Kathmandu 1986 +15 min) so non-hourly shifts can
        # never bypass the +1 h DST safety check.
        distinct_offsets = sorted(set(daily_offsets))
        std_offset = distinct_offsets[0]
        utc_offset_hours: int | float = std_offset.total_seconds() / 3600
        if utc_offset_hours.is_integer():
            utc_offset_hours = int(utc_offset_hours)
        non_hourly_steps = [
            step for step in transition_steps if abs(step - 1.0) > 1e-9
        ]
        offset_step_hours: float | None = None
        if non_hourly_steps:
            offset_step_hours = non_hourly_steps[0]
        elif transition_steps:
            offset_step_hours = transition_steps[0]
        elif len(distinct_offsets) == 2:
            offset_step_hours = (
                distinct_offsets[1] - distinct_offsets[0]
            ).total_seconds() / 3600
        logger_supy.debug(f"[DLS] UTC offset in standard time: {utc_offset_hours}")

        return DSTSchedule(
            timezone=tz_name,
            utc_offset_hours=utc_offset_hours,
            starts=tuple(starts),
            ends=tuple(ends),
            scan_complete=True,
            offset_step_hours=offset_step_hours,
            distinct_offset_count=len(distinct_offsets),
        )

    def compute_dst_transitions(self):
        schedule = self.inspect_dst_schedule()
        return schedule.start, schedule.end, schedule.utc_offset_hours


def nullify_co2_block_recursive(block: Any) -> None:
    """
    In-place canonical nullifier for CO2 / RefValue / DayProfile-like YAML blocks.

    Behaviour:
    - If a dict looks like a RefValue leaf (contains "value"), set "value" to None.
      If the value is a list, replace it with a list of the same length of None.
    - If a dict contains day-profile keys ("working_day" and/or "holiday"), preserve
      the inner structure but nullify its contents:
        - dict -> keep keys, set each value to None
        - list -> list of same length of None
        - otherwise -> set to None
    - Recurse into nested dicts and lists; scalar leaves are replaced with None.
    - Robust to empty/missing keys and arbitrary shapes.
    """
    if isinstance(block, dict):
        # Ref-only placeholder ({"ref": ...}): already value-less; leave its
        # metadata intact rather than nullifying ref.desc / ref.DOI (gh#2297).
        if is_ref_only(block):
            return
        # Handle RefValue leaf ({"value": ...})
        if "value" in block:
            val = block["value"]
            if isinstance(val, list):
                block["value"] = [None] * len(val)
            else:
                block["value"] = None
            return

        # Handle DayProfile / HourlyProfile style dicts by preserving shape but nulling contents
        def _nullify_profile_field(field):
            if field in block:
                v = block[field]
                if isinstance(v, dict):
                    # Keep keys, set each inner value to None (preserve shape)
                    for k in list(v.keys()):
                        v[k] = None
                    block[field] = v
                elif isinstance(v, list):
                    block[field] = [None] * len(v)
                else:
                    block[field] = None

        if "working_day" in block or "holiday" in block:
            _nullify_profile_field("working_day")
            _nullify_profile_field("holiday")
            return

        # Recurse into dict entries; scalar leaves -> None
        for k, v in list(block.items()):
            if isinstance(v, (dict, list)):
                nullify_co2_block_recursive(v)
            else:
                block[k] = None

    elif isinstance(block, list):
        for i, item in enumerate(block):
            if isinstance(item, (dict, list)):
                nullify_co2_block_recursive(item)
            else:
                block[i] = None


def collect_nullified_paths(before: Any, after: Any, path: str = "") -> List[str]:
    """
    Return list of dot/list-index paths that were non-null in `before` and
    became null/empty in `after`.

    Recognises RefValue leaves ({"value": ...}) and DayProfile keys
    ("working_day", "holiday") and handles dicts, lists and scalars.
    """
    paths: List[str] = []

    def _p(base: str, key: str) -> str:
        return f"{base}.{key}" if base else key

    def _is_null(value: Any) -> bool:
        # A list-valued RefValue is nullified element-wise ([None, None, ...]).
        if isinstance(value, list):
            return bool(value) and all(item in (None, "") for item in value)
        return value in (None, "")

    # Dict vs Dict
    if isinstance(before, dict) and isinstance(after, dict):
        # RefValue leaf detection
        if "value" in before and "value" in after:
            if not _is_null(before.get("value")) and _is_null(after.get("value")):
                paths.append(path or "value")
            return paths

        # Day/profile keys
        if ("working_day" in before or "holiday" in before) and (
            ("working_day" in after) or ("holiday" in after)
        ):
            if before.get("working_day") not in (None, "") and after.get(
                "working_day"
            ) in (None, ""):
                paths.append(_p(path, "working_day"))
            if before.get("holiday") not in (None, "") and after.get("holiday") in (
                None,
                "",
            ):
                paths.append(_p(path, "holiday"))
            return paths

        # Preserve mapping order: set iteration made report text vary with
        # PYTHONHASHSEED. New keys still follow the original input's keys.
        for k in dict.fromkeys([*before, *after]):
            b = before.get(k)
            a = after.get(k)
            if b in (None, "") and a in (None, ""):
                continue
            new_path = _p(path, k)
            if a in (None, "") and b not in (None, ""):
                paths.append(new_path)
                continue
            paths.extend(collect_nullified_paths(b, a, new_path))
        return paths

    # List vs List
    if isinstance(before, list) and isinstance(after, list):
        for i, (b_item, a_item) in enumerate(zip(before, after)):
            item_path = f"{path}[{i}]" if path else f"[{i}]"
            if a_item in (None, "") and b_item not in (None, ""):
                paths.append(item_path)
            else:
                paths.extend(collect_nullified_paths(b_item, a_item, item_path))
        if len(before) > len(after):
            for i in range(len(after), len(before)):
                if before[i] not in (None, ""):
                    paths.append(f"{path}[{i}]")
        return paths

    # Scalar comparison
    if before not in (None, "") and after in (None, ""):
        paths.append(path or "")
    return paths


def _nullify_biogenic_in_props(props: dict) -> bool:
    """Nullify biogenic CO2 params under properties.land_cover for selected surfaces."""
    if not isinstance(props, dict):
        return False
    land_cover = props.get("land_cover") or {}
    if not isinstance(land_cover, dict):
        return False

    params = (
        "alpha_bioco2",
        "alpha_bio_co2",
        "alpha_enh_bioco2",
        "beta_bioco2",
        "beta_bio_co2",
        "beta_enh_bioco2",
        "min_res_bioco2",
        "theta_bioco2",
        "theta_bio_co2",
        "resp_a",
        "resp_b",
    )
    surfaces = ("dectr", "evetr", "grass")

    changed = False
    for surface in surfaces:
        surface_props = land_cover.get(surface)
        if not isinstance(surface_props, dict):
            continue

        surface_changed = False
        for param in params:
            # Reads RefValue dicts and bare values alike; ref-only placeholders
            # and nulls have no value to clear, so they are left as written.
            current = get_ref_or_bare(surface_props, param)
            if current is None:
                continue
            if isinstance(current, list):
                if all(item is None for item in current):
                    continue
                cleared = [None] * len(current)
            else:
                cleared = None
            set_ref_or_bare(surface_props, param, cleared)
            surface_changed = True

        if surface_changed:
            land_cover[surface] = surface_props
            changed = True

    if changed:
        props["land_cover"] = land_cover
    return changed


def get_mean_monthly_air_temperature(
    lat: float, lon: float, month: int, spatial_res: float = 0.5
) -> float:
    """
    Calculate mean monthly air temperature using CRU TS4.06 climatological data.

    This function uses the CRU TS4.06 cell monthly normals dataset (1991-2020)
    to provide accurate location-specific temperature estimates. CRU data is
    required - the function will raise an error if CRU data is not available.

    Args:
        lat (float): Site latitude in degrees (positive for Northern Hemisphere, negative for Southern).
        lon (float): Site longitude in degrees (-180 to 180).
        month (int): Month of the year (1 = January, 12 = December).
        spatial_res (float): Search spatial resolution for finding nearest CRU grid cell (degrees). Default 0.5.

    Returns:
        float: Mean monthly air temperature for the given location and month (°C).

    Raises:
        ValueError: If the input month is not between 1 and 12, coordinates are invalid,
                   or no CRU data found within spatial resolution.
        FileNotFoundError: If CRU data file is not found.
    """
    import pandas as pd
    import numpy as np

    # Validate inputs
    if not (1 <= month <= 12):
        raise ValueError(f"Month must be between 1 and 12, got {month}")
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Load CRU data from package resources using importlib.resources
    # Access the Parquet file in the ext_data directory
    cru_resource = trv_supy_module / "ext_data" / "CRU_TS4.06_1991_2020.parquet"

    if not cru_resource.exists():
        raise FileNotFoundError(
            f"CRU data file not found at {cru_resource}. "
            "Please ensure the CRU Parquet file is available in the package."
        )

    # Read the Parquet file - this works even when package is installed
    with cru_resource.open("rb") as f:
        df = pd.read_parquet(f)

    # Validate required columns
    required_cols = ["Month", "Latitude", "Longitude", "NormalTemperature"]
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns in CRU data: {missing_cols}")

    # Filter for the specific month
    month_data = df[df["Month"] == month]
    if month_data.empty:
        raise ValueError(f"No CRU data available for month {month}")

    # Find nearest grid cell using both lat and lon
    # Use both latitude and longitude for precise matching
    lat_distances = np.abs(month_data["Latitude"] - lat)
    lon_distances = np.abs(month_data["Longitude"] - lon)

    # Find points within spatial_res for both coordinates
    lat_mask = lat_distances <= spatial_res
    lon_mask = lon_distances <= spatial_res
    nearby_data = month_data[lat_mask & lon_mask]

    if nearby_data.empty:
        # Try with larger spatial_res
        spatial_res_expanded = spatial_res * 2
        lat_mask = lat_distances <= spatial_res_expanded
        lon_mask = lon_distances <= spatial_res_expanded
        nearby_data = month_data[lat_mask & lon_mask]

        if nearby_data.empty:
            raise ValueError(
                f"No CRU data found within {spatial_res_expanded} degrees of coordinates "
                f"({lat}, {lon}) for month {month}. Try increasing spatial resolution or "
                f"check if coordinates are within CRU data coverage area."
            )

    # Calculate Euclidean distance for closest point
    if len(nearby_data) > 1:
        distances = np.sqrt(
            lat_distances[nearby_data.index] ** 2
            + lon_distances[nearby_data.index] ** 2
        )
        closest_idx = distances.idxmin()
        temperature = float(month_data.loc[closest_idx, "NormalTemperature"])
    else:
        temperature = float(nearby_data.iloc[0]["NormalTemperature"])

    return temperature

def get_monthly_air_temperature_diffmax(
    lat: float, lon: float, spatial_res: float = 0.5
) -> float:
    """
    Calculates the maximum difference between mean monthly air temperatures
    using CRU TS4.06 climatological data. The result is the difference between
    the warmest and coldest mean monthly temperature within the annual cycle
    at the specified location.

    Parameters
    ----------
    lat : float
        Site latitude in degrees (positive for Northern Hemisphere, negative for Southern).
    lon : float
        Site longitude in degrees (-180 to 180).
    spatial_res : float, optional
        Search spatial resolution for nearest CRU grid cell (default is 0.5).

    Returns
    -------
    float
        Maximum difference in mean monthly air temperature (°C).

    Raises
    ------
    ValueError
        If input values are invalid or CRU data cannot be found.
    FileNotFoundError
        If the CRU data file is missing.
    """
    monthly_temps = []
    for month in range(1, 13):
        monthly_temp = get_mean_monthly_air_temperature(lat, lon, month, spatial_res)
        monthly_temps.append(monthly_temp)
    diffmax = float(max(monthly_temps) - min(monthly_temps))
    return diffmax


def get_mean_annual_air_temperature(
    lat: float, lon: float, spatial_res: float = 0.5
) -> float:
    """
    Calculate mean annual air temperature using CRU TS4.06 climatological data.

    This function uses the CRU TS4.06 cell monthly normals dataset (1991-2020)
    to provide accurate location-specific temperature estimates by averaging all
    12 monthly values. This provides a stable, long-term average annual temperature
    suitable for initialising parameters that do not vary rapidly with seasons.

    Args:
        lat (float): Site latitude in degrees (positive for Northern Hemisphere, negative for Southern).
        lon (float): Site longitude in degrees (-180 to 180).
        spatial_res (float): Search spatial resolution for finding nearest CRU grid cell (degrees). Default 0.5.

    Returns:
        float: Mean annual air temperature based on 1991-2020 climate normals (°C).

    Raises:
        ValueError: If coordinates are invalid or no CRU data found within spatial resolution.
        FileNotFoundError: If CRU data file is not found.
    """
    import numpy as np

    # Validate inputs (lat/lon validation also done by monthly function, but validate here for clarity)
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Get all 12 monthly temperatures and calculate the average
    # Note: get_mean_monthly_air_temperature validates month internally and raises ValueError/FileNotFoundError
    # We let these exceptions propagate as they indicate fundamental data issues
    monthly_temps = []
    try:
        for month in range(1, 13):
            monthly_temp = get_mean_monthly_air_temperature(
                lat, lon, month, spatial_res
            )
            monthly_temps.append(monthly_temp)
    except (ValueError, FileNotFoundError) as e:
        # Re-raise with context about which month failed
        raise type(e)(f"Failed to calculate annual temperature: {e}") from e

    annual_temp = float(np.mean(monthly_temps))
    return annual_temp


_BOUND_FORMAT_HINT = "a date (YYYY-MM-DD) or a timestamp (YYYY-MM-DD HH:MM)"


def _bound_as_date(value, name: str, errors: List[str]) -> Optional[str]:
    """Return ``value`` as a ``YYYY-MM-DD`` string, or record why it cannot be."""
    if not isinstance(value, str) or not value.strip():
        errors.append(
            f"Missing or invalid '{name}' in model.control: must be {_BOUND_FORMAT_HINT}"
        )
        return None
    try:
        ts = pd.Timestamp(value)
    except (TypeError, ValueError):
        ts = None
    if ts is None or pd.isna(ts):
        errors.append(
            f"Invalid '{name}' in model.control ({value!r}): must be {_BOUND_FORMAT_HINT}"
        )
        return None
    return ts.strftime("%Y-%m-%d")


def parse_simulation_bounds(
    data: dict, allow_missing: bool = False
) -> Tuple[List[str], Optional[int], Optional[str], Optional[str]]:
    """Read ``model.control.start_time`` / ``end_time`` for the validators.

    Both fields accept a date or a timestamp (see ``ModelControl``). The
    validators only need the calendar day, so each bound is returned as a
    ``YYYY-MM-DD`` string together with the model year taken from the start.

    Parameters
    ----------
    data : dict
        Raw YAML configuration.
    allow_missing : bool, optional
        When True, a null or absent bound is returned as None without an
        error; ``ModelControl`` documents both as optional (the run then uses
        the forcing bounds). A bound that is set but unparsable is still an
        error. By default both bounds are required.

    Returns
    -------
    tuple
        ``(errors, model_year, start_date, end_date)``; ``errors`` is empty
        when both bounds parse.
    """
    model = data.get("model") if isinstance(data, Mapping) else None
    control = model.get("control") if isinstance(model, Mapping) else None
    if not isinstance(control, Mapping):
        control = {}
    errors: List[str] = []
    bounds = {}
    for name in ("start_time", "end_time"):
        value = control.get(name)
        if isinstance(value, Mapping) and "value" in value:
            value = value["value"]
        if value is None and allow_missing:
            bounds[name] = None
        else:
            bounds[name] = _bound_as_date(value, name, errors)
    start_date = bounds["start_time"]
    end_date = bounds["end_time"]
    model_year = int(start_date[:4]) if start_date is not None else None
    return errors, model_year, start_date, end_date
