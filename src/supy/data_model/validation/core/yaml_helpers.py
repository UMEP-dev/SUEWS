from typing import (
    Dict,
    List,
    Optional,
    Union,
    Literal,
    Tuple,
    Type,
    Generic,
    TypeVar,
    Any,
)
from pydantic import (
    ConfigDict,
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
    ValidationError,
)
import numpy as np
import pandas as pd
import yaml
import csv
import os
from copy import deepcopy
from datetime import datetime
import pytz

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
import os


def get_value_safe(param_dict, param_key, default=None):
    """Safely extract value from RefValue or plain format.

    Args:
        param_dict: Dictionary containing the parameter
        param_key: Key to look up
        default: Default value if key not found

    Returns:
        The parameter value, handling both RefValue {"value": X} and plain X formats
    """
    param = param_dict.get(param_key, default)
    if isinstance(param, dict) and "value" in param:
        return param["value"]  # RefValue format: {"value": 1}
    else:
        return param  # Plain format: 1


class SeasonCheck(BaseModel):
    start_date: str  # Expected format: YYYY-MM-DD
    lat: float

    def get_season(self) -> str:
        try:
            start = datetime.strptime(self.start_date, "%Y-%m-%d").timetuple().tm_yday
        except ValueError:
            raise ValueError("start_date must be in YYYY-MM-DD format")

        abs_lat = abs(self.lat)

        if abs_lat <= 10:
            return "equatorial"
        if 10 < abs_lat < 23.5:
            return "tropical"

        if self.lat >= 0:  # Northern Hemisphere
            if 150 < start < 250:
                return "summer"
            elif 60 < start <= 150:
                return "spring"
            elif 250 <= start < 335:
                return "fall"
            else:
                return "winter"
        else:  # Southern Hemisphere
            if 150 < start < 250:
                return "winter"
            elif 60 < start <= 150:
                return "fall"
            elif 250 <= start < 335:
                return "spring"
            else:
                return "summer"


class DLSCheck(BaseModel):
    lat: float
    lng: float
    year: int
    startdls: Optional[int] = None
    enddls: Optional[int] = None

    def compute_dst_transitions(self):
        if not HAS_TIMEZONE_FINDER:
            logger_supy.debug(
                "[DLS] No timezone finder available, skipping DST calculation."
            )
            return None, None, None

        tf = TimezoneFinder()
        tz_name = tf.timezone_at(lat=self.lat, lng=self.lng)

        if not tz_name:
            logger_supy.debug(
                f"[DLS] Cannot determine timezone for lat={self.lat}, lng={self.lng}"
            )
            return None, None, None

        logger_supy.debug(f"[DLS] Timezone identified as '{tz_name}'")
        tz = pytz.timezone(tz_name)

        def find_transition(month: int) -> Optional[int]:
            try:
                prev_dt = tz.localize(datetime(self.year, month, 1, 12), is_dst=None)
                prev_offset = prev_dt.utcoffset()
                for day in range(2, 32):
                    try:
                        curr_dt = tz.localize(
                            datetime(self.year, month, day, 12), is_dst=None
                        )
                        curr_offset = curr_dt.utcoffset()
                        if curr_offset != prev_offset:
                            return curr_dt.timetuple().tm_yday
                        prev_offset = curr_offset
                    except Exception:
                        continue
                return None
            except Exception:
                return None

        # Get standard UTC offset (in winter)
        try:
            std_dt = tz.localize(datetime(self.year, 1, 15), is_dst=False)
            utc_offset_hours = int(std_dt.utcoffset().total_seconds() / 3600)
            logger_supy.debug(f"[DLS] UTC offset in standard time: {utc_offset_hours}")
        except Exception as e:
            logger_supy.debug(f"[DLS] Failed to compute UTC offset: {e}")
            utc_offset_hours = None

        # Determine DST start and end days
        if self.lat >= 0:  # Northern Hemisphere
            start = find_transition(3) or find_transition(4)
            end = find_transition(10) or find_transition(11)
        else:  # Southern Hemisphere
            start = find_transition(9) or find_transition(10)
            end = find_transition(3) or find_transition(4)

        return start, end, utc_offset_hours


def collect_yaml_differences(original: Any, updated: Any, path: str = "") -> List[dict]:
    """
    Recursively compare two YAML data structures and collect all differences.

    For each mismatch between the original and updated YAML dictionaries or lists, this function:

    - Records the site index (if applicable, extracted from path strings like `sites[0]`).
    - Identifies the affected parameter (either the key before `.value` or the final key in the path).
    - Reports the old and new values.
    - Includes a standard reason string: "Updated by precheck".

    This function is used to build a human-readable report of all changes made during precheck.

    Args:
        original (Any): The original YAML data (typically before precheck adjustments).
        updated (Any): The updated YAML data (after precheck).
        path (str, optional): The current nested path within the YAML structure (used internally for recursion).

    Returns:
        List[dict]: A list of dictionaries, each describing a difference with keys:
            - 'site' (int or None)
            - 'parameter' (str)
            - 'old_value' (Any)
            - 'new_value' (Any)
            - 'reason' (str)
    """

    diffs = []

    if isinstance(original, dict) and isinstance(updated, dict):
        all_keys = set(original.keys()) | set(updated.keys())
        for key in all_keys:
            new_path = f"{path}.{key}" if path else key
            orig_val = original.get(key, "__MISSING__")
            updated_val = updated.get(key, "__MISSING__")
            diffs.extend(collect_yaml_differences(orig_val, updated_val, new_path))

    elif isinstance(original, list) and isinstance(updated, list):
        max_len = max(len(original), len(updated))
        for i in range(max_len):
            orig_val = original[i] if i < len(original) else "__MISSING__"
            updated_val = updated[i] if i < len(updated) else "__MISSING__"
            new_path = f"{path}[{i}]"
            diffs.extend(collect_yaml_differences(orig_val, updated_val, new_path))

    else:
        if original != updated:
            # Extract site index
            site = None
            if "sites[" in path:
                try:
                    site = int(path.split("sites[")[1].split("]")[0])
                except Exception:
                    site = None

            # Get param name: key before '.value' or the last part of the path
            if ".value" in path:
                param_name = path.split(".")[-2]
            else:
                param_name = path.split(".")[-1]

            diffs.append({
                "site": site,
                "parameter": param_name,
                "old_value": original,
                "new_value": updated,
                "reason": "Updated by precheck",
            })

    return diffs


def save_precheck_diff_report(diffs: List[dict], original_yaml_path: str):
    """
    Save the list of YAML differences found during precheck as a CSV report.

    The report is saved in the same directory as the original YAML file, with a filename like
    `precheck_report_<original_filename>.csv`.

    Each row in the CSV contains:
    - Site index (or None if not site-specific)
    - Parameter name
    - Old value
    - New value
    - Reason for the change (typically "Updated by precheck")

    If no differences are found, the function logs an info message and does not create any file.

    Args:
        diffs (List[dict]): List of differences produced by `collect_yaml_differences`.
        original_yaml_path (str): Full path to the original YAML file (used to determine output location and name).

    Returns:
        None
    """
    if not diffs:
        logger_supy.info("No differences found between original and updated YAML.")
        return

    report_filename = f"precheck_report_{os.path.basename(original_yaml_path).replace('.yml', '.csv')}"
    report_path = os.path.join(os.path.dirname(original_yaml_path), report_filename)

    with open(report_path, "w", newline="") as csvfile:
        writer = csv.DictWriter(
            csvfile,
            fieldnames=["site", "parameter", "old_value", "new_value", "reason"],
        )
        writer.writeheader()
        for row in diffs:
            for key in ["old_value", "new_value"]:
                if row[key] is None:
                    row[key] = "null"
            writer.writerow(row)

    logger_supy.info(f"Precheck difference report saved to: {report_path}")


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
    from pathlib import Path

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


def precheck_printing(data: dict) -> dict:
    """
    Log the start of the precheck process.

    This function prints a simple info message to indicate that the precheck process has started.
    It does not modify the input data.

    Args:
        data (dict): The SUEWS configuration dictionary.

    Returns:
        dict: The original input data, unmodified.
    """

    logger_supy.info("Running basic precheck...")
    return data


def precheck_start_end_date(data: dict) -> Tuple[dict, int, str, str]:
    """
    Extract model year, start date, and end date from YAML dict.

    This function reads the 'start_time' and 'end_time' fields from the input YAML dict
    (under 'model.control'), validates that both exist and are in 'YYYY-MM-DD' format,
    and extracts the model year from the start date.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Raises:
        ValueError: If 'start_time' or 'end_time' is missing or has an invalid format.

    Returns:
        Tuple[dict, int, str, str]:
            - Unmodified input data (for chaining)
            - Model year (int, extracted from start date)
            - Start date (str, in YYYY-MM-DD format)
            - End date (str, in YYYY-MM-DD format)
    """

    control = data.get("model", {}).get("control", {})

    start_date = control.get("start_time")
    end_date = control.get("end_time")

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError(
            "Missing or invalid 'start_time' in model.control — must be in 'YYYY-MM-DD' format."
        )

    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError(
            "Missing or invalid 'end_time' in model.control — must be in 'YYYY-MM-DD' format."
        )

    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError(
            "Could not extract model year from 'start_time'. Ensure it is in 'YYYY-MM-DD' format."
        )

    return data, model_year, start_date, end_date


def precheck_model_physics_params(data: dict) -> dict:
    """
    Validate presence and non-emptiness of required model physics parameters.

    This function checks that all required keys exist under 'model.physics' in the YAML
    dict and that none of them are empty or null. If 'model.physics' is empty, the check
    is skipped (used to allow partial configurations during early stages).

    Required fields include:
        - netradiationmethod
        - emissionsmethod
        - storageheatmethod
        - ohmincqf
        - roughlenmommethod
        - roughlenheatmethod
        - stabilitymethod
        - smdmethod
        - waterusemethod
        - rslmethod
        - faimethod
        - rsllevel
        - snowuse
        - stebbsmethod

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Raises:
        ValueError: If required parameters are missing or contain empty/null values.

    Returns:
        dict: Unmodified input data (for chaining).
    """

    physics = data.get("model", {}).get("physics", {})

    if not physics:
        logger_supy.debug("Skipping physics param check — physics is empty.")
        return data

    required = [
        "netradiationmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "roughlenmommethod",
        "roughlenheatmethod",
        "stabilitymethod",
        "smdmethod",
        "waterusemethod",
        "rslmethod",
        "faimethod",
        "rsllevel",
        "snowuse",
        "stebbsmethod",
    ]

    missing = [k for k in required if k not in physics]
    if missing:
        raise ValueError(f"[model.physics] Missing required params: {missing}")

    empty = [k for k in required if get_value_safe(physics, k) in ("", None)]
    if empty:
        raise ValueError(f"[model.physics] Empty or null values for: {empty}")

    logger_supy.debug("All model.physics required params present and non-empty.")
    return data


def precheck_model_options_constraints(data: dict) -> dict:
    """
    Enforce internal consistency between model physics options.

    This function verifies logical dependencies between selected model physics methods.
    Specifically, if 'rslmethod' is set to 2, it enforces that 'stabilitymethod' equals 3,
    as required for diagnostic aerodynamic calculations.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Raises:
        ValueError: If model physics options violate internal consistency rules.

    Returns:
        dict: Unmodified input data (for chaining).
    """

    physics = data.get("model", {}).get("physics", {})

    diag = get_value_safe(physics, "rslmethod")
    stability = get_value_safe(physics, "stabilitymethod")

    if diag == 2 and stability != 3:
        raise ValueError(
            "[model.physics] If rslmethod == 2, stabilitymethod must be 3."
        )

    logger_supy.debug("rslmethod-stabilitymethod constraint passed.")
    return data


def precheck_replace_empty_strings_with_none(data: dict) -> dict:
    """
    Replace empty string values with None across the entire YAML dictionary,
    except for parameters inside 'model.control' and 'model.physics'.

    This step ensures that empty strings are treated as missing values for Pydantic validation,
    while preserving intentional empty strings in control and physics settings.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: Cleaned YAML dictionary with empty strings replaced by None,
              except within 'model.control' and 'model.physics'.
    """

    ignore_keys = {"control", "physics"}

    def recurse(obj: Any, path=()):
        if isinstance(obj, dict):
            new = {}
            for k, v in obj.items():
                sub_path = path + (k,)
                if v == "" and not (
                    len(sub_path) >= 2
                    and sub_path[0] == "model"
                    and sub_path[1] in ignore_keys
                ):
                    new[k] = None
                else:
                    new[k] = recurse(v, sub_path)
            return new
        elif isinstance(obj, list):
            return [None if item == "" else recurse(item, path) for item in obj]
        else:
            return obj

    cleaned = recurse(data)
    logger_supy.info(
        "Empty strings replaced with None (except model.control and model.physics)."
    )
    return cleaned


def precheck_site_season_adjustments(
    data: dict, start_date: str, model_year: int
) -> dict:
    """
    Adjust site-specific parameters based on season and geographic location.

    This step:
    - Determines the season (summer, winter, spring, fall, tropical, equatorial) for each site based on latitude and start_date.
    - Nullifies 'snowalb' in initial states for summer/tropical/equatorial sites.
    - Sets 'lai_id' for deciduous trees based on the detected season and LAI min/max values.
    - Runs DLSCheck to calculate daylight saving time start/end days and timezone offset for each site, overwriting any existing values.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.
        start_date (str): Start date of the simulation (format 'YYYY-MM-DD').
        model_year (int): Model year extracted from start_date.

    Returns:
        dict: Updated YAML dictionary with site-level season-dependent adjustments.
    """

    cleaned_sites = []

    for i, site in enumerate(data.get("sites", [])):
        if isinstance(site, BaseModel):
            site = site.model_dump(mode="python")

        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # --------------------
        # 1. Determine season
        # --------------------
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        lng = get_value_safe(props, "lng")
        season = None

        try:
            if lat is not None:  # <- Placeholder: consider cases where lat is None
                season = SeasonCheck(start_date=start_date, lat=lat).get_season()
                logger_supy.debug(f"[site #{i}] Season detected: {season}")

                # If equatorial / tropical / summer → nullify snowalb
                if (
                    season in ("summer", "tropical", "equatorial")
                    and "snowalb" in initial_states
                ):
                    if isinstance(initial_states["snowalb"], dict):
                        initial_states["snowalb"]["value"] = None
                        logger_supy.info(f"[site #{i}] Set snowalb to None")
        except Exception as e:
            raise ValueError(f"[site #{i}] SeasonCheck failed: {e}")

        # --------------------------------------
        # 2. Seasonal adjustment for DecTrees LAI
        # --------------------------------------
        dectr = props.get("land_cover", {}).get("dectr", {})
        sfr = get_value_safe(dectr, "sfr", 0)

        if sfr > 0:
            lai = dectr.get("lai", {})
            laimin = get_value_safe(lai, "laimin")
            laimax = get_value_safe(lai, "laimax")
            lai_val = None

            if laimin is not None and laimax is not None:
                if season == "summer":
                    lai_val = laimax
                elif season == "winter":
                    lai_val = laimin
                elif season in ("spring", "fall"):
                    lai_val = (laimax + laimin) / 2

                if "dectr" in initial_states:
                    initial_states["dectr"]["lai_id"] = {"value": lai_val}
                    logger_supy.debug(
                        f"[site #{i}] Set lai_id to {lai_val} for season {season}"
                    )
        else:
            if "dectr" in initial_states:
                initial_states["dectr"]["lai_id"] = {"value": None}
                logger_supy.debug(f"[site #{i}] Nullified lai_id (no dectr surface)")

        # --------------------------------------
        # 3. DLS Check (timezone and DST start/end days)
        # --------------------------------------
        if (
            lat is not None and lng is not None
        ):  # <- Placeholder: consider cases where lat is None
            try:
                dls = DLSCheck(lat=lat, lng=lng, year=model_year)
                start_dls, end_dls, tz_offset = dls.compute_dst_transitions()

                if start_dls and end_dls:
                    props["anthropogenic_emissions"]["startdls"] = {"value": start_dls}
                    props["anthropogenic_emissions"]["enddls"] = {"value": end_dls}
                    logger_supy.debug(
                        f"[site #{i}] DLS: start={start_dls}, end={end_dls}"
                    )

                if tz_offset is not None:
                    props["timezone"] = {"value": tz_offset}
                    logger_supy.debug(f"[site #{i}] Timezone set to {tz_offset}")

                logger_supy.info(
                    f"[site #{i}] Overwriting pre-existing startdls and enddls with computed values."
                )
            except Exception as e:
                logger_supy.warning(
                    f"[site #{i}] DLSCheck failed: {e}. DST will not be configured automatically."
                )

        # Final update
        site["properties"] = props
        site["initial_states"] = initial_states
        cleaned_sites.append(site)

    data["sites"] = cleaned_sites
    return data


def precheck_update_temperature(data: dict, start_date: str) -> dict:
    """
    Set initial surface temperatures for all surface types based on latitude and start month.

    For each site:
    - Uses the site's latitude and the month from start_date to estimate mean monthly air temperature using CRU data.
    - Applies this temperature to all layers of surface temperature arrays, as well as 'tsfc' and 'tin' for each surface type (paved, bldgs, evetr, dectr, grass, bsoil, water).
    - If latitude is missing, the site is skipped with a warning.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.
        start_date (str): Start date of the simulation (format 'YYYY-MM-DD').

    Returns:
        dict: Updated YAML dictionary with surface temperatures initialised.
    """

    month = datetime.strptime(start_date, "%Y-%m-%d").month

    for site_idx, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # Get site latitude
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        if lat is None:
            logger_supy.warning(
                f"[site #{site_idx}] Latitude missing, skipping surface temperature update."
            )
            continue

        # Get site longitude (required for CRU matching)
        lng_entry = props.get("lng", {})
        lng = lng_entry.get("value") if isinstance(lng_entry, dict) else lng_entry

        # If longitude is missing, skip this site (should not happen with valid config)
        if lng is None:
            logger_supy.warning(
                f"[site #{site_idx}] Longitude not found in configuration, skipping temperature initialization"
            )
            continue

        # Get estimated average temperature
        avg_temp = get_mean_monthly_air_temperature(lat, lng, month)
        coord_info = f"lat={lat}, lng={lng}"
        logger_supy.info(
            f"[site #{site_idx}] Setting surface temperatures to {avg_temp} C for month {month} ({coord_info})"
        )

        # Loop over all surface types
        for surface_type in [
            "paved",
            "bldgs",
            "evetr",
            "dectr",
            "grass",
            "bsoil",
            "water",
        ]:
            surf = initial_states.get(surface_type, {})
            if not isinstance(surf, dict):
                continue

            # Set 5-layer temperature array
            if "temperature" in surf and isinstance(surf["temperature"], dict):
                surf["temperature"]["value"] = [avg_temp] * 5

            # Set tsfc
            if "tsfc" in surf and isinstance(surf["tsfc"], dict):
                surf["tsfc"]["value"] = avg_temp

            # Set tin
            if "tin" in surf and isinstance(surf["tin"], dict):
                surf["tin"]["value"] = avg_temp

            initial_states[surface_type] = surf

        # Save back
        site["initial_states"] = initial_states
        data["sites"][site_idx] = site

    return data


def precheck_thermal_layer_cp_renaming(data: dict) -> dict:
    """
    Rename legacy 'cp' field to 'rho_cp' in thermal_layers for all surface types.

    This function scans both land_cover surface types and vertical_layers for
    thermal_layers that contain the legacy 'cp' field and automatically renames
    it to 'rho_cp', which is the correct field name for volumetric heat capacity.

    For each site:
    - Loops through all surface types under 'land_cover'.
    - Loops through all vertical layer items under 'vertical_layers'.
    - If thermal_layers contain a 'cp' field:
        - Renames 'cp' to 'rho_cp'
        - Logs an informative message about the change
        - Preserves all other thermal_layers data unchanged

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: Updated YAML dictionary with cp fields renamed to rho_cp.
    """

    total_renames = 0

    for site_idx, site in enumerate(data.get("sites", [])):
        properties = site.get("properties", {})

        # Process land_cover thermal_layers
        land_cover = properties.get("land_cover", {})
        for surf_type, props in land_cover.items():
            if not isinstance(props, dict):
                continue

            thermal_layers = props.get("thermal_layers")
            if isinstance(thermal_layers, dict) and "cp" in thermal_layers:
                # Rename cp to rho_cp
                thermal_layers["rho_cp"] = thermal_layers.pop("cp")
                total_renames += 1

                logger_supy.info(
                    f"[site #{site_idx}] Renamed '{surf_type}.thermal_layers.cp' → "
                    f"'{surf_type}.thermal_layers.rho_cp' (legacy field name updated)"
                )

        # Process vertical_layers thermal_layers
        vertical_layers = properties.get("vertical_layers", {})
        if isinstance(vertical_layers, dict):
            # Process roofs, walls, and other array structures within vertical_layers
            for structure_name in ["roofs", "walls"]:  # Add more as needed
                structure_array = vertical_layers.get(structure_name, [])
                if isinstance(structure_array, list):
                    for item_idx, item in enumerate(structure_array):
                        if not isinstance(item, dict):
                            continue

                        thermal_layers = item.get("thermal_layers")
                        if isinstance(thermal_layers, dict) and "cp" in thermal_layers:
                            # Rename cp to rho_cp
                            thermal_layers["rho_cp"] = thermal_layers.pop("cp")
                            total_renames += 1

                            logger_supy.info(
                                f"[site #{site_idx}] Renamed 'vertical_layers.{structure_name}[{item_idx}].thermal_layers.cp' → "
                                f"'vertical_layers.{structure_name}[{item_idx}].thermal_layers.rho_cp' (legacy field name updated)"
                            )

    if total_renames > 0:
        logger_supy.info(
            f"[precheck] Automatically renamed {total_renames} legacy 'cp' field(s) to 'rho_cp' "
            f"in thermal_layers. The 'cp' field name is deprecated - use 'rho_cp' for "
            f"volumetric heat capacity (J/m³/K) in future configurations."
        )

    return data


def precheck_land_cover_fractions(data: dict) -> dict:
    """
    Validate and adjust land cover surface fractions (`sfr`) for each site.

    For each site in the configuration, this function:

    - Calculates the total sum of all surface fractions (`sfr` values) across land cover types.
    - Allows small floating point inaccuracies (~0.0001):
        - If the total is slightly below 1.0 (e.g., 0.9999 ≤ sum < 1.0), it auto-increases the largest surface fraction to force the sum to exactly 1.0.
        - If the total is slightly above 1.0 (e.g., 1.0 < sum ≤ 1.0001), it auto-decreases the largest surface fraction to force the sum to exactly 1.0.
    - If the total `sfr` differs from 1.0 by more than the allowed epsilon, raises an error.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: The updated YAML dictionary with corrected `sfr` sums.

    Raises:
        ValueError: If land cover fractions sum too low or too high beyond the allowed tolerance.
    """

    for i, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})

        land_cover = props.get("land_cover")
        if not land_cover:
            raise ValueError(f"[site #{i}] Missing land_cover block.")

        # Calculate sum of all non-null surface fractions
        sfr_sum = sum(
            get_value_safe(v, "sfr", 0)
            for v in land_cover.values()
            if isinstance(v, dict) and get_value_safe(v, "sfr") is not None
        )

        logger_supy.debug(f"[site #{i}] Total land_cover sfr sum: {sfr_sum:.6f}")

        # Auto-fix tiny floating point errors (epsilon ~0.0001)
        if 0.9999 <= sfr_sum < 1.0:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if get_value_safe(v, "sfr") is not None
                ),
                key=lambda k: get_value_safe(land_cover[k], "sfr"),
            )
            correction = 1.0 - sfr_sum
            # Handle both RefValue and plain formats for writing
            if isinstance(land_cover[max_key].get("sfr"), dict):
                land_cover[max_key]["sfr"]["value"] += correction  # RefValue format
            else:
                land_cover[max_key]["sfr"] += correction  # Plain format
            logger_supy.info(
                f"[site #{i}] Adjusted {max_key}.sfr up by {correction:.6f} to reach 1.0"
            )

        elif 1.0 < sfr_sum <= 1.0001:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if get_value_safe(v, "sfr") is not None
                ),
                key=lambda k: get_value_safe(land_cover[k], "sfr"),
            )
            correction = sfr_sum - 1.0
            # Handle both RefValue and plain formats for writing
            if isinstance(land_cover[max_key].get("sfr"), dict):
                land_cover[max_key]["sfr"]["value"] -= correction  # RefValue format
            else:
                land_cover[max_key]["sfr"] -= correction  # Plain format
            logger_supy.info(
                f"[site #{i}] Adjusted {max_key}.sfr down by {correction:.6f} to reach 1.0"
            )

        elif abs(sfr_sum - 1.0) > 0.0001:
            raise ValueError(f"[site #{i}] Invalid land_cover sfr sum: {sfr_sum:.6f}")

        site["properties"] = props

    return data


def precheck_nullify_zero_sfr_params(data: dict) -> dict:
    """
    Nullify all land cover parameters for surface types with zero surface fraction (sfr == 0).

    For each site:
    - Loops through all surface types under 'land_cover'.
    - If a surface type has sfr == 0:
        - Sets all associated parameters (except 'sfr') to None.
        - This includes both single-value parameters and nested structures (e.g., thermal_layers, ohm_coef).
        - For list-valued parameters, replaces each element with None.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: Updated YAML dictionary with unused surface type parameters nullified.
    """

    for site_idx, site in enumerate(data.get("sites", [])):
        land_cover = site.get("properties", {}).get("land_cover", {})
        for surf_type, props in land_cover.items():
            sfr = get_value_safe(props, "sfr", 0)
            if sfr == 0:
                logger_supy.info(
                    f"[site #{site_idx}] Nullifying params for surface '{surf_type}' with sfr == 0"
                )
                for param_key, param_val in props.items():
                    if param_key == "sfr":
                        continue
                    # Nullify simple params
                    if isinstance(param_val, dict) and "value" in param_val:
                        param_val["value"] = None
                    # Nullify nested blocks (like ohm_coef, thermal_layers etc)
                    elif isinstance(param_val, dict):

                        def recursive_nullify(d):
                            for k, v in d.items():
                                if isinstance(v, dict):
                                    if "value" in v:
                                        if isinstance(v["value"], list):
                                            v["value"] = [None] * len(v["value"])
                                        else:
                                            v["value"] = None
                                    else:
                                        recursive_nullify(v)

                        recursive_nullify(param_val)
    return data


def precheck_warn_zero_sfr_params(data: dict) -> dict:
    """
    Log an informational warning listing all land cover parameters that were not prechecked for surfaces with zero surface fraction (sfr == 0).

    For each site:
    - Scans all surface types under 'land_cover'.
    - If a surface type has sfr == 0:
        - Collects the names of all parameters (including nested ones) defined under that surface type.
        - Logs a compact info message listing these parameters, warning that they have not been physically prechecked.

    Note:
        This function does not modify the input data.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: The original, unmodified YAML dictionary.
    """
    for site_idx, site in enumerate(data.get("sites", [])):
        land_cover = site.get("properties", {}).get("land_cover", {})
        for surf_type, props in land_cover.items():
            sfr = get_value_safe(props, "sfr", 0)
            if sfr == 0:
                param_list = []

                def collect_param_names(d: dict, prefix: str = ""):
                    for k, v in d.items():
                        if k == "sfr":
                            continue
                        current_path = f"{prefix}.{k}" if prefix else k
                        if isinstance(v, dict):
                            if "value" in v:
                                param_list.append(current_path)
                            else:
                                collect_param_names(v, current_path)

                collect_param_names(props)

                if param_list:
                    param_str = "', '".join(param_list)
                    logger_supy.info(
                        f"[site #{site_idx}] As '{surf_type}' (sfr == 0), the following parameters are not prechecked for this surface type : '{param_str}'"
                    )

    return data


def precheck_nonzero_sfr_requires_nonnull_params(data: dict) -> dict:
    """
    Validate that all parameters for land cover surfaces with nonzero surface fraction (sfr > 0) are set and non-null.

    For each site:
    - Iterates over all surface types in 'land_cover'.
    - For each surface where sfr > 0:
        - Recursively checks that all associated parameters (except 'sfr') are:
            - Not None
            - Not empty strings
            - For lists: do not contain None or empty string elements

    If any required parameter is unset (None or empty), the function raises a ValueError with details.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: The validated YAML dictionary (unchanged if all checks pass).

    Raises:
        ValueError: If any required parameter for a nonzero-sfr surface is unset or empty.
    """

    def check_recursively(d: dict, path: list, site_idx: int):
        if isinstance(d, dict):
            if "value" in d:
                val = d["value"]
                if val in (None, "") or (
                    isinstance(val, list) and any(v in (None, "") for v in val)
                ):
                    full_path = ".".join(path)
                    raise ValueError(
                        f"[site #{site_idx}] land_cover.{full_path} must be set (not None or empty) "
                        f"because {path[0]}.sfr > 0"
                    )
            else:
                for k, v in d.items():
                    check_recursively(v, path + [k], site_idx)

        elif isinstance(d, list):
            for idx, item in enumerate(d):
                check_recursively(item, path + [f"[{idx}]"], site_idx)

    for site_idx, site in enumerate(data.get("sites", [])):
        land_cover = site.get("properties", {}).get("land_cover", {})
        for surf_type, props in land_cover.items():
            sfr = get_value_safe(props, "sfr", 0)
            if sfr > 0:
                for param_key, param_val in props.items():
                    if param_key == "sfr":
                        continue
                    check_recursively(
                        param_val, path=[surf_type, param_key], site_idx=site_idx
                    )

    logger_supy.info(
        "[precheck] Nonzero sfr parameters validated (all required fields are set)."
    )
    return data


def precheck_model_option_rules(data: dict) -> dict:
    """
    If a method is switched off, recursively nullify all site-level methods parameters.

    Args:
        data (dict): YAML configuration data loaded as a dict.

    Returns:
        dict: The updated YAML dict after applying the STEBBS nullification rule.
    """
    physics = data.get("model", {}).get("physics", {})

    # --- STEBBSMETHOD RULE: when stebbsmethod == 0, wipe out all stebbs params ---
    stebbsmethod = get_value_safe(physics, "stebbsmethod")
    if stebbsmethod == 0:
        logger_supy.info(
            "[precheck] stebbsmethod==0 detected → nullifying all 'stebbs' values."
        )
        for site_idx, site in enumerate(data.get("sites", [])):
            props = site.get("properties", {})
            stebbs_block = props.get("stebbs", {})

            def _recursive_nullify(block: dict):
                for key, val in block.items():
                    if isinstance(val, dict):
                        if "value" in val:
                            val["value"] = None
                        else:
                            _recursive_nullify(val)

            _recursive_nullify(stebbs_block)
            props["stebbs"] = stebbs_block

    logger_supy.info("[precheck] STEBBS nullification complete.")
    return data


def run_precheck(path: str) -> dict:
    """
    Perform full preprocessing (precheck) on a YAML configuration file.

    This function runs the complete SUEWS precheck pipeline, applying a sequence of
    automated corrections, defaults, nullifications, and consistency checks to a YAML
    configuration file before Pydantic validation.

    The steps include:
    1. Loading the YAML file into a Python dictionary.
    2. Extracting simulation dates and model year.
    3. Validating and completing `model.physics` parameters.
    4. Enforcing constraints between model physics options.
    5. Replacing empty strings with `None` (except in `model.control` and `model.physics`).
    6. Renaming legacy 'cp' field to 'rho_cp' in thermal_layers for all surface types.
    7. Applying site-specific seasonal and location-based adjustments (e.g., LAI, snowalb, DLS).
    8. Setting initial surface temperatures based on latitude and month.
    9. Logging warnings for parameters of surfaces with `sfr == 0` that were not prechecked.
    10. Validating that parameters for surfaces with `sfr > 0` are not empty or null.
    11. Checking and auto-fixing small floating point errors in land cover surface fractions.
    12. Nullify model-option-dependent parameters if specific models are switched off
    13. Saving the updated YAML to a new file (prefixed with `py0_`).
    14. Writing a CSV diff report listing all changes made.
    15. Logging completion.

    Args:
        path (str): Full path to the input YAML configuration file.

    Returns:
        dict: The fully prechecked and updated YAML configuration dictionary.
    """

    # ---- Step 0: Load yaml from path into a dict ----
    with open(path, "r") as file:
        data = yaml.load(file, Loader=yaml.FullLoader)

    original_data = deepcopy(data)

    # ---- Step 1: Print start message ----
    data = precheck_printing(data)

    # ---- Step 2: Extract start_date, end_date, model_year ----
    data, model_year, start_date, end_date = precheck_start_end_date(data)
    logger_supy.debug(
        f"Start date: {start_date}, end date: {end_date}, year: {model_year}"
    )

    # ---- Step 3: Check model.physics parameters ----
    data = precheck_model_physics_params(data)

    # ---- Step 4: Enforce model option constraints ----
    data = precheck_model_options_constraints(data)

    # ---- Step 5: Clean empty strings (except model.control and model.physics) ----
    data = precheck_replace_empty_strings_with_none(data)

    # ---- Step 6: Rename legacy 'cp' to 'rho_cp' in thermal_layers ----
    data = precheck_thermal_layer_cp_renaming(data)

    # ---- Step 7: Season + LAI + DLS adjustments per site ----
    data = precheck_site_season_adjustments(
        data, start_date=start_date, model_year=model_year
    )

    # ---- Step 8: Update temperatures using CRU mean monthly air temperature ----
    data = precheck_update_temperature(data, start_date=start_date)

    # ---- Step 9: Print warnings for params related to surfaces with sfr == 0 ----
    data = precheck_warn_zero_sfr_params(data)

    # ---- Step 10: Check existence of params for surfaces with sfr > 0 ----
    data = precheck_nonzero_sfr_requires_nonnull_params(data)

    # ---- Step 11: Land Cover Fractions checks & adjustments ----
    data = precheck_land_cover_fractions(data)

    # ---- Step 12: Rules associated to selected model options ----
    data = precheck_model_option_rules(data)

    # ---- Step 13: Save output YAML ----
    output_filename = f"py0_{os.path.basename(path)}"
    output_path = os.path.join(os.path.dirname(path), output_filename)

    with open(output_path, "w") as f:
        yaml.dump(data, f, sort_keys=False, allow_unicode=True)

    logger_supy.info(f"Saved updated YAML file to: {output_path}")

    # ---- Step 14: Generate precheck diff report CSV ----
    diffs = collect_yaml_differences(original_data, data)
    save_precheck_diff_report(diffs, path)

    # ---- Step 15: Print completion ----
    logger_supy.info("Precheck complete.\n")
    return data
