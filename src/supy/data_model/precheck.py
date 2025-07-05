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
from timezonefinder import TimezoneFinder
import pytz
from .._env import logger_supy
import os


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
                "reason": "Updated by precheck"
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
        writer = csv.DictWriter(csvfile, fieldnames=["site", "parameter", "old_value", "new_value", "reason"])
        writer.writeheader()
        for row in diffs:
            for key in ["old_value", "new_value"]:
                if row[key] is None:
                    row[key] = "null"
            writer.writerow(row)

    logger_supy.info(f"Precheck difference report saved to: {report_path}")

def get_monthly_avg_temp(lat: float, month: int) -> float:

    """
    Estimate the average air temperature for a given latitude and month.

    This function uses predefined climatological values for four broad latitude bands:
    - Tropics (|lat| < 10°)
    - Subtropics (10° ≤ |lat| < 35°)
    - Midlatitudes (35° ≤ |lat| < 60°)
    - Polar regions (|lat| ≥ 60°)

    The returned value represents a typical monthly average temperature (°C)
    for the specified latitude band and month.

    Args:
        lat (float): Site latitude in degrees (positive for Northern Hemisphere, negative for Southern).
        month (int): Month of the year (1 = January, 12 = December).

    Returns:
        float: Estimated average air temperature for the given latitude and month.

    Raises:
        ValueError: If the input month is not between 1 and 12.
    """

    lat_band = None
    abs_lat = abs(lat)

    if abs_lat < 10:
        lat_band = "tropics"
    elif abs_lat < 35:
        lat_band = "subtropics"
    elif abs_lat < 60:
        lat_band = "midlatitudes"
    else:
        lat_band = "polar"

    monthly_temp = {
        "tropics": [26.0, 26.5, 27.0, 27.5, 28.0, 28.5, 28.0, 27.5, 27.0, 26.5, 26.0, 25.5],
        "subtropics": [15.0, 16.0, 18.0, 20.0, 24.0, 28.0, 30.0, 29.0, 26.0, 22.0, 18.0, 15.0],
        "midlatitudes": [5.0, 6.0, 9.0, 12.0, 17.0, 21.0, 23.0, 22.0, 19.0, 14.0, 9.0, 6.0],
        "polar": [-15.0, -13.0, -10.0, -5.0, 0.0, 5.0, 8.0, 7.0, 3.0, -2.0, -8.0, -12.0],
    }

    return monthly_temp[lat_band][month - 1]

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

    empty = [k for k in required if physics.get(k, {}).get("value") in ("", None)]
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

    diag = physics.get("rslmethod", {}).get("value")
    stability = physics.get("stabilitymethod", {}).get("value")

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
        lng = props.get("lng", {}).get("value")
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
        sfr = dectr.get("sfr", {}).get("value", 0)

        if sfr > 0:
            lai = dectr.get("lai", {})
            laimin = lai.get("laimin", {}).get("value")
            laimax = lai.get("laimax", {}).get("value")
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
                raise ValueError(f"[site #{i}] DLSCheck failed: {e}")

        # Final update
        site["properties"] = props
        site["initial_states"] = initial_states
        cleaned_sites.append(site)

    data["sites"] = cleaned_sites
    return data

def precheck_update_surface_temperature(data: dict, start_date: str) -> dict:

    """
    Set initial surface temperatures for all surface types based on latitude and start month.

    For each site:
    - Uses the site's latitude and the month from start_date to estimate an average temperature.
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
            logger_supy.warning(f"[site #{site_idx}] Latitude missing, skipping surface temperature update.")
            continue

        # Get estimated average temperature
        avg_temp = get_monthly_avg_temp(lat, month)
        logger_supy.info(f"[site #{site_idx}] Setting surface temperatures to {avg_temp} °C for month {month} (lat={lat})")

        # Loop over all surface types
        for surface_type in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]:
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
            v.get("sfr", {}).get("value", 0)
            for v in land_cover.values()
            if isinstance(v, dict) and v.get("sfr", {}).get("value") is not None
        )

        logger_supy.debug(f"[site #{i}] Total land_cover sfr sum: {sfr_sum:.6f}")

        # Auto-fix tiny floating point errors (epsilon ~0.0001)
        if 0.9999 <= sfr_sum < 1.0:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if v.get("sfr", {}).get("value") is not None
                ),
                key=lambda k: land_cover[k]["sfr"]["value"],
            )
            correction = 1.0 - sfr_sum
            land_cover[max_key]["sfr"]["value"] += correction
            logger_supy.info(
                f"[site #{i}] Adjusted {max_key}.sfr up by {correction:.6f} to reach 1.0"
            )

        elif 1.0 < sfr_sum <= 1.0001:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if v.get("sfr", {}).get("value") is not None
                ),
                key=lambda k: land_cover[k]["sfr"]["value"],
            )
            correction = sfr_sum - 1.0
            land_cover[max_key]["sfr"]["value"] -= correction
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
            sfr = props.get("sfr", {}).get("value", 0)
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
            sfr = props.get("sfr", {}).get("value", 0)
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
            sfr = props.get("sfr", {}).get("value", 0)
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
    Apply model-option-dependent validation rules and parameter adjustments based on model physics settings.

    For each site, this function applies checks and actions depending on selected model options in `model.physics`:

    - **If `rslmethod == 2` (diagnostic method enabled):**
        - For any site where `bldgs.sfr > 0`, verifies that `faibldg` is set and non-null.

    - **If `storageheatmethod == 6` (DyOHM method):**
        - Verifies that `vertical_layers.walls` exists and contains at least one wall.
        - Checks that the first wall has non-empty lists for `dz`, `k`, and `cp` in `thermal_layers`.
        - Verifies that `lambda_c` is set and non-null.

    - **If `stebbsmethod == 0`:**
        - Recursively nullifies all parameters under the `stebbs` block at site level.

    Args:
        data (dict): YAML configuration data loaded as a dictionary.

    Returns:
        dict: The updated YAML dictionary after applying model-option rules.

    Raises:
        ValueError: If any required condition based on model options is violated.
    """

    physics = data.get("model", {}).get("physics", {})
    rslmethod = physics.get("rslmethod", {}).get("value")
    storagemethod = physics.get("storageheatmethod", {}).get("value")
    stebbsmethod = physics.get("stebbsmethod", {}).get("value")

    # --- RSLMETHOD RULES (diagnostic method logic) ---
    if rslmethod == 2:
        logger_supy.info("[precheck] rslmethod==2 detected → checking faibldg for bldgs with sfr > 0.")

        for site_idx, site in enumerate(data.get("sites", [])):
            props = site.get("properties", {})
            land_cover = props.get("land_cover", {})
            bldgs = land_cover.get("bldgs", {})
            sfr = bldgs.get("sfr", {}).get("value", 0)

            if sfr > 0:
                faibldg = bldgs.get("faibldg", {})
                faibldg_value = faibldg.get("value")
                if faibldg_value in (None, "", []):
                    raise ValueError(f"[site #{site_idx}] For rslmethod==2 and bldgs.sfr > 0, faibldg must be set and non-null.")

    # --- STORAGEHEATMETHOD RULES (DyOHM logic) ---
    if storagemethod == 6:
        logger_supy.info("[precheck] storageheatmethod==6 detected → checking wall thermal layers and lambda_c.")

        for site_idx, site in enumerate(data.get("sites", [])):
            props = site.get("properties", {})
            vertical_layers = props.get("vertical_layers", {})
            walls = vertical_layers.get("walls", [])

            if not walls or not isinstance(walls, list) or len(walls) == 0:
                raise ValueError(f"[site #{site_idx}] Missing vertical_layers.walls for storageheatmethod == 6.")

            wall0 = walls[0]
            thermal = wall0.get("thermal_layers", {})

            for param in ["dz", "k", "cp"]:
                param_list = thermal.get(param, {}).get("value")
                if not isinstance(param_list, list) or len(param_list) == 0:
                    raise ValueError(f"[site #{site_idx}] Missing wall thermal_layers.{param} for storageheatmethod == 6.")
                if param_list[0] in (None, ""):
                    raise ValueError(f"[site #{site_idx}] wall thermal_layers.{param}[0] must be set for storageheatmethod == 6.")

            lambda_c = props.get("lambda_c", {}).get("value")
            if lambda_c in (None, ""):
                raise ValueError(f"[site #{site_idx}] properties.lambda_c must be set for storageheatmethod == 6.")

    # --- STEBBSMETHOD RULES ---
    if stebbsmethod == 0:
        logger_supy.info("[precheck] stebbsmethod==0 detected → nullifying stebbs parameters at site level.")

        for site_idx, site in enumerate(data.get("sites", [])):
            props = site.get("properties", {})
            stebbs_block = props.get("stebbs", {})

            def recursive_nullify(d):
                for k, v in d.items():
                    if isinstance(v, dict):
                        if "value" in v:
                            v["value"] = None
                        else:
                            recursive_nullify(v)

            recursive_nullify(stebbs_block)
            site["properties"]["stebbs"] = stebbs_block

    logger_supy.info("[precheck] Model-option-based rules completed.")
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
    6. Applying site-specific seasonal and location-based adjustments (e.g., LAI, snowalb, DLS).
    7. Setting initial surface temperatures based on latitude and month.
    8. Logging warnings for parameters of surfaces with `sfr == 0` that were not prechecked.
    9. Validating that parameters for surfaces with `sfr > 0` are not empty or null.
    10. Checking and auto-fixing small floating point errors in land cover surface fractions.
    11. Applying model-option-dependent rules (e.g., RSL, DyOHM, Stebbs).
    12. Saving the updated YAML to a new file (prefixed with `py0_`).
    13. Writing a CSV diff report listing all changes made.
    14. Logging completion.

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

    # ---- Step 6: Season + LAI + DLS adjustments per site ----
    data = precheck_site_season_adjustments(
        data, start_date=start_date, model_year=model_year
    )

    # ---- Step 7: Update surface temperatures from lat/month ----
    data = precheck_update_surface_temperature(data, start_date=start_date)

    # ---- Step 8: Nullify params for surfaces with sfr == 0 ----
    #data = precheck_nullify_zero_sfr_params(data)

    # ---- Step 8: Print warnings for params related to surfaces with sfr == 0 ----
    data = precheck_warn_zero_sfr_params(data)

    # ---- Step 9: Check existence of params for surfaces with sfr > 0 ----
    data = precheck_nonzero_sfr_requires_nonnull_params(data)

    # ---- Step 10: Land Cover Fractions checks & adjustments ----
    data = precheck_land_cover_fractions(data)

    # ---- Step 11: Rules associated to selected model options ----
    data = precheck_model_option_rules(data)

    # ---- Step 12: Save output YAML ----
    output_filename = f"py0_{os.path.basename(path)}"
    output_path = os.path.join(os.path.dirname(path), output_filename)

    with open(output_path, "w") as f:
        yaml.dump(data, f, sort_keys=False, allow_unicode=True)

    logger_supy.info(f"Saved updated YAML file to: {output_path}")

    # ---- Step 13: Generate precheck diff report CSV ----
    diffs = collect_yaml_differences(original_data, data)
    save_precheck_diff_report(diffs, path)

    # ---- Step 14: Print completion ----
    logger_supy.info("Precheck complete.\n")
    return data
