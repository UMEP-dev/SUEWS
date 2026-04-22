"""
SUEWS Physics Validation Check - Phase B

This module performs physics validation and consistency checks on YAML configurations
that have already been processed by Phase A.

Phase B focuses on:
- Physics parameter validation
- Geographic coordinate and timezone validation
- Seasonal parameter adjustments (LAI, snowalb, surface temperatures)
- Land cover fraction validation and consistency
- Model physics option interdependency checks
- Automatic physics-based corrections where appropriate

Phase B assumes Phase A has completed successfully and builds upon clean YAML output
without duplicating parameter detection or YAML structure validation.
"""

import math
import yaml
import os
import calendar
from typing import Dict, List, Optional, Union, Any, Tuple
from dataclasses import dataclass
from datetime import datetime
from copy import deepcopy
import pandas as pd
import numpy as np

# Import from validation package
from .. import logger_supy
from .report_writer import REPORT_WRITER
from ..core.yaml_helpers import (
    get_mean_monthly_air_temperature as _get_mean_monthly_air_temperature,
    get_monthly_air_temperature_diffmax as _get_monthly_air_temperature_diffmax,
    get_mean_annual_air_temperature as _get_mean_annual_air_temperature,
    nullify_co2_block_recursive,
    collect_nullified_paths,
    _nullify_biogenic_in_props,
    DLSCheck,
    get_value_safe,
    HAS_TIMEZONE_FINDER,
)

from .phase_b_rules import (
    RulesRegistry,
    ValidationResult,
    ValidationContext,
    SFR_FRACTION_TOL,
)

@dataclass
class ScientificAdjustment:
    """
    Record of automatic scientific adjustment applied.

    Attributes
    ----------
    parameter : str
        Name of the parameter that was adjusted.
    site_index : Optional[int], default=None
        Array index of the site (for internal use).
    site_gridid : Optional[int], default=None
        GRIDID value of the site (for display purposes).
    old_value : Any, default=None
        The original value of the parameter before adjustment.
    new_value : Any, default=None
        The new value of the parameter after adjustment.
    reason : str, default=""
        Description or reason for the adjustment.
    """

    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


def get_site_gridid(site_data: dict) -> int:
    """
    Extract GRIDID from site data, handling both direct and RefValue formats.

    Parameters
    ----------
    site_data : dict
        Dictionary containing site information, which may include a 'gridiv' key.

    Returns
    -------
    int or None
        The GRIDID value if found, otherwise None.
    """
    if isinstance(site_data, dict):
        gridiv = site_data.get("gridiv")
        if isinstance(gridiv, dict) and "value" in gridiv:
            return gridiv["value"]
        elif gridiv is not None:
            return gridiv
    return None


def validate_phase_b_inputs(
    uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str
) -> Tuple[dict, dict, dict]:
    """
    Validate Phase B inputs and load YAML files.

    Checks the existence of the provided YAML files, loads their contents, and parses them into dictionaries.
    Raises an error if any file is missing or if the YAML format is invalid.

    Parameters
    ----------
    uptodate_yaml_file : str
        Path to the "up to date" YAML file.
    user_yaml_file : str
        Path to the user-provided YAML file.
    standard_yaml_file : str
        Path to the standard YAML file.

    Returns
    -------
    tuple of dict
        A tuple containing three dictionaries: (uptodate_data, user_data, standard_data), each corresponding to the loaded contents of the respective YAML files.

    Raises
    ------
    FileNotFoundError
        If any of the specified YAML files do not exist.
    ValueError
        If any of the YAML files contain invalid YAML syntax.
    """
    for file_path in [uptodate_yaml_file, user_yaml_file, standard_yaml_file]:
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"Required file not found: {file_path}")

    try:
        with open(uptodate_yaml_file, "r") as f:
            uptodate_content = f.read()
            uptodate_data = yaml.safe_load(uptodate_content)

        is_phase_a_output = "UP TO DATE YAML" in uptodate_content

        with open(user_yaml_file, "r") as f:
            user_data = yaml.safe_load(f)

        with open(standard_yaml_file, "r") as f:
            standard_data = yaml.safe_load(f)

    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}")

    return uptodate_data, user_data, standard_data


def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """
    Extracts simulation parameters for validation from a nested YAML data dictionary.

    Parameters
    ----------
    yaml_data : dict
        Dictionary containing the parsed YAML data, expected to have the structure:
        {'model': {'control': {'start_time': str, 'end_time': str}}}

    Returns
    -------
    Tuple[int, str, str]
        A tuple containing:
            - model_year (int): The year extracted from 'start_time'.
            - start_date (str): The simulation start date in 'YYYY-MM-DD' format.
            - end_date (str): The simulation end date in 'YYYY-MM-DD' format.

    Raises
    ------
    ValueError
        If 'start_time' or 'end_time' are missing or not in the expected 'YYYY-MM-DD' format,
        or if the model year cannot be extracted from 'start_time'.

    Notes
    -----
    All validation errors are collected and reported together in the exception message.
    """
    control = yaml_data.get("model", {}).get("control", {})

    start_date = control.get("start_time")
    end_date = control.get("end_time")

    # Collect all validation errors instead of failing on first error
    errors = []

    if not isinstance(start_date, str) or "-" not in str(start_date):
        errors.append(
            "Missing or invalid 'start_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    if not isinstance(end_date, str) or "-" not in str(end_date):
        errors.append(
            "Missing or invalid 'end_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    # Try to extract model year if start_date looks valid
    model_year = None
    if isinstance(start_date, str) and "-" in str(start_date):
        try:
            model_year = int(start_date.split("-")[0])
        except Exception:
            errors.append(
                "Could not extract model year from 'start_time' - ensure 'YYYY-MM-DD' format"
            )

    # If we have errors, combine them into a single error message
    if errors:
        error_msg = "; ".join(errors)
        raise ValueError(error_msg)

    return model_year, start_date, end_date


def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """
    Check for missing or empty parameters in a surface configuration.

    Parameters
    ----------
    surface_props : dict
        Dictionary containing the properties of a surface.
    surface_type : str
        The type of surface being checked (e.g., 'paved', 'grass').

    Returns
    -------
    List[str]
        List of parameter paths that are missing or empty in the surface configuration.
    """
    missing_params = []

    def _check_recursively(props: dict, path: str = ""):
        for key, value in props.items():
            if key == "sfr":
                continue

            current_path = f"{path}.{key}" if path else key

            if isinstance(value, dict):
                if "value" in value:
                    param_value = value["value"]
                    if param_value in (None, "") or (
                        isinstance(param_value, list)
                        and any(v in (None, "") for v in param_value)
                    ):
                        missing_params.append(current_path)
                else:
                    _check_recursively(value, current_path)

    _check_recursively(surface_props)
    return missing_params


def validate_geographic_parameters(yaml_data: dict) -> List[ValidationResult]:
    """
    Perform comprehensive validation of geographic coordinates and related parameters.

    This function checks for the presence, type, and valid range of latitude and longitude
    for each site, as well as the presence of timezone and daylight saving parameters.
    It provides detailed error and warning messages to guide users in correcting issues.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing site configurations.

    Returns
    -------
    List[ValidationResult]
        List of validation results for all geographic parameters, including errors,
        warnings, and a pass message if all checks succeed.
    """
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        site_gridid = get_site_gridid(site)

        lat = get_value_safe(props, "lat")

        if lat is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Latitude is missing or null",
                    suggested_value="Set latitude value between -90 and 90 degrees",
                )
            )
        elif not isinstance(lat, (int, float)):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Latitude must be a numeric value",
                    suggested_value="Set latitude as a number between -90 and 90 degrees",
                )
            )
        elif not (-90 <= lat <= 90):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=f"Latitude {lat} is outside valid range [-90, 90]",
                    suggested_value="Set latitude between -90 and 90 degrees",
                )
            )

        lng = get_value_safe(props, "lng")

        if lng is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Longitude is missing or null",
                    suggested_value="Set longitude value between -180 and 180 degrees",
                )
            )
        elif not isinstance(lng, (int, float)):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Longitude must be a numeric value",
                    suggested_value="Set longitude as a number between -180 and 180 degrees",
                )
            )
        elif not (-180 <= lng <= 180):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=f"Longitude {lng} is outside valid range [-180, 180]",
                    suggested_value="Set longitude between -180 and 180 degrees",
                )
            )

        timezone = get_value_safe(props, "timezone")

        if timezone is None:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="GEOGRAPHY",
                    parameter="timezone",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Timezone parameter is missing - will be calculated automatically from latitude and longitude (see updated parameters)",
                    suggested_value="Timezone will be set based on your coordinates. You can also manually set the timezone value if you prefer a specific UTC offset",
                )
            )

        anthro_emissions = props.get("anthropogenic_emissions", {})
        if anthro_emissions:
            startdls = get_value_safe(anthro_emissions, "startdls")
            enddls = get_value_safe(anthro_emissions, "enddls")

            if startdls is None or enddls is None:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="GEOGRAPHY",
                        parameter="anthropogenic_emissions.startdls,enddls",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="Daylight saving parameters (startdls, enddls) are missing - will be calculated automatically from geographic coordinates (see updated parameters)",
                        suggested_value="Parameters will be set based on your location. You can also manually set startdls and enddls if you prefer specific values",
                    )
                )

    error_count = sum(1 for r in results if r.status == "ERROR")
    if error_count == 0:
        results.append(
            ValidationResult(
                status="PASS",
                category="GEOGRAPHY",
                parameter="geographic_coordinates",
                message="Geographic coordinates validated successfully",
            )
        )

    return results


def validate_irrigation_doy(
    ie_start: Optional[float],
    ie_end: Optional[float],
    lat: float,
    model_year: int,
    site_name: str,
) -> List[ValidationResult]:
    """
    Validate irrigation DOY parameters with leap year, hemisphere, and tropical awareness.

    Parameters
    ----------
    ie_start : Optional[float]
        Irrigation start day of year.
    ie_end : Optional[float]
        Irrigation end day of year.
    lat : float
        Site latitude (for hemisphere and tropical detection).
    model_year : int
        Simulation year (for leap year detection).
    site_name : str
        Site identifier for error messages.

    Returns
    -------
    List[ValidationResult]
        List of ValidationResult objects (errors, warnings, or empty).

    Notes
    -----
    This validator ensures irrigation timing parameters are logically consistent
    and appropriate for the site's location. It checks:
    1. DOY values are within valid range (1-365 or 1-366 for leap years)
    2. Both parameters are set together or both disabled
    3. Warm season appropriateness based on hemisphere and latitude:
       - Tropical regions (|lat| < 23.5): No restrictions, irrigation allowed year-round
       - Northern Hemisphere (lat >= 23.5): Warm season is May-September (DOY 121-273)
       - Southern Hemisphere (lat <= -23.5): Warm season is November-March (DOY 305-90)
    """
    results = []

    # Helper: treat 0 and None as equivalent (both mean "disabled")
    def is_disabled(value):
        return value is None or value == 0 or value == 0.0

    start_disabled = is_disabled(ie_start)
    end_disabled = is_disabled(ie_end)

    # Case 1: Both disabled = valid (irrigation not configured)
    if start_disabled and end_disabled:
        return results

    # Case 2: One disabled, one enabled = ERROR (inconsistent)
    if start_disabled != end_disabled:
        results.append(
            ValidationResult(
                status="ERROR",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message="Both ie_start and ie_end must be specified together (both set to valid DOY), "
                "or both left as None/0 (irrigation disabled). "
                f"Currently: ie_start={ie_start}, ie_end={ie_end}",
                suggested_value="Set both to valid DOY (1-365/366) or both to None/0",
            )
        )
        return results

    # Case 3: Both enabled = validate DOY range and hemisphere logic
    is_leap = calendar.isleap(model_year)
    max_doy = 366 if is_leap else 365

    # Validate DOY range (must be 1-365/366, not 0)
    for param_name, param_value in [("ie_start", ie_start), ("ie_end", ie_end)]:
        if param_value < 1 or param_value > max_doy:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="IRRIGATION",
                    parameter=f"{param_name} in site {site_name}",
                    message=f"{param_name}={param_value} is invalid. "
                    f"Must be between 1 and {max_doy} for {'leap' if is_leap else 'non-leap'} "
                    f"year {model_year}, or 0/None to disable irrigation",
                    suggested_value=f"Valid range: 1-{max_doy}, or 0/None to disable",
                )
            )

    # Seasonal validation based on hemisphere and latitude
    abs_lat = abs(lat)

    # Tropical regions (within Tropic of Cancer/Capricorn): irrigation allowed all year
    if abs_lat < 23.5:
        return results

    # Helper function to check if DOY is in warm season
    def is_in_warm_season(doy: float, latitude: float) -> bool:
        """
        Check if a DOY falls within the warm season for the given hemisphere.

        Parameters
        ----------
        doy : float
            Day of year (1-365/366).
        latitude : float
            Latitude in degrees.

        Returns
        -------
        bool
            True if the DOY is within the warm season for the hemisphere, False otherwise.

        Notes
        -----
        Warm season definitions:
        - Northern Hemisphere (lat >= 23.5): May-September (DOY 121-273)
        - Southern Hemisphere (lat <= -23.5): November-March (DOY 305-90, wraps year boundary)
        """
        if latitude >= 0:  # Northern Hemisphere
            # May to September (DOY 121-273)
            return 121 <= doy <= 273
        else:  # Southern Hemisphere
            # November to March (DOY 305-365/366 or 1-90)
            return doy >= 305 or doy <= 90

    # Check if ie_start and ie_end are within warm season
    start_in_warm_season = is_in_warm_season(ie_start, lat)
    end_in_warm_season = is_in_warm_season(ie_end, lat)

    if not start_in_warm_season or not end_in_warm_season:
        hemisphere = "Northern" if lat >= 0 else "Southern"
        season_range = (
            "May-September (DOY 121-273)" if lat >= 0 else "November-March (DOY 305-90)"
        )

        results.append(
            ValidationResult(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=f"Irrigation period (DOY {int(ie_start)} to {int(ie_end)}) falls outside "
                f"the typical warm season for {hemisphere} Hemisphere sites (lat={lat:.2f}). "
                f"Irrigation typically occurs during warm season: {season_range}. "
                f"Verify this configuration is intentional.",
                suggested_value=f"Consider adjusting to warm season: {season_range}",
            )
        )

    # Check for unusual year-wrapping irrigation patterns
    # This helps catch likely user errors where start/end are swapped
    if lat >= 23.5 and ie_start > ie_end:  # NH with year-wrapping
        results.append(
            ValidationResult(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=f"Irrigation period wraps year boundary (DOY {int(ie_start)} to {int(ie_end)}). "
                f"This means irrigation spans {365 - int(ie_start) + int(ie_end)} days including cold season. "
                f"Verify this is intentional for Northern Hemisphere.",
                suggested_value="Check if ie_start should be less than ie_end",
            )
        )
    elif lat <= -23.5 and ie_start < ie_end:  # SH without year-wrapping
        results.append(
            ValidationResult(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=f"Irrigation period does not wrap year boundary (DOY {int(ie_start)} to {int(ie_end)}). "
                f"In Southern Hemisphere, warm season typically spans November-March (year-wrapping).",
                suggested_value="Check if ie_start should be greater than ie_end",
            )
        )

    return results


def validate_irrigation_parameters(
    yaml_data: dict, model_year: int
) -> List[ValidationResult]:
    """
    Validate irrigation DOY parameters for all sites.

    Extracts irrigation parameters from each site configuration and validates
    them using context-aware checks (leap year, hemisphere).

    Parameters
    ----------
    yaml_data : dict
        Complete YAML configuration dictionary.
    model_year : int
        Simulation year for leap year detection.

    Returns
    -------
    List[ValidationResult]
        List of ValidationResult objects for all sites.
    """
    results = []
    sites = yaml_data.get("sites", [])

    # Handle both list and dict formats for sites
    if isinstance(sites, dict):
        # Dict format: {site_name: {lat: ..., ...}, ...}
        sites_list = [(site_name, site_data) for site_name, site_data in sites.items()]
    elif isinstance(sites, list):
        # List format: [{name: site_name, lat: ..., ...}, ...]
        sites_list = [
            (site.get("name", f"site_{idx}"), site) for idx, site in enumerate(sites)
        ]
    else:
        return results  # No valid sites structure

    for site_name, site_data in sites_list:
        # Extract latitude from properties
        properties = site_data.get("properties", {})
        lat = get_value_safe(properties, "lat", 0.0)

        # Extract irrigation parameters from properties
        irrigation = properties.get("irrigation", {})
        ie_start = get_value_safe(irrigation, "ie_start")
        ie_end = get_value_safe(irrigation, "ie_end")

        # Run validation
        results.extend(
            validate_irrigation_doy(ie_start, ie_end, lat, model_year, site_name)
        )

    return results


def check_missing_vegetation_albedo(yaml_data: dict) -> List[ValidationResult]:
    """
    Report when vegetated surfaces have null alb_id.

    This is informational: SUEWSConfig will auto-calculate alb_id from
    LAI state before the model run. Trees use a direct LAI-albedo
    relationship (higher LAI -> higher albedo); grass uses a reversed
    relationship (higher LAI -> lower albedo).

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing site configurations.

    Returns
    -------
    List[ValidationResult]
        List of informational ValidationResult objects for each vegetated surface
        with a null alb_id, indicating that alb_id will be auto-calculated.
    """
    results = []
    sites = yaml_data.get("sites", [])

    surface_labels = {
        "evetr": "evergreen trees",
        "dectr": "deciduous trees",
        "grass": "grass",
    }

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        land_cover = props.get("land_cover", {})
        site_gridid = get_site_gridid(site)

        for surf_key, surf_label in surface_labels.items():
            surf_props = land_cover.get(surf_key, {})
            surf_state = initial_states.get(surf_key, {})
            if not surf_props or not surf_state:
                continue

            # Check if surface has non-zero fraction
            sfr_entry = surf_props.get("sfr", {})
            sfr_val = (
                sfr_entry.get("value") if isinstance(sfr_entry, dict) else sfr_entry
            )
            if not sfr_val or sfr_val <= 0:
                continue

            # Check if alb_id is null
            alb_entry = surf_state.get("alb_id", {})
            alb_val = (
                alb_entry.get("value") if isinstance(alb_entry, dict) else alb_entry
            )
            if alb_val is not None:
                continue

            results.append(
                ValidationResult(
                    status="INFO",
                    category="SEASONAL",
                    parameter=f"{surf_key}.alb_id",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"alb_id is null for {surf_label} (sfr={sfr_val}). "
                        "It will be auto-calculated from LAI state during "
                        "SUEWSConfig construction"
                    ),
                )
            )

    return results


def get_mean_monthly_air_temperature(
    lat: float, lon: float, month: int, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate mean monthly air temperature using CRU TS4.06 climatological data.

    This function wraps `yaml_helpers.get_mean_monthly_air_temperature` and returns
    None if CRU data is not available (e.g., in standalone mode).

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to 90).
    lon : float
        Longitude in degrees (-180 to 180).
    month : int
        Month of the year (1-12).
    spatial_res : float, optional
        Spatial resolution in degrees for finding the nearest CRU grid cell (default: 0.5).

    Returns
    -------
    float or None
        Mean monthly air temperature in Celsius for the specified location and month,
        or None if CRU data is not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (month, lat, or lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (1 <= month <= 12):
        raise ValueError(f"Month must be between 1 and 12, got {month}")
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Only catch availability errors, not validation errors
    try:
        return _get_mean_monthly_air_temperature(lat, lon, month, spatial_res)
    except (FileNotFoundError, IOError, OSError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature validation skipped."
        )
        return None
    except ValueError as e:
        # Only catch data availability errors from CRU
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature validation skipped."
            )
            return None
        else:
            # Re-raise validation errors
            raise

def get_monthly_air_temperature_diffmax(
    lat: float, lon: float, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate the maximum difference between mean monthly air temperatures
    using CRU TS4.06 climatological data. This is defined as the difference 
    between the warmest and coldest mean monthly temperature within the annual 
    cycle at the specified location.

    Parameters
    ----------
    lat : float
        Site latitude in degrees (-90 to 90).
    lon : float
        Site longitude in degrees (-180 to 180).
    spatial_res : float, optional
        Search spatial resolution for nearest CRU grid cell, by default 0.5.

    Returns
    -------
    float or None
        Maximum difference in mean monthly air temperature (degC),
        or None if CRU data not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (lat, lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    try:
        return _get_monthly_air_temperature_diffmax(lat, lon, spatial_res)
    except (FileNotFoundError, IOError, OSError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature diffmax validation skipped."
        )
        return None
    except ValueError as e:
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature diffmax validation skipped."
            )
            return None
        else:
            raise

def get_mean_annual_air_temperature(
    lat: float, lon: float, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate mean annual air temperature using CRU TS4.06 climate normals.

    This function wraps `yaml_helpers.get_mean_annual_air_temperature` and returns
    None if CRU data is not available (e.g., in standalone mode).

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to 90).
    lon : float
        Longitude in degrees (-180 to 180).
    spatial_res : float, optional
        Spatial resolution in degrees for finding the nearest CRU grid cell (default: 0.5).

    Returns
    -------
    float or None
        Mean annual air temperature in Celsius for the specified location,
        or None if CRU data is not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (lat, lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Only catch availability errors, not validation errors
    try:
        return _get_mean_annual_air_temperature(lat, lon, spatial_res)
    except (FileNotFoundError, IOError, OSError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature validation skipped."
        )
        return None
    except ValueError as e:
        # Only catch data availability errors from CRU
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature validation skipped."
            )
            return None
        else:
            # Re-raise validation errors
            raise


def update_temperature_parameters(
    element: dict, avg_temp: float
) -> Tuple[dict, List[str]]:
    """
    Update temperature-related parameters ('temperature', 'tsfc', 'tin') in a dictionary element.

    Parameters
    ----------
    element : dict
        Dictionary containing temperature parameters to be updated.
    avg_temp : float
        Target temperature value to set for the parameters.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated element dictionary.
            - list of str: Names of parameters that were updated.

    Notes
    -----
    - 'temperature' is set to a list of five values, each equal to `avg_temp`.
    - 'tsfc' and 'tin' are set to `avg_temp` as a scalar.
    - Only parameters whose values differ from the target are updated and reported.
    """
    updated_params = []

    if "temperature" in element and isinstance(element["temperature"], dict):
        current_temp = element["temperature"].get("value")
        if current_temp != [avg_temp] * 5:
            element["temperature"]["value"] = [avg_temp] * 5
            updated_params.append("temperature")

    if "tsfc" in element and isinstance(element["tsfc"], dict):
        current_tsfc = element["tsfc"].get("value")
        if current_tsfc != avg_temp:
            element["tsfc"]["value"] = avg_temp
            updated_params.append("tsfc")

    if "tin" in element and isinstance(element["tin"], dict):
        current_tin = element["tin"].get("value")
        if current_tin != avg_temp:
            element["tin"]["value"] = avg_temp
            updated_params.append("tin")

    return element, updated_params


def adjust_surface_temperatures(
    yaml_data: dict, start_date: str
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Set initial surface temperatures based on location and season.

    This function updates the initial surface temperature parameters for each site
    using climatological data (CRU TS4.06) based on the site's latitude, longitude,
    and the simulation start month. It adjusts the following parameters for each
    surface type and building element:
        - 'temperature' (list of 5 values)
        - 'tsfc' (scalar)
        - 'tin' (scalar)
    It also updates STEBBS-related temperature parameters and applies annual and
    monthly temperature corrections where available.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing site configurations.
    start_date : str
        Simulation start date in 'YYYY-MM-DD' format.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with adjusted surface temperatures.
            - list of ScientificAdjustment: Records of all adjustments applied.

    Notes
    -----
    - If CRU data is unavailable for a site, temperature adjustments are skipped for that site.
    - Only parameters whose values differ from the climatological value are updated.
    - Adjustments are recorded for reporting purposes.
    """
    adjustments = []
    month = datetime.strptime(start_date, "%Y-%m-%d").month

    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        stebbs = props.get("stebbs", {})
        site_gridid = get_site_gridid(site)

        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry

        if lat is None:
            continue  # Skip if no latitude (will be caught by validation)

        lng_entry = props.get("lng", {})
        lng = lng_entry.get("value") if isinstance(lng_entry, dict) else lng_entry

        if lng is None:
            continue  # Skip if no longitude (will be caught by validation)

        avg_temp = get_mean_monthly_air_temperature(lat, lng, month)

        # Skip temperature validation if CRU data not available
        if avg_temp is None:
            logger_supy.debug(
                "Skipping temperature validation - CRU data not available"
            )
            continue

        surface_types = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]

        for surface_type in surface_types:
            surf = initial_states.get(surface_type, {})
            if not isinstance(surf, dict):
                continue

            surf, updated_params = update_temperature_parameters(surf, avg_temp)

            if updated_params:
                param_list = ", ".join(updated_params)
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"initial_states.{surface_type}",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=param_list,
                        new_value=f"{avg_temp} C",
                        reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f}) for month {month}",
                    )
                )

            initial_states[surface_type] = surf

        # Update STEBBS temperature parameter values to avg_temp
        for key in ("initial_outdoor_temperature", "initial_indoor_temperature",):
            if key in stebbs and isinstance(stebbs[key], dict):
                old_val = stebbs[key].get("value")
                if old_val != avg_temp:
                    stebbs[key]["value"] = avg_temp
                    adjustments.append(
                        ScientificAdjustment(
                            parameter=f"stebbs.{key}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=str(old_val),
                            new_value=f"{avg_temp} C",
                            reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f}) for month {month}",
                        )
                    )

        # Update month_mean_air_temperature_diffmax in stebbs if present
        diffmax_val = get_monthly_air_temperature_diffmax(lat, lng)
        if diffmax_val is None:
            logger_supy.debug("Skipping diffmax update - CRU data not available")
        else:
            for key in ("month_mean_air_temperature_diffmax",):
                if key in stebbs and isinstance(stebbs[key], dict):
                    old_val = stebbs[key].get("value")
                    if old_val != diffmax_val:
                        stebbs[key]["value"] = diffmax_val
                        adjustments.append(
                            ScientificAdjustment(
                                parameter=f"stebbs.{key}",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(old_val),
                                new_value=f"{diffmax_val} C",
                                reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f})",
                            )
                        )

        # Update STEBBS annual_mean_air_temperature using annual mean from CRU data
        annual_temp = get_mean_annual_air_temperature(lat, lng)
        if annual_temp is not None and "annual_mean_air_temperature" in stebbs:
            if isinstance(stebbs["annual_mean_air_temperature"], dict):
                old_annual_val = stebbs["annual_mean_air_temperature"].get("value")
                if old_annual_val != annual_temp:
                    stebbs["annual_mean_air_temperature"]["value"] = annual_temp
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="stebbs.annual_mean_air_temperature",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=str(old_annual_val),
                            new_value=f"{annual_temp} C",
                            reason=f"Set from CRU annual mean (1991-2020 normals) for coordinates ({lat:.2f}, {lng:.2f})",
                        )
                    )

        # Update temperatures in roofs and walls arrays
        for array_name in ["roofs", "walls"]:
            if array_name in initial_states:
                array = initial_states[array_name]
                if isinstance(array, list):
                    for element_idx, element in enumerate(array):
                        if not isinstance(element, dict):
                            continue

                        element, updated_params = update_temperature_parameters(
                            element, avg_temp
                        )

                        if updated_params:
                            param_list = ", ".join(updated_params)
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter=f"initial_states.{array_name}[{element_idx}]",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=param_list,
                                    new_value=f"{avg_temp} C",
                                    reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f}) for month {month}",
                                )
                            )

        # Save back to site
        site["initial_states"] = initial_states
        props["stebbs"] = stebbs
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def adjust_land_cover_fractions(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Auto-fix small floating point errors in surface fractions.

    This function ensures that the sum of all land cover surface fractions (sfr)
    for each site is exactly 1.0, within a small tolerance. If the sum is slightly
    less than or greater than 1.0 (within SFR_FRACTION_TOL), the largest surface
    fraction is adjusted to compensate for the floating point error.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing site configurations.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with corrected surface fractions.
            - list of ScientificAdjustment: Records of all adjustments applied.

    Notes
    -----
    - Only small floating point errors are auto-corrected (within SFR_FRACTION_TOL).
    - The largest surface fraction is adjusted to ensure the sum is exactly 1.0.
    - All corrections are recorded in the adjustments list.
    """
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")
        site_gridid = get_site_gridid(site)

        if not land_cover:
            continue

        # Calculate sum of all surface fractions
        surface_fractions = {}
        sfr_sum = 0.0

        for surface_type, surface_props in land_cover.items():
            if isinstance(surface_props, dict):
                sfr_value = surface_props.get("sfr", {}).get("value")
                if sfr_value is not None:
                    surface_fractions[surface_type] = sfr_value
                    sfr_sum += sfr_value

        correction_applied = False

        lower = 1.0 - SFR_FRACTION_TOL
        upper = 1.0 + SFR_FRACTION_TOL

        # Auto-correct only small floating point errors (same as precheck logic)
        if lower <= sfr_sum < 1.0:
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = 1.0 - sfr_sum
            old_value = surface_fractions[max_surface]
            new_value = old_value + correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            # Always report adjustments, but use appropriate format based on visibility
            if abs(correction) >= 1e-6:  # If change is visible at 6 decimal places
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=f"{old_value:.6f}",
                        new_value=f"{new_value:.6f}",
                        reason=f"Auto-corrected {max_surface}.sfr to have sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
                    )
                )
            else:  # Tiny correction not visible at display precision
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value="rounded to achieve sum of land cover fractions equal to 1.0",
                        new_value=f"tolerance level: {abs(correction):.2e}",
                        reason="Small floating point rounding applied to surface with max surface fraction value",
                    )
                )

        elif 1.0 < sfr_sum <= upper:
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = sfr_sum - 1.0
            old_value = surface_fractions[max_surface]
            new_value = old_value - correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            # Always report adjustments, but use appropriate format based on visibility
            if abs(correction) >= 1e-6:  # If change is visible at 6 decimal places
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=f"{old_value:.6f}",
                        new_value=f"{new_value:.6f}",
                        reason=f"Auto-corrected {max_surface}.sfr to have sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
                    )
                )
            else:  # Tiny correction not visible at display precision
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value="rounded to achieve sum of land cover fractions equal to 1.0",
                        new_value=f"tolerance level: {abs(correction):.2e}",
                        reason="Small floating point rounding applied to surface with max surface fraction value",
                    )
                )

        if correction_applied:
            site["properties"] = props
            yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def adjust_model_dependent_nullification(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Nullify parameters for disabled model options.

    This function automatically sets to None (nullifies) parameters that are not relevant
    when certain model physics options are disabled. For example, if 'stebbsmethod' is 0,
    all STEBBS and building_archetype parameters are nullified. If 'emissionsmethod' is 0-4,
    all anthropogenic CO2 and biogenic dectr parameters are nullified.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing the model configuration.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with nullified parameters.
            - list of ScientificAdjustment: Records of all adjustments applied.

    Notes
    -----
    - This function is called as part of the scientific adjustment pipeline.
    - Nullification is performed recursively for nested blocks.
    - All changes are recorded in the adjustments list for reporting.
    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})

    # --- STEBBS ---
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 0:
        sites = yaml_data.get("sites", [])

        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            site_gridid = get_site_gridid(site)

            def _nullify_block(block_name: str, block: Union[dict, list]) -> bool:
                if not isinstance(block, (dict, list)) or not block:
                    return False

                nullified_params: List[str] = []

                def _recursive_nullify(node: Any, path: str):
                    if isinstance(node, dict):
                        if "value" in node:
                            inner = node.get("value")
                            if isinstance(inner, list):
                                if any(item is not None for item in inner):
                                    node["value"] = [None] * len(inner)
                                    nullified_params.append(path)
                            else:
                                if inner is not None:
                                    node["value"] = None
                                    nullified_params.append(path)
                        else:
                            for key, val in node.items():
                                child_path = f"{path}.{key}" if path else key
                                if isinstance(val, (dict, list)):
                                    _recursive_nullify(val, child_path)
                                else:
                                    if val is not None:
                                        node[key] = None
                                        nullified_params.append(child_path)
                    elif isinstance(node, list):
                        for idx, item in enumerate(node):
                            child_path = f"{path}[{idx}]" if path else f"[{idx}]"
                            if isinstance(item, (dict, list)):
                                _recursive_nullify(item, child_path)
                            else:
                                if item is not None:
                                    node[idx] = None
                                    nullified_params.append(child_path)

                _recursive_nullify(block, block_name)
                if nullified_params:
                    param_list = ", ".join(nullified_params)
                    adjustments.append(
                        ScientificAdjustment(
                            parameter=block_name,
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=f"stebbsmethod is switched off, nullified {len(nullified_params)} related parameters - {param_list}",
                            new_value="null",
                            reason=f"stebbsmethod switched off, nullified {len(nullified_params)} related parameters",
                        )
                    )
                    return True

                return False

            site_updated = False
            for block_name in ("stebbs", "building_archetype"):
                block = props.get(block_name)
                if _nullify_block(block_name, block):
                    props[block_name] = block
                    site_updated = True

            if site_updated:
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

    # --- ANTHROPOGENIC CO2 ---
    emissionsmethod = get_value_safe(physics, "emissions")

    if emissionsmethod is not None and emissionsmethod in [0, 1, 2, 3, 4]:
        sites = yaml_data.get("sites", [])

        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            anth_emis = props.get("anthropogenic_emissions", {})
            co2_block = anth_emis.get("co2", {})
            site_gridid = get_site_gridid(site)

            if co2_block:
                before_snapshot = deepcopy(co2_block)
                try:
                    nullify_co2_block_recursive(co2_block)
                except Exception:
                    logger_supy.exception(
                        "[precheck] failed to nullify co2 block with shared helper"
                    )

                nullified_params = collect_nullified_paths(before_snapshot, co2_block)
                if nullified_params:
                    param_list = ", ".join(nullified_params)
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="anthropogenic_emissions.co2",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=f"emissionsmethod 0..4 (CO2 disabled), nullified {len(nullified_params)} related parameters - {param_list}",
                            new_value="null",
                            reason=f"emissionsmethod 0..4 (CO2 disabled), nullified {len(nullified_params)} related parameters",
                        )
                    )

                anth_emis["co2"] = co2_block
                props["anthropogenic_emissions"] = anth_emis
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

            # Nullify biogenic dectr params here as well (same canonical helper)
            try:
                if _nullify_biogenic_in_props(props):
                    yaml_data["sites"][site_idx]["properties"] = props
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="land_cover.dectr.biogenic_params",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value="biogenic dectr params present",
                            new_value="null",
                            reason="emissionsmethod 0..4 (CO2 disabled) – nullified dectr biogenic parameters",
                        )
                    )
            except Exception:
                logger_supy.exception(
                    "[phase_b] failed to nullify dectr biogenic params for site %d",
                    site_idx,
                )

    return yaml_data, adjustments


def get_season_from_doy(doy: int, lat: float) -> str:
    """
    Determine the season based on day of year and latitude.

    Parameters
    ----------
    doy : int
        Day of year (1-365 or 1-366).
    lat : float
        Latitude in degrees.

    Returns
    -------
    str
        Season string: one of 'equatorial', 'tropical', 'summer', 'winter', 'spring', or 'fall'.

    Notes
    -----
    - For |lat| <= 10, returns 'equatorial'.
    - For 10 < |lat| < 23.5, returns 'tropical'.
    - For |lat| >= 23.5, returns a temperate season based on hemisphere and doy.
    - Northern Hemisphere (lat >= 0):
        - 'summer': doy 151-249
        - 'spring': doy 61-150
        - 'fall': doy 250-334
        - 'winter': otherwise
    - Southern Hemisphere (lat < 0):
        - 'winter': doy 151-249
        - 'fall': doy 61-150
        - 'spring': doy 250-334
        - 'summer': otherwise
    """
    abs_lat = abs(lat)

    if abs_lat <= 10:
        return "equatorial"
    if 10 < abs_lat < 23.5:
        return "tropical"

    if lat >= 0:  # Northern Hemisphere
        if 150 < doy < 250:
            return "summer"
        elif 60 < doy <= 150:
            return "spring"
        elif 250 <= doy < 335:
            return "fall"
        else:
            return "winter"
    else:  # Southern Hemisphere
        if 150 < doy < 250:
            return "winter"
        elif 60 < doy <= 150:
            return "fall"
        elif 250 <= doy < 335:
            return "spring"
        else:
            return "summer"


def get_season(start_date: str, lat: float) -> str:
    """
    Determine the season based on simulation start date and latitude.

    Parameters
    ----------
    start_date : str
        Simulation start date in 'YYYY-MM-DD' format.
    lat : float
        Latitude in degrees.

    Returns
    -------
    str
        Season string: one of 'equatorial', 'tropical', 'summer', 'winter', 'spring', or 'fall'.

    Raises
    ------
    ValueError
        If start_date is not in 'YYYY-MM-DD' format.

    Notes
    -----
    - Uses day of year from start_date and latitude to determine the season.
    - Delegates to get_season_from_doy for actual season logic.
    """
    try:
        start = datetime.strptime(start_date, "%Y-%m-%d").timetuple().tm_yday
    except ValueError:
        raise ValueError("start_date must be in YYYY-MM-DD format")

    return get_season_from_doy(start, lat)


def _get_range_and_id(surf_props: dict, surf_state: dict) -> Tuple[Optional[float], Optional[float], Optional[float]]:
    """
    Extracts albedo parameter range and current value for a surface.

    Parameters
    ----------
    surf_props : dict
        Dictionary containing the properties of a surface (e.g., from land_cover).
    surf_state : dict
        Dictionary containing the state parameters of a surface (e.g., from initial_states).

    Returns
    -------
    tuple of (float or None, float or None, float or None)
        - alb_min : float or None
            Minimum albedo value for the surface (from surf_props).
        - alb_max : float or None
            Maximum albedo value for the surface (from surf_props).
        - alb_id : float or None
            Current albedo value for the surface (from surf_state).
    """
    alb_min = get_value_safe(surf_props, "alb_min")
    alb_max = get_value_safe(surf_props, "alb_max")
    alb_id = get_value_safe(surf_state, "alb_id")
    return alb_min, alb_max, alb_id


def _set_alb_id(
    initial_states: dict,
    surf_key: str,
    new_alb_id: Optional[float],
):
    if new_alb_id is None:
        return False, None, None
    if surf_key not in initial_states:
        initial_states[surf_key] = {}
    surf_state = initial_states[surf_key]
    if not isinstance(surf_state, dict):
        return False, None, None
    old_val = get_value_safe(surf_state, "alb_id")
    if old_val is not None and math.isclose(old_val, new_alb_id):
        return False, old_val, new_alb_id
    surf_state["alb_id"] = {"value": new_alb_id}
    return True, old_val, new_alb_id


def adjust_seasonal_parameters(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Apply seasonal adjustments to site parameters including LAI, snowalb, and DLS calculations.

    This function updates site parameters based on the simulation start date and latitude,
    applying seasonal logic to LAI (Leaf Area Index), snow albedo, and daylight saving time (DLS)
    parameters. It also adjusts vegetation albedo for grass, deciduous, and evergreen trees.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing site configurations.
    start_date : str
        Simulation start date in 'YYYY-MM-DD' format.
    model_year : int
        Simulation year for leap year and DLS calculations.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with seasonal adjustments applied.
            - list of ScientificAdjustment: Records of all adjustments performed.

    Notes
    -----
    - LAI is set based on season: summer uses laimax, winter uses laimin, spring/fall use the mean.
    - snowalb is nullified for summer/tropical/equatorial seasons.
    - Vegetation albedo (alb_id) is set based on season and surface type.
    - DLS (startdls, enddls) and timezone are calculated and set if missing or outdated.
    - All changes are recorded for reporting.
    """
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        site_gridid = get_site_gridid(site)

        # Get site coordinates
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        lng = get_value_safe(props, "lng")

        if lat is None:
            continue  # Skip if no latitude

        try:
            season = get_season(start_date, lat)
        except Exception as e:
            continue  # Skip on season detection error

        if (
            season in ("summer", "tropical", "equatorial")
            and "snowalb" in initial_states
        ):
            current_snowalb = initial_states["snowalb"].get("value")
            if current_snowalb is not None:
                initial_states["snowalb"]["value"] = None
                adjustments.append(
                    ScientificAdjustment(
                        parameter="snowalb",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(current_snowalb),
                        new_value="null",
                        reason=f"Nullified for {season} season (no snow expected)",
                    )
                )

        land_cover = props.get("land_cover", {})
        dectr = land_cover.get("dectr", {})
        if dectr:
            sfr = dectr.get("sfr", {}).get("value", 0)

            if sfr > 0:
                lai = dectr.get("lai", {})
                laimin = get_value_safe(lai, "lai_min")
                if laimin is None:
                    laimin = get_value_safe(lai, "laimin")
                laimax = get_value_safe(lai, "lai_max")
                if laimax is None:
                    laimax = get_value_safe(lai, "laimax")

                if laimin is not None and laimax is not None:
                    if season == "summer":
                        lai_val = laimax
                    elif season == "winter":
                        lai_val = laimin
                    elif season in ("spring", "fall"):
                        lai_val = (laimax + laimin) / 2
                    else:  # tropical/equatorial
                        lai_val = laimax

                    if "dectr" not in initial_states:
                        initial_states["dectr"] = {}

                    current_lai = get_value_safe(initial_states["dectr"], "lai_id")
                    if current_lai != lai_val:
                        initial_states["dectr"]["lai_id"] = {"value": lai_val}
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="dectr.lai_id",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(current_lai)
                                if current_lai is not None
                                else "undefined",
                                new_value=str(lai_val),
                                reason=f"Set seasonal LAI for {season} (laimin={laimin}, laimax={laimax})",
                            )
                        )
            # Note: When sfr=0, we don't nullify lai_id - we simply skip validation
            # The warning "Parameters not checked because surface fraction is 0" covers this

        # Seasonal adjustment of vegetation albedo ranges (alb_id only)
        vegetated_surfaces = (
            ("grass", "grass"),
            ("dectr", "deciduous trees"),
            ("evetr", "evergreen trees"),
        )

        for surf_key, label in vegetated_surfaces:
            surf_props = land_cover.get(surf_key, {})
            # Check surface fraction:
            sfr = surf_props.get("sfr", {}).get("value", 0)
            if not sfr:
                continue  # Skip albedo adjustment if surface fraction is zero

            surf_state = initial_states.get(surf_key, {})
            alb_min, alb_max, alb_id_val = _get_range_and_id(surf_props, surf_state)
            if alb_min is None or alb_max is None:
                continue
            if season in ("summer", "tropical", "equatorial"):
                target = alb_min if surf_key == "grass" else alb_max
            elif season == "winter":
                target = alb_max if surf_key == "grass" else alb_min
            else:
                if alb_id_val is None:
                    continue
                target = 0.5 * (alb_min + alb_max)
            changed, old_val, new_alb_id = _set_alb_id(
                initial_states,
                surf_key,
                target,
            )
            if changed:
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{surf_key}.alb_id",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(old_val),
                        new_value=str(new_alb_id),
                        reason=f"Set seasonal albedo for {season} on {label} based on (alb_min, alb_max)",
                    )
                )

        if lat is not None and lng is not None:
            try:
                dls = DLSCheck(lat=lat, lng=lng, year=model_year)
                start_dls, end_dls, tz_offset = dls.compute_dst_transitions()

                anthro_emissions = props.get("anthropogenic_emissions", {})
                if anthro_emissions and start_dls and end_dls:
                    current_startdls = anthro_emissions.get("startdls", {}).get("value")
                    current_enddls = anthro_emissions.get("enddls", {}).get("value")

                    dls_updated = False
                    if current_startdls != start_dls:
                        anthro_emissions["startdls"] = {"value": start_dls}
                        dls_updated = True

                    if current_enddls != end_dls:
                        anthro_emissions["enddls"] = {"value": end_dls}
                        dls_updated = True

                    if dls_updated:
                        # Add separate adjustments for each parameter
                        if current_startdls != start_dls:
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter="anthropogenic_emissions.startdls",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(current_startdls),
                                    new_value=str(start_dls),
                                    reason=f"Calculated DLS start for coordinates ({lat:.2f}, {lng:.2f})",
                                )
                            )
                        if current_enddls != end_dls:
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter="anthropogenic_emissions.enddls",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(current_enddls),
                                    new_value=str(end_dls),
                                    reason=f"Calculated DLS end for coordinates ({lat:.2f}, {lng:.2f})",
                                )
                            )
                        logger_supy.debug(
                            f"[site #{site_idx}] DLS: start={start_dls}, end={end_dls}"
                        )

                if tz_offset is not None:
                    current_timezone = props.get("timezone", {}).get("value")
                    if current_timezone != tz_offset:
                        props["timezone"] = {"value": tz_offset}
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="timezone",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(current_timezone),
                                new_value=str(tz_offset),
                                reason=f"Calculated timezone offset for coordinates ({lat:.2f}, {lng:.2f})",
                            )
                        )
                        logger_supy.debug(
                            f"[site #{site_idx}] Timezone set to {tz_offset}"
                        )

            except Exception as e:
                logger_supy.debug(f"[site #{site_idx}] DLS calculation failed: {e}")
                pass

        # Save back to site
        site["properties"] = props
        site["initial_states"] = initial_states
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments

def adjust_model_option_rcmethod(yaml_data: dict) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Adjust roof_outer_heat_capacity_fraction and wall_outer_heat_capacity_fraction if rcmethod == 0.

    If the model physics option 'outer_cap_fraction' is set to 0, this function sets
    'roof_outer_heat_capacity_fraction' and 'wall_outer_heat_capacity_fraction' to
    0.5 for all sites' building_archetype blocks, as required by the model
    specification.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing the model configuration.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with adjusted parameters.
            - list of ScientificAdjustment: Records of all adjustments applied.

    Notes
    -----
    - Only applies the adjustment if 'outer_cap_fraction' is exactly 0.
    - Records each parameter change in the adjustments list for reporting.
    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})
    rcmethod_value = get_value_safe(physics, "outer_cap_fraction")

    if rcmethod_value == 0:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            site_gridid = get_site_gridid(site)

            # roof_outer_heat_capacity_fraction
            roof_frac_entry = building_archetype.get(
                "roof_outer_heat_capacity_fraction", {}
            )
            old_roof_frac = roof_frac_entry.get("value") if isinstance(roof_frac_entry, dict) else roof_frac_entry
            if old_roof_frac != 0.5:
                building_archetype["roof_outer_heat_capacity_fraction"] = {"value": 0.5}
                adjustments.append(
                    ScientificAdjustment(
                        parameter="building_archetype.roof_outer_heat_capacity_fraction",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(old_roof_frac),
                        new_value="0.5",
                        reason="outer_cap_fraction == 0, set roof_outer_heat_capacity_fraction to 0.5"
                    )
                )

            # wall_outer_heat_capacity_fraction
            wall_frac_entry = building_archetype.get(
                "wall_outer_heat_capacity_fraction", {}
            )
            old_wall_frac = wall_frac_entry.get("value") if isinstance(wall_frac_entry, dict) else wall_frac_entry
            if old_wall_frac != 0.5:
                building_archetype["wall_outer_heat_capacity_fraction"] = {"value": 0.5}
                adjustments.append(
                    ScientificAdjustment(
                        parameter="building_archetype.wall_outer_heat_capacity_fraction",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(old_wall_frac),
                        new_value="0.5",
                        reason="outer_cap_fraction == 0, set wall_outer_heat_capacity_fraction to 0.5"
                    )
                )

            props["building_archetype"] = building_archetype
            site["properties"] = props
            yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments

def adjust_model_option_setpointmethod(yaml_data: dict) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    If setpoint == 0 or 1, set all entries in HeatingSetpointTemperatureProfile and
    CoolingSetpointTemperatureProfile in building_archetype to null for all sites.
    If setpoint == 2, set HeatingSetpointTemperature and CoolingSetpointTemperature
    in building_archetype to null for all sites (they are not needed).
    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})
    setpointmethod = get_value_safe(physics, "setpoint")

    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        building_archetype = props.get("building_archetype", {})
        site_gridid = get_site_gridid(site)

        if setpointmethod == 2:
            for param in ["heating_setpoint_temperature", "cooling_setpoint_temperature"]:
                entry = building_archetype.get(param)
                if isinstance(entry, dict):
                    old_val = entry.get("value")
                    if old_val is not None:
                        building_archetype[param]["value"] = None
                        adjustments.append(
                            ScientificAdjustment(
                                parameter=f"building_archetype.{param}",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(old_val),
                                new_value="null",
                                reason="setpoint == 2, parameter not needed"
                            )
                        )
        elif setpointmethod == 0 or setpointmethod == 1:
            for prof_param in ["heating_setpoint_temperature_profile", "cooling_setpoint_temperature_profile"]:
                profile = building_archetype.get(prof_param)
                if isinstance(profile, dict):
                    for daytype in ("working_day", "holiday"):
                        day_profile = profile.get(daytype)
                        if isinstance(day_profile, dict):
                            changed_slice = []
                            for slice_str, temp_val in day_profile.items():
                                if temp_val is not None:
                                    old_val = temp_val
                                    day_profile[slice_str] = None
                                    changed_slice.append(slice_str)
                            if changed_slice:
                                adjustments.append(
                                    ScientificAdjustment(
                                        parameter=f"building_archetype.{prof_param}.{daytype}",
                                        site_index=site_idx,
                                        site_gridid=site_gridid,
                                        old_value="non-null profile entries",
                                        new_value="null",
                                        reason="setpoint == 0 or 1, profile entries not needed",
                                    )
                                )
        props["building_archetype"] = building_archetype
        site["properties"] = props
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments

def adjust_model_option_stebbsmethod(yaml_data: dict) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Adjust STEBBS-related parameters based on the 'stebbsmethod' and 'WWR' options.

    This function nullifies window or external wall parameters in the STEBBS and
    building_archetype blocks depending on the Window-to-Wall Ratio (WWR) when
    'stebbsmethod' is set to 1.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing the model configuration.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with adjusted parameters.
            - list of ScientificAdjustment: Records of all adjustments applied.

    Notes
    -----
    - If 'stebbsmethod' == 1 and 'WWR' == 0.0, all window-related parameters are set to None.
    - If 'stebbsmethod' == 1 and 'WWR' == 1.0, all external wall-related parameters are set to None.
    - All changes are recorded in the adjustments list for reporting.
    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            bldgarc = props.get("building_archetype", {})

            site_gridid = get_site_gridid(site)

            wwr_entry = bldgarc.get("window_to_wall_ratio", {})
            wwr = wwr_entry.get("value") if isinstance(wwr_entry, dict) else wwr_entry

            if wwr == 0.0:
                window_params_stebbs = [
                    "window_internal_convection_coefficient",
                    "window_external_convection_coefficient",
                ]
                window_params_bldgarc = [
                    "window_thickness",
                    "window_effective_conductivity",
                    "window_density",
                    "window_specific_heat_capacity",
                    "window_external_emissivity",
                    "window_internal_emissivity",
                    "window_transmissivity",
                    "window_absorptivity",
                    "window_reflectivity",
                ]
                # Nullify in stebbs
                for param in window_params_stebbs:
                    if param in stebbs and isinstance(stebbs[param], dict):
                        old_val = stebbs[param].get("value")
                        if old_val is not None:
                            stebbs[param]["value"] = None
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter=f"stebbs.{param}",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(old_val),
                                    new_value="null",
                                    reason="WWR == 0, window parameter nullified"
                                )
                            )
                # Nullify in building_archetype
                for param in window_params_bldgarc:
                    if param in bldgarc and isinstance(bldgarc[param], dict):
                        old_val = bldgarc[param].get("value")
                        if old_val is not None:
                            bldgarc[param]["value"] = None
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter=f"building_archetype.{param}",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(old_val),
                                    new_value="null",
                                    reason="WWR == 0, window parameter nullified"
                                )
                            )
                props["stebbs"] = stebbs
                props["building_archetype"] = bldgarc
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

            elif wwr == 1.0:
                # Nullify external wall parameters in stebbs and building_archetype
                wall_params_stebbs = [
                    "wall_external_convection_coefficient",
                    "wall_internal_convection_coefficient",
                    ]
                wall_params_bldgarc = [
                    "wall_external_emissivity",
                    "wall_internal_emissivity",
                    "wall_transmissivity",
                    "wall_absorptivity",
                    "wall_reflectivity",
                    "wall_thickness",
                    "wall_effective_conductivity",
                    "wall_density",
                    "wall_specific_heat_capacity",
                    ]
                for param in wall_params_stebbs:
                    entry = stebbs.get(param)
                    if isinstance(entry, dict) and entry.get("value") is not None:
                        old_val = entry["value"]
                        entry["value"] = None
                        adjustments.append(
                            ScientificAdjustment(
                                parameter=f"stebbs.{param}",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(old_val),
                                new_value="null",
                                reason="WWR == 1, external wall parameter nullified"
                            )
                        )
                for param in wall_params_bldgarc:
                    entry = bldgarc.get(param)
                    if isinstance(entry, dict) and entry.get("value") is not None:
                        old_val = entry["value"]
                        entry["value"] = None
                        adjustments.append(
                            ScientificAdjustment(
                                parameter=f"building_archetype.{param}",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(old_val),
                                new_value="null",
                                reason="WWR == 1, external wall parameter nullified"
                            )
                        )
                props["stebbs"] = stebbs
                props["building_archetype"] = bldgarc
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments

def run_scientific_adjustment_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Apply automatic scientific corrections and adjustments.

    This function runs the scientific adjustment pipeline, applying a series of
    automatic corrections to the YAML configuration based on climatological data,
    model physics options, and seasonal logic.

    Parameters
    ----------
    yaml_data : dict
        Parsed YAML data containing the model configuration.
    start_date : str
        Simulation start date in 'YYYY-MM-DD' format.
    model_year : int
        Simulation year for leap year and seasonal calculations.

    Returns
    -------
    tuple
        A tuple containing:
            - dict: The updated YAML data with all scientific adjustments applied.
            - list of ScientificAdjustment: Records of all adjustments performed.

    Notes
    -----
    The pipeline applies the following adjustments in order:
        1. Surface temperature initialization from climatology.
        2. Land cover fraction normalization.
        3. Nullification of parameters for disabled model options.
        4. Seasonal parameter adjustments (LAI, snow albedo, DLS, vegetation albedo).
        5. Model option-dependent adjustments (e.g., rcmethod, stebbsmethod).
    All changes are recorded for reporting.
    """
    updated_data = deepcopy(yaml_data)
    adjustments = []

    for adjust_func, args in [
        (adjust_surface_temperatures, (updated_data, start_date)),
        (adjust_land_cover_fractions, (updated_data,)),
        (adjust_model_dependent_nullification, (updated_data,)),
        (adjust_seasonal_parameters, (updated_data, start_date, model_year)),
        (adjust_model_option_rcmethod, (updated_data,)),
        (adjust_model_option_setpointmethod, (updated_data,)),
        (adjust_model_option_stebbsmethod, (updated_data,))
    ]:
        updated_data, adj = adjust_func(*args)
        adjustments.extend(adj)

    return updated_data, adjustments


def create_science_report(
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    science_yaml_filename: str = None,
    phase_a_report_file: str = None,
    mode: str = "public",
    phase: str = "B",
) -> str:
    """
    Generate a comprehensive scientific validation report.

    Parameters
    ----------
    validation_results : List[ValidationResult]
        List of validation results from scientific checks.
    adjustments : List[ScientificAdjustment]
        List of automatic scientific adjustments applied.
    science_yaml_filename : str, optional
        Name of the science-checked YAML file (for display).
    phase_a_report_file : str, optional
        Path to the Phase A report file (for extracting previous changes).
    mode : str, default="public"
        Output mode for the report ("public" or other).
    phase : str, default="B"
        Validation phase identifier.

    Returns
    -------
    str
        The formatted scientific validation report as a string.

    Notes
    -----
    - Summarizes errors, warnings, adjustments, and informational notes.
    - Integrates Phase A report highlights if available.
    - Designed for both human readability and traceability.
    """
    report_lines = []

    # Use unified report title for all validation phases
    title = "SUEWS Validation Report"

    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(
        f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}"
    )
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            phase_a_content = REPORT_WRITER.read(phase_a_report_file)

            lines = phase_a_content.split("\n")
            current_section = None

            for line in lines:
                line = line.strip()
                if "Updated (" in line and "renamed parameter" in line:
                    current_section = "renames"
                elif "Updated (" in line and "optional missing parameter" in line:
                    current_section = "optional"
                elif "parameter(s) not in standard" in line:
                    current_section = "not_standard"
                elif line.startswith("--") and current_section == "renames":
                    phase_a_renames.append(line[2:].strip())
                elif line.startswith("--") and current_section == "optional":
                    phase_a_optional_missing.append(line[2:].strip())
                elif line.startswith("--") and current_section == "not_standard":
                    phase_a_not_in_standard.append(line[2:].strip())
        except Exception:
            # If we can't read Phase A report, continue without it
            pass

    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]
    passed = [r for r in validation_results if r.status == "PASS"]

    if errors:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(
            f"- Found ({len(errors)}) critical scientific parameter error(s):"
        )
        for error in errors:
            site_ref = (
                f" at site [{error.site_gridid}]"
                if error.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {error.parameter}{site_ref}: {error.message}")
            if error.suggested_value is not None:
                report_lines.append(f"   Suggested fix: {error.suggested_value}")
        report_lines.append("")

    report_lines.append("## NO ACTION NEEDED")

    if adjustments:
        total_params_changed = 0
        for adjustment in adjustments:
            if "temperature, tsfc, tin" in adjustment.old_value:
                total_params_changed += 3
            elif adjustment.parameter == "stebbs" and "nullified" in adjustment.reason:
                import re

                match = re.search(r"nullified (\d+) parameters", adjustment.reason)
                if match:
                    total_params_changed += int(match.group(1))
                else:
                    total_params_changed += 1
            elif adjustment.parameter in [
                "anthropogenic_emissions.startdls",
                "anthropogenic_emissions.enddls",
            ]:
                total_params_changed += 1
            else:
                total_params_changed += 1

        report_lines.append(f"- Updated ({total_params_changed}) parameter(s):")
        for adjustment in adjustments:
            site_ref = (
                f" at site [{adjustment.site_gridid}]"
                if adjustment.site_gridid is not None
                else ""
            )
            report_lines.append(
                f"-- {adjustment.parameter}{site_ref}: {adjustment.old_value} -> {adjustment.new_value} ({adjustment.reason})"
            )

    phase_a_items = []
    if phase_a_renames:
        phase_a_items.append(
            f"- Updated ({len(phase_a_renames)}) renamed parameter(s) to current standards:"
        )
        for rename in phase_a_renames:
            phase_a_items.append(f"-- {rename}")

    if phase_a_optional_missing:
        phase_a_items.append(
            f"- Updated ({len(phase_a_optional_missing)}) optional missing parameter(s) with null values:"
        )
        for param in phase_a_optional_missing:
            phase_a_items.append(f"-- {param}")

    if phase_a_not_in_standard:
        phase_a_items.append(
            f"- Found ({len(phase_a_not_in_standard)}) parameter(s) not in standard:"
        )
        for param in phase_a_not_in_standard:
            phase_a_items.append(f"-- {param}")

    if phase_a_items:
        report_lines.extend(phase_a_items)
        if warnings or (not adjustments and not errors):
            report_lines.append("")

    infos = [r for r in validation_results if r.status == "INFO"]

    if warnings:
        report_lines.append(f"- Revise ({len(warnings)}) warnings:")
        for warning in warnings:
            site_ref = (
                f" at site [{warning.site_gridid}]"
                if warning.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {warning.parameter}{site_ref}: {warning.message}")
        # Skip adding generic "passed" message when there are warnings
    else:
        if not adjustments and not errors:
            if not phase_a_items:
                report_lines.append("- All scientific validations passed")
                report_lines.append("- Model physics parameters are consistent")
                report_lines.append("- Geographic parameters are valid")
            # Skip generic messages when phase A items exist
        # Skip generic messages when there are no errors

    if infos:
        report_lines.append(f"- Note ({len(infos)}):")
        for info in infos:
            site_ref = (
                f" at site [{info.site_gridid}]"
                if info.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {info.parameter}{site_ref}: {info.message}")

    report_lines.append("")

    report_lines.append("# " + "=" * 50)

    return "\n".join(report_lines)


def print_critical_halt_message(critical_errors: List[ValidationResult]):
    """
    Print a critical halt message when Phase B detects errors that require Phase A.

    Parameters
    ----------
    critical_errors : List[ValidationResult]
        List of ERROR-level validation results that triggered the halt.

    Notes
    -----
    This function prints a clear message to the terminal, summarizing all critical
    scientific errors found during Phase B validation. It also provides guidance
    on how to resolve the issues, including running Phase A for automatic fixes.
    """
    print()
    print("=" * 60)
    print(" PHASE B HALTED - CRITICAL ERRORS DETECTED")
    print("=" * 60)
    print()
    print("Phase B detected critical scientific errors:")
    print()

    for error in critical_errors:
        site_ref = (
            f" at site [{error.site_gridid}]" if error.site_gridid is not None else ""
        )
        print(f"  [X] {error.parameter}{site_ref}")
        print(f"    {error.message}")
        if error.suggested_value is not None:
            print(f"    Suggested: {error.suggested_value}")
        print()

    print("OPTIONS TO RESOLVE:")
    print("1. Fix the issues manually in your YAML file, or")
    print("2. Run Phase A first to auto-detect and fix missing parameters:")
    print("   python suews_yaml_processor.py user.yml --phase A")
    print("3. Then re-run Phase B")
    print()
    print("Phase A can help detect missing parameters and provide")
    print("appropriate defaults for critical physics options.")
    print()
    print("=" * 60)


def print_science_check_results(
    validation_results: List[ValidationResult], adjustments: List[ScientificAdjustment]
):
    """
    Print concise terminal output for Phase B scientific validation results.

    Parameters
    ----------
    validation_results : list of ValidationResult
        List of validation results (errors, warnings, infos, passes).
    adjustments : list of ScientificAdjustment
        List of automatic scientific adjustments applied.

    Notes
    -----
    - Errors are printed with details and guidance.
    - Warnings and info messages are summarized.
    - Adjustment count is shown if any were applied.
    - Output is formatted for clarity in terminal use.
    """
    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]
    infos = [r for r in validation_results if r.status == "INFO"]

    if errors:
        print("PHASE B -- SCIENTIFIC ERRORS FOUND:")
        for error in errors:
            site_ref = (
                f" at site [{error.site_gridid}]"
                if error.site_gridid is not None
                else ""
            )
            print(f"  - {error.parameter}{site_ref}: {error.message}")
        print(
            "\nNext step: Check science_report_user.txt for detailed scientific guidance"
        )
    elif warnings:
        print(f"PHASE B -- SCIENTIFIC WARNINGS ({len(warnings)} found)")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")
        print("Check science_report_user.txt for details")
    else:
        print("PHASE B -- PASSED")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")

    if infos:
        for info in infos:
            site_ref = (
                f" at site [{info.site_gridid}]"
                if info.site_gridid is not None
                else ""
            )
            print(f"  Note: {info.parameter}{site_ref}: {info.message}")


def create_science_yaml_header(phase_a_performed: bool = True) -> str:
    """
    Create header for the final science-checked YAML file.

    Parameters
    ----------
    phase_a_performed : bool, optional
        Whether Phase A was performed before Phase B (default: True).

    Returns
    -------
    str
        Standardized header string for the science-checked YAML file.

    Notes
    -----
    - The header indicates that the file has been updated by the SUEWS processor.
    - It references the user-provided YAML and the existence of a detailed report.
    """
    # Use the standardized header format for all Phase B outputs
    header = """# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

"""
    return header


def run_science_check(
    uptodate_yaml_file: str,
    user_yaml_file: str,
    standard_yaml_file: str,
    science_yaml_file: str = None,
    science_report_file: str = None,
    phase_a_report_file: str = None,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
) -> dict:
    """
    Main Phase B workflow: perform scientific validation and scientific adjustments.

    Parameters
    ----------
    uptodate_yaml_file : str
        Path to the Phase A output YAML file (cleaned and updated user YAML).
    user_yaml_file : str
        Path to the original user-provided YAML file.
    standard_yaml_file : str
        Path to the standard reference YAML file.
    science_yaml_file : str, optional
        Path to write the science-checked output YAML file.
    science_report_file : str, optional
        Path to write the scientific validation report.
    phase_a_report_file : str, optional
        Path to the Phase A report file, if available.
    phase_a_performed : bool, default=True
        Whether Phase A was performed before Phase B.
    mode : str, default="public"
        Output mode for the report ("public" or other).
    phase : str, default="B"
        Validation phase identifier.

    Returns
    -------
    dict
        Final science-checked YAML configuration dictionary.

    Raises
    ------
    FileNotFoundError
        If any required input files are missing.
    ValueError
        If Phase A did not complete successfully or YAML is invalid.

    Notes
    -----
    - Runs all scientific validation and adjustment steps for Phase B.
    - Writes a detailed report and optionally the updated YAML file.
    - Halts with a clear message if critical scientific errors are detected.
    """
    try:
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )

        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)

        validation_context = ValidationContext(
            yaml_data=uptodate_data,
            start_date=start_date,
            model_year=model_year,
        )

        validation_results = RulesRegistry(
            context=validation_context
        ).run_validation()

    except (ValueError, FileNotFoundError, KeyError) as e:
        # Handle initialization failures and create error report
        error_message = str(e)

        # Create individual validation results for each error if multiple errors are present
        validation_results = []
        if ";" in error_message:
            # Multiple errors - split them and create separate ValidationResult objects
            individual_errors = error_message.split("; ")
            for error in individual_errors:
                validation_results.append(
                    ValidationResult(
                        status="ERROR",
                        category="INITIALIZATION",
                        parameter="model.control",
                        message=f"Phase B initialization failed: {error.strip()}",
                        suggested_value=None,
                    )
                )
        else:
            # Single error
            validation_results = [
                ValidationResult(
                    status="ERROR",
                    category="INITIALIZATION",
                    parameter="model.control",
                    message=f"Phase B initialization failed: {error_message}",
                    suggested_value=None,
                )
            ]

        # Create error report
        science_yaml_filename = (
            os.path.basename(science_yaml_file) if science_yaml_file else None
        )
        report_content = create_science_report(
            validation_results,
            [],  # No adjustments since we failed early
            science_yaml_filename,
            phase_a_report_file,
            mode,
            phase,
        )

        # Write error report file
        if science_report_file:
            REPORT_WRITER.write(science_report_file, report_content)

        # Re-raise the exception so orchestrator knows it failed
        raise e

    critical_errors = [r for r in validation_results if r.status == "ERROR"]
    if not critical_errors:
        science_checked_data, adjustments = run_scientific_adjustment_pipeline(
            uptodate_data, start_date, model_year
        )
    else:
        science_checked_data = deepcopy(uptodate_data)
        adjustments = []

    science_yaml_filename = (
        os.path.basename(science_yaml_file) if science_yaml_file else None
    )
    report_content = create_science_report(
        validation_results,
        adjustments,
        science_yaml_filename,
        phase_a_report_file,
        mode,
        phase,
    )

    if science_report_file:
        REPORT_WRITER.write(science_report_file, report_content)

    if critical_errors:
        print_critical_halt_message(critical_errors)
        raise ValueError("Critical scientific errors detected - Phase B halted")

    print_science_check_results(validation_results, adjustments)

    if science_yaml_file and not critical_errors:
        header = create_science_yaml_header(phase_a_performed)
        with open(science_yaml_file, "w", encoding="utf-8", newline="\n") as f:
            f.write(header)
            yaml.dump(
                science_checked_data, f, default_flow_style=False, sort_keys=False
            )

    return science_checked_data


def main():
    """Main entry point for science_check.py Phase B."""
    print(" SUEWS Scientific Validation (Phase B)")
    print("=" * 50)

    user_file = "src/supy/data_model/user.yml"
    uptodate_file = "src/supy/data_model/uptodate_user.yml"
    standard_file = "src/supy/sample_data/sample_config.yml"

    print(f"Phase A output (uptodate): {uptodate_file}")
    print(f"Original user YAML: {user_file}")
    print(f"Standard YAML: {standard_file}")
    print()

    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]

    science_yaml_filename = f"science_checked_{basename}"
    science_report_filename = f"science_report_{name_without_ext}.txt"

    science_yaml_file = os.path.join(dirname, science_yaml_filename)
    science_report_file = os.path.join(dirname, science_report_filename)

    try:
        science_checked_data = run_science_check(
            uptodate_yaml_file=uptodate_file,
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            science_yaml_file=science_yaml_file,
            science_report_file=science_report_file,
            phase_a_performed=True,  # Assumes Phase A was run (looking for uptodate_file)
            phase="B",
        )

        print(f"\nPhase B completed successfully!")
        print(f"Science-checked YAML: {science_yaml_file}")
        print(f"Science report: {science_report_file}")

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            # Critical errors already printed by print_critical_halt_message
            return 1
        else:
            print(f"\nPhase B failed: {e}")
            return 1
    except Exception as e:
        print(f"\nPhase B failed with unexpected error: {e}")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
