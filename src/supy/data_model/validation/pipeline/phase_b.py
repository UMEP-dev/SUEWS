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
    """Record of automatic scientific adjustment applied."""

    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


def get_site_gridid(site_data: dict) -> int:
    """Extract GRIDID from site data, handling both direct and RefValue formats."""
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
    """Validate Phase B inputs and load YAML files."""
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
    """Extract simulation parameters for validation."""
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

def run_scientific_validation_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> List[ValidationResult]:
    """Execute all scientific validation checks."""
    validation_results = []

    validation_context = ValidationContext(
        yaml_data=yaml_data,
        model_year=model_year,
    )

    for rule_id, rule_fn in RulesRegistry().phase_b.items():
        validation_results.extend(rule_fn(validation_context))

    return validation_results


def get_mean_monthly_air_temperature(
    lat: float, lon: float, month: int, spatial_res: float = 0.5
) -> Optional[float]:
    """Calculate monthly air temperature using CRU TS4.06 data.

    Wrapper around yaml_helpers.get_mean_monthly_air_temperature that returns
    None if CRU data is not available (e.g., in standalone mode).

    Raises:
        ValueError: If input parameters are invalid (month, lat, lon out of range)
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


def get_mean_annual_air_temperature(
    lat: float, lon: float, spatial_res: float = 0.5
) -> Optional[float]:
    """Calculate annual mean air temperature using CRU TS4.06 climate normals.

    Wrapper around yaml_helpers.get_mean_annual_air_temperature that returns
    None if CRU data is not available (e.g., in standalone mode).

    Args:
        lat: Latitude in degrees (-90 to 90)
        lon: Longitude in degrees (-180 to 180)
        spatial_res: Spatial resolution in degrees for finding nearest grid cell (default: 0.5)

    Returns:
        Annual mean temperature in Celsius based on 1991-2020 climate normals,
        or None if CRU data not available

    Raises:
        ValueError: If input parameters are invalid (lat, lon out of range)
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
    Update temperature, tsfc, and tin parameters in a dictionary element.

    Args:
        element: Dictionary containing temperature parameters
        avg_temp: Target temperature value

    Returns:
        Tuple of (updated element dict, list of parameter names that were updated)
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
    """Set initial surface temperatures based on location and season."""
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
        for key in ("InitialOutdoorTemperature", "InitialIndoorTemperature",):
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

        # Update STEBBS OutdoorAirAnnualTemperature using annual mean from CRU data
        annual_temp = get_mean_annual_air_temperature(lat, lng)
        if annual_temp is not None and "AnnualMeanAirTemperature" in stebbs:
            if isinstance(stebbs["AnnualMeanAirTemperature"], dict):
                old_annual_val = stebbs["AnnualMeanAirTemperature"].get("value")
                if old_annual_val != annual_temp:
                    stebbs["AnnualMeanAirTemperature"]["value"] = annual_temp
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="stebbs.AnnualMeanAirTemperature",
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
    """Auto-fix small floating point errors in surface fractions."""
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
    """Nullify parameters for disabled model options."""
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})

    # --- STEBBS ---
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

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
    emissionsmethod = get_value_safe(physics, "emissionsmethod")

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
    Determine season based on day of year and latitude.

    Args:
        doy: Day of year (1-365/366)
        lat: Latitude in degrees

    Returns:
        Season string: 'equatorial', 'tropical', 'summer', 'winter', 'spring', 'fall'
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
    """Determine season based on start date and latitude."""
    try:
        start = datetime.strptime(start_date, "%Y-%m-%d").timetuple().tm_yday
    except ValueError:
        raise ValueError("start_date must be in YYYY-MM-DD format")

    return get_season_from_doy(start, lat)


def adjust_seasonal_parameters(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply seasonal adjustments including LAI, snowalb, and DLS calculations."""
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
                laimin = lai.get("laimin", {}).get("value")
                laimax = lai.get("laimax", {}).get("value")

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

                    current_lai = initial_states["dectr"].get("lai_id", {}).get("value")
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
    """If rcmethod == 0, set RoofOuterCapFrac and WallOuterCapFrac to 0.5 for all sites."""
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})
    rcmethod_value = get_value_safe(physics, "rcmethod")

    if rcmethod_value == 0:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            site_gridid = get_site_gridid(site)

            # RoofOuterCapFrac
            roof_frac_entry = building_archetype.get("RoofOuterCapFrac", {})
            old_roof_frac = roof_frac_entry.get("value") if isinstance(roof_frac_entry, dict) else roof_frac_entry
            if old_roof_frac != 0.5:
                building_archetype["RoofOuterCapFrac"] = {"value": 0.5}
                adjustments.append(
                    ScientificAdjustment(
                        parameter="building_archetype.RoofOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(old_roof_frac),
                        new_value="0.5",
                        reason="rcmethod == 0, set RoofOuterCapFrac to 0.5"
                    )
                )

            # WallOuterCapFrac
            wall_frac_entry = building_archetype.get("WallOuterCapFrac", {})
            old_wall_frac = wall_frac_entry.get("value") if isinstance(wall_frac_entry, dict) else wall_frac_entry
            if old_wall_frac != 0.5:
                building_archetype["WallOuterCapFrac"] = {"value": 0.5}
                adjustments.append(
                    ScientificAdjustment(
                        parameter="building_archetype.WallOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(old_wall_frac),
                        new_value="0.5",
                        reason="rcmethod == 0, set WallOuterCapFrac to 0.5"
                    )
                )

            props["building_archetype"] = building_archetype
            site["properties"] = props
            yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments

def adjust_model_option_stebbsmethod(yaml_data: dict) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Adjusts stebbs-related parameters according to stebbsmethod options.

    - If 'stebbsmethod' is 1 and 'WWR' is 0.0 for a site, all window-related parameters are set to None.
    - If 'stebbsmethod' is 1 and 'WWR' is 1.0 for a site, all external wall-related parameters are set to None.

    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            bldgarc = props.get("building_archetype", {})

            site_gridid = get_site_gridid(site)

            wwr_entry = bldgarc.get("WWR", {})
            wwr = wwr_entry.get("value") if isinstance(wwr_entry, dict) else wwr_entry

            if wwr == 0.0:
                window_params_stebbs = [
                    "WindowInternalConvectionCoefficient",
                    "WindowExternalConvectionCoefficient",
                ]
                window_params_bldgarc = [
                    "WindowThickness",
                    "WindowEffectiveConductivity",
                    "WindowDensity",
                    "WindowCp",
                    "WindowExternalEmissivity",
                    "WindowInternalEmissivity",
                    "WindowTransmissivity",
                    "WindowAbsorbtivity",
                    "WindowReflectivity",
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
                    "WallExternalConvectionCoefficient",
                    "WallInternalConvectionCoefficient",
                    ]
                wall_params_bldgarc = [
                    "WallExternalEmissivity",
                    "WallInternalEmissivity",
                    "WallTransmissivity",
                    "WallAbsorbtivity",
                    "WallReflectivity",
                    "WallThickness",
                    "WallEffectiveConductivity",
                    "WallDensity",
                    "WallCp",
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
    """Apply automatic scientific corrections and adjustments."""
    updated_data = deepcopy(yaml_data)
    adjustments = []

    for adjust_func, args in [
        (adjust_surface_temperatures, (updated_data, start_date)),
        (adjust_land_cover_fractions, (updated_data,)),
        (adjust_model_dependent_nullification, (updated_data,)),
        (adjust_seasonal_parameters, (updated_data, start_date, model_year)),
        (adjust_model_option_rcmethod, (updated_data,)),
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
    """Generate comprehensive scientific validation report."""
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
    Print critical halt message when Phase B detects errors requiring Phase A.

    Args:
        critical_errors: List of ERROR-level validation results
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
    Print clean terminal output for Phase B results.

    Args:
        validation_results: List of validation results
        adjustments: List of automatic adjustments applied
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
    """Create header for final science-checked YAML file.

    Args:
        phase_a_performed: Whether Phase A was performed before Phase B
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
    Main Phase B workflow - perform scientific validation and adjustments.

    Args:
        uptodate_yaml_file: Path to Phase A output (clean YAML)
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for science-checked output YAML
        science_report_file: Path for scientific validation report
        phase_a_report_file: Path to Phase A report file (if available)
        phase_a_performed: Whether Phase A was performed before Phase B

    Returns:
        Final science-checked YAML configuration dictionary

    Raises:
        FileNotFoundError: If required input files are missing
        ValueError: If Phase A did not complete or YAML is invalid
    """
    try:
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )

        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)

        validation_results = run_scientific_validation_pipeline(
            uptodate_data, start_date, model_year
        )
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
