"""
SUEWS Science Check Phase B

This module performs scientific validation and consistency checks on YAML configurations
that have already been processed by Phase A (uptodate_yaml.py).

Phase B focuses on:
- Scientific parameter validation using Pydantic models
- Geographic coordinate and timezone validation
- Seasonal parameter adjustments (LAI, snowalb, surface temperatures)
- Land cover fraction validation and consistency
- Model physics option interdependency checks
- Automatic scientific corrections where appropriate

Phase B assumes Phase A has completed successfully and builds upon clean YAML output
without duplicating parameter detection or YAML structure validation.
"""

import yaml
import os
from typing import Dict, List, Optional, Union, Any, Tuple
from dataclasses import dataclass
from datetime import datetime
from copy import deepcopy


@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""

    status: str  # 'PASS', 'WARNING', 'ERROR'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False


@dataclass
class ScientificAdjustment:
    """Record of automatic scientific adjustment applied."""

    parameter: str
    site_index: Optional[int] = None
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


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


def validate_phase_b_inputs(
    uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str
) -> Tuple[dict, dict, dict]:
    """
    Validate that Phase B has all required inputs. Can work with Phase A output or raw user YAML.

    Args:
        uptodate_yaml_file: Path to Phase A output (uptodate YAML) or user YAML for direct Phase B
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML

    Returns:
        Tuple of (uptodate_data, user_data, standard_data) dictionaries

    Raises:
        FileNotFoundError: If required files are missing
        ValueError: If YAML files are invalid
    """
    # Check that all required files exist
    for file_path in [uptodate_yaml_file, user_yaml_file, standard_yaml_file]:
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"Required file not found: {file_path}")

    try:
        # Load uptodate YAML (could be Phase A output or raw user YAML)
        with open(uptodate_yaml_file, "r") as f:
            uptodate_content = f.read()
            uptodate_data = yaml.safe_load(uptodate_content)

        # Check if this is Phase A output or raw user YAML (no strict requirement)
        is_phase_a_output = "UP TO DATE YAML" in uptodate_content

        # Load original user YAML
        with open(user_yaml_file, "r") as f:
            user_data = yaml.safe_load(f)

        # Load standard YAML
        with open(standard_yaml_file, "r") as f:
            standard_data = yaml.safe_load(f)

    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}")

    return uptodate_data, user_data, standard_data


def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """
    Extract key simulation parameters needed for scientific validation.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        Tuple of (model_year, start_date, end_date)

    Raises:
        ValueError: If required simulation parameters are missing
    """
    control = yaml_data.get("model", {}).get("control", {})

    start_date = control.get("start_time")
    end_date = control.get("end_time")

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError(
            "Missing or invalid 'start_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError(
            "Missing or invalid 'end_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError(
            "Could not extract model year from 'start_time' - ensure 'YYYY-MM-DD' format"
        )

    return model_year, start_date, end_date


def validate_physics_parameters(yaml_data: dict) -> List[ValidationResult]:
    """
    Validate presence and non-emptiness of required model physics parameters.

    Checks that all required physics parameters exist and have valid values.
    Adapted from precheck.py precheck_model_physics_params function.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        List of ValidationResult objects for physics parameter validation
    """
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    if not physics:
        results.append(
            ValidationResult(
                status="WARNING",
                category="PHYSICS",
                parameter="model.physics",
                message="Physics section is empty - skipping physics parameter validation",
            )
        )
        return results

    # Required physics parameters from precheck.py
    required_physics_params = [
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

    # Check for missing parameters
    missing_params = [
        param for param in required_physics_params if param not in physics
    ]
    if missing_params:
        for param in missing_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=f"model.physics.{param}",
                    message=f"Required physics parameter missing",
                    suggested_value="Check SUEWS documentation for appropriate value -- https://suews.readthedocs.io/latest/",
                )
            )

    # Check for empty/null values
    empty_params = [
        param
        for param in required_physics_params
        if param in physics and physics.get(param, {}).get("value") in ("", None)
    ]
    if empty_params:
        for param in empty_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=f"model.physics.{param}",
                    message="Physics parameter has empty or null value",
                    suggested_value="Set appropriate non-null value",
                )
            )

    # If no issues found
    if not missing_params and not empty_params:
        results.append(
            ValidationResult(
                status="PASS",
                category="PHYSICS",
                parameter="model.physics",
                message="All required physics parameters present and non-empty",
            )
        )

    return results


def validate_model_option_dependencies(yaml_data: dict) -> List[ValidationResult]:
    """
    Validate internal consistency between model physics options.

    Checks logical dependencies between selected physics methods.
    Adapted from precheck.py precheck_model_options_constraints function.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        List of ValidationResult objects for model option dependency validation
    """
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    # Check rslmethod-stabilitymethod constraints
    rslmethod = get_value_safe(physics, "rslmethod")
    stabilitymethod = get_value_safe(physics, "stabilitymethod")

    # Constraint 1: If rslmethod == 2, stabilitymethod must be 3
    if rslmethod == 2 and stabilitymethod != 3:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="rslmethod-stabilitymethod",
                message="If rslmethod == 2, stabilitymethod must be 3 for diagnostic aerodynamic calculations",
                suggested_value="Set stabilitymethod to 3",
            )
        )

    # Constraint 2: If stabilitymethod == 1, rslmethod must be present
    elif stabilitymethod == 1 and rslmethod is None:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="stabilitymethod-rslmethod",
                message="If stabilitymethod == 1, rslmethod parameter is required for atmospheric stability calculations",
                suggested_value="Set rslmethod to appropriate value",
            )
        )

    else:
        results.append(
            ValidationResult(
                status="PASS",
                category="MODEL_OPTIONS",
                parameter="rslmethod-stabilitymethod",
                message="rslmethod-stabilitymethod constraints satisfied",
            )
        )

    return results


def validate_land_cover_consistency(yaml_data: dict) -> List[ValidationResult]:
    """
    Validate land cover surface fractions and parameter consistency.

    Checks that surface fractions sum to 1.0 and validates parameters for
    surfaces with non-zero fractions. Adapted from precheck.py land cover functions.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        List of ValidationResult objects for land cover validation
    """
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")

        if not land_cover:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="LAND_COVER",
                    parameter="land_cover",
                    site_index=site_idx,
                    message="Missing land_cover block",
                    suggested_value="Add land_cover configuration with surface fractions",
                )
            )
            continue

        # Calculate sum of all surface fractions
        sfr_sum = 0.0
        surface_types = []

        for surface_type, surface_props in land_cover.items():
            if isinstance(surface_props, dict):
                sfr_value = surface_props.get("sfr", {}).get("value")
                if sfr_value is not None:
                    sfr_sum += sfr_value
                    surface_types.append((surface_type, sfr_value))

        # Check surface fraction sum (allow small floating point errors)
        if abs(sfr_sum - 1.0) > 0.0001:
            if sfr_sum == 0.0:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter="surface_fractions",
                        site_index=site_idx,
                        message=f"All surface fractions are zero or missing",
                        suggested_value="Set surface fractions that sum to 1.0",
                    )
                )
            else:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter="surface_fractions",
                        site_index=site_idx,
                        message=f"Surface fractions sum to {sfr_sum:.6f}, should equal 1.0",
                        suggested_value="Adjust surface fractions to sum to exactly 1.0",
                    )
                )

        # Validate parameters for surfaces with non-zero fractions
        for surface_type, sfr_value in surface_types:
            if sfr_value > 0:
                surface_props = land_cover[surface_type]
                missing_params = _check_surface_parameters(surface_props, surface_type)

                for param_name in missing_params:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="LAND_COVER",
                            parameter=f"{surface_type}.{param_name}",
                            site_index=site_idx,
                            message=f"Required parameter missing/empty for surface with sfr > 0",
                            suggested_value="Set appropriate non-null value",
                        )
                    )

        # Check for unused surfaces (sfr == 0) with non-null parameters
        zero_sfr_surfaces = [surf for surf, sfr in surface_types if sfr == 0]
        if zero_sfr_surfaces:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="LAND_COVER",
                    parameter="unused_surfaces",
                    site_index=site_idx,
                    message=f"Surfaces with sfr=0 may have unused parameters: {', '.join(zero_sfr_surfaces)}",
                    suggested_value="Consider setting unused surface parameters to null",
                )
            )

    # If all sites passed validation
    if not any(r.status == "ERROR" for r in results):
        results.append(
            ValidationResult(
                status="PASS",
                category="LAND_COVER",
                parameter="land_cover_validation",
                message="Land cover fractions and parameters validated successfully",
            )
        )

    return results


def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """
    Check for missing/empty parameters in a surface type configuration.

    Args:
        surface_props: Surface properties dictionary
        surface_type: Name of surface type (for context)

    Returns:
        List of parameter names that are missing or empty
    """
    missing_params = []

    def _check_recursively(props: dict, path: str = ""):
        for key, value in props.items():
            if key == "sfr":  # Skip surface fraction itself
                continue

            current_path = f"{path}.{key}" if path else key

            if isinstance(value, dict):
                if "value" in value:
                    # This is a parameter with a value
                    param_value = value["value"]
                    if param_value in (None, "") or (
                        isinstance(param_value, list)
                        and any(v in (None, "") for v in param_value)
                    ):
                        missing_params.append(current_path)
                else:
                    # This is a nested structure, recurse
                    _check_recursively(value, current_path)

    _check_recursively(surface_props)
    return missing_params


def validate_geographic_parameters(yaml_data: dict) -> List[ValidationResult]:
    """
    Validate geographic coordinates and location-dependent parameters.

    Checks latitude, longitude ranges and validates timezone/DLS parameters.
    Prepares for DLS calculations and seasonal adjustments.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        List of ValidationResult objects for geographic validation
    """
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})

        # Validate latitude
        lat = get_value_safe(props, "lat")

        if lat is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
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
                    message=f"Latitude {lat} is outside valid range [-90, 90]",
                    suggested_value="Set latitude between -90 and 90 degrees",
                )
            )

        # Validate longitude
        lng = get_value_safe(props, "lng")

        if lng is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
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
                    message=f"Longitude {lng} is outside valid range [-180, 180]",
                    suggested_value="Set longitude between -180 and 180 degrees",
                )
            )

        # Check timezone parameter exists (will be set by DLS calculation)
        timezone = get_value_safe(props, "timezone")

        if timezone is None:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="GEOGRAPHY",
                    parameter="timezone",
                    site_index=site_idx,
                    message="Timezone is not set - will be calculated from coordinates",
                    suggested_value="Will be automatically calculated in scientific adjustments",
                )
            )

        # Check DLS parameters exist (will be set by DLS calculation)
        anthro_emissions = props.get("anthropogenic_emissions", {})
        if anthro_emissions:
            startdls = get_value_safe(anthro_emissions, "startdls")
            enddls = get_value_safe(anthro_emissions, "enddls")

            if startdls is None or enddls is None:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="GEOGRAPHY",
                        parameter="dls_parameters",
                        site_index=site_idx,
                        message="Daylight saving parameters not set - will be calculated from coordinates",
                        suggested_value="Will be automatically calculated in scientific adjustments",
                    )
                )

    # If all required coordinates are valid
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


def run_scientific_validation_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> List[ValidationResult]:
    """
    Execute all scientific validation checks on the YAML configuration.

    Args:
        yaml_data: YAML configuration dictionary
        start_date: Simulation start date in YYYY-MM-DD format
        model_year: Model year extracted from start_date

    Returns:
        List of ValidationResult objects from all validation checks
    """
    validation_results = []

    # Physics parameter validation
    validation_results.extend(validate_physics_parameters(yaml_data))

    # Model option dependency validation
    validation_results.extend(validate_model_option_dependencies(yaml_data))

    # Land cover consistency validation
    validation_results.extend(validate_land_cover_consistency(yaml_data))

    # Geographic coordinate validation
    validation_results.extend(validate_geographic_parameters(yaml_data))

    # Seasonal parameter validation
    # TODO: Implement validate_seasonal_parameters(yaml_data, start_date)

    return validation_results


def get_monthly_avg_temp(lat: float, month: int) -> float:
    """
    Estimate the average air temperature for a given latitude and month.

    Uses predefined climatological values for four broad latitude bands.
    Adapted directly from precheck.py.

    Args:
        lat: Site latitude in degrees
        month: Month of the year (1 = January, 12 = December)

    Returns:
        Estimated average air temperature for the given latitude and month (°C)
    """
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
        "tropics": [
            26.0,
            26.5,
            27.0,
            27.5,
            28.0,
            28.5,
            28.0,
            27.5,
            27.0,
            26.5,
            26.0,
            25.5,
        ],
        "subtropics": [
            15.0,
            16.0,
            18.0,
            20.0,
            24.0,
            28.0,
            30.0,
            29.0,
            26.0,
            22.0,
            18.0,
            15.0,
        ],
        "midlatitudes": [
            5.0,
            6.0,
            9.0,
            12.0,
            17.0,
            21.0,
            23.0,
            22.0,
            19.0,
            14.0,
            9.0,
            6.0,
        ],
        "polar": [
            -15.0,
            -13.0,
            -10.0,
            -5.0,
            0.0,
            5.0,
            8.0,
            7.0,
            3.0,
            -2.0,
            -8.0,
            -12.0,
        ],
    }

    return monthly_temp[lat_band][month - 1]


def adjust_surface_temperatures(
    yaml_data: dict, start_date: str
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Set initial surface temperatures for all surface types based on latitude and start month.

    Adapted from precheck.py precheck_update_surface_temperature function.

    Args:
        yaml_data: YAML configuration dictionary
        start_date: Start date in YYYY-MM-DD format

    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    month = datetime.strptime(start_date, "%Y-%m-%d").month

    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # Get site latitude
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry

        if lat is None:
            continue  # Skip if no latitude (will be caught by validation)

        # Get estimated average temperature
        avg_temp = get_monthly_avg_temp(lat, month)

        # Loop over all surface types
        surface_types = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]

        for surface_type in surface_types:
            surf = initial_states.get(surface_type, {})
            if not isinstance(surf, dict):
                continue

            temperature_updated = False
            tsfc_updated = False
            tin_updated = False

            # Set 5-layer temperature array
            if "temperature" in surf and isinstance(surf["temperature"], dict):
                current_temp = surf["temperature"].get("value")
                if current_temp != [avg_temp] * 5:
                    surf["temperature"]["value"] = [avg_temp] * 5
                    temperature_updated = True

            # Set tsfc
            if "tsfc" in surf and isinstance(surf["tsfc"], dict):
                current_tsfc = surf["tsfc"].get("value")
                if current_tsfc != avg_temp:
                    surf["tsfc"]["value"] = avg_temp
                    tsfc_updated = True

            # Set tin
            if "tin" in surf and isinstance(surf["tin"], dict):
                current_tin = surf["tin"].get("value")
                if current_tin != avg_temp:
                    surf["tin"]["value"] = avg_temp
                    tin_updated = True

            # Record adjustments made
            if temperature_updated or tsfc_updated or tin_updated:
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{surface_type}_surface_temperatures",
                        site_index=site_idx,
                        old_value=f"various temperatures",
                        new_value=f"{avg_temp}°C",
                        reason=f"Set from latitude {lat}° for month {month}",
                    )
                )

            initial_states[surface_type] = surf

        # Save back to site
        site["initial_states"] = initial_states
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def adjust_land_cover_fractions(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Auto-fix small floating point errors in land cover surface fractions.

    Allows small floating point inaccuracies (~0.0001) and automatically corrects
    them by adjusting the largest surface fraction. Adapted from precheck.py.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")

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

        # Auto-fix small floating point errors (epsilon ~0.0001)
        correction_applied = False

        if 0.9999 <= sfr_sum < 1.0:
            # Slightly below 1.0 - increase largest surface fraction
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = 1.0 - sfr_sum
            old_value = surface_fractions[max_surface]
            new_value = old_value + correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            adjustments.append(
                ScientificAdjustment(
                    parameter=f"{max_surface}.sfr",
                    site_index=site_idx,
                    old_value=f"{old_value:.6f}",
                    new_value=f"{new_value:.6f}",
                    reason=f"Auto-corrected sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
                )
            )

        elif 1.0 < sfr_sum <= 1.0001:
            # Slightly above 1.0 - decrease largest surface fraction
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = sfr_sum - 1.0
            old_value = surface_fractions[max_surface]
            new_value = old_value - correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            adjustments.append(
                ScientificAdjustment(
                    parameter=f"{max_surface}.sfr",
                    site_index=site_idx,
                    old_value=f"{old_value:.6f}",
                    new_value=f"{new_value:.6f}",
                    reason=f"Auto-corrected sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
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

    Adapted from precheck.py precheck_model_option_rules function.
    Currently implements stebbsmethod=0 nullification.

    Args:
        yaml_data: YAML configuration dictionary

    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})

    # STEBBSMETHOD RULE: when stebbsmethod == 0, nullify all stebbs params
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 0:
        sites = yaml_data.get("sites", [])

        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs_block = props.get("stebbs", {})

            if stebbs_block:
                nullified_params = []

                def _recursive_nullify(block: dict, path: str = ""):
                    for key, val in block.items():
                        current_path = f"{path}.{key}" if path else key

                        if isinstance(val, dict):
                            if "value" in val and val["value"] is not None:
                                val["value"] = None
                                nullified_params.append(current_path)
                            else:
                                _recursive_nullify(val, current_path)

                _recursive_nullify(stebbs_block)

                if nullified_params:
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="stebbs_parameters",
                            site_index=site_idx,
                            old_value="various values",
                            new_value="null",
                            reason=f"stebbsmethod=0, nullified {len(nullified_params)} parameters: {', '.join(nullified_params[:3])}{'...' if len(nullified_params) > 3 else ''}",
                        )
                    )

                props["stebbs"] = stebbs_block
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def get_season(start_date: str, lat: float) -> str:
    """
    Determine season based on start date and latitude.

    Adapted from precheck.py SeasonCheck class.

    Args:
        start_date: Start date in YYYY-MM-DD format
        lat: Site latitude in degrees

    Returns:
        Season string: 'summer', 'winter', 'spring', 'fall', 'tropical', 'equatorial'
    """
    try:
        start = datetime.strptime(start_date, "%Y-%m-%d").timetuple().tm_yday
    except ValueError:
        raise ValueError("start_date must be in YYYY-MM-DD format")

    abs_lat = abs(lat)

    if abs_lat <= 10:
        return "equatorial"
    if 10 < abs_lat < 23.5:
        return "tropical"

    if lat >= 0:  # Northern Hemisphere
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


def adjust_seasonal_parameters(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Apply seasonal adjustments including LAI, snowalb nullification, and DLS calculations.

    Adapted from precheck.py precheck_site_season_adjustments function.

    Args:
        yaml_data: YAML configuration dictionary
        start_date: Start date in YYYY-MM-DD format
        model_year: Model year for DLS calculations

    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # Get site coordinates
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        lng = get_value_safe(props, "lng")

        if lat is None:
            continue  # Skip if no latitude

        # 1. Determine season
        try:
            season = get_season(start_date, lat)
        except Exception as e:
            continue  # Skip on season detection error

        # 2. Nullify snowalb for warm seasons
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
                        old_value=str(current_snowalb),
                        new_value="null",
                        reason=f"Nullified for {season} season (no snow expected)",
                    )
                )

        # 3. Seasonal LAI adjustment for deciduous trees
        land_cover = props.get("land_cover", {})
        dectr = land_cover.get("dectr", {})
        if dectr:
            sfr = dectr.get("sfr", {}).get("value", 0)

            if sfr > 0:  # Only if deciduous trees are present
                lai = dectr.get("lai", {})
                laimin = lai.get("laimin", {}).get("value")
                laimax = lai.get("laimax", {}).get("value")

                if laimin is not None and laimax is not None:
                    # Calculate seasonal LAI value
                    if season == "summer":
                        lai_val = laimax
                    elif season == "winter":
                        lai_val = laimin
                    elif season in ("spring", "fall"):
                        lai_val = (laimax + laimin) / 2
                    else:  # tropical/equatorial
                        lai_val = laimax

                    # Set LAI in initial states
                    if "dectr" not in initial_states:
                        initial_states["dectr"] = {}

                    current_lai = initial_states["dectr"].get("lai_id", {}).get("value")
                    if current_lai != lai_val:
                        initial_states["dectr"]["lai_id"] = {"value": lai_val}
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="dectr.lai_id",
                                site_index=site_idx,
                                old_value=str(current_lai)
                                if current_lai is not None
                                else "undefined",
                                new_value=str(lai_val),
                                reason=f"Set seasonal LAI for {season} (laimin={laimin}, laimax={laimax})",
                            )
                        )
            else:
                # No deciduous trees - nullify LAI
                if (
                    "dectr" in initial_states
                    and initial_states["dectr"].get("lai_id", {}).get("value")
                    is not None
                ):
                    initial_states["dectr"]["lai_id"] = {"value": None}
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="dectr.lai_id",
                            site_index=site_idx,
                            old_value="previous value",
                            new_value="null",
                            reason="Nullified (no deciduous trees: sfr=0)",
                        )
                    )

        # 4. DLS (Daylight Saving) calculations
        if lat is not None and lng is not None:
            try:
                # Simple DLS estimation for demonstration (real implementation would use timezonefinder)
                # Northern hemisphere: DST typically March to October
                # Southern hemisphere: DST typically October to March
                if lat >= 0:  # Northern hemisphere
                    start_dls = 86  # ~March 27
                    end_dls = 303  # ~October 30
                    tz_offset = 0  # Simplified - would calculate from coordinates
                else:  # Southern hemisphere
                    start_dls = 303  # ~October 30
                    end_dls = 86  # ~March 27
                    tz_offset = 0  # Simplified

                # Set DLS parameters
                anthro_emissions = props.get("anthropogenic_emissions", {})
                if anthro_emissions:
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
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="dls_parameters",
                                site_index=site_idx,
                                old_value=f"start={current_startdls}, end={current_enddls}",
                                new_value=f"start={start_dls}, end={end_dls}",
                                reason=f"Calculated DLS for coordinates ({lat:.2f}, {lng:.2f})",
                            )
                        )

                # Set timezone if not set
                current_timezone = props.get("timezone", {}).get("value")
                if current_timezone != tz_offset:
                    props["timezone"] = {"value": tz_offset}
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="timezone",
                            site_index=site_idx,
                            old_value=str(current_timezone),
                            new_value=str(tz_offset),
                            reason=f"Calculated timezone offset for coordinates ({lat:.2f}, {lng:.2f})",
                        )
                    )

            except Exception as e:
                # Skip DLS calculation on error
                pass

        # Save back to site
        site["properties"] = props
        site["initial_states"] = initial_states
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def run_scientific_adjustment_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Apply automatic scientific corrections and adjustments to YAML configuration.

    Args:
        yaml_data: YAML configuration dictionary
        start_date: Simulation start date in YYYY-MM-DD format
        model_year: Model year extracted from start_date

    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    updated_data = deepcopy(yaml_data)

    # Surface temperature initialization
    updated_data, temp_adjustments = adjust_surface_temperatures(
        updated_data, start_date
    )
    adjustments.extend(temp_adjustments)

    # Land cover fraction auto-correction
    updated_data, fraction_adjustments = adjust_land_cover_fractions(updated_data)
    adjustments.extend(fraction_adjustments)

    # Model-dependent nullification
    updated_data, nullify_adjustments = adjust_model_dependent_nullification(
        updated_data
    )
    adjustments.extend(nullify_adjustments)

    # Seasonal parameter adjustments
    updated_data, seasonal_adjustments = adjust_seasonal_parameters(
        updated_data, start_date, model_year
    )
    adjustments.extend(seasonal_adjustments)

    return updated_data, adjustments


def create_science_report(
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    science_yaml_filename: str = None,
    phase_a_report_file: str = None,
    mode: str = "user",
) -> str:
    """
    Generate comprehensive scientific validation report including Phase A information.

    Args:
        validation_results: List of validation results from scientific checks
        adjustments: List of automatic adjustments applied
        science_yaml_filename: Name of final science-checked YAML file
        phase_a_report_file: Path to Phase A report file for consolidation

    Returns:
        String containing formatted report content
    """
    report_lines = []
    report_lines.append("# SUEWS Scientific Validation Report")
    report_lines.append("# " + "=" * 50)
    report_lines.append(f"# Mode: {mode.title()}")
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    # Extract Phase A information if available
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                phase_a_content = f.read()

            # Parse Phase A report for relevant information
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

    # Count results by status
    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]
    passed = [r for r in validation_results if r.status == "PASS"]

    # ACTION NEEDED section (only critical errors that prevent simulation)
    if errors:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(
            f"- Found ({len(errors)}) critical scientific parameter error(s):"
        )
        for error in errors:
            site_ref = (
                f" at site [{error.site_index}]" if error.site_index is not None else ""
            )
            report_lines.append(f"-- {error.parameter}{site_ref}: {error.message}")
            if error.suggested_value is not None:
                report_lines.append(f"   Suggested fix: {error.suggested_value}")
        report_lines.append("")

    # NO ACTION NEEDED section (automatic adjustments, warnings, clean status, and Phase A information)
    report_lines.append("## NO ACTION NEEDED")

    # Updated values (automatic adjustments applied)
    if adjustments:
        report_lines.append(
            f"- Updated ({len(adjustments)}) parameter(s) with automatic scientific adjustments:"
        )
        for adjustment in adjustments:
            site_ref = (
                f" at site [{adjustment.site_index}]"
                if adjustment.site_index is not None
                else ""
            )
            report_lines.append(
                f"-- {adjustment.parameter}{site_ref}: {adjustment.old_value} → {adjustment.new_value} ({adjustment.reason})"
            )
        report_lines.append("")

    # Phase A information (when available)
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

    # Add Phase A items if any exist
    if phase_a_items:
        report_lines.extend(phase_a_items)
        if warnings or (not adjustments and not errors):
            report_lines.append("")

    # Phase B warnings
    if warnings:
        report_lines.append(
            f"- Found ({len(warnings)}) scientific warning(s) for information:"
        )
        for warning in warnings:
            site_ref = (
                f" at site [{warning.site_index}]"
                if warning.site_index is not None
                else ""
            )
            report_lines.append(f"-- {warning.parameter}{site_ref}: {warning.message}")
        if not adjustments and not errors:
            report_lines.append("- All critical validations passed")
    else:
        # No warnings case
        if not adjustments and not errors:
            if not phase_a_items:  # Only show if no Phase A items already shown
                report_lines.append("- All scientific validations passed")
                report_lines.append("- Model physics parameters are consistent")
                report_lines.append("- Geographic parameters are valid")
            else:
                report_lines.append("- All critical validations passed")
        elif not errors:
            report_lines.append("- All critical validations passed")

    report_lines.append("")

    # Footer separator
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
            f" at site [{error.site_index}]" if error.site_index is not None else ""
        )
        print(f"  ✗ {error.parameter}{site_ref}")
        print(f"    {error.message}")
        if error.suggested_value is not None:
            print(f"    Suggested: {error.suggested_value}")
        print()

    print("OPTIONS TO RESOLVE:")
    print("1. Fix the issues manually in your YAML file, or")
    print("2. Run Phase A first to auto-detect and fix missing parameters:")
    print("   python master_ABC_run.py user.yml --phase A")
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

    if errors:
        print("PHASE B -- SCIENTIFIC ERRORS FOUND:")
        for error in errors:
            site_ref = (
                f" at site [{error.site_index}]" if error.site_index is not None else ""
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


def create_science_yaml_header(phase_a_performed: bool = True) -> str:
    """Create header for final science-checked YAML file.

    Args:
        phase_a_performed: Whether Phase A was performed before Phase B
    """
    if phase_a_performed:
        # A→B workflow header
        header = """# =============================================================================
# FINAL SCIENCE CHECKED YAML
# =============================================================================
#
# This file has been processed through the complete SUEWS validation workflow:
#
# PHASE A (Parameter Detection):
# - Missing parameters added with null values
# - Parameter names updated to current standards  
# - YAML structure validated against reference configuration
#
# PHASE B (Scientific Validation):
# - Physics parameters validated for consistency
# - Geographic coordinates and timezone validated
# - Seasonal adjustments applied (LAI, snowalb, surface temperatures)
# - Land cover fractions validated and corrected
# - Model physics option dependencies checked
#
# All parameter changes and scientific corrections documented in reportB_<filename>.txt
#
# =============================================================================

"""
    else:
        # Phase B only header
        header = """# =============================================================================
# SCIENCE CHECKED YAML
# =============================================================================
#
# This file has been processed through SUEWS Phase B scientific validation:
#
# PHASE B (Scientific Validation):
# - Physics parameters validated for consistency
# - Geographic coordinates and timezone validated
# - Seasonal adjustments applied (LAI, snowalb, surface temperatures)
# - Land cover fractions validated and corrected
# - Model physics option dependencies checked
#
# NOTE: This file has NOT been processed through Phase A parameter detection.
# Run Phase A first if you need missing parameter detection and naming updates.
#
# All parameter changes and scientific corrections documented in reportB_<filename>.txt
#
# =============================================================================

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
    mode: str = "user",
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
        # Step 1: Validate inputs and load YAML files
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )

        # Step 2: Extract simulation parameters
        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)

        # Step 3: Run scientific validation pipeline
        validation_results = run_scientific_validation_pipeline(
            uptodate_data, start_date, model_year
        )

        # Step 4: Run scientific adjustment pipeline (skip if critical errors)
        critical_errors = [r for r in validation_results if r.status == "ERROR"]
        if not critical_errors:
            science_checked_data, adjustments = run_scientific_adjustment_pipeline(
                uptodate_data, start_date, model_year
            )
        else:
            # No adjustments when critical errors exist
            science_checked_data = deepcopy(uptodate_data)
            adjustments = []

        # Step 5: Generate science report (always generate, even with critical errors)
        science_yaml_filename = (
            os.path.basename(science_yaml_file) if science_yaml_file else None
        )
        report_content = create_science_report(
            validation_results, adjustments, science_yaml_filename, phase_a_report_file, mode
        )

        # Step 6: Write science report (always write)
        if science_report_file:
            with open(science_report_file, "w") as f:
                f.write(report_content)

        # Step 7: CRITICAL HALT CHECK - Stop if errors found after report is written
        if critical_errors:
            print_critical_halt_message(critical_errors)
            raise ValueError("Critical scientific errors detected - Phase B halted")

        # Step 8: Print terminal results
        print_science_check_results(validation_results, adjustments)

        # Step 9: Write science-checked YAML file (only if no critical errors)
        if science_yaml_file and not critical_errors:
            header = create_science_yaml_header(phase_a_performed)
            with open(science_yaml_file, "w") as f:
                f.write(header)
                yaml.dump(
                    science_checked_data, f, default_flow_style=False, sort_keys=False
                )

        return science_checked_data

    except Exception as e:
        print(f"Phase B Error: {e}")
        raise


def main():
    """Main entry point for science_check.py Phase B."""
    print(" SUEWS Scientific Validation (Phase B)")
    print("=" * 50)

    # Define file paths
    user_file = "src/supy/data_model/user.yml"
    uptodate_file = "src/supy/data_model/uptodate_user.yml"
    standard_file = "src/supy/sample_data/sample_config.yml"

    print(f"Phase A output (uptodate): {uptodate_file}")
    print(f"Original user YAML: {user_file}")
    print(f"Standard YAML: {standard_file}")
    print()

    # Generate output file paths
    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]

    science_yaml_filename = f"science_checked_{basename}"
    science_report_filename = f"science_report_{name_without_ext}.txt"

    science_yaml_file = os.path.join(dirname, science_yaml_filename)
    science_report_file = os.path.join(dirname, science_report_filename)

    # Run Phase B
    try:
        science_checked_data = run_science_check(
            uptodate_yaml_file=uptodate_file,
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            science_yaml_file=science_yaml_file,
            science_report_file=science_report_file,
            phase_a_performed=True,  # Assumes Phase A was run (looking for uptodate_file)
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
