from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ...core.yaml_helpers import get_value_safe
from collections.abc import Mapping
import calendar
from typing import Dict, List, Optional, Union, Any, Tuple


# Constants 
SFR_FRACTION_TOL = 1e-4

def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """
    Check for missing or empty parameters in a surface configuration dictionary.

    This function recursively traverses the given surface properties dictionary and identifies
    any parameters (with a "value" key) that are missing (None) or empty (""). It skips any
    key named "sfr". The function returns a list of parameter paths (dot-separated) that are
    missing or empty.

    Parameters
    ----------
    surface_props : dict
        The dictionary containing surface properties to be checked.
    surface_type : str
        The type of surface being checked (not used in the function logic, but may be used for context).

    Returns
    -------
    List[str]
        A list of dot-separated parameter paths that are missing or empty in the surface configuration.
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



@RulesRegistry.add_rule("land_cover")
def validate_land_cover_consistency(context) -> List[ValidationResult]:
    """
    Validate land cover fractions and associated parameters for each site.

    This function checks the consistency of land cover surface fractions (`sfr`) and the presence of required parameters
    for each active surface type in the provided YAML configuration. It ensures that the sum of all surface fractions
    equals 1.0 within a specified tolerance, and that all necessary parameters are set for surfaces with nonzero fractions.
    Special handling is included for biogenic CO2 parameters depending on the model's emissions method.
    Warnings are issued for parameters defined under surfaces with zero fraction.

    Parameters
    ----------
    context : object
        An object containing the parsed YAML data as `context.yaml_data`.

    Returns
    -------
    List[ValidationResult]
        A list of `ValidationResult` objects indicating errors, warnings, or pass status for each site and surface.

    Notes
    -----
    - If the sum of surface fractions deviates from 1.0 beyond the allowed tolerance, an error is reported.
    - For surfaces with `sfr > 0`, all required parameters must be present; missing parameters trigger errors.
    - Biogenic CO2 parameters are only required if the emissions method enables CO2 for the relevant surfaces.
    - Parameters under surfaces with `sfr == 0` are not validated, but a warning is issued if such parameters exist.
    """
    yaml_data = context.yaml_data

    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")
        site_gridid = site.get("gridiv")

        if not land_cover:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="LAND_COVER",
                    parameter="land_cover",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Missing land_cover block",
                    suggested_value="Add land_cover configuration with surface fractions",
                )
            )
            continue

        # Calculate sum of all surface fractions
        sfr_sum = 0.0
        surface_types = []

        for surface_type, surface_props in land_cover.items():
            if isinstance(surface_props, Mapping):
                sfr_value = surface_props.get("sfr", {}).get("value")
                if sfr_value is not None:
                    sfr_sum += sfr_value
                    surface_types.append((surface_type, sfr_value))

        if abs(sfr_sum - 1.0) > SFR_FRACTION_TOL:
            if sfr_sum == 0.0:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter="land_cover.surface_fractions",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"All surface fractions are zero or missing",
                        suggested_value="Set surface fractions (paved.sfr, bldgs.sfr, evetr.sfr, dectr.sfr, grass.sfr, bsoil.sfr, water.sfr) that sum to 1.0",
                    )
                )
            else:
                surface_list = ", ".join([
                    f"{surf}={val:.4f}" for surf, val in surface_types
                ])
                # Identify the surface with the largest fraction (same as auto-correction logic)
                surface_dict = dict(surface_types)
                max_surface = (
                    max(surface_dict.keys(), key=lambda k: surface_dict[k])
                    if surface_dict
                    else "surface"
                )
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"Surface fractions sum to {sfr_sum:.4f}, should equal 1.0 (auto-correction range: 1.0 ± {SFR_FRACTION_TOL:.1e}, current: {surface_list}. Validator will auto‑correct small deviations in this range.)",
                        suggested_value=f"Adjust the max surface {max_surface}.sfr or other surface fractions so they sum to exactly 1.0",
                    )
                )

        # Determine if biogenic CO2 parameters should be required
        physics = yaml_data.get("model", {}).get("physics", {})
        emissionsmethod = get_value_safe(physics, "emissions")
        biogenic_params = {
            "alpha_bioco2",
            "alpha_enh_bioco2",
            "beta_bioco2",
            "beta_enh_bioco2",
            "min_res_bioco2",
            "theta_bioco2",
            "resp_a",
            "resp_b",
        }
        biogenic_surfaces = {"dectr", "evetr", "grass"}

        for surface_type, sfr_value in surface_types:
            if sfr_value > 0:
                surface_props = land_cover[surface_type]
                missing_params = _check_surface_parameters(surface_props, surface_type)

                # If emissionsmethod disables CO2, skip biogenic params for relevant surfaces
                if (
                    emissionsmethod is not None
                    and emissionsmethod in [0, 1, 2, 3, 4]
                    and surface_type in biogenic_surfaces
                ):
                    missing_params = [
                        p
                        for p in missing_params
                        if p.split(".")[-1] not in biogenic_params
                    ]

                for param_name in missing_params:
                    readable_message = (
                        f"Surface '{surface_type}' is active (sfr > 0) but parameter '{param_name}' "
                        f"is missing or null. Active surfaces require all their parameters to be "
                        f"properly configured for accurate simulation results."
                    )

                    actionable_suggestion = (
                        f"Set parameter '{param_name}' to an appropriate non-null value. "
                        f"Refer to SUEWS documentation for typical values for '{surface_type}' surfaces."
                    )

                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="LAND_COVER",
                            parameter=f"{surface_type}.{param_name}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message=readable_message,
                            suggested_value=actionable_suggestion,
                        )
                    )

        zero_sfr_surfaces = [surf for surf, sfr in surface_types if sfr == 0]
        if zero_sfr_surfaces:
            for surf_type in zero_sfr_surfaces:
                param_list = []
                surf_props = (
                    site.get("properties", {}).get("land_cover", {}).get(surf_type, {})
                )

                def collect_param_names(d: dict, prefix: str = ""):
                    for k, v in d.items():
                        if k == "sfr":
                            continue
                        current_path = f"{prefix}.{k}" if prefix else k
                        if isinstance(v, Mapping):
                            if "value" in v:
                                param_list.append(current_path)
                            else:
                                collect_param_names(v, current_path)

                collect_param_names(surf_props)

                if param_list:
                    message = f"Parameters under sites.properties.land_cover.{surf_type} are not checked because '{surf_type}' surface fraction is 0."
                    param_names = ", ".join(param_list)
                    suggested_fix = f"Either set {surf_type} surface fraction > 0 to activate validation, or remove unused parameters: {param_names}"

                    results.append(
                        ValidationResult(
                            status="WARNING",
                            category="LAND_COVER",
                            parameter=f"land_cover.{surf_type}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message=message,
                            suggested_value=suggested_fix,
                        )
                    )

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


@RulesRegistry.add_rule("geographic")
def validate_geographic_parameters(context) -> List[ValidationResult]:
    """
    Validate geographic coordinates and related location parameters for each site.

    This function checks the presence, type, and valid range of latitude and longitude
    for each site in the provided context. It also verifies the presence of timezone
    and daylight saving parameters, issuing warnings if they are missing (as they can
    be calculated automatically). Validation results are returned for each issue found,
    and a PASS result is added if no errors are detected.

    Parameters
    ----------
    context : object
        An object containing the YAML data to be validated. Must have a `yaml_data`
        attribute with a "sites" key, where each site is a dictionary of properties.

    Returns
    -------
    List[ValidationResult]
        A list of ValidationResult objects describing errors, warnings, or a pass
        status for the geographic parameters of each site.

    Notes
    -----
    - Latitude must be a numeric value between -90 and 90.
    - Longitude must be a numeric value between -180 and 180.
    - Timezone and daylight saving parameters are optional; warnings are issued if missing.
    """
    yaml_data = context.yaml_data
    
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        site_gridid = site.get("gridiv")

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
    Validates irrigation Day-Of-Year (DOY) parameters with consideration for leap years, hemisphere, and tropical regions.

    Parameters
    ----------
    ie_start : float or None
        Irrigation start day of year.
    ie_end : float or None
        Irrigation end day of year.
    lat : float
        Site latitude (used for hemisphere and tropical detection).
    model_year : int
        Simulation year (used for leap year detection).
    site_name : str
        Site identifier for error messages.

    Returns
    -------
    List[ValidationResult]
        List of ValidationResult objects (errors, warnings, or empty).

    Notes
    -----
    - Checks that DOY values are within valid range (1-365 or 1-366 for leap years).
    - Ensures both parameters are set together or both disabled.
    - Checks warm season appropriateness based on hemisphere and latitude:
        * Tropical regions (|lat| < 23.5): No restrictions, irrigation allowed year-round.
        * Northern Hemisphere (lat >= 23.5): Warm season is May-September (DOY 121-273).
        * Southern Hemisphere (lat <= -23.5): Warm season is November-March (DOY 305-90).
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
        Determine if a given Day-Of-Year (DOY) falls within the warm season for the specified hemisphere.

        Parameters
        ----------
        doy : float
            Day of year to check.
        latitude : float
            Site latitude (positive for Northern Hemisphere, negative for Southern Hemisphere).

        Returns
        -------
        bool
            True if DOY is within the warm season for the given hemisphere, False otherwise.

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



@RulesRegistry.add_rule("irrigation")
def validate_irrigation_parameters(context) -> List[ValidationResult]:
    """
    Validate irrigation DOY parameters for all sites.

    Extracts irrigation parameters from each site configuration and validates
    them using context-aware checks (leap year, hemisphere).

    Parameters
    ----------
    context : object
        An object containing the YAML data (`yaml_data`) and simulation year (`model_year`).

    Returns
    -------
    List[ValidationResult]
        List of ValidationResult objects for all sites.
    """
    yaml_data = context.yaml_data
    model_year = context.model_year

    results = []
    sites = yaml_data.get("sites", [])

    # Handle both list and dict formats for sites
    if isinstance(sites, Mapping):
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

@RulesRegistry.add_rule("veg_albedo")
def check_missing_vegetation_albedo(context) -> List[ValidationResult]:
    """
    Report when vegetated surfaces have null alb_id.

    This is informational only: SUEWSConfig will auto-calculate alb_id from
    LAI state before the model run.

    Trees use a direct LAI-albedo relationship (higher LAI -> higher albedo).
    Grass uses a reversed relationship (higher LAI -> lower albedo).

    Returns
    -------
    List[ValidationResult]
        Informational results for each vegetated surface with null alb_id.
    """
    yaml_data = context.yaml_data

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
        site_gridid = site.get("gridiv")

        for surf_key, surf_label in surface_labels.items():
            surf_props = land_cover.get(surf_key, {})
            surf_state = initial_states.get(surf_key, {})
            if not surf_props or not surf_state:
                continue

            # Check if surface has non-zero fraction
            sfr_entry = surf_props.get("sfr", {})
            sfr_val = (
                sfr_entry.get("value") if isinstance(sfr_entry, Mapping) else sfr_entry
            )
            if not sfr_val or sfr_val <= 0:
                continue

            # Check if alb_id is null
            alb_entry = surf_state.get("alb_id", {})
            alb_val = (
                alb_entry.get("value") if isinstance(alb_entry, Mapping) else alb_entry
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
