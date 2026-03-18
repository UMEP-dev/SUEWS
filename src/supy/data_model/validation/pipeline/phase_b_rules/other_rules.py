from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ...core.yaml_helpers import get_value_safe
from collections.abc import Mapping

@RulesRegistry.add_phase_b("land_cover")
def validate_land_cover_consistency(context) -> List[ValidationResult]:
    """Validate land cover fractions and parameters."""
    yaml_data = context.yaml_data

    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")
        site_gridid = get_site_gridid(site)

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
        emissionsmethod = get_value_safe(physics, "emissionsmethod")
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


@RulesRegistry.add_phase_b("geographic")
def validate_geographic_parameters(context) -> List[ValidationResult]:
    """Validate geographic coordinates and location parameters."""
    yaml_data = context.yaml_data
    
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


@RulesRegistry.add_phase_b("irrigation")
def validate_irrigation_parameters(context) -> List[ValidationResult]:
    """
    Validate irrigation DOY parameters for all sites.

    Extracts irrigation parameters from each site configuration and validates
    them using context-aware checks (leap year, hemisphere).

    Args:
        yaml_data: Complete YAML configuration
        model_year: Simulation year for leap year detection

    Returns:
        List of ValidationResult objects for all sites
    """
    yaml_data = context.yaml_data
    model_year = context.model_year

    results = []
    sites = yaml_data.get("sites", [])

    # Handle both list and dict formats for sites
    if isinstance(sites, Mapping):
        # Dict format: {site_name: {lat: ..., ...}, ...}
        sites_list = [(site_name, site_data) for site_name, site_data in sites.items()]
    elif isinstance(sites, Mapping):
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

@RulesRegistry.add_phase_b("veg_albedo")
def check_missing_vegetation_albedo() -> List[ValidationResult]:
    """Report when vegetated surfaces have null alb_id.

    This is informational: SUEWSConfig will auto-calculate alb_id from
    LAI state before the model run. Trees use a direct LAI-albedo
    relationship (higher LAI -> higher albedo); grass uses a reversed
    relationship (higher LAI -> lower albedo).
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
        site_gridid = get_site_gridid(site)

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
