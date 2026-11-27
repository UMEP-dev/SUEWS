from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ..document import is_ref_only, is_ref_wrapper, unwrap_entry
from ...core.yaml_helpers import get_value_safe
from ...rules.advisory import (
    check_irrigation_season_advisory,
    check_spartacus_lowest_layer_advisories,
)
from ...rules.hard_invariants import (
    SPARTACUS_METHODS,
    check_dls_doy_pair,
    check_irrigation_doy_pair,
    is_modelled_irrigation_active,
)
from ...rules.typed_view import (
    TypedConfigView,
    as_float as _as_float,
    is_absent_doy_mapping as _is_absent_doy_mapping,
)
from collections.abc import Mapping
import math
from typing import List, Optional, Union


# Constants
SFR_FRACTION_TOL = 1e-4


def _is_spartacus_enabled(yaml_data: Mapping) -> bool:
    return TypedConfigView.from_mapping(yaml_data).physics.is_spartacus_enabled


def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """Check for missing or empty parameters in a surface configuration mapping."""
    missing_params = []

    def _check_recursively(props: Mapping, path: str = ""):
        for key, value in props.items():
            if key == "sfr":
                continue

            current_path = f"{path}.{key}" if path else key

            if isinstance(value, Mapping):
                if is_ref_wrapper(value):
                    param_value = unwrap_entry(value)
                    if param_value in (None, "") or (
                        isinstance(param_value, (list, tuple))
                        and any(v in (None, "") for v in param_value)
                    ):
                        missing_params.append(current_path)
                else:
                    _check_recursively(value, current_path)
            elif value in (None, "") or (
                # ValidationContext freezes lists into tuples.
                isinstance(value, (list, tuple))
                and any(v in (None, "") for v in value)
            ):
                missing_params.append(current_path)

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
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []
    emissionsmethod = get_value_safe(view.physics.raw, "emissions")
    spartacus_enabled = view.physics.is_spartacus_enabled

    for site in view.sites:
        site_idx = site.index
        land_cover = site.land_cover
        site_gridid = site.gridid

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

        raw_surface_types = site.surface_fractions()
        surface_types = []
        has_invalid_sfr = False
        for surf_name, raw_sfr in raw_surface_types:
            if isinstance(raw_sfr, bool):
                num_sfr = None
            else:
                try:
                    num_sfr = float(raw_sfr)
                except (TypeError, ValueError, OverflowError):
                    num_sfr = None
            if num_sfr is None or not math.isfinite(num_sfr):
                has_invalid_sfr = True
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter=f"{surf_name}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=(
                            f"Surface fraction '{surf_name}.sfr' ({raw_sfr!r}) is not a finite number"
                        ),
                        suggested_value="Set surface fraction to a finite number between 0.0 and 1.0",
                    )
                )
            else:
                surface_types.append((surf_name, num_sfr))

        sfr_sum = sum(sfr_value for _, sfr_value in surface_types)

        if not has_invalid_sfr and abs(sfr_sum - 1.0) > SFR_FRACTION_TOL:
            if sfr_sum == 0.0:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter="land_cover.surface_fractions",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="All surface fractions are zero or missing",
                        suggested_value="Set surface fractions (paved.sfr, bldgs.sfr, evetr.sfr, dectr.sfr, grass.sfr, bsoil.sfr, water.sfr) that sum to 1.0",
                    )
                )
            else:
                surface_list = ", ".join(
                    f"{surf}={val:.4f}" for surf, val in surface_types
                )
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
                        message=f"Surface fractions sum to {sfr_sum:.4f}, should equal 1.0 (auto-correction range: 1.0 +/- {SFR_FRACTION_TOL:.1e}, current: {surface_list}. Validator will auto-correct small deviations in this range.)",
                        suggested_value=f"Adjust the max surface {max_surface}.sfr or other surface fractions so they sum to exactly 1.0",
                    )
                )

        if spartacus_enabled:
            results.extend(
                ValidationResult.from_advisory(f)
                for f in check_spartacus_lowest_layer_advisories(
                    site, sfr_tol=SFR_FRACTION_TOL
                )
            )

        biogenic_params = {
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
        }
        biogenic_surfaces = {"dectr", "evetr", "grass"}

        for surface_type, sfr_value in surface_types:
            if sfr_value > 0:
                surface_props = land_cover[surface_type]
                missing_params = _check_surface_parameters(surface_props, surface_type)

                if (
                    emissionsmethod is not None
                    and emissionsmethod in [0, 1, 2, 3, 4, 5, 6]
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
                surf_props = land_cover.get(surf_type, {})

                def collect_param_names(d: Mapping, prefix: str = ""):
                    for k, v in d.items():
                        if k == "sfr":
                            continue
                        current_path = f"{prefix}.{k}" if prefix else k
                        if isinstance(v, Mapping) and not is_ref_wrapper(v) and not is_ref_only(v):
                            collect_param_names(v, current_path)
                        else:
                            param_list.append(current_path)

                if isinstance(surf_props, Mapping):
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
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    # Prefer the run year resolved on context (which already falls back to the
    # forcing when start_time is unset); fall back to raw start_time for direct
    # unit callers that omit model_year on context.
    dls_year = getattr(context, "model_year", None)
    if dls_year is None and view.start_time is not None:
        try:
            dls_year = int(str(view.start_time)[:4])
        except (TypeError, ValueError):
            dls_year = None

    for site in view.sites:
        site_idx = site.index
        props = site.properties
        site_gridid = site.gridid

        lat = site.scalar(props, "lat")

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

        lng = site.scalar(props, "lng")

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

        timezone = site.scalar(props, "timezone")

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

        anthro_emissions = site.anthropogenic_emissions
        if anthro_emissions:
            startdls = site.scalar(anthro_emissions, "startdls")
            enddls = site.scalar(anthro_emissions, "enddls")
            if _is_absent_doy_mapping(startdls):
                startdls = None
            if _is_absent_doy_mapping(enddls):
                enddls = None

            dls_violations, half_configured, _, _ = check_dls_doy_pair(
                startdls,
                enddls,
                model_year=dls_year,
                site_index=site_idx,
                site_gridid=site_gridid,
            )
            results.extend(ValidationResult.from_violation(v) for v in dls_violations)

            if half_configured:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="GEOGRAPHY",
                        parameter="anthropogenic_emissions.startdls,enddls",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=(
                            "Only one daylight saving parameter (startdls or enddls) "
                            "is set - provide both integer days of year (1-366), or "
                            "set both to null / omit both to disable daylight saving"
                        ),
                        suggested_value=(
                            "Set both startdls and enddls (or let --science-fixes apply "
                            "fill them from your coordinates), or disable daylight saving "
                            "by setting both to null / omitting both"
                        ),
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
    ie_start: Optional[Union[int, float]],
    ie_end: Optional[Union[int, float]],
    lat: float,
    model_year: int | None,
    site_name: str,
) -> List[ValidationResult]:
    """Validate irrigation DOY parameters using the shared kernel and seasonal advisories."""
    norm_start = None if _is_absent_doy_mapping(ie_start) else ie_start
    norm_end = None if _is_absent_doy_mapping(ie_end) else ie_end

    violations, ie_start_int, ie_end_int = check_irrigation_doy_pair(
        norm_start,
        norm_end,
        model_year=model_year,
        site_name=site_name,
    )
    if violations:
        return [ValidationResult.from_violation(v) for v in violations]

    if ie_start_int is None or ie_end_int is None:
        return []

    findings = check_irrigation_season_advisory(
        ie_start_int, ie_end_int, lat, site_name, model_year=model_year
    )
    return [ValidationResult.from_advisory(f) for f in findings]


@RulesRegistry.add_rule("irrigation")
def validate_irrigation_parameters(context) -> List[ValidationResult]:
    """Validate irrigation DOY parameters for sites with active modelled irrigation."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    model_year = context.model_year
    physics_raw = view.physics.raw
    if "water_use" in physics_raw or "waterusemethod" in physics_raw:
        water_use = view.physics.read("water_use")
    else:
        water_use = view.physics.read("water_use", 0)

    results = []
    for site in view.sites:
        if not is_modelled_irrigation_active(water_use, site.irrigation):
            continue
        site_name = site.name if site.raw.get("name") is not None or isinstance(
            context.yaml_data.get("sites"), Mapping
        ) else f"site_{site.index}"
        lat = site.scalar(site.properties, "lat", 0.0)
        ie_start = site.scalar(site.irrigation, "ie_start")
        ie_end = site.scalar(site.irrigation, "ie_end")
        results.extend(
            validate_irrigation_doy(ie_start, ie_end, lat, model_year, site_name)
        )

    return results


@RulesRegistry.add_rule("veg_albedo")
def check_missing_vegetation_albedo(context) -> List[ValidationResult]:
    """Report when vegetated surfaces have null alb_id."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    surface_labels = {
        "evetr": "evergreen trees",
        "dectr": "deciduous trees",
        "grass": "grass",
    }

    for site in view.sites:
        land_cover = site.land_cover
        initial_states = site.initial_states
        site_gridid = site.gridid

        for surf_key, surf_label in surface_labels.items():
            surf_props = land_cover.get(surf_key, {})
            surf_state = initial_states.get(surf_key, {})
            if not isinstance(surf_props, Mapping) or not surf_props:
                continue
            if not isinstance(surf_state, Mapping) or not surf_state:
                continue

            sfr_val = site.exact_scalar(surf_props, "sfr")
            if not sfr_val or sfr_val <= 0:
                continue

            alb_val = site.exact_scalar(surf_state, "alb_id")
            if alb_val is not None:
                continue

            results.append(
                ValidationResult(
                    status="INFO",
                    category="SEASONAL",
                    parameter=f"{surf_key}.alb_id",
                    site_index=site.index,
                    site_gridid=site_gridid,
                    message=(
                        f"alb_id is null for {surf_label} (sfr={sfr_val}). "
                        "It will be auto-calculated from LAI state during "
                        "SUEWSConfig construction"
                    ),
                )
            )

    return results
