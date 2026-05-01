from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from collections.abc import Mapping
from ...core.yaml_helpers import get_value_safe
from typing import Dict, List, Optional, Union, Any, Tuple

def check_archetype_radiation_properties(archetype_data, facet):
    """
    Validate that the sum of reflectivity, absorptivity, and transmissivity for a given facet equals 1.

    This function checks the radiation properties (reflectivity, absorptivity, and transmissivity) of a specified facet
    in the provided archetype data. If any property is missing, validation is skipped. If properties are provided as
    dictionaries (e.g., with a "value" key), their values are extracted. The function returns a ValidationResult indicating
    whether the sum of the three properties equals 1.

    Parameters
    ----------
    archetype_data : dict
        Dictionary containing the radiation properties for different facets.
    facet : str
        The facet name (e.g., "wall", "roof") whose radiation properties are to be validated.

    Returns
    -------
    ValidationResult or None
        Returns a ValidationResult with status "PASS" if the sum is 1, "ERROR" otherwise.
        Returns None if any property is missing.
    """
    archetype_facet_reflectivity = archetype_data.get(f"{facet}_reflectivity")
    archetype_facet_absorptivity = archetype_data.get(f"{facet}_absorptivity")
    archetype_facet_transmissivity = archetype_data.get(f"{facet}_transmissivity")

    # Skip validation if any radiation property is missing
    if any(v is None for v in (archetype_facet_reflectivity, archetype_facet_absorptivity, archetype_facet_transmissivity)):
        return None

    # Extract values from RefValue dicts if needed
    if isinstance(archetype_facet_reflectivity, Mapping):
        archetype_facet_reflectivity = archetype_facet_reflectivity.get("value", archetype_facet_reflectivity)
    if isinstance(archetype_facet_absorptivity, Mapping):
        archetype_facet_absorptivity = archetype_facet_absorptivity.get("value", archetype_facet_absorptivity)
    if isinstance(archetype_facet_transmissivity, Mapping):
        archetype_facet_transmissivity = archetype_facet_transmissivity.get("value", archetype_facet_transmissivity)

    result = ValidationResult(
        status="PASS",
        category="Archetype",
        parameter=f"{facet}_reflectivity, {facet}_absorptivity, {facet}_transmissivity",
    )

    radiation_properties_total = archetype_facet_reflectivity + archetype_facet_absorptivity + archetype_facet_transmissivity

    if radiation_properties_total != 1.0:
        result.status="ERROR"
        result.message = f"Facet reflectivity, absorptivity and transmissivity must sum to 1. Current total = {radiation_properties_total}"

    return result

@RulesRegistry.add_rule("archetype_properties")
def check_archetype_properties(context):
    """
    Checks the properties of building archetypes for each site when the Stebbs method is enabled.

    This rule is registered under the name "archetype_properties" and is intended to validate
    the presence and correctness of radiation-related properties for building archetypes
    (specifically for "Wall" and "Roof" facets) in the provided YAML data context.

    Parameters
    ----------
    context : object
        An object containing the YAML data to be validated. It is expected to have a `yaml_data`
        attribute, which is a dictionary representing the configuration.

    Returns
    -------
    list
        A list of result objects from `check_archetype_radiation_properties` for each site and facet
        that requires validation. Each result object may have additional attributes set, such as
        `site_index` and `site_gridid`.

    Notes
    -----
    - The rule only applies if the "stebbs_method" in the physics model is set to 1.
    - For each site, the function checks the "Wall" and "Roof" facets of the building archetype properties.
    - If a validation result is found, it is annotated with the site's index and grid ID before being added to the results list.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})
            archetype_props = site_props.get("building_archetype", {})

            for facet in ["wall", "roof"]:
                result = check_archetype_radiation_properties(archetype_props, facet=facet)
                if result is not None:
                    result.site_index = i
                    result.site_gridid = site.get("gridiv")
                    results.append(result)

    return results

@RulesRegistry.add_rule("occupants_metabolism")
def check_occupants_metabolism(context):
    """
    Check for inconsistency between the number of occupants and metabolism profile.

    This rule validates that if the number of occupants is set to 0.0 in the building archetype,
    then all entries in the profile_metabolism must also be zero. If any nonzero values are found
    in the profile_metabolism when occupants are zero, an error is reported for each problematic entry.

    Parameters
    ----------
    context : object
        The validation context containing the parsed YAML data under `context.yaml_data`.

    Returns
    -------
    list of ValidationResult
        A list of validation results indicating errors where the profile_metabolism contains
        nonzero entries while the number of occupants is zero. Each result includes details
        about the site, parameter, and suggested correction.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            occupants_entry = building_archetype.get("occupants", {})
            occupants = occupants_entry.get("value") if isinstance(occupants_entry, Mapping) else occupants_entry
            metabolism_profile = building_archetype.get("profile_metabolism", {})
            if occupants == 0.0 and isinstance(metabolism_profile, Mapping):
                problematic_entries = []
                for daytype in ("working_day", "holiday"):
                    profile = metabolism_profile.get(daytype, {})
                    if isinstance(profile, Mapping):
                        for hour_str, metab_val in profile.items():
                            if metab_val not in (0, 0.0, None):
                                problematic_entries.append(
                                    f"{daytype}.{hour_str}={metab_val}"
                                )
                if problematic_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="building_archetype.profile_metabolism",
                            site_index=site_idx,
                            site_gridid=site.get("gridiv"),
                            message=(
                                f"occupants is 0.0 but profile_metabolism has nonzero entries: {', '.join(problematic_entries)} (should all be 0)."
                            ),
                            suggested_value="Set all profile_metabolism entries to 0 if occupants is 0.0",
                        )
                    )
    return results


@RulesRegistry.add_rule("daylight_control")
def check_daylight_control(context):
    """
    Validate the 'daylight_control' flag and related lighting parameters for each site when STEBBS is active.

    - Checks that the 'daylight_control' flag under each site's 'stebbs' properties is set to 0 or 1
      (accepting both integer and float representations), but only if the 'stebbs_method' in the model
      physics is set to 1.
    - If 'daylight_control' is 1, 'lighting_illuminance_threshold' must be provided by the user in the YAML.

    Parameters
    ----------
    context : object
        An object containing the parsed YAML data as an attribute `yaml_data`.

    Returns
    -------
    results : list of ValidationResult
        A list of ValidationResult objects for each site with an invalid or missing value.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            site_gridid = site.get("gridiv")

            daylight_control = stebbs.get("daylight_control", {})
            dc_val = daylight_control.get("value") if isinstance(daylight_control, Mapping) else daylight_control

            # Validate daylight_control value
            if dc_val not in (0, 1, 0.0, 1.0, None):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="stebbs.daylight_control",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"daylight_control flag must be 0 (off) or 1 (on), got '{dc_val}'.",
                        suggested_value="Set daylight_control to 0 or 1",
                    )
                )

            # Check lighting_illuminance_threshold if daylight_control is 1
            if dc_val in (1, 1.0):
                lit = stebbs.get("lighting_illuminance_threshold", None)
                lit_val = lit.get("value") if isinstance(lit, Mapping) else lit
                if lit_val is None:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="stebbs.lighting_illuminance_threshold",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message="lighting_illuminance_threshold must be provided when daylight_control is 1.",
                            suggested_value="Provide a value for lighting_illuminance_threshold in stebbs.",
                        )
                    )

    return results


@RulesRegistry.add_rule("stebbs_props")
def check_stebbs_properties(context):
    """
    Validate the 'hot_water_flow_profile' values in the STEBBS properties for each site.

    Checks that, if the 'stebbs_method' is set to 1 in the model physics configuration,
    the 'hot_water_flow_profile' for both 'working_day' and 'holiday' day types contains
    only values of 0 or 1 for each hour. If any value is not 0 or 1, an error is added
    to the results.

    Parameters
    ----------
    context : object
        An object containing the parsed YAML data as an attribute `yaml_data`.

    Returns
    -------
    list of ValidationResult
        A list of ValidationResult objects describing any errors found in the
        'hot_water_flow_profile' values for each site.
    """

    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            site_gridid = site.get("gridiv")
            hwfp_entry = stebbs.get("hot_water_flow_profile", {})
            for daytype in ("working_day", "holiday"):
                day_profile = hwfp_entry.get(daytype, {})
                if isinstance(day_profile, Mapping):
                    for hour_str, v in day_profile.items():
                        if v not in (0, 1, 0.0, 1.0):
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"stebbs.hot_water_flow_profile.{daytype}.{hour_str}",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    message=(
                                        f"hot_water_flow_profile for '{daytype}' hour '{hour_str}' must be 0 or 1, got '{v}'."
                                    ),
                                    suggested_value="Set hot_water_flow_profile to 0 or 1"
                                )
                            )

    return results

@RulesRegistry.add_rule("setpoint")
def validate_model_option_setpoint(context) -> List[ValidationResult]:
    """
    Validate setpoint temperature configuration for buildings.

    Parameters
    ----------
    context : object
        Validation context containing the parsed YAML data.

    Returns
    -------
    List[ValidationResult]
        List of validation results for setpoint temperature configuration.

    Notes
    -----
    - For `setpointmethod` 0 or 1, checks that `temperature_air_heating_setpoint` and
      `temperature_air_cooling_setpoint` are set in each site's `building_archetype`.
    - For `setpointmethod` 2, checks that all entries in
      `profile_temperature_air_heating_setpoint` and `profile_temperature_air_cooling_setpoint`
      are present (not null), that heating values are less than 30.0, and cooling
      values are greater than 15.0 for all 144 ten-minute slices in both
      `working_day` and `holiday` profiles.
    - All checks are only performed if stebbsmethod == 1.
    """
    results = []
    yaml_data = context.yaml_data
    physics = yaml_data.get("model", {}).get("physics", {})

    setpointmethod = get_value_safe(physics, "setpoint")
    stebbsmethod = get_value_safe(physics, "stebbs")

    if stebbsmethod != 1:
        return results

    if setpointmethod == 0 or setpointmethod == 1:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            heating = get_value_safe(building_archetype, "temperature_air_heating_setpoint")
            cooling = get_value_safe(building_archetype, "temperature_air_cooling_setpoint")
            if heating is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="temperature_air_heating_setpoint",
                        site_gridid=site_name,
                        site_index=None,
                        message="temperature_air_heating_setpoint must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set temperature_air_heating_setpoint to a valid temperature value."
                    )
                )
            if cooling is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="temperature_air_cooling_setpoint",
                        site_gridid=site_name,
                        site_index=None,
                        message="temperature_air_cooling_setpoint must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set temperature_air_cooling_setpoint to a valid temperature value."
                    )
                )
    elif setpointmethod == 2:
        for site_idx, site in enumerate(yaml_data.get("sites", [])):
            site_name = site.get("name", "Unknown")
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            heating_profile = building_archetype.get("profile_temperature_air_heating_setpoint", {})
            cooling_profile = building_archetype.get("profile_temperature_air_cooling_setpoint", {})

            # Check heating profile
            heating_missing_entries = []
            heating_out_of_range = []
            if not isinstance(heating_profile, Mapping):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="profile_temperature_air_heating_setpoint",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="profile_temperature_air_heating_setpoint must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set profile_temperature_air_heating_setpoint to a mapping with working_day and holiday keys, each mapping ten-minute slices to temperature.",
                    )
                )
            else:
                for daytype in ("working_day", "holiday"):
                    profile = heating_profile.get(daytype, {})
                    if not isinstance(profile, Mapping):
                        results.append(
                            ValidationResult(
                                status="ERROR",
                                category="MODEL_OPTIONS",
                                parameter=f"profile_temperature_air_heating_setpoint.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"profile_temperature_air_heating_setpoint.{daytype} must be a mapping of ten-minute slice to value.",
                                suggested_value="Set each daytype to a mapping of ten-minute slice (as string) to temperature value.",
                            )
                        )
                    else:
                        # Enforce exactly 144 ten-minute slices per day-type (1-144)
                        expected_slices = {str(i) for i in range(1, 145)}
                        actual_slices = set(profile.keys())
                        missing_slices = expected_slices - actual_slices
                        extra_slices = actual_slices - expected_slices
                        for slice_str in expected_slices:
                            temp_val = profile.get(slice_str)
                            if temp_val is None:
                                heating_missing_entries.append(f"{daytype}.{slice_str}")
                            else:
                                try:
                                    if float(temp_val) >= 30.0:
                                        heating_out_of_range.append(f"{daytype}.{slice_str}={temp_val}")
                                except Exception:
                                    heating_out_of_range.append(f"{daytype}.{slice_str}={temp_val}")
                        if missing_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_temperature_air_heating_setpoint.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_temperature_air_heating_setpoint.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minutes slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_temperature_air_heating_setpoint.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_temperature_air_heating_setpoint.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if heating_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_temperature_air_heating_setpoint",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_temperature_air_heating_setpoint has null entries at: {', '.join(heating_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in profile_temperature_air_heating_setpoint to valid temperature values.",
                        )
                    )
                if heating_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_temperature_air_heating_setpoint",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_temperature_air_heating_setpoint has values >= 30.0 at: {', '.join(heating_out_of_range)}. All heating setpoints must be less than 30.0.",
                            suggested_value="Set all entries in profile_temperature_air_heating_setpoint to values less than 30.0.",
                        )
                    )

            # Check cooling profile
            cooling_missing_entries = []
            cooling_out_of_range = []
            if not isinstance(cooling_profile, Mapping):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="profile_temperature_air_cooling_setpoint",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="profile_temperature_air_cooling_setpoint must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set profile_temperature_air_cooling_setpoint to a mapping with working_day and holiday keys, each mapping ten-minutes slice to temperature.",
                    )
                )
            else:
                for daytype in ("working_day", "holiday"):
                    profile = cooling_profile.get(daytype, {})
                    if not isinstance(profile, Mapping):
                        results.append(
                            ValidationResult(
                                status="ERROR",
                                category="MODEL_OPTIONS",
                                parameter=f"profile_temperature_air_cooling_setpoint.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"profile_temperature_air_cooling_setpoint.{daytype} must be a mapping of ten-minute slice to value.",
                                suggested_value="Set each daytype to a mapping of ten-minute slice (as string) to temperature value.",
                            )
                        )
                    else:
                        # Enforce exactly 144 ten-minute slices per day-type (1-144)
                        expected_slices = {str(i) for i in range(1, 145)}
                        actual_slices = set(profile.keys())
                        missing_slices = expected_slices - actual_slices
                        extra_slices = actual_slices - expected_slices
                        for slice_str in expected_slices:
                            temp_val = profile.get(slice_str)
                            if temp_val is None:
                                cooling_missing_entries.append(f"{daytype}.{slice_str}")
                            else:
                                try:
                                    if float(temp_val) <= 15.0:
                                        cooling_out_of_range.append(f"{daytype}.{slice_str}={temp_val}")
                                except Exception:
                                    cooling_out_of_range.append(f"{daytype}.{slice_str}={temp_val}")
                        if missing_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_temperature_air_cooling_setpoint.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_temperature_air_cooling_setpoint.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minute slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_temperature_air_cooling_setpoint.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_temperature_air_cooling_setpoint.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if cooling_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_temperature_air_cooling_setpoint",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_temperature_air_cooling_setpoint has null entries at: {', '.join(cooling_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in profile_temperature_air_cooling_setpoint to valid temperature values.",
                        )
                    )
                if cooling_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_temperature_air_cooling_setpoint",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_temperature_air_cooling_setpoint has values <= 15.0 at: {', '.join(cooling_out_of_range)}. All cooling setpoints must be greater than 15.0.",
                            suggested_value="Set all entries in profile_temperature_air_cooling_setpoint to values greater than 15.0.",
                        )
                    )
    return results
