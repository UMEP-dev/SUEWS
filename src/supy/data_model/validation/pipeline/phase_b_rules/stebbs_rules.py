from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from collections.abc import Mapping
from ...core.yaml_helpers import get_value_safe
from typing import Dict, List, Optional, Union, Any, Tuple

def check_archetype_radiation_properties(archetype_data, facet):
    """
    Validate that the sum of reflectivity, absorbtivity, and transmissivity for a given facet equals 1.

    This function checks the radiation properties (reflectivity, absorbtivity, and transmissivity) of a specified facet
    in the provided archetype data. If any property is missing, validation is skipped. If properties are provided as
    dictionaries (e.g., with a "value" key), their values are extracted. The function returns a ValidationResult indicating
    whether the sum of the three properties equals 1.

    Parameters
    ----------
    archetype_data : dict
        Dictionary containing the radiation properties for different facets.
    facet : str
        The facet name (e.g., "Wall", "Roof") whose radiation properties are to be validated.

    Returns
    -------
    ValidationResult or None
        Returns a ValidationResult with status "PASS" if the sum is 1, "ERROR" otherwise.
        Returns None if any property is missing.
    """
    archetype_facet_reflectivity = archetype_data.get(f"{facet}Reflectivity")
    archetype_facet_absorbtivity = archetype_data.get(f"{facet}Absorbtivity")
    archetype_facet_transmissivity = archetype_data.get(f"{facet}Transmissivity")

    # Skip validation if any radiation property is missing
    if any(v is None for v in (archetype_facet_reflectivity, archetype_facet_absorbtivity, archetype_facet_transmissivity)):
        return None

    # Extract values from RefValue dicts if needed
    if isinstance(archetype_facet_reflectivity, Mapping):
        archetype_facet_reflectivity = archetype_facet_reflectivity.get("value", archetype_facet_reflectivity)
    if isinstance(archetype_facet_absorbtivity, Mapping):
        archetype_facet_absorbtivity = archetype_facet_absorbtivity.get("value", archetype_facet_absorbtivity)
    if isinstance(archetype_facet_transmissivity, Mapping):
        archetype_facet_transmissivity = archetype_facet_transmissivity.get("value", archetype_facet_transmissivity)

    result = ValidationResult(
        status="PASS",
        category="Archetype",
        parameter=f"{facet}Reflectivity, {facet}Absorbtivity, {facet}Transmissivity",
    )

    radiation_properties_total = archetype_facet_reflectivity + archetype_facet_absorbtivity + archetype_facet_transmissivity

    if radiation_properties_total != 1.0:
        result.status="ERROR"
        result.message = f"Facet reflectivity, absorbtivity and transmissivity must sum to 1. Current total = {radiation_properties_total}"

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
    - The rule only applies if the "stebbsmethod" in the physics model is set to 1.
    - For each site, the function checks the "Wall" and "Roof" facets of the building archetype properties.
    - If a validation result is found, it is annotated with the site's index and grid ID before being added to the results list.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})
            archetype_props = site_props.get("building_archetype", {})

            for facet in ["Wall", "Roof"]:
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
    then all entries in the MetabolismProfile must also be zero. If any nonzero values are found
    in the MetabolismProfile when occupants are zero, an error is reported for each problematic entry.

    Parameters
    ----------
    context : object
        The validation context containing the parsed YAML data under `context.yaml_data`.

    Returns
    -------
    list of ValidationResult
        A list of validation results indicating errors where the MetabolismProfile contains
        nonzero entries while the number of occupants is zero. Each result includes details
        about the site, parameter, and suggested correction.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            occupants_entry = building_archetype.get("Occupants", {})
            occupants = occupants_entry.get("value") if isinstance(occupants_entry, Mapping) else occupants_entry
            metabolism_profile = building_archetype.get("MetabolismProfile", {})
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
                            parameter="building_archetype.MetabolismProfile",
                            site_index=site_idx,
                            site_gridid=site.get("gridiv"),
                            message=(
                                f"Occupants is 0.0 but MetabolismProfile has nonzero entries: {', '.join(problematic_entries)} (should all be 0)."
                            ),
                            suggested_value="Set all MetabolismProfile entries to 0 if Occupants is 0.0",
                        )
                    )
    return results


@RulesRegistry.add_rule("daylight_control")
def check_daylight_control(context):
    """
    Validate the 'DaylightControl' flag and related lighting parameters for each site when STEBBS is active.

    - Checks that the 'DaylightControl' flag under each site's 'stebbs' properties is set to 0 or 1
      (accepting both integer and float representations), but only if the 'stebbsmethod' in the model
      physics is set to 1.
    - If 'DaylightControl' is 1, 'LightingIlluminanceThreshold' must be provided by the user in the YAML.
    - For both 0 and 1, 'LightingPowerDensity' must be provided.

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
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            site_gridid = site.get("gridiv")

            daylight_control = stebbs.get("DaylightControl", {})
            dc_val = daylight_control.get("value") if isinstance(daylight_control, Mapping) else daylight_control

            # Validate DaylightControl value
            if dc_val not in (0, 1, 0.0, 1.0, None):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="stebbs.DaylightControl",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"DaylightControl flag must be 0 (off) or 1 (on), got '{dc_val}'.",
                        suggested_value="Set DaylightControl to 0 or 1",
                    )
                )

            # Check LightingIlluminanceThreshold if DaylightControl is 1
            if dc_val in (1, 1.0):
                lit = stebbs.get("LightingIlluminanceThreshold", None)
                lit_val = lit.get("value") if isinstance(lit, Mapping) else lit
                if lit_val is None:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="stebbs.LightingIlluminanceThreshold",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message="LightingIlluminanceThreshold must be provided when DaylightControl is 1.",
                            suggested_value="Provide a value for LightingIlluminanceThreshold in stebbs.",
                        )
                    )

            # Check LightingPowerDensity for both 0 and 1
            lpd = stebbs.get("LightingPowerDensity", None)
            lpd_val = lpd.get("value") if isinstance(lpd, Mapping) else lpd
            if lpd_val is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="stebbs.LightingPowerDensity",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="LightingPowerDensity must be provided when STEBBS is active.",
                        suggested_value="Provide a value for LightingPowerDensity in stebbs.",
                    )
                )
    return results


@RulesRegistry.add_rule("stebbs_props")
def check_stebbs_properties(context):
    """
    Validate the 'HotWaterFlowProfile' values in the STEBBS properties for each site.

    Checks that, if the 'stebbsmethod' is set to 1 in the model physics configuration,
    the 'HotWaterFlowProfile' for both 'working_day' and 'holiday' day types contains
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
        'HotWaterFlowProfile' values for each site.
    """

    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs = props.get("stebbs", {})
            site_gridid = site.get("gridiv")
            hwfp_entry = stebbs.get("HotWaterFlowProfile", {})
            for daytype in ("working_day", "holiday"):
                day_profile = hwfp_entry.get(daytype, {})
                if isinstance(day_profile, Mapping):
                    for hour_str, v in day_profile.items():
                        if v not in (0, 1, 0.0, 1.0):
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"stebbs.HotWaterFlowProfile.{daytype}.{hour_str}",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    message=(
                                        f"HotWaterFlowProfile for '{daytype}' hour '{hour_str}' must be 0 or 1, got '{v}'."
                                    ),
                                    suggested_value="Set HotWaterFlowProfile to 0 or 1"
                                )
                            )

    return results
