from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from collections.abc import Mapping
from ...core.yaml_helpers import (
    get_ref_or_bare,
    get_value_safe,
)
from ...rules.hard_invariants import check_stebbs_internal_shading
from ...rules.typed_view import TypedConfigView
from typing import List
import math

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
    # Canonical archetype names; ``normalise_yaml_renames`` rewrites
    # the legacy ``{facet}_reflectivity`` spellings before the rules run.
    keys = tuple(
        f"{prop}_{facet}_external"
        for prop in ("reflectivity", "absorptivity", "transmissivity")
    )
    values = [get_ref_or_bare(archetype_data, key) for key in keys]

    # Skip validation if any radiation property is missing or null
    if any(v is None for v in values):
        return None

    parameter = ", ".join(keys)
    if not all(
        isinstance(v, (int, float)) and not isinstance(v, bool) for v in values
    ):
        return ValidationResult(
            status="ERROR",
            category="Archetype",
            parameter=parameter,
            message=(
                f"Facet {facet} reflectivity, absorptivity and transmissivity must "
                f"be numbers. Current values = {values}"
            ),
        )

    result = ValidationResult(
        status="PASS",
        category="Archetype",
        parameter=parameter,
    )

    radiation_properties_total = sum(values)

    if not math.isclose(radiation_properties_total, 1.0, rel_tol=0.0, abs_tol=1e-6):
        result.status = "ERROR"
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
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    if view.physics.stebbs_method == 1:
        for site in view.sites:
            archetype_props = site.building_archetype
            for facet in ("wall", "roof"):
                result = check_archetype_radiation_properties(
                    archetype_props, facet=facet
                )
                if result is not None:
                    result.site_index = site.index
                    result.site_gridid = site.gridid
                    results.append(result)

    return results


@RulesRegistry.add_rule("occupants_metabolism")
def check_occupants_metabolism(context):
    """Check for inconsistency between the number of occupants and metabolism profile."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    if view.physics.stebbs_method == 1:
        for site in view.sites:
            building_archetype = site.building_archetype
            occupants = site.exact_scalar(building_archetype, "occupants")
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
                            site_index=site.index,
                            site_gridid=site.gridid,
                            message=(
                                f"occupants is 0.0 but profile_metabolism has nonzero entries: {', '.join(problematic_entries)} (should all be 0)."
                            ),
                            suggested_value="Set all profile_metabolism entries to 0 if occupants is 0.0",
                        )
                    )
    return results


@RulesRegistry.add_rule("daylight_control")
def check_daylight_control(context):
    """Validate the 'daylight_control' flag and related lighting parameters for each site when STEBBS is active."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    if view.physics.stebbs_method == 1:
        for site in view.sites:
            stebbs = site.stebbs
            site_gridid = site.gridid

            # get_value_safe accepts the legacy `control_daylight` spelling too.
            dc_val = site.scalar(stebbs, "daylight_control")

            if dc_val not in (0, 1, 0.0, 1.0, None):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="stebbs.daylight_control",
                        site_index=site.index,
                        site_gridid=site_gridid,
                        message=f"daylight_control flag must be 0 (off) or 1 (on), got '{dc_val}'.",
                        suggested_value="Set daylight_control to 0 or 1",
                    )
                )

            if dc_val in (1, 1.0):
                lit_val = site.scalar(stebbs, "threshold_lighting_illuminance")
                if lit_val is None:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="stebbs.threshold_lighting_illuminance",
                            site_index=site.index,
                            site_gridid=site_gridid,
                            message="threshold_lighting_illuminance must be provided when daylight_control is 1.",
                            suggested_value="Provide a value for threshold_lighting_illuminance in stebbs.",
                        )
                    )

    return results


@RulesRegistry.add_rule("internal_shading")
def check_internal_shading(context):
    """Validate internal window-shading controls when STEBBS is active."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    if view.physics.stebbs_method != 1:
        return results

    for site in view.sites:
        stebbs = site.stebbs
        shading_mode = site.scalar(stebbs, "internal_shading")
        values = {
            field_name: site.scalar(stebbs, field_name)
            for field_name in (
                "reduction_factor_shading",
                "temperature_threshold_shading",
                "radiation_threshold_shading",
            )
        }
        violations = check_stebbs_internal_shading(
            shading_mode,
            values,
            require_explicit_presence=False,
            site_index=site.index,
            site_gridid=site.gridid,
        )
        results.extend(ValidationResult.from_violation(v) for v in violations)

    return results


@RulesRegistry.add_rule("stebbs_props")
def check_stebbs_properties(context):
    """Validate the 'profile_flow_hot_water' values in STEBBS properties when STEBBS is active."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    if view.physics.stebbs_method == 1:
        for site in view.sites:
            stebbs = site.stebbs
            site_gridid = site.gridid
            hwfp_entry = site.scalar(stebbs, "profile_flow_hot_water", {})
            if not isinstance(hwfp_entry, Mapping):
                continue
            for daytype in ("working_day", "holiday"):
                day_profile = hwfp_entry.get(daytype, {})
                if isinstance(day_profile, Mapping):
                    for hour_str, v in day_profile.items():
                        if v not in (0, 1, 0.0, 1.0):
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"stebbs.profile_flow_hot_water.{daytype}.{hour_str}",
                                    site_index=site.index,
                                    site_gridid=site_gridid,
                                    message=(
                                        f"profile_flow_hot_water for '{daytype}' hour '{hour_str}' must be 0 or 1, got '{v}'."
                                    ),
                                    suggested_value="Set profile_flow_hot_water to 0 or 1"
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
    - For `setpointmethod` 0 or 1, checks that `setpoint_temperature_heating_air` and
      `setpoint_temperature_cooling_air` are set in each site's `building_archetype`.
    - For `setpointmethod` 2, checks that all entries in
      `profile_setpoint_temperature_heating_air` and `profile_setpoint_temperature_cooling_air`
      are present (not null), that heating values are less than 30.0, and cooling
      values are greater than 15.0 for all 144 ten-minute slices in both
      `working_day` and `holiday` profiles.
    - All checks are only performed if stebbsmethod == 1.
    """
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []

    setpointmethod = view.physics.stebbs_value("setpoint")
    stebbsmethod = view.physics.stebbs_method

    if stebbsmethod != 1:
        return results

    if setpointmethod == 0 or setpointmethod == 1:
        for site in view.sites:
            site_name = site.name
            building_archetype = site.building_archetype
            heating = site.scalar(building_archetype, "setpoint_temperature_heating_air")
            cooling = site.scalar(building_archetype, "setpoint_temperature_cooling_air")
            if heating is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="setpoint_temperature_heating_air",
                        site_gridid=site_name,
                        site_index=None,
                        message="setpoint_temperature_heating_air must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set setpoint_temperature_heating_air to a valid temperature value."
                    )
                )
            if cooling is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="setpoint_temperature_cooling_air",
                        site_gridid=site_name,
                        site_index=None,
                        message="setpoint_temperature_cooling_air must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set setpoint_temperature_cooling_air to a valid temperature value."
                    )
                )
    elif setpointmethod == 2:
        for site in view.sites:
            site_idx = site.index
            site_name = site.name
            building_archetype = site.building_archetype
            # rename-aware: resolves the legacy `profile_temperature_air_*_setpoint`
            # spellings in un-normalised (Phase-B-standalone/BC) input.
            heating_profile = site.scalar(
                building_archetype, "profile_setpoint_temperature_heating_air", {}
            )
            cooling_profile = site.scalar(
                building_archetype, "profile_setpoint_temperature_cooling_air", {}
            )

            # Check heating profile
            heating_missing_entries = []
            heating_out_of_range = []
            if not isinstance(heating_profile, Mapping):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="profile_setpoint_temperature_heating_air",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="profile_setpoint_temperature_heating_air must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set profile_setpoint_temperature_heating_air to a mapping with working_day and holiday keys, each mapping ten-minute slices to temperature.",
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
                                parameter=f"profile_setpoint_temperature_heating_air.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"profile_setpoint_temperature_heating_air.{daytype} must be a mapping of ten-minute slice to value.",
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
                                    parameter=f"profile_setpoint_temperature_heating_air.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_setpoint_temperature_heating_air.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minutes slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_setpoint_temperature_heating_air.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_setpoint_temperature_heating_air.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if heating_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_setpoint_temperature_heating_air",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_setpoint_temperature_heating_air has null entries at: {', '.join(heating_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in profile_setpoint_temperature_heating_air to valid temperature values.",
                        )
                    )
                if heating_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_setpoint_temperature_heating_air",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_setpoint_temperature_heating_air has values >= 30.0 at: {', '.join(heating_out_of_range)}. All heating setpoints must be less than 30.0.",
                            suggested_value="Set all entries in profile_setpoint_temperature_heating_air to values less than 30.0.",
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
                        parameter="profile_setpoint_temperature_cooling_air",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="profile_setpoint_temperature_cooling_air must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set profile_setpoint_temperature_cooling_air to a mapping with working_day and holiday keys, each mapping ten-minutes slice to temperature.",
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
                                parameter=f"profile_setpoint_temperature_cooling_air.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"profile_setpoint_temperature_cooling_air.{daytype} must be a mapping of ten-minute slice to value.",
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
                                    parameter=f"profile_setpoint_temperature_cooling_air.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_setpoint_temperature_cooling_air.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minute slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"profile_setpoint_temperature_cooling_air.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"profile_setpoint_temperature_cooling_air.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if cooling_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_setpoint_temperature_cooling_air",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_setpoint_temperature_cooling_air has null entries at: {', '.join(cooling_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in profile_setpoint_temperature_cooling_air to valid temperature values.",
                        )
                    )
                if cooling_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="profile_setpoint_temperature_cooling_air",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"profile_setpoint_temperature_cooling_air has values <= 15.0 at: {', '.join(cooling_out_of_range)}. All cooling setpoints must be greater than 15.0.",
                            suggested_value="Set all entries in profile_setpoint_temperature_cooling_air to values greater than 15.0.",
                        )
                    )
    return results
