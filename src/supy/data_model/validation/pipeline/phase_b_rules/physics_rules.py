from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ...core.yaml_helpers import get_value_safe
from ...core.yaml_helpers import unwrap_nested_value as _unwrap_nested_value

from collections.abc import Mapping
from typing import Dict, List, Optional, Union, Any, Tuple


@RulesRegistry.add_rule("physics_params")
def validate_physics_parameters(context) -> List[ValidationResult]:
    """Validate required physics parameters."""
    yaml_data = context.yaml_data

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
        "gsmodel",
        "snowuse",
        "stebbsmethod",
        "rcmethod",
        "same_albedo_wall",
        "same_albedo_roof",
        "same_emissivity_wall",
        "same_emissivity_roof",
        "setpointmethod",
    ]

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
                    message=f"Physics parameter '{param}' is required but missing or null. This parameter controls critical model behaviour and must be specified for the simulation to run properly.",
                    suggested_value=f"Set '{param}' to an appropriate value. Consult the SUEWS documentation for parameter descriptions and typical values: https://docs.suews.io/latest/",
                )
            )

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
                    message=f"Physics parameter '{param}' has null value. This parameter controls critical model behaviour and must be set for proper simulation.",
                    suggested_value=f"Set '{param}' to an appropriate non-null value. Check documentation for parameter details: https://docs.suews.io/en/latest",
                )
            )

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



def validate_rslmethod_dependency(rslmethod, stabilitymethod):
    if rslmethod == 2 and stabilitymethod != 3:
        return ValidationResult(
            status="ERROR",
            category="MODEL_OPTIONS",
            parameter="rslmethod-stabilitymethod",
            message="If rslmethod == 2, stabilitymethod must be 3",
            suggested_value="Set stabilitymethod to 3",
        )

    elif stabilitymethod == 1 and rslmethod is None:
        return ValidationResult(
            status="ERROR",
            category="MODEL_OPTIONS",
            parameter="stabilitymethod-rslmethod",
            message="If stabilitymethod == 1, rslmethod parameter is required for atmospheric stability calculations",
            suggested_value="Set rslmethod to appropriate value",
        )

    return ValidationResult(
        status="PASS",
        category="MODEL_OPTIONS",
        parameter="rslmethod-stabilitymethod",
        message="rslmethod-stabilitymethod constraints satisfied",
    )


def validate_storageheatmethod_dependency(storageheatmethod, ohmincqf):
    if storageheatmethod == 1 and ohmincqf != 0:
        return ValidationResult(
            status="ERROR",
            category="MODEL_OPTIONS",
            parameter="storageheatmethod-ohmincqf",
            message=f"StorageHeatMethod is set to {storageheatmethod} and OhmIncQf is set to {ohmincqf}. You should switch to OhmIncQf=0.",
            suggested_value="Set OhmIncQf to 0",
        )
    return ValidationResult(
        status="PASS",
        category="MODEL_OPTIONS",
        parameter="storageheatmethod-ohmincqf",
        message="StorageHeatMethod-OhmIncQf compatibility validated",
    )


def validate_smdmethod_dependency(smdmethod, yaml_data):
    results = []

    # Only method 1 (OHM_WITHOUT_QF) has specific compatibility requirements
    if smdmethod:  # Truthy check: skips None and 0 (modelled), validates 1+ (observed)
        sites = yaml_data.get("sites", [])
        sites_missing_soil_obs = []
        for site in sites:
            site_name = site.get("name", "Unknown")
            properties = site.get("properties", {})
            soil_obs = properties.get("soil_observation")
            if soil_obs is None:
                sites_missing_soil_obs.append(site_name)

        if sites_missing_soil_obs:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="MODEL_OPTIONS",
                    parameter="smdmethod-soil_observation",
                    message=(
                        f"SMDMethod is set to {smdmethod} (observed soil moisture), "
                        f"but site(s) {sites_missing_soil_obs} are missing the required "
                        "'soil_observation' configuration block."
                    ),
                    suggested_value=(
                        "Add 'soil_observation' block to site properties with: "
                        "depth, smcap, soil_not_rocks, and bulk_density"
                    ),
                )
            )
        else:
            results.append(
                ValidationResult(
                    status="PASS",
                    category="MODEL_OPTIONS",
                    parameter="smdmethod-soil_observation",
                    message="SMDMethod-soil_observation configuration validated",
                )
            )
    # When SMDMethod=0 (modelled), no validation needed - skip adding PASS result
    # to reduce noise in validation output.
    return results



@RulesRegistry.add_rule("option_dependencies")
def validate_model_option_dependencies(context) -> List[ValidationResult]:
    """Validate consistency between model physics options."""
    yaml_data = context.yaml_data
    
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    rslmethod = get_value_safe(physics, "rslmethod")
    stabilitymethod = get_value_safe(physics, "stabilitymethod")
    storageheatmethod = get_value_safe(physics, "storageheatmethod")
    ohmincqf = get_value_safe(physics, "ohmincqf")
    smdmethod = get_value_safe(physics, "smdmethod")

    # RSL method and stability method dependencies
    result = validate_rslmethod_dependency(rslmethod=rslmethod, stabilitymethod=stabilitymethod)
    if result is not None: results.append(result)

    # Storage heat method and OhmIncQf compatibility check
    result = validate_storageheatmethod_dependency(storageheatmethod=storageheatmethod, ohmincqf=ohmincqf)
    if result is not None: results.append(result)

    # SMDMethod and soil_observation dependency
    results.extend(validate_smdmethod_dependency(smdmethod=smdmethod, yaml_data=yaml_data))

    return results



def check_rcmethod2_facet(required_params, building_archetype, site_idx, site_gridid, facet):
    # Collect provided wall params
    provided_facet = []
    results = []
    for param in required_params:
        entry = building_archetype.get(param, {})
        value = entry.get("value") if isinstance(entry, Mapping) else entry
        if value in (None, ""):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="MODEL_OPTIONS",
                    parameter=f"building_archetype.{param}",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=f"{param} must be provided and non-null when rcmethod == 2.",
                    suggested_value=f"Set {param} to a valid numeric value."
                )
            )
        else:
            provided_facet.append(f"{param}={value}")
    if provided_facet:
        results.append(
            ValidationResult(
                status="WARNING",
                category="MODEL_OPTIONS",
                parameter=f"building_archetype.{facet.lower()}_external_parameters",
                site_index=site_idx,
                site_gridid=site_gridid,
                message=f"The following {facet.lower()} material parameters will be used for parameterisation: {', '.join(provided_facet)}. Please check that these values are valid for your building material.",
                suggested_value=f"Review {facet.lower()} material properties for accuracy."
            )
        )

    return results


def check_outercapfrac_facet(building_archetype, facet, site_idx, site_gridid):
    facet_frac_entry = building_archetype.get(f"{facet}OuterCapFrac", {})
    facet_frac = facet_frac_entry.get("value") if isinstance(facet_frac_entry, Mapping) else facet_frac_entry
    
    result = ValidationResult(
        status="ERROR",
        category="MODEL_OPTIONS",
        parameter=f"building_archetype.{facet}OuterCapFrac",
        site_index=site_idx,
        site_gridid=site_gridid,
    )
    
    if facet_frac is None:
        result.message=f"{facet}OuterCapFrac must be explicitly provided when rcmethod == 1."
        result.suggested_value=f"Set {facet}OuterCapFrac to a value between 0 and 1 (exclusive)."
        return result
    elif not (0 < facet_frac < 1):
        result.message=f"{facet}OuterCapFrac value {facet_frac} is out of valid range (0, 1) when rcmethod == 1."
        result.suggested_value=f"Set {facet}OuterCapFrac to a value strictly between 0 and 1."
        return result

@RulesRegistry.add_rule("rcmethod")
def validate_model_option_rcmethod(context) -> List[ValidationResult]:
    """Validate RoofOuterCapFrac and WallOuterCapFrac if rcmethod == 1.
    For rcmethod == 2, validate required roof/wall external parameters are not null.
    If provided, emit a warning with their values for user review.
    """
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    rcmethod_value = get_value_safe(physics, "rcmethod")

    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        building_archetype = props.get("building_archetype", {})
        site_gridid = get_value_safe(site, "gridiv")

        facets = ["Roof", "Wall"]

        if rcmethod_value == 1:
            for facet in facets:
                result = check_outercapfrac_facet(building_archetype=building_archetype, facet=facet, site_idx=site_idx, site_gridid=site_gridid)
                if result is not None:
                    results.append(result)

        elif rcmethod_value == 2:
            required_wall_params = [
                "WallextThickness",
                "WallextEffectiveConductivity",
                "WallextDensity",
                "WallextCp",
            ]
            required_roof_params = [
                "RoofextThickness",
                "RoofextEffectiveConductivity",
                "RoofextDensity",
                "RoofextCp",
            ]
            # Collect provided wall params
            for facet, facet_params in zip(
                facets,
                [required_roof_params, required_wall_params]
            ):
                results_walls = check_rcmethod2_facet(
                    required_params=facet_params,
                    building_archetype=building_archetype,
                    site_idx=site_idx,
                    site_gridid=site_gridid,
                    facet=facet
                )
                results.extend(results_walls)

    return results

def validate_model_option_same_albedo_facet(site_data, facet):
    site_name = site_data.get("name", "Unknown")
    vlay = site_data.get("properties", {}).get("vertical_layers", {})
    facet_layers = vlay.get(f"{facet.lower()}s", [])
    if isinstance(facet_layers, Mapping):  # rare but possible
        facet_layers = [facet_layers]
    found_albedos = []
    for layer in facet_layers:
        alb_val = get_value_safe(layer, "alb")
        if alb_val is not None:
            found_albedos.append(alb_val)
    building_archetype = site_data.get("properties", {}).get("building_archetype", {})
    facetrefl_val = get_value_safe(building_archetype, f"{facet}Reflectivity")
    msg = (
        f"same_albedo_{facet.lower()} == 0. No check of consistency between {facet.lower()}s albedo (found values: {found_albedos}) and {facet}Reflectivity (found value: {facetrefl_val})."
    )
    return ValidationResult(
        status="WARNING",
        category="MODEL_OPTIONS",
        parameter=f"same_albedo_{facet.lower()}",
        site_gridid=site_name,
        site_index=None,
        message=f"{msg}",
        suggested_value=None,
    )

@RulesRegistry.add_rule("same_albedo")
def validate_model_option_same_albedo(context) -> List[ValidationResult]:
    """Validate consistency between model physics options, reporting site names."""
    yaml_data = context.yaml_data
    
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    same_albedo_roof = get_value_safe(physics, "same_albedo_roof")
    same_albedo_wall = get_value_safe(physics, "same_albedo_wall")

    if same_albedo_wall == 0:
        for site in yaml_data.get("sites", []):
            results.append(
                validate_model_option_same_albedo_facet(site, "Wall")
            )
    if same_albedo_roof == 0:
        for site in yaml_data.get("sites", []):
            results.append(
                validate_model_option_same_albedo_facet(site, "Roof")
            )

    return results


@RulesRegistry.add_rule("same_emissivity")
def validate_model_option_same_emissivity(context) -> List[ValidationResult]:
    """
    Validates the consistency of model physics options related to wall and roof emissivities.
    """
    results = []
    yaml_data = context.yaml_data
    physics = yaml_data.get("model", {}).get("physics", {})

    same_emissivity_roof = get_value_safe(physics, "same_emissivity_roof")
    same_emissivity_wall = get_value_safe(physics, "same_emissivity_wall")

    if same_emissivity_wall == 0:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            vlay = site.get("properties", {}).get("vertical_layers", {})
            walls = vlay.get("walls", [])
            if isinstance(walls, dict):  # rare but possible
                walls = [walls]
            found_emissivities = []
            for wall in walls:
                emis_val = get_value_safe(wall, "emis")
                if emis_val is not None:
                    found_emissivities.append(emis_val)
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            wallemis_val = get_value_safe(building_archetype, "WallExternalEmissivity")
            msg = (
                f"same_emissivity_wall == 0. No check of consistency between walls emissivity (found values: {found_emissivities}) and WallExternalEmissivity (found value: {wallemis_val})."
            )
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="MODEL_OPTIONS",
                    parameter="same_emissivity_wall",
                    site_gridid=site_name,
                    site_index=None,
                    message=msg,
                    suggested_value=None,
                )
            )

    if same_emissivity_roof == 0:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            vlay = site.get("properties", {}).get("vertical_layers", {})
            roofs = vlay.get("roofs", [])
            if isinstance(roofs, dict):  # rare but possible
                roofs = [roofs]
            found_emissivities = []
            for roof in roofs:
                emis_val = get_value_safe(roof, "emis")
                if emis_val is not None:
                    found_emissivities.append(emis_val)
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            roofemis_val = get_value_safe(building_archetype, "RoofExternalEmissivity")
            msg = (
                f"same_emissivity_roof == 0. No check of consistency between roofs emissivity (found values: {found_emissivities}) and RoofExternalEmissivity (found value: {roofemis_val})."
            )
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="MODEL_OPTIONS",
                    parameter="same_emissivity_roof",
                    site_gridid=site_name,
                    site_index=None,
                    message=msg,
                    suggested_value=None,
                )
            )

    return results

@RulesRegistry.add_rule("setpointmethod")
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
    - For `setpointmethod` 0 or 1, checks that `HeatingSetpointTemperature` and
      `CoolingSetpointTemperature` are set in each site's `building_archetype`.
    - For `setpointmethod` 2, checks that all entries in
      `HeatingSetpointTemperatureProfile` and `CoolingSetpointTemperatureProfile`
      are present (not null), that heating values are less than 30.0, and cooling
      values are greater than 15.0 for all 144 ten-minute slices in both
      `working_day` and `holiday` profiles.
    """
    results = []
    yaml_data = context.yaml_data
    physics = yaml_data.get("model", {}).get("physics", {})

    setpointmethod = get_value_safe(physics, "setpointmethod")

    if setpointmethod == 0 or setpointmethod == 1:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            heating = get_value_safe(building_archetype, "HeatingSetpointTemperature")
            cooling = get_value_safe(building_archetype, "CoolingSetpointTemperature")
            if heating is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="HeatingSetpointTemperature",
                        site_gridid=site_name,
                        site_index=None,
                        message="HeatingSetpointTemperature must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set HeatingSetpointTemperature to a valid temperature value."
                    )
                )
            if cooling is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="CoolingSetpointTemperature",
                        site_gridid=site_name,
                        site_index=None,
                        message="CoolingSetpointTemperature must be set when setpointmethod == 0 or 1.",
                        suggested_value="Set CoolingSetpointTemperature to a valid temperature value."
                    )
                )
    elif setpointmethod == 2:
        for site_idx, site in enumerate(yaml_data.get("sites", [])):
            site_name = site.get("name", "Unknown")
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            heating_profile = building_archetype.get("HeatingSetpointTemperatureProfile", {})
            cooling_profile = building_archetype.get("CoolingSetpointTemperatureProfile", {})

            # Check heating profile
            heating_missing_entries = []
            heating_out_of_range = []
            if not isinstance(heating_profile, Mapping):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="HeatingSetpointTemperatureProfile",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="HeatingSetpointTemperatureProfile must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set HeatingSetpointTemperatureProfile to a mapping with working_day and holiday keys, each mapping ten-minute slices to temperature.",
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
                                parameter=f"HeatingSetpointTemperatureProfile.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"HeatingSetpointTemperatureProfile.{daytype} must be a mapping of ten-minute slice to value.",
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
                                    parameter=f"HeatingSetpointTemperatureProfile.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"HeatingSetpointTemperatureProfile.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minutes slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"HeatingSetpointTemperatureProfile.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"HeatingSetpointTemperatureProfile.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if heating_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="HeatingSetpointTemperatureProfile",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"HeatingSetpointTemperatureProfile has null entries at: {', '.join(heating_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in HeatingSetpointTemperatureProfile to valid temperature values.",
                        )
                    )
                if heating_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="HeatingSetpointTemperatureProfile",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"HeatingSetpointTemperatureProfile has values >= 30.0 at: {', '.join(heating_out_of_range)}. All heating setpoints must be less than 30.0.",
                            suggested_value="Set all entries in HeatingSetpointTemperatureProfile to values less than 30.0.",
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
                        parameter="CoolingSetpointTemperatureProfile",
                        site_gridid=site_name,
                        site_index=site_idx,
                        message="CoolingSetpointTemperatureProfile must be a mapping with daytype keys when setpointmethod == 2.",
                        suggested_value="Set CoolingSetpointTemperatureProfile to a mapping with working_day and holiday keys, each mapping ten-minutes slice to temperature.",
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
                                parameter=f"CoolingSetpointTemperatureProfile.{daytype}",
                                site_gridid=site_name,
                                site_index=site_idx,
                                message=f"CoolingSetpointTemperatureProfile.{daytype} must be a mapping of ten-minute slice to value.",
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
                                    parameter=f"CoolingSetpointTemperatureProfile.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"CoolingSetpointTemperatureProfile.{daytype} is missing {len(missing_slices)} entries: {', '.join(sorted(missing_slices))}. Must have all 144 entries.",
                                    suggested_value="Define all 144 ten-minute slice entries in the profile.",
                                )
                            )
                        if extra_slices:
                            results.append(
                                ValidationResult(
                                    status="ERROR",
                                    category="MODEL_OPTIONS",
                                    parameter=f"CoolingSetpointTemperatureProfile.{daytype}",
                                    site_gridid=site_name,
                                    site_index=site_idx,
                                    message=f"CoolingSetpointTemperatureProfile.{daytype} has {len(extra_slices)} unexpected entries: {', '.join(sorted(extra_slices))}. Only entries 1-144 are valid.",
                                    suggested_value="Remove any keys not in the range 1-144.",
                                )
                            )

                if cooling_missing_entries:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="CoolingSetpointTemperatureProfile",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"CoolingSetpointTemperatureProfile has null entries at: {', '.join(cooling_missing_entries)}. All entries must be set when setpointmethod == 2.",
                            suggested_value="Set all entries in CoolingSetpointTemperatureProfile to valid temperature values.",
                        )
                    )
                if cooling_out_of_range:
                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="MODEL_OPTIONS",
                            parameter="CoolingSetpointTemperatureProfile",
                            site_gridid=site_name,
                            site_index=site_idx,
                            message=f"CoolingSetpointTemperatureProfile has values <= 15.0 at: {', '.join(cooling_out_of_range)}. All cooling setpoints must be greater than 15.0.",
                            suggested_value="Set all entries in CoolingSetpointTemperatureProfile to values greater than 15.0.",
                        )
                    )
    return results


@RulesRegistry.add_rule("forcing_height")
def validate_forcing_height_vs_buildings(context) -> List[ValidationResult]:
    """
    Validate that the forcing height (z) is at least twice the mean and maximum building heights.

    Parameters
    ----------
    context : object
        Validation context containing the parsed YAML data.

    Returns
    -------
    List[ValidationResult]
        List of validation results indicating errors or warnings if the forcing height is too low.

    Notes
    -----
    - Raises an ERROR if `z` < 2 * mean building height (`land_cover.bldgs.bldgh`).
    - Raises a WARNING if `z` < 2 * maximum configured building height, where the maximum is the largest of:
        - `land_cover.bldgs.bldgh`
        - `building_archetype.stebbs_Height` (if `stebbsmethod == 1`)
        - Last non-zero entry in `vertical_layers.height` (SPARTACUS top height, if enabled)
    - This check ensures the forcing height is appropriate relative to the urban morphology.
    """
    def _as_float(x: Any) -> Optional[float]:
        if x is None:
            return None
        try:
            return float(x)
        except (TypeError, ValueError):
            return None

    def _last_nonzero_from_height(height_arr: Any) -> Optional[float]:
        """Given unwrapped height array/list, return last non-zero float, else None."""
        if not isinstance(height_arr, (list, tuple)) or not height_arr:
            return None
        last_nz = None
        for h in height_arr:
            hf = _as_float(_unwrap_nested_value(h))
            if hf is not None and hf != 0.0:
                last_nz = hf
        return last_nz

    yaml_data = context.yaml_data
    results: List[ValidationResult] = []

    physics = yaml_data.get("model", {}).get("physics", {})

    stebbsmethod_val = _unwrap_nested_value(physics.get("stebbsmethod"))
    try:
        stebbsmethod_val = int(stebbsmethod_val)
    except (TypeError, ValueError):
        stebbsmethod_val = None

    netradiationmethod_val = _unwrap_nested_value(physics.get("netradiationmethod"))
    try:
        netradiationmethod_val = int(netradiationmethod_val)
    except (TypeError, ValueError):
        netradiationmethod_val = None

    for site_idx, site in enumerate(yaml_data.get("sites", [])):
        props = site.get("properties", {})
        site_gridid = get_value_safe(site, "gridiv")
        site_name = site.get("name", "Unknown")

        # forcing height
        z = _as_float(_unwrap_nested_value(props.get("z")))
        if z is None:
            continue

        # mean building height (SUEWS land cover buildings)
        land_cover_raw = _unwrap_nested_value(props.get("land_cover") or {})
        bldgs = _unwrap_nested_value(land_cover_raw.get("bldgs", {})) if isinstance(land_cover_raw, Mapping) else None
        bldgh = _as_float(_unwrap_nested_value(bldgs.get("bldgh"))) if isinstance(bldgs, Mapping) else None

        # optional STEBBS archetype height (only when STEBBS is enabled)
        stebbs_height = None
        if stebbsmethod_val == 1:
            archetype = _unwrap_nested_value(props.get("building_archetype"))
            if isinstance(archetype, Mapping):
                stebbs_height = _as_float(_unwrap_nested_value(archetype.get("stebbs_Height")))

        # SPARTACUS heights (only if SPARTACUS is enabled via netradiationmethod)
        spartacus_top = None

        if netradiationmethod_val in (1001, 1002, 1003):
            vertical_layers = _unwrap_nested_value(props.get("vertical_layers"))
            height_arr = None
            if isinstance(vertical_layers, Mapping):
                height_arr = _unwrap_nested_value(vertical_layers.get("height"))
                spartacus_top = _last_nonzero_from_height(height_arr)

        # --- ERROR: z smaller than 2x mean building height ---
        if bldgh is None:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="FORCING",
                    parameter="properties.land_cover.bldgs.bldgh",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': cannot validate forcing height z={z} against "
                        "2*mean building height because land_cover.bldgs.bldgh is missing."
                    ),
                    suggested_value="Provide land_cover.bldgs.bldgh to enable this check.",
                )
            )
        else:
            threshold_mean = 2.0 * bldgh
            if z < threshold_mean:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="FORCING",
                        parameter="properties.z",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=(
                            f"Site '{site_name}': forcing height z={z} is smaller than "
                            f"2*mean building height (2*bldgh={threshold_mean}; bldgh={bldgh}). "
                        ),
                        suggested_value=f"Increase z to >= {threshold_mean} (or reduce bldgh).",
                    )
                )

        # --- WARNING: z smaller than 2x max configured building height ---
        candidates = [h for h in [bldgh, stebbs_height, spartacus_top] if h is not None]
        if candidates:
            h_max = max(candidates)
            threshold_max = 2.0 * h_max
            if z < threshold_max:
                parts = []
                if bldgh is not None:
                    parts.append(f"bldgh={bldgh}")
                if stebbs_height is not None:
                    parts.append(f"stebbs_Height={stebbs_height}")
                if spartacus_top is not None:
                    parts.append(f"SPARTACUS top layer height={spartacus_top}")

                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="FORCING",
                        parameter="properties.z",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=(
                            f"Site '{site_name}': forcing height z={z} m is smaller than "
                            f"2*max building height (2*max building height={threshold_max}; max building height={h_max} "
                            f"from {', '.join(parts)})."
                        ),
                        suggested_value=f"Consider increasing z to >= {threshold_max} if appropriate for your case.",
                    )
                )

    return results
