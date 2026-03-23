from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ...core.yaml_helpers import get_value_safe
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
    Validates that HeatingSetpointTemperature and CoolingSetpointTemperature are set
    in building_archetype when setpointmethod == 0 or 1.
    For setpointmethod == 2, validates that all entries in HeatingSetpointTemperatureProfile
    and CoolingSetpointTemperatureProfile are set (not null), and that heating values < 30.0,
    cooling values > 15.0.
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
                        suggested_value="Set HeatingSetpointTemperatureProfile to a mapping with working_day and holiday keys, each mapping hour to temperature.",
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
                                message=f"HeatingSetpointTemperatureProfile.{daytype} must be a mapping of hour to value.",
                                suggested_value="Set each daytype to a mapping of hour (as string) to temperature value.",
                            )
                        )
                    else:
                        for hour_str, temp_val in profile.items():
                            if temp_val is None:
                                heating_missing_entries.append(f"{daytype}.{hour_str}")
                            else:
                                try:
                                    if float(temp_val) >= 30.0:
                                        heating_out_of_range.append(f"{daytype}.{hour_str}={temp_val}")
                                except Exception:
                                    heating_out_of_range.append(f"{daytype}.{hour_str}={temp_val}")

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
                        suggested_value="Set CoolingSetpointTemperatureProfile to a mapping with working_day and holiday keys, each mapping hour to temperature.",
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
                                message=f"CoolingSetpointTemperatureProfile.{daytype} must be a mapping of hour to value.",
                                suggested_value="Set each daytype to a mapping of hour (as string) to temperature value.",
                            )
                        )
                    else:
                        for hour_str, temp_val in profile.items():
                            if temp_val is None:
                                cooling_missing_entries.append(f"{daytype}.{hour_str}")
                            else:
                                try:
                                    if float(temp_val) <= 15.0:
                                        cooling_out_of_range.append(f"{daytype}.{hour_str}={temp_val}")
                                except Exception:
                                    cooling_out_of_range.append(f"{daytype}.{hour_str}={temp_val}")

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
