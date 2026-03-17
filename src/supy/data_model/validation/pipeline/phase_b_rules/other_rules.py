from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ...core.yaml_helpers import get_value_safe
@RulesRegistry.add_phase_b("physics_params")
def validate_physics_parameters(yaml_data: dict) -> List[ValidationResult]:
    """Validate required physics parameters."""
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
        "samealbedo_wall",
        "samealbedo_roof",
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


@RulesRegistry.add_phase_b("option_dependencies")
def validate_model_option_dependencies(yaml_data: dict) -> List[ValidationResult]:
    """Validate consistency between model physics options."""
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    rslmethod = get_value_safe(physics, "rslmethod")
    stabilitymethod = get_value_safe(physics, "stabilitymethod")
    storageheatmethod = get_value_safe(physics, "storageheatmethod")
    ohmincqf = get_value_safe(physics, "ohmincqf")

    # RSL method and stability method dependencies
    if rslmethod == 2 and stabilitymethod != 3:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="rslmethod-stabilitymethod",
                message="If rslmethod == 2, stabilitymethod must be 3",
                suggested_value="Set stabilitymethod to 3",
            )
        )

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

    # Storage heat method and OhmIncQf compatibility check
    # Only method 1 (OHM_WITHOUT_QF) has specific compatibility requirements
    if storageheatmethod == 1 and ohmincqf != 0:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message=f"StorageHeatMethod is set to {storageheatmethod} and OhmIncQf is set to {ohmincqf}. You should switch to OhmIncQf=0.",
                suggested_value="Set OhmIncQf to 0",
            )
        )
    else:
        results.append(
            ValidationResult(
                status="PASS",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message="StorageHeatMethod-OhmIncQf compatibility validated",
            )
        )

    # SMDMethod and soil_observation dependency
    smdmethod = get_value_safe(physics, "smdmethod")
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

@RulesRegistry.add_phase_b("samealbedo")
def validate_model_option_samealbedo(yaml_data: dict) -> List[ValidationResult]:
    """Validate consistency between model physics options, reporting site names."""
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    samealbedo_roof = get_value_safe(physics, "samealbedo_roof")
    samealbedo_wall = get_value_safe(physics, "samealbedo_wall")

    if samealbedo_wall == 0:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            vlay = site.get("properties", {}).get("vertical_layers", {})
            walls = vlay.get("walls", [])
            if isinstance(walls, dict):  # rare but possible
                walls = [walls]
            found_albedos = []
            for wall in walls:
                alb_val = get_value_safe(wall, "alb")
                if alb_val is not None:
                    found_albedos.append(alb_val)
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            wallrefl_val = get_value_safe(building_archetype, "WallReflectivity")
            msg = (
                f"samealbedo_wall == 0. No check of consistency between walls albedo (found values: {found_albedos}) and WallReflectivity (found value: {wallrefl_val})."
            )
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="MODEL_OPTIONS",
                    parameter="samealbedo_wall",
                    site_gridid=site_name,
                    site_index=None,
                    message=f"{msg}",
                    suggested_value=None,
                )
            )

    if samealbedo_roof == 0:
        for site in yaml_data.get("sites", []):
            site_name = site.get("name", "Unknown")
            vlay = site.get("properties", {}).get("vertical_layers", {})
            roofs = vlay.get("roofs", [])
            if isinstance(roofs, dict):  # rare but possible
                roofs = [roofs]
            found_albedos = []
            for roof in roofs:
                alb_val = get_value_safe(roof, "alb")
                if alb_val is not None:
                    found_albedos.append(alb_val)
            building_archetype = site.get("properties", {}).get("building_archetype", {})
            roofrefl_val = get_value_safe(building_archetype, "RoofReflectivity")
            msg = (
                f"samealbedo_roof == 0. No check of consistency between roofs albedo (found values: {found_albedos}) and RoofReflectivity (found value: {roofrefl_val})."
            )
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="MODEL_OPTIONS",
                    parameter="samealbedo_roof",
                    site_gridid=site_name,
                    site_index=None,
                    message=f"{msg}",
                    suggested_value=None,
                )
            )

    return results