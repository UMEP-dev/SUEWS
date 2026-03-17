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


@RulesRegistry.add_phase_b("rcmethod")
def validate_model_option_rcmethod(yaml_data: dict) -> List[ValidationResult]:
    """Validate RoofOuterCapFrac and WallOuterCapFrac if rcmethod == 1.
    For rcmethod == 2, validate required roof/wall external parameters are not null.
    If provided, emit a warning with their values for user review.
    """
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    rcmethod_value = get_value_safe(physics, "rcmethod")

    if rcmethod_value == 1:
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            site_gridid = get_value_safe(site, "gridiv")

            # RoofOuterCapFrac
            roof_frac_entry = building_archetype.get("RoofOuterCapFrac", {})
            roof_frac = roof_frac_entry.get("value") if isinstance(roof_frac_entry, dict) else roof_frac_entry
            if roof_frac is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.RoofOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="RoofOuterCapFrac must be explicitly provided when rcmethod == 1.",
                        suggested_value="Set RoofOuterCapFrac to a value between 0 and 1 (exclusive)."
                    )
                )
            elif not (0 < roof_frac < 1):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.RoofOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"RoofOuterCapFrac value {roof_frac} is out of valid range (0, 1) when rcmethod == 1.",
                        suggested_value="Set RoofOuterCapFrac to a value strictly between 0 and 1."
                    )
                )

            # WallOuterCapFrac
            wall_frac_entry = building_archetype.get("WallOuterCapFrac", {})
            wall_frac = wall_frac_entry.get("value") if isinstance(wall_frac_entry, dict) else wall_frac_entry
            if wall_frac is None:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.WallOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="WallOuterCapFrac must be explicitly provided when rcmethod == 1.",
                        suggested_value="Set WallOuterCapFrac to a value between 0 and 1 (exclusive)."
                    )
                )
            elif not (0 < wall_frac < 1):
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.WallOuterCapFrac",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"WallOuterCapFrac value {wall_frac} is out of valid range (0, 1) when rcmethod == 1.",
                        suggested_value="Set WallOuterCapFrac to a value strictly between 0 and 1."
                    )
                )

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
        sites = yaml_data.get("sites", [])
        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            building_archetype = props.get("building_archetype", {})
            site_gridid = get_value_safe(site, "gridiv")

            # Collect provided wall params
            provided_wall = []
            for param in required_wall_params:
                entry = building_archetype.get(param, {})
                value = entry.get("value") if isinstance(entry, dict) else entry
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
                    provided_wall.append(f"{param}={value}")

            # Collect provided roof params
            provided_roof = []
            for param in required_roof_params:
                entry = building_archetype.get(param, {})
                value = entry.get("value") if isinstance(entry, dict) else entry
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
                    provided_roof.append(f"{param}={value}")

            # Emit warning if any required params are provided
            if provided_wall:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.wall_external_parameters",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"The following wall material parameters will be used for parameterisation: {', '.join(provided_wall)}. Please check that these values are valid for your building material.",
                        suggested_value="Review wall material properties for accuracy."
                    )
                )
            if provided_roof:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="MODEL_OPTIONS",
                        parameter="building_archetype.roof_external_parameters",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"The following roof material parameters will be used for parameterisation: {', '.join(provided_roof)}. Please check that these values are valid for your building material.",
                        suggested_value="Review roof material properties for accuracy."
                    )
                )

    return results


@RulesRegistry.add_phase_b("land_cover")
def validate_land_cover_consistency(yaml_data: dict) -> List[ValidationResult]:
    """Validate land cover fractions and parameters."""
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
            if isinstance(surface_props, dict):
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
                        if isinstance(v, dict):
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
