from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from ..document import read_scalar, unwrap_entry
from ...core.yaml_helpers import get_value_safe
from ...required_fields import BUILDING_PRESENT_SFR_TOL
from ...rules.advisory import check_same_surface_unchecked_advisory
from ...rules.hard_invariants import (
    SPARTACUS_METHODS,
    check_spartacus_storage_heat,
)
from ...rules.typed_view import (
    TypedConfigView,
    as_float as _as_float,
    method_code as _method_code,
)

from collections.abc import Mapping
from typing import List, Optional


def _are_buildings_present(
    yaml_data: Mapping, sfr_fraction_tol: float = BUILDING_PRESENT_SFR_TOL
) -> bool:
    """Return True if any site has ``land_cover.bldgs.sfr > sfr_fraction_tol``."""
    return TypedConfigView.from_mapping(yaml_data).any_buildings_present(
        tol=sfr_fraction_tol
    )


def validate_spartacus_dependencies(
    bldgs_present: bool, netradiationmethod, storageheatmethod
) -> list[ValidationResult]:
    """Validate the storage heat method chosen alongside SPARTACUS (gh#1834).

    Delegates the decision to
    ``validation.rules.hard_invariants.check_spartacus_storage_heat`` so
    ``SUEWSConfig`` and Phase B share one authoritative rule.
    """
    net_code = _method_code(netradiationmethod, "net_radiation")
    storage_code = _method_code(storageheatmethod, "storage_heat")

    if net_code not in SPARTACUS_METHODS or storage_code is None:
        return []

    violations = check_spartacus_storage_heat(
        net_code,
        storage_code,
        bldgs_present=bldgs_present,
        check_stebbs_for_method_7=False,
    )
    if violations:
        return [ValidationResult.from_violation(v) for v in violations]

    return [
        ValidationResult(
            status="PASS",
            category="MODEL_OPTIONS",
            parameter="netradiationmethod-storageheatmethod-bldgs",
            message="SPARTACUS storage heat method compatibility validated",
        )
    ]


@RulesRegistry.add_rule("physics_params")
def validate_physics_parameters(context) -> List[ValidationResult]:
    """
    Validate the presence and values of required physics parameters in the model configuration.

    This function checks the `model.physics` section of the provided YAML data for the presence
    and non-null values of all required physics parameters. It returns a list of `ValidationResult`
    objects indicating whether each required parameter is present and properly set, or if any are
    missing or empty.

    Parameters
    ----------
    context : ValidationContext
        The validation context containing the parsed YAML configuration data and any additional context needed for validation.

    Returns
    -------
    List[ValidationResult]
        A list of `ValidationResult` objects, each representing the outcome of the validation for
        a specific parameter or the overall section. The status can be "PASS", "ERROR", or "WARNING"
        depending on the validation result.

    Notes
    -----
    - If the `model.physics` section is missing or empty, a warning is returned and further validation is skipped.
    - If any required parameter is missing or has a null/empty value, an error is returned for each such parameter.
    - If all required parameters are present and non-empty, a pass result is returned.
    - For parameter descriptions and typical values, consult the SUEWS documentation: https://docs.suews.io/latest/
    """
    view = TypedConfigView.from_mapping(context.yaml_data)
    physics = view.physics.raw

    results = []
    if view.physics.is_empty:
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
        "net_radiation",
        "emissions",
        "storage_heat",
        "ohm_inc_qf",
        "roughness_length_momentum",
        "roughness_length_heat",
        "stability",
        "soil_moisture",
        "water_use",
        "roughness_sublayer",
        "frontal_area_index",
        "roughness_sublayer_level",
        "surface_conductance",
        "snow_use",
    ]

    # gh#1456: the STEBBS switches now live under model.physics.stebbs.*.
    # `enabled`/`parameters` (master toggle) carry sensible defaults and are
    # not required; the remaining switches are required, but checked against
    # the nested `stebbs` sub-object rather than the flat physics dict.
    required_stebbs_params = [
        "capacitance",
        "setpoint",
        "same_albedo_wall",
        "same_albedo_roof",
        "same_emissivity_wall",
        "same_emissivity_roof",
    ]
    stebbs_block = view.physics.stebbs_block

    # (display path, container, key) for every required physics switch.
    # The family switches are read from the flat model.physics dict; the
    # relocated STEBBS switches (gh#1456) from the nested model.physics.stebbs
    # sub-object.
    required_checks = [
        (f"model.physics.{param}", physics, param) for param in required_physics_params
    ] + [
        (f"model.physics.stebbs.{param}", stebbs_block, param)
        for param in required_stebbs_params
    ]

    # Resolve each required key rename-aware. In Phase-B-standalone / BC mode the
    # input YAML is not Phase-A-normalised, so a still-compatible config may use a
    # legacy spelling (e.g. `outer_cap_fraction` for `capacitance`, or the
    # long-renamed `netradiationmethod` for `net_radiation`). get_value_safe
    # accepts both spellings; the _MISSING sentinel distinguishes an absent key
    # from one present with a null value (so the two checks below stay distinct).
    _MISSING = object()
    _present = {
        path: get_value_safe(container, key, _MISSING)
        for path, container, key in required_checks
    }
    missing_params = [
        (path, key)
        for path, _container, key in required_checks
        if _present[path] is _MISSING
    ]
    if missing_params:
        for path, key in missing_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=path,
                    message=f"Physics parameter '{key}' is required but missing or null. This parameter controls critical model behaviour and must be specified for the simulation to run properly.",
                    suggested_value=f"Set '{key}' to an appropriate value. Consult the SUEWS documentation for parameter descriptions and typical values: https://docs.suews.io/latest/",
                )
            )

    def _is_empty(value):
        # Null / empty scalar, or a present-but-value-less mapping such as
        # `capacitance: {}` (get_value_safe returns the bare mapping when there
        # is no `value` key). A *non-empty* mapping is the gh#972 nested-family
        # form (e.g. `{spartacus: {value: 1}}`) and is NOT empty.
        # ValidationContext freezes dicts into read-only mappings.
        return value in ("", None) or (isinstance(value, Mapping) and not value)

    empty_params = [
        (path, key)
        for path, _container, key in required_checks
        if _present[path] is not _MISSING and _is_empty(_present[path])
    ]
    if empty_params:
        for path, key in empty_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=path,
                    message=f"Physics parameter '{key}' has null value. This parameter controls critical model behaviour and must be set for proper simulation.",
                    suggested_value=f"Set '{key}' to an appropriate non-null value. Check documentation for parameter details: https://docs.suews.io/stable",
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
    """
    Validate the dependency between `rslmethod` and `stabilitymethod` parameters.

    Parameters
    ----------
    rslmethod : int or None
        The roughness sublayer method identifier. Can be None or an integer.
    stabilitymethod : int or None
        The atmospheric stability method identifier. Can be None or an integer.

    Returns
    -------
    ValidationResult
        The result of the validation, indicating whether the parameter constraints are satisfied or an error with a suggested fix.

    Notes
    -----
    - If `rslmethod` is 2, `stabilitymethod` must be 3.
    - If `stabilitymethod` is 1, `rslmethod` must be provided (not None).
    """
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
    """Validate the legacy StorageHeatMethod-OhmIncQf pair."""
    return validate_storageheatmethod_dependencies(storageheatmethod, ohmincqf)[0]


def validate_storageheatmethod_dependencies(
    storageheatmethod,
    ohmincqf,
    netradiationmethod=None,
    stebbsmethod=None,
):
    """
    Validate the dependency between StorageHeatMethod and OhmIncQf model options.

    Parameters
    ----------
    storageheatmethod : int
        The value of the StorageHeatMethod parameter.
    ohmincqf : int
        The value of the OhmIncQf parameter.
    netradiationmethod : int, optional
        The value of the NetRadiationMethod parameter.
    stebbsmethod : int, optional
        The composed STEBBS method value.

    Returns
    -------
    list of ValidationResult
        Validation results for the storage heat dependency checks.

    Notes
    -----
    - If StorageHeatMethod == 1, OhmIncQf must be 0.
    - If OhmIncQf == 1, an OHM-like storage heat branch must be active.
    - StorageHeatMethod == 5 (EHC) runs with any NetRadiationMethod; only
      SPARTACUS codes resolve roof and wall facets.
    - If StorageHeatMethod == 7, NetRadiationMethod must be a SPARTACUS code
      and STEBBS must be active.
    """
    storageheatmethod = _method_code(storageheatmethod, "storage_heat")
    ohmincqf = _method_code(ohmincqf, "ohm_inc_qf")
    netradiationmethod = _method_code(netradiationmethod, "net_radiation")
    stebbsmethod = _method_code(stebbsmethod, "stebbs")

    results: List[ValidationResult] = []

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
    elif storageheatmethod == 8 and ohmincqf != 0:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message=(
                    "StorageHeatMethod 8 (dyohm_building) uses DyOHM only for "
                    "the building storage heat flux; all other land-cover "
                    "surfaces use ordinary OHM, which requires OhmIncQf=0."
                ),
                suggested_value="Set OhmIncQf to 0",
            )
        )
    elif ohmincqf == 1 and storageheatmethod not in {6, 7}:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message=(
                    "OhmIncQf=1 is only meaningful for OHM-like storage heat "
                    "branches that include QF. StorageHeatMethod=1 is documented "
                    "as OHM without QF and must use OhmIncQf=0."
                ),
                suggested_value="Use StorageHeatMethod 6 or 7 with OhmIncQf=1, or set OhmIncQf to 0.",
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

    if storageheatmethod == 5:
        # EHC runs with either radiation scheme (gh#1574): SPARTACUS supplies
        # roof/wall facet radiation; otherwise all land covers are lumped
        # into one plan-area slab from their own thermal layers.
        if netradiationmethod is not None and netradiationmethod > 1000:
            message = "StorageHeatMethod EHC-SPARTACUS compatibility validated"
        else:
            message = (
                "StorageHeatMethod=5 (EHC) with a non-SPARTACUS "
                "NetRadiationMethod: all land covers, buildings included, "
                "are lumped into one plan-area slab using each land "
                "cover's thermal_layers; roof and wall layers are unused. "
                "Roof and wall facets are resolved only with SPARTACUS "
                "(NetRadiationMethod > 1000)."
            )
        results.append(
            ValidationResult(
                status="PASS",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-netradiationmethod",
                message=message,
            )
        )

    if storageheatmethod == 7:
        if netradiationmethod not in {1001, 1002, 1003}:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="MODEL_OPTIONS",
                    parameter="storageheatmethod-netradiationmethod",
                    message=(
                        "StorageHeatMethod=7 uses separate STEBBS roof and wall "
                        "temperatures and therefore requires a SPARTACUS-Surface "
                        "NetRadiationMethod (1001, 1002, or 1003)."
                    ),
                    suggested_value=(
                        "Set net_radiation to a SPARTACUS method such as 1003 "
                        "or net_radiation.spartacus.ldown=air."
                    ),
                )
            )
        else:
            results.append(
                ValidationResult(
                    status="PASS",
                    category="MODEL_OPTIONS",
                    parameter="storageheatmethod-netradiationmethod",
                    message=(
                        "StorageHeatMethod STEBBS-SPARTACUS compatibility validated"
                    ),
                )
            )

        if stebbsmethod not in {1, 2}:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="MODEL_OPTIONS",
                    parameter="storageheatmethod-stebbsmethod",
                    message=(
                        "StorageHeatMethod=7 uses STEBBS storage heat and "
                        "requires STEBBS to be enabled."
                    ),
                    suggested_value="Set model.physics.stebbs.enabled=true with parameters default or provided.",
                )
            )
        else:
            results.append(
                ValidationResult(
                    status="PASS",
                    category="MODEL_OPTIONS",
                    parameter="storageheatmethod-stebbsmethod",
                    message="StorageHeatMethod-STEBBS compatibility validated",
                )
            )

    return results


def validate_rcmethod_stebbs_dependency(rcmethod, stebbsmethod):
    """Validate that non-default RC/capacitance choices are used with STEBBS."""
    rcmethod = _method_code(rcmethod, "capacitance")
    stebbsmethod = _method_code(stebbsmethod, "stebbs")

    if rcmethod in (None, 0):
        return []
    if stebbsmethod not in {1, 2}:
        return [
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="rcmethod-stebbsmethod",
                message=(
                    "Non-default stebbs.capacitance (rcmethod) is only "
                    "meaningful when STEBBS is enabled."
                ),
                suggested_value="Set model.physics.stebbs.enabled=true, or use stebbs.capacitance=default.",
            )
        ]
    return [
        ValidationResult(
            status="PASS",
            category="MODEL_OPTIONS",
            parameter="rcmethod-stebbsmethod",
            message="RC/capacitance-STEBBS compatibility validated",
        )
    ]


def validate_smdmethod_dependency(smdmethod, yaml_data):
    """
    Validate the dependency between SMDMethod and the presence of soil_observation configuration in site properties.

    Parameters
    ----------
    smdmethod : int
        The value of the SMDMethod parameter. If set to a truthy value (typically 1, indicating observed soil moisture), additional validation is performed.
    yaml_data : dict
        The parsed YAML configuration data containing site definitions and their properties.

    Returns
    -------
    list of ValidationResult
        A list of validation results. Returns an error if SMDMethod is set to observed and any site is missing the required 'soil_observation' block; otherwise, returns a pass result if validation succeeds. Returns an empty list if SMDMethod is modelled (0 or None).

    Notes
    -----
    - If SMDMethod is set to observed, each site must include a 'soil_observation' block in its properties.
    - The 'soil_observation' block should contain: layer_depth and optional layer_top, soil_not_rocks and bulk_density.
    - If any site is missing this block, an error is returned with a suggested fix.
    """
    results = []

    smdmethod = _method_code(smdmethod, "soil_moisture")
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
                        "layer_depth and optional layer_top, soil_not_rocks and bulk_density"
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
    """
    Validate dependencies and consistency between model physics options.

    This function checks for consistency and required dependencies among various physics-related
    model options defined in the YAML configuration file. It performs the following validations:
        - Ensures that the selected RSL method is compatible with the chosen stability method.
        - Checks compatibility between the storage heat method and the OhmIncQf option.
        - Validates that the SMD method is consistent with the presence of required soil observation data.
        - Validates that SPARTACUS is not combined with observed storage heat
          and, where buildings are present, uses a facet-resolved storage
          heat method (EHC or STEBBS).

    Parameters
    ----------
    context : ValidationContext
        The validation context containing the parsed YAML configuration data and any additional context needed for validation.

    Returns
    -------
    List[ValidationResult]
        A list of `ValidationResult` objects, each representing the outcome of the validation for
        a specific parameter or the overall section. The status can be "PASS", "ERROR", or "WARNING"
        depending on the validation result.
    """

    view = TypedConfigView.from_mapping(context.yaml_data)
    physics = view.physics

    results = []
    rslmethod = physics.read("roughness_sublayer")
    stabilitymethod = physics.read("stability")
    storageheatmethod = physics.read("storage_heat")
    ohmincqf = physics.read("ohm_inc_qf")
    smdmethod = physics.read("soil_moisture")
    netradiationmethod = physics.read("net_radiation")
    stebbsmethod = physics.stebbs_method
    rcmethod = physics.stebbs_value("capacitance")

    # RSL method and stability method dependencies
    result = validate_rslmethod_dependency(rslmethod=rslmethod, stabilitymethod=stabilitymethod)
    if result is not None:
        results.append(result)

    # Storage heat method and OhmIncQf compatibility check
    results.extend(
        validate_storageheatmethod_dependencies(
            storageheatmethod=storageheatmethod,
            ohmincqf=ohmincqf,
            netradiationmethod=netradiationmethod,
            stebbsmethod=stebbsmethod,
        )
    )

    # RC/capacitance method and STEBBS compatibility check
    results.extend(
        validate_rcmethod_stebbs_dependency(
            rcmethod=rcmethod,
            stebbsmethod=stebbsmethod,
        )
    )

    # SMDMethod and soil_observation dependency
    results.extend(
        validate_smdmethod_dependency(smdmethod=smdmethod, yaml_data=context.yaml_data)
    )

    # SPARTACUS and storage heat method dependency (gh#1834)
    results.extend(
        validate_spartacus_dependencies(
            bldgs_present=view.any_buildings_present(),
            netradiationmethod=netradiationmethod,
            storageheatmethod=storageheatmethod,
        )
    )

    return results


def check_rcmethod2_facet(required_params, building_archetype, site_idx, site_gridid, facet):
    """Validate required facet material parameters when ``capacitance == 2``."""
    provided_facet = []
    results = []
    for param in required_params:
        value = unwrap_entry(get_value_safe(building_archetype, param))
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
    """Validate outer heat-capacity fraction for a facet when ``capacitance == 1``."""
    key = f"fraction_heat_capacity_{facet}_external"
    facet_frac = read_scalar(building_archetype, key)

    result = ValidationResult(
        status="ERROR",
        category="MODEL_OPTIONS",
        parameter=f"building_archetype.{key}",
        site_index=site_idx,
        site_gridid=site_gridid,
    )

    if facet_frac is None:
        result.message = f"{key} must be explicitly provided when rcmethod == 1."
        result.suggested_value = f"Set {key} to a value between 0 and 1 (exclusive)."
        return result
    elif not (0 < facet_frac < 1):
        result.message = f"{key} value {facet_frac} is out of valid range (0, 1) when rcmethod == 1."
        result.suggested_value = f"Set {key} to a value strictly between 0 and 1."
        return result


@RulesRegistry.add_rule("rcmethod")
def validate_model_option_rcmethod(context) -> List[ValidationResult]:
    """Validate dependencies and consistency for the capacitance (rcmethod) option."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []
    rcmethod_value = view.physics.stebbs_value("capacitance")

    for site in view.sites:
        building_archetype = site.building_archetype
        site_gridid = site.gridid
        facets = ["roof", "wall"]

        if rcmethod_value == 1:
            for facet in facets:
                result = check_outercapfrac_facet(
                    building_archetype=building_archetype,
                    facet=facet,
                    site_idx=site.index,
                    site_gridid=site_gridid,
                )
                if result is not None:
                    results.append(result)

        elif rcmethod_value == 2:
            required_wall_params = [
                "thickness_wall_outer",
                "conductivity_wall_outer",
                "density_wall_outer",
                "specific_heat_capacity_wall_outer",
            ]
            required_roof_params = [
                "thickness_roof_outer",
                "conductivity_roof_outer",
                "density_roof_outer",
                "specific_heat_capacity_roof_outer",
            ]
            for facet, facet_params in zip(
                facets,
                [required_roof_params, required_wall_params],
            ):
                results.extend(
                    check_rcmethod2_facet(
                        required_params=facet_params,
                        building_archetype=building_archetype,
                        site_idx=site.index,
                        site_gridid=site_gridid,
                        facet=facet,
                    )
                )

    return results


def validate_model_option_same_albedo_facet(site_data, facet, *, site_view=None):
    """Generate the advisory warning when ``same_albedo_<facet> == 0``."""
    if site_view is None:
        site_view = TypedConfigView.from_mapping({"sites": [site_data]}).sites[0]
    facet_lower = facet.lower()
    layers_label = f"{facet_lower}s"
    facetrefl_key = f"reflectivity_{facet_lower}_external"
    finding = check_same_surface_unchecked_advisory(
        site_view.name,
        param_name=f"same_albedo_{facet_lower}",
        layers_label=layers_label,
        property_name="albedo",
        archetype_label=facetrefl_key,
        layer_values=site_view.facet_layer_values(layers_label, "alb"),
        archetype_value=site_view.scalar(site_view.building_archetype, facetrefl_key),
        site_index=site_view.index,
        site_gridid=site_view.gridid,
    )
    return ValidationResult.from_advisory(finding)


@RulesRegistry.add_rule("same_albedo")
def validate_model_option_same_albedo(context) -> List[ValidationResult]:
    """Emit advisory warnings when ``same_albedo_wall`` or ``same_albedo_roof`` is 0."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []
    same_albedo_roof = view.physics.stebbs_value("same_albedo_roof")
    same_albedo_wall = view.physics.stebbs_value("same_albedo_wall")

    if same_albedo_wall == 0:
        for site in view.sites:
            results.append(
                validate_model_option_same_albedo_facet(site.raw, "Wall", site_view=site)
            )
    if same_albedo_roof == 0:
        for site in view.sites:
            results.append(
                validate_model_option_same_albedo_facet(site.raw, "Roof", site_view=site)
            )

    return results


@RulesRegistry.add_rule("same_emissivity")
def validate_model_option_same_emissivity(context) -> List[ValidationResult]:
    """Emit advisory warnings when ``same_emissivity_wall`` or ``same_emissivity_roof`` is 0."""
    view = TypedConfigView.from_mapping(context.yaml_data)
    results = []
    same_emissivity_roof = view.physics.stebbs_value("same_emissivity_roof")
    same_emissivity_wall = view.physics.stebbs_value("same_emissivity_wall")

    for switch_val, param_name, layers_label, arch_key, arch_label in (
        (
            same_emissivity_wall,
            "same_emissivity_wall",
            "walls",
            "emissivity_wall_external",
            "wall_external_emissivity",
        ),
        (
            same_emissivity_roof,
            "same_emissivity_roof",
            "roofs",
            "emissivity_roof_external",
            "roof_external_emissivity",
        ),
    ):
        if switch_val != 0:
            continue
        for site in view.sites:
            finding = check_same_surface_unchecked_advisory(
                site.name,
                param_name=param_name,
                layers_label=layers_label,
                property_name="emissivity",
                archetype_label=arch_label,
                layer_values=site.facet_layer_values(layers_label, "emis"),
                archetype_value=site.scalar(site.building_archetype, arch_key),
                site_index=site.index,
                site_gridid=site.gridid,
            )
            results.append(ValidationResult.from_advisory(finding))

    return results


@RulesRegistry.add_rule("forcing_height")
def validate_forcing_height_vs_buildings(context) -> List[ValidationResult]:
    """Validate that forcing height (z) lies within an acceptable range relative to urban morphology.

    Sites without buildings (``bldgs.sfr <= 0``) are skipped because they have
    no building height to scale ``z`` against.
    """
    view = TypedConfigView.from_mapping(context.yaml_data)
    results: List[ValidationResult] = []

    stebbsmethod_val = view.physics.stebbs_method_int
    netradiationmethod_val = view.physics.code("net_radiation", 0)

    for site in view.sites:
        site_idx = site.index
        site_gridid = site.gridid
        site_name = site.name

        raw_z = unwrap_entry(site.properties.get("z"))
        if raw_z is None:
            continue
        z = _as_float(raw_z)
        if z is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="FORCING",
                    parameter="properties.z",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': forcing height z={raw_z!r} must be a finite number."
                    ),
                    suggested_value="Provide a finite positive forcing height z (in metres).",
                )
            )
            continue

        bldgs = unwrap_entry(site.land_cover.get("bldgs"))
        bldgh = _as_float(bldgs.get("bldgh")) if isinstance(bldgs, Mapping) else None
        sfr = site.surface_sfr("bldgs")

        # A site without buildings (bldgs.sfr == 0) has no building height to
        # scale z against, whatever bldgh holds: skip the check.
        if sfr is not None and sfr <= 0.0:
            continue

        stebbs_height = None
        if stebbsmethod_val == 1:
            stebbs_height = _as_float(site.building_archetype.get("archetype_height"))

        spartacus_top = None
        if netradiationmethod_val in SPARTACUS_METHODS:
            spartacus_top = site.last_nonzero_height()

        # --- Check for missing bldgh or sfr ---
        if bldgh is None or sfr is None:
            missing = []
            if bldgh is None:
                missing.append("land_cover.bldgs.bldgh")
            if sfr is None:
                missing.append("land_cover.bldgs.sfr")
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="FORCING",
                    parameter="properties.land_cover.bldgs",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': cannot validate forcing height z={z} against building height and sfr because {', '.join(missing)} is missing."
                    ),
                    suggested_value="Provide both land_cover.bldgs.bldgh and land_cover.bldgs.sfr to enable this check.",
                )
            )
            continue

        # --- Determine max building height ---
        # bldgh is guaranteed non-None here (checked above), so candidates is never empty
        candidates = [h for h in [bldgh, stebbs_height, spartacus_top] if h is not None]
        h_max = max(candidates)
        h_mean = bldgh  # mean building height is always bldgh

        # --- Determine allowed z range based on sfr ---
        if sfr < 0.35:
            min_factor = 2.0
        else:
            min_factor = 1.5
        max_factor = 5.0

        min_z_mean = min_factor * h_mean
        max_z_mean = max_factor * h_mean
        min_z_max = min_factor * h_max
        max_z_max = max_factor * h_max

        # --- Check z against allowed range for mean height (ERROR) ---
        if z < min_z_mean:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="FORCING",
                    parameter="properties.z",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': forcing height z={z} is below the minimum allowed ({min_factor}* mean building height = {min_z_mean}) for sfr={sfr}."
                    ),
                    suggested_value=f"Increase z to >= {min_z_mean} (mean building height={h_mean}, sfr={sfr})",
                )
            )
        elif z > max_z_mean:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="FORCING",
                    parameter="properties.z",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': forcing height z={z} is above the maximum allowed ({max_factor}* mean building height = {max_z_mean})."
                    ),
                    suggested_value=f"Reduce z to <= {max_z_mean} (mean building height={h_mean}).",
                )
            )

        # --- Check z against allowed range for max height (WARNING) ---
        if z < min_z_max:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="FORCING",
                    parameter="properties.z",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': forcing height z={z} is below the minimum allowed ({min_factor}* max building height = {min_z_max}) for sfr={sfr}."
                    ),
                    suggested_value=f"Increase z to >= {min_z_max} (max building height={h_max}, sfr={sfr}).",
                )
            )
        elif z > max_z_max:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="FORCING",
                    parameter="properties.z",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=(
                        f"Site '{site_name}': forcing height z={z} is above the maximum allowed ({max_factor}* max building height = {max_z_max})."
                    ),
                    suggested_value=f"Reduce z to <= {max_z_max} (max building height={h_max}).",
                )
            )

    return results
