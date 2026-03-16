from .rules_core import (
    RulesRegistry,
    ValidationResult,
)

def check_archetype_radiation_properties(archetype_data, facet):
    """Validate parameters for STEBBS radiation checks."""
    result = ValidationResult(
        status="PASS",
        category="Archetype",
        parameter=f"{facet}Reflectivity, {facet}Absorbtivity, {facet}Transmissivity",
    )

    archetype_facet_reflectivity = archetype_data.get(f"{facet}Reflectivity").get("value")
    archetype_facet_absorbtivity = archetype_data.get(f"{facet}Absorbtivity").get("value")
    archetype_facet_transmissivity = archetype_data.get(f"{facet}Transmissivity").get("value")

    radiation_properties_total = archetype_facet_reflectivity + archetype_facet_absorbtivity + archetype_facet_transmissivity

    if radiation_properties_total != 1.0:
        result.status="ERROR"
        result.message = f"Facet reflectivity, absorbtivity and transmissivity must sum to 1. Current total = {radiation_properties_total}"

    return result

@RulesRegistry.add_phase_b("archetype_properties")
def check_archetype_properties(config_data):
    errors = []

    sites = config_data.get("sites", [])
    for i, site in enumerate(sites):
        site_props = site.get("properties", {})
        archetype_props = site_props.get("building_archetype", {})

        for facet in ["Wall", "Roof"]:
            result = check_archetype_radiation_properties(archetype_props, facet=facet)
            result.site_index = i
            result.site_gridid = site.get("gridiv")
            errors.append(result)

    return errors

@RulesRegistry.add_phase_b("stebbs_props")
def check_stebbs_properties(yaml_data):
    results = []
    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        stebbs = props.get("stebbs", {})
        site_gridid = site.get("gridiv")
        hwfp_entry = stebbs.get("HotWaterFlowProfile", {})
        for daytype in ("working_day", "holiday"):
            day_profile = hwfp_entry.get(daytype, {})
            if isinstance(day_profile, dict):
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
