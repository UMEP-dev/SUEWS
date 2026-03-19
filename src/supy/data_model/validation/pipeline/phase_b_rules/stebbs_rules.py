from .rules_core import (
    RulesRegistry,
    ValidationResult,
)
from collections.abc import Mapping
from ...core.yaml_helpers import get_value_safe

def check_archetype_radiation_properties(archetype_data, facet):
    """Validate parameters for STEBBS radiation checks."""
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
    yaml_data = context.yaml_data

    results = []
    physics = yaml_data.get("model", {}).get("physics", {})
    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 1:
        sites = config_data.get("sites", [])
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
    """Check for inconsistency: zero occupants with nonzero metabolism."""
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


@RulesRegistry.add_rule("stebbs_props")
def check_stebbs_properties(context):
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
