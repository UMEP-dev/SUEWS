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