from .rules_core import (
    RulesRegistry,
    ValidationResult,
)

def check_archetype_radiation_properties(archetype_data, facet, site_index, site_gridid):
    """Validate parameters for STEBBS radiation checks."""

    archetype_wall_reflectivity = archetype_data.get("WallReflectivity").get("value")
    archetype_wall_absorbtivity = archetype_data.get("WallAbsorbtivity").get("value")
    archetype_wall_transmissivity = archetype_data.get("WallTransmissivity").get("value")

    radiation_properties_total = archetype_wall_reflectivity + archetype_wall_absorbtivity + archetype_wall_transmissivity

    if radiation_properties_total != 1.0:
        result = ValidationResult(
            status="ERROR",
            category="Archetype",
            parameter="WallReflectivity, WallAbsorbtivity, WallTransmissivity",
            site_index=site_index,
            site_gridid=site_gridid,
            message=f"Facet reflectivity, absorbtivity and transmissivity must sum to 1. Current total = {radiation_properties_total}",
            suggested_value=None,
            applied_fix=False,
        )
    else:
        result = ValidationResult(
            status="PASS",
            category="Archetype",
            parameter="",
        )

    return result


@RulesRegistry.add_phase_b("archetype_properties")
def check_archetype_properties(config_data):
    errors = []

    sites = config_data.get("sites", [])
    for i, site in enumerate(sites):
        site_props = site.get("properties", {})
        archetype_props = site_props.get("building_archetype", {})

        for facet in ["Wall", "Roof"]:
            errors.append(
                check_archetype_radiation_properties(archetype_props, facet=facet, site_index=i, site_gridid=site.get("gridiv"))
            )
    return errors