"""Parameter suggestion tool for SUEWS MCP."""

from typing import Any, Dict
import json

from ..utils import SUEWSBridge


async def suggest_parameters(
    partial_config: str, context: str = "general", climate_zone: str = "temperate"
) -> str:
    """
    Suggest parameters based on partial configuration and context.

    Args:
        partial_config: Path to partial YAML configuration
        context: Urban context (city_centre, suburban, industrial, etc.)
        climate_zone: Climate classification

    Returns:
        Parameter suggestions with scientific rationale
    """
    bridge = SUEWSBridge()

    try:
        # Load partial configuration
        config_dict = bridge.load_config_from_yaml(partial_config)

        # Get suggestions based on context
        suggestions = get_contextual_suggestions(config_dict, context, climate_zone)

        return format_suggestions(suggestions, context, climate_zone)

    except Exception as e:
        return f"❌ Error generating suggestions: {str(e)}"


def get_contextual_suggestions(
    partial_config: Dict[str, Any], context: str, climate_zone: str
) -> Dict[str, Any]:
    """Generate context-aware parameter suggestions."""

    suggestions = {}

    # Building morphology suggestions
    morphology_values = {
        "city_centre": {
            "height_mean": {
                "value": 25.0,
                "range": [15.0, 50.0],
                "rationale": "Typical for central business districts",
            },
            "height_stdev": {
                "value": 10.0,
                "range": [5.0, 20.0],
                "rationale": "Moderate variability in building heights",
            },
            "frontal_area_index": {
                "value": 0.35,
                "range": [0.25, 0.45],
                "rationale": "Dense urban development",
            },
            "plan_area_fraction": {
                "value": 0.45,
                "range": [0.35, 0.55],
                "rationale": "High building coverage",
            },
        },
        "suburban": {
            "height_mean": {
                "value": 8.0,
                "range": [5.0, 12.0],
                "rationale": "Low-rise residential buildings",
            },
            "height_stdev": {
                "value": 3.0,
                "range": [1.0, 5.0],
                "rationale": "Relatively uniform building heights",
            },
            "frontal_area_index": {
                "value": 0.15,
                "range": [0.10, 0.25],
                "rationale": "Sparse building arrangement",
            },
            "plan_area_fraction": {
                "value": 0.25,
                "range": [0.15, 0.35],
                "rationale": "Lower building density",
            },
        },
        "industrial": {
            "height_mean": {
                "value": 12.0,
                "range": [8.0, 20.0],
                "rationale": "Warehouses and industrial buildings",
            },
            "height_stdev": {
                "value": 5.0,
                "range": [2.0, 8.0],
                "rationale": "Mixed building types",
            },
            "frontal_area_index": {
                "value": 0.20,
                "range": [0.15, 0.30],
                "rationale": "Large footprint buildings",
            },
            "plan_area_fraction": {
                "value": 0.35,
                "range": [0.25, 0.45],
                "rationale": "Significant impervious coverage",
            },
        },
    }

    # Surface cover suggestions
    surface_fractions = {
        "city_centre": {
            "building_fraction": 0.45,
            "paved_fraction": 0.40,
            "evergreen_fraction": 0.02,
            "deciduous_fraction": 0.03,
            "grass_fraction": 0.05,
            "bare_soil_fraction": 0.02,
            "water_fraction": 0.03,
        },
        "suburban": {
            "building_fraction": 0.25,
            "paved_fraction": 0.25,
            "evergreen_fraction": 0.05,
            "deciduous_fraction": 0.10,
            "grass_fraction": 0.30,
            "bare_soil_fraction": 0.03,
            "water_fraction": 0.02,
        },
        "industrial": {
            "building_fraction": 0.35,
            "paved_fraction": 0.50,
            "evergreen_fraction": 0.02,
            "deciduous_fraction": 0.03,
            "grass_fraction": 0.08,
            "bare_soil_fraction": 0.02,
            "water_fraction": 0.00,
        },
    }

    # Climate-specific adjustments
    climate_adjustments = {
        "tropical": {
            "albedo_adjustment": -0.05,  # Darker surfaces in tropics
            "emissivity_adjustment": 0.02,  # Higher moisture content
            "runoff_adjustment": 0.1,  # Higher rainfall intensity
        },
        "arid": {
            "albedo_adjustment": 0.10,  # Lighter, dusty surfaces
            "emissivity_adjustment": -0.02,  # Dry materials
            "runoff_adjustment": -0.2,  # Lower rainfall, higher infiltration
        },
        "temperate": {
            "albedo_adjustment": 0.0,
            "emissivity_adjustment": 0.0,
            "runoff_adjustment": 0.0,
        },
    }

    # Build suggestions
    if context in morphology_values:
        suggestions["morphology"] = morphology_values[context]

    if context in surface_fractions:
        suggestions["surface_fractions"] = surface_fractions[context]

    if climate_zone in climate_adjustments:
        suggestions["climate_adjustments"] = climate_adjustments[climate_zone]

    # Add physics method suggestions
    suggestions["recommended_methods"] = get_method_recommendations(context, climate_zone)

    return suggestions


def get_method_recommendations(context: str, climate_zone: str) -> Dict[str, str]:
    """Recommend physics methods based on context."""

    # Base recommendations
    methods = {
        "NetRadiationMethod": "NARP",  # Default net all-wave radiation
        "StorageHeatMethod": "OHM",  # Default storage heat flux
        "EmissionsMethod": "FixedWeekly",  # Default anthropogenic heat
        "RoughnessMethod": "RM3",  # Default roughness calculation
        "StabilityMethod": "BusStab",  # Default stability
        "RSLMethod": "MOST",  # Default urban microclimate
    }

    # Context-specific adjustments
    if context == "city_centre":
        methods["StorageHeatMethod"] = "ESTM"  # Better for complex geometries
        methods["RSLMethod"] = "RST"  # Better for tall buildings
        methods["EmissionsMethod"] = "DailyMonthlyProfile"  # Detailed emissions

    if climate_zone == "tropical":
        methods["StabilityMethod"] = "SG2000"  # Better for convective conditions

    return methods


def format_suggestions(suggestions: Dict[str, Any], context: str, climate_zone: str) -> str:
    """Format suggestions into readable output."""

    output = f"""🎯 **Parameter Suggestions for {context.replace("_", " ").title()} Site**

Climate Zone: {climate_zone.title()}

## Building Morphology Parameters

"""

    if "morphology" in suggestions:
        for param, info in suggestions["morphology"].items():
            output += f"**{param}**:\n"
            output += f"- Suggested value: {info['value']}\n"
            output += f"- Typical range: {info['range'][0]} - {info['range'][1]}\n"
            output += f"- Rationale: {info['rationale']}\n\n"

    if "surface_fractions" in suggestions:
        output += "## Surface Cover Fractions\n\n"
        output += "Suggested fractions (must sum to 1.0):\n"
        for surface, fraction in suggestions["surface_fractions"].items():
            output += f"- {surface}: {fraction}\n"

        total = sum(suggestions["surface_fractions"].values())
        output += f"\nTotal: {total:.2f} ✓\n"

    if "recommended_methods" in suggestions:
        output += "\n## Recommended Physics Methods\n\n"
        for method, selection in suggestions["recommended_methods"].items():
            output += f"- {method}: **{selection}**\n"

    if "climate_adjustments" in suggestions:
        output += f"\n## Climate-Specific Adjustments\n\n"
        adjustments = suggestions["climate_adjustments"]
        output += f"- Albedo adjustment: {adjustments['albedo_adjustment']:+.2f}\n"
        output += f"- Emissivity adjustment: {adjustments['emissivity_adjustment']:+.2f}\n"
        output += f"- Runoff adjustment: {adjustments['runoff_adjustment']:+.2f}\n"

    output += """
## Next Steps

1. Apply these suggestions to your configuration
2. Run `validate_config` to check for errors
3. Use `check_physics_compatibility` to verify method selection
4. Consider site-specific adjustments based on local knowledge
"""

    return output
