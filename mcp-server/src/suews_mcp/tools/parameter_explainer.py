"""Parameter explanation tool for SUEWS MCP."""

from typing import Dict, Optional
import logging

logger = logging.getLogger(__name__)

# Try to import SuPy data models for enhanced validation
try:
    from supy.data_model import (
        SUEWSConfig,
        SiteParameters, 
        SurfaceParameters,
        AnthroHeatParameters
    )
    SUPY_AVAILABLE = True
    logger.info("SuPy data models loaded for enhanced parameter information")
except ImportError:
    SUPY_AVAILABLE = False
    logger.info("SuPy not available - using built-in knowledge base only")

# Comprehensive parameter knowledge base
PARAMETER_KNOWLEDGE = {
    # Model Control Parameters
    "tstep": {
        "name": "Time Step",
        "unit": "seconds",
        "description": "Model calculation time step. Controls temporal resolution of calculations.",
        "typical_values": {
            "recommended": 300,
            "range": [60, 3600],
            "note": "5 minutes (300s) balances accuracy and computation time",
        },
        "scientific_context": "Shorter time steps improve accuracy for rapidly changing conditions (e.g., shadows, clouds) but increase computation time. Must be ≤ forcing data interval.",
        "examples": [
            "300: Standard 5-minute step for most applications",
            "60: High resolution for detailed shadow/radiation studies",
            "900: 15 minutes for long-term climate runs",
        ],
    },
    # Physics Method Parameters
    "NetRadiationMethod": {
        "name": "Net Radiation Method",
        "unit": "method code",
        "description": "Method for calculating net all-wave radiation (Q*).",
        "typical_values": {
            "recommended": 3,
            "options": {
                0: "OBSERVED - Use measured Q* from forcing",
                1: "LDOWN_OBSERVED - Model Q* with observed L↓",
                2: "LDOWN_CLOUD - Model Q* using cloud cover",
                3: "LDOWN_AIR - Model Q* from air temp/RH (recommended)",
            },
        },
        "scientific_context": "Q* = K↓ - K↑ + L↓ - L↑. Method 3 (LDOWN_AIR) is most robust as it only requires standard meteorological measurements.",
        "examples": [
            "3: Best for operational use with standard met data",
            "1: When longwave radiation measurements available",
            "0: For validation studies with complete radiation obs",
        ],
    },
    "StorageHeatMethod": {
        "name": "Storage Heat Flux Method",
        "unit": "method code",
        "description": "Method for calculating storage heat flux (ΔQS).",
        "typical_values": {
            "recommended": 1,
            "options": {
                0: "OBSERVED - Rare, uses measured ΔQS",
                1: "OHM - Objective Hysteresis Model",
                4: "AnOHM - Analytical OHM",
                5: "ESTM - Element Surface Temperature Method",
                6: "OHM_ENHANCED - Enhanced OHM",
            },
        },
        "scientific_context": "ΔQS represents heat stored/released by urban fabric. OHM relates ΔQS to Q* through empirical coefficients capturing thermal inertia.",
        "examples": [
            "1: Standard OHM for most urban studies",
            "5: ESTM when detailed thermal properties known",
            "6: Enhanced OHM for improved accuracy",
        ],
    },
    # Site Parameters
    "latitude": {
        "name": "Latitude",
        "unit": "decimal degrees",
        "description": "Site latitude for solar calculations.",
        "typical_values": {"range": [-90, 90], "note": "Positive = North, Negative = South"},
        "scientific_context": "Controls solar angles, day length, and seasonal variations. Critical for radiation calculations.",
        "examples": ["51.5: London, UK", "40.7: New York, USA", "-33.9: Sydney, Australia"],
    },
    "z": {
        "name": "Measurement Height",
        "unit": "metres",
        "description": "Height of meteorological measurements above ground.",
        "typical_values": {
            "recommended": 10,
            "range": [2, 50],
            "note": "Standard meteorological measurement height",
        },
        "scientific_context": "Used for wind profile extrapolation and stability calculations. Should be > 2×mean building height for representative measurements.",
        "examples": [
            "10: Standard met station height",
            "2: Near-surface measurements",
            "30: Rooftop station in urban area",
        ],
    },
    "bldgh": {
        "name": "Mean Building Height",
        "unit": "metres",
        "description": "Area-weighted mean height of buildings.",
        "typical_values": {"city_centre": [15, 50], "suburban": [6, 12], "industrial": [8, 15]},
        "scientific_context": "Key morphometric parameter affecting roughness, displacement height, and wind flow. Use area-weighted mean, not simple average.",
        "examples": [
            "25: Typical city centre",
            "8: Suburban residential",
            "40: High-rise district",
        ],
    },
    "faibldg": {
        "name": "Frontal Area Index - Buildings",
        "unit": "dimensionless",
        "description": "Ratio of building frontal area to plan area.",
        "typical_values": {
            "recommended": "0.25 × λp (plan area fraction)",
            "range": [0.1, 1.0],
            "city_centre": [0.3, 0.6],
        },
        "scientific_context": "Represents aerodynamic 'blockiness' of urban area. Critical for roughness length and drag calculations. FAI = Af/AT where Af is frontal area.",
        "examples": [
            "0.1: Open low-rise development",
            "0.4: Typical urban area",
            "0.6: Dense city centre",
        ],
    },
    # Surface Parameters
    "alb": {
        "name": "Albedo",
        "unit": "dimensionless",
        "description": "Surface reflectivity for shortwave radiation.",
        "typical_values": {
            "concrete": [0.1, 0.35],
            "asphalt": [0.05, 0.20],
            "grass": [0.15, 0.25],
            "trees": [0.10, 0.20],
            "water": [0.03, 0.10],
        },
        "scientific_context": "Fraction of incoming solar radiation reflected. Fresh surfaces have higher albedo. Dark/wet surfaces absorb more radiation.",
        "examples": [
            "0.15: Typical urban surface mix",
            "0.08: Fresh asphalt (very dark)",
            "0.30: Light concrete",
        ],
    },
    "emis": {
        "name": "Emissivity",
        "unit": "dimensionless",
        "description": "Surface emissivity for longwave radiation.",
        "typical_values": {"recommended": 0.95, "range": [0.85, 0.99], "urban": [0.90, 0.97]},
        "scientific_context": "Efficiency of longwave radiation emission. Most urban surfaces are near-blackbodies (ε ≈ 0.95). Lower for metals/glass.",
        "examples": [
            "0.95: Most urban surfaces",
            "0.88: Glass/metal facades",
            "0.97: Vegetation/water",
        ],
    },
    # Vegetation Parameters
    "lai_max": {
        "name": "Maximum Leaf Area Index",
        "unit": "m²/m²",
        "description": "Maximum one-sided leaf area per unit ground area.",
        "typical_values": {
            "grass": [2, 4],
            "deciduous": [4, 7],
            "evergreen": [3, 6],
            "urban_trees": [3, 5],
        },
        "scientific_context": "Peak LAI during growing season. Controls maximum transpiration and interception capacity. Urban trees often have lower LAI than forest.",
        "examples": ["5.5: Healthy deciduous trees", "3.0: Urban grass", "4.0: Street trees"],
    },
    # Water Balance Parameters
    "soilstore_capacity": {
        "name": "Soil Store Capacity",
        "unit": "mm",
        "description": "Maximum water storage in soil layer.",
        "typical_values": {"grass": [100, 200], "trees": [150, 300], "paved": [0, 10]},
        "scientific_context": "Available water for evapotranspiration. Function of soil depth, type, and root zone. Urban soils often compacted with reduced capacity.",
        "examples": [
            "150: Typical urban greenspace",
            "5: Minimal storage in paved areas",
            "250: Deep soil under trees",
        ],
    },
    # Anthropogenic Parameters
    "population_density": {
        "name": "Population Density",
        "unit": "people/hectare",
        "description": "Resident population density for QF calculations.",
        "typical_values": {
            "city_centre": [5000, 15000],
            "suburban": [1000, 5000],
            "residential": [2000, 8000],
        },
        "scientific_context": "Used to estimate metabolic heat release and activity patterns. Consider both residential and workplace populations.",
        "examples": ["10000: Dense city centre", "3000: Suburban area", "100: Urban park"],
    },
    "qf_a": {
        "name": "QF Coefficient A",
        "unit": "W/m²/K",
        "description": "Temperature-dependent anthropogenic heat coefficient.",
        "typical_values": {"weekday": [0.1, 0.5], "weekend": [0.05, 0.3], "range": [-0.5, 1.0]},
        "scientific_context": "Linear coefficient in QF = QF0 + a(T - T0). Positive in cold climates (heating), negative in hot climates (cooling).",
        "examples": [
            "0.2: Moderate climate weekday",
            "0.4: Cold climate with heating",
            "-0.3: Hot climate with AC",
        ],
    },
}


def get_supy_field_info(param_name: str) -> Optional[Dict[str, str]]:
    """Get field information from SuPy data models if available."""
    if not SUPY_AVAILABLE:
        return None
    
    try:
        # Check all data models for the field
        models = [
            (SUEWSConfig, "SUEWSConfig"),
            (SiteParameters, "SiteParameters"),
            (SurfaceParameters, "SurfaceParameters"),
            (AnthroHeatParameters, "AnthroHeatParameters")
        ]
        
        for model_class, model_name in models:
            if hasattr(model_class, '__fields__'):
                fields = model_class.__fields__
                if param_name in fields:
                    field = fields[param_name]
                    return {
                        'model': model_name,
                        'type': str(field.annotation),
                        'required': field.is_required(),
                        'default': str(field.default) if field.default is not None else None,
                        'description': field.field_info.description if hasattr(field.field_info, 'description') else None
                    }
        
        # Check nested fields
        if hasattr(SUEWSConfig, 'model_fields'):
            # Pydantic v2 style
            for field_name, field_info in SUEWSConfig.model_fields.items():
                if param_name in str(field_info):
                    return {
                        'model': 'SUEWSConfig',
                        'parent_field': field_name,
                        'info': 'Nested parameter'
                    }
                    
    except Exception as e:
        logger.debug(f"Error getting SuPy field info: {e}")
    
    return None


def get_parameter_info(param_name: str) -> Optional[Dict]:
    """Get parameter information from knowledge base and SuPy models."""
    # Try exact match first
    if param_name in PARAMETER_KNOWLEDGE:
        info = PARAMETER_KNOWLEDGE[param_name].copy()
        
        # Enhance with SuPy field info if available
        supy_info = get_supy_field_info(param_name)
        if supy_info:
            info['supy_model_info'] = supy_info
            
        return info

    # Try case-insensitive match
    param_lower = param_name.lower()
    for key, value in PARAMETER_KNOWLEDGE.items():
        if key.lower() == param_lower:
            info = value.copy()
            supy_info = get_supy_field_info(key)
            if supy_info:
                info['supy_model_info'] = supy_info
            return info

    # Try partial match
    for key, value in PARAMETER_KNOWLEDGE.items():
        if param_lower in key.lower() or key.lower() in param_lower:
            info = value.copy()
            supy_info = get_supy_field_info(key)
            if supy_info:
                info['supy_model_info'] = supy_info
            return info

    # If not in knowledge base, try SuPy models directly
    supy_info = get_supy_field_info(param_name)
    if supy_info:
        return {
            'name': param_name,
            'from_supy_model': True,
            'supy_model_info': supy_info,
            'description': f"Parameter found in {supy_info['model']}",
            'unit': 'See model documentation',
            'typical_values': {},
            'scientific_context': f"This parameter is defined in the {supy_info['model']} data model."
        }

    return None


def format_parameter_explanation(param_info: Dict, param_name: str, include_examples: bool) -> str:
    """Format parameter information into readable explanation."""
    output = f"# Parameter: {param_info['name']} ({param_name})\n\n"

    output += f"**Unit**: {param_info['unit']}\n\n"
    output += f"**Description**: {param_info['description']}\n\n"

    # Typical values
    output += "## Typical Values\n\n"
    typical = param_info["typical_values"]

    if "recommended" in typical:
        output += f"- **Recommended**: {typical['recommended']}\n"

    if "range" in typical:
        range_val = typical["range"]
        output += f"- **Range**: {range_val[0]} to {range_val[1]}\n"

    if "options" in typical:
        output += "- **Options**:\n"
        for code, desc in typical["options"].items():
            output += f"  - `{code}`: {desc}\n"

    # Add other typical value categories
    for key, value in typical.items():
        if key not in ["recommended", "range", "options", "note"]:
            if isinstance(value, list):
                output += f"- **{key.title()}**: {value[0]} to {value[1]}\n"
            else:
                output += f"- **{key.title()}**: {value}\n"

    if "note" in typical:
        output += f"\n*Note: {typical['note']}*\n"

    output += "\n"

    # Scientific context
    output += f"## Scientific Context\n\n{param_info['scientific_context']}\n\n"
    
    # SuPy model information if available
    if 'supy_model_info' in param_info and param_info['supy_model_info']:
        supy_info = param_info['supy_model_info']
        output += "## Data Model Information\n\n"
        output += f"- **Model**: `{supy_info.get('model', 'Unknown')}`\n"
        if 'type' in supy_info:
            output += f"- **Type**: `{supy_info['type']}`\n"
        if 'required' in supy_info:
            output += f"- **Required**: {'Yes' if supy_info['required'] else 'No'}\n"
        if 'default' in supy_info and supy_info['default']:
            output += f"- **Default**: `{supy_info['default']}`\n"
        output += "\n"

    # Examples
    if include_examples and "examples" in param_info:
        output += "## Examples\n\n"
        for example in param_info["examples"]:
            output += f"- {example}\n"
        output += "\n"

    # Related parameters
    output += "## Related Parameters\n\n"

    # Suggest related parameters based on the current one
    if param_name in ["NetRadiationMethod", "StorageHeatMethod", "EmissionsMethod"]:
        output += "- Other model physics methods\n"
        output += "- `OHMIncQF` (for storage heat calculations)\n"
    elif param_name in ["alb", "emis"]:
        output += "- Surface radiation properties for all surface types\n"
        output += "- `NetRadiationMethod` for radiation calculations\n"
    elif param_name in ["bldgh", "faibldg"]:
        output += "- `z0m` (roughness length for momentum)\n"
        output += "- `zd` (displacement height)\n"
        output += "- Building surface properties\n"
    elif param_name in ["lai_max", "lai_min"]:
        output += "- Vegetation parameters for all vegetation types\n"
        output += "- `gsmodel` (stomatal conductance model)\n"

    return output


async def explain_parameter(parameter_name: str, include_examples: bool = True) -> str:
    """Explain a SUEWS parameter with scientific context."""

    # Get parameter information
    param_info = get_parameter_info(parameter_name)

    if not param_info:
        # Try to provide helpful suggestions
        suggestions = []
        param_lower = parameter_name.lower()

        for key in PARAMETER_KNOWLEDGE.keys():
            if (len(param_lower) > 2 and param_lower[:3] in key.lower()) or (
                len(key) > 2 and key[:3].lower() in param_lower
            ):
                suggestions.append(key)

        response = f"# Parameter Not Found: '{parameter_name}'\n\n"
        response += "The parameter was not found in the knowledge base.\n\n"

        if suggestions:
            response += "## Did you mean?\n\n"
            for sugg in suggestions[:5]:
                response += f"- `{sugg}` - {PARAMETER_KNOWLEDGE[sugg]['name']}\n"
            response += "\n"

        response += "## Common Parameter Categories\n\n"
        response += "- **Model Control**: tstep, forcing_file, output_file\n"
        response += "- **Model Physics**: NetRadiationMethod, StorageHeatMethod, EmissionsMethod\n"
        response += "- **Site Properties**: latitude, longitude, altitude, timezone\n"
        response += "- **Surface Properties**: alb (albedo), emis (emissivity), heights\n"
        response += "- **Vegetation**: lai_max, lai_min, gsmodel parameters\n"
        response += "- **Anthropogenic**: population_density, qf_a, qf_b, qf_c\n"
        response += "- **Water Balance**: soilstore_capacity, runoff parameters\n"

        return response

    # Format and return explanation
    return format_parameter_explanation(param_info, parameter_name, include_examples)
