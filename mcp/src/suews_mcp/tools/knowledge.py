"""Knowledge tools - SUEWS domain knowledge access."""

import json
import re
from pathlib import Path
from typing import Any, Optional


def get_config_schema() -> dict[str, Any]:
    """Get SUEWS configuration schema overview and navigation guide.

    The full JSON Schema (92k tokens) exceeds MCP response limits.
    Instead, this returns a high-level overview and guides you to better
    tools for detailed exploration.

    Returns:
        Dictionary with schema overview and navigation guide
    """
    try:
        from supy.data_model.core.config import SUEWSConfig

        # Get basic schema info without full expansion
        schema = SUEWSConfig.model_json_schema()
        top_level_props = list(schema.get("properties", {}).keys())

        return {
            "success": True,
            "schema_overview": {
                "schema_version": "0.1",
                "top_level_sections": top_level_props,
                "description": "SUEWS configuration schema generated from Pydantic data models",
            },
            "navigation_guide": {
                "list_all_models": "Use list_available_models() to see all configurable models",
                "config_details": "Use get_config_docs('ModelName') for specific configuration parameters",
                "example_configs": "See test/fixtures/ directory for working configuration examples",
                "validation": "Use validate_config() to check if a YAML file is valid",
            },
            "schema_url": "https://umep-dev.github.io/SUEWS/schema/suews-config/latest",
            "guidance": "The full schema is too large for MCP (92k tokens). Use list_available_models() + get_config_docs() for detailed exploration of specific models.",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


def get_config_docs(config_name: str) -> dict[str, Any]:
    """Get documentation for a specific configuration model.

    Returns parameter documentation for SUEWS configuration models (Pydantic models).
    Use this to understand what parameters you can configure for different parts
    of SUEWS (Site, OHM, vegetation, etc.).

    Note: This returns configuration parameter docs, not physics implementation.
    For physics algorithms, use get_physics_implementation().

    Args:
        config_name: Name of configuration model (e.g., 'Site', 'SurfaceProperties', 'OHM')

    Returns:
        Dictionary with configuration parameter documentation
    """
    try:
        from supy.data_model.doc_utils import ModelDocExtractor

        extractor = ModelDocExtractor()

        # Extract all models documentation
        # extract_all_models returns {'models': {...}, 'hierarchy': {...}, 'metadata': {...}}
        all_docs_structure = extractor.extract_all_models(include_internal=False)
        all_docs = all_docs_structure.get("models", {})

        if config_name not in all_docs:
            # Try with internal models included
            all_docs_structure = extractor.extract_all_models(include_internal=True)
            all_docs = all_docs_structure.get("models", {})
            if config_name not in all_docs:
                return {
                    "success": False,
                    "error": f"Configuration model '{config_name}' not found",
                    "available_models": list(extractor.all_models.keys()),
                }

        docs = all_docs[config_name]

        # Sanitize documentation to ensure JSON serializability
        # Convert any Pydantic special types to strings
        def make_serializable(obj):
            """Recursively convert object to JSON-serializable form."""
            if obj is None or isinstance(obj, (str, int, float, bool)):
                return obj
            elif isinstance(obj, dict):
                return {str(k): make_serializable(v) for k, v in obj.items()}
            elif isinstance(obj, (list, tuple)):
                return [make_serializable(item) for item in obj]
            else:
                # Convert any other type (including PydanticUndefinedType) to string
                return str(obj)

        sanitized_docs = make_serializable(docs)

        return {
            "success": True,
            "config_name": config_name,
            "documentation": sanitized_docs,
            "guidance": "These are configuration parameters for SUEWS. For physics algorithms, use get_physics_implementation().",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


def list_available_models() -> dict[str, Any]:
    """List all available Pydantic models in SUEWS.

    Returns:
        Dictionary with model names and brief descriptions
    """
    try:
        from supy.data_model.doc_utils import ModelDocExtractor

        extractor = ModelDocExtractor()
        extractor._discover_models()

        models = {
            name: {"class": model_class.__name__, "module": model_class.__module__}
            for name, model_class in extractor.all_models.items()
        }

        return {
            "success": True,
            "models": models,
            "count": len(models),
            "guidance": "Use get_config_docs() to get detailed configuration parameters for each model",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


def get_variable_info(variable_name: Optional[str] = None) -> dict[str, Any]:
    """Get information about SUEWS output variables.

    If variable_name provided, returns details for that variable.
    If not provided, returns list of all available output variables.

    Args:
        variable_name: Optional name of output variable (e.g., 'QH', 'QE')

    Returns:
        Dictionary with variable information
    """
    try:
        # Load variables from metadata file
        metadata_path = Path(__file__).parent.parent / "data" / "variables_metadata.json"

        if not metadata_path.exists():
            return {
                "success": False,
                "error": f"Variables metadata file not found: {metadata_path}"
            }

        with open(metadata_path, encoding="utf-8") as f:
            metadata = json.load(f)

        variables = metadata["variables"]
        concepts = metadata.get("concepts", {})

        if variable_name:
            if variable_name in variables:
                var_info = variables[variable_name].copy()
                result = {
                    "success": True,
                    "variable": variable_name,
                    "info": var_info,
                }

                # Add relevant concept information
                var_type = var_info.get("type")
                if var_type == "energy_flux" and "energy_balance" in concepts:
                    result["concept"] = concepts["energy_balance"]
                elif var_type in ["water_flux", "water_state"] and "water_balance" in concepts:
                    result["concept"] = concepts["water_balance"]

                return result
            else:
                return {
                    "success": False,
                    "error": f"Variable '{variable_name}' not found",
                    "available_variables": list(variables.keys()),
                }
        else:
            return {
                "success": True,
                "variables": variables,
                "count": len(variables),
                "concepts": concepts,
                "guidance": "These are SUEWS output variables extracted from source code. Use variable name to get detailed information.",
                "metadata": metadata.get("metadata", {}),
            }

    except Exception as e:
        return {"success": False, "error": str(e)}


def list_physics_schemes() -> dict[str, Any]:
    """List available physics schemes and modules with their source files.

    Returns physics schemes, control modules, and utility modules available
    via get_physics_implementation().

    Returns:
        Dictionary with module names, descriptions, and source file names
    """
    # Main physics schemes
    schemes = {
        "OHM": {
            "name": "Objective Hysteresis Model",
            "purpose": "Calculate storage heat flux",
            "category": "physics",
            "file": "suews_phys_ohm.f95",
            "description": "Storage heat flux using hysteresis relation with net radiation",
        },
        "water_balance": {
            "name": "Water Balance",
            "purpose": "Water distribution and drainage",
            "category": "physics",
            "file": "suews_phys_waterdist.f95",
            "description": "Surface water distribution, drainage, and runoff calculations",
        },
        "evaporation": {
            "name": "Evapotranspiration",
            "purpose": "Calculate evaporation and transpiration",
            "category": "physics",
            "file": "suews_phys_evap.f95",
            "description": "Evaporation from surfaces and transpiration from vegetation",
        },
        "LUMPS": {
            "name": "Local-scale Urban Meteorological Parameterization Scheme",
            "purpose": "Simple sensible/latent heat flux",
            "category": "physics",
            "file": "suews_phys_lumps.f95",
            "description": "Simplified turbulent flux calculations based on vegetation fraction",
        },
        "NARP": {
            "name": "Net All-wave Radiation Parameterization",
            "purpose": "Calculate radiation components",
            "category": "physics",
            "file": "suews_phys_narp.f95",
            "description": "Radiation balance including shortwave and longwave components",
        },
        "anthropogenic_heat": {
            "name": "Anthropogenic Heat",
            "purpose": "Calculate human activity heat",
            "category": "physics",
            "file": "suews_phys_anthro.f95",
            "description": "Heat flux from vehicles, buildings, and human metabolism",
        },
        "snow": {
            "name": "Snow Model",
            "purpose": "Snow accumulation and melting",
            "category": "physics",
            "file": "suews_phys_snow.f95",
            "description": "Snow pack dynamics including accumulation, melting, and albedo changes",
        },
        "SPARTACUS": {
            "name": "SPARTACUS-Surface",
            "purpose": "3D radiation interaction",
            "category": "physics",
            "file": "suews_phys_spartacus.f95",
            "description": "3D shortwave and longwave radiation with complex canopies",
        },
        "ESTM": {
            "name": "Element Surface Temperature Method",
            "purpose": "Surface temperature calculations",
            "category": "physics",
            "file": "suews_phys_estm.f95",
            "description": "Calculates surface temperatures for different urban elements",
        },
        "BEERS": {
            "name": "BEERS Radiation Scheme",
            "purpose": "Radiation through vegetation",
            "category": "physics",
            "file": "suews_phys_beers.f95",
            "description": "Beer's law radiation attenuation through vegetation canopy",
        },
        "SOLWEIG": {
            "name": "SOLWEIG Radiation Model",
            "purpose": "Mean radiant temperature",
            "category": "physics",
            "file": "suews_phys_solweig.f95",
            "description": "Calculates mean radiant temperature and 3D radiation",
        },
        "STEBBS": {
            "name": "STEBBS Building Energy",
            "purpose": "Building energy balance",
            "category": "physics",
            "file": "suews_phys_stebbs.f95",
            "description": "Detailed building energy balance model",
        },
        "resistance": {
            "name": "Aerodynamic Resistance",
            "purpose": "Surface and boundary layer resistance",
            "category": "physics",
            "file": "suews_phys_resist.f95",
            "description": "Calculates aerodynamic, boundary layer, and surface resistance",
        },
        "RSL": {
            "name": "Rough Sublayer Profile",
            "purpose": "Wind and temperature profiles",
            "category": "physics",
            "file": "suews_phys_rslprof.f95",
            "description": "Profiles of wind speed and temperature in rough sublayer",
        },
        "biogenic_CO2": {
            "name": "Biogenic CO2",
            "purpose": "CO2 exchange calculations",
            "category": "physics",
            "file": "suews_phys_biogenco2.f95",
            "description": "Biogenic CO2 fluxes from photosynthesis and respiration",
        },
        "atmospheric_stability": {
            "name": "Atmospheric Stability",
            "purpose": "Stability corrections",
            "category": "physics",
            "file": "suews_phys_atmmoiststab.f95",
            "description": "Atmospheric moisture and stability calculations",
        },
        "daily_state": {
            "name": "Daily State Updates",
            "purpose": "Daily evolution of states",
            "category": "physics",
            "file": "suews_phys_dailystate.f95",
            "description": "Updates daily-evolving state variables (LAI, albedo, etc.)",
        },
        "element_heat_capacity": {
            "name": "Element Heat Capacity",
            "purpose": "Thermal properties",
            "category": "physics",
            "file": "suews_phys_ehc.f95",
            "description": "Heat capacity calculations for urban elements",
        },
        # Control modules
        "driver": {
            "name": "SUEWS Main Driver",
            "purpose": "Main calculation orchestration",
            "category": "control",
            "file": "suews_ctrl_driver.f95",
            "description": "Main driver coordinating all physics calculations",
        },
        "constants": {
            "name": "Constants and Parameters",
            "purpose": "Physical constants",
            "category": "control",
            "file": "suews_ctrl_const.f95",
            "description": "Physical constants, conversion factors, and default parameters",
        },
        "types": {
            "name": "Type Definitions",
            "purpose": "Data structures",
            "category": "control",
            "file": "suews_ctrl_type.f95",
            "description": "Fortran type definitions for SUEWS data structures",
        },
        "output": {
            "name": "Output Formatting",
            "purpose": "Results formatting",
            "category": "control",
            "file": "suews_ctrl_output.f95",
            "description": "Output formatting and variable list management",
        },
        # Utility modules
        "meteorology": {
            "name": "Meteorological Utilities",
            "purpose": "Met calculations",
            "category": "utility",
            "file": "suews_util_meteo.f95",
            "description": "Meteorological calculations (saturation, humidity, etc.)",
        },
        "time_utilities": {
            "name": "Time Utilities",
            "purpose": "Time conversions",
            "category": "utility",
            "file": "suews_util_time.f95",
            "description": "Time and date conversion utilities",
        },
    }

    return {
        "success": True,
        "schemes": schemes,
        "count": len(schemes),
        "categories": {
            "physics": len([s for s in schemes.values() if s.get("category") == "physics"]),
            "control": len([s for s in schemes.values() if s.get("category") == "control"]),
            "utility": len([s for s in schemes.values() if s.get("category") == "utility"]),
        },
        "guidance": "Use get_physics_implementation() to see actual Fortran code for each module. Large files return structured summaries with subroutine lists.",
    }


def get_physics_implementation(scheme_name: str, extract_concepts: bool = False) -> dict[str, Any]:
    """Get Fortran source code for a physics scheme with smart size handling.

    For small schemes (<20k estimated tokens), returns full source code.
    For large schemes (NARP, snow), returns structured overview with subroutine
    navigation to work within MCP's 25k token response limit.

    Args:
        scheme_name: Name of physics scheme (e.g., 'OHM', 'water_balance')
        extract_concepts: If True, extract key concepts from source code comments and variables

    Returns:
        Dictionary with source code and metadata, or structured overview if too large
    """
    try:
        # Get scheme mapping
        schemes_info = list_physics_schemes()["schemes"]

        if scheme_name not in schemes_info:
            return {
                "success": False,
                "error": f"Scheme '{scheme_name}' not found",
                "available_schemes": list(schemes_info.keys()),
            }

        scheme = schemes_info[scheme_name]
        source_file = scheme["file"]

        # Read Fortran source file
        physics_dir = Path(__file__).parent.parent / "physics_code"
        source_path = physics_dir / source_file

        if not source_path.exists():
            return {
                "success": False,
                "error": f"Source file not found: {source_file}",
            }

        with open(source_path, encoding="utf-8") as f:
            source_code = f.read()

        lines = source_code.split("\n")
        line_count = len(lines)

        # Estimate tokens (rough: 1 line ≈ 25 tokens)
        estimated_tokens = line_count * 25

        # Extract detailed subroutine information
        subroutines = _extract_subroutine_details(source_code)

        # Check if file is too large for MCP
        if estimated_tokens > 20000:
            # Too large - return structured summary
            return {
                "success": True,
                "scheme": scheme_name,
                "description": scheme["description"],
                "source_file": source_file,
                "size_info": {
                    "total_lines": line_count,
                    "estimated_tokens": estimated_tokens,
                    "status": "too_large_for_mcp",
                    "mcp_limit": 25000,
                },
                "subroutines": subroutines,
                "github_url": f"https://github.com/UMEP-dev/SUEWS/blob/master/src/suews/src/{source_file}",
                "guidance": "This scheme is too large for MCP. Options: 1) View full code on GitHub using the URL above, 2) Review subroutine list to understand code organization, 3) Use extract_concepts=True to get physics concepts from comments",
            }
        else:
            # Small enough - return full code
            result = {
                "success": True,
                "scheme": scheme_name,
                "description": scheme["description"],
                "source_file": source_file,
                "source_code": source_code,
                "size_info": {
                    "total_lines": line_count,
                    "estimated_tokens": estimated_tokens,
                },
                "subroutines": [sub["name"] for sub in subroutines],
                "guidance": "Full Fortran source code for physics scheme",
            }

            # Extract concepts if requested
            if extract_concepts:
                result["concepts"] = _extract_fortran_concepts(source_code)

            return result

    except Exception as e:
        return {"success": False, "error": str(e)}


def _extract_subroutine_details(fortran_code: str) -> list[dict[str, Any]]:
    """Extract detailed subroutine information from Fortran code.

    Parses SUBROUTINE and FUNCTION declarations to extract names,
    line ranges, and descriptions from preceding comments.

    Args:
        fortran_code: Fortran source code text

    Returns:
        List of dictionaries with subroutine details
    """
    subroutines = []
    lines = fortran_code.split("\n")

    in_subroutine = False
    current_sub = None
    preceding_comments = []

    for i, line in enumerate(lines, 1):
        line_stripped = line.strip()

        # Collect comments that might describe the next subroutine
        if line_stripped.startswith("!") and not in_subroutine:
            comment = line_stripped.lstrip("!").strip()
            if comment and not all(c in "=-*" for c in comment):
                preceding_comments.append(comment)
            continue

        # Match SUBROUTINE declaration
        sub_match = re.match(r"^\s*SUBROUTINE\s+(\w+)", line, re.IGNORECASE)
        if sub_match:
            current_sub = {
                "name": sub_match.group(1),
                "start_line": i,
                "type": "subroutine",
                "description": " ".join(preceding_comments[-3:]) if preceding_comments else "",
            }
            in_subroutine = True
            preceding_comments = []
            continue

        # Match FUNCTION declaration
        func_match = re.match(r"^\s*FUNCTION\s+(\w+)", line, re.IGNORECASE)
        if func_match:
            current_sub = {
                "name": func_match.group(1),
                "start_line": i,
                "type": "function",
                "description": " ".join(preceding_comments[-3:]) if preceding_comments else "",
            }
            in_subroutine = True
            preceding_comments = []
            continue

        # Match END SUBROUTINE/FUNCTION
        if in_subroutine and re.match(r"^\s*END\s+(SUBROUTINE|FUNCTION)", line, re.IGNORECASE):
            current_sub["end_line"] = i
            subroutines.append(current_sub)
            in_subroutine = False
            current_sub = None
            continue

        # Clear preceding comments if we hit non-comment, non-subroutine code
        if not line_stripped.startswith("!") and not in_subroutine:
            preceding_comments = []

    return subroutines


def _extract_fortran_concepts(source_code: str) -> dict[str, Any]:
    """Extract key concepts from Fortran source code.

    Parses variable declarations, comments, and equations to extract
    physics concepts and their explanations.

    Args:
        source_code: Fortran source code text

    Returns:
        Dictionary with extracted concepts
    """
    concepts = {
        "variables": {},
        "equations": [],
        "comments": [],
    }

    lines = source_code.split("\n")

    for i, line in enumerate(lines):
        line_stripped = line.strip()

        # Extract variable declarations with inline comments
        # Pattern: REAL(...), INTENT(in/out) :: VarName !Comment
        var_match = re.match(
            r"REAL.*INTENT\((in|out|inout)\)\s*::\s*(\w+)\s*(!.*)?",
            line_stripped,
            re.IGNORECASE
        )
        if var_match:
            intent = var_match.group(1)
            var_name = var_match.group(2)
            comment = var_match.group(3)

            if comment:
                # Clean up comment
                comment = comment.lstrip("!").strip()

                # Extract units if present [unit]
                unit_match = re.search(r"\[([^\]]+)\]", comment)
                units = unit_match.group(1) if unit_match else None

                # Remove units from description
                description = re.sub(r"\[([^\]]+)\]", "", comment).strip()

                concepts["variables"][var_name] = {
                    "description": description,
                    "units": units,
                    "intent": intent,
                    "line": i + 1,
                }

        # Extract standalone comments (explanatory text)
        elif line_stripped.startswith("!") and not line_stripped.startswith("!!"):
            comment = line_stripped.lstrip("!").strip()
            # Skip empty comments, print statements, and dividers
            if comment and "PRINT" not in comment and not all(c in "=-*" for c in comment):
                concepts["comments"].append({
                    "text": comment,
                    "line": i + 1,
                })

    return concepts


def get_forcing_format_guide(
    source_format: Optional[str] = None,
    variables_available: Optional[list[str]] = None,
) -> dict[str, Any]:
    """Get guidance for converting meteorological data to SUEWS forcing format.

    Provides comprehensive information for users to convert their own
    weather station data or model output to SUEWS-compatible forcing files.

    Args:
        source_format: Optional hint about source data format (e.g., 'csv', 'netcdf', 'weather_station')
        variables_available: Optional list of available variable names in source data

    Returns:
        Dictionary with format specifications, conversion templates, and guidance
    """
    # Define required SUEWS forcing variables
    required_variables = {
        "iy": {
            "name": "Year",
            "unit": "year",
            "description": "Year (4 digits)",
            "required": True,
            "typical_range": "1900-2100",
        },
        "id": {
            "name": "Day of year",
            "unit": "day",
            "description": "Day of year (1-366)",
            "required": True,
            "typical_range": "1-366",
        },
        "it": {
            "name": "Hour",
            "unit": "hour",
            "description": "Hour (0-23)",
            "required": True,
            "typical_range": "0-23",
        },
        "imin": {
            "name": "Minute",
            "unit": "minute",
            "description": "Minute (0-59)",
            "required": True,
            "typical_range": "0-59",
        },
        "kdown": {
            "name": "Shortwave radiation",
            "unit": "W/m2",
            "description": "Incoming shortwave radiation",
            "required": True,
            "typical_range": "0-1400",
            "validation": "Must be >= 0, typically 0 at night, max ~1400 at noon",
        },
        "ldown": {
            "name": "Longwave radiation",
            "unit": "W/m2",
            "description": "Incoming longwave radiation",
            "required": True,
            "typical_range": "100-500",
            "validation": "Must be >= 0, typically 200-450 depending on cloud cover",
        },
        "Tair": {
            "name": "Air temperature",
            "unit": "degC",
            "description": "Air temperature at measurement height",
            "required": True,
            "typical_range": "-90 to 90",
            "validation": "Must be between -90 and 90 degC",
        },
        "RH": {
            "name": "Relative humidity",
            "unit": "%",
            "description": "Relative humidity (0-100)",
            "required": True,
            "typical_range": "0-100",
            "validation": "Must be 0-105 (slight overshoot allowed for measurement error)",
        },
        "pres": {
            "name": "Air pressure",
            "unit": "kPa",
            "description": "Atmospheric pressure",
            "required": True,
            "typical_range": "80-110",
            "validation": "Typical range 90-105 kPa at sea level",
        },
        "rain": {
            "name": "Rainfall",
            "unit": "mm",
            "description": "Rainfall amount per timestep",
            "required": True,
            "typical_range": "0-100",
            "validation": "Must be >= 0, cumulative over timestep",
        },
        "wdir": {
            "name": "Wind direction",
            "unit": "degrees",
            "description": "Wind direction (0-360, 0=North, 90=East)",
            "required": False,
            "typical_range": "0-360",
            "validation": "0-360 degrees, can use -999 for calm winds",
        },
        "wspeed": {
            "name": "Wind speed",
            "unit": "m/s",
            "description": "Wind speed at measurement height",
            "required": True,
            "typical_range": "0-50",
            "validation": "Must be >= 0.001 to avoid division by zero in calculations",
            "special": "Never use exactly 0! Minimum value should be 0.001 m/s",
        },
    }

    # Format specifications
    format_spec = {
        "file_type": "Space-separated text file (.txt)",
        "header": "No header row (data only)",
        "delimiter": "Single space character",
        "column_order": ["iy", "id", "it", "imin", "kdown", "ldown", "Tair", "RH", "pres", "rain", "wdir", "wspeed"],
        "decimal_precision": "Recommended: 1-3 decimal places",
        "missing_values": "Use -999 for missing values (except wind speed which must be > 0)",
        "temporal_requirements": {
            "continuity": "Must be continuous time series (no gaps)",
            "resolution": "Typically hourly or sub-hourly",
            "ordering": "Must be chronologically ordered (ascending)",
            "duplicates": "No duplicate timestamps allowed",
        },
    }

    # Python conversion template
    python_template = '''"""
Convert meteorological data to SUEWS forcing format.

This template shows how to convert a pandas DataFrame with weather data
to SUEWS forcing format. Adapt the variable names and units to match your data.
"""

import pandas as pd

# Load your data (adapt to your format)
# Example: CSV with datetime index
df = pd.read_csv("your_weather_data.csv", parse_dates=['datetime'])
df = df.set_index('datetime')

# Create SUEWS forcing DataFrame
forcing = pd.DataFrame()

# Time columns (extract from datetime index)
forcing['iy'] = df.index.year
forcing['id'] = df.index.dayofyear
forcing['it'] = df.index.hour
forcing['imin'] = df.index.minute

# Radiation variables
# Adapt variable names to match your data columns
forcing['kdown'] = df['shortwave_radiation_W_m2']  # W/m2
forcing['ldown'] = df['longwave_radiation_W_m2']   # W/m2

# Temperature and humidity
forcing['Tair'] = df['temperature_degC']           # degC
forcing['RH'] = df['relative_humidity_percent']    # %

# Pressure - convert if needed
# Example: If pressure is in Pa, convert to kPa
forcing['pres'] = df['pressure_Pa'] / 1000         # Pa -> kPa

# Rainfall - ensure cumulative per timestep
forcing['rain'] = df['rainfall_mm']                # mm per timestep

# Wind - calculate if only components available
if 'u_wind' in df.columns and 'v_wind' in df.columns:
    # Calculate wind speed and direction from components
    forcing['wspeed'] = (df['u_wind']**2 + df['v_wind']**2)**0.5
    forcing['wdir'] = (270 - np.arctan2(df['v_wind'], df['u_wind']) * 180/np.pi) % 360
else:
    forcing['wspeed'] = df['wind_speed_m_s']      # m/s
    forcing['wdir'] = df['wind_direction_deg']    # degrees

# CRITICAL: Ensure wind speed > 0 (SUEWS requirement)
forcing['wspeed'] = forcing['wspeed'].clip(lower=0.001)

# Validate ranges
forcing['kdown'] = forcing['kdown'].clip(lower=0, upper=1400)
forcing['ldown'] = forcing['ldown'].clip(lower=0, upper=600)
forcing['Tair'] = forcing['Tair'].clip(lower=-90, upper=90)
forcing['RH'] = forcing['RH'].clip(lower=0, upper=105)
forcing['rain'] = forcing['rain'].clip(lower=0)

# Handle missing values
forcing = forcing.fillna(-999)

# Save to SUEWS format (space-separated, no header, 3 decimals)
forcing.to_csv(
    'suews_forcing.txt',
    sep=' ',
    header=False,
    index=False,
    float_format='%.3f'
)

print(f"Forcing file created: {len(forcing)} timesteps")
print(f"Period: {df.index[0]} to {df.index[-1]}")
'''

    # Common unit conversions
    unit_conversions = {
        "temperature": {
            "Kelvin_to_Celsius": "T_degC = T_K - 273.15",
            "Fahrenheit_to_Celsius": "T_degC = (T_degF - 32) * 5/9",
        },
        "pressure": {
            "Pa_to_kPa": "P_kPa = P_Pa / 1000",
            "hPa_to_kPa": "P_kPa = P_hPa / 10",
            "mb_to_kPa": "P_kPa = P_mb / 10",
            "atm_to_kPa": "P_kPa = P_atm * 101.325",
        },
        "radiation": {
            "J_m2_to_W_m2": "R_W_m2 = R_J_m2 / timestep_seconds",
            "note": "ERA5 provides cumulative J/m2, divide by timestep to get W/m2",
        },
        "rainfall": {
            "m_to_mm": "rain_mm = rain_m * 1000",
            "inches_to_mm": "rain_mm = rain_inches * 25.4",
            "rate_to_cumulative": "rain_mm_cumulative = rain_rate_mm_hr * (timestep_seconds / 3600)",
        },
        "wind": {
            "u_v_to_speed": "wspeed = sqrt(u^2 + v^2)",
            "u_v_to_direction": "wdir = (270 - arctan2(v, u) * 180/pi) % 360",
            "note": "u=eastward, v=northward components; wdir=0 is North, 90 is East",
        },
        "humidity": {
            "specific_to_relative": "RH = (q / q_sat(T, P)) * 100",
            "dewpoint_to_relative": "RH = 100 * exp((17.625*Td)/(243.04+Td)) / exp((17.625*T)/(243.04+T))",
            "note": "q=specific humidity, Td=dewpoint temperature",
        },
    }

    # Validation script template
    validation_script = '''"""
Validate SUEWS forcing file before running simulation.

Run this script on your forcing file to check for common issues.
"""

import pandas as pd
import numpy as np

def validate_forcing(forcing_file):
    """Validate SUEWS forcing file."""

    # Load forcing file
    columns = ['iy', 'id', 'it', 'imin', 'kdown', 'ldown', 'Tair', 'RH', 'pres', 'rain', 'wdir', 'wspeed']
    df = pd.read_csv(forcing_file, sep=r'\\s+', header=None, names=columns)

    # Create datetime index
    df['datetime'] = pd.to_datetime(
        df['iy'].astype(str) + '-' + df['id'].astype(str),
        format='%Y-%j'
    ) + pd.to_timedelta(df['it'], unit='h') + pd.to_timedelta(df['imin'], unit='m')

    issues = []

    # Check for duplicates
    if df['datetime'].duplicated().any():
        issues.append(f"ERROR: {df['datetime'].duplicated().sum()} duplicate timestamps found")

    # Check temporal continuity
    time_diff = df['datetime'].diff()
    if time_diff.nunique() > 2:  # More than one unique interval (plus NaT)
        issues.append("WARNING: Non-uniform timestep detected")

    # Check ranges
    checks = {
        'kdown': (0, 1400, "Shortwave radiation"),
        'ldown': (0, 600, "Longwave radiation"),
        'Tair': (-90, 90, "Temperature"),
        'RH': (0, 105, "Relative humidity"),
        'pres': (50, 110, "Pressure"),
        'rain': (0, 500, "Rainfall"),
        'wspeed': (0.001, 100, "Wind speed"),
    }

    for var, (min_val, max_val, name) in checks.items():
        out_of_range = ((df[var] < min_val) | (df[var] > max_val)) & (df[var] != -999)
        if out_of_range.any():
            issues.append(f"WARNING: {name} ({var}) has {out_of_range.sum()} values outside [{min_val}, {max_val}]")

    # Check wind speed (must never be exactly 0)
    if (df['wspeed'] == 0).any():
        issues.append(f"ERROR: Wind speed has {(df['wspeed'] == 0).sum()} zero values (must be >= 0.001)")

    # Check for missing values
    for var in columns[4:]:  # Skip time columns
        missing = (df[var] == -999).sum()
        if missing > 0:
            issues.append(f"INFO: {var} has {missing} missing values (-999)")

    # Summary
    print(f"Validation results for: {forcing_file}")
    print(f"Time period: {df['datetime'].min()} to {df['datetime'].max()}")
    print(f"Timesteps: {len(df)}")
    print(f"Timestep: {time_diff.mode().values[0] if len(time_diff.mode()) > 0 else 'Variable'}")
    print()

    if issues:
        print("Issues found:")
        for issue in issues:
            print(f"  {issue}")
        return False
    else:
        print("All checks passed!")
        return True

# Run validation
if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python validate_forcing.py <forcing_file.txt>")
    else:
        validate_forcing(sys.argv[1])
'''

    # Build response
    result = {
        "success": True,
        "format_specification": format_spec,
        "required_variables": required_variables,
        "unit_conversions": unit_conversions,
        "conversion_template": {
            "language": "Python",
            "code": python_template,
            "notes": "Adapt variable names and unit conversions to match your source data",
        },
        "validation_script": {
            "language": "Python",
            "code": validation_script,
            "usage": "python validate_forcing.py your_forcing_file.txt",
        },
        "common_sources": {
            "weather_stations": {
                "description": "Local meteorological station data",
                "advantages": "High accuracy, site-specific",
                "challenges": "May have gaps, limited variables",
                "typical_variables": "Temperature, humidity, wind, rain (often missing radiation)",
            },
            "reanalysis": {
                "description": "ERA5, MERRA-2, JRA-55 global datasets",
                "advantages": "Global coverage, complete variables, no gaps",
                "challenges": "Coarse resolution (~25km), may not capture local effects",
                "tool_available": "Use get_era5_forcing() tool for automated ERA5 retrieval",
            },
            "mesoscale_models": {
                "description": "WRF, WRF-Chem output",
                "advantages": "Higher resolution than reanalysis, physics-based",
                "challenges": "Requires model setup and validation",
            },
        },
        "guidance": {
            "priority_checks": [
                "Wind speed must be > 0 (use minimum 0.001 m/s)",
                "Ensure continuous time series with no gaps",
                "Validate that units match SUEWS requirements",
                "Check physical ranges (e.g., RH 0-100%, kdown >= 0)",
            ],
            "alternative_tools": {
                "era5_retrieval": "Use get_era5_forcing() to automatically retrieve ERA5 data for any location",
                "validation": "Use inspect_forcing_data() to validate forcing file before simulation",
            },
        },
    }

    # Add context-specific guidance if source format or variables provided
    if source_format or variables_available:
        result["context"] = {}
        if source_format:
            result["context"]["source_format"] = source_format
        if variables_available:
            result["context"]["available_variables"] = variables_available

            # Check which SUEWS variables are missing
            missing = []
            for var, spec in required_variables.items():
                if spec.get("required") and var not in variables_available:
                    missing.append(var)

            if missing:
                result["context"]["missing_variables"] = missing
                result["context"]["missing_guidance"] = "Missing variables may need to be estimated or obtained from alternative sources"

    return result
