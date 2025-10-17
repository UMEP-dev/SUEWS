"""Knowledge tools - SUEWS domain knowledge access."""

import json
import re
from pathlib import Path
from typing import Any, Optional


def get_config_schema() -> dict[str, Any]:
    """Get SUEWS configuration JSON Schema.

    Returns full JSON Schema for SUEWSConfig, generated from Pydantic models.
    This is the authoritative schema definition for YAML configurations.

    Returns:
        Dictionary with success status and schema
    """
    try:
        from supy.data_model.core.config import SUEWSConfig

        schema = SUEWSConfig.model_json_schema()

        return {
            "success": True,
            "schema": schema,
            "guidance": "Use this schema to validate and build SUEWS configurations",
            "schema_url": "https://umep-dev.github.io/SUEWS/schema/suews-config/latest",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


def get_model_docs(model_name: str) -> dict[str, Any]:
    """Get documentation for a specific Pydantic model.

    Uses ModelDocExtractor to get parameter meanings, types, and constraints
    directly from the data model definitions.

    Args:
        model_name: Name of model to document (e.g., 'Site', 'SurfaceProperties', 'OHM')

    Returns:
        Dictionary with model documentation
    """
    try:
        from supy.data_model.doc_utils import ModelDocExtractor

        extractor = ModelDocExtractor()

        # Extract all models documentation
        # extract_all_models returns {'models': {...}, 'hierarchy': {...}, 'metadata': {...}}
        all_docs_structure = extractor.extract_all_models(include_internal=False)
        all_docs = all_docs_structure.get("models", {})

        if model_name not in all_docs:
            # Try with internal models included
            all_docs_structure = extractor.extract_all_models(include_internal=True)
            all_docs = all_docs_structure.get("models", {})
            if model_name not in all_docs:
                return {
                    "success": False,
                    "error": f"Model '{model_name}' not found",
                    "available_models": list(extractor.all_models.keys()),
                }

        docs = all_docs[model_name]

        return {
            "success": True,
            "model_name": model_name,
            "documentation": docs,
            "guidance": "These are parameter definitions from SUEWS data models",
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
            "guidance": "Use get_model_docs() to get detailed documentation for each model",
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
    """List available physics schemes with their source files.

    Returns:
        Dictionary with scheme names, descriptions, and source file names
    """
    schemes = {
        "OHM": {
            "name": "Objective Hysteresis Model",
            "purpose": "Calculate storage heat flux",
            "file": "suews_phys_ohm.f95",
            "description": "Storage heat flux using hysteresis relation with net radiation",
        },
        "water_balance": {
            "name": "Water Balance",
            "purpose": "Water distribution and drainage",
            "file": "suews_phys_waterdist.f95",
            "description": "Surface water distribution, drainage, and runoff calculations",
        },
        "evaporation": {
            "name": "Evapotranspiration",
            "purpose": "Calculate evaporation and transpiration",
            "file": "suews_phys_evap.f95",
            "description": "Evaporation from surfaces and transpiration from vegetation",
        },
        "LUMPS": {
            "name": "Local-scale Urban Meteorological Parameterization Scheme",
            "purpose": "Simple sensible/latent heat flux",
            "file": "suews_phys_lumps.f95",
            "description": "Simplified turbulent flux calculations based on vegetation fraction",
        },
        "NARP": {
            "name": "Net All-wave Radiation Parameterization",
            "purpose": "Calculate radiation components",
            "file": "suews_phys_narp.f95",
            "description": "Radiation balance including shortwave and longwave components",
        },
        "anthropogenic_heat": {
            "name": "Anthropogenic Heat",
            "purpose": "Calculate human activity heat",
            "file": "suews_phys_anthro.f95",
            "description": "Heat flux from vehicles, buildings, and human metabolism",
        },
        "snow": {
            "name": "Snow Model",
            "purpose": "Snow accumulation and melting",
            "file": "suews_phys_snow.f95",
            "description": "Snow pack dynamics including accumulation, melting, and albedo changes",
        },
        "SPARTACUS": {
            "name": "SPARTACUS-Surface",
            "purpose": "3D radiation interaction",
            "file": "suews_phys_spartacus.f95",
            "description": "3D shortwave and longwave radiation with complex canopies",
        },
    }

    return {
        "success": True,
        "schemes": schemes,
        "count": len(schemes),
        "guidance": "Use get_physics_implementation() to see actual Fortran code for each scheme",
    }


def get_physics_implementation(scheme_name: str, extract_concepts: bool = False) -> dict[str, Any]:
    """Get Fortran source code for a physics scheme.

    Args:
        scheme_name: Name of physics scheme (e.g., 'OHM', 'water_balance')
        extract_concepts: If True, extract key concepts from source code comments and variables

    Returns:
        Dictionary with source code and metadata
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

        # Extract subroutine/function names
        subroutines = re.findall(
            r"(?:SUBROUTINE|FUNCTION)\s+(\w+)", source_code, re.IGNORECASE
        )

        result = {
            "success": True,
            "scheme": scheme_name,
            "description": scheme["description"],
            "source_file": source_file,
            "source_code": source_code,
            "subroutines": subroutines,
            "line_count": source_code.count("\n") + 1,
            "guidance": "This is the actual Fortran implementation from SUEWS source code",
        }

        # Extract concepts if requested
        if extract_concepts:
            result["concepts"] = _extract_fortran_concepts(source_code)

        return result

    except Exception as e:
        return {"success": False, "error": str(e)}


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
