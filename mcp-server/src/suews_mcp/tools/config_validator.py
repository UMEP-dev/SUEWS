"""Configuration validation tool for SUEWS MCP."""

import json
from pathlib import Path
from typing import Any, Dict

from ..utils import SUEWSBridge


async def validate_config(config_path: str, strict: bool = True) -> str:
    """
    Validate a SUEWS configuration file.

    Args:
        config_path: Path to YAML configuration file
        strict: Whether to enforce all validation rules

    Returns:
        Formatted validation results with errors and suggestions
    """
    bridge = SUEWSBridge()

    try:
        # Load configuration
        config_dict = bridge.load_config_from_yaml(config_path)

        # Validate using bridge
        result = bridge.validate_config(config_dict, use_conditional=not strict)

        if result["valid"]:
            return format_success_message(config_path)
        else:
            return format_error_message(result["errors"], config_path)

    except FileNotFoundError:
        return f"❌ Configuration file not found: {config_path}"
    except Exception as e:
        return f"❌ Error validating configuration: {str(e)}"


def format_success_message(config_path: str) -> str:
    """Format a success message."""
    return f"""✅ **Configuration Valid**

File: `{config_path}`

The SUEWS configuration is scientifically sound and ready to use. All parameters are within acceptable ranges and physics methods are compatible.

**Next Steps:**
- Run your SUEWS simulation with confidence
- Consider using `diagnose_energy_balance` after simulation to check results
- Use `generate_insights_report` for comprehensive analysis"""


def format_error_message(errors: list[Dict[str, Any]], config_path: str) -> str:
    """Format error messages with suggestions."""
    message = f"""❌ **Configuration Validation Failed**

File: `{config_path}`

Found {len(errors)} validation error(s):

"""

    for i, error in enumerate(errors, 1):
        message += f"**Error {i}:**\n"
        message += f"- Location: `{error['location']}`\n"
        message += f"- Issue: {error['message']}\n"
        message += f"- Type: {error['type']}\n"

        # Add suggestions based on error type
        suggestion = get_error_suggestion(error)
        if suggestion:
            message += f"- **Suggestion**: {suggestion}\n"

        message += "\n"

    message += """**How to Fix:**
1. Review each error location in your configuration
2. Check parameter values against physical constraints
3. Ensure all required parameters are present
4. Use `suggest_parameters` for guidance on appropriate values
5. Run validation again after making changes"""

    return message


def get_error_suggestion(error: Dict[str, Any]) -> str:
    """Get specific suggestions for common errors."""
    error_type = error["type"]
    location = error["location"]

    suggestions = {
        "missing": "Add this required parameter to your configuration",
        "type_error": "Check the data type - should it be a number, string, or list?",
        "value_error": "Value is outside acceptable range - check physical constraints",
        "assertion_error": "Parameter violates a physical constraint - review the validation rules",
    }

    # Specific parameter suggestions
    if "albedo" in location and "value_error" in error_type:
        return "Albedo must be between 0 and 1 (0 = perfect absorber, 1 = perfect reflector)"

    if "fraction" in location and "assertion_error" in error_type:
        return "Surface fractions must sum to 1.0 - check all surface cover values"

    if "height" in location and "value_error" in error_type:
        return "Building heights typically range from 5-50m for urban areas"

    if "population_density" in location:
        return "Population density in people/hectare - typical urban values: 50-500"

    return suggestions.get(error_type, "Review the parameter documentation for valid values")
