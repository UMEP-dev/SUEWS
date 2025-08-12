"""
SUEWS Parameter Reference Generator

This script generates a comprehensive parameter reference page from the Pydantic data models.
It creates a searchable index of all parameters with their descriptions, units, types, and constraints.

To run this script, navigate to the `docs/` directory and execute:
    python generate_parameter_reference.py
"""

import inspect
import importlib
from pathlib import Path
import sys
from typing import Any, Dict, List, Optional, Type, Union, get_args, get_origin, Literal
import argparse
import re
from enum import Enum

from pydantic import BaseModel
from pydantic.fields import FieldInfo

# Add the project root to sys.path to allow importing supy
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SRC_PATH = PROJECT_ROOT / "src"
sys.path.insert(0, str(SRC_PATH))

# Import data models - work directly with source files
data_model_path = SRC_PATH / "supy" / "data_model"
if not data_model_path.exists():
    print(f"Error: data_model directory not found at {data_model_path}")
    sys.exit(1)

# Add data_model parent to path so we can import
sys.path.insert(0, str(SRC_PATH / "supy"))


# Create a mock supy module structure
class MockDataModel:
    __file__ = str(data_model_path / "__init__.py")


supy = type("supy", (), {})()
supy.data_model = MockDataModel()


def format_unit(unit: str) -> str:
    """Convert plain text units to RST substitution format."""
    if not unit:
        return unit

    # Handle special characters
    unit = unit.replace("³", "^3").replace("²", "^2").replace("⁻", "^-")

    # Handle division
    if "/" in unit:
        parts = unit.split("/")
        if len(parts) == 2:
            numerator = parts[0].strip()
            denominator = parts[1].strip()

            # Convert denominator to negative exponent
            if "^" in denominator:
                base, exp = denominator.split("^", 1)
                denominator = f"{base}^-{exp}"
            else:
                denominator = f"{denominator}^-1"

            return f"{process_unit_part(numerator)} {process_unit_part(denominator)}"

    return process_unit_part(unit)


def process_unit_part(part: str) -> str:
    """Process a single unit part."""
    known_patterns = [
        "km^-1",
        "mm^-1",
        "m^-1",
        "m^-2",
        "m^-3",
        "m^2",
        "m^3",
        "s^-1",
        "kg^-1",
        "K^-1",
        "J^-1",
        "W^-1",
        "h^-1",
        "day^-1",
    ]

    if part in known_patterns:
        return f"|{part}|"

    # Handle compound units
    words = part.split()
    formatted_words = []
    for word in words:
        if word in known_patterns or (
            "^" in word
            and any(
                word.startswith(base) for base in ["m", "s", "kg", "K", "W", "h", "d"]
            )
        ):
            formatted_words.append(f"|{word}|")
        else:
            formatted_words.append(word)

    return " ".join(formatted_words)


def get_user_friendly_type_name(type_hint: Any) -> str:
    """Generate user-friendly type name."""
    origin = get_origin(type_hint)
    args = get_args(type_hint)

    if origin is Union:
        if len(args) == 2 and type(None) in args:
            non_none_arg = next(arg for arg in args if arg is not type(None))
            return f"{get_user_friendly_type_name(non_none_arg)} (Optional)"
        return " | ".join(get_user_friendly_type_name(arg) for arg in args)

    if origin is list or origin is List:
        return f"List of {get_user_friendly_type_name(args[0])}" if args else "List"

    if origin is dict or origin is Dict:
        if args and len(args) == 2:
            return f"Mapping"
        return "Mapping"

    if hasattr(type_hint, "__name__"):
        if type_hint.__name__ == "RefValue" and args:
            return f"Value with optional reference"
        return type_hint.__name__

    return str(type_hint)


def extract_enum_options(enum_class: Type[Enum]) -> List[tuple]:
    """Extract options from an Enum class."""
    options = []
    if enum_class and hasattr(enum_class, "__members__"):
        for name, member in enum_class.__members__.items():
            # Skip internal options
            if hasattr(member, "_internal") and member._internal:
                continue

            # Get description from docstring
            desc = ""
            if enum_class.__doc__:
                # Try to extract description from docstring
                lines = enum_class.__doc__.split("\n")
                for line in lines:
                    # Look for pattern like "0: NO_EMISSIONS - Description"
                    pattern = f"{member.value}:\\s*{name}\\s*-\\s*(.+)"
                    match = re.search(pattern, line)
                    if match:
                        desc = match.group(1).strip()
                        break

            options.append((member.value, name, desc))

    return options


def get_all_models_in_module(module) -> Dict[str, Type[BaseModel]]:
    """Get all Pydantic models in a module."""
    models = {}
    for name, obj in inspect.getmembers(module):
        if (
            inspect.isclass(obj)
            and issubclass(obj, BaseModel)
            and obj.__module__ == module.__name__
        ):
            models[name] = obj
    return models


def extract_parameters_from_model(
    model_class: Type[BaseModel], model_path: str = "", processed_models: set = None
) -> List[Dict[str, Any]]:
    """
    Extract all parameters from a Pydantic model recursively.

    Returns a list of parameter dictionaries with full paths.
    """
    if processed_models is None:
        processed_models = set()

    # Avoid infinite recursion
    if model_class in processed_models:
        return []
    processed_models.add(model_class)

    parameters = []

    for field_name, field_info in model_class.model_fields.items():
        # Skip internal fields
        if isinstance(field_info.json_schema_extra, dict):
            if field_info.json_schema_extra.get("internal_only", False):
                continue

        # Build parameter path
        if model_path:
            param_path = f"{model_path}.{field_name}"
        else:
            param_path = field_name

        # Get field type
        field_type_hint = field_info.annotation
        user_type = get_user_friendly_type_name(field_type_hint)

        # Get description
        description = getattr(field_info, "description", "")

        # Get unit
        unit = None
        if isinstance(field_info.json_schema_extra, dict):
            unit = field_info.json_schema_extra.get("unit")

        # Get default value
        default_value = None
        if (
            field_info.default is not None
            and field_info.default != inspect.Parameter.empty
        ):
            default_value = field_info.default
        elif field_info.default_factory is not None:
            try:
                default_value = field_info.default_factory()
            except:
                default_value = "Dynamically generated"

        # Check for enum options
        enum_options = []
        origin = get_origin(field_type_hint) or field_type_hint
        args = get_args(field_type_hint)

        # Check if it's an enum
        if hasattr(field_type_hint, "__bases__") and Enum in field_type_hint.__bases__:
            enum_options = extract_enum_options(field_type_hint)
        # Check for RefValue[Enum]
        elif hasattr(origin, "__name__") and origin.__name__ in [
            "RefValue",
            "FlexibleRefValue",
        ]:
            if args and hasattr(args[0], "__bases__") and Enum in args[0].__bases__:
                enum_options = extract_enum_options(args[0])

        # Create parameter entry
        param_entry = {
            "path": param_path,
            "name": field_name,
            "type": user_type,
            "description": description,
            "unit": unit,
            "default": default_value,
            "model": model_class.__name__,
            "options": enum_options,
        }

        parameters.append(param_entry)

        # Recursively process nested models
        possible_types = [field_type_hint] + list(get_args(field_type_hint))
        for pt in possible_types:
            origin_pt = get_origin(pt) or pt
            if (
                hasattr(origin_pt, "__name__")
                and inspect.isclass(origin_pt)
                and issubclass(origin_pt, BaseModel)
                and origin_pt != model_class
            ):
                nested_params = extract_parameters_from_model(
                    origin_pt, param_path, processed_models
                )
                parameters.extend(nested_params)

    return parameters


def generate_parameter_reference(output_file: Path):
    """Generate comprehensive parameter reference RST file."""

    # Collect all data models
    all_models = {}
    data_model_module_root = Path(supy.data_model.__file__).parent

    for py_file in data_model_module_root.glob("*.py"):
        if py_file.name in [
            "__init__.py",
            "validation_controller.py",
            "validation_feedback.py",
            "validation_utils.py",
            "yaml_annotator.py",
            "yaml_annotator_json.py",
            "timezone_enum.py",
            "precheck.py",
        ]:
            continue  # Skip non-model files

        module_name = f"data_model.{py_file.stem}"

        try:
            module = importlib.import_module(module_name)
            all_models.update(get_all_models_in_module(module))
        except ImportError as e:
            print(f"Warning: Could not import {module_name}: {e}")
            continue

    # Extract parameters from top-level models
    all_parameters = []
    processed_models = set()

    # Start with main configuration models
    top_models = ["Model", "Site"]
    for model_name in top_models:
        if model_name in all_models:
            params = extract_parameters_from_model(
                all_models[model_name], model_name.lower(), processed_models
            )
            all_parameters.extend(params)

    # Sort parameters alphabetically by path
    all_parameters.sort(key=lambda x: x["path"].lower())

    # Generate RST content
    rst_content = []

    # Title and introduction
    rst_content.append(".. _parameter_reference:")
    rst_content.append("")
    rst_content.append("Parameter Reference")
    rst_content.append("===================")
    rst_content.append("")
    rst_content.append(
        "This page provides a comprehensive, searchable reference of all SUEWS parameters "
    )
    rst_content.append(
        "available in the YAML configuration format. Parameters are organized alphabetically "
    )
    rst_content.append("by their full configuration path.")
    rst_content.append("")
    rst_content.append(".. contents:: Quick Navigation")
    rst_content.append("   :local:")
    rst_content.append("   :depth: 2")
    rst_content.append("")

    # Group parameters by first level (model/site)
    grouped_params = {}
    for param in all_parameters:
        first_level = param["path"].split(".")[0]
        if first_level not in grouped_params:
            grouped_params[first_level] = []
        grouped_params[first_level].append(param)

    # Generate sections for each group
    for group_name in sorted(grouped_params.keys()):
        rst_content.append(f"{group_name.title()} Parameters")
        rst_content.append("-" * (len(group_name) + 11))
        rst_content.append("")

        # Create parameter list
        for param in grouped_params[group_name]:
            # Parameter heading with anchor
            param_id = param["path"].replace(".", "_")
            rst_content.append(f".. _param_{param_id}:")
            rst_content.append("")
            rst_content.append(f"**{param['path']}**")
            rst_content.append("~" * (len(param["path"]) + 4))
            rst_content.append("")

            # Description
            if param["description"]:
                # Parse and clean description
                desc = param["description"]
                # Remove "Options:" section if present (will be handled separately)
                if "Options:" in desc:
                    desc = desc.split("Options:")[0].strip()
                rst_content.append(desc)
                rst_content.append("")

            # Parameter details in definition list format
            rst_content.append(":Type:")
            rst_content.append(f"   {param['type']}")
            rst_content.append("")

            if param["unit"]:
                formatted_unit = format_unit(param["unit"])
                rst_content.append(":Unit:")
                rst_content.append(f"   {formatted_unit}")
                rst_content.append("")

            if param["default"] is not None:
                rst_content.append(":Default:")
                rst_content.append(f"   ``{param['default']}``")
                rst_content.append("")

            # Options for enum parameters
            if param["options"]:
                rst_content.append(":Options:")
                for value, name, desc in param["options"]:
                    if desc:
                        rst_content.append(f"   * ``{value}`` ({name}) - {desc}")
                    else:
                        rst_content.append(f"   * ``{value}`` ({name})")
                rst_content.append("")

            # Location in configuration
            rst_content.append(":Configuration Path:")
            rst_content.append(f"   ``{param['path']}``")
            rst_content.append("")

            # Source model
            rst_content.append(":Defined in:")
            model_lower = param["model"].lower()
            rst_content.append(f"   :doc:`yaml/schema/{model_lower}`")
            rst_content.append("")

            rst_content.append("")  # Extra blank line between parameters

    # Add search tips
    rst_content.append("")
    rst_content.append("Search Tips")
    rst_content.append("-----------")
    rst_content.append("")
    rst_content.append(
        "* Use your browser's search function (Ctrl+F / Cmd+F) to find specific parameters"
    )
    rst_content.append(
        "* Search by parameter name (e.g., 'tstep') or full path (e.g., 'model.control.tstep')"
    )
    rst_content.append(
        "* Search by unit (e.g., 'm^2') to find all parameters with that unit"
    )
    rst_content.append(
        "* Search by type (e.g., 'float') to find all parameters of that type"
    )
    rst_content.append("")

    # Write to file
    with open(output_file, "w") as f:
        f.write("\n".join(rst_content))

    print(f"Generated parameter reference: {output_file}")
    print(f"Total parameters documented: {len(all_parameters)}")

    # Generate parameter index for quick lookup
    index_file = output_file.parent / "parameter_index.rst"
    generate_parameter_index(all_parameters, index_file)


def generate_parameter_index(parameters: List[Dict], output_file: Path):
    """Generate an alphabetical index of all parameters."""

    rst_content = []

    rst_content.append(".. _parameter_index:")
    rst_content.append("")
    rst_content.append("Parameter Index")
    rst_content.append("===============")
    rst_content.append("")
    rst_content.append(
        "Alphabetical index of all SUEWS parameters. Click on any parameter to jump to its detailed documentation."
    )
    rst_content.append("")

    # Group by first letter
    by_letter = {}
    for param in parameters:
        # Use the last part of the path as the index name
        name = param["path"].split(".")[-1]
        first_letter = name[0].upper()
        if first_letter not in by_letter:
            by_letter[first_letter] = []
        by_letter[first_letter].append((name, param["path"]))

    # Generate index sections
    for letter in sorted(by_letter.keys()):
        rst_content.append(f"**{letter}**")
        rst_content.append("")

        # Sort entries within each letter
        entries = sorted(by_letter[letter], key=lambda x: x[0].lower())

        for name, path in entries:
            param_id = path.replace(".", "_")
            rst_content.append(f"* :ref:`{name} <param_{param_id}>` (``{path}``)")

        rst_content.append("")

    # Write to file
    with open(output_file, "w") as f:
        f.write("\n".join(rst_content))

    print(f"Generated parameter index: {output_file}")


def main():
    """Main function."""
    parser = argparse.ArgumentParser(
        description="Generate parameter reference documentation for SUEWS"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).parent / "source" / "inputs" / "parameter_reference.rst",
        help="Output file path for the parameter reference",
    )

    args = parser.parse_args()

    # Ensure output directory exists
    args.output.parent.mkdir(parents=True, exist_ok=True)

    # Generate parameter reference
    generate_parameter_reference(args.output)


if __name__ == "__main__":
    main()
