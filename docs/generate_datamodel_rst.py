"""
SUEWS Data Model RST Generator.

This script generates reStructuredText (.rst) files for the SUEWS Pydantic data models.
It should be run manually by developers whenever the data model definitions in
`src/supy/data_model/` are changed.

The generated files are saved in `docs/source/inputs/yaml/schema/` and are used
by Sphinx to build the official documentation.

To run this script, navigate to the `docs/` directory and execute:
    python generate_datamodel_rst.py
"""

import argparse
from enum import Enum
import importlib
import inspect
from pathlib import Path
import re
import sys
from typing import Any, Literal, Optional, Union, get_args, get_origin

from pydantic import BaseModel
from pydantic.fields import FieldInfo

# Add the project root to sys.path to allow importing supy
# Assuming this script is in 'docs/generate_datamodel_rst.py'
# And supy is in 'src/supy'
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SRC_PATH = PROJECT_ROOT / "src"
sys.path.insert(0, str(SRC_PATH))

# Import data models - work directly with source files to avoid build issues
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


# --- Helper function to format units with proper typography ---
def format_unit(unit: str) -> str:
    """
    Convert plain text units from Python source to RST substitution format.

    The conversion transforms units like "m^2" to "|m^2|" so that the
    rst_prolog substitutions in conf.py will render them properly.

    Examples
    --------
        m^2 -> |m^2|
        m^-1 -> |m^-1|
        W/m^2 -> W |m^-2|
        m³/m³ -> |m^3| |m^-3|
    """
    if not unit:
        return unit

    # First handle special cases with Unicode characters
    unit = unit.replace("³", "^3").replace("²", "^2").replace("⁻", "^-")

    # Handle division - convert denominators to negative exponents
    if "/" in unit:
        parts = unit.split("/")
        if len(parts) == 2:
            numerator = parts[0].strip()
            denominator = parts[1].strip()

            # Convert denominator exponents to negative
            if "^" in denominator:
                base, exp = denominator.split("^", 1)
                denominator = f"{base}^-{exp}"
            else:
                denominator = f"{denominator}^-1"

            # Process both parts
            numerator = process_unit_part(numerator)
            denominator = process_unit_part(denominator)

            return f"{numerator} {denominator}"

    # No division, just process the unit
    return process_unit_part(unit)


def process_unit_part(part: str) -> str:
    """Process a single part of a unit (no division)."""
    # Check if this matches a known substitution pattern
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
        "cap^-1",
        "ha^-1",
        "d^-1",
        "d^-2",
    ]

    # Direct match
    if part in known_patterns:
        return f"|{part}|"

    # Handle compound units like "kg m^-3"
    words = part.split()
    formatted_words = []
    for word in words:
        if word in known_patterns:
            formatted_words.append(f"|{word}|")
        elif "^" in word and any(
            word.startswith(base) for base in ["m", "s", "kg", "K", "W", "h", "d"]
        ):
            # This looks like a unit with exponent
            formatted_words.append(f"|{word}|")
        else:
            formatted_words.append(word)

    return " ".join(formatted_words)


# --- Helper function to parse and format method options ---
def get_enum_class_from_field(
    field_info: FieldInfo, field_type_hint: Any
) -> Optional[type]:
    """Extract the enum class from a field's type hint."""
    # Handle Union types
    origin = get_origin(field_type_hint)
    if origin is Union:
        # Check each type in the Union
        for arg in get_args(field_type_hint):
            enum_class = get_enum_class_from_field(field_info, arg)
            if enum_class:
                return enum_class
        return None

    # Handle RefValue[EnumClass] and FlexibleRefValue(EnumClass)
    origin = get_origin(field_type_hint) or field_type_hint
    args = get_args(field_type_hint)

    if (
        hasattr(origin, "__name__")
        and origin.__name__
        in {
            "RefValue",
            "FlexibleRefValue",
        }
        and args
        and hasattr(args[0], "__bases__")
        and Enum in args[0].__bases__
    ):
        return args[0]

    # Direct enum type
    if hasattr(field_type_hint, "__bases__") and Enum in field_type_hint.__bases__:
        return field_type_hint

    return None


def is_internal_enum_value(enum_class: type, value: int) -> bool:
    """Check if an enum value is marked as internal."""
    if not enum_class:
        return False

    try:
        # Find the enum member with this value
        for member in enum_class:
            if member.value == value:
                return getattr(member, "_internal", False)
    except Exception:
        pass

    return False


def _should_skip_option(
    num: str, enum_class: Optional[type], include_internal: bool
) -> bool:
    """Check if an option should be skipped based on internal flags."""
    if include_internal or not enum_class:
        return False

    # Handle ranges like "11-13" or "100-300"
    if "-" in num:
        start, end = map(int, num.split("-"))
        # If any value in range is internal, skip the whole range
        return any(is_internal_enum_value(enum_class, v) for v in range(start, end + 1))
    # Single value
    return is_internal_enum_value(enum_class, int(num))


def _format_option_description(desc: str) -> str:
    """Format option description with appropriate markdown."""
    replacements = [
        ("(recommended)", "**(recommended)**"),
        ("(not recommended)", "**(not recommended)**"),
        ("(experimental)", "**(experimental)**"),
    ]
    for old, new in replacements:
        if old in desc:
            desc = desc.replace(old, new)
    return desc


def parse_method_options(
    description: str, enum_class: Optional[type] = None, include_internal: bool = True
) -> tuple[str, list[str]]:
    """
    Parse method options from description string.

    Args:
        description: The field description to parse
        enum_class: The enum class if this field uses one
        include_internal: Whether to include internal options

    Returns
    -------
        tuple: (main_description, list_of_option_strings)
    """
    if "Options:" not in description:
        return description, []

    parts = description.split("Options:", 1)
    main_desc = parts[0].strip()
    options_text = parts[1].strip()

    # Parse individual options (semicolon separated)
    options = []
    for opt_text in options_text.split(";"):
        opt_text_stripped = opt_text.strip()
        if not opt_text_stripped:
            continue

        # Format: "0 (NAME) = Description" or similar
        # Extract number, name in parentheses, and description
        match = re.match(
            r"^(\d+(?:-\d+)?)\s*\(([^)]+)\)\s*=\s*(.+)$", opt_text_stripped
        )
        if not match:
            # Fallback for other formats
            options.append(opt_text_stripped)
            continue

        num, name, desc = match.groups()

        # Check if this option should be filtered out
        if _should_skip_option(num, enum_class, include_internal):
            continue

        # Format description with markdown
        desc = _format_option_description(desc)
        options.append(f"``{num}`` ({name}) = {desc}")

    return main_desc, options


# --- Helper function to get user-friendly type names ---
def get_user_friendly_type_name(type_hint: Any) -> str:
    """Generate a user-friendly string representation of a type hint."""
    origin = get_origin(type_hint)
    args = get_args(type_hint)

    if origin is Union:
        # Handle Optional[X] as X (Optional)
        if len(args) == 2 and type(None) in args:
            non_none_arg = next(arg for arg in args if arg is not type(None))
            return f"{get_user_friendly_type_name(non_none_arg)} (Optional)"
        return " | ".join(get_user_friendly_type_name(arg) for arg in args)

    if origin in {list, list}:
        return f"List of {get_user_friendly_type_name(args[0])}" if args else "List"

    if origin in {dict, dict}:
        if args and len(args) == 2:
            return f"Mapping from {get_user_friendly_type_name(args[0])} to {get_user_friendly_type_name(args[1])}"
        return "Mapping"

    if hasattr(type_hint, "__name__"):
        # Check if it's a RefValue wrapper
        if type_hint.__name__ == "RefValue" and args:
            return f"Value (type: {get_user_friendly_type_name(args[0])}) with DOI/Reference"
        return type_hint.__name__

    return str(type_hint)


# --- Helper functions for RST generation ---
def _add_rst_header(
    rst_content: list, model_name: str, model_class: type[BaseModel]
) -> None:
    """Add RST header including meta tags, references, and title."""
    # Get display name from model config if available
    display_name = model_name.replace("_", " ").title()  # Default
    if hasattr(model_class, "model_config"):
        config = model_class.model_config
        # model_config is a dict, not a ConfigDict instance
        if isinstance(config, dict) and "title" in config:
            display_name = config["title"]

    # Add meta tags for search optimization
    rst_content.append(".. meta::")
    rst_content.append(
        f"   :description: SUEWS YAML configuration for {display_name.lower()} parameters"
    )
    rst_content.append(
        f"   :keywords: SUEWS, YAML, {model_name.lower()}, parameters, configuration"
    )
    rst_content.append("")

    # Add reference label for cross-referencing
    rst_content.append(f".. _{model_name.lower()}:")
    rst_content.append("")

    # Add index entries for search
    rst_content.append(".. index::")
    rst_content.append(f"   single: {model_name} (YAML parameter)")
    rst_content.append(f"   single: YAML; {model_name}")
    rst_content.append("")

    # Title
    rst_content.append(display_name)
    rst_content.append("=" * len(rst_content[-1]))
    rst_content.append("")


def _process_model_physics_docstring(docstring: str) -> str:
    """Add cross-references to ModelPhysics docstring."""
    lines = docstring.split("\n")
    processed_lines = []

    for line in lines:
        # Add cross-references to method names in the key interactions section
        processed_line = line
        if "- diagmethod:" in line:
            processed_line = processed_line.replace(
                "- diagmethod:", "- :ref:`diagmethod <diagmethod>`:"
            )
            processed_line = processed_line.replace(
                "diagmethod calculations", "``diagmethod`` calculations"
            )
        elif "- stabilitymethod:" in line:
            processed_line = processed_line.replace(
                "- stabilitymethod:",
                "- :ref:`stabilitymethod <stabilitymethod>`:",
            )
            processed_line = processed_line.replace(
                "BY diagmethod", "**BY** ``diagmethod``"
            )
        elif "- localclimatemethod:" in line:
            processed_line = processed_line.replace(
                "- localclimatemethod:",
                "- :ref:`localclimatemethod <localclimatemethod>`:",
            )
            processed_line = processed_line.replace(
                "FROM diagmethod", "**FROM** ``diagmethod``"
            )
        elif "- gsmodel:" in line:
            processed_line = processed_line.replace(
                "- gsmodel:", "- :ref:`gsmodel <gsmodel>`:"
            )
            processed_line = processed_line.replace(
                "localclimatemethod adjustments",
                "``localclimatemethod`` adjustments",
            )

        # Add bold for emphasis words
        processed_line = processed_line.replace(" HOW ", " **HOW** ")

        processed_lines.append(processed_line)

    docstring = "\n".join(processed_lines)

    # Add bold to Key method interactions and ensure proper list formatting
    docstring = docstring.replace(
        "Key method interactions:", "**Key method interactions:**\n"
    )

    return docstring


def _is_site_specific_field(field_name: str, model_name: str) -> bool:
    """Check if a field is site-specific (requires user input)."""
    site_specific_patterns = [
        # Geographic and site location
        "lat",
        "lng",
        "longitude",
        "latitude",
        "alt",
        "altitude",
        "timezone",
        # Site dimensions and areas
        "area",
        "height",
        "width",
        "depth",
        "surfacearea",
        "z",
        "z_meas",
        # Model control parameters (typically user-specified)
        "tstep",
        "forcing_file",
        "output_file",
        "start_time",
        "end_time",
        # Population and traffic
        "population",
        "traffic",
        "popdens",
        "trafficrate",
        # Surface properties (material-specific)
        "albedo",
        "emissivity",
        "reflectance",
        "transmittance",
        # Roughness parameters
        "z0m_in",
        "zdm_in",
        "z0",
        "zd",
        "roughness",
        # Temperature initializations
        "temp_c",
        "temp_s",
        "tsurf",
        "tair",
        "soiltemp",
        # State variables
        "state_",
        "initial",
        "soilstore",
        "soil_moisture",
        "snow_water",
        "snow_albedo",
        "snowpack",
        "swe",
        # Land cover and vegetation
        "fraction",
        "frac",
        "lai_max",
        "lai_min",
        "lai",
        "veg_frac",
        "bldg_frac",
        "paved_frac",
        # Surface fluxes and conductance
        "conductance",
        "resistance",
        "g_max",
        "g_min",
        "runoff",
        "drainage",
        "infiltration",
        # OHM and energy balance
        "ohm",
        "qf",
        "qh",
        "qe",
        "qs",
        "qn",
        # Building parameters
        "bldg_height",
        "wall_area",
        "roof_area",
        # Water balance
        "precipitation",
        "irrigation",
        "water_use",
        # SPARTACUS specific
        "ground_albedo_dir_mult_fact",
        "use_sw_direct_albedo",
    ]

    # Also check parent model name for context
    model_context_samples = [
        "initialstate",  # All initial states are site-specific
        "properties",  # Surface properties are material/site specific
        "landcover",  # Land cover fractions are site-specific
        "modelcontrol",  # Model control parameters are typically user-specified
        "site",  # Site-level parameters are location-specific
        "spartacus",  # SPARTACUS parameters often need tuning
    ]

    return any(
        pattern in field_name.lower() for pattern in site_specific_patterns
    ) or any(context in model_name.lower() for context in model_context_samples)


# --- Main RST Generation Logic ---
def generate_rst_for_model(
    model_class: type[BaseModel],
    output_dir: Path,
    processed_models: set[type[BaseModel]],
    all_supy_models: dict[str, type[BaseModel]],
    include_internal: bool = True,
) -> None:
    """Generate an .rst file for a given Pydantic model, focusing on user configuration."""
    if model_class in processed_models:
        return
    processed_models.add(model_class)

    model_name = model_class.__name__
    rst_content = []

    # Add header elements
    _add_rst_header(rst_content, model_name, model_class)

    # Model Docstring (if any)
    if model_class.__doc__:
        docstring = inspect.cleandoc(model_class.__doc__)

        # Special handling for ModelPhysics to add cross-references
        if model_name == "ModelPhysics" and "Key method interactions:" in docstring:
            docstring = _process_model_physics_docstring(docstring)

        rst_content.append(docstring)
        rst_content.append("")

    rst_content.append("**Parameters:**")
    rst_content.append("")

    for field_name, field_info in model_class.model_fields.items():
        # Check if field is marked as internal
        if (
            not include_internal
            and isinstance(field_info.json_schema_extra, dict)
            and field_info.json_schema_extra.get("internal_only", False)
        ):
            continue  # Skip internal fields in user docs
        field_type_hint = field_info.annotation

        # Add index entry for this parameter
        rst_content.append(".. index::")
        rst_content.append(f"   single: {field_name} (YAML parameter)")
        rst_content.append(f"   single: {model_name}; {field_name}")
        rst_content.append("")

        # Add reference label for cross-referencing specific fields (for method fields)
        if field_name in {
            "diagmethod",
            "stabilitymethod",
            "localclimatemethod",
            "gsmodel",
        }:
            rst_content.append(f".. _{field_name}:")
            rst_content.append("")

        # Start option block - no type notation for cleaner user docs
        rst_content.append(f".. option:: {field_name}")
        rst_content.append("")

        # Description
        description_parts = []
        base_description = getattr(field_info, "description", None)
        options_list = []

        if base_description:
            # Get enum class if this field uses one
            enum_class = get_enum_class_from_field(field_info, field_type_hint)

            # Parse method options if present
            main_desc, options = parse_method_options(
                base_description, enum_class, include_internal
            )

            if options:
                # Store options for later, just add main description here
                description_parts.append(f"   {main_desc}")
                options_list = options
            else:
                # No options to parse, use description as-is
                description_parts.append(f"   {base_description.strip()}")

        # Special handling for 'ref' field at model level
        if field_name == "ref" and not base_description:
            # Add a contextual description based on the model
            model_context = {
                "ModelPhysics": "Reference/citation for the physics configuration methods used",
                "ModelControl": "Reference/citation for the control parameters configuration",
                "Site": "Reference/citation for this site's data and configuration",
                "SiteProperties": "Reference/citation for the site properties data",
                "InitialStates": "Reference/citation for the initial state values",
                "LandCover": "Reference/citation for the land cover fractions",
                "AnthropogenicEmissions": "Reference/citation for the emissions data and methods",
                "Conductance": "Reference/citation for the conductance parameters",
                "SnowParams": "Reference/citation for the snow model parameters",
            }
            ref_desc = model_context.get(
                model_name,
                f"Reference/citation for this {model_name.lower()} configuration",
            )
            description_parts.append(f"   {ref_desc}")

        # YAML structure hint for RefValue fields
        origin_type_for_doi_check = get_origin(field_type_hint) or field_type_hint
        if (
            hasattr(origin_type_for_doi_check, "__name__")
            and origin_type_for_doi_check.__name__ == "RefValue"
        ):
            doi_args = get_args(field_type_hint)
            val_type_name_for_yaml = "..."  # Default placeholder
            is_complex_value = False
            if doi_args:
                inner_arg_type = get_origin(doi_args[0]) or doi_args[0]
                if (
                    hasattr(inner_arg_type, "__name__")
                    and inner_arg_type.__name__ in all_supy_models
                    and issubclass(inner_arg_type, BaseModel)
                ):
                    is_complex_value = True  # The value itself is a documented model
                else:
                    val_type_name_for_yaml = get_user_friendly_type_name(doi_args[0])

            hint = f"   In YAML, this is typically specified using a ``value`` key, e.g.: ``{field_name}: {{value: {val_type_name_for_yaml}}}``."
            if is_complex_value:
                hint += " The structure of this ``value`` is detailed in the linked section below."
            description_parts.append(hint)

        if description_parts:
            rst_content.extend(description_parts)
            rst_content.append("")  # Blank line after description block

        # Options (if any)
        if options_list:
            rst_content.append("   :Options:")
            for opt in options_list:
                rst_content.append(f"      | {opt}")
            rst_content.append("")  # Blank line after options

        # Unit - skip for method/enum parameters
        unit = None
        # Extract unit from the `unit` kwarg in `Field`
        if isinstance(field_info.json_schema_extra, dict):
            unit = field_info.json_schema_extra.get("unit")

        # Check if this is a method/enum parameter by looking for Options in description
        is_method_param = options_list or (
            base_description and "Options:" in base_description
        )

        if unit and not is_method_param:
            # Format units with proper typography
            formatted_unit = format_unit(unit)
            rst_content.append(f"   :Unit: {formatted_unit}")
        elif not is_method_param:
            # Fallback: Try to parse from description, e.g., "Some value [unit]"
            if base_description and "[" in base_description and "]" in base_description:
                try:
                    parsed_unit = base_description[
                        base_description.rfind("[") + 1 : base_description.rfind("]")
                    ]
                    if (
                        len(parsed_unit) < 20 and " " not in parsed_unit
                    ):  # Basic sanity check
                        formatted_unit = format_unit(parsed_unit)
                        rst_content.append(f"   :Unit: {formatted_unit}")
                except Exception:
                    pass  # Ignore parsing errors

        # Default or Sample value
        # Determine if this is a true default or just a sample value
        default_value = None
        has_default = False

        if (
            field_info.default is not None
            and field_info.default != inspect.Parameter.empty
        ):
            default_value = field_info.default
            has_default = True
        elif field_info.default_factory is not None:
            try:
                default_value = field_info.default_factory()
                has_default = True
            except Exception:
                default_value = "Dynamically generated"
                has_default = True

        # Check if this is actually a required field (no real default)
        # PydanticUndefined or similar indicates no default
        if has_default:
            # Check if it's PydanticUndefined or similar sentinel values
            default_str = str(default_value)
            if "PydanticUndefined" in default_str or "undefined" in default_str.lower():
                has_default = False

        # For physical/site-specific parameters, even numeric defaults might be samples
        is_site_specific = _is_site_specific_field(field_name, model_name)

        # Format the value for display
        if not has_default:
            display_value = "Not specified"
            label = "Sample value"  # Required fields show sample
        elif is_site_specific:
            # Site-specific params are really samples, regardless of type
            if (
                isinstance(default_value, str)
                and default_value == "Dynamically generated"
            ):
                display_value = default_value
            else:
                display_value = f"``{default_value!r}``"
            label = "Sample value"
        else:
            # True defaults
            if (
                isinstance(default_value, str)
                and default_value == "Dynamically generated"
            ):
                display_value = default_value
            else:
                display_value = f"``{default_value!r}``"
            label = "Default"

        rst_content.append(f"   :{label}: {display_value}")

        # Add Reference field for RefValue types
        origin_type = get_origin(field_type_hint) or field_type_hint
        type_name = getattr(origin_type, "__name__", "")

        # Check if it's a RefValue or FlexibleRefValue
        if type_name in {"RefValue", "FlexibleRefValue"} or "RefValue" in str(
            field_type_hint
        ):
            rst_content.append(
                "   :Reference: Optional - see :doc:`reference` for DOI/citation format"
            )

        # Constraints
        constraints_desc = []
        # Standard Pydantic v2 constraint attributes
        constraint_attrs_map = {
            "gt": ">",
            "ge": ">=",
            "lt": "<",
            "le": "<=",
            "min_length": "Minimum length",
            "max_length": "Maximum length",
            "multiple_of": "Must be a multiple of",
            "pattern": "Must match regex pattern",
        }

        for attr, desc_prefix in constraint_attrs_map.items():
            if hasattr(field_info, attr):
                value = getattr(field_info, attr)
                if value is not None:  # Ensure the constraint is actually set
                    constraints_desc.append(f"{desc_prefix}: ``{value!r}``")

        # Check for enum/Literal constraints from the type hint itself
        origin_type = get_origin(field_type_hint)
        args = get_args(field_type_hint)
        if origin_type is Literal:
            constraints_desc.append(
                f"Allowed values: {', '.join(f'``{arg!r}``' for arg in args)}"
            )
        elif origin_type is Union and any(get_origin(arg) is Literal for arg in args):
            # Handle Union of Literals, e.g. Optional[Literal['a', 'b']]
            literal_args_combined = []
            for union_arg in args:
                if get_origin(union_arg) is Literal:
                    literal_args_combined.extend(get_args(union_arg))
            if literal_args_combined:
                constraints_desc.append(
                    f"Allowed values: {', '.join(f'``{arg!r}``' for arg in set(literal_args_combined))}"
                )

        if constraints_desc:
            rst_content.append(f"   :Constraints: {'; '.join(constraints_desc)}")

        # Link to nested models
        # Check if the raw type or any type argument is a Pydantic model we know
        possible_model_types = [field_type_hint, *list(get_args(field_type_hint))]
        nested_model_to_document = None
        for pt in possible_model_types:
            origin_pt = get_origin(pt) or pt  # get actual type if it's a generic alias
            if (
                hasattr(origin_pt, "__name__")
                and origin_pt.__name__ in all_supy_models
                and issubclass(origin_pt, BaseModel)
                and origin_pt != model_class  # Do not link to self
            ):
                # Prioritize the model that is directly the field's type or the first arg of RefValue/List/Dict
                if (
                    field_type_hint == origin_pt
                    or (
                        get_origin(field_type_hint) in {list, list, dict, dict}
                        and get_args(field_type_hint)
                        and (
                            get_origin(get_args(field_type_hint)[0])
                            or get_args(field_type_hint)[0]
                        )
                        == origin_pt
                    )
                    or (
                        hasattr(
                            get_origin(field_type_hint) or field_type_hint, "__name__"
                        )
                        and (get_origin(field_type_hint) or field_type_hint).__name__
                        == "RefValue"
                        and get_args(field_type_hint)
                        and (
                            get_origin(get_args(field_type_hint)[0])
                            or get_args(field_type_hint)[0]
                        )
                        == origin_pt
                    )
                ):
                    nested_model_to_document = origin_pt
                    break
                if not nested_model_to_document:  # Fallback if no direct match yet
                    nested_model_to_document = origin_pt

        if nested_model_to_document:
            nested_model_name_lower = nested_model_to_document.__name__.lower()
            rst_content.append("")  # Blank line before link text

            origin_of_field = get_origin(field_type_hint)
            args_of_field = get_args(field_type_hint)
            link_message = ""

            is_ref_value_wrapping_model = (
                hasattr(origin_of_field or field_type_hint, "__name__")
                and (origin_of_field or field_type_hint).__name__ == "RefValue"
                and args_of_field
                and (get_origin(args_of_field[0]) or args_of_field[0])
                == nested_model_to_document
            )

            if is_ref_value_wrapping_model:
                link_message = (
                    f"   The structure for the ``value`` key of ``{field_name}`` is detailed in "
                    f":doc:`{nested_model_name_lower}`."
                )
            elif origin_of_field is list or origin_of_field is list:
                link_message = (
                    f"   Each item in the ``{field_name}`` list must conform to the "
                    f":doc:`{nested_model_name_lower}` structure."
                )
            elif origin_of_field is dict or origin_of_field is dict:
                link_message = (
                    f"   Each value in the ``{field_name}`` mapping (dictionary) must conform to the "
                    f":doc:`{nested_model_name_lower}` structure."
                )
            elif (
                get_origin(field_type_hint) or field_type_hint
            ) == nested_model_to_document:
                # Direct nesting: my_field: NestedModelType
                link_message = (
                    f"   The ``{field_name}`` parameter group is defined by the "
                    f":doc:`{nested_model_name_lower}` structure."
                )
            else:
                # Fallback if the exact relationship isn't matched by above (e.g. Union types)
                link_message = (
                    f"   For ``{field_name}``, if using the {nested_model_to_document.__name__} structure, "
                    f"see :doc:`{nested_model_name_lower}` for details."
                )

            rst_content.append(link_message)
            # The recursive call was here, it should remain to generate the linked doc.
            generate_rst_for_model(
                nested_model_to_document,
                output_dir,
                processed_models,
                all_supy_models,
                include_internal,
            )

        rst_content.append("")  # Blank line after each option

    # Write to file
    rst_file_path = output_dir / f"{model_name.lower()}.rst"
    with open(rst_file_path, "w", encoding="utf-8") as f:
        f.write("\n".join(rst_content))
    print(f"Generated: {rst_file_path}")


def get_all_models_in_module(module) -> dict[str, type[BaseModel]]:
    """Inspects a module and returns a dictionary of Pydantic models."""
    models = {}
    for name, obj in inspect.getmembers(module):
        if (
            inspect.isclass(obj)
            and issubclass(obj, BaseModel)
            and obj.__module__ == module.__name__
        ):
            models[name] = obj
    return models


def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(
        description="Generate RST documentation for SUEWS data models"
    )
    parser.add_argument(
        "--include-internal",
        action="store_true",
        help="Include internal/developer options in documentation",
    )
    args = parser.parse_args()
    output_dir_name = "inputs/yaml/schema"
    docs_source_path = PROJECT_ROOT / "docs" / "source"
    output_dir = docs_source_path / output_dir_name
    output_dir.mkdir(exist_ok=True)

    # Do NOT clean up previously generated RST files. Only generate new/updated ones.

    processed_models = set()
    all_supy_data_models = {}

    # Discover models in supy.data_model and its submodules
    data_model_module_root = Path(supy.data_model.__file__).parent
    for py_file in data_model_module_root.glob("*.py"):
        if py_file.name in {
            "__init__.py",
            "validation_controller.py",
            "validation_feedback.py",
            "validation_utils.py",
            "yaml_annotator.py",
            "yaml_annotator_json.py",
            "timezone_enum.py",
            "precheck.py",
        }:
            continue  # Skip non-model files

        module_name_to_import = f"data_model.{py_file.stem}"

        try:
            module = importlib.import_module(module_name_to_import)
            all_supy_data_models.update(get_all_models_in_module(module))
        except ImportError as e:
            print(f"Could not import {module_name_to_import}: {e}")
            continue

    # Define top-level models for documentation (these will be the starting points)
    # Users should provide a list of "main" configuration models
    # For now, let's try with a few common ones if they exist
    top_level_model_names = [
        "Model",
        "Site",
        # "SiteProperties",
        # "LandCover",
        # "SnowParams",
        # "InitialStates",
    ]  # Add more as needed

    models_to_process = []
    for name in top_level_model_names:
        if name in all_supy_data_models:
            models_to_process.append(all_supy_data_models[name])
        else:
            print(f"Warning: Top-level model '{name}' not found in supy.data_model.")

    if not models_to_process:
        print("No top-level models found to process. Exiting.")
        print(f"Available models: {list(all_supy_data_models.keys())}")
        return

    for model_class in models_to_process:
        generate_rst_for_model(
            model_class,
            output_dir,
            processed_models,
            all_supy_data_models,
            args.include_internal,
        )

    # Do NOT generate an index file here. User will manage their own index file.
    print(
        f"RST files for models written to {output_dir}. Please create your own index file if needed."
    )


if __name__ == "__main__":
    main()
