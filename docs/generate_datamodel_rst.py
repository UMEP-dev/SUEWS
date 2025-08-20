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

    # Handle special cases with a single return point
    result = None

    if origin is Union:
        # Handle Optional[X] as X (Optional)
        if len(args) == 2 and type(None) in args:
            non_none_arg = next(arg for arg in args if arg is not type(None))
            result = f"{get_user_friendly_type_name(non_none_arg)} (Optional)"
        else:
            result = " | ".join(get_user_friendly_type_name(arg) for arg in args)
    elif origin in {list, list}:
        result = f"List of {get_user_friendly_type_name(args[0])}" if args else "List"
    elif origin in {dict, dict}:
        if args and len(args) == 2:
            result = f"Mapping from {get_user_friendly_type_name(args[0])} to {get_user_friendly_type_name(args[1])}"
        else:
            result = "Mapping"
    elif hasattr(type_hint, "__name__"):
        # Check if it's a RefValue wrapper
        if type_hint.__name__ == "RefValue" and args:
            result = f"Value (type: {get_user_friendly_type_name(args[0])}) with DOI/Reference"
        else:
            result = type_hint.__name__

    return result if result is not None else str(type_hint)


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


def _is_complex_default(value) -> bool:
    """Check if a default value is too complex to display in full."""
    # Check if it's a list of objects (not simple types)
    if isinstance(value, list) and value:
        # If the first element has attributes (is an object), it's complex
        first_elem = value[0]
        if hasattr(first_elem, "__dict__"):
            return True
        # Also check if the repr is too long
        if len(repr(value)) > 200:
            return True
    # Check for other complex types that have long reprs
    return hasattr(value, "__dict__") and len(repr(value)) > 200


def _format_complex_default(value) -> str:
    """Format complex default values in a user-friendly way."""
    # Handle lists of objects
    if isinstance(value, list) and value:
        first_elem = value[0]
        # If it's a list of Pydantic models
        if isinstance(first_elem, BaseModel):
            class_name = first_elem.__class__.__name__
            # Convert class name to lowercase for RST reference
            # SurfaceInitialState -> surfaceinitialstate
            doc_name = class_name.lower()
            return f"List of {len(value)} {class_name} objects - see :doc:`{doc_name}`"
        # Generic list of objects
        elif hasattr(first_elem, "__dict__"):
            class_name = first_elem.__class__.__name__
            return f"List of {len(value)} {class_name} objects"
        else:
            # Truncate long lists
            return f"List with {len(value)} items"

    # Handle single complex objects
    if isinstance(value, BaseModel):
        class_name = value.__class__.__name__
        doc_name = class_name.lower()
        return f"{class_name} object - see :doc:`{doc_name}`"

    # Fallback for other complex types
    return "Complex object - see related documentation"


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


# --- Helper functions for field processing ---
def _should_skip_field(field_info: FieldInfo, include_internal: bool) -> bool:
    """Check if a field should be skipped in documentation."""
    return (
        not include_internal
        and isinstance(field_info.json_schema_extra, dict)
        and field_info.json_schema_extra.get("internal_only", False)
    )


def _add_field_index_entries(
    rst_content: list, field_name: str, model_name: str
) -> None:
    """Add index entries for a field."""
    rst_content.append(".. index::")
    rst_content.append(f"   single: {field_name} (YAML parameter)")
    rst_content.append(f"   single: {model_name}; {field_name}")
    rst_content.append("")


def _add_field_reference_label(rst_content: list, field_name: str) -> None:
    """Add reference label for cross-referencing specific fields."""
    method_fields = {"diagmethod", "stabilitymethod", "localclimatemethod", "gsmodel"}
    if field_name in method_fields:
        rst_content.append(f".. _{field_name}:")
        rst_content.append("")


def _process_field_description(
    field_info: FieldInfo,
    field_name: str,
    field_type_hint: Any,
    model_name: str,
    all_supy_models: dict[str, type[BaseModel]],
    include_internal: bool,
) -> tuple[list[str], list[str]]:
    """Process field description and extract method options."""
    description_parts = []
    options_list = []
    base_description = getattr(field_info, "description", None)

    if base_description:
        # Get enum class if this field uses one
        enum_class = get_enum_class_from_field(field_info, field_type_hint)

        # Parse method options if present
        main_desc, options = parse_method_options(
            base_description, enum_class, include_internal
        )

        if options:
            description_parts.append(f"   {main_desc}")
            options_list = options
        else:
            description_parts.append(f"   {base_description.strip()}")

    # Special handling for 'ref' field at model level
    if field_name == "ref" and not base_description:
        ref_contexts = {
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
        ref_desc = ref_contexts.get(
            model_name,
            f"Reference/citation for this {model_name.lower()} configuration",
        )
        description_parts.append(f"   {ref_desc}")

    # YAML structure hint for RefValue fields
    origin_type = get_origin(field_type_hint) or field_type_hint
    if hasattr(origin_type, "__name__") and origin_type.__name__ == "RefValue":
        doi_args = get_args(field_type_hint)
        val_type_name = "..."  # Default placeholder
        is_complex = False

        if doi_args:
            inner_type = get_origin(doi_args[0]) or doi_args[0]
            if (
                hasattr(inner_type, "__name__")
                and inner_type.__name__ in all_supy_models
                and issubclass(inner_type, BaseModel)
            ):
                is_complex = True
            else:
                val_type_name = get_user_friendly_type_name(doi_args[0])

        hint = f"   In YAML, this is typically specified using a ``value`` key, e.g.: ``{field_name}: {{value: {val_type_name}}}``."
        if is_complex:
            hint += " The structure of this ``value`` is detailed in the linked section below."
        description_parts.append(hint)

    return description_parts, options_list


def _add_field_unit(
    rst_content: list,
    field_info: FieldInfo,
    base_description: Optional[str],
    options_list: list,
) -> None:
    """Add unit information for a field."""
    unit = None
    if isinstance(field_info.json_schema_extra, dict):
        unit = field_info.json_schema_extra.get("unit")

    # Check if this is a method/enum parameter
    is_method_param = options_list or (
        base_description and "Options:" in base_description
    )

    if unit and not is_method_param:
        formatted_unit = format_unit(unit)
        rst_content.append(f"   :Unit: {formatted_unit}")
    elif not is_method_param and base_description:
        # Try to parse from description
        if "[" in base_description and "]" in base_description:
            try:
                parsed_unit = base_description[
                    base_description.rfind("[") + 1 : base_description.rfind("]")
                ]
                if len(parsed_unit) < 20 and " " not in parsed_unit:
                    formatted_unit = format_unit(parsed_unit)
                    rst_content.append(f"   :Unit: {formatted_unit}")
            except Exception:
                pass


def _add_field_constraints(
    rst_content: list,
    field_info: FieldInfo,
    field_type_hint: Any,
) -> None:
    """Add constraint information for a field."""
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
            if value is not None:
                constraints_desc.append(f"{desc_prefix}: ``{value!r}``")

    # Check for enum/Literal constraints
    origin_type = get_origin(field_type_hint)
    args = get_args(field_type_hint)
    if origin_type is Literal:
        constraints_desc.append(
            f"Allowed values: {', '.join(f'``{arg!r}``' for arg in args)}"
        )
    elif origin_type is Union and any(get_origin(arg) is Literal for arg in args):
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


def _is_direct_model_match(field_type_hint: Any, origin_pt: Any) -> bool:
    """Check if the model directly matches the field type."""
    return field_type_hint == origin_pt


def _is_list_or_dict_of_model(field_type_hint: Any, origin_pt: Any) -> bool:
    """Check if field is a list/dict containing the model."""
    origin = get_origin(field_type_hint)
    if origin not in {list, list, dict, dict}:
        return False

    args = get_args(field_type_hint)
    if not args:
        return False

    first_arg = get_origin(args[0]) or args[0]
    return first_arg == origin_pt


def _is_refvalue_of_model(field_type_hint: Any, origin_pt: Any) -> bool:
    """Check if field is a RefValue wrapping the model."""
    origin = get_origin(field_type_hint) or field_type_hint

    if not hasattr(origin, "__name__") or origin.__name__ != "RefValue":
        return False

    args = get_args(field_type_hint)
    if not args:
        return False

    first_arg = get_origin(args[0]) or args[0]
    return first_arg == origin_pt


def _format_initial_state_message(
    field_name: str, nested_model_name: str, is_basic: bool
) -> str:
    """Format message for initial state fields."""
    if is_basic:
        return (
            f"   For ``{field_name}``, one generic SurfaceInitialState object is used to specify initial conditions - "
            f"see :doc:`surfaceinitialstate` for details."
        )
    return (
        f"   For ``{field_name}``, one vegetation-specific initial state with additional parameters is used - "
        f"see :doc:`{nested_model_name.lower()}` for details."
    )


def _generate_nested_model_link_message(
    field_name: str,
    field_type_hint: Any,
    nested_model_to_document: type[BaseModel],
) -> str:
    """Generate appropriate link message for nested model documentation."""
    nested_model_name = nested_model_to_document.__name__
    nested_model_name_lower = nested_model_name.lower()
    origin_of_field = get_origin(field_type_hint)
    args_of_field = get_args(field_type_hint)

    # Build the message based on type
    message = None

    # Check RefValue wrapping
    if _is_refvalue_of_model(field_type_hint, nested_model_to_document):
        message = (
            f"   The structure for the ``value`` key of ``{field_name}`` is detailed in "
            f":doc:`{nested_model_name_lower}`."
        )
    # Check list types
    elif origin_of_field is list or origin_of_field is list:
        if nested_model_name == "SurfaceInitialState":
            message = (
                f"   For ``{field_name}``, a list of generic SurfaceInitialState objects is used to specify initial conditions for each layer - "
                f"see :doc:`surfaceinitialstate` for details."
            )
        else:
            message = (
                f"   Each item in the ``{field_name}`` list must conform to the "
                f":doc:`{nested_model_name_lower}` structure."
            )
    # Check dict types
    elif origin_of_field is dict or origin_of_field is dict:
        message = (
            f"   Each value in the ``{field_name}`` mapping (dictionary) must conform to the "
            f":doc:`{nested_model_name_lower}` structure."
        )
    # Check direct nesting
    elif (get_origin(field_type_hint) or field_type_hint) == nested_model_to_document:
        if (
            nested_model_name.startswith("InitialState")
            and nested_model_name != "InitialStates"
        ):
            basic_states = {
                "InitialStatePaved",
                "InitialStateBldgs",
                "InitialStateBsoil",
                "InitialStateWater",
            }
            is_basic = nested_model_name in basic_states
            message = _format_initial_state_message(
                field_name, nested_model_name, is_basic
            )
        else:
            message = (
                f"   The ``{field_name}`` parameter group is defined by the "
                f":doc:`{nested_model_name_lower}` structure."
            )
    # Check Optional[List[X]]
    elif origin_of_field is Union:
        for union_arg in args_of_field:
            if get_origin(union_arg) is list:
                list_args = get_args(union_arg)
                if list_args and list_args[0].__name__ == "SurfaceInitialState":
                    message = (
                        f"   For ``{field_name}``, a list of generic SurfaceInitialState objects is used to specify initial conditions for each layer - "
                        f"see :doc:`surfaceinitialstate` for details."
                    )
                    break

    # Fallback
    if message is None:
        message = (
            f"   For ``{field_name}``, if using the {nested_model_name} structure, "
            f"see :doc:`{nested_model_name_lower}` for details."
        )

    return message


def _find_nested_model(
    field_type_hint: Any,
    all_supy_models: dict[str, type[BaseModel]],
    model_class: type[BaseModel],
) -> Optional[type[BaseModel]]:
    """Find nested model in field type hint."""
    possible_model_types = [field_type_hint, *list(get_args(field_type_hint))]

    # Also check for models nested inside List/Dict types
    for arg in get_args(field_type_hint):
        if get_origin(arg) in {list, dict}:
            possible_model_types.extend(get_args(arg))

    for pt in possible_model_types:
        origin_pt = get_origin(pt) or pt
        if (
            hasattr(origin_pt, "__name__")
            and origin_pt.__name__ in all_supy_models
            and issubclass(origin_pt, BaseModel)
            and origin_pt != model_class
        ):
            # Prioritize direct matches
            if (
                _is_direct_model_match(field_type_hint, origin_pt)
                or _is_list_or_dict_of_model(field_type_hint, origin_pt)
                or _is_refvalue_of_model(field_type_hint, origin_pt)
            ):
                return origin_pt
            # Return as fallback if no better match found
            return origin_pt

    return None


def _process_nested_model(
    nested_model: type[BaseModel],
    field_name: str,
    field_type_hint: Any,
    rst_content: list,
    output_dir: Path,
    processed_models: set[type[BaseModel]],
    all_supy_models: dict[str, type[BaseModel]],
    include_internal: bool,
) -> None:
    """Process and document a nested model."""
    rst_content.append("")  # Blank line before link text
    link_message = _generate_nested_model_link_message(
        field_name, field_type_hint, nested_model
    )
    rst_content.append(link_message)

    # Recursively generate documentation for the nested model
    generate_rst_for_model(
        nested_model,
        output_dir,
        processed_models,
        all_supy_models,
        include_internal,
    )


def _add_reference_field(rst_content: list, field_type_hint: Any) -> None:
    """Add reference field for RefValue types."""
    origin_type = get_origin(field_type_hint) or field_type_hint
    type_name = getattr(origin_type, "__name__", "")

    if type_name in {"RefValue", "FlexibleRefValue"} or "RefValue" in str(
        field_type_hint
    ):
        rst_content.append(
            "   :Reference: Optional - provide DOI/citation in standard format"
        )


def _process_field_default(
    field_info: FieldInfo,
    field_name: str,
    model_name: str,
) -> tuple[str, str]:
    """Process default value for a field."""
    default_value = None
    has_default = False

    if field_info.default is not None and field_info.default != inspect.Parameter.empty:
        default_value = field_info.default
        has_default = True
    elif field_info.default_factory is not None:
        try:
            default_value = field_info.default_factory()
            has_default = True
        except Exception:
            default_value = "Dynamically generated"
            has_default = True

    # Check if this is actually a required field
    if has_default:
        default_str = str(default_value)
        if "PydanticUndefined" in default_str or "undefined" in default_str.lower():
            has_default = False

    # Check if field is site-specific
    is_site_specific = _is_site_specific_field(field_name, model_name)

    # Format the value for display
    if not has_default:
        display_value = "Not specified"
        label = "Sample value"
    elif is_site_specific:
        if isinstance(default_value, str) and default_value == "Dynamically generated":
            display_value = default_value
        elif _is_complex_default(default_value):
            display_value = _format_complex_default(default_value)
        else:
            display_value = f"``{default_value!r}``"
        label = "Sample value"
    else:
        if isinstance(default_value, str) and default_value == "Dynamically generated":
            display_value = default_value
        elif _is_complex_default(default_value):
            display_value = _format_complex_default(default_value)
        else:
            display_value = f"``{default_value!r}``"
        label = "Default"

    return label, display_value


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
        # Check if field should be skipped
        if _should_skip_field(field_info, include_internal):
            continue

        field_type_hint = field_info.annotation

        # Add index entries
        _add_field_index_entries(rst_content, field_name, model_name)

        # Add reference label for method fields
        _add_field_reference_label(rst_content, field_name)

        # Start option block
        rst_content.append(f".. option:: {field_name}")
        rst_content.append("")

        # Process description and options
        description_parts, options_list = _process_field_description(
            field_info,
            field_name,
            field_type_hint,
            model_name,
            all_supy_models,
            include_internal,
        )

        base_description = getattr(field_info, "description", None)

        if description_parts:
            rst_content.extend(description_parts)
            rst_content.append("")  # Blank line after description block

        # Options (if any)
        if options_list:
            rst_content.append("   :Options:")
            for opt in options_list:
                rst_content.append(f"      | {opt}")
            rst_content.append("")  # Blank line after options

        # Add unit information
        _add_field_unit(rst_content, field_info, base_description, options_list)

        # Process and add default value
        label, display_value = _process_field_default(
            field_info, field_name, model_name
        )
        rst_content.append(f"   :{label}: {display_value}")

        # Add reference field for RefValue types
        _add_reference_field(rst_content, field_type_hint)

        # Add constraints
        _add_field_constraints(rst_content, field_info, field_type_hint)

        # Find and process nested models
        nested_model = _find_nested_model(field_type_hint, all_supy_models, model_class)

        if nested_model:
            _process_nested_model(
                nested_model,
                field_name,
                field_type_hint,
                rst_content,
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
    # Classes that are internal and should not be documented
    INTERNAL_CLASSES = {
        "HDD_ID",  # Internal heating/cooling degree days tracking
        "WaterUse",  # Internal water use tracking
        # Note: SurfaceInitialState is used in roofs/walls fields so needs documentation
    }

    models = {}
    for name, obj in inspect.getmembers(module):
        if (
            inspect.isclass(obj)
            and issubclass(obj, BaseModel)
            and obj.__module__ == module.__name__
            and name not in INTERNAL_CLASSES  # Skip internal classes
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

    # Discover models in supy.data_model.core submodule
    data_model_module_root = Path(supy.data_model.__file__).parent / "core"
    if not data_model_module_root.exists():
        print(f"Error: core directory not found at {data_model_module_root}")
        return
        
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
            "type.py",  # Skip internal type definitions
        }:
            continue  # Skip non-model files

        module_name_to_import = f"data_model.core.{py_file.stem}"

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
