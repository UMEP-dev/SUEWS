"""
SUEWS Data Model RST Generator.

This script generates reStructuredText (.rst) files for the SUEWS Pydantic data models.
It uses the doc_utils module to extract model documentation as JSON, then formats it as RST.

To run this script, navigate to the `docs/` directory and execute:
    python generate_datamodel_rst.py
"""

import argparse
import json
from pathlib import Path
import sys
from typing import Any

# Add the project root to sys.path to allow importing supy
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SRC_PATH = PROJECT_ROOT / "src"
sys.path.insert(0, str(SRC_PATH))

# Import the documentation extractor through supy
from supy.data_model.doc_utils import ModelDocExtractor  # noqa: E402


class RSTGenerator:
    """Generate RST documentation from extracted model documentation."""

    def __init__(self, doc_data: dict[str, Any]):
        """
        Initialize the RST generator with extracted documentation data.

        Args:
            doc_data: Dictionary containing model documentation from ModelDocExtractor
        """
        self.doc_data = doc_data
        self.models = doc_data.get("models", {})
        self.hierarchy = doc_data.get("hierarchy", {})
        self.metadata = doc_data.get("metadata", {})

    def generate_all_rst(self, output_dir: Path) -> None:
        """
        Generate RST files for all models.

        Args:
            output_dir: Directory to write RST files to
        """
        output_dir.mkdir(parents=True, exist_ok=True)

        # Generate RST for each model
        for model_name, model_doc in self.models.items():
            if model_doc.get("circular_ref"):
                continue  # Skip circular references

            rst_content = self._format_model(model_name, model_doc)
            rst_file = output_dir / f"{model_name.lower()}.rst"

            with open(rst_file, "w", encoding="utf-8") as f:
                f.write(rst_content)

            print(f"Generated: {rst_file}")

        # Generate index.rst
        index_content = self._generate_index()
        index_file = output_dir / "index.rst"
        with open(index_file, "w", encoding="utf-8") as f:
            f.write(index_content)
        print(f"Generated: {index_file}")

        # Also generate a tabbed version for comparison
        index_tabs_content = self._generate_index_tabs()
        index_tabs_file = output_dir / "index-tabs.rst"
        with open(index_tabs_file, "w", encoding="utf-8") as f:
            f.write(index_tabs_content)
        print(f"Generated: {index_tabs_file}")

        print(f"\nGenerated {len(self.models)} RST files + index in {output_dir}")
        print(f"Total fields documented: {self.metadata.get('total_fields', 0)}")

    def _format_model(self, model_name: str, model_doc: dict[str, Any]) -> str:
        """Format a single model as RST."""
        lines = []

        # Add meta tags
        lines.extend(self._format_meta_tags(model_name, model_doc))

        # Add reference label and index
        lines.extend(self._format_reference_and_index(model_name))

        # Add title
        title = model_doc.get("title", model_name)
        lines.append(title)
        lines.append("=" * len(title))
        lines.append("")

        # Add description
        description = model_doc.get("description", "")
        if description:
            # Special handling for ModelPhysics
            if (
                model_name == "ModelPhysics"
                and "Key method interactions:" in description
            ):
                description = self._process_model_physics_description(description)
            lines.append(description)
            lines.append("")

        # Add parameters section
        if model_doc.get("fields"):
            lines.append("**Parameters:**")
            lines.append("")

            # Format each field
            for field_doc in model_doc["fields"]:
                lines.extend(self._format_field(field_doc, model_name))
                lines.append("")  # Blank line between fields

        return "\n".join(lines)

    @staticmethod
    def _format_meta_tags(
        model_name: str, model_doc: dict[str, Any]
    ) -> list[str]:
        """Format meta tags for SEO."""
        title = model_doc.get("title", model_name)
        return [
            ".. meta::",
            f"   :description: SUEWS YAML configuration for {title.lower()} parameters",
            f"   :keywords: SUEWS, YAML, {model_name.lower()}, parameters, configuration",
            "",
        ]

    @staticmethod
    def _format_reference_and_index(model_name: str) -> list[str]:
        """Format reference label and index entries."""
        return [
            f".. _{model_name.lower()}:",
            "",
            ".. index::",
            f"   single: {model_name} (YAML parameter)",
            f"   single: YAML; {model_name}",
            "",
        ]

    @staticmethod
    def _add_field_index_entries(field_name: str, model_name: str) -> list[str]:
        """Add index entries for a field.

        Args:
            field_name: Name of the field
            model_name: Name of the containing model

        Returns
        -------
            List of RST lines for index entries
        """
        lines = [
            ".. index::",
            f"   single: {field_name} (YAML parameter)",
            f"   single: {model_name}; {field_name}",
            "",
        ]

        # Add reference label for special method fields
        if field_name in {
            "diagmethod",
            "stabilitymethod",
            "localclimatemethod",
            "gsmodel",
        }:
            lines.append(f".. _{field_name}:")
            lines.append("")

        return lines

    @staticmethod
    def _format_field_description(field_doc: dict[str, Any]) -> list[str]:
        """Format the description section of a field.

        Args:
            field_doc: Field documentation dictionary

        Returns
        -------
            List of RST lines for the description
        """
        lines = []
        description = field_doc.get("description", "")

        if description:
            # Parse for method options if present
            if field_doc.get("options") and "Options:" in description:
                main_desc = description.split("Options:", 1)[0].strip()
                lines.append(f"   {main_desc}")
            else:
                lines.append(f"   {description}")
            lines.append("")

        return lines

    @staticmethod
    def _format_ref_value_hint(
        field_name: str, field_doc: dict[str, Any], type_info: dict[str, Any]
    ) -> list[str]:
        """Format YAML structure hint for RefValue types.

        Args:
            field_name: Name of the field
            field_doc: Field documentation dictionary
            type_info: Type information dictionary

        Returns
        -------
            List of RST lines for RefValue hint
        """
        if not type_info.get("is_ref_value"):
            return []

        lines = []
        nested_model = field_doc.get("nested_model")

        if nested_model:
            lines.extend([
                "   In YAML, this is typically specified using a ``value`` key, e.g.: ",
                f"   ``{field_name}: {{value: ...}}``.",
                "   The structure of this ``value`` is detailed in the linked section below.",
                "",
            ])
        else:
            type_str = field_doc.get("type", "...")
            lines.extend([
                "   In YAML, this is typically specified using a ``value`` key, e.g.: ",
                f"   ``{field_name}: {{value: {type_str}}}``.",
                "",
            ])

        return lines

    def _format_field_metadata(
        self, field_doc: dict[str, Any], type_info: dict[str, Any]
    ) -> list[str]:
        """Format metadata sections (options, unit, default, constraints).

        Args:
            field_doc: Field documentation dictionary
            type_info: Type information dictionary

        Returns
        -------
            List of RST lines for metadata
        """
        lines = []

        # Add options if present
        if field_doc.get("options"):
            lines.append("   :Options:")
            for opt in field_doc["options"]:
                opt_str = self._format_option(opt)
                lines.append(f"      | {opt_str}")
            lines.append("")

        # Add unit (not for enum fields)
        unit = field_doc.get("unit")
        if unit and not field_doc.get("options"):
            formatted_unit = self._format_unit(unit)
            lines.append(f"   :Unit: {formatted_unit}")

        # Add default/sample value
        default_label, default_value = self._format_default(field_doc)
        lines.append(f"   :{default_label}: {default_value}")

        # Add reference field for RefValue types
        if type_info.get("is_ref_value"):
            lines.append(
                "   :Reference: Optional - provide DOI/citation in standard format"
            )

        # Add constraints
        constraints = field_doc.get("constraints")
        if constraints:
            constraint_str = self._format_constraints(constraints)
            if constraint_str:
                lines.append(f"   :Constraints: {constraint_str}")

        return lines

    def _format_field(self, field_doc: dict[str, Any], model_name: str) -> list[str]:
        """Format a single field as RST."""
        lines = []
        field_name = field_doc["name"]
        type_info = field_doc.get("type_info", {})

        # Add index entries
        lines.extend(self._add_field_index_entries(field_name, model_name))

        # Start option block
        lines.append(f".. option:: {field_name}")
        lines.append("")

        # Add description
        lines.extend(self._format_field_description(field_doc))

        # Handle RefValue types - add YAML structure hint
        lines.extend(self._format_ref_value_hint(field_name, field_doc, type_info))

        # Add metadata (options, unit, default, constraints)
        lines.extend(self._format_field_metadata(field_doc, type_info))

        # Add link to nested model documentation
        nested_model = field_doc.get("nested_model")
        if nested_model:
            lines.append("")
            lines.append(
                self._format_nested_model_link(field_name, nested_model, type_info)
            )

        return lines

    def _format_option(self, option: dict[str, Any]) -> str:
        """Format a single option."""
        value = option["value"]
        name = option["name"]
        desc = option["description"]

        # Apply formatting to description
        desc = self._format_option_description(desc)

        return f"``{value}`` ({name}) = {desc}"

    @staticmethod
    def _format_option_description(desc: str) -> str:
        """Format option description with markdown."""
        replacements = [
            ("(recommended)", "**(recommended)**"),
            ("(not recommended)", "**(not recommended)**"),
            ("(experimental)", "**(experimental)**"),
        ]
        for old, new in replacements:
            if old in desc:
                desc = desc.replace(old, new)
        return desc

    def _format_unit(self, unit: str) -> str:
        """Format units for RST display."""
        if not unit:
            return unit

        # Convert to RST substitution format for proper rendering
        unit = unit.replace("³", "^3").replace("²", "^2").replace("⁻", "^-")

        # Handle division
        if "/" in unit:
            parts = unit.split("/")
            if len(parts) == 2:
                numerator = parts[0].strip()
                denominator = parts[1].strip()

                if "^" in denominator:
                    base, exp = denominator.split("^", 1)
                    denominator = f"{base}^-{exp}"
                else:
                    denominator = f"{denominator}^-1"

                numerator = self._process_unit_part(numerator)
                denominator = self._process_unit_part(denominator)

                return f"{numerator} {denominator}"

        return self._process_unit_part(unit)

    @staticmethod
    def _process_unit_part(part: str) -> str:
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
            "cap^-1",
            "ha^-1",
            "d^-1",
            "d^-2",
        ]

        if part in known_patterns:
            return f"|{part}|"

        # Handle compound units
        words = part.split()
        formatted = []
        for word in words:
            if word in known_patterns or (
                "^" in word and any(
                    word.startswith(b) for b in ["m", "s", "kg", "K", "W", "h", "d"]
                )
            ):
                formatted.append(f"|{word}|")
            else:
                formatted.append(word)

        return " ".join(formatted)

    @staticmethod
    def _format_default(field_doc: dict[str, Any]) -> tuple[str, str]:
        """Format default value for display."""
        is_required = field_doc.get("is_required", False)
        is_site_specific = field_doc.get("is_site_specific", False)

        # Check for default value
        if "default" in field_doc:
            default = field_doc["default"]
            is_complex = field_doc.get("is_complex", False)

            if is_complex:
                # Complex default already formatted as string
                display_value = default
            elif isinstance(default, dict) and "value" in default:
                # Enum default
                display_value = f"``{default['value']}`` ({default.get('name', '')})"
            else:
                display_value = f"``{default!r}``"

            label = "Sample value" if is_site_specific else "Default"
        elif is_required:
            display_value = "Required - must be specified"
            label = "Default"
        else:
            display_value = "Not specified"
            label = "Sample value" if is_site_specific else "Default"

        return label, display_value

    @staticmethod
    def _format_constraints(constraints: dict[str, Any]) -> str:
        """Format constraints for display."""
        parts = []

        # Numeric constraints
        constraint_map = {
            "gt": ">",
            "ge": ">=",
            "lt": "<",
            "le": "<=",
            "multiple_of": "Must be a multiple of",
            "min_length": "Minimum length",
            "max_length": "Maximum length",
            "pattern": "Must match regex pattern",
        }

        for key, desc in constraint_map.items():
            if key in constraints:
                value = constraints[key]
                parts.append(f"{desc}: ``{value!r}``")

        # Allowed values
        if "allowed_values" in constraints:
            values = constraints["allowed_values"]
            values_str = ", ".join(f"``{v!r}``" for v in values)
            parts.append(f"Allowed values: {values_str}")

        return "; ".join(parts)

    @staticmethod
    def _get_initial_state_message(field_name: str, nested_model: str, nested_lower: str) -> str:
        """Get message for initial state models."""
        basic_states = {
            "InitialStatePaved",
            "InitialStateBldgs",
            "InitialStateBsoil",
            "InitialStateWater",
        }

        if nested_model in basic_states:
            return (
                f"   For ``{field_name}``, one generic SurfaceInitialState object "
                f"is used to specify initial conditions - "
                f"see :doc:`surfaceinitialstate` for details."
            )
        return (
            f"   For ``{field_name}``, one vegetation-specific initial state "
            f"with additional parameters is used - "
            f"see :doc:`{nested_lower}` for details."
        )

    @staticmethod
    def _format_nested_model_link(
        field_name: str, nested_model: str, type_info: dict[str, Any]
    ) -> str:
        """Format link to nested model documentation."""
        nested_lower = nested_model.lower()

        # Build message based on type
        message = ""

        # Check type and build appropriate message
        if type_info.get("is_ref_value"):
            message = (
                f"   The structure for the ``value`` key of ``{field_name}`` "
                f"is detailed in :doc:`{nested_lower}`."
            )
        elif type_info.get("is_list"):
            if nested_model == "SurfaceInitialState":
                message = (
                    f"   For ``{field_name}``, a list of generic SurfaceInitialState "
                    f"objects is used to specify initial conditions for each layer - "
                    f"see :doc:`surfaceinitialstate` for details."
                )
            else:
                message = (
                    f"   Each item in the ``{field_name}`` list must conform to the "
                    f":doc:`{nested_lower}` structure."
                )
        elif type_info.get("is_dict"):
            message = (
                f"   Each value in the ``{field_name}`` mapping must conform to the "
                f":doc:`{nested_lower}` structure."
            )
        elif nested_model.startswith("InitialState") and nested_model != "InitialStates":
            message = RSTGenerator._get_initial_state_message(field_name, nested_model, nested_lower)
        else:
            message = (
                f"   The ``{field_name}`` parameter group is defined by the "
                f":doc:`{nested_lower}` structure."
            )

        return message

    @staticmethod
    def _process_model_physics_description(description: str) -> str:
        """Add cross-references to ModelPhysics description."""
        lines = description.split("\n")
        processed = []

        for line in lines:
            # Add cross-references to method names
            modified_line = line
            if "- diagmethod:" in modified_line:
                modified_line = modified_line.replace(
                    "- diagmethod:", "- :ref:`diagmethod <diagmethod>`:"
                )
                modified_line = modified_line.replace(
                    "diagmethod calculations", "``diagmethod`` calculations"
                )
            elif "- stabilitymethod:" in modified_line:
                modified_line = modified_line.replace(
                    "- stabilitymethod:", "- :ref:`stabilitymethod <stabilitymethod>`:"
                )
                modified_line = modified_line.replace(
                    "BY diagmethod", "**BY** ``diagmethod``"
                )
            elif "- localclimatemethod:" in modified_line:
                modified_line = modified_line.replace(
                    "- localclimatemethod:",
                    "- :ref:`localclimatemethod <localclimatemethod>`:",
                )
                modified_line = modified_line.replace(
                    "FROM diagmethod", "**FROM** ``diagmethod``"
                )
            elif "- gsmodel:" in modified_line:
                modified_line = modified_line.replace(
                    "- gsmodel:", "- :ref:`gsmodel <gsmodel>`:"
                )
                modified_line = modified_line.replace(
                    "localclimatemethod adjustments",
                    "``localclimatemethod`` adjustments",
                )

            # Add bold for emphasis
            modified_line = modified_line.replace(" HOW ", " **HOW** ")

            processed.append(modified_line)

        description = "\n".join(processed)

        # Ensure proper formatting
        description = description.replace(
            "Key method interactions:", "**Key method interactions:**\n"
        )

        return description

    def _generate_index(self) -> str:
        """Generate index.rst with hierarchical structure from JSON."""
        lines = [
            ".. _yaml_config_reference:",
            "",
            "YAML Configuration Reference",
            "============================",
            "",
            "This documentation follows the hierarchical structure of SUEWS YAML configuration files.",
            "",
        ]

        # Build the hierarchical structure from the models
        hierarchy = self._build_hierarchy()

        # Choose display style (uncomment the one you want):
        # Style 1: Simple nested list (all fields visible)
        # self._generate_hierarchy_rst_simple(hierarchy, lines, level=0, max_level=5)

        # Style 2: Mixed approach with collapsible sections for long groups (recommended)
        self._generate_hierarchy_rst_collapsible(hierarchy, lines, level=0, max_level=5)

        # Style 3: Aggressive collapse (most compact, everything >5 fields collapsed)
        # self._generate_hierarchy_rst_aggressive_collapse(hierarchy, lines, level=0, max_level=5)

        # Add toctree at the end with all documents (hidden)
        lines.extend([
            "",
            ".. toctree::",
            "   :hidden:",
            "   :maxdepth: 3",
            "",
        ])

        # Add all model files to toctree (excluding RefValue and Reference)
        for model_name in sorted(self.models.keys()):
            if model_name not in {"RefValue", "Reference"}:
                lines.append(f"   {model_name.lower()}")

        return "\n".join(lines)

    def _generate_index_tabs(self) -> str:
        """Generate index-tabs.rst with tabbed layout."""
        lines = [
            ".. _yaml_config_reference_tabs:",
            "",
            "YAML Configuration Reference (Tabbed Layout)",
            "=============================================",
            "",
            "This documentation follows the hierarchical structure of SUEWS YAML configuration files.",
            "",
            ".. note::",
            "   Click on the tabs below to navigate through different configuration sections.",
            "",
        ]

        # Build the hierarchical structure from the models
        hierarchy = self._build_hierarchy()

        # Use the tabbed layout style
        self._generate_hierarchy_rst_tabbed(hierarchy, lines, level=0, max_level=4)

        # Add toctree at the end with all documents (hidden)
        lines.extend([
            "",
            ".. toctree::",
            "   :hidden:",
            "   :maxdepth: 3",
            "",
        ])

        # Add all model files to toctree (excluding RefValue and Reference)
        for model_name in sorted(self.models.keys()):
            if model_name not in {"RefValue", "Reference"}:
                lines.append(f"   {model_name.lower()}")

        return "\n".join(lines)

    def _build_hierarchy(self) -> dict:
        """Build hierarchical structure from model relationships."""
        hierarchy = {}

        # Start with SUEWSConfig as root
        if "SUEWSConfig" in self.models:
            # Get full model structure including simple fields
            hierarchy["SUEWSConfig"] = self._get_model_children("SUEWSConfig", depth=0)

        return hierarchy

    def _get_model_children(self, model_name: str, depth: int = 0) -> dict:
        """Get children of a model based on its fields."""
        if model_name not in self.models:
            return {"title": model_name, "children": {}, "simple_fields": []}

        model_data = self.models[model_name]
        result = {
            "title": model_data.get("title", model_name),
            "model": model_name,
            "children": {},
            "simple_fields": [],  # Add simple fields list
        }

        # Stop expanding at certain depth or for profile/utility types
        profile_types = {"DayProfile", "HourlyProfile", "WeeklyProfile"}
        utility_types = {"Reference", "RefValue"}

        # Don't expand profile types or utility types or if we're too deep
        if model_name in profile_types or model_name in utility_types or depth > 4:
            return result

        # Collect all fields - both nested models and simple fields
        for field in model_data.get("fields", []):
            field_name = field["name"]
            nested_model = field.get("nested_model")

            # Skip Reference and RefValue as they're utility types
            if (
                nested_model
                and nested_model in self.models
                and nested_model not in utility_types
            ):
                # For profile types, just note the field name without expanding
                if nested_model in profile_types:
                    # Add profile fields as simple fields instead
                    result["simple_fields"].append({
                        "name": field_name,
                        "type": nested_model,
                        "description": field.get("description", ""),
                    })
                else:
                    # Use field name as key, get nested model structure
                    field_key = field_name
                    # For certain models with generic names, use field-specific titles
                    child_info = self._get_model_children(nested_model, depth + 1)

                    # Special handling for InitialStates fields with SurfaceInitialState
                    if model_name == "InitialStates" and field_key in {
                        "roofs",
                        "walls",
                    }:
                        # These are lists of SurfaceInitialState, show with proper field name
                        child_info["title"] = f"{field_key.capitalize()} Initial State"

                    # Only add if we don't already have this exact field
                    # (This allows multiple fields with the same nested model but different names)
                    if field_key not in result["children"]:
                        result["children"][field_key] = child_info
            elif not field_name.startswith("_"):
                result["simple_fields"].append({
                    "name": field_name,
                    "type": field.get("type", ""),
                    "description": field.get("description", ""),
                })

        return result

    def _generate_hierarchy_rst(self, hierarchy: dict, lines: list, level: int = 0):
        """Generate RST from hierarchy with proper heading levels."""
        # RST heading characters for different levels
        heading_chars = ["=", "-", "~", "^", '"', "'"]

        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})

            # Add heading
            if level < len(heading_chars):
                lines.append("")
                lines.append(title)
                lines.append(heading_chars[level] * len(title))

                # Add link to detail page
                lines.append(f":doc:`{model_ref.lower()}`")

                # Process children
                if children:
                    self._generate_hierarchy_rst(children, lines, level + 1)

    def _generate_hierarchy_rst_simple(
        self, hierarchy: dict, lines: list, level: int = 0, max_level: int = 4
    ):
        """Generate RST as a simple nested list, showing all fields sequentially."""
        # In RST, nested lists use indentation, not different markers
        # We'll use * for odd levels and - for even levels
        list_markers = ["*", "-", "*", "-"]

        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})
            simple_fields = model_info.get("simple_fields", [])

            # Skip if we're beyond max level
            if level >= max_level:
                return

            # Indent for nested lists
            indent = "  " * level
            marker = list_markers[min(level, len(list_markers) - 1)]

            # Create the main entry based on level
            if level == 0:
                # Top level - just the document link
                lines.append(f"{indent}{marker} :doc:`{title} <{model_ref.lower()}>`")
                lines.append("")
            else:
                # For nested items, use doc link
                lines.append(f"{indent}{marker} :doc:`{title} <{model_ref.lower()}>`")
                lines.append("")

            # Process children - both simple fields and nested models at the next level
            if level + 1 < max_level:
                next_indent = "  " * (level + 1)
                next_marker = list_markers[min(level + 1, len(list_markers) - 1)]

                # First list simple fields
                if simple_fields:
                    for field in simple_fields:
                        field_name = field["name"]
                        # Skip ref fields
                        if field_name == "ref":
                            continue
                        # Use :option: for field references
                        lines.append(
                            f"{next_indent}{next_marker} :option:`{field_name}`"
                        )
                        lines.append("")

                    # Add extra blank line after simple fields if there are nested children
                    if children:
                        lines.append("")

                # Then recursively process nested items
                if children:
                    # Recursively call for each child
                    self._generate_hierarchy_rst_simple(
                        children, lines, level + 1, max_level
                    )

    @staticmethod
    def _format_collapsible_fields(
        fields: list, indent: str
    ) -> list[str]:
        """Format fields as a collapsible section.

        Args:
            fields: List of field dictionaries
            indent: Base indentation

        Returns
        -------
            List of RST lines
        """
        lines = []
        field_count = len([f for f in fields if f["name"] != "ref"])

        lines.append(f"{indent}.. collapse:: {field_count} parameters")
        lines.append(f"{indent}   :class: collapse-sm")
        lines.append("")

        # Add fields inside collapse
        collapse_indent = indent + "   "
        for field in fields:
            field_name = field["name"]
            if field_name == "ref":
                continue
            lines.append(f"{collapse_indent}- :option:`{field_name}`")
        lines.append("")

        return lines

    def _format_fields_by_count(
        self, fields: list, indent: str, field_count: int,
        collapse_threshold: int = 15, horizontal_threshold: int = 8
    ) -> list[str]:
        """Format fields based on their count using appropriate display style.

        Args:
            fields: List of field dictionaries
            indent: Base indentation
            field_count: Pre-calculated field count
            collapse_threshold: Threshold for using collapsible sections
            horizontal_threshold: Threshold for using horizontal lists

        Returns
        -------
            List of RST lines
        """
        lines = []

        if field_count > collapse_threshold:
            # Use collapsible section for many fields
            lines.extend(self._format_collapsible_fields(fields, indent))
        elif field_count > horizontal_threshold:
            # Use columns for medium number of fields
            columns = 3
            field_lines = self._format_field_list(fields, indent, columns)
            lines.extend(field_lines)
            lines.append("")
        else:
            # Normal list for few fields
            field_lines = self._format_field_list(fields, indent, 0)
            lines.extend(field_lines)
            lines.append("")

        return lines

    def _generate_hierarchy_rst_collapsible(
        self, hierarchy: dict, lines: list, level: int = 0, max_level: int = 4
    ):
        """Generate RST with collapsible sections for long parameter groups."""
        # In RST, nested lists use indentation
        list_markers = ["*", "-", "*", "-"]

        # Thresholds for different display styles
        COLLAPSE_THRESHOLD = 15
        HORIZONTAL_THRESHOLD = 8

        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})
            simple_fields = model_info.get("simple_fields", [])

            # Skip if we're beyond max level
            if level >= max_level:
                return

            # Count non-ref fields
            field_count = len([f for f in simple_fields if f["name"] != "ref"])

            # Indent for nested lists
            indent = "  " * level
            marker = list_markers[min(level, len(list_markers) - 1)]

            # Create the main entry
            lines.append(f"{indent}{marker} :doc:`{title} <{model_ref.lower()}>`")
            lines.append("")

            # Process children - both simple fields and nested models
            if level + 1 < max_level:
                next_indent = "  " * (level + 1)

                # Handle simple fields based on count
                if field_count > 0:
                    field_lines = self._format_fields_by_count(
                        simple_fields, next_indent, field_count,
                        COLLAPSE_THRESHOLD, HORIZONTAL_THRESHOLD
                    )
                    lines.extend(field_lines)

                # Recursively process nested items
                if children:
                    self._generate_hierarchy_rst_collapsible(
                        children, lines, level + 1, max_level
                    )

    @staticmethod
    def _format_dropdown_fields(
        fields: list, indent: str
    ) -> list[str]:
        """Format fields as a dropdown section.

        Args:
            fields: List of field dictionaries
            indent: Base indentation

        Returns
        -------
            List of RST lines
        """
        lines = []
        field_count = len([f for f in fields if f["name"] != "ref"])

        lines.append(f"{indent}.. dropdown:: {field_count} parameters")
        lines.append(f"{indent}   :class-container: sd-shadow-sm")
        lines.append("")

        # Add fields inside dropdown
        dropdown_indent = indent + "   "
        for field in fields:
            field_name = field["name"]
            if field_name == "ref":
                continue
            lines.append(f"{dropdown_indent}- :option:`{field_name}`")
        lines.append("")

        return lines

    def _generate_hierarchy_rst_aggressive_collapse(
        self, hierarchy: dict, lines: list, level: int = 0, max_level: int = 4
    ):
        """Generate RST with aggressive collapsing - most sections collapsed by default."""
        # In RST, nested lists use indentation
        list_markers = ["*", "-", "*", "-"]

        # More aggressive thresholds - collapse almost everything
        COLLAPSE_THRESHOLD = 5
        HORIZONTAL_THRESHOLD = 20

        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})
            simple_fields = model_info.get("simple_fields", [])

            # Skip if we're beyond max level
            if level >= max_level:
                return

            # Count non-ref fields
            field_count = len([f for f in simple_fields if f["name"] != "ref"])

            # Indent for nested lists
            indent = "  " * level
            marker = list_markers[min(level, len(list_markers) - 1)]

            # Create the main entry
            lines.append(f"{indent}{marker} :doc:`{title} <{model_ref.lower()}>`")
            lines.append("")

            # Process children - both simple fields and nested models
            if level + 1 < max_level:
                next_indent = "  " * (level + 1)

                # Handle simple fields based on count
                if field_count > 0:
                    if field_count > COLLAPSE_THRESHOLD:
                        # Use dropdown for many fields
                        lines.extend(self._format_dropdown_fields(simple_fields, next_indent))
                    elif field_count > HORIZONTAL_THRESHOLD:
                        # Use columns for medium number of fields
                        columns = 3
                        field_lines = self._format_field_list(simple_fields, next_indent, columns)
                        lines.extend(field_lines)
                        lines.append("")
                    else:
                        # Normal list for few fields
                        field_lines = self._format_field_list(simple_fields, next_indent, 0)
                        lines.extend(field_lines)
                        lines.append("")

                # Recursively process nested items
                if children:
                    self._generate_hierarchy_rst_collapsible(
                        children, lines, level + 1, max_level
                    )

    @staticmethod
    def _format_field_list(
        fields: list, indent: str, columns: int = 0
    ) -> list[str]:
        """Format a list of fields as RST with optional columns.

        Args:
            fields: List of field dictionaries
            indent: Indentation string
            columns: Number of columns (0 for simple list, 2-3 for hlist)

        Returns
        -------
            List of formatted RST lines
        """
        lines = []
        # Filter out ref fields
        filtered_fields = [f for f in fields if f.get("name") != "ref"]

        if columns > 0:
            lines.append(f"{indent}.. hlist::")
            lines.append(f"{indent}   :columns: {columns}")
            lines.append("")
            for field in filtered_fields:
                field_name = (
                    field.get("name", field) if isinstance(field, dict) else field
                )
                lines.append(f"{indent}   * :option:`{field_name}`")
        else:
            for field in filtered_fields:
                field_name = (
                    field.get("name", field) if isinstance(field, dict) else field
                )
                lines.append(f"{indent}* :option:`{field_name}`")

        return lines

    @staticmethod
    def _get_field_display_format(field_count: int) -> int:
        """Determine the display format based on field count.

        Args:
            field_count: Number of fields to display

        Returns
        -------
            Number of columns (0 for list, 2-3 for columns)
        """
        if field_count > 15:
            return 3
        elif field_count > 8:
            return 2
        else:
            return 0

    def _generate_parameters_tab_content(
        self, fields: list, indent: str, field_count: int
    ) -> list[str]:
        """Generate content for a Parameters tab.

        Args:
            fields: List of simple fields
            indent: Base indentation
            field_count: Pre-calculated field count

        Returns
        -------
            List of RST lines for the tab content
        """
        lines = []
        columns = self._get_field_display_format(field_count)

        if columns > 0:
            lines.append(f"{indent}      .. hlist::")
            lines.append(f"{indent}         :columns: {columns}")
            lines.append("")
            for field in fields:
                field_name = field["name"]
                if field_name == "ref":
                    continue
                lines.append(f"{indent}         * :option:`{field_name}`")
        else:
            for field in fields:
                field_name = field["name"]
                if field_name == "ref":
                    continue
                lines.append(f"{indent}      * :option:`{field_name}`")

        lines.append("")
        return lines

    def _generate_nested_model_tab_content(
        self, tab_name: str, tab_content: dict, indent: str, level: int, max_level: int
    ) -> list[str]:
        """Generate content for a nested model tab.

        Args:
            tab_name: Name of the tab
            tab_content: Content dictionary for the tab
            indent: Base indentation
            level: Current nesting level
            max_level: Maximum nesting level

        Returns
        -------
            List of RST lines for the tab content
        """
        lines = []
        child_model = tab_content.get("model", tab_name)
        child_children = tab_content.get("children", {})
        child_simple_fields = tab_content.get("simple_fields", [])

        # Count non-ref fields in this child
        child_field_count = len([f for f in child_simple_fields if f["name"] != "ref"])

        # If this tab only has simple fields (no nested children), expand them directly
        if child_field_count > 0 and not child_children:
            lines.append(f"{indent}      :doc:`{tab_name} <{child_model.lower()}>`")
            lines.append("")
            lines.append(f"{indent}      **Parameters:**")
            lines.append("")

            # Generate field list
            columns = self._get_field_display_format(child_field_count)
            field_lines = self._format_field_list(
                child_simple_fields, f"{indent}      ", columns
            )
            lines.extend(field_lines)
            lines.append("")
        else:
            # Has nested children or no fields - keep original behavior
            lines.append(f"{indent}      :doc:`{tab_name} <{child_model.lower()}>`")
            lines.append("")

            # Then recurse for nested content if needed
            if child_children:
                nested_lines = []
                self._generate_hierarchy_rst_tabbed(
                    {tab_name: tab_content},
                    nested_lines,
                    0,  # Reset level for clean nesting inside tab
                    max_level - level - 1,
                )
                # Add the nested content with proper indentation
                for i, nested_line in enumerate(nested_lines):
                    if i == 0 and nested_line.startswith("**"):
                        continue  # Skip duplicate title
                    if nested_line:
                        lines.append(f"{indent}      {nested_line}")
                    else:
                        lines.append("")

        return lines

    def _generate_hierarchy_rst_tabbed(
        self, hierarchy: dict, lines: list, level: int = 0, max_level: int = 4
    ):
        """Generate RST with nested tabs for parameter groups."""
        # Process each model in the hierarchy
        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})
            simple_fields = model_info.get("simple_fields", [])

            # Skip if we're beyond max level
            if level >= max_level:
                return

            # Count non-ref fields
            field_count = len([f for f in simple_fields if f["name"] != "ref"])

            # Indent for nested content
            indent = "  " * level

            # Create the main entry with link
            # Don't add the main doc link at level 0 since it will be in tabs
            if level > 0:
                lines.append(
                    f"{indent}**{title}** - :doc:`Go to documentation <{model_ref.lower()}>`"
                )
                lines.append("")

            # If we have both simple fields and children, use tabs
            if (field_count > 0 or children) and level + 1 < max_level:
                # Determine if we need tabs
                tabs_needed = []
                if field_count > 0:
                    tabs_needed.append(("Parameters", simple_fields))
                for child_name, child_info in children.items():
                    child_title = child_info.get("title", child_name)
                    tabs_needed.append((child_title, child_info))

                # If we have multiple groups, use tabs
                if len(tabs_needed) > 1:
                    lines.append(f"{indent}.. tab-set::")
                    lines.append("")

                    for tab_name, tab_content in tabs_needed:
                        # Clean up tab name for use as a label
                        tab_label = tab_name.replace(" ", "_").replace("/", "_")

                        lines.append(f"{indent}   .. tab-item:: {tab_name}")
                        if tab_label != tab_name:
                            lines.append(f"{indent}      :name: {tab_label}")
                        lines.append("")

                        if tab_name == "Parameters":
                            # Generate parameters tab content
                            param_lines = self._generate_parameters_tab_content(
                                tab_content, indent, field_count
                            )
                            lines.extend(param_lines)
                        else:
                            # Generate nested model tab content
                            nested_lines = self._generate_nested_model_tab_content(
                                tab_name, tab_content, indent, level, max_level
                            )
                            lines.extend(nested_lines)

                elif field_count > 0:
                    # Only simple fields, no tabs needed
                    lines.append(f"{indent}**Parameters:**")
                    lines.append("")

                    columns = self._get_field_display_format(field_count)
                    field_lines = self._format_field_list(
                        simple_fields, indent, columns
                    )
                    lines.extend(field_lines)
                    lines.append("")

                elif children:
                    # Only children, recurse without tabs
                    self._generate_hierarchy_rst_tabbed(
                        children, lines, level + 1, max_level
                    )

    def _categorize_models(self) -> dict[str, list[str]]:
        """Categorize models based on their names and relationships matching the YAML hierarchy."""
        categories = {
            "root": [],
            "model_config": [],
            "site_structure": [],
            "site_properties_params": [],  # Parameters nested under site.properties
            "surfaces": [],
            "initial_states": [],
            "profiles": [],
            "utility": [],
        }

        for model_name in self.models:
            model_lower = model_name.lower()

            # Root models
            if model_name in {"Model", "Site", "SUEWSConfig"}:
                categories["root"].append(model_name)
            # Model configuration (under model: control, physics, output)
            elif model_name in {"ModelControl", "ModelPhysics", "OutputConfig"}:
                categories["model_config"].append(model_name)
            # Site structure components (direct children of site)
            elif model_name in {"SiteProperties", "LandCover", "InitialStates"}:
                categories["site_structure"].append(model_name)
            # Surface properties (nested under land_cover)
            elif "properties" in model_lower and model_name not in {
                "SiteProperties",
                "StEBBSProperties",
            }:
                categories["surfaces"].append(model_name)
            # Initial states (for surfaces)
            elif "initialstate" in model_lower:
                categories["initial_states"].append(model_name)
            # Profiles (temporal variations)
            elif "profile" in model_lower:
                categories["profiles"].append(model_name)
            # Utility types
            elif model_name in {"RefValue", "Reference"}:
                categories["utility"].append(model_name)
            # Site properties parameters (nested under site.properties like conductance, irrigation, etc.)
            # Building/thermal layers (also under site.properties)
            elif any(
                x in model_lower
                for x in (
                    "conductance",
                    "irrigation",
                    "anthropogenic",
                    "lumps",
                    "spartacus",
                    "stebbs",
                    "snow",
                    "water",
                    "storage",
                    "lai",
                    "co2",
                    "ohm",
                    "emissions",
                    "layer",
                    "thermal",
                    "vertical",
                    "params",
                    "coefficient",
                )
            ):
                categories["site_properties_params"].append(model_name)
            # Default to site properties params
            else:
                categories["site_properties_params"].append(model_name)

        # Remove empty categories
        return {k: v for k, v in categories.items() if v}


def main():
    """Run the RST generator."""
    # Parse command line arguments
    parser = argparse.ArgumentParser(
        description="Generate RST documentation for SUEWS data models"
    )
    parser.add_argument(
        "--include-internal",
        action="store_true",
        help="Include internal/developer options in documentation",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for RST files (default: docs/source/inputs/yaml/config-reference)",
    )
    parser.add_argument(
        "--save-json", type=Path, help="Save intermediate JSON to file for debugging"
    )
    parser.add_argument(
        "--load-json",
        type=Path,
        help="Load documentation from saved JSON file instead of extracting",
    )

    args = parser.parse_args()

    # Set output directory
    if args.output_dir:
        output_dir = args.output_dir
    else:
        docs_source_path = PROJECT_ROOT / "docs" / "source"
        output_dir = docs_source_path / "inputs" / "yaml" / "config-reference"

    # Extract or load documentation
    if args.load_json:
        print(f"Loading documentation from {args.load_json}")
        with open(args.load_json, encoding="utf-8") as f:
            doc_data = json.load(f)
    else:
        print("Extracting documentation from data models...")
        extractor = ModelDocExtractor()
        doc_data = extractor.extract_all_models(include_internal=args.include_internal)

        # Optionally save JSON
        if args.save_json:
            print(f"Saving documentation to {args.save_json}")
            extractor.save_json(doc_data, args.save_json)

    # Generate RST files
    print(f"Generating RST files in {output_dir}...")
    generator = RSTGenerator(doc_data)
    generator.generate_all_rst(output_dir)

    print("\nRST generation complete!")
    print(f"Files written to: {output_dir}")
    print(
        "\nNote: Remember to update any references from 'schema' to 'config-reference' in other documentation files."
    )


if __name__ == "__main__":
    main()
