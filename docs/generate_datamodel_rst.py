"""
SUEWS Data Model RST Generator.

This script generates reStructuredText (.rst) files for the SUEWS Pydantic data models.
It uses the doc_utils module to extract model documentation as JSON, then formats it as RST.

To run this script, navigate to the `docs/` directory and execute:
    python generate_datamodel_rst.py
"""

import argparse
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

# Add the project root to sys.path to allow importing supy
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SRC_PATH = PROJECT_ROOT / "src"
sys.path.insert(0, str(SRC_PATH))

# Import the documentation extractor through supy
from supy.data_model.doc_utils import ModelDocExtractor


class RSTGenerator:
    """Generate RST documentation from extracted model documentation."""

    def __init__(self, doc_data: Dict[str, Any]):
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

        print(f"\nGenerated {len(self.models)} RST files + index in {output_dir}")
        print(f"Total fields documented: {self.metadata.get('total_fields', 0)}")

    def _format_model(self, model_name: str, model_doc: Dict[str, Any]) -> str:
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

    def _format_meta_tags(
        self, model_name: str, model_doc: Dict[str, Any]
    ) -> List[str]:
        """Format meta tags for SEO."""
        title = model_doc.get("title", model_name)
        return [
            ".. meta::",
            f"   :description: SUEWS YAML configuration for {title.lower()} parameters",
            f"   :keywords: SUEWS, YAML, {model_name.lower()}, parameters, configuration",
            "",
        ]

    def _format_reference_and_index(self, model_name: str) -> List[str]:
        """Format reference label and index entries."""
        return [
            f".. _{model_name.lower()}:",
            "",
            ".. index::",
            f"   single: {model_name} (YAML parameter)",
            f"   single: YAML; {model_name}",
            "",
        ]

    def _format_field(self, field_doc: Dict[str, Any], model_name: str) -> List[str]:
        """Format a single field as RST."""
        lines = []
        field_name = field_doc["name"]

        # Add index entries
        lines.extend([
            ".. index::",
            f"   single: {field_name} (YAML parameter)",
            f"   single: {model_name}; {field_name}",
            "",
        ])

        # Add reference label for special method fields
        if field_name in {
            "diagmethod",
            "stabilitymethod",
            "localclimatemethod",
            "gsmodel",
        }:
            lines.append(f".. _{field_name}:")
            lines.append("")

        # Start option block
        lines.append(f".. option:: {field_name}")
        lines.append("")

        # Add description
        description = field_doc.get("description", "")
        if description:
            # Parse for method options if present
            if field_doc.get("options"):
                # Extract main description before options
                if "Options:" in description:
                    main_desc = description.split("Options:", 1)[0].strip()
                    lines.append(f"   {main_desc}")
                else:
                    lines.append(f"   {description}")
            else:
                lines.append(f"   {description}")
            lines.append("")

        # Handle RefValue types - add YAML structure hint
        type_info = field_doc.get("type_info", {})
        if type_info.get("is_ref_value"):
            nested_model = field_doc.get("nested_model")
            if nested_model:
                lines.append(
                    f"   In YAML, this is typically specified using a ``value`` key, e.g.: "
                )
                lines.append(f"   ``{field_name}: {{value: ...}}``.")
                lines.append(
                    f"   The structure of this ``value`` is detailed in the linked section below."
                )
            else:
                type_str = field_doc.get("type", "...")
                lines.append(
                    f"   In YAML, this is typically specified using a ``value`` key, e.g.: "
                )
                lines.append(f"   ``{field_name}: {{value: {type_str}}}``.")
            lines.append("")

        # Add options if present
        if field_doc.get("options"):
            lines.append("   :Options:")
            for opt in field_doc["options"]:
                opt_str = self._format_option(opt)
                lines.append(f"      | {opt_str}")
            lines.append("")

        # Add unit
        unit = field_doc.get("unit")
        if unit and not field_doc.get("options"):  # Don't show units for enum fields
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

        # Add link to nested model documentation
        nested_model = field_doc.get("nested_model")
        if nested_model:
            lines.append("")
            lines.append(
                self._format_nested_model_link(field_name, nested_model, type_info)
            )

        return lines

    def _format_option(self, option: Dict[str, Any]) -> str:
        """Format a single option."""
        value = option["value"]
        name = option["name"]
        desc = option["description"]

        # Apply formatting to description
        desc = self._format_option_description(desc)

        return f"``{value}`` ({name}) = {desc}"

    def _format_option_description(self, desc: str) -> str:
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

    def _process_unit_part(self, part: str) -> str:
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
            if word in known_patterns:
                formatted.append(f"|{word}|")
            elif "^" in word and any(
                word.startswith(b) for b in ["m", "s", "kg", "K", "W", "h", "d"]
            ):
                formatted.append(f"|{word}|")
            else:
                formatted.append(word)

        return " ".join(formatted)

    def _format_default(self, field_doc: Dict[str, Any]) -> tuple[str, str]:
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

    def _format_constraints(self, constraints: Dict[str, Any]) -> str:
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

    def _format_nested_model_link(
        self, field_name: str, nested_model: str, type_info: Dict[str, Any]
    ) -> str:
        """Format link to nested model documentation."""
        nested_lower = nested_model.lower()

        # Different messages based on type
        if type_info.get("is_ref_value"):
            return (
                f"   The structure for the ``value`` key of ``{field_name}`` "
                f"is detailed in :doc:`{nested_lower}`."
            )
        elif type_info.get("is_list"):
            if nested_model == "SurfaceInitialState":
                return (
                    f"   For ``{field_name}``, a list of generic SurfaceInitialState "
                    f"objects is used to specify initial conditions for each layer - "
                    f"see :doc:`surfaceinitialstate` for details."
                )
            else:
                return (
                    f"   Each item in the ``{field_name}`` list must conform to the "
                    f":doc:`{nested_lower}` structure."
                )
        elif type_info.get("is_dict"):
            return (
                f"   Each value in the ``{field_name}`` mapping must conform to the "
                f":doc:`{nested_lower}` structure."
            )
        else:
            # Direct nesting
            if (
                nested_model.startswith("InitialState")
                and nested_model != "InitialStates"
            ):
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
                else:
                    return (
                        f"   For ``{field_name}``, one vegetation-specific initial state "
                        f"with additional parameters is used - "
                        f"see :doc:`{nested_lower}` for details."
                    )
            else:
                return (
                    f"   The ``{field_name}`` parameter group is defined by the "
                    f":doc:`{nested_lower}` structure."
                )

    def _process_model_physics_description(self, description: str) -> str:
        """Add cross-references to ModelPhysics description."""
        lines = description.split("\n")
        processed = []

        for line in lines:
            # Add cross-references to method names
            if "- diagmethod:" in line:
                line = line.replace(
                    "- diagmethod:", "- :ref:`diagmethod <diagmethod>`:"
                )
                line = line.replace(
                    "diagmethod calculations", "``diagmethod`` calculations"
                )
            elif "- stabilitymethod:" in line:
                line = line.replace(
                    "- stabilitymethod:", "- :ref:`stabilitymethod <stabilitymethod>`:"
                )
                line = line.replace("BY diagmethod", "**BY** ``diagmethod``")
            elif "- localclimatemethod:" in line:
                line = line.replace(
                    "- localclimatemethod:",
                    "- :ref:`localclimatemethod <localclimatemethod>`:",
                )
                line = line.replace("FROM diagmethod", "**FROM** ``diagmethod``")
            elif "- gsmodel:" in line:
                line = line.replace("- gsmodel:", "- :ref:`gsmodel <gsmodel>`:")
                line = line.replace(
                    "localclimatemethod adjustments",
                    "``localclimatemethod`` adjustments",
                )

            # Add bold for emphasis
            line = line.replace(" HOW ", " **HOW** ")

            processed.append(line)

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

        # Check if sphinx-design is available
        sphinx_design_available = self._check_sphinx_design()

        # Build the hierarchical structure from the models
        hierarchy = self._build_hierarchy()

        # Generate RST from hierarchy (with or without dropdowns)
        if sphinx_design_available:
            lines.append(".. note::")
            lines.append(
                "   Click on any section below to expand and see its parameters."
            )
            lines.append("")
            self._generate_hierarchy_rst_dropdown(hierarchy, lines, level=0)
        else:
            self._generate_hierarchy_rst(hierarchy, lines, level=0)

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

    def _check_sphinx_design(self) -> bool:
        """Check if sphinx-design extension is available."""
        try:
            import sphinx_design

            return True
        except ImportError:
            return False

    def _build_hierarchy(self) -> dict:
        """Build hierarchical structure from model relationships."""
        hierarchy = {}

        # Start with SUEWSConfig as root
        if "SUEWSConfig" in self.models:
            suews_fields = self.models["SUEWSConfig"].get("fields", [])
            hierarchy["SUEWSConfig"] = {
                "title": self.models["SUEWSConfig"].get("title", "SUEWS Config"),
                "children": {},
            }

            # Find model and sites fields
            for field in suews_fields:
                if field["name"] == "model" and field.get("nested_model") == "Model":
                    hierarchy["SUEWSConfig"]["children"]["Model"] = (
                        self._get_model_children("Model", depth=1)
                    )
                elif field["name"] == "sites":
                    # Sites is a list of Site objects
                    hierarchy["SUEWSConfig"]["children"]["Sites"] = {
                        "title": "Sites",
                        "model": "Site",
                        "children": self._get_model_children("Site", depth=1)[
                            "children"
                        ],
                    }

        return hierarchy

    def _get_model_children(self, model_name: str, depth: int = 0) -> dict:
        """Get children of a model based on its fields."""
        if model_name not in self.models:
            return {"title": model_name, "children": {}}

        model_data = self.models[model_name]
        result = {
            "title": model_data.get("title", model_name),
            "model": model_name,
            "children": {},
        }

        # Stop expanding at certain depth or for profile/utility types
        profile_types = {"DayProfile", "HourlyProfile", "WeeklyProfile"}
        utility_types = {"Reference", "RefValue"}

        # Don't expand profile types or utility types or if we're too deep
        if model_name in profile_types or model_name in utility_types or depth > 3:
            return result

        # Look for nested models in fields
        for field in model_data.get("fields", []):
            nested_model = field.get("nested_model")
            # Skip Reference and RefValue as they're utility types
            if (
                nested_model
                and nested_model in self.models
                and nested_model not in utility_types
            ):
                # For profile types, just note the field name without expanding
                if nested_model in profile_types:
                    # Don't expand profiles, just show field exists
                    continue
                else:
                    # Use field name as key, get nested model structure
                    field_key = field["name"]
                    result["children"][field_key] = self._get_model_children(
                        nested_model, depth + 1
                    )

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

    def _generate_hierarchy_rst_dropdown(
        self, hierarchy: dict, lines: list, level: int = 0
    ):
        """Generate RST from hierarchy using sphinx-design dropdowns."""
        for model_name, model_info in hierarchy.items():
            title = model_info.get("title", model_name)
            model_ref = model_info.get("model", model_name)
            children = model_info.get("children", {})

            # Indent for nested dropdowns
            indent = "   " * level

            # Create dropdown for items with children
            if children:
                lines.append(f"{indent}.. dropdown:: {title}")
                lines.append(f"{indent}   :open:")
                lines.append(f"{indent}   :animate: fade-in")
                lines.append("")
                lines.append(f"{indent}   :doc:`{model_ref.lower()}`")
                lines.append("")

                # Process children with increased indentation
                self._generate_hierarchy_rst_dropdown(children, lines, level + 1)
            else:
                # Leaf node - just show title and link
                lines.append(f"{indent}**{title}**")
                lines.append(f"{indent}:doc:`{model_ref.lower()}`")
                lines.append("")

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
            elif any(
                x in model_lower
                for x in {
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
                }
            ):
                categories["site_properties_params"].append(model_name)
            # Building/thermal layers (also under site.properties)
            elif any(x in model_lower for x in {"layer", "thermal", "vertical"}):
                categories["site_properties_params"].append(model_name)
            # Other parameters
            elif "params" in model_lower or "coefficient" in model_lower:
                categories["site_properties_params"].append(model_name)
            # Default to site properties params
            else:
                categories["site_properties_params"].append(model_name)

        # Remove empty categories
        return {k: v for k, v in categories.items() if v}


def main():
    """Main entry point for the RST generator."""
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
        import json

        with open(args.load_json, "r") as f:
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
