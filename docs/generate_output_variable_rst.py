"""
SUEWS Output Variable RST Generator.

This script generates reStructuredText (.rst) files for SUEWS output variables.
It uses the OutputVariableRegistry to extract variable documentation and formats it as RST.

To run this script, navigate to the `docs/` directory and execute:
    python generate_output_variable_rst.py
"""

import argparse
import re
import sys
from pathlib import Path
from typing import Any

# Import supy - it must be installed (via make dev) for docs to build
PROJECT_ROOT = Path(__file__).resolve().parent.parent

try:
    from supy.data_model.output import OUTPUT_REGISTRY
    from supy.data_model.output.variables import (
        OutputGroup,
        AggregationMethod,
    )
except ImportError as e:
    print(
        "ERROR: Cannot import supy. Documentation build requires supy to be installed.",
        file=sys.stderr,
    )
    print("Please run: make dev", file=sys.stderr)
    print(f"Import error: {e}", file=sys.stderr)
    sys.exit(1)


class OutputVariableRSTGenerator:
    """Generate RST documentation from output variable registry."""

    def __init__(self):
        """Initialize the RST generator."""
        self.registry = OUTPUT_REGISTRY

    def generate_all_rst(self, output_dir: Path, style: str = "tabbed") -> None:
        """
        Generate RST files for all output variable groups.

        Args:
            output_dir: Directory to write RST files to
            style: Style to use ("simple", "tabbed", "dropdown")
        """
        output_dir.mkdir(parents=True, exist_ok=True)

        # Generate RST for each output group
        for group in OutputGroup:
            variables = self.registry.by_group(group)
            if not variables:
                continue

            rst_content = self._format_group(group, variables)
            rst_file = output_dir / f"{group.value.lower()}.rst"

            with open(rst_file, "w", encoding="utf-8") as f:
                f.write(rst_content)

            print(f"Generated: {rst_file}")

        # Generate index file
        if style == "simple":
            index_content = self._generate_index_simple()
        elif style == "dropdown":
            index_content = self._generate_index_dropdown()
        else:  # tabbed (default)
            index_content = self._generate_index_tabbed()

        index_file = output_dir / "index.rst"
        with open(index_file, "w", encoding="utf-8") as f:
            f.write(index_content)
        print(f"Generated: {index_file} (style: {style})")

        print(f"\nGenerated documentation for {len(list(OutputGroup))} output groups")
        print(f"Total variables documented: {len(self.registry.variables)}")

    def _format_group(self, group: OutputGroup, variables: list) -> str:
        """Format a single output group as RST."""
        lines = []

        # Add meta tags
        group_name = group.value
        lines.extend([
            ".. meta::",
            f"   :description: SUEWS {group_name} output variables",
            f"   :keywords: SUEWS, output, {group_name.lower()}, variables",
            "",
        ])

        # Add reference label and index
        group_lower = group_name.lower()
        lines.extend([
            f".. _{group_lower}_output:",
            "",
            ".. index::",
            f"   single: {group_name} (output group)",
            f"   single: Output; {group_name}",
            "",
        ])

        # Add title
        title = f"{group_name} Output Variables"
        lines.append(title)
        lines.append("=" * len(title))
        lines.append("")

        # Add group description
        group_desc = self._get_group_description(group)
        if group_desc:
            lines.append(group_desc)
            lines.append("")

        # Add variable count summary
        lines.append(f"This group contains {len(variables)} output variables.")
        lines.append("")

        # Sort variables by name
        sorted_vars = sorted(variables, key=lambda v: v.name)

        # Format each variable
        for var in sorted_vars:
            lines.extend(self._format_variable(var))
            lines.append("")  # Blank line between variables

        return "\n".join(lines)

    def _format_variable(self, var: Any) -> list[str]:
        """Format a single variable as RST."""
        lines = []

        # Get group value (handle both enum and string)
        group_val = var.group.value if hasattr(var.group, "value") else var.group

        # Add index entries
        lines.extend([
            ".. index::",
            f"   single: {var.name} (output variable)",
            f"   single: {group_val}; {var.name}",
            "",
        ])

        # Use yaml:option directive for consistency with input docs
        lines.append(f".. yaml:option:: {var.name}")
        lines.append("")

        # Add description
        if var.description:
            lines.append(f"   {var.description}")
            lines.append("")

        # Add metadata
        # Skip Unit for datetime variables (they're temporal indices, not physical quantities)
        is_datetime = group_val.lower() == "datetime"

        # Unit
        if var.unit and not is_datetime:
            formatted_unit = self._format_unit(var.unit)
            lines.append(f"   :Unit: {formatted_unit}")

        # Aggregation method
        agg_desc = self._get_aggregation_description(var.aggregation)
        lines.append(f"   :Aggregation: {agg_desc}")

        return lines

    @staticmethod
    def _format_unit(unit: str) -> str:
        """Format units for RST display."""
        if not unit:
            return unit

        # Normalise hyphen-style exponents to caret-style for consistent processing
        # Convert patterns like "m-2" to "m^-2", "s-1" to "s^-1", "m2" to "m^2", etc.
        # Match letter followed by hyphen and digit(s) for negative exponents
        unit = re.sub(r"([a-zA-Z])(\d*)-(\d+)", r"\1\2^-\3", unit)
        # Match letter followed directly by digit(s) for positive exponents (e.g., m2 -> m^2)
        unit = re.sub(r"([a-zA-Z])(\d+)(?!\^)", r"\1^\2", unit)

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

                numerator = OutputVariableRSTGenerator._process_unit_part(numerator)
                denominator = OutputVariableRSTGenerator._process_unit_part(denominator)

                return f"{numerator} {denominator}"

        return OutputVariableRSTGenerator._process_unit_part(unit)

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
                "^" in word
                and any(
                    word.startswith(b) for b in ["m", "s", "kg", "K", "W", "h", "d"]
                )
            ):
                formatted.append(f"|{word}|")
            else:
                formatted.append(word)

        return " ".join(formatted)

    @staticmethod
    def _get_aggregation_description(method: Any) -> str:
        """Get human-readable description of aggregation method."""
        # Handle both enum and string values
        if isinstance(method, str):
            method_val = method
        else:
            method_val = method.value if hasattr(method, "value") else str(method)

        descriptions = {
            "T": "Time (timestamp only, no aggregation)",
            "A": "Average (mean over period)",
            "S": "Sum (cumulative over period)",
            "L": "Last (final value in period)",
        }
        return descriptions.get(method_val, str(method))


    @staticmethod
    def _get_group_description(group: Any) -> str:
        """Get description for an output group."""
        # Handle both enum and string values
        if isinstance(group, str):
            group_val = group
        else:
            group_val = group.value if hasattr(group, "value") else str(group)

        descriptions = {
            "datetime": "Date and time information for output records.",
            "SUEWS": "Core SUEWS energy balance, water balance, and meteorological outputs.",
            "snow": "Snow-specific outputs for each surface type.",
            "ESTM": "Element Surface Temperature Model outputs.",
            "EHC": "Element Heat Capacity model outputs for building thermal layers.",
            "RSL": "Roughness Sublayer vertical profile outputs.",
            "BL": "Boundary Layer model outputs.",
            "debug": "Debug and diagnostic outputs for model development.",
            "BEERS": "BEERS radiation model outputs.",
            "DailyState": "Daily accumulated state variables.",
            "SPARTACUS": "SPARTACUS radiation model outputs (experimental).",
            "STEBBS": "STEBBS model outputs (experimental).",
            "NHood": "Neighbourhood-scale outputs (experimental).",
        }
        return descriptions.get(group_val, "")

    def _generate_index_simple(self) -> str:
        """Generate simple index with nested lists."""
        lines = [
            ".. _output_variable_reference:",
            "",
            "Output Variable Reference",
            "=========================",
            "",
            "This reference documents all SUEWS output variables organised by output group.",
            "",
            "Output Groups",
            "-------------",
            "",
        ]

        # List all groups
        for group in OutputGroup:
            variables = self.registry.by_group(group)
            if not variables:
                continue

            group_name = group.value
            lines.append(
                f"* :doc:`{group_name} <{group_name.lower()}>` - {len(variables)} variables"
            )
            lines.append("")

        # Add toctree
        lines.extend([
            "",
            ".. toctree::",
            "   :hidden:",
            "   :maxdepth: 2",
            "",
        ])

        for group in OutputGroup:
            if self.registry.by_group(group):
                lines.append(f"   {group.value.lower()}")

        return "\n".join(lines)

    def _generate_index_dropdown(self) -> str:
        """Generate index with dropdown sections."""
        lines = [
            ".. _output_variable_reference:",
            "",
            "Output Variable Reference",
            "=========================",
            "",
            "This reference documents all SUEWS output variables organised by output group.",
            "",
            "Click on each group to see the available variables.",
            "",
        ]

        # Create dropdown for each group
        for group in OutputGroup:
            variables = self.registry.by_group(group)
            if not variables:
                continue

            group_name = group.value
            group_desc = self._get_group_description(group)

            lines.append(f".. dropdown:: {group_name} ({len(variables)} variables)")
            lines.append("   :class-container: sd-shadow-sm")
            lines.append("   :chevron: down-up")
            lines.append("")

            if group_desc:
                lines.append(f"   {group_desc}")
                lines.append("")

            lines.append(
                f"   :doc:`View {group_name} variables <{group_name.lower()}>`"
            )
            lines.append("")

        # Add toctree
        lines.extend([
            "",
            ".. toctree::",
            "   :hidden:",
            "   :maxdepth: 2",
            "",
        ])

        for group in OutputGroup:
            if self.registry.by_group(group):
                lines.append(f"   {group.value.lower()}")

        return "\n".join(lines)

    def _generate_index_tabbed(self) -> str:
        """Generate index with tabbed interface."""
        lines = [
            ".. _output_variable_reference:",
            "",
            "Output Variable Reference",
            "=========================",
            "",
            "This reference documents all SUEWS output variables organised by output group.",
            "",
            ".. tab-set::",
            "",
        ]

        # Create tab for each group
        for group in OutputGroup:
            variables = self.registry.by_group(group)
            if not variables:
                continue

            group_name = group.value
            group_desc = self._get_group_description(group)

            lines.append(f"   .. tab-item:: {group_name}")
            lines.append("")

            if group_desc:
                lines.append(f"      {group_desc}")
                lines.append("")

            lines.append(
                f"      **{len(variables)} variables** - :doc:`View details <{group_name.lower()}>`"
            )
            lines.append("")

        # Add toctree
        lines.extend([
            "",
            ".. toctree::",
            "   :hidden:",
            "   :maxdepth: 2",
            "",
        ])

        for group in OutputGroup:
            if self.registry.by_group(group):
                lines.append(f"   {group.value.lower()}")

        return "\n".join(lines)


def main():
    """Run the RST generator."""
    parser = argparse.ArgumentParser(
        description="Generate RST documentation for SUEWS output variables"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for RST files (default: docs/source/outputs/variables)",
    )
    parser.add_argument(
        "--style",
        choices=["simple", "dropdown", "tabbed"],
        default="tabbed",
        help="Style to use for index page (default: tabbed)",
    )

    args = parser.parse_args()

    # Set output directory
    if args.output_dir:
        output_dir = args.output_dir
    else:
        docs_source_path = PROJECT_ROOT / "docs" / "source"
        output_dir = docs_source_path / "outputs" / "variables"

    # Generate RST files
    print(f"Generating RST files in {output_dir}...")
    print(f"Style: {args.style}")
    generator = OutputVariableRSTGenerator()
    generator.generate_all_rst(output_dir, style=args.style)

    print("\nRST generation complete!")
    print(f"Files written to: {output_dir}")


if __name__ == "__main__":
    main()
