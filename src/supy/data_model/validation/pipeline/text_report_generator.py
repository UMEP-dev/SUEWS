"""
TextReportGenerator: Generate human-readable text reports from JSON structure.

This module generates text reports from the ValidationReporter's JSON structure,
ensuring that text reports maintain exactly the same format as existing reports
while being derived from structured data.
"""

from typing import Dict, List, Any, Optional
from pathlib import Path


class TextReportGenerator:
    """
    Generate human-readable text reports from JSON validation data.

    This class maintains backward compatibility with existing text report formats
    while generating them from the structured JSON data.
    """

    # Phase titles mapping
    PHASE_TITLES = {
        "A": "SUEWS - Phase A (Up-to-date YAML check) Report",
        "B": "SUEWS - Phase B (Scientific Validation) Report",
        "C": "SUEWS - Phase C (Pydantic Validation) Report",
        "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
        "AC": "SUEWS - Phase AC (Up-to-date YAML check and Pydantic Validation) Report",
        "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
        "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report",
    }

    def __init__(self):
        """Initialize the text report generator."""
        pass

    def generate(
        self, json_data: Dict[str, Any], phase: str, mode: str = "public"
    ) -> str:
        """
        Generate text report from JSON data maintaining exact current format.

        Args:
            json_data: The JSON validation data from ValidationReporter
            phase: The phase or phase combination (A, B, C, AB, AC, BC, ABC)
            mode: The validation mode (public or dev)

        Returns:
            Formatted text report as string
        """
        lines = []

        # Generate header
        title = self.PHASE_TITLES.get(phase, f"SUEWS - Phase {phase} Report")
        lines.extend(self._generate_header(title, mode))

        # Check if validation passed or failed
        has_errors = json_data["summary"]["total_errors"] > 0

        if has_errors:
            # Generate ACTION NEEDED section for errors
            action_items = self._extract_action_items(json_data, phase)
            if action_items:
                lines.append("## ACTION NEEDED")
                lines.extend(action_items)
                lines.append("")

        # Generate NO ACTION NEEDED section for info and non-critical warnings
        info_items = self._extract_info_items(json_data, phase)
        if info_items:
            lines.append("## NO ACTION NEEDED")
            lines.extend(info_items)
            lines.append("")

        # If no issues at all, add success message
        if not has_errors and not info_items:
            lines.append(f"Phase {phase} passed")
            lines.append("")

        # Add footer
        lines.append("# ==================================================")

        return "\n".join(lines)

    def _generate_header(self, title: str, mode: str) -> List[str]:
        """Generate report header lines."""
        mode_str = "Public" if mode.lower() == "public" else mode.title()

        return [
            f"# {title}",
            "# ============================================",
            f"# Mode: {mode_str}",
            "# ============================================",
            "",
        ]

    def _extract_action_items(self, json_data: Dict[str, Any], phase: str) -> List[str]:
        """Extract ACTION NEEDED items from JSON data."""
        items = []

        # Process errors by phase
        for error in json_data.get("errors", []):
            if self._should_include_in_phase(error.get("phase", ""), phase):
                items.extend(self._format_error_item(error))

        # Process critical warnings that require action
        for warning in json_data.get("warnings", []):
            if warning.get("details", {}).get("requires_action", False):
                if self._should_include_in_phase(warning.get("phase", ""), phase):
                    items.extend(self._format_warning_item(warning, is_action=True))

        return items

    def _extract_info_items(self, json_data: Dict[str, Any], phase: str) -> List[str]:
        """Extract NO ACTION NEEDED items from JSON data."""
        items = []

        # Process info items by phase
        for info in json_data.get("info", []):
            if self._should_include_in_phase(info.get("phase", ""), phase):
                items.extend(self._format_info_item(info))

        # Process non-critical warnings
        for warning in json_data.get("warnings", []):
            if not warning.get("details", {}).get("requires_action", False):
                if self._should_include_in_phase(warning.get("phase", ""), phase):
                    items.extend(self._format_warning_item(warning, is_action=False))

        return items

    def _should_include_in_phase(self, item_phase: str, report_phase: str) -> bool:
        """Check if an item should be included based on phase."""
        # If report is for combined phases, include items from any component phase
        return item_phase in report_phase

    def _format_error_item(self, error: Dict[str, Any]) -> List[str]:
        """Format an error item for the report."""
        lines = []

        # Format based on error type
        error_type = error.get("type", "")
        details = error.get("details", {})

        if error_type == "missing_required_parameter":
            field = error.get("field_path", "unknown")
            lines.append(f"- Missing required parameter: {field}")
            if error.get("suggestions"):
                for suggestion in error["suggestions"]:
                    lines.append(f"  Suggested fix: {suggestion}")

        elif error_type == "science_validation_error":
            message = error.get("message", "Validation error")
            lines.append(f"- {message}")
            if "physics_constraint" in details:
                lines.append(f"  Constraint violated: {details['physics_constraint']}")

        elif error_type == "pydantic_validation_error":
            field = error.get("field_path", "unknown")
            message = error.get("message", "Validation error")
            lines.append(f"- Validation error at {field}: {message}")

        else:
            # Generic error formatting
            message = error.get("message", "Validation error")
            lines.append(f"- {message}")
            if error.get("field_path"):
                lines.append(f"  Field: {error['field_path']}")

        return lines

    def _format_warning_item(
        self, warning: Dict[str, Any], is_action: bool
    ) -> List[str]:
        """Format a warning item for the report."""
        lines = []

        warning_type = warning.get("type", "")
        details = warning.get("details", {})

        if warning_type == "parameter_renamed":
            old_name = details.get("old_name", "unknown")
            new_name = details.get("new_name", "unknown")
            lines.append(f"- Parameter renamed: {old_name} → {new_name}")

        elif warning_type == "missing_optional_parameter":
            field = warning.get("field_path", "unknown")
            default = details.get("default_value", "default")
            lines.append(
                f"- Optional parameter {field} missing, using default: {default}"
            )

        elif warning_type == "science_warning":
            message = warning.get("message", "Warning")
            lines.append(f"- Scientific warning: {message}")

        else:
            # Generic warning formatting
            message = warning.get("message", "Warning")
            prefix = "- WARNING: " if is_action else "- "
            lines.append(f"{prefix}{message}")

        return lines

    def _format_info_item(self, info: Dict[str, Any]) -> List[str]:
        """Format an info item for the report."""
        lines = []

        info_type = info.get("type", "")
        details = info.get("details", {})

        if info_type == "parameter_updated":
            field = info.get("field_path", "unknown")
            old_val = details.get("old_value", "")
            new_val = details.get("new_value", "")
            lines.append(f"- Updated {field}: {old_val} → {new_val}")

        elif info_type == "auto_correction":
            field = info.get("field_path", "unknown")
            message = info.get("message", "Auto-corrected")
            lines.append(f"- {message} for {field}")

        elif info_type == "default_applied":
            field = info.get("field_path", "unknown")
            default = details.get("default_value", "default")
            lines.append(f"- {field} found null/missing, using default: {default}")

        else:
            # Generic info formatting
            message = info.get("message", "Information")
            lines.append(f"- {message}")

        return lines

    def save_text_report(
        self,
        json_data: Dict[str, Any],
        filepath: Path,
        phase: str,
        mode: str = "public",
    ) -> None:
        """
        Generate and save text report to file.

        Args:
            json_data: The JSON validation data
            filepath: Path to save the text report
            phase: The phase or phase combination
            mode: The validation mode
        """
        text_report = self.generate(json_data, phase, mode)

        with open(filepath, "w") as f:
            f.write(text_report)
