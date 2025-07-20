"""
Validation feedback system for SUEWS configuration.

This module provides a more user-friendly validation feedback mechanism
that ensures users receive clear, actionable guidance about their configuration.
"""

from typing import List, Dict, Optional, Any, Union
from pydantic import ValidationError, BaseModel
import warnings
import logging
from enum import Enum
from collections import defaultdict


class ValidationLevel(Enum):
    """Severity levels for validation issues."""

    ERROR = "error"  # Configuration will not work
    WARNING = "warning"  # Configuration may work but suboptimal
    INFO = "info"  # Suggestions for improvement


class ValidationIssue:
    """Represents a single validation issue."""

    def __init__(
        self,
        level: ValidationLevel,
        location: str,
        parameter: str,
        message: str,
        fix_suggestion: Optional[str] = None,
    ):
        self.level = level
        self.location = location
        self.parameter = parameter
        self.message = message
        self.fix_suggestion = fix_suggestion

    def __str__(self):
        """Format issue for display."""
        level_symbol = {
            ValidationLevel.ERROR: "[FAILED]",
            ValidationLevel.WARNING: "[WARNING] ",
            ValidationLevel.INFO: "[TIP]",
        }[self.level]

        msg = f"{level_symbol} {self.location}: {self.message}"
        if self.fix_suggestion:
            msg += f"\n   Fix: {self.fix_suggestion}"
        return msg


class ValidationReport:
    """Collects and presents validation results."""

    def __init__(self):
        self.issues: List[ValidationIssue] = []
        self._grouped_issues: Dict[str, List[ValidationIssue]] = defaultdict(list)

    def add_issue(
        self,
        level: ValidationLevel,
        location: str,
        parameter: str,
        message: str,
        fix_suggestion: Optional[str] = None,
    ):
        """Add a validation issue to the report."""
        issue = ValidationIssue(level, location, parameter, message, fix_suggestion)
        self.issues.append(issue)
        self._grouped_issues[location].append(issue)

    def has_errors(self) -> bool:
        """Check if there are any errors."""
        return any(issue.level == ValidationLevel.ERROR for issue in self.issues)

    def has_warnings(self) -> bool:
        """Check if there are any warnings."""
        return any(issue.level == ValidationLevel.WARNING for issue in self.issues)

    def get_summary(self) -> str:
        """Get a summary of validation results."""
        error_count = sum(1 for i in self.issues if i.level == ValidationLevel.ERROR)
        warning_count = sum(
            1 for i in self.issues if i.level == ValidationLevel.WARNING
        )
        info_count = sum(1 for i in self.issues if i.level == ValidationLevel.INFO)

        parts = []
        if error_count:
            parts.append(f"{error_count} error(s)")
        if warning_count:
            parts.append(f"{warning_count} warning(s)")
        if info_count:
            parts.append(f"{info_count} suggestion(s)")

        return ", ".join(parts) if parts else "No issues found"

    def display(self, max_issues: int = 10, verbose: bool = False):
        """Display validation results in a user-friendly format."""
        if not self.issues:
            print("[PASSED] Configuration validation passed!")
            return

        print("\n" + "=" * 60)
        print("CONFIGURATION VALIDATION REPORT")
        print("=" * 60)
        print(f"\nSummary: {self.get_summary()}")

        # Group by location for better readability
        for location in sorted(self._grouped_issues.keys()):
            location_issues = self._grouped_issues[location]
            print(f"\n[LOCATION] {location}:")

            # Show errors first, then warnings, then info
            for level in [
                ValidationLevel.ERROR,
                ValidationLevel.WARNING,
                ValidationLevel.INFO,
            ]:
                level_issues = [i for i in location_issues if i.level == level]

                if verbose or level == ValidationLevel.ERROR:
                    # Show all errors, limited warnings/info
                    display_issues = level_issues
                else:
                    display_issues = level_issues[:3]

                for issue in display_issues:
                    print(f"  {issue}")

                # Show count if truncated
                if len(level_issues) > len(display_issues):
                    remaining = len(level_issues) - len(display_issues)
                    print(f"  ... and {remaining} more {level.value}(s)")

        if (
            not verbose
            and sum(len(v) for v in self._grouped_issues.values()) > max_issues
        ):
            print(f"\n[TIP] Run with --verbose to see all {len(self.issues)} issues")

        print("\n" + "=" * 60)

    def raise_if_errors(self):
        """Raise ValidationError if there are any errors."""
        if self.has_errors():
            error_messages = [
                f"{issue.location}: {issue.message}"
                for issue in self.issues
                if issue.level == ValidationLevel.ERROR
            ]
            raise ValidationError(
                f"Configuration has {len(error_messages)} error(s):\n"
                + "\n".join(error_messages)
            )


class ValidatedConfig(BaseModel):
    """Mixin class that adds validation feedback capabilities."""

    def validate_configuration(self, verbose: bool = False) -> ValidationReport:
        """
        Validate the configuration and return a detailed report.

        This method should be overridden by subclasses to implement
        specific validation logic.
        """
        report = ValidationReport()
        # Subclasses should implement validation logic here
        return report

    @classmethod
    def from_yaml_with_validation(
        cls, yaml_dict: Dict[str, Any], verbose: bool = False
    ):
        """
        Create instance from YAML dict with validation feedback.

        Args:
            yaml_dict: Dictionary from parsed YAML
            verbose: Show all validation issues

        Returns:
            Tuple of (instance, validation_report)
        """
        # First, try to create the instance
        instance = cls(**yaml_dict)

        # Then validate it
        report = instance.validate_configuration(verbose=verbose)

        # Display the report
        report.display(verbose=verbose)

        # Optionally raise if there are errors
        # report.raise_if_errors()

        return instance, report


def emit_validation_feedback(
    issues: List[Dict[str, Any]], mode: str = "warnings"
) -> None:
    """
    Emit validation feedback using the specified mode.

    Args:
        issues: List of validation issues
        mode: How to emit feedback ('warnings', 'logging', 'print')
    """
    if not issues:
        return

    if mode == "warnings":
        for issue in issues:
            warnings.warn(
                f"{issue['location']}: {issue['message']}", UserWarning, stacklevel=3
            )

    elif mode == "logging":
        logger = logging.getLogger("SUEWS.validation")
        for issue in issues:
            level = {
                "error": logging.ERROR,
                "warning": logging.WARNING,
                "info": logging.INFO,
            }.get(issue.get("level", "warning"), logging.WARNING)

            logger.log(level, f"{issue['location']}: {issue['message']}")

    elif mode == "print":
        print("\nValidation Issues:")
        for issue in issues:
            print(f"  - {issue['location']}: {issue['message']}")
