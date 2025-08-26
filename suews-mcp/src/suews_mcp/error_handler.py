"""Intelligent error handling and diagnosis for SUEWS MCP Server.

Provides context-aware error messages, root cause analysis, and actionable suggestions
for common SUEWS configuration and simulation issues.
"""

import logging
import re
import traceback
from enum import Enum
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)


class ErrorCategory(Enum):
    """Categories of SUEWS-related errors."""

    CONFIGURATION = "configuration"
    DATA_QUALITY = "data_quality"
    FILE_IO = "file_io"
    SIMULATION = "simulation"
    PHYSICS = "physics"
    MEMORY = "memory"
    DEPENDENCY = "dependency"
    NETWORK = "network"
    UNKNOWN = "unknown"


class ErrorSeverity(Enum):
    """Severity levels for errors."""

    CRITICAL = "critical"  # Simulation cannot proceed
    ERROR = "error"  # Will cause incorrect results
    WARNING = "warning"  # May affect results
    INFO = "info"  # Informational only


@dataclass
class ErrorContext:
    """Context information for error diagnosis."""

    operation: str  # What operation was being performed
    file_path: Optional[Path] = None
    config_section: Optional[str] = None
    variable_name: Optional[str] = None
    value: Optional[Any] = None
    line_number: Optional[int] = None
    additional_info: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DiagnosedError:
    """A diagnosed error with suggestions for resolution."""

    category: ErrorCategory
    severity: ErrorSeverity
    message: str
    root_cause: str
    suggestions: List[str]
    context: ErrorContext
    original_error: Optional[Exception] = None
    documentation_links: List[str] = field(default_factory=list)
    related_errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict:
        """Convert to dictionary for JSON serialization."""
        return {
            "category": self.category.value,
            "severity": self.severity.value,
            "message": self.message,
            "root_cause": self.root_cause,
            "suggestions": self.suggestions,
            "context": {
                "operation": self.context.operation,
                "file_path": str(self.context.file_path)
                if self.context.file_path
                else None,
                "config_section": self.context.config_section,
                "variable_name": self.context.variable_name,
                "value": str(self.context.value) if self.context.value else None,
                "line_number": self.context.line_number,
                **self.context.additional_info,
            },
            "documentation_links": self.documentation_links,
            "related_errors": self.related_errors,
        }

    def format_for_user(self, verbose: bool = False) -> str:
        """Format error for user display."""
        lines = [
            f"\n{'='*60}",
            f"❌ {self.severity.value.upper()}: {self.message}",
            f"{'='*60}\n",
            f"📁 Category: {self.category.value}",
            f"🔍 Root Cause: {self.root_cause}\n",
        ]

        if self.context.file_path:
            lines.append(f"📄 File: {self.context.file_path}")
        if self.context.line_number:
            lines.append(f"📍 Line: {self.context.line_number}")
        if self.context.variable_name:
            lines.append(f"🔤 Variable: {self.context.variable_name}")
        if self.context.value is not None:
            lines.append(f"📊 Value: {self.context.value}")

        if self.suggestions:
            lines.append("\n💡 Suggestions:")
            for i, suggestion in enumerate(self.suggestions, 1):
                lines.append(f"  {i}. {suggestion}")

        if self.documentation_links:
            lines.append("\n📚 Documentation:")
            for link in self.documentation_links:
                lines.append(f"  • {link}")

        if verbose and self.related_errors:
            lines.append("\n🔗 Related Issues:")
            for error in self.related_errors:
                lines.append(f"  • {error}")

        if verbose and self.original_error:
            lines.append(f"\n🐛 Original Error: {str(self.original_error)}")

        lines.append(f"{'='*60}\n")
        return "\n".join(lines)


class ErrorPatternMatcher:
    """Matches error patterns to known issues."""

    def __init__(self):
        self.patterns = self._initialize_patterns()

    def _initialize_patterns(self) -> List[Tuple[re.Pattern, Dict[str, Any]]]:
        """Initialize error pattern database."""
        return [
            # File not found errors
            (
                re.compile(
                    r"(FileNotFoundError|No such file|cannot find).*?([\w/\\.-]+)",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.FILE_IO,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "File or directory does not exist",
                    "suggestions": [
                        "Check if the file path is correct",
                        "Ensure the file exists in the specified location",
                        "Verify you have read permissions for the file",
                        "Use absolute paths instead of relative paths for clarity",
                    ],
                },
            ),
            # Surface fraction errors
            (
                re.compile(
                    r"surface.*fraction.*sum.*not.*equal.*1|fraction.*validation.*failed",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.CONFIGURATION,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "Surface fractions do not sum to 1.0",
                    "suggestions": [
                        "Ensure all surface fractions (paved, buildings, vegetation, water, soil) sum to exactly 1.0",
                        "Check for rounding errors in fraction values",
                        "Verify no surface fraction is negative",
                        "Consider using the normalize_fractions utility function",
                    ],
                    "documentation_links": [
                        "https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo.html#surface-fractions"
                    ],
                },
            ),
            # Missing required columns
            (
                re.compile(
                    r"missing.*required.*column|column.*not.*found|KeyError.*['\"]([\w_]+)['\"]",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.DATA_QUALITY,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "Required data column is missing",
                    "suggestions": [
                        "Check if all required columns are present in your forcing data",
                        "Verify column names match expected format (case-sensitive)",
                        "Use the validate_forcing_data tool to check data completeness",
                        "Refer to the forcing data documentation for required columns",
                    ],
                    "documentation_links": [
                        "https://suews.readthedocs.io/en/latest/input_files/met_input.html"
                    ],
                },
            ),
            # Time series gaps
            (
                re.compile(
                    r"time.*gap|missing.*data.*points|discontinuous.*time.*series",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.DATA_QUALITY,
                    "severity": ErrorSeverity.WARNING,
                    "diagnosis": "Gaps detected in time series data",
                    "suggestions": [
                        "Use gap-filling methods for small gaps (< 2 hours)",
                        "Consider using ERA5 or similar reanalysis data for filling",
                        "Check if data logger had failures during the gap periods",
                        "For critical variables, consider interpolation or statistical filling",
                    ],
                },
            ),
            # Negative values in physical variables
            (
                re.compile(
                    r"negative.*(radiation|pressure|wind|humidity)|invalid.*physical.*value",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.DATA_QUALITY,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "Physically impossible values detected",
                    "suggestions": [
                        "Check for sensor calibration issues",
                        "Verify unit conversions are correct",
                        "Look for data corruption during transfer or storage",
                        "Apply quality control filters to remove bad data points",
                        "Consider using the data_quality_check tool",
                    ],
                },
            ),
            # Memory errors
            (
                re.compile(
                    r"MemoryError|out of memory|cannot allocate|memory allocation failed",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.MEMORY,
                    "severity": ErrorSeverity.CRITICAL,
                    "diagnosis": "Insufficient memory for simulation",
                    "suggestions": [
                        "Reduce the simulation period or spatial domain",
                        "Close other applications to free up memory",
                        "Consider using a machine with more RAM",
                        "Process data in smaller chunks",
                        "Check for memory leaks in long-running simulations",
                    ],
                },
            ),
            # Stability issues
            (
                re.compile(
                    r"numerical.*instability|solution.*diverged|NaN.*detected|overflow",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.SIMULATION,
                    "severity": ErrorSeverity.CRITICAL,
                    "diagnosis": "Numerical instability in model calculations",
                    "suggestions": [
                        "Reduce the time step for better numerical stability",
                        "Check input data for extreme or unrealistic values",
                        "Verify initial conditions are within reasonable ranges",
                        "Review recent changes to configuration parameters",
                        "Consider using more conservative physics options",
                    ],
                },
            ),
            # Python version issues
            (
                re.compile(
                    r"unsupported.*python.*version|requires.*python|python.*compatibility",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.DEPENDENCY,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "Python version incompatibility",
                    "suggestions": [
                        "Check if you're using Python 3.9 or later",
                        "Create a virtual environment with the correct Python version",
                        "Update your Python installation if needed",
                        "Use conda/mamba to manage Python versions",
                    ],
                },
            ),
            # SuPy import errors
            (
                re.compile(
                    r"ImportError.*supy|No module.*supy|cannot import.*supy",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.DEPENDENCY,
                    "severity": ErrorSeverity.CRITICAL,
                    "diagnosis": "SuPy is not installed or not accessible",
                    "suggestions": [
                        "Install SuPy using: pip install supy",
                        "Ensure you're in the correct Python environment",
                        "Check if SuPy is installed: pip list | grep supy",
                        "Try reinstalling SuPy if the installation is corrupted",
                    ],
                    "documentation_links": [
                        "https://supy.readthedocs.io/en/latest/installation.html"
                    ],
                },
            ),
            # Configuration version mismatch
            (
                re.compile(
                    r"version.*mismatch|incompatible.*version|outdated.*configuration",
                    re.IGNORECASE,
                ),
                {
                    "category": ErrorCategory.CONFIGURATION,
                    "severity": ErrorSeverity.ERROR,
                    "diagnosis": "Configuration file version incompatibility",
                    "suggestions": [
                        "Check the SUEWS version you're using",
                        "Update configuration files to match the current version",
                        "Use the configuration migration tool if available",
                        "Refer to the changelog for breaking changes",
                    ],
                    "documentation_links": [
                        "https://suews.readthedocs.io/en/latest/version-history.html"
                    ],
                },
            ),
        ]

    def match(self, error_text: str) -> Optional[Dict[str, Any]]:
        """Match error text against known patterns."""
        for pattern, info in self.patterns:
            match = pattern.search(error_text)
            if match:
                return {
                    **info,
                    "matched_text": match.group(0),
                    "captured_groups": match.groups(),
                }
        return None


class SUEWSErrorHandler:
    """Main error handler for SUEWS MCP operations."""

    def __init__(self):
        self.pattern_matcher = ErrorPatternMatcher()
        self.error_history: List[DiagnosedError] = []
        self.max_history = 100

    def diagnose(
        self, error: Exception, context: ErrorContext, include_traceback: bool = False
    ) -> DiagnosedError:
        """Diagnose an error and provide actionable suggestions."""

        error_text = str(error)
        error_type = type(error).__name__

        # Try pattern matching first
        pattern_match = self.pattern_matcher.match(error_text)

        if pattern_match:
            diagnosed = self._create_from_pattern(error, context, pattern_match)
        else:
            diagnosed = self._create_generic_diagnosis(error, context)

        # Add traceback if requested
        if include_traceback:
            diagnosed.context.additional_info["traceback"] = traceback.format_exc()

        # Store in history
        self._add_to_history(diagnosed)

        # Check for related errors
        diagnosed.related_errors = self._find_related_errors(diagnosed)

        return diagnosed

    def _create_from_pattern(
        self, error: Exception, context: ErrorContext, pattern_info: Dict
    ) -> DiagnosedError:
        """Create diagnosed error from pattern match."""
        return DiagnosedError(
            category=pattern_info["category"],
            severity=pattern_info["severity"],
            message=str(error),
            root_cause=pattern_info["diagnosis"],
            suggestions=pattern_info["suggestions"],
            context=context,
            original_error=error,
            documentation_links=pattern_info.get("documentation_links", []),
            related_errors=[],
        )

    def _create_generic_diagnosis(
        self, error: Exception, context: ErrorContext
    ) -> DiagnosedError:
        """Create generic diagnosis for unrecognized errors."""
        error_type = type(error).__name__

        # Categorize based on error type
        if isinstance(error, (FileNotFoundError, IOError, OSError)):
            category = ErrorCategory.FILE_IO
        elif isinstance(error, (ValueError, TypeError, KeyError)):
            category = ErrorCategory.CONFIGURATION
        elif isinstance(error, MemoryError):
            category = ErrorCategory.MEMORY
        elif isinstance(error, ImportError):
            category = ErrorCategory.DEPENDENCY
        else:
            category = ErrorCategory.UNKNOWN

        suggestions = [
            "Check the error message for specific details",
            "Review recent changes to configuration or data",
            "Ensure all dependencies are properly installed",
            "Check the SUEWS documentation for similar issues",
        ]

        # Add context-specific suggestions
        if context.file_path:
            suggestions.insert(0, f"Verify the file exists: {context.file_path}")
        if context.variable_name:
            suggestions.insert(0, f"Check the value of: {context.variable_name}")

        return DiagnosedError(
            category=category,
            severity=ErrorSeverity.ERROR,
            message=str(error),
            root_cause=f"{error_type} occurred during {context.operation}",
            suggestions=suggestions,
            context=context,
            original_error=error,
            documentation_links=[
                "https://suews.readthedocs.io/en/latest/troubleshooting.html"
            ],
            related_errors=[],
        )

    def _add_to_history(self, error: DiagnosedError):
        """Add error to history for pattern analysis."""
        self.error_history.append(error)
        if len(self.error_history) > self.max_history:
            self.error_history = self.error_history[-self.max_history :]

    def _find_related_errors(self, error: DiagnosedError) -> List[str]:
        """Find related errors from history."""
        related = []
        for historical in self.error_history[-10:]:  # Check last 10 errors
            if historical == error:
                continue
            if (
                historical.category == error.category
                and historical.root_cause != error.root_cause
            ):
                related.append(historical.root_cause)
        return list(set(related))[:3]  # Return up to 3 unique related errors

    def get_error_summary(self) -> Dict[str, Any]:
        """Get summary of recent errors."""
        if not self.error_history:
            return {"total_errors": 0, "categories": {}, "recent": []}

        categories = {}
        for error in self.error_history:
            cat = error.category.value
            categories[cat] = categories.get(cat, 0) + 1

        recent = [
            {
                "message": e.message,
                "category": e.category.value,
                "severity": e.severity.value,
                "operation": e.context.operation,
            }
            for e in self.error_history[-5:]
        ]

        return {
            "total_errors": len(self.error_history),
            "categories": categories,
            "recent": recent,
        }

    def suggest_preventive_measures(self) -> List[str]:
        """Suggest preventive measures based on error history."""
        if not self.error_history:
            return []

        suggestions = set()
        category_counts = {}

        for error in self.error_history:
            cat = error.category
            category_counts[cat] = category_counts.get(cat, 0) + 1

        # Add suggestions based on most common error categories
        for category, count in sorted(
            category_counts.items(), key=lambda x: x[1], reverse=True
        ):
            if category == ErrorCategory.FILE_IO:
                suggestions.add("Implement file existence checks before operations")
                suggestions.add("Use absolute paths to avoid ambiguity")
            elif category == ErrorCategory.DATA_QUALITY:
                suggestions.add("Add data validation step before simulations")
                suggestions.add("Implement automatic data quality reports")
            elif category == ErrorCategory.CONFIGURATION:
                suggestions.add("Use configuration templates to avoid errors")
                suggestions.add("Validate configurations before running simulations")
            elif category == ErrorCategory.SIMULATION:
                suggestions.add("Start with conservative time steps and parameters")
                suggestions.add("Monitor simulation stability indicators")
            elif category == ErrorCategory.DEPENDENCY:
                suggestions.add("Create a requirements.txt file for dependencies")
                suggestions.add("Use virtual environments for isolation")

        return list(suggestions)[:5]  # Return top 5 suggestions


# Global error handler instance
error_handler = SUEWSErrorHandler()


def handle_error(error: Exception, operation: str, **context_kwargs) -> DiagnosedError:
    """Convenience function to handle errors.

    Args:
        error: The exception that occurred
        operation: Description of what was being attempted
        **context_kwargs: Additional context information

    Returns:
        DiagnosedError with suggestions for resolution
    """
    context = ErrorContext(operation=operation, **context_kwargs)
    return error_handler.diagnose(error, context)


def format_error_for_mcp(diagnosed_error: DiagnosedError) -> Dict[str, Any]:
    """Format diagnosed error for MCP response."""
    return {
        "success": False,
        "error": diagnosed_error.to_dict(),
        "formatted_message": diagnosed_error.format_for_user(verbose=False),
    }
