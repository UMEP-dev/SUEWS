"""Standardized JSON output format for CI/CMD integration.

This module provides consistent JSON output structures for all SUEWS CLI commands,
designed for easy parsing by CI tools and command-line utilities.
"""

import json
import sys
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Union
from enum import Enum


class ErrorCode(Enum):
    """Machine-readable error codes for CI integration."""

    # Validation errors (1000-1999)
    VALIDATION_FAILED = 1001
    MISSING_REQUIRED_FIELD = 1002
    INVALID_VALUE = 1003
    TYPE_ERROR = 1004
    PHYSICS_INCOMPATIBLE = 1005
    SCIENTIFIC_INVALID = 1006

    # File errors (2000-2999)
    FILE_NOT_FOUND = 2001
    FILE_READ_ERROR = 2002
    FILE_WRITE_ERROR = 2003
    INVALID_YAML = 2004

    # Processing errors (3000-3999)
    PHASE_A_FAILED = 3001
    PHASE_B_FAILED = 3002
    PHASE_C_FAILED = 3003
    PROCESSING_ERROR = 3004

    # Schema errors (4000-4999)
    SCHEMA_VERSION_MISMATCH = 4001
    MIGRATION_FAILED = 4002
    SCHEMA_NOT_FOUND = 4003


class ValidationError:
    """Structured validation error with code, location, and details."""

    def __init__(
        self,
        code: ErrorCode,
        message: str,
        field: Optional[str] = None,
        location: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None,
    ):
        self.code = code
        self.message = message
        self.field = field
        self.location = location
        self.details = details or {}

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        result = {
            "code": self.code.value,
            "code_name": self.code.name,
            "severity": "ERROR",
            "path": self.field or self.location,
            "site_gridid": None,
            "message": self.message,
            "suggested_value": self.details.get("suggested_value"),
            "source": self.details.get("source", "suews-validate"),
        }
        if self.field:
            result["field"] = self.field
        if self.location:
            result["location"] = self.location
        if self.details:
            result["details"] = self.details
        return result


class JSONOutput:
    """Standardized JSON output formatter for all SUEWS commands."""

    def __init__(self, command: str, version: str = "2025.8.20"):
        """Initialize JSON output with command context.

        Args:
            command: Name of the command being executed
            version: SUEWS version
        """
        self.command = command
        self.version = version
        self.start_time = datetime.utcnow()

    @staticmethod
    def _normalise_issue(issue: Any, severity: str) -> Dict[str, Any]:
        """Return the stable validation issue shape used by JSON output."""
        if isinstance(issue, str):
            return {
                "code": ErrorCode.VALIDATION_FAILED.value,
                "code_name": ErrorCode.VALIDATION_FAILED.name,
                "severity": severity,
                "path": None,
                "site_gridid": None,
                "message": issue,
                "suggested_value": None,
                "source": "suews-validate",
            }
        if isinstance(issue, ValidationError):
            result = issue.to_dict()
        elif hasattr(issue, "to_dict"):
            result = issue.to_dict()
        else:
            result = dict(issue)

        result.setdefault("severity", severity)
        result.setdefault("path", result.get("field") or result.get("location"))
        result.setdefault("site_gridid", None)
        result.setdefault("message", "")
        result.setdefault("suggested_value", None)
        result.setdefault("source", "suews-validate")
        return result

    def validation_result(
        self,
        files: List[Dict[str, Any]],
        schema_version: Optional[str] = None,
        dry_run: bool = False,
    ) -> Dict[str, Any]:
        """Format validation results for JSON output.

        Args:
            files: List of file validation results
            schema_version: Schema version used for validation
            dry_run: Whether this was a dry-run

        Returns:
            Structured JSON-serializable dictionary
        """
        # Calculate summary statistics
        total_files = len(files)
        valid_files = sum(1 for f in files if f.get("valid", False))
        total_errors = sum(f.get("error_count", len(f.get("errors", []))) for f in files)
        total_warnings = sum(len(f.get("warnings", [])) for f in files)
        total_suggestions = sum(len(f.get("suggestions", [])) for f in files)
        total_applied = sum(len(f.get("applied_fixes", [])) for f in files)

        # Structure issue arrays properly and keep all expected arrays present.
        for file_result in files:
            for field_name, severity in (
                ("errors", "ERROR"),
                ("warnings", "WARNING"),
                ("suggestions", "SUGGESTION"),
                ("applied_fixes", "APPLIED_FIX"),
                ("info", "INFO"),
            ):
                issues = file_result.get(field_name, [])
                if not isinstance(issues, list):
                    issues = [issues]
                file_result[field_name] = [
                    self._normalise_issue(issue, severity) for issue in issues
                ]

        return {
            "status": "success" if valid_files == total_files else "failure",
            "command": self.command,
            "timestamp": self.start_time.isoformat() + "Z",
            "duration": (datetime.utcnow() - self.start_time).total_seconds(),
            "metadata": {
                "suews_version": self.version,
                "schema_version": schema_version,
                "dry_run": dry_run,
            },
            "summary": {
                "total_files": total_files,
                "valid_files": valid_files,
                "invalid_files": total_files - valid_files,
                "total_errors": total_errors,
                "total_warnings": total_warnings,
                "total_suggestions": total_suggestions,
                "total_applied_fixes": total_applied,
                "success_rate": valid_files / total_files if total_files > 0 else 0.0,
            },
            "results": files,
        }

    def phase_result(
        self,
        phase: str,
        success: bool,
        input_file: str,
        output_file: Optional[str] = None,
        report_file: Optional[str] = None,
        errors: Optional[List[Union[str, ValidationError]]] = None,
        warnings: Optional[List[str]] = None,
        suggestions: Optional[List[Any]] = None,
        applied_fixes: Optional[List[Any]] = None,
        info: Optional[List[Any]] = None,
    ) -> Dict[str, Any]:
        """Format phase processing results for JSON output.

        Args:
            phase: Phase identifier (A, B, C, AB, AC, BC, ABC)
            success: Whether the phase succeeded
            input_file: Input file path
            output_file: Output file path if generated
            report_file: Report file path if generated
            errors: List of errors if any
            warnings: List of warnings if any

        Returns:
            Structured JSON-serializable dictionary
        """
        structured_errors = [
            self._normalise_issue(error, "ERROR") for error in (errors or [])
        ]
        structured_warnings = [
            self._normalise_issue(warning, "WARNING") for warning in (warnings or [])
        ]
        structured_suggestions = [
            self._normalise_issue(suggestion, "SUGGESTION")
            for suggestion in (suggestions or [])
        ]
        structured_applied = [
            self._normalise_issue(applied, "APPLIED_FIX")
            for applied in (applied_fixes or [])
        ]
        structured_info = [
            self._normalise_issue(info_item, "INFO") for info_item in (info or [])
        ]

        return {
            "status": "success" if success else "failure",
            "command": f"{self.command}_phase_{phase}",
            "timestamp": self.start_time.isoformat() + "Z",
            "duration": (datetime.utcnow() - self.start_time).total_seconds(),
            "metadata": {"suews_version": self.version, "phase": phase},
            "summary": {
                "success": success,
                "error_count": len(structured_errors),
                "warning_count": len(structured_warnings),
                "suggestion_count": len(structured_suggestions),
                "applied_fix_count": len(structured_applied),
            },
            "files": {
                "input": input_file,
                "output": output_file,
                "report": report_file,
            },
            "errors": structured_errors if structured_errors else [],
            "warnings": structured_warnings if structured_warnings else [],
            "suggestions": structured_suggestions if structured_suggestions else [],
            "applied_fixes": structured_applied if structured_applied else [],
            "info": structured_info if structured_info else [],
        }

    def schema_operation_result(
        self,
        operation: str,
        success: bool,
        details: Dict[str, Any],
        errors: Optional[List[Union[str, ValidationError]]] = None,
    ) -> Dict[str, Any]:
        """Format schema operation results for JSON output.

        Args:
            operation: Operation type (info, version, migrate, export)
            success: Whether the operation succeeded
            details: Operation-specific details
            errors: List of errors if any

        Returns:
            Structured JSON-serializable dictionary
        """
        # Structure errors
        structured_errors = []
        if errors:
            for error in errors:
                if isinstance(error, str):
                    structured_errors.append({
                        "code": ErrorCode.SCHEMA_NOT_FOUND.value,
                        "code_name": ErrorCode.SCHEMA_NOT_FOUND.name,
                        "message": error,
                    })
                elif isinstance(error, ValidationError):
                    structured_errors.append(error.to_dict())
                else:
                    structured_errors.append(error)

        return {
            "status": "success" if success else "failure",
            "command": f"schema_{operation}",
            "timestamp": self.start_time.isoformat() + "Z",
            "duration": (datetime.utcnow() - self.start_time).total_seconds(),
            "metadata": {"suews_version": self.version, "operation": operation},
            "result": details,
            "errors": structured_errors if structured_errors else [],
        }

    @staticmethod
    def output(data: Dict[str, Any], file=None):
        """Output JSON data to stdout or file.

        Args:
            data: Dictionary to output as JSON
            file: Optional file handle (defaults to stdout)
        """
        output = json.dumps(data, indent=2, ensure_ascii=False)
        if file:
            file.write(output)
        else:
            # Write directly to stdout to avoid Rich console formatting
            sys.stdout.write(output)
            sys.stdout.write("\n")
            sys.stdout.flush()

    @staticmethod
    def parse_validation_error(error_str: str) -> ValidationError:
        """Parse a validation error string into structured format.

        Args:
            error_str: Error string from validator

        Returns:
            Structured ValidationError object
        """
        # Common patterns in validation errors
        if "Missing required" in error_str:
            return ValidationError(
                code=ErrorCode.MISSING_REQUIRED_FIELD, message=error_str
            )
        elif "Invalid value" in error_str or "outside range" in error_str:
            return ValidationError(code=ErrorCode.INVALID_VALUE, message=error_str)
        elif "Type error" in error_str or "expected" in error_str:
            return ValidationError(code=ErrorCode.TYPE_ERROR, message=error_str)
        elif "physics" in error_str.lower() or "incompatible" in error_str:
            return ValidationError(
                code=ErrorCode.PHYSICS_INCOMPATIBLE, message=error_str
            )
        elif "scientific" in error_str.lower():
            return ValidationError(code=ErrorCode.SCIENTIFIC_INVALID, message=error_str)
        else:
            return ValidationError(code=ErrorCode.VALIDATION_FAILED, message=error_str)
