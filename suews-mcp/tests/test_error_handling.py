"""Tests for intelligent error handling and diagnosis."""

import pytest
import json
import yaml
from pathlib import Path
import tempfile
import pandas as pd
import numpy as np
from unittest.mock import Mock, patch, MagicMock

from suews_mcp.error_handler import (
    ErrorCategory,
    ErrorSeverity,
    ErrorContext,
    DiagnosedError,
    ErrorPatternMatcher,
    SUEWSErrorHandler,
    handle_error,
    format_error_for_mcp,
)
from suews_mcp.validation import (
    ValidationRule,
    ValidationResult,
    ConfigurationValidator,
    ForcingDataValidator,
    validate_configuration,
    validate_forcing_data,
)


class TestErrorPatternMatcher:
    """Test error pattern matching."""

    def test_file_not_found_pattern(self):
        """Test detection of file not found errors."""
        matcher = ErrorPatternMatcher()

        # Test various file not found messages
        errors = [
            "FileNotFoundError: /path/to/missing.txt",
            "No such file: config.yml",
            "cannot find forcing_data.txt",
        ]

        for error_text in errors:
            result = matcher.match(error_text)
            assert result is not None
            assert result["category"] == ErrorCategory.FILE_IO
            assert "Check if the file path is correct" in result["suggestions"]

    def test_surface_fraction_error(self):
        """Test detection of surface fraction errors."""
        matcher = ErrorPatternMatcher()

        errors = [
            "Surface fractions sum to 0.95, not equal to 1",
            "Fraction validation failed: total = 1.05",
        ]

        for error_text in errors:
            result = matcher.match(error_text)
            assert result is not None
            assert result["category"] == ErrorCategory.CONFIGURATION
            assert any("sum to exactly 1.0" in s for s in result["suggestions"])

    def test_missing_column_error(self):
        """Test detection of missing data column errors."""
        matcher = ErrorPatternMatcher()

        errors = [
            "Missing required column: Tair",
            "Column not found: RH",
            "KeyError: 'kdown'",
        ]

        for error_text in errors:
            result = matcher.match(error_text)
            assert result is not None
            assert result["category"] == ErrorCategory.DATA_QUALITY
            assert any("required columns" in s for s in result["suggestions"])

    def test_memory_error(self):
        """Test detection of memory errors."""
        matcher = ErrorPatternMatcher()

        errors = [
            "MemoryError: Unable to allocate array",
            "out of memory",
            "memory allocation failed",
        ]

        for error_text in errors:
            result = matcher.match(error_text)
            assert result is not None
            assert result["category"] == ErrorCategory.MEMORY
            assert result["severity"] == ErrorSeverity.CRITICAL

    def test_numerical_instability(self):
        """Test detection of numerical instability."""
        matcher = ErrorPatternMatcher()

        errors = [
            "Numerical instability detected",
            "Solution diverged at timestep 100",
            "NaN detected in temperature calculation",
        ]

        for error_text in errors:
            result = matcher.match(error_text)
            assert result is not None
            assert result["category"] == ErrorCategory.SIMULATION
            assert any("time step" in s for s in result["suggestions"])

    def test_no_match(self):
        """Test when no pattern matches."""
        matcher = ErrorPatternMatcher()

        result = matcher.match("Some random error message")
        assert result is None


class TestSUEWSErrorHandler:
    """Test SUEWS error handler."""

    def test_diagnose_known_error(self):
        """Test diagnosis of known error patterns."""
        handler = SUEWSErrorHandler()

        error = FileNotFoundError("/path/to/missing.txt")
        context = ErrorContext(operation="loading configuration")

        diagnosed = handler.diagnose(error, context)

        assert diagnosed.category == ErrorCategory.FILE_IO
        assert diagnosed.severity == ErrorSeverity.ERROR
        assert "File or directory does not exist" in diagnosed.root_cause
        assert len(diagnosed.suggestions) > 0

    def test_diagnose_unknown_error(self):
        """Test diagnosis of unknown errors."""
        handler = SUEWSErrorHandler()

        error = RuntimeError("Something went wrong")
        context = ErrorContext(
            operation="running simulation", file_path=Path("/test/config.yml")
        )

        diagnosed = handler.diagnose(error, context)

        assert diagnosed.category == ErrorCategory.UNKNOWN
        assert diagnosed.severity == ErrorSeverity.ERROR
        assert "RuntimeError" in diagnosed.root_cause
        assert len(diagnosed.suggestions) > 0
        assert str(context.file_path) in diagnosed.suggestions[0]

    def test_error_history(self):
        """Test error history tracking."""
        handler = SUEWSErrorHandler()

        # Generate multiple errors
        for i in range(5):
            error = ValueError(f"Test error {i}")
            context = ErrorContext(operation=f"operation_{i}")
            handler.diagnose(error, context)

        summary = handler.get_error_summary()

        assert summary["total_errors"] == 5
        assert "configuration" in summary["categories"]
        assert len(summary["recent"]) == 5

    def test_related_errors(self):
        """Test finding related errors."""
        handler = SUEWSErrorHandler()

        # Generate errors in same category
        errors = [
            FileNotFoundError("config.yml"),
            FileNotFoundError("forcing.txt"),
            IOError("Permission denied"),
        ]

        for error in errors:
            context = ErrorContext(operation="file access")
            handler.diagnose(error, context)

        # Check last diagnosed error has related errors
        assert len(handler.error_history) == 3
        last_error = handler.error_history[-1]
        assert len(last_error.related_errors) > 0

    def test_preventive_measures(self):
        """Test preventive measure suggestions."""
        handler = SUEWSErrorHandler()

        # Generate various error types
        errors = [
            (FileNotFoundError("test.txt"), "loading"),
            (ValueError("Invalid config"), "parsing"),
            (MemoryError("Out of memory"), "simulation"),
        ]

        for error, operation in errors:
            context = ErrorContext(operation=operation)
            handler.diagnose(error, context)

        measures = handler.suggest_preventive_measures()

        assert len(measures) > 0
        assert any("file existence" in m for m in measures)

    def test_format_for_user(self):
        """Test user-friendly error formatting."""
        diagnosed = DiagnosedError(
            category=ErrorCategory.CONFIGURATION,
            severity=ErrorSeverity.ERROR,
            message="Test error message",
            root_cause="Invalid configuration",
            suggestions=["Check config file", "Validate parameters"],
            context=ErrorContext(
                operation="loading",
                file_path=Path("/test/config.yml"),
                variable_name="timestep",
                value=0,
            ),
            documentation_links=["https://docs.example.com"],
        )

        formatted = diagnosed.format_for_user(verbose=False)

        assert "ERROR" in formatted
        assert "Invalid configuration" in formatted
        assert "Check config file" in formatted
        assert "/test/config.yml" in formatted
        assert "timestep" in formatted

    def test_format_for_mcp(self):
        """Test MCP response formatting."""
        diagnosed = DiagnosedError(
            category=ErrorCategory.DATA_QUALITY,
            severity=ErrorSeverity.WARNING,
            message="Missing data detected",
            root_cause="Gaps in time series",
            suggestions=["Use gap filling"],
            context=ErrorContext(operation="validation"),
        )

        mcp_response = format_error_for_mcp(diagnosed)

        assert mcp_response["success"] is False
        assert "error" in mcp_response
        assert mcp_response["error"]["category"] == "data_quality"
        assert mcp_response["error"]["severity"] == "warning"
        assert "formatted_message" in mcp_response


class TestConfigurationValidator:
    """Test configuration validation."""

    def test_validate_valid_config(self, tmp_path):
        """Test validation of valid configuration."""
        config = {
            "surfaces": {
                "fr_paved": 0.2,
                "fr_bldgs": 0.3,
                "fr_evetr": 0.1,
                "fr_dectr": 0.1,
                "fr_grass": 0.2,
                "fr_bsoil": 0.05,
                "fr_water": 0.05,
            },
            "morphology": {"h_bldg_mean": 10, "h_bldg_max": 20},
            "initial_conditions": {"temp_c": 20, "rh_pct": 60, "pressure_hpa": 1013},
            "time": {"dt": 3600, "start_date": "2023-01-01", "end_date": "2023-01-02"},
        }

        config_file = tmp_path / "config.json"
        config_file.write_text(json.dumps(config))

        validator = ConfigurationValidator()
        result = validator.validate(config_file)

        assert result.passed
        assert result.passed_checks > 0
        assert len(result.issues) == 0

    def test_validate_invalid_fractions(self, tmp_path):
        """Test detection of invalid surface fractions."""
        config = {
            "surfaces": {
                "fr_paved": 0.5,
                "fr_bldgs": 0.3,
                "fr_evetr": 0.1,
                "fr_dectr": 0.1,
                "fr_grass": 0.2,  # Sum > 1.0
                "fr_bsoil": 0.05,
                "fr_water": 0.05,
            }
        }

        config_file = tmp_path / "config.yml"
        config_file.write_text(yaml.dump(config))

        validator = ConfigurationValidator()
        result = validator.validate(config_file)

        assert not result.passed
        assert any("surface_fractions" in i["rule"] for i in result.issues)
        assert any("sum to 1.0" in s for s in result.suggestions)

    def test_validate_invalid_heights(self, tmp_path):
        """Test detection of invalid building heights."""
        config = {
            "morphology": {
                "h_bldg_mean": 30,
                "h_bldg_max": 20,  # Mean > max
            }
        }

        config_file = tmp_path / "config.json"
        config_file.write_text(json.dumps(config))

        validator = ConfigurationValidator()
        result = validator.validate(config_file)

        assert not result.passed
        issues = [i for i in result.issues if "building_heights" in i["rule"]]
        assert len(issues) > 0
        assert "Mean height" in issues[0]["details"]

    def test_validate_missing_file(self, tmp_path):
        """Test handling of missing configuration file."""
        config_file = tmp_path / "nonexistent.json"

        validator = ConfigurationValidator()
        result = validator.validate(config_file)

        assert not result.passed
        assert result.failed_checks > 0
        assert any("not found" in i["message"] for i in result.issues)

    def test_validation_report_format(self, tmp_path):
        """Test validation report formatting."""
        config = {
            "surfaces": {
                "fr_paved": 0.5,
                "fr_bldgs": 0.6,  # Invalid sum
            }
        }

        config_file = tmp_path / "config.json"
        config_file.write_text(json.dumps(config))

        validator = ConfigurationValidator()
        result = validator.validate(config_file)
        report = result.format_report()

        assert "FAILED" in report
        assert "Issues Found:" in report
        assert "surface_fractions" in report


class TestForcingDataValidator:
    """Test forcing data validation."""

    def test_validate_valid_forcing(self, tmp_path):
        """Test validation of valid forcing data."""
        # Create valid forcing data
        data = {
            "iy": [2023] * 24,
            "id": [1] * 24,
            "it": list(range(24)),
            "imin": [0] * 24,
            "qn1": [100] * 24,
            "qh": [50] * 24,
            "qe": [30] * 24,
            "qs": [20] * 24,
            "qf": [10] * 24,
            "U": [3] * 24,
            "RH": [60] * 24,
            "Tair": [20] * 24,
            "pres": [1013] * 24,
            "rain": [0] * 24,
            "kdown": [200] * 24,
            "snow": [0] * 24,
            "ldown": [300] * 24,
            "fcld": [0.5] * 24,
            "Wuh": [0] * 24,
            "xsmd": [0] * 24,
            "lai": [3] * 24,
            "kdiff": [100] * 24,
            "kdir": [100] * 24,
            "wdir": [180] * 24,
        }

        df = pd.DataFrame(data)
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, sep="\t", index=False)

        validator = ForcingDataValidator()
        result = validator.validate(forcing_file)

        assert result.passed
        assert result.passed_checks > 0

    def test_validate_missing_columns(self, tmp_path):
        """Test detection of missing columns."""
        # Create forcing data with missing columns
        data = {
            "iy": [2023] * 24,
            "id": [1] * 24,
            "it": list(range(24)),
            "imin": [0] * 24,
            "Tair": [20] * 24,
            "RH": [60] * 24,
            # Missing many required columns
        }

        df = pd.DataFrame(data)
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, sep="\t", index=False)

        validator = ForcingDataValidator()
        result = validator.validate(forcing_file)

        assert not result.passed
        assert any("Missing required columns" in i["message"] for i in result.issues)

    def test_validate_physical_ranges(self, tmp_path):
        """Test detection of values outside physical ranges."""
        # Create forcing data with invalid values
        data = {
            "iy": [2023] * 24,
            "id": [1] * 24,
            "it": list(range(24)),
            "imin": [0] * 24,
            "Tair": [100] * 24,  # Too hot
            "RH": [150] * 24,  # > 100%
            "U": [-5] * 24,  # Negative wind
            "pres": [500] * 24,  # Too low
            # Add other required columns with valid values
            "qn1": [100] * 24,
            "qh": [50] * 24,
            "qe": [30] * 24,
            "qs": [20] * 24,
            "qf": [10] * 24,
            "rain": [0] * 24,
            "kdown": [200] * 24,
            "snow": [0] * 24,
            "ldown": [300] * 24,
            "fcld": [0.5] * 24,
            "Wuh": [0] * 24,
            "xsmd": [0] * 24,
            "lai": [3] * 24,
            "kdiff": [100] * 24,
            "kdir": [100] * 24,
            "wdir": [180] * 24,
        }

        df = pd.DataFrame(data)
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, sep="\t", index=False)

        validator = ForcingDataValidator()
        result = validator.validate(forcing_file)

        assert not result.passed
        assert any("outside" in i["message"] for i in result.issues)
        assert any(
            "Check units" in s
            for issue in result.issues
            for s in issue.get("suggestions", [])
        )

    def test_validate_time_gaps(self, tmp_path):
        """Test detection of time gaps."""
        # Create forcing data with time gap
        hours = list(range(10)) + list(range(15, 24))  # Gap from hour 10-14
        data = {
            "iy": [2023] * len(hours),
            "id": [1] * len(hours),
            "it": hours,
            "imin": [0] * len(hours),
            "Tair": [20] * len(hours),
            "RH": [60] * len(hours),
            "U": [3] * len(hours),
            "pres": [1013] * len(hours),
            # Add other required columns
            "qn1": [100] * len(hours),
            "qh": [50] * len(hours),
            "qe": [30] * len(hours),
            "qs": [20] * len(hours),
            "qf": [10] * len(hours),
            "rain": [0] * len(hours),
            "kdown": [200] * len(hours),
            "snow": [0] * len(hours),
            "ldown": [300] * len(hours),
            "fcld": [0.5] * len(hours),
            "Wuh": [0] * len(hours),
            "xsmd": [0] * len(hours),
            "lai": [3] * len(hours),
            "kdiff": [100] * len(hours),
            "kdir": [100] * len(hours),
            "wdir": [180] * len(hours),
        }

        df = pd.DataFrame(data)
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, sep="\t", index=False)

        validator = ForcingDataValidator()
        result = validator.validate(forcing_file)

        # Should have warnings about gaps
        assert any("gap" in i["message"].lower() for i in result.issues)

    def test_validate_data_completeness(self, tmp_path):
        """Test detection of missing data."""
        # Create forcing data with missing values
        data = {
            "iy": [2023] * 24,
            "id": [1] * 24,
            "it": list(range(24)),
            "imin": [0] * 24,
            "Tair": [20] * 12 + [np.nan] * 12,  # 50% missing
            "RH": [60] * 24,
            "U": [3] * 24,
            "pres": [1013] * 24,
            # Add other columns
            "qn1": [100] * 24,
            "qh": [50] * 24,
            "qe": [30] * 24,
            "qs": [20] * 24,
            "qf": [10] * 24,
            "rain": [0] * 24,
            "kdown": [200] * 24,
            "snow": [0] * 24,
            "ldown": [300] * 24,
            "fcld": [0.5] * 24,
            "Wuh": [0] * 24,
            "xsmd": [0] * 24,
            "lai": [3] * 24,
            "kdiff": [100] * 24,
            "kdir": [100] * 24,
            "wdir": [180] * 24,
        }

        df = pd.DataFrame(data)
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, sep="\t", index=False)

        validator = ForcingDataValidator()
        result = validator.validate(forcing_file)

        assert not result.passed
        assert any("missing data" in i["message"].lower() for i in result.issues)
        assert any("gap-filling" in s for s in result.suggestions)


class TestConvenienceFunctions:
    """Test convenience functions."""

    def test_handle_error(self):
        """Test handle_error convenience function."""
        error = ValueError("Test error")
        diagnosed = handle_error(
            error, "testing", file_path=Path("/test.txt"), variable_name="test_var"
        )

        assert isinstance(diagnosed, DiagnosedError)
        assert diagnosed.context.operation == "testing"
        assert diagnosed.context.file_path == Path("/test.txt")
        assert diagnosed.context.variable_name == "test_var"

    def test_validate_configuration_function(self, tmp_path):
        """Test validate_configuration convenience function."""
        config = {"surfaces": {"fr_paved": 1.0}}
        config_file = tmp_path / "config.json"
        config_file.write_text(json.dumps(config))

        result = validate_configuration(config_file)
        assert isinstance(result, ValidationResult)

    def test_validate_forcing_data_function(self, tmp_path):
        """Test validate_forcing_data convenience function."""
        df = pd.DataFrame({"iy": [2023], "Tair": [20]})
        forcing_file = tmp_path / "forcing.txt"
        df.to_csv(forcing_file, index=False)

        result = validate_forcing_data(forcing_file)
        assert isinstance(result, ValidationResult)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
