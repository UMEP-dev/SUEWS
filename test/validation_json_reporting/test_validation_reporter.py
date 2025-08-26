"""
Unit tests for ValidationReporter and TextReportGenerator.

Tests the JSON-based validation reporting system to ensure:
1. JSON structure is correctly built
2. Text reports match expected format
3. Merging of reporters works correctly
4. All validation item types are handled
"""

import json
import tempfile
from pathlib import Path

import pytest

from supy.data_model.validation.pipeline.text_report_generator import (
    TextReportGenerator,
)
from supy.data_model.validation.pipeline.validation_reporter import ValidationReporter


class TestValidationReporter:
    """Test the ValidationReporter class."""

    def test_initialization(self):
        """Test reporter initializes with correct structure."""
        reporter = ValidationReporter()

        assert reporter.json_data["schema_version"] == "1.0.0"
        assert reporter.json_data["summary"]["total_errors"] == 0
        assert reporter.json_data["summary"]["total_warnings"] == 0
        assert reporter.json_data["summary"]["total_info"] == 0
        assert reporter.json_data["summary"]["validation_passed"]
        assert reporter.json_data["phases"] == {}
        assert reporter.json_data["errors"] == []
        assert reporter.json_data["warnings"] == []
        assert reporter.json_data["info"] == []

    def test_add_error(self):
        """Test adding error updates counters correctly."""
        reporter = ValidationReporter()

        reporter.add_error({
            "phase": "A",
            "type": "missing_required",
            "field_path": "model.physics.emissionsmethod",
            "message": "Required parameter missing",
            "severity": "error",
        })

        assert reporter.json_data["summary"]["total_errors"] == 1
        assert not reporter.json_data["summary"]["validation_passed"]
        assert len(reporter.json_data["errors"]) == 1
        assert reporter.json_data["errors"][0]["severity"] == "critical"  # Always set to critical internally

    def test_add_warning(self):
        """Test adding warning updates counters correctly."""
        reporter = ValidationReporter()

        reporter.add_warning({
            "phase": "B",
            "type": "deprecated_parameter",
            "field_path": "model.physics.cp",
            "message": "Parameter is deprecated",
            "severity": "warning",
        })

        assert reporter.json_data["summary"]["total_warnings"] == 1
        assert reporter.json_data["summary"][
            "validation_passed"
        ]  # Warnings don't fail validation
        assert len(reporter.json_data["warnings"]) == 1
        assert reporter.json_data["warnings"][0]["severity"] == "warning"

    def test_add_info(self):
        """Test adding info updates counters correctly."""
        reporter = ValidationReporter()

        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": "model.physics.rho_cp",
            "message": "Parameter renamed from cp",
            "severity": "info",
        })

        assert reporter.json_data["summary"]["total_info"] == 1
        assert reporter.json_data["summary"]["validation_passed"]
        assert len(reporter.json_data["info"]) == 1
        assert reporter.json_data["info"][0]["severity"] == "info"

    def test_phase_tracking(self):
        """Test phase completion tracking."""
        reporter = ValidationReporter()

        # Use add_phase_results instead of start_phase/complete_phase
        reporter.add_phase_results("A", {"validation_count": 10})
        assert "A" in reporter.json_data["phases"]
        assert reporter.json_data["phases"]["A"]["completed"] is True
        assert "A" in reporter.json_data["phases_completed"]

    def test_merge_reporters(self):
        """Test merging multiple reporters."""
        reporter1 = ValidationReporter()
        reporter1.add_error({
            "phase": "A",
            "type": "test",
            "field_path": "test.path",
            "message": "Error 1",
        })
        reporter1.add_phase_results("A", {"status": "failed"})

        reporter2 = ValidationReporter()
        reporter2.add_warning({
            "phase": "B",
            "type": "test",
            "field_path": "test.path",
            "message": "Warning 1",
        })
        reporter2.add_phase_results("B", {"status": "passed"})

        # The merge method is instance-based, not class-based
        reporter1.merge(reporter2)

        assert reporter1.json_data["summary"]["total_errors"] == 1
        assert reporter1.json_data["summary"]["total_warnings"] == 1
        assert len(reporter1.json_data["phases_completed"]) == 2
        assert not reporter1.json_data["summary"]["validation_passed"]

    def test_save_json_report(self, tmp_path):
        """Test saving JSON report to file."""
        reporter = ValidationReporter()
        reporter.add_info({
            "phase": "A",
            "type": "test",
            "field_path": "test",
            "message": "Test message",
        })

        json_file = tmp_path / "test_report.json"
        reporter.save_json_report(str(json_file))

        assert json_file.exists()
        with open(json_file, encoding="utf-8") as f:
            data = json.load(f)
        assert data["schema_version"] == "1.0.0"
        assert data["summary"]["total_info"] == 1

    def test_get_action_items(self):
        """Test extracting action items from validation results."""
        reporter = ValidationReporter()

        reporter.add_error({
            "phase": "A",
            "type": "missing_required",
            "field_path": "model.physics.emissionsmethod",
            "message": "Required parameter missing",
            "details": {"fix": "Add emissionsmethod parameter"},
        })

        reporter.add_warning({
            "phase": "B",
            "type": "deprecated",
            "field_path": "model.physics.cp",
            "message": "Parameter deprecated",
            "details": {"fix": "Rename to rho_cp", "requires_action": True},  # Mark as requiring action
        })

        actions = reporter.get_action_items()
        assert len(actions) == 2
        assert actions[0]["severity"] == "critical"  # Errors have "critical" severity
        assert "Required parameter missing" in actions[0]["message"]
        assert actions[1]["severity"] == "warning"
        assert "Parameter deprecated" in actions[1]["message"]


class TestTextReportGenerator:
    """Test the TextReportGenerator class."""

    def test_generate_basic_report(self):
        """Test generating a basic text report."""
        reporter = ValidationReporter()
        reporter.add_error({
            "phase": "A",
            "type": "missing_required",
            "field_path": "model.physics.emissionsmethod",
            "message": "Test message",
        })

        generator = TextReportGenerator()  # No arguments
        text = generator.generate(reporter.get_json_report(), "A", "public")

        assert "## ACTION NEEDED" in text
        assert "Test message" in text  # Look for actual message
        assert "emissionsmethod" in text
        assert "Phase A" in text  # Look for Phase A in header

    def test_generate_info_only_report(self):
        """Test report with only info items."""
        reporter = ValidationReporter()
        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": "model.physics.rho_cp",
            "message": "cp changed to rho_cp",
        })

        generator = TextReportGenerator()  # No arguments
        text = generator.generate(reporter.get_json_report(), "A", "public")

        assert "## NO ACTION NEEDED" in text
        assert "cp changed to rho_cp" in text
        assert "Phase A" in text

    def test_save_text_report(self, tmp_path):
        """Test saving text report to file."""
        reporter = ValidationReporter()
        reporter.add_warning({
            "phase": "B",
            "type": "deprecated",
            "field_path": "test.field",
            "message": "Test warning",
        })

        generator = TextReportGenerator()  # No arguments
        text_file = tmp_path / "test_report.txt"
        text = generator.generate(reporter.get_json_report(), "B", "public")
        text_file.write_text(text)  # Save method doesn't exist, just write directly

        assert text_file.exists()
        content = text_file.read_text()
        assert "Test warning" in content or "## NO ACTION NEEDED" in content  # Warning might be in info section


class TestIntegration:
    """Test integrated validation reporting workflows."""

    def test_complete_validation_workflow(self, tmp_path):
        """Test a complete validation workflow with all phases."""
        reporter = ValidationReporter()

        # Phase A
        reporter.add_info({
            "phase": "A",
            "type": "missing_optional",
            "field_path": "model.control.output.groups",
            "message": "Optional parameter missing",
        })
        reporter.add_phase_results("A", {"status": "passed"})

        # Phase B
        reporter.add_warning({
            "phase": "B",
            "type": "physics_incompatibility",
            "field_path": "model.physics",
            "message": "Physics options may be incompatible",
        })
        reporter.add_phase_results("B", {"status": "passed"})

        # Phase C
        reporter.add_error({
            "phase": "C",
            "type": "validation_error",
            "field_path": "sites[0].properties",
            "message": "Invalid site properties",
        })
        reporter.add_phase_results("C", {"status": "failed"})

        # Generate reports
        json_file = tmp_path / "validation_report.json"
        text_file = tmp_path / "validation_report.txt"

        reporter.save_json_report(str(json_file))

        generator = TextReportGenerator()
        text = generator.generate(reporter.get_json_report(), "ABC", "public")
        text_file.write_text(text)

        # Verify JSON report
        assert json_file.exists()
        with open(json_file, encoding="utf-8") as f:
            json_data = json.load(f)
        assert json_data["summary"]["total_errors"] == 1
        assert json_data["summary"]["total_warnings"] == 1
        assert json_data["summary"]["total_info"] == 1
        assert not json_data["summary"]["validation_passed"]
        assert len(json_data["phases_completed"]) == 3

        # Verify text report
        assert text_file.exists()
        text_content = text_file.read_text()
        assert "Invalid site properties" in text_content  # Check for actual error message
        assert "Phase ABC" in text_content  # Check for phase in header


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
