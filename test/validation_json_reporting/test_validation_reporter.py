"""
Unit tests for ValidationReporter and TextReportGenerator.

Tests the JSON-based validation reporting system to ensure:
1. JSON structure is correctly built
2. Text reports match expected format
3. Merging of reporters works correctly
4. All validation item types are handled
"""

import pytest
import json
import tempfile
from pathlib import Path

# Add src directory to path
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from supy.data_model.validation.pipeline.validation_reporter import ValidationReporter
from supy.data_model.validation.pipeline.text_report_generator import TextReportGenerator


class TestValidationReporter:
    """Test the ValidationReporter class."""
    
    def test_initialization(self):
        """Test reporter initializes with correct structure."""
        reporter = ValidationReporter()
        
        assert reporter.json_data["schema_version"] == "1.0.0"
        assert reporter.json_data["summary"]["total_errors"] == 0
        assert reporter.json_data["summary"]["total_warnings"] == 0
        assert reporter.json_data["summary"]["total_info"] == 0
        assert reporter.json_data["summary"]["validation_passed"] == True
        assert reporter.json_data["phases"] == {}
        assert reporter.json_data["errors"] == []
        assert reporter.json_data["warnings"] == []
        assert reporter.json_data["info"] == []
    
    def test_add_error(self):
        """Test adding errors to the report."""
        reporter = ValidationReporter()
        
        reporter.add_error({
            "phase": "A",
            "type": "missing_required_parameter",
            "field_path": "model.physics.emissionsmethod",
            "message": "Required parameter 'emissionsmethod' is missing",
            "details": {"required": True},
            "suggestions": ["Add 'emissionsmethod' with value between 0-2"]
        })
        
        assert reporter.json_data["summary"]["total_errors"] == 1
        assert reporter.json_data["summary"]["validation_passed"] == False
        assert len(reporter.json_data["errors"]) == 1
        assert reporter.json_data["errors"][0]["type"] == "missing_required_parameter"
        assert reporter.json_data["errors"][0]["severity"] == "critical"
        
        # Check phase-specific errors
        assert "A" in reporter.json_data["phases"]
        assert len(reporter.json_data["phases"]["A"]["errors"]) == 1
    
    def test_add_warning(self):
        """Test adding warnings to the report."""
        reporter = ValidationReporter()
        
        reporter.add_warning({
            "phase": "B",
            "type": "science_warning",
            "field_path": "metforcing.temp_c",
            "message": "Temperature value seems unusually high",
            "details": {"value": 55, "typical_range": [-20, 45]}
        })
        
        assert reporter.json_data["summary"]["total_warnings"] == 1
        assert reporter.json_data["summary"]["validation_passed"] == True  # Warnings don't fail validation
        assert len(reporter.json_data["warnings"]) == 1
        assert reporter.json_data["warnings"][0]["severity"] == "warning"
    
    def test_add_info(self):
        """Test adding info items to the report."""
        reporter = ValidationReporter()
        
        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": "model.physics",
            "message": "Parameter renamed for consistency",
            "details": {"old_name": "bldgh", "new_name": "bldg_height"}
        })
        
        assert reporter.json_data["summary"]["total_info"] == 1
        assert reporter.json_data["summary"]["validation_passed"] == True
        assert len(reporter.json_data["info"]) == 1
    
    def test_merge_reporters(self):
        """Test merging multiple reporters."""
        reporter1 = ValidationReporter()
        reporter1.add_error({
            "phase": "A",
            "type": "missing_required_parameter",
            "field_path": "field1",
            "message": "Missing field1"
        })
        reporter1.add_phase_results("A", {"parameters_checked": 10})
        
        reporter2 = ValidationReporter()
        reporter2.add_warning({
            "phase": "B",
            "type": "science_warning",
            "field_path": "field2",
            "message": "Warning for field2"
        })
        reporter2.add_phase_results("B", {"validations_run": 5})
        
        # Merge reporter2 into reporter1
        reporter1.merge(reporter2)
        
        assert reporter1.json_data["summary"]["total_errors"] == 1
        assert reporter1.json_data["summary"]["total_warnings"] == 1
        assert "A" in reporter1.json_data["phases_completed"]
        assert "B" in reporter1.json_data["phases_completed"]
        assert reporter1.json_data["phases"]["A"]["results"]["parameters_checked"] == 10
        assert reporter1.json_data["phases"]["B"]["results"]["validations_run"] == 5
    
    def test_phase_results(self):
        """Test adding phase-specific results."""
        reporter = ValidationReporter()
        
        results = {
            "renamed_parameters": ["bldgh", "faibldg"],
            "missing_parameters": ["emissionsmethod"],
            "extra_parameters": ["unknown_param"],
            "total_processed": 25
        }
        
        reporter.add_phase_results("A", results)
        
        assert "A" in reporter.json_data["phases_completed"]
        assert reporter.json_data["phases"]["A"]["completed"] == True
        assert reporter.json_data["phases"]["A"]["results"] == results
    
    def test_get_action_items(self):
        """Test extraction of action items."""
        reporter = ValidationReporter()
        
        # Add an error (requires action)
        reporter.add_error({
            "phase": "A",
            "type": "error",
            "field_path": "field1",
            "message": "Error message"
        })
        
        # Add a warning that requires action
        reporter.add_warning({
            "phase": "B",
            "type": "warning",
            "field_path": "field2",
            "message": "Critical warning",
            "details": {"requires_action": True}
        })
        
        # Add a warning that doesn't require action
        reporter.add_warning({
            "phase": "B",
            "type": "warning",
            "field_path": "field3",
            "message": "Minor warning",
            "details": {"requires_action": False}
        })
        
        action_items = reporter.get_action_items()
        assert len(action_items) == 2  # Error + critical warning
    
    def test_save_json_report(self):
        """Test saving JSON report to file."""
        reporter = ValidationReporter()
        reporter.add_info({
            "phase": "A",
            "type": "info",
            "field_path": "test",
            "message": "Test message"
        })
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            filepath = Path(f.name)
        
        try:
            reporter.save_json_report(filepath)
            
            # Load and verify
            with open(filepath, 'r') as f:
                loaded_data = json.load(f)
            
            assert loaded_data["schema_version"] == "1.0.0"
            assert loaded_data["summary"]["total_info"] == 1
        finally:
            filepath.unlink()  # Clean up


class TestTextReportGenerator:
    """Test the TextReportGenerator class."""
    
    def test_generate_basic_report(self):
        """Test generating a basic text report from JSON."""
        reporter = ValidationReporter()
        reporter.add_info({
            "phase": "A",
            "type": "parameter_updated",
            "field_path": "model.physics.bldgh",
            "message": "Building height updated",
            "details": {"old_value": 10, "new_value": 20}
        })
        
        generator = TextReportGenerator()
        text_report = generator.generate(reporter.get_json_report(), "A", "public")
        
        assert "# SUEWS - Phase A (Up-to-date YAML check) Report" in text_report
        assert "# Mode: Public" in text_report
        assert "## NO ACTION NEEDED" in text_report
        assert "Updated model.physics.bldgh: 10 → 20" in text_report
    
    def test_generate_error_report(self):
        """Test generating report with errors."""
        reporter = ValidationReporter()
        reporter.add_error({
            "phase": "B",
            "type": "science_validation_error",
            "field_path": "metforcing.temp_c",
            "message": "Temperature below absolute zero",
            "details": {"physics_constraint": "temp_c >= -273.15"}
        })
        
        generator = TextReportGenerator()
        text_report = generator.generate(reporter.get_json_report(), "B", "public")
        
        assert "## ACTION NEEDED" in text_report
        assert "Temperature below absolute zero" in text_report
        assert "Constraint violated: temp_c >= -273.15" in text_report
    
    def test_generate_combined_phase_report(self):
        """Test generating report for combined phases."""
        reporter = ValidationReporter()
        
        # Add Phase A item
        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": "param1",
            "message": "Parameter renamed",
            "details": {"old_name": "old", "new_name": "new"}
        })
        
        # Add Phase B item
        reporter.add_warning({
            "phase": "B",
            "type": "science_warning",
            "field_path": "param2",
            "message": "Science warning"
        })
        
        generator = TextReportGenerator()
        text_report = generator.generate(reporter.get_json_report(), "AB", "public")
        
        assert "Phase AB" in text_report
        assert "Parameter renamed" in text_report  # Generic format doesn't include details
        assert "Scientific warning" in text_report
    
    def test_phase_filtering(self):
        """Test that only relevant phase items are included."""
        reporter = ValidationReporter()
        
        # Add Phase A item
        reporter.add_info({
            "phase": "A",
            "type": "info",
            "field_path": "field1",
            "message": "Phase A info"
        })
        
        # Add Phase B item
        reporter.add_info({
            "phase": "B",
            "type": "info",
            "field_path": "field2",
            "message": "Phase B info"
        })
        
        generator = TextReportGenerator()
        
        # Generate Phase A report - should only include Phase A items
        text_report_a = generator.generate(reporter.get_json_report(), "A", "public")
        assert "Phase A info" in text_report_a
        assert "Phase B info" not in text_report_a
        
        # Generate Phase B report - should only include Phase B items
        text_report_b = generator.generate(reporter.get_json_report(), "B", "public")
        assert "Phase A info" not in text_report_b
        assert "Phase B info" in text_report_b
        
        # Generate Phase AB report - should include both
        text_report_ab = generator.generate(reporter.get_json_report(), "AB", "public")
        assert "Phase A info" in text_report_ab
        assert "Phase B info" in text_report_ab
    
    def test_empty_report(self):
        """Test generating report when validation passes with no issues."""
        reporter = ValidationReporter()
        reporter.add_phase_results("C", {"validations_passed": True})
        
        generator = TextReportGenerator()
        text_report = generator.generate(reporter.get_json_report(), "C", "public")
        
        assert "Phase C passed" in text_report
        assert "## ACTION NEEDED" not in text_report
        assert "## NO ACTION NEEDED" not in text_report
    
    def test_save_text_report(self):
        """Test saving text report to file."""
        reporter = ValidationReporter()
        reporter.add_info({
            "phase": "A",
            "type": "info",
            "field_path": "test",
            "message": "Test message"
        })
        
        generator = TextReportGenerator()
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            filepath = Path(f.name)
        
        try:
            generator.save_text_report(
                reporter.get_json_report(),
                filepath,
                "A",
                "public"
            )
            
            # Load and verify
            with open(filepath, 'r') as f:
                content = f.read()
            
            assert "# SUEWS - Phase A" in content
            assert "Test message" in content
        finally:
            filepath.unlink()  # Clean up


if __name__ == "__main__":
    pytest.main([__file__, "-v"])