"""
Integration tests for Phase A refactored to use ValidationReporter.
"""

import json
import tempfile
from pathlib import Path

import pytest
import yaml

from supy.data_model.validation.pipeline.phase_a_refactored import (
    annotate_missing_parameters_refactored,
    build_phase_a_reporter,
)


class TestPhaseAIntegration:
    """Test Phase A integration with ValidationReporter."""

    def create_test_yaml(self, content: dict, filepath: Path):
        """Helper to create test YAML files."""
        with open(filepath, "w") as f:
            yaml.dump(content, f)

    def test_phase_a_missing_critical_parameter(self):
        """Test Phase A detecting missing critical physics parameters."""
        # Create test YAML files
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # User YAML missing critical parameter
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "netradiationmethod": {"value": 1}
                            # Missing emissionsmethod (critical)
                        }
                    }
                },
                user_yaml,
            )

            # Standard YAML with all parameters
            standard_yaml = tmpdir / "standard.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                standard_yaml,
            )

            # Output files
            uptodate_yaml = tmpdir / "updatedA_user.yml"
            report_txt = tmpdir / "reportA_user.txt"

            # Run Phase A with refactored version
            reporter = annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                str(uptodate_yaml),
                str(report_txt),
                mode="public",
                phase="A",
            )

            # Verify reporter structure
            assert reporter.has_errors()
            json_data = reporter.get_json_report()

            # Check for critical parameter error
            errors = json_data.get("errors", [])
            assert len(errors) > 0

            critical_error = next(
                (e for e in errors if e["type"] == "missing_required_parameter"), None
            )
            assert critical_error is not None
            assert "emissionsmethod" in critical_error["field_path"]

            # Verify JSON report was saved
            json_report = tmpdir / "reportA_user.json"
            assert json_report.exists()

            with open(json_report, "r") as f:
                saved_json = json.load(f)
            assert saved_json["schema_version"] == "1.0.0"
            assert saved_json["summary"]["total_errors"] > 0

            # Verify text report was generated
            assert report_txt.exists()
            with open(report_txt, "r") as f:
                text_content = f.read()
            assert "## ACTION NEEDED" in text_content
            assert "emissionsmethod" in text_content

    def test_phase_a_renamed_parameters(self):
        """Test Phase A handling renamed parameters."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # User YAML with old parameter name
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "cp": {"value": 1200000}  # Old name
                        }
                    }
                },
                user_yaml,
            )

            # Standard YAML with new name
            standard_yaml = tmpdir / "standard.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "rho_cp": {"value": 1200000}  # New name
                        }
                    }
                },
                standard_yaml,
            )

            report_txt = tmpdir / "reportA_user.txt"

            # Run Phase A
            reporter = annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(report_txt),
                mode="public",
            )

            # Check for renamed parameter info
            json_data = reporter.get_json_report()
            info_items = json_data.get("info", [])

            renamed_info = next(
                (i for i in info_items if i["type"] == "parameter_renamed"), None
            )
            assert renamed_info is not None
            assert renamed_info["details"]["old_name"] == "cp"
            assert renamed_info["details"]["new_name"] == "rho_cp"

            # Check text report
            with open(report_txt, "r") as f:
                text_content = f.read()
            assert "## NO ACTION NEEDED" in text_content
            assert "cp changed to rho_cp" in text_content

    def test_phase_a_extra_parameters_public_mode(self):
        """Test Phase A handling extra parameters in public mode."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # User YAML with extra parameter
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "netradiationmethod": {"value": 1},
                            "custom_param": {"value": 999},  # Not in standard
                        }
                    }
                },
                user_yaml,
            )

            # Standard YAML without extra parameter
            standard_yaml = tmpdir / "standard.yml"
            self.create_test_yaml(
                {"model": {"physics": {"netradiationmethod": {"value": 1}}}},
                standard_yaml,
            )

            report_txt = tmpdir / "reportA_user.txt"

            # Run Phase A in public mode
            reporter = annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(report_txt),
                mode="public",
            )

            # In public mode, extra parameters should be errors
            assert reporter.has_errors()
            json_data = reporter.get_json_report()

            extra_error = next(
                (e for e in json_data["errors"] if "custom_param" in e["field_path"]),
                None,
            )
            assert extra_error is not None
            assert extra_error["type"] == "extra_parameter_forbidden"

            # Check text report
            with open(report_txt, "r") as f:
                text_content = f.read()
            assert "## ACTION NEEDED" in text_content
            assert "custom_param" in text_content
            assert "not allowed extra parameter" in text_content.lower()

    def test_phase_a_json_text_consistency(self):
        """Test that JSON and text reports contain consistent information."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create test scenario with multiple issues
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "cp": {"value": 1200000},  # Renamed
                            # Missing emissionsmethod
                            "extra_param": {"value": 123},  # Extra
                        }
                    }
                },
                user_yaml,
            )

            standard_yaml = tmpdir / "standard.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "rho_cp": {"value": 1200000},
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                standard_yaml,
            )

            report_txt = tmpdir / "reportA_user.txt"
            report_json = tmpdir / "reportA_user.json"

            # Run Phase A
            reporter = annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(report_txt),
                mode="public",
            )

            # Load both reports
            with open(report_txt, "r") as f:
                text_content = f.read()

            with open(report_json, "r") as f:
                json_content = json.load(f)

            # Verify consistency
            # 1. Check error count matches
            assert (
                json_content["summary"]["total_errors"] == 2
            )  # missing critical + extra param
            assert "ACTION NEEDED" in text_content

            # 2. Check all JSON errors appear in text
            for error in json_content["errors"]:
                param_name = error.get("details", {}).get("parameter_name", "")
                if param_name:
                    assert param_name in text_content

            # 3. Check info items
            assert json_content["summary"]["total_info"] == 1  # renamed parameter
            for info in json_content["info"]:
                if info["type"] == "parameter_renamed":
                    old_name = info["details"]["old_name"]
                    new_name = info["details"]["new_name"]
                    assert f"{old_name} changed to {new_name}" in text_content


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
