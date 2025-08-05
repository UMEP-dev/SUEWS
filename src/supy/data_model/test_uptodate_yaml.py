"""
Comprehensive test suite for uptodate_yaml.py

This test suite validates all functionality of the YAML configuration updating script,
using the standard sample_config.yml as the reference configuration.

Tests cover:
- Missing parameter detection (URGENT physics and optional parameters)
- Renamed parameter handling (OUTDATED → RENAMED IN STANDARD)
- NOT IN STANDARD parameter detection
- Clean YAML generation with null placeholders
- Analysis report generation
- Edge cases and error handling
"""

import unittest
import tempfile
import os
import yaml
from pathlib import Path

# Import the functions we want to test
from uptodate_yaml import (
    find_missing_parameters,
    find_extra_parameters,
    handle_renamed_parameters,
    is_physics_option,
    get_null_placeholder,
    create_clean_missing_param_annotation,
    create_uptodate_yaml_with_missing_params,
    create_analysis_report,
    annotate_missing_parameters,
    RENAMED_PARAMS,
    PHYSICS_OPTIONS,
)


class TestUptodateYaml(unittest.TestCase):
    """Test suite for uptodate_yaml.py functionality."""

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures using real sample_config.yml."""
        # Path to the standard configuration file (relative to project root)
        cls.standard_file = "../sample_run/sample_config.yml"

        # Load the standard configuration
        with open(cls.standard_file, "r") as f:
            cls.standard_data = yaml.safe_load(f)

        # Create test data scenarios
        cls.create_test_scenarios()

    @classmethod
    def create_test_scenarios(cls):
        """Create various test YAML scenarios."""

        # Scenario 1: Missing physics parameter (URGENT)
        cls.missing_physics_yaml = {
            "name": "test config",
            "model": {
                "control": {"tstep": 300},
                "physics": {
                    "emissionsmethod": {"value": 2},
                    # netradiationmethod missing - URGENT!
                    "storageheatmethod": {"value": 1},
                },
            },
            "sites": [{"name": "test", "gridiv": 1}],
        }

        # Scenario 2: Missing optional parameter
        cls.missing_optional_yaml = {
            "name": "test config",
            "model": {
                "control": {"tstep": 300},
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "emissionsmethod": {"value": 2},
                },
            },
            "sites": [
                {
                    "name": "test",
                    "gridiv": 1,
                    "properties": {
                        "irrigation": {
                            "wuprofm_24hr": {
                                # holiday parameter missing (optional)
                                "working_day": {"1": -999.0}
                            }
                        }
                    },
                }
            ],
        }

        # Scenario 3: Renamed parameters
        renamed_yaml_content = """
name: test config
model:
  control:
    tstep: 300
  physics:
    netradiationmethod:
      value: 3
    emissionsmethod:
      value: 2
    diagmethod:  # OLD NAME - should be rslmethod
      value: 2
    cp:  # OLD NAME - should be rho_cp
      value: 1005
sites:
- name: test
  gridiv: 1
"""
        cls.renamed_yaml_content = renamed_yaml_content.strip()
        cls.renamed_yaml_data = yaml.safe_load(renamed_yaml_content)

        # Scenario 4: NOT IN STANDARD parameters
        cls.extra_params_yaml = {
            "name": "test config",
            "model": {
                "control": {
                    "tstep": 300,
                    "custom_param": "hello",  # NOT IN STANDARD
                },
                "physics": {"netradiationmethod": {"value": 3}},
            },
            "sites": [
                {
                    "name": "test",
                    "gridiv": 1,
                    "properties": {
                        "extra_property": {"test": 123}  # NOT IN STANDARD
                    },
                }
            ],
        }

        # Scenario 5: Complete minimal YAML (no missing parameters)
        cls.complete_yaml = {
            "name": "complete config",
            "model": cls.standard_data["model"].copy(),
            "sites": [
                {
                    "name": "test",
                    "gridiv": 1,
                    "properties": cls.standard_data["sites"][0]["properties"].copy(),
                }
            ],
        }

    def test_physics_option_classification(self):
        """Test that physics options are correctly identified as URGENT."""
        # Test known physics options
        self.assertTrue(is_physics_option("model.physics.netradiationmethod"))
        self.assertTrue(is_physics_option("model.physics.emissionsmethod"))
        self.assertTrue(is_physics_option("model.physics.gsmodel"))

        # Test non-physics options
        self.assertFalse(is_physics_option("model.control.tstep"))
        self.assertFalse(is_physics_option("sites.properties.irrigation.holiday"))
        self.assertFalse(is_physics_option("sites.name"))

        # Test that all PHYSICS_OPTIONS entries are classified correctly
        for physics_param in PHYSICS_OPTIONS:
            self.assertTrue(is_physics_option(f"model.physics.{physics_param}"))

    def test_missing_parameter_detection_physics(self):
        """Test detection of missing URGENT physics parameters."""
        missing_params = find_missing_parameters(
            self.missing_physics_yaml, self.standard_data
        )

        # Filter for physics parameters
        physics_missing = [
            (path, val, is_physics)
            for path, val, is_physics in missing_params
            if is_physics
        ]

        # Should find netradiationmethod as missing and URGENT
        physics_paths = [path for path, _, _ in physics_missing]
        self.assertIn("model.physics.netradiationmethod", physics_paths)

        # Verify it's marked as physics (URGENT)
        netradiationmethod_entry = next(
            (
                item
                for item in missing_params
                if item[0] == "model.physics.netradiationmethod"
            ),
            None,
        )
        self.assertIsNotNone(netradiationmethod_entry)
        self.assertTrue(
            netradiationmethod_entry[2],
            "netradiationmethod should be marked as physics/URGENT",
        )

    def test_missing_parameter_detection_optional(self):
        """Test detection of missing optional parameters."""
        missing_params = find_missing_parameters(
            self.missing_optional_yaml, self.standard_data
        )

        # Filter for non-physics parameters
        optional_missing = [
            (path, val, is_physics)
            for path, val, is_physics in missing_params
            if not is_physics
        ]

        # Should find many missing optional parameters
        self.assertGreater(
            len(optional_missing), 0, "Should find missing optional parameters"
        )

        # All should be marked as non-physics (optional)
        for _, _, is_physics in optional_missing:
            self.assertFalse(
                is_physics, "Optional parameters should not be marked as physics"
            )

    def test_renamed_parameter_handling(self):
        """Test handling of renamed parameters."""
        updated_content, replacements = handle_renamed_parameters(
            self.renamed_yaml_content
        )

        # Should find the renamed parameters
        replacement_dict = dict(replacements)
        self.assertIn("diagmethod", replacement_dict)
        self.assertIn("cp", replacement_dict)
        self.assertEqual(replacement_dict["diagmethod"], "rslmethod")
        self.assertEqual(replacement_dict["cp"], "rho_cp")

        # Updated content should contain new names
        self.assertIn("rslmethod:", updated_content)
        self.assertIn("rho_cp:", updated_content)
        self.assertIn("#RENAMED IN STANDARD", updated_content)

    def test_extra_parameter_detection(self):
        """Test detection of NOT IN STANDARD parameters."""
        extra_params = find_extra_parameters(self.extra_params_yaml, self.standard_data)

        # Should find the extra parameters
        self.assertIn("model.control.custom_param", extra_params)
        self.assertIn("sites[0].properties.extra_property", extra_params)

        # Should not find standard parameters as extra
        standard_paths = [
            path
            for path in extra_params
            if "tstep" in path or "netradiationmethod" in path
        ]
        self.assertEqual(
            len(standard_paths), 0, "Standard parameters should not be marked as extra"
        )

    def test_null_placeholder_generation(self):
        """Test that null placeholders are generated correctly."""
        null_value = get_null_placeholder()
        self.assertEqual(null_value, "null")

    def test_clean_annotation_generation(self):
        """Test generation of clean parameter annotations."""
        # Test simple parameter
        lines = create_clean_missing_param_annotation("test_param", 123)
        self.assertEqual(lines, ["test_param: null"])

        # Test complex parameter (dict)
        complex_value = {"value": 123, "units": "test"}
        lines = create_clean_missing_param_annotation("complex_param", complex_value)
        expected = ["complex_param:", "  value: null", "  units: null"]
        self.assertEqual(lines, expected)

    def test_uptodate_yaml_generation(self):
        """Test generation of clean uptodate YAML."""
        # Create test missing parameters
        missing_params = [("model.physics.netradiationmethod", {"value": 3}, True)]

        yaml_content = (
            "name: test\nmodel:\n  physics:\n    emissionsmethod:\n      value: 2"
        )

        result = create_uptodate_yaml_with_missing_params(yaml_content, missing_params)

        # Should contain header
        self.assertIn("UP TO DATE YAML", result)
        self.assertIn("uptodate_yaml.py", result)

        # Should contain null value for missing parameter
        self.assertIn("netradiationmethod:", result)
        self.assertIn("value: null", result)

        # Should not contain inline comments in clean output
        self.assertNotIn("#RENAMED IN STANDARD", result)

    def test_analysis_report_generation(self):
        """Test generation of analysis report."""
        missing_params = [
            ("model.physics.netradiationmethod", {"value": 3}, True),  # URGENT
            ("sites[0].properties.irrigation.holiday", {"1": -999}, False),  # Optional
        ]
        renamed_params = [("diagmethod", "rslmethod")]
        extra_params = ["model.control.custom_param"]

        report = create_analysis_report(missing_params, renamed_params, extra_params)

        # Should contain all sections
        self.assertIn("SUEWS Configuration Analysis Report", report)
        self.assertIn("## Summary", report)
        self.assertIn("MISSING IN STANDARD Parameters", report)
        self.assertIn("RENAMED IN STANDARD Parameters", report)
        self.assertIn("NOT IN STANDARD Parameters", report)
        self.assertIn("## Next Steps", report)

        # Should properly categorize URGENT vs optional
        self.assertIn("URGENT", report)
        self.assertIn("physics options", report)
        self.assertIn("netradiationmethod", report)

        # Should contain renamed parameter info
        self.assertIn("diagmethod -> rslmethod", report)

        # Should contain extra parameter info
        self.assertIn("custom_param", report)

    def test_complete_workflow_with_files(self):
        """Test complete workflow using temporary files."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create temporary user file with issues
            user_file = os.path.join(temp_dir, "user.yml")
            with open(user_file, "w") as f:
                yaml.dump(self.missing_physics_yaml, f)

            # Create output file paths
            uptodate_file = os.path.join(temp_dir, "uptodate_user.yml")
            report_file = os.path.join(temp_dir, "report_user.txt")

            # Run the complete workflow
            annotate_missing_parameters(
                user_file=user_file,
                standard_file=self.standard_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
            )

            # Verify output files were created
            self.assertTrue(os.path.exists(uptodate_file))
            self.assertTrue(os.path.exists(report_file))

            # Verify uptodate file content
            with open(uptodate_file, "r") as f:
                uptodate_content = f.read()

            self.assertIn("UP TO DATE YAML", uptodate_content)
            self.assertIn("netradiationmethod:", uptodate_content)
            self.assertIn("value: null", uptodate_content)

            # Verify report file content
            with open(report_file, "r") as f:
                report_content = f.read()

            self.assertIn("SUEWS Configuration Analysis Report", report_content)
            self.assertIn("URGENT", report_content)
            self.assertIn("netradiationmethod", report_content)

    def test_no_missing_parameters_scenario(self):
        """Test behavior when no parameters are missing."""
        # Use complete YAML with all required parameters
        missing_params = find_missing_parameters(self.complete_yaml, self.standard_data)

        # Should find many missing parameters since complete_yaml is minimal
        # but test that the function handles complete scenarios

        # Test with truly complete standard data
        minimal_standard = {
            "name": "standard",
            "model": {"control": {"tstep": 300}},
            "sites": [{"name": "test", "gridiv": 1}],
        }

        minimal_user = {
            "name": "user",
            "model": {"control": {"tstep": 300}},
            "sites": [{"name": "test", "gridiv": 1}],
        }

        missing = find_missing_parameters(minimal_user, minimal_standard)
        self.assertEqual(
            len(missing), 0, "Should find no missing parameters in complete match"
        )

    def test_error_handling(self):
        """Test error handling for invalid inputs."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Test with non-existent file
            non_existent = os.path.join(temp_dir, "does_not_exist.yml")
            result = annotate_missing_parameters(
                user_file=non_existent, standard_file=self.standard_file
            )
            # Should handle gracefully (returns None)
            self.assertIsNone(result)

            # Test with invalid YAML
            invalid_yaml = os.path.join(temp_dir, "invalid.yml")
            with open(invalid_yaml, "w") as f:
                f.write("invalid: yaml: content: [unclosed")

            result = annotate_missing_parameters(
                user_file=invalid_yaml, standard_file=self.standard_file
            )
            # Should handle gracefully
            self.assertIsNone(result)

    def test_renamed_params_consistency(self):
        """Test that RENAMED_PARAMS dictionary is consistent."""
        # All old names should be different from new names
        for old_name, new_name in RENAMED_PARAMS.items():
            self.assertNotEqual(
                old_name,
                new_name,
                f"Renamed parameter {old_name} -> {new_name} should be different",
            )

        # Should contain expected mappings
        expected_mappings = {"cp": "rho_cp", "diagmethod": "rslmethod"}

        for old, new in expected_mappings.items():
            self.assertIn(old, RENAMED_PARAMS)
            self.assertEqual(RENAMED_PARAMS[old], new)

    def test_physics_options_completeness(self):
        """Test that PHYSICS_OPTIONS set contains expected physics parameters."""
        # Should contain key physics options that we know exist in sample_config
        expected_physics_options = {
            "netradiationmethod",
            "emissionsmethod",
            "storageheatmethod",
            "gsmodel",
            "snowuse",
        }

        for option in expected_physics_options:
            self.assertIn(
                option,
                PHYSICS_OPTIONS,
                f"Physics option {option} should be in PHYSICS_OPTIONS",
            )

        # All entries should be strings
        for option in PHYSICS_OPTIONS:
            self.assertIsInstance(
                option, str, f"Physics option {option} should be a string"
            )


class TestRealWorldScenarios(unittest.TestCase):
    """Test real-world scenarios with actual sample_config.yml dependencies."""

    def setUp(self):
        """Set up with real standard configuration."""
        self.standard_file = "../sample_run/sample_config.yml"
        with open(self.standard_file, "r") as f:
            self.standard_data = yaml.safe_load(f)

    def test_benchmark_configuration_compatibility(self):
        """Test compatibility with benchmark configurations."""
        # This test ensures our script works with real SUEWS configurations
        # Create a simplified version of a typical user configuration

        user_config = {
            "name": "benchmark test",
            "description": "test configuration",
            "model": {
                "control": {
                    "tstep": 300,
                    "forcing_file": {"value": "test_forcing.txt"},
                    "output_file": {"format": "txt", "freq": 3600, "groups": ["SUEWS"]},
                    "start_time": "2011-01-01",
                    "end_time": "2011-12-31",
                },
                "physics": {
                    # Missing some physics parameters to test detection
                    "emissionsmethod": {"value": 2},
                    "storageheatmethod": {"value": 1},
                    # netradiationmethod missing - should be detected as URGENT
                },
            },
            "sites": [
                {
                    "name": "TestSite",
                    "gridiv": 1,
                    "properties": {
                        # Minimal properties - many will be missing
                        "alb": {"value": 0.15}
                    },
                }
            ],
        }

        # Test missing parameter detection
        missing_params = find_missing_parameters(user_config, self.standard_data)

        # Should find netradiationmethod as URGENT
        urgent_params = [
            (path, val, is_physics)
            for path, val, is_physics in missing_params
            if is_physics
        ]
        urgent_paths = [path for path, _, _ in urgent_params]

        self.assertIn(
            "model.physics.netradiationmethod",
            urgent_paths,
            "Should detect missing netradiationmethod as URGENT",
        )

        # Should find many optional parameters
        optional_params = [
            (path, val, is_physics)
            for path, val, is_physics in missing_params
            if not is_physics
        ]
        self.assertGreater(
            len(optional_params), 0, "Should find missing optional parameters"
        )

    def test_standard_config_integrity(self):
        """Test that the standard configuration file is valid and complete."""
        # Verify standard config loads properly
        self.assertIsInstance(self.standard_data, dict)
        self.assertIn("name", self.standard_data)
        self.assertIn("model", self.standard_data)
        self.assertIn("sites", self.standard_data)

        # Verify physics section exists and contains expected parameters
        physics = self.standard_data["model"]["physics"]
        for physics_param in [
            "netradiationmethod",
            "emissionsmethod",
            "storageheatmethod",
        ]:
            self.assertIn(
                physics_param,
                physics,
                f"Standard config should contain {physics_param}",
            )

        # Verify sites structure
        self.assertIsInstance(self.standard_data["sites"], list)
        self.assertGreater(len(self.standard_data["sites"]), 0)

        site = self.standard_data["sites"][0]
        self.assertIn("name", site)
        self.assertIn("properties", site)


class TestEndToEndWorkflow(unittest.TestCase):
    """Comprehensive end-to-end workflow testing."""

    def test_complete_workflow_all_scenarios(self):
        """Test complete end-to-end workflow covering all scenarios at once."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create a comprehensive user YAML that has all types of issues
            problematic_yaml_content = """
name: comprehensive test config
description: A test config with all types of issues
model:
  control:
    tstep: 300
    forcing_file:
      value: test_forcing.txt
    output_file:
      format: txt
      freq: 3600
      groups: ["SUEWS"]
    start_time: "2011-01-01"
    end_time: "2011-12-31"
    custom_control_param: "hello world"  # NOT IN STANDARD
  physics:
    # Missing netradiationmethod - URGENT!
    emissionsmethod:
      value: 2
    storageheatmethod:
      value: 1
    diagmethod:  # OLD NAME - should be rslmethod
      value: 2
    cp:  # OLD NAME - should be rho_cp
      value: 1005
    custom_physics_param:  # NOT IN STANDARD
      value: 999
sites:
- name: ComprehensiveTestSite
  gridiv: 1
  properties:
    alb:
      value: 0.15
    emis:
      value: 0.95
    # Missing many optional parameters like irrigation.wuprofm_24hr.holiday
    custom_site_property:  # NOT IN STANDARD
      test_value: 123
      nested_custom:
        deep_custom: "not in standard"
"""

            # Write the problematic user config
            user_file = os.path.join(temp_dir, "comprehensive_user.yml")
            with open(user_file, "w") as f:
                f.write(problematic_yaml_content.strip())

            # Define output files
            uptodate_file = os.path.join(temp_dir, "uptodate_comprehensive_user.yml")
            report_file = os.path.join(temp_dir, "report_comprehensive_user.txt")

            # Run the complete workflow
            standard_file = "../sample_run/sample_config.yml"
            annotate_missing_parameters(
                user_file=user_file,
                standard_file=standard_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
            )

            # === VERIFY OUTPUT FILES EXIST ===
            self.assertTrue(
                os.path.exists(uptodate_file), "Uptodate YAML file should be created"
            )
            self.assertTrue(
                os.path.exists(report_file), "Report file should be created"
            )

            # === VERIFY UPTODATE YAML CONTENT ===
            with open(uptodate_file, "r") as f:
                uptodate_content = f.read()

            # Should contain header
            self.assertIn("UP TO DATE YAML", uptodate_content)
            self.assertIn("uptodate_yaml.py", uptodate_content)

            # Should have RENAMED parameters updated (no old names)
            self.assertNotIn(
                "diagmethod:", uptodate_content, "Old parameter name should be replaced"
            )
            self.assertNotIn(
                "cp:", uptodate_content, "Old parameter name should be replaced"
            )
            self.assertIn(
                "rslmethod:", uptodate_content, "New parameter name should be present"
            )
            self.assertIn(
                "rho_cp:", uptodate_content, "New parameter name should be present"
            )

            # Should have MISSING parameters added with null values
            self.assertIn(
                "netradiationmethod:",
                uptodate_content,
                "Missing URGENT parameter should be added",
            )
            uptodate_yaml_data = yaml.safe_load(uptodate_content)
            self.assertEqual(
                uptodate_yaml_data["model"]["physics"]["netradiationmethod"]["value"],
                None,
                "Missing parameter should have null value",
            )

            # Should contain NOT IN STANDARD parameters (preserved)
            self.assertIn(
                "custom_control_param:",
                uptodate_content,
                "NOT IN STANDARD params should be preserved",
            )
            self.assertIn(
                "custom_physics_param:",
                uptodate_content,
                "NOT IN STANDARD params should be preserved",
            )
            self.assertIn(
                "custom_site_property:",
                uptodate_content,
                "NOT IN STANDARD params should be preserved",
            )

            # Should NOT contain inline comments in clean YAML
            self.assertNotIn(
                "#RENAMED IN STANDARD",
                uptodate_content,
                "Clean YAML should have no inline comments",
            )
            self.assertNotIn(
                "#NOT IN STANDARD",
                uptodate_content,
                "Clean YAML should have no inline comments",
            )
            self.assertNotIn(
                "#MISSING",
                uptodate_content,
                "Clean YAML should have no inline comments",
            )

            # === VERIFY REPORT CONTENT ===
            with open(report_file, "r") as f:
                report_content = f.read()

            # Should contain all sections
            self.assertIn("SUEWS Configuration Analysis Report", report_content)
            self.assertIn("## Summary", report_content)
            self.assertIn("MISSING IN STANDARD Parameters", report_content)
            self.assertIn("RENAMED IN STANDARD Parameters", report_content)
            self.assertIn("NOT IN STANDARD Parameters", report_content)
            self.assertIn("## Next Steps", report_content)

            # Should properly identify URGENT vs optional missing parameters
            self.assertIn(
                "URGENT", report_content, "Should identify URGENT physics parameters"
            )
            self.assertIn(
                "netradiationmethod",
                report_content,
                "Should list missing physics parameter",
            )
            self.assertIn(
                "physics options", report_content, "Should categorize physics options"
            )

            # Should list renamed parameters with old -> new mapping
            self.assertIn(
                "diagmethod -> rslmethod",
                report_content,
                "Should show parameter renaming",
            )
            self.assertIn(
                "cp -> rho_cp", report_content, "Should show parameter renaming"
            )

            # Should list NOT IN STANDARD parameters
            self.assertIn(
                "custom_control_param",
                report_content,
                "Should list extra control parameter",
            )
            self.assertIn(
                "custom_physics_param",
                report_content,
                "Should list extra physics parameter",
            )
            self.assertIn(
                "custom_site_property",
                report_content,
                "Should list extra site parameter",
            )

            # Should contain usage instructions with manual link
            self.assertIn(
                "https://suews.readthedocs.io/latest/",
                report_content,
                "Should contain manual link",
            )
            self.assertIn(
                "uptodate_user.yml", report_content, "Should reference output file"
            )

            # === VERIFY DATA COMPLETENESS ===
            # Load both original and updated YAML for comparison
            with open(user_file, "r") as f:
                original_data = yaml.safe_load(f.read())

            updated_data = yaml.safe_load(uptodate_content)

            # Original user data should be preserved
            self.assertEqual(updated_data["name"], original_data["name"])
            self.assertEqual(updated_data["description"], original_data["description"])
            self.assertEqual(
                updated_data["model"]["control"]["tstep"],
                original_data["model"]["control"]["tstep"],
            )

            # NOT IN STANDARD parameters should be preserved
            self.assertEqual(
                updated_data["model"]["control"]["custom_control_param"],
                original_data["model"]["control"]["custom_control_param"],
            )
            self.assertEqual(
                updated_data["model"]["physics"]["custom_physics_param"],
                original_data["model"]["physics"]["custom_physics_param"],
            )

            # RENAMED parameters should be updated
            self.assertNotIn(
                "diagmethod",
                updated_data["model"]["physics"],
                "Old parameter should be removed",
            )
            self.assertNotIn(
                "cp",
                updated_data["model"]["physics"],
                "Old parameter should be removed",
            )
            self.assertIn(
                "rslmethod",
                updated_data["model"]["physics"],
                "New parameter should be added",
            )
            self.assertIn(
                "rho_cp",
                updated_data["model"]["physics"],
                "New parameter should be added",
            )

            # Values should be preserved during renaming
            self.assertEqual(updated_data["model"]["physics"]["rslmethod"]["value"], 2)
            self.assertEqual(updated_data["model"]["physics"]["rho_cp"]["value"], 1005)

            # MISSING parameters should be added with null values
            self.assertIn(
                "netradiationmethod",
                updated_data["model"]["physics"],
                "Missing parameter should be added",
            )
            self.assertIsNone(
                updated_data["model"]["physics"]["netradiationmethod"]["value"],
                "Missing parameter should have null value",
            )

            # === VERIFY COUNTS AND STATISTICS ===
            # Parse the report to verify statistics
            lines = report_content.split("\n")
            summary_section = []
            in_summary = False
            for line in lines:
                if "## Summary" in line:
                    in_summary = True
                    continue
                elif line.startswith("##") and in_summary:
                    break
                elif in_summary:
                    summary_section.append(line)

            summary_text = "\n".join(summary_section)

            # Should report correct counts
            self.assertIn(
                "MISSING IN STANDARD parameters",
                summary_text,
                "Should count missing parameters",
            )
            self.assertIn(
                "RENAMED IN STANDARD parameters",
                summary_text,
                "Should count renamed parameters",
            )
            self.assertIn(
                "NOT IN STANDARD parameters",
                summary_text,
                "Should count extra parameters",
            )
            self.assertIn("URGENT:", summary_text, "Should count urgent parameters")

            # === VERIFY YAML VALIDITY ===
            # Final verification that the output YAML is valid and parseable
            try:
                yaml.safe_load(uptodate_content)
            except yaml.YAMLError as e:
                self.fail(f"Generated YAML should be valid, but got error: {e}")

            # === VERIFY FILE STRUCTURE INTEGRITY ===
            # Ensure the updated YAML maintains proper structure
            self.assertIn("model", updated_data, "Should maintain model section")
            self.assertIn(
                "control", updated_data["model"], "Should maintain control section"
            )
            self.assertIn(
                "physics", updated_data["model"], "Should maintain physics section"
            )
            self.assertIn("sites", updated_data, "Should maintain sites section")
            self.assertIsInstance(updated_data["sites"], list, "Sites should be a list")
            self.assertGreater(
                len(updated_data["sites"]), 0, "Should have at least one site"
            )

            print(f"\n✅ End-to-end test completed successfully!")
            print(
                f"   - Generated uptodate YAML: {os.path.getsize(uptodate_file)} bytes"
            )
            print(f"   - Generated report: {os.path.getsize(report_file)} bytes")
            print(f"   - All scenarios tested: MISSING, RENAMED, NOT IN STANDARD")

    def test_workflow_performance_and_scalability(self):
        """Test workflow performance with larger configurations."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create a larger test configuration
            large_config = {
                "name": "large test config",
                "model": {
                    "control": {"tstep": 300},
                    "physics": {
                        "emissionsmethod": {"value": 2},
                        # netradiationmethod missing (URGENT)
                    },
                },
                "sites": [],
            }

            # Add multiple sites with various missing parameters
            for i in range(10):  # 10 sites to test scalability
                site = {
                    "name": f"Site_{i}",
                    "gridiv": 1,
                    "properties": {
                        "alb": {"value": 0.15},
                        # Many optional parameters will be missing
                        f"custom_param_{i}": {
                            "value": f"custom_value_{i}"
                        },  # NOT IN STANDARD
                    },
                }
                large_config["sites"].append(site)

            # Write large config
            user_file = os.path.join(temp_dir, "large_user.yml")
            with open(user_file, "w") as f:
                yaml.dump(large_config, f)

            uptodate_file = os.path.join(temp_dir, "uptodate_large_user.yml")
            report_file = os.path.join(temp_dir, "report_large_user.txt")

            # Measure performance
            import time

            start_time = time.time()

            annotate_missing_parameters(
                user_file=user_file,
                standard_file="../sample_run/sample_config.yml",
                uptodate_file=uptodate_file,
                report_file=report_file,
            )

            end_time = time.time()
            processing_time = end_time - start_time

            # Verify files were created
            self.assertTrue(os.path.exists(uptodate_file))
            self.assertTrue(os.path.exists(report_file))

            # Verify content correctness even with larger scale
            with open(uptodate_file, "r") as f:
                uptodate_content = f.read()

            # Should handle all sites correctly
            for i in range(10):
                self.assertIn(f"Site_{i}", uptodate_content, f"Should contain Site_{i}")
                self.assertIn(
                    f"custom_param_{i}",
                    uptodate_content,
                    f"Should preserve custom_param_{i}",
                )

            # Should still add missing URGENT parameter
            self.assertIn("netradiationmethod:", uptodate_content)

            print(f"\n✅ Performance test completed!")
            print(f"   - Processing time: {processing_time:.3f} seconds")
            print(f"   - Sites processed: 10")
            print(f"   - Output file size: {os.path.getsize(uptodate_file)} bytes")

            # Performance should be reasonable (less than 10 seconds for this scale)
            self.assertLess(
                processing_time,
                10.0,
                "Processing should complete within reasonable time",
            )


if __name__ == "__main__":
    # Run the test suite
    unittest.main(verbosity=2)
