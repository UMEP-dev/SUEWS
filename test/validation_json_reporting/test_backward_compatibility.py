"""
Test backward compatibility of refactored validation system.

This test ensures that the refactored JSON-based system produces
identical text reports to the original implementation.
"""

import difflib
import json
import tempfile
from pathlib import Path

import pytest
import yaml

from supy.data_model.validation.pipeline.phase_a_parameter_update import (
    annotate_missing_parameters,
)
from supy.data_model.validation.pipeline.phase_a_reporter import (
    annotate_missing_parameters_refactored,
)


class TestBackwardCompatibility:
    """Test that refactored system maintains backward compatibility."""

    def create_test_yaml(self, content: dict, filepath: Path):
        """Helper to create test YAML files."""
        with open(filepath, "w") as f:
            yaml.dump(content, f)

    def normalize_text(self, text: str) -> str:
        """Normalize text for comparison (handle minor formatting differences)."""
        # Remove trailing whitespace and normalize line endings
        lines = [line.rstrip() for line in text.split("\n")]
        return "\n".join(lines)

    def compare_reports(
        self, original: str, refactored: str, show_diff: bool = True
    ) -> bool:
        """Compare two report texts and show differences if any."""
        original_norm = self.normalize_text(original)
        refactored_norm = self.normalize_text(refactored)

        if original_norm != refactored_norm:
            if show_diff:
                print("\n=== Report Differences Found ===")
                diff = difflib.unified_diff(
                    original_norm.splitlines(keepends=True),
                    refactored_norm.splitlines(keepends=True),
                    fromfile="original",
                    tofile="refactored",
                )
                print("".join(diff))
            return False
        return True

    def test_phase_a_missing_critical_parameter(self):
        """Test Phase A with missing critical parameter produces same report."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create test YAMLs
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "netradiationmethod": {"value": 1}
                            # Missing emissionsmethod
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
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                standard_yaml,
            )

            # Run original version
            original_updated = tmpdir / "original_updated.yml"
            original_report = tmpdir / "original_report.txt"

            annotate_missing_parameters(
                str(user_yaml),
                str(standard_yaml),
                str(original_updated),
                str(original_report),
                mode="public",
                phase="A",
            )

            # Run refactored version
            refactored_updated = tmpdir / "refactored_updated.yml"
            refactored_report = tmpdir / "refactored_report.txt"

            annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                str(refactored_updated),
                str(refactored_report),
                mode="public",
                phase="A",
            )

            # Compare text reports
            with open(original_report, "r") as f:
                original_text = f.read()
            with open(refactored_report, "r") as f:
                refactored_text = f.read()

            assert self.compare_reports(original_text, refactored_text), (
                "Text reports should be identical"
            )

            # Verify JSON report was created
            json_report = tmpdir / "refactored_report.json"
            assert json_report.exists(), "JSON report should be created"

            # Verify JSON structure is valid
            with open(json_report, "r") as f:
                json_data = json.load(f)
            assert json_data["schema_version"] == "1.0.0"
            assert json_data["summary"]["total_errors"] > 0

    def test_phase_a_renamed_parameters(self):
        """Test Phase A with renamed parameters produces same report."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create test YAMLs with old parameter name
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "cp": {"value": 1200000},  # Old name
                            "emissionsmethod": {"value": 2},
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
                            "rho_cp": {"value": 1200000},  # New name
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                standard_yaml,
            )

            # Run both versions
            original_report = tmpdir / "original_report.txt"
            annotate_missing_parameters(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(original_report),
                mode="public",
            )

            refactored_report = tmpdir / "refactored_report.txt"
            annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(refactored_report),
                mode="public",
            )

            # Compare reports
            with open(original_report, "r") as f:
                original_text = f.read()
            with open(refactored_report, "r") as f:
                refactored_text = f.read()

            assert self.compare_reports(original_text, refactored_text), (
                "Reports should be identical for renamed parameters"
            )

    def test_phase_a_extra_parameters_public_mode(self):
        """Test Phase A with extra parameters in public mode."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                            "custom_param": {"value": 999},  # Extra
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
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                standard_yaml,
            )

            # Run both versions
            original_report = tmpdir / "original_report.txt"
            annotate_missing_parameters(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(original_report),
                mode="public",
            )

            refactored_report = tmpdir / "refactored_report.txt"
            annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                report_file=str(refactored_report),
                mode="public",
            )

            # Compare reports
            with open(original_report, "r") as f:
                original_text = f.read()
            with open(refactored_report, "r") as f:
                refactored_text = f.read()

            assert self.compare_reports(original_text, refactored_text), (
                "Reports should be identical for extra parameters"
            )

    def test_phase_a_complex_scenario(self):
        """Test Phase A with combination of issues."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Complex scenario with multiple issues
            user_yaml = tmpdir / "user.yml"
            self.create_test_yaml(
                {
                    "model": {
                        "physics": {
                            "cp": {"value": 1200000},  # Renamed
                            "netradiationmethod": {"value": 1},
                            # Missing emissionsmethod (critical)
                            "custom_param": {"value": 123},  # Extra
                        },
                        "optional": {"some_field": {"value": 1}},
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
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                            "storageheatmethod": {"value": 1},  # Missing optional
                        },
                        "optional": {
                            "some_field": {"value": 1},
                            "another_field": {"value": 2},  # Missing optional
                        },
                    }
                },
                standard_yaml,
            )

            # Run both versions
            original_report = tmpdir / "original_report.txt"
            original_updated = tmpdir / "original_updated.yml"

            annotate_missing_parameters(
                str(user_yaml),
                str(standard_yaml),
                str(original_updated),
                str(original_report),
                mode="public",
            )

            refactored_report = tmpdir / "refactored_report.txt"
            refactored_updated = tmpdir / "refactored_updated.yml"

            annotate_missing_parameters_refactored(
                str(user_yaml),
                str(standard_yaml),
                str(refactored_updated),
                str(refactored_report),
                mode="public",
            )

            # Compare reports
            with open(original_report, "r") as f:
                original_text = f.read()
            with open(refactored_report, "r") as f:
                refactored_text = f.read()

            assert self.compare_reports(original_text, refactored_text), (
                "Reports should be identical for complex scenario"
            )

            # Verify JSON captures all issues
            json_report = tmpdir / "refactored_report.json"
            with open(json_report, "r") as f:
                json_data = json.load(f)

            # Should have errors for missing critical and extra param
            assert json_data["summary"]["total_errors"] >= 2
            # Should have info for renamed and optional missing
            assert json_data["summary"]["total_info"] >= 1


def run_compatibility_test():
    """Run a quick compatibility test from command line."""
    import tempfile
    from pathlib import Path

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Create simple test case
        user_yaml = tmpdir / "user.yml"
        with open(user_yaml, "w") as f:
            yaml.dump(
                {
                    "model": {
                        "physics": {
                            "cp": {"value": 1200000},  # Renamed parameter
                            "netradiationmethod": {"value": 1},
                            # Missing emissionsmethod
                        }
                    }
                },
                f,
            )

        standard_yaml = tmpdir / "standard.yml"
        with open(standard_yaml, "w") as f:
            yaml.dump(
                {
                    "model": {
                        "physics": {
                            "rho_cp": {"value": 1200000},
                            "netradiationmethod": {"value": 1},
                            "emissionsmethod": {"value": 2},
                        }
                    }
                },
                f,
            )

        # Run original
        original_report = tmpdir / "original.txt"
        print("Running original Phase A...")
        annotate_missing_parameters(
            str(user_yaml),
            str(standard_yaml),
            report_file=str(original_report),
            mode="public",
        )

        # Run refactored
        refactored_report = tmpdir / "refactored.txt"
        print("Running refactored Phase A...")
        reporter = annotate_missing_parameters_refactored(
            str(user_yaml),
            str(standard_yaml),
            report_file=str(refactored_report),
            mode="public",
        )

        # Compare
        with open(original_report, "r") as f:
            original = f.read()
        with open(refactored_report, "r") as f:
            refactored = f.read()

        if original.strip() == refactored.strip():
            print("\n✅ SUCCESS: Reports are identical!")
            print("\nJSON Report Summary:")
            json_data = reporter.get_json_report()
            print(f"  - Errors: {json_data['summary']['total_errors']}")
            print(f"  - Warnings: {json_data['summary']['total_warnings']}")
            print(f"  - Info: {json_data['summary']['total_info']}")

            # Check JSON file was created
            json_file = Path(str(refactored_report).replace(".txt", ".json"))
            if json_file.exists():
                print(f"\n✅ JSON report saved to: {json_file}")

            return True
        else:
            print("\n❌ FAILURE: Reports differ!")
            print("\nDifferences:")
            diff = difflib.unified_diff(
                original.splitlines(keepends=True),
                refactored.splitlines(keepends=True),
                fromfile="original",
                tofile="refactored",
            )
            print("".join(diff))
            return False


if __name__ == "__main__":
    # Run quick test
    print("=== Testing Backward Compatibility ===\n")
    if run_compatibility_test():
        print("\n=== Running Full Test Suite ===\n")
        pytest.main([__file__, "-v"])
