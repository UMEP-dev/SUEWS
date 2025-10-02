#!/usr/bin/env python3
"""
Test script to verify validation pipeline changes.
Run this to ensure our changes to phase naming and redundant dictionaries work correctly.
"""

import subprocess
import sys


def run_test(test_path):
    """Run a specific test and return success status."""
    print(f"\n{'=' * 60}")
    print(f"Running: {test_path}")
    print("=" * 60)

    result = subprocess.run(
        [sys.executable, "-m", "pytest", test_path, "-v", "-x"],
        capture_output=True,
        text=True,
    )

    if result.returncode == 0:
        print(f"✅ PASSED: {test_path}")
        # Show summary
        for line in result.stdout.split("\n"):
            if "passed" in line or "PASSED" in line:
                print(f"  {line.strip()}")
        return True
    else:
        print(f"❌ FAILED: {test_path}")
        # Show error details
        print("Error output:")
        print(result.stdout[-2000:] if len(result.stdout) > 2000 else result.stdout)
        return False


def main():
    """Run key tests for validation pipeline."""

    tests = [
        # Test Phase A (Configuration structure check)
        "test/data_model/test_yaml_processing.py::TestPhaseAUptoDateYaml::test_phase_a_basic_validation",
        "test/data_model/test_yaml_processing.py::TestPhaseAUptoDateYaml::test_phase_a_report_generation",
        # Test Phase B (Physics validation check)
        "test/data_model/test_yaml_processing.py::TestPhaseBScienceCheck::test_phase_b_basic_validation",
        "test/data_model/test_yaml_processing.py::TestPhaseBScienceCheck::test_phase_b_report_generation",
        # Test Phase C (Configuration consistency check)
        "test/data_model/test_yaml_processing.py::TestPhaseCPydanticValidation::test_phase_c_basic_validation",
        "test/data_model/test_yaml_processing.py::TestPhaseCReporting::test_phase_c_report_format",
        # Test Orchestrator (multi-phase workflows)
        "test/data_model/test_yaml_processing.py::TestSuewsYamlProcessorOrchestrator::test_orchestrator_phase_abc",
        "test/data_model/test_yaml_processing.py::TestSuewsYamlProcessorOrchestrator::test_consolidated_report_generation",
        # Test report generation
        "test/data_model/test_yaml_processing.py::TestEndToEndWorkflow::test_complete_workflow_with_reports",
    ]

    print("Testing validation pipeline changes...")
    print(f"Running {len(tests)} key tests")

    all_passed = True
    passed_count = 0

    for test in tests:
        if run_test(test):
            passed_count += 1
        else:
            all_passed = False
            # Continue running remaining tests to see full picture

    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Tests run: {len(tests)}")
    print(f"Passed: {passed_count}")
    print(f"Failed: {len(tests) - passed_count}")

    if all_passed:
        print("\n✅ ALL TESTS PASSED - Changes are working correctly!")
        return 0
    else:
        print("\n❌ SOME TESTS FAILED - Please review the changes")
        return 1


if __name__ == "__main__":
    sys.exit(main())
