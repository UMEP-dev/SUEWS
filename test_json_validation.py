#!/usr/bin/env python
"""
Quick test script to see the JSON validation reports in action.
Run this to generate and view JSON reports from Phase A validation.
"""

import json
import tempfile
from pathlib import Path
import yaml

from supy.data_model.validation.pipeline.phase_a_refactored import (
    annotate_missing_parameters_refactored,
)


def create_test_case_1():
    """Create a test case with missing critical parameter."""
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        
        # User YAML missing critical parameter
        user_yaml = tmpdir / "user.yml"
        with open(user_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "netradiationmethod": {"value": 1},
                        # Missing emissionsmethod (critical)
                    }
                }
            }, f)
        
        # Standard YAML with all parameters
        standard_yaml = tmpdir / "standard.yml"
        with open(standard_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "netradiationmethod": {"value": 1},
                        "emissionsmethod": {"value": 2},
                    }
                }
            }, f)
        
        report_txt = tmpdir / "report.txt"
        report_json = tmpdir / "report.json"
        
        print("\n" + "="*60)
        print("TEST CASE 1: Missing Critical Parameter")
        print("="*60)
        
        # Run validation
        reporter = annotate_missing_parameters_refactored(
            str(user_yaml),
            str(standard_yaml),
            report_file=str(report_txt),
            mode="public",
        )
        
        # Display JSON
        json_data = reporter.get_json_report()
        print("\nGenerated JSON Report:")
        print(json.dumps(json_data, indent=2))
        
        # Show summary
        print("\n" + "-"*40)
        print("SUMMARY:")
        print(f"  Total Errors: {json_data['summary']['total_errors']}")
        print(f"  Total Warnings: {json_data['summary']['total_warnings']}")
        print(f"  Total Info: {json_data['summary']['total_info']}")
        print(f"  Validation Passed: {json_data['summary']['validation_passed']}")
        
        # Show action items
        action_items = reporter.get_action_items()
        if action_items:
            print("\nACTION ITEMS:")
            for item in action_items:
                print(f"  - [{item['severity']}] {item['message']}")
        
        # Show text report
        print("\n" + "-"*40)
        print("TEXT REPORT:")
        with open(report_txt, "r") as f:
            print(f.read())
        
        return json_data


def create_test_case_2():
    """Create a test case with renamed parameters and extras."""
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        
        # User YAML with old names and extra parameters
        user_yaml = tmpdir / "user.yml"
        with open(user_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "cp": {"value": 1200000},  # Old name
                        "netradiationmethod": {"value": 1},
                        "custom_param": {"value": 999},  # Extra
                    }
                }
            }, f)
        
        # Standard YAML with new names
        standard_yaml = tmpdir / "standard.yml"
        with open(standard_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "rho_cp": {"value": 1200000},  # New name
                        "netradiationmethod": {"value": 1},
                    }
                }
            }, f)
        
        report_txt = tmpdir / "report.txt"
        report_json = tmpdir / "report.json"
        
        print("\n" + "="*60)
        print("TEST CASE 2: Renamed Parameters + Extra Parameters")
        print("="*60)
        
        # Run validation
        reporter = annotate_missing_parameters_refactored(
            str(user_yaml),
            str(standard_yaml),
            report_file=str(report_txt),
            mode="public",
        )
        
        # Display JSON
        json_data = reporter.get_json_report()
        print("\nGenerated JSON Report:")
        print(json.dumps(json_data, indent=2))
        
        # Show summary
        print("\n" + "-"*40)
        print("SUMMARY:")
        print(f"  Total Errors: {json_data['summary']['total_errors']}")
        print(f"  Total Warnings: {json_data['summary']['total_warnings']}")
        print(f"  Total Info: {json_data['summary']['total_info']}")
        print(f"  Validation Passed: {json_data['summary']['validation_passed']}")
        
        return json_data


def create_test_case_3():
    """Create a complex test case with multiple issues."""
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        
        # Complex user YAML
        user_yaml = tmpdir / "user.yml"
        with open(user_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "cp": {"value": 1200000},  # Renamed
                        "netradiationmethod": {"value": 1},
                        # Missing emissionsmethod (critical)
                        "custom_param": {"value": 123},  # Extra
                    },
                    "optional": {
                        "some_field": {"value": 1}
                    }
                }
            }, f)
        
        # Standard YAML
        standard_yaml = tmpdir / "standard.yml"
        with open(standard_yaml, "w") as f:
            yaml.dump({
                "model": {
                    "physics": {
                        "rho_cp": {"value": 1200000},
                        "netradiationmethod": {"value": 1},
                        "emissionsmethod": {"value": 2},
                        "storageheatmethod": {"value": 1},  # Optional
                    },
                    "optional": {
                        "some_field": {"value": 1},
                        "another_field": {"value": 2},  # Optional
                    }
                }
            }, f)
        
        report_txt = tmpdir / "report.txt"
        
        print("\n" + "="*60)
        print("TEST CASE 3: Complex Scenario (Multiple Issues)")
        print("="*60)
        
        # Run validation in dev mode to see different behaviour
        reporter = annotate_missing_parameters_refactored(
            str(user_yaml),
            str(standard_yaml),
            report_file=str(report_txt),
            mode="dev",  # Using dev mode for this test
        )
        
        # Display JSON
        json_data = reporter.get_json_report()
        print("\nGenerated JSON Report (DEV MODE):")
        print(json.dumps(json_data, indent=2))
        
        # Show categorised issues
        print("\n" + "-"*40)
        print("ISSUES BY CATEGORY:")
        
        if json_data['errors']:
            print("\nERRORS:")
            for error in json_data['errors']:
                print(f"  - {error['message']}")
        
        if json_data['warnings']:
            print("\nWARNINGS:")
            for warning in json_data['warnings']:
                print(f"  - {warning['message']}")
        
        if json_data['info']:
            print("\nINFO:")
            for info in json_data['info']:
                print(f"  - {info['message']}")
        
        return json_data


def save_sample_json():
    """Save a sample JSON report to file for inspection."""
    print("\n" + "="*60)
    print("SAVING SAMPLE JSON REPORTS TO FILES")
    print("="*60)
    
    # Create output directory
    output_dir = Path("sample_json_reports")
    output_dir.mkdir(exist_ok=True)
    
    # Generate and save different scenarios
    scenarios = [
        ("missing_critical", create_test_case_1),
        ("renamed_and_extra", create_test_case_2),
        ("complex_scenario", create_test_case_3),
    ]
    
    for name, test_func in scenarios:
        json_data = test_func()
        output_file = output_dir / f"{name}.json"
        
        with open(output_file, "w") as f:
            json.dump(json_data, f, indent=2)
        
        print(f"\n✅ Saved: {output_file}")
    
    print(f"\nAll JSON reports saved to: {output_dir.absolute()}/")
    print("\nYou can inspect them with:")
    print(f"  cat {output_dir}/missing_critical.json | jq .")
    print(f"  cat {output_dir}/renamed_and_extra.json | jq .")
    print(f"  cat {output_dir}/complex_scenario.json | jq .")


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1 and sys.argv[1] == "--save":
        save_sample_json()
    else:
        # Run all test cases
        create_test_case_1()
        create_test_case_2()
        create_test_case_3()
        
        print("\n" + "="*60)
        print("TIP: Run with --save flag to save JSON reports to files:")
        print("  python test_json_validation.py --save")
        print("="*60)