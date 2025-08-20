#!/usr/bin/env python3
"""
CI validation script using structured JSON output.

This script demonstrates how to use the new JSON output format
from suews-validate in CI/CD pipelines.
"""

import json
import subprocess
import sys
from pathlib import Path
from typing import List, Dict, Any

def run_validation(config_files: List[str]) -> Dict[str, Any]:
    """Run suews-validate with JSON output and parse results."""
    
    # Run the validation command
    cmd = [
        "suews-validate",
        "validate",
        "--format", "json"
    ] + config_files
    
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=False  # Don't raise on non-zero exit
        )
        
        # Parse JSON output
        if result.stdout:
            return json.loads(result.stdout)
        else:
            # Command failed to produce output
            return {
                "status": "error",
                "message": result.stderr or "Validation command failed"
            }
            
    except json.JSONDecodeError as e:
        return {
            "status": "error",
            "message": f"Failed to parse JSON output: {e}"
        }
    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to run validation: {e}"
        }

def format_github_annotations(validation_result: Dict[str, Any]) -> None:
    """Format validation errors as GitHub Actions annotations."""
    
    if validation_result.get("status") == "error":
        print(f"::error::Validation failed: {validation_result.get('message')}")
        return
    
    # Process each file's results
    for file_result in validation_result.get("results", []):
        filename = file_result["file"]
        
        if not file_result["valid"]:
            for error in file_result.get("errors", []):
                if isinstance(error, dict):
                    # Structured error with code
                    code = error.get("code_name", "UNKNOWN")
                    message = error.get("message", "Unknown error")
                    field = error.get("field", "")
                    
                    # Format as GitHub annotation
                    if field:
                        print(f"::error file={filename}::[{code}] {field}: {message}")
                    else:
                        print(f"::error file={filename}::[{code}] {message}")
                else:
                    # Simple string error
                    print(f"::error file={filename}::{error}")

def check_error_codes(validation_result: Dict[str, Any]) -> Dict[str, int]:
    """Count errors by error code for reporting."""
    
    error_counts = {}
    
    for file_result in validation_result.get("results", []):
        for error in file_result.get("errors", []):
            if isinstance(error, dict) and "code_name" in error:
                code = error["code_name"]
                error_counts[code] = error_counts.get(code, 0) + 1
    
    return error_counts

def main():
    """Main CI validation script."""
    
    # Find all YAML files to validate
    config_dir = Path("test")
    config_files = list(config_dir.glob("**/*.yml"))
    
    if not config_files:
        print("::warning::No configuration files found to validate")
        sys.exit(0)
    
    print(f"Found {len(config_files)} configuration files to validate")
    
    # Run validation
    validation_result = run_validation([str(f) for f in config_files])
    
    # Check for command errors
    if validation_result.get("status") == "error":
        print(f"::error::Validation command failed: {validation_result.get('message')}")
        sys.exit(2)
    
    # Format GitHub annotations for errors
    format_github_annotations(validation_result)
    
    # Get summary statistics
    summary = validation_result.get("summary", {})
    total_files = summary.get("total_files", 0)
    valid_files = summary.get("valid_files", 0)
    total_errors = summary.get("total_errors", 0)
    
    # Count errors by type
    error_counts = check_error_codes(validation_result)
    
    # Print summary
    print(f"\n📊 Validation Summary:")
    print(f"   Total files: {total_files}")
    print(f"   Valid files: {valid_files}")
    print(f"   Invalid files: {total_files - valid_files}")
    print(f"   Total errors: {total_errors}")
    
    if error_counts:
        print(f"\n🔍 Errors by type:")
        for code, count in sorted(error_counts.items()):
            print(f"   {code}: {count}")
    
    # Set exit code based on validation status
    if validation_result.get("status") == "success":
        print("\n✅ All configurations are valid!")
        sys.exit(0)
    else:
        print(f"\n❌ Validation failed: {total_files - valid_files} files have errors")
        sys.exit(1)

if __name__ == "__main__":
    main()