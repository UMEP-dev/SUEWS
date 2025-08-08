"""
SUEWS Master Phase A-B-C Runner

This script provides a complete workflow for SUEWS YAML configuration processing:
- Phase A: Parameter detection and YAML structure updates (uptodate_yaml.py)
- Phase B: Scientific validation and automatic adjustments (science_check.py)
- Phase C: Conditional Pydantic validation based on model physics options (core.py)

Usage:
    python master_ABC_run.py <user_yaml_file> [--phase PHASE]

Examples:
    python master_ABC_run.py my_config.yml                    # A→B workflow (default)
    python master_ABC_run.py my_config.yml --phase C          # Phase C only
    python master_ABC_run.py my_config.yml --phase BC         # B→C workflow
    python master_ABC_run.py my_config.yml --phase ABC        # Complete A→B→C workflow

The script supports individual phases (A, B, C) or combined workflows (AB, AC, BC, ABC):
1. Phase A: Detects missing parameters and updates YAML structure
2. Phase B: Performs scientific validation and automatic adjustments
3. Phase C: Runs conditional Pydantic validation for model-specific requirements

Input: Original user YAML configuration file
Output: Validated YAML configuration + comprehensive validation reports
"""

import sys
import os
import subprocess
import argparse
import yaml
from pathlib import Path
from typing import Tuple, Optional
import tempfile
import shutil
import io
from contextlib import redirect_stdout, redirect_stderr

# Import Phase A and B functions
try:
    from uptodate_yaml import annotate_missing_parameters
    from science_check import run_science_check
except ImportError as e:
    print(f"Error importing required modules: {e}")
    print("Make sure uptodate_yaml.py and science_check.py are in the same directory")
    sys.exit(1)


def detect_pydantic_defaults(
    original_data: dict,
    processed_data: dict,
    path: str = "",
    standard_data: dict = None,
) -> tuple:
    """
    Detect where Pydantic applied default values by comparing original vs processed data.
    Only reports parameters as "missing" if they exist in the standard sample_config.yml.
    Separates critical physics parameters that would crash df_state from normal defaults.

    Args:
        original_data: The original YAML data as parsed
        processed_data: The Pydantic-processed data with defaults applied
        path: Current path in the data structure (for recursion)
        standard_data: The standard sample_config.yml data for comparison

    Returns:
        Tuple of (critical_nulls, normal_defaults) where:
        - critical_nulls: Physics parameters that are null and would crash df_state
        - normal_defaults: Normal Pydantic default applications for parameters in standard config
    """
    # Critical physics parameters that get converted to int() in df_state
    CRITICAL_PHYSICS_PARAMS = [
        "netradiationmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "roughlenmommethod",
        "roughlenheatmethod",
        "stabilitymethod",
        "smdmethod",
        "waterusemethod",
        "rslmethod",
        "faimethod",
        "rsllevel",
        "gsmodel",
        "snowuse",
        "stebbsmethod",
    ]

    # Internal parameters that are not used by SUEWS and should not be reported to users
    # These are typically legacy parameter names or unused model components
    INTERNAL_UNUSED_PARAMS = [
        "ch_anohm",
        "rho_cp_anohm",
        "k_anohm",  # Unused ANOHM (Analytical OHM) parameters
        "_yaml_path",
        "_auto_generate_annotated",  # Internal Pydantic metadata fields
    ]

    def parameter_exists_in_standard(param_path: str, standard_data: dict) -> bool:
        """Check if a parameter exists in the standard config at the given path"""
        if not standard_data:
            return True  # If no standard data, assume all params are valid (fallback behavior)

        # Navigate to the parameter using the path
        path_parts = param_path.split(".") if param_path else []
        current = standard_data

        for part in path_parts:
            # Handle array indices like sites[0]
            if "[" in part and "]" in part:
                array_name = part.split("[")[0]
                if array_name not in current or not isinstance(
                    current[array_name], list
                ):
                    return False
                current = current[array_name][0] if current[array_name] else {}
            else:
                if not isinstance(current, dict) or part not in current:
                    return False
                current = current[part]

        return True

    critical_nulls = []
    normal_defaults = []

    # Handle None/missing values in original data
    if original_data is None:
        original_data = {}
    if processed_data is None:
        processed_data = {}

    # Compare dictionaries recursively
    if isinstance(processed_data, dict) and isinstance(original_data, dict):
        # Check all fields in processed data
        for key, processed_value in processed_data.items():
            current_path = f"{path}.{key}" if path else key
            original_value = original_data.get(key)

            # Check for RefValue wrapper pattern (SUEWS specific) or plain values from model_dump()
            if isinstance(processed_value, dict) and "value" in processed_value:
                # This is a RefValue field (from config object directly)
                original_ref_value = (
                    original_value.get("value")
                    if isinstance(original_value, dict)
                    else None
                )
                processed_ref_value = processed_value.get("value")

                # Check if original was None/null but processed has a value (default applied)
                if original_ref_value is None and processed_ref_value is not None:
                    # Skip internal unused parameters that shouldn't be reported to users
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    # Determine if parameter was missing or explicitly set to null
                    was_missing = key not in original_data
                    status_text = "found missing" if was_missing else "found null"

                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": original_ref_value,
                        "default_value": processed_ref_value,
                        "field_name": key,
                        "status": status_text,
                    })
                # Check for critical null physics parameters that would crash df_state
                elif (
                    original_ref_value is None
                    and processed_ref_value is None
                    and original_value is not None
                ):
                    # Skip internal unused parameters that shouldn't be reported to users
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    if key in CRITICAL_PHYSICS_PARAMS:
                        critical_nulls.append({
                            "field_path": current_path,
                            "original_value": original_ref_value,
                            "field_name": key,
                            "parameter_type": "physics",
                        })
                    else:
                        # Only report null values if the parameter exists in standard config
                        if parameter_exists_in_standard(current_path, standard_data):
                            normal_defaults.append({
                                "field_path": current_path,
                                "original_value": original_ref_value,
                                "default_value": "null (accepted by Pydantic)",
                                "field_name": key,
                                "status": "found null",  # This was explicitly set to null
                            })
            # Handle plain values (from model_dump() which extracts values from RefValue wrappers)
            elif (
                not isinstance(processed_value, (dict, list))
                and processed_value is not None
            ):
                # Skip internal unused parameters that shouldn't be reported to users
                if key in INTERNAL_UNUSED_PARAMS:
                    continue

                # Check if this is a default value (parameter missing in original)
                # Only report if parameter exists in standard config
                if key not in original_data and parameter_exists_in_standard(
                    current_path, standard_data
                ):
                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": None,
                        "default_value": processed_value,
                        "field_name": key,
                        "status": "found missing",
                    })
            elif isinstance(processed_value, dict):
                # Recurse into nested dictionaries
                nested_critical, nested_defaults = detect_pydantic_defaults(
                    original_value, processed_value, current_path, standard_data
                )
                critical_nulls.extend(nested_critical)
                normal_defaults.extend(nested_defaults)
            elif isinstance(processed_value, list) and isinstance(original_value, list):
                # Handle lists (like sites)
                for i, (orig_item, proc_item) in enumerate(
                    zip(original_value, processed_value)
                ):
                    list_path = f"{current_path}[{i}]"
                    nested_critical, nested_defaults = detect_pydantic_defaults(
                        orig_item, proc_item, list_path, standard_data
                    )
                    critical_nulls.extend(nested_critical)
                    normal_defaults.extend(nested_defaults)
            else:
                # Check for direct value defaults (non-RefValue fields)
                # If the field wasn't in original data but appears in processed data, it's a default
                # Skip internal Pydantic fields and unused parameters that users don't need to know about
                if (
                    key not in original_data
                    and processed_value is not None
                    and not key.startswith("_")
                    and key not in INTERNAL_UNUSED_PARAMS
                    and parameter_exists_in_standard(current_path, standard_data)
                ):
                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": None,
                        "default_value": str(
                            processed_value
                        ),  # Convert to string for display
                        "field_name": key,
                        "status": "found missing",  # Direct fields are always missing when not in original
                    })

    return critical_nulls, normal_defaults


def validate_input_file(user_yaml_file: str) -> str:
    """
    Validate that the input YAML file exists and is readable.

    Args:
        user_yaml_file: Path to user YAML file

    Returns:
        Absolute path to the validated file

    Raises:
        FileNotFoundError: If file doesn't exist
        ValueError: If file is not a YAML file
    """
    if not os.path.exists(user_yaml_file):
        raise FileNotFoundError(f"Input file not found: {user_yaml_file}")

    # Check file extension
    if not user_yaml_file.lower().endswith((".yml", ".yaml")):
        raise ValueError(
            f"Input file must be a YAML file (.yml or .yaml): {user_yaml_file}"
        )

    # Check if file is readable
    try:
        with open(user_yaml_file, "r") as f:
            f.read(1)  # Try to read first character
    except PermissionError:
        raise PermissionError(f"Cannot read input file: {user_yaml_file}")

    return os.path.abspath(user_yaml_file)


def setup_output_paths(
    user_yaml_file: str, phase: str
) -> Tuple[str, str, str, str, str, str, str]:
    """
    Generate all output file paths based on input file and phase.

    Args:
        user_yaml_file: Path to input user YAML file
        phase: Phase mode ('A', 'B', 'C', 'AB', 'AC', 'BC', or 'ABC')

    Returns:
        Tuple of (uptodate_file, report_file, science_yaml_file, science_report_file, pydantic_yaml_file, pydantic_report_file, dirname)
    """
    basename = os.path.basename(user_yaml_file)
    dirname = os.path.dirname(user_yaml_file)
    name_without_ext = os.path.splitext(basename)[0]

    # Initialize all output files
    uptodate_file = None
    report_file = None
    science_yaml_file = None
    science_report_file = None
    pydantic_yaml_file = None
    pydantic_report_file = None

    if phase == "A":
        # Phase A only
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
    elif phase == "B":
        # Phase B only
        science_yaml_file = os.path.join(dirname, f"updatedB_{basename}")
        science_report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
    elif phase == "C":
        # Phase C only
        pydantic_yaml_file = os.path.join(dirname, f"updatedC_{basename}")
        pydantic_report_file = os.path.join(dirname, f"reportC_{name_without_ext}.txt")
    elif phase == "AB":
        # Complete A→B workflow - use AB naming
        uptodate_file = os.path.join(
            dirname, f"updatedA_{basename}"
        )  # Intermediate A file
        report_file = os.path.join(
            dirname, f"reportA_{name_without_ext}.txt"
        )  # Intermediate A report
        science_yaml_file = os.path.join(
            dirname, f"updatedAB_{basename}"
        )  # Final AB file
        science_report_file = os.path.join(
            dirname, f"reportAB_{name_without_ext}.txt"
        )  # Final AB report
    elif phase == "AC":
        # A→C workflow
        uptodate_file = os.path.join(
            dirname, f"updatedA_{basename}"
        )  # Intermediate A file
        report_file = os.path.join(
            dirname, f"reportA_{name_without_ext}.txt"
        )  # Intermediate A report
        pydantic_yaml_file = os.path.join(
            dirname, f"updatedAC_{basename}"
        )  # Final AC file
        pydantic_report_file = os.path.join(
            dirname, f"reportAC_{name_without_ext}.txt"
        )  # Final AC report
    elif phase == "BC":
        # B→C workflow
        science_yaml_file = os.path.join(
            dirname, f"updatedB_{basename}"
        )  # Intermediate B file
        science_report_file = os.path.join(
            dirname, f"reportB_{name_without_ext}.txt"
        )  # Intermediate B report
        pydantic_yaml_file = os.path.join(
            dirname, f"updatedBC_{basename}"
        )  # Final BC file
        pydantic_report_file = os.path.join(
            dirname, f"reportBC_{name_without_ext}.txt"
        )  # Final BC report
    elif phase == "ABC":
        # Complete A→B→C workflow
        uptodate_file = os.path.join(
            dirname, f"updatedA_{basename}"
        )  # Intermediate A file
        report_file = os.path.join(
            dirname, f"reportA_{name_without_ext}.txt"
        )  # Intermediate A report
        science_yaml_file = os.path.join(
            dirname, f"updatedB_{basename}"
        )  # Intermediate B file
        science_report_file = os.path.join(
            dirname, f"reportB_{name_without_ext}.txt"
        )  # Intermediate B report
        pydantic_yaml_file = os.path.join(
            dirname, f"updatedABC_{basename}"
        )  # Final ABC file
        pydantic_report_file = os.path.join(
            dirname, f"reportABC_{name_without_ext}.txt"
        )  # Final ABC report

    return (
        uptodate_file,
        report_file,
        science_yaml_file,
        science_report_file,
        pydantic_yaml_file,
        pydantic_report_file,
        dirname,
    )


def run_phase_a(
    user_yaml_file: str,
    standard_yaml_file: str,
    uptodate_file: str,
    report_file: str,
    mode: str = "user",
) -> bool:
    """
    Execute Phase A: Parameter detection and YAML structure updates.

    Args:
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        uptodate_file: Path for Phase A output YAML
        report_file: Path for Phase A report
        mode: Processing mode ('user' or 'dev')

    Returns:
        True if Phase A completed successfully, False otherwise
    """
    print("Phase A: Parameter detection...")

    try:
        # Run Phase A using the imported function (suppress verbose output)
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            annotate_missing_parameters(
                user_file=user_yaml_file,
                standard_file=standard_yaml_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
                mode=mode,
            )

        # Check if Phase A produced output files
        if not os.path.exists(uptodate_file):
            print()
            print("✗ Phase A failed: No uptodate YAML file generated")
            return False

        if not os.path.exists(report_file):
            print()
            print("✗ Phase A failed: No report file generated")
            return False

        # Check if uptodate file has Phase A header
        with open(uptodate_file, "r") as f:
            content = f.read()
            if "UP TO DATE YAML" not in content:
                print()
                print("✗ Phase A failed: Missing Phase A completion header")
                return False

        # Check Phase A report for critical issues
        with open(report_file, "r") as f:
            report_content = f.read()

        # Phase A should halt workflow if there are any ACTION NEEDED items
        if "## ACTION NEEDED" in report_content:
            print()
            print("✗ Phase A halted: ACTION NEEDED items must be resolved first")
            print(f"  Fix issues in reportA file: {report_file}")
            print(f"  Then re-run with the updated YAML file")
            return False

        # If Phase A succeeds with no critical errors, we'll let Phase B create the consolidated report
        # Keep the Phase A report file for Phase B to read, but don't present it as final output
        print("✓ Phase A completed")
        return True

    except Exception as e:
        print()
        print(f"✗ Phase A failed with error: {e}")
        return False


def run_phase_b(
    user_yaml_file: str,
    uptodate_file: str,
    standard_yaml_file: str,
    science_yaml_file: str,
    science_report_file: str,
    phase_a_report_file: str,
    phase_a_performed: bool = True,
    mode: str = "user",
) -> bool:
    """
    Execute Phase B: Scientific validation and automatic adjustments.

    Args:
        user_yaml_file: Path to original user YAML
        uptodate_file: Path to Phase A output YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for Phase B output YAML
        science_report_file: Path for Phase B report
        phase_a_report_file: Path to Phase A report file (if available)
        phase_a_performed: Whether Phase A was performed before Phase B

    Returns:
        True if Phase B completed successfully, False otherwise
    """
    print("Phase B: Scientific validation...")

    try:
        # Run Phase B using the imported function (suppress verbose output)
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            science_checked_data = run_science_check(
                uptodate_yaml_file=uptodate_file,
                user_yaml_file=user_yaml_file,
                standard_yaml_file=standard_yaml_file,
                science_yaml_file=science_yaml_file,
                science_report_file=science_report_file,
                phase_a_report_file=phase_a_report_file,
                phase_a_performed=phase_a_performed,
                mode=mode,
            )

        # Check if Phase B produced output files
        if not os.path.exists(science_yaml_file):
            print()
            print("✗ Phase B failed: No science-checked YAML file generated")
            return False

        if not os.path.exists(science_report_file):
            print()
            print("✗ Phase B failed: No science report file generated")
            return False

        # Check if Phase B report indicates critical issues
        with open(science_report_file, "r") as f:
            report_content = f.read()

        if "CRITICAL ISSUES DETECTED" in report_content or "URGENT" in report_content:
            print()
            print("✗ Phase B halted: Critical scientific issues detected")
            print(f"  Review issues in reportB file: {science_report_file}")
            print(f"  Fix the issues and re-run the workflow")
            return False

        print("✓ Phase B completed")
        return True

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            print()
            print("✗ Phase B halted: Critical scientific errors detected")
            print(f"  Check reportB file for details: {science_report_file}")
            print(
                "  Suggestion: Fix the critical issues or run Phase A first if parameters are missing."
            )
            return False
        else:
            print()
            print(f"✗ Phase B failed: Validation error - {e}")
            print(f"  Check reportB file for details: {science_report_file}")
            return False
    except Exception as e:
        print()
        print(f"✗ Phase B failed with unexpected error: {e}")
        return False


def run_phase_c(
    input_yaml_file: str,
    pydantic_yaml_file: str,
    pydantic_report_file: str,
    mode: str = "user",
    phase_a_report_file: str = None,
) -> bool:
    """
    Execute Phase C: Conditional Pydantic validation based on model physics options.

    Args:
        input_yaml_file: Path to input YAML file (could be from Phase A, B, or original user file)
        pydantic_yaml_file: Path for Phase C output YAML (for future use)
        pydantic_report_file: Path for Phase C validation report

    Returns:
        True if Phase C completed successfully, False otherwise
    """
    print("Phase C: Pydantic validation...")

    try:
        # Add current directory to Python path for imports
        current_dir = os.path.dirname(os.path.abspath(__file__))
        # Use the working supy import pattern like SUEWSSimulation does
        # Navigate to supy root directory to ensure proper imports
        supy_root = os.path.abspath(os.path.join(current_dir, "../../"))
        if supy_root not in sys.path:
            sys.path.insert(0, supy_root)

        # Import SUEWSConfig using the established working pattern from tests
        try:
            from supy.data_model import SUEWSConfig

            # Load and validate the YAML using SUEWSConfig (like SUEWSSimulation does)
            try:
                # Load original YAML data for comparison
                import yaml

                with open(input_yaml_file, "r") as f:
                    original_data = yaml.safe_load(f)

                config = SUEWSConfig.from_yaml(input_yaml_file)

                # Get the Pydantic-processed data (with defaults applied)
                processed_data = config.model_dump()

                # Load standard config for comparison
                try:
                    with open("src/supy/sample_data/sample_config.yml", "r") as f:
                        standard_data = yaml.safe_load(f)
                except FileNotFoundError:
                    print(
                        "Warning: Standard config file not found, reporting all defaults"
                    )
                    standard_data = None

                # Detect critical null physics parameters and normal defaults
                critical_nulls, normal_defaults = detect_pydantic_defaults(
                    original_data, processed_data, "", standard_data
                )

                # Pydantic validation passed - create updatedC YAML (copy of original user file)
                import shutil

                shutil.copy2(input_yaml_file, pydantic_yaml_file)

                # Generate success report with Phase A consolidation if applicable
                phase_a_info = ""
                if phase_a_report_file and os.path.exists(phase_a_report_file):
                    phase_a_info = "\n\n## PHASE A INFORMATION CONSOLIDATED\nSee below for Phase A parameter detection results.\n"
                    try:
                        with open(phase_a_report_file, "r") as f:
                            phase_a_content = f.read()
                        # Extract sections from Phase A report for consolidation
                        phase_a_info += f"\n{phase_a_content}"
                    except Exception:
                        phase_a_info = ""

                # Check if we have critical nulls that need ACTION
                if critical_nulls:
                    # Generate failure report with ACTION NEEDED for critical nulls
                    action_needed = "\n## ACTION NEEDED\n"
                    action_needed += f"- Found ({len(critical_nulls)}) critical physics parameter(s) set to null that will crash df_state conversion:\n"

                    for critical in critical_nulls:
                        field_name = critical.get("field_name", "unknown")
                        field_path = critical.get("field_path", "unknown")
                        action_needed += f"-- {field_name} at level {field_path}: Physics parameter is null and will cause runtime crash\n"
                        action_needed += f"   Suggested fix: Set to appropriate non-null value - see documentation at https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteSelect/\n"

                    failure_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {mode.title()}
# ============================================
{action_needed}
## NO ACTION NEEDED

# =================================================="""

                    with open(pydantic_report_file, "w") as f:
                        f.write(failure_report)

                    print(
                        "✗ Phase C failed - Critical null physics parameters detected"
                    )
                    print(
                        f"  Report generated: {os.path.basename(pydantic_report_file)}"
                    )
                    print(f"  Check ACTION NEEDED section in report for required fixes")
                    return False

                # Build default applications section if any were detected
                default_info = ""
                if normal_defaults:
                    default_info = "\n\n## PYDANTIC DEFAULT VALUES APPLIED\n"
                    for default_app in normal_defaults:
                        field_name = default_app.get("field_name", "unknown")
                        default_value = default_app.get("default_value", "unknown")
                        field_path = default_app.get("field_path", "unknown")
                        status = default_app.get(
                            "status", "found null"
                        )  # Default to old behavior
                        default_info += f"- {field_name} {status} in user YAML at level {field_path}. Pydantic will interpret that as default value: {default_value}\n"

                success_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {mode.title()}
# ============================================

## PHASE C - PASSED

All conditional Pydantic validation checks completed successfully:
- Model physics method compatibility validated
- Conditional parameter requirements satisfied  
- RefValue wrapper validation passed
- Physical constraint validation passed

Input file: {input_yaml_file}
Output file: {pydantic_yaml_file}
Validation result: Pydantic validation completed successfully

No further action required.{default_info}{phase_a_info}"""

                with open(pydantic_report_file, "w") as f:
                    f.write(success_report)

                print("✓ Phase C completed - Pydantic validation passed")
                return True

            except Exception as validation_error:
                # Pydantic validation failed - still create updatedC YAML (copy of original for user to modify)
                import shutil

                shutil.copy2(input_yaml_file, pydantic_yaml_file)

                # Generate structured ACTION NEEDED report
                try:
                    from phase_c_reports import generate_phase_c_report

                    generate_phase_c_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                    )

                except Exception as report_error:
                    # Fallback to simple error report if structured report generation fails
                    from phase_c_reports import generate_fallback_report

                    generate_fallback_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                    )

                print("✗ Phase C failed - Pydantic validation errors detected")
                print(f"  Report generated: {os.path.basename(pydantic_report_file)}")
                print(f"  YAML file generated: {os.path.basename(pydantic_yaml_file)}")
                print(f"  Check ACTION NEEDED section in report for required fixes")
                return False

        except ImportError as import_error:
            print(f"✗ Phase C failed - Cannot import SUEWSConfig: {import_error}")

            # Import error report
            error_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {mode.title()}
# ============================================

## PHASE C - FAILED

Input file: {input_yaml_file}
Error: Cannot import SUEWSConfig

Import error details:
{str(import_error)}

## Status:
Phase C validation could not be executed due to import issues.

## Recommended Actions:
1. Check if supy package is properly installed
2. Ensure you're running from the correct directory
3. Use Phase A + B validation as alternative:
   python master_ABC_run.py user.yml --phase AB

## Error Details:
{str(import_error)}
"""

            with open(pydantic_report_file, "w") as f:
                f.write(error_report)

            print(f"  Report generated: {os.path.basename(pydantic_report_file)}")
            return False

    except Exception as e:
        print(f"✗ Phase C failed: {e}")

        # General error report
        error_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {mode.title()}
# ============================================

## PHASE C - FAILED

Error running Phase C validation: {e}

Input file: {input_yaml_file}

## Status:
Phase C validation could not be executed due to system errors.

## Recommended Actions:
1. Use Phase A + B validation instead:
   python master_ABC_run.py user.yml --phase AB

2. Check system configuration and try again

## Error Details:
{str(e)}
"""

        with open(pydantic_report_file, "w") as f:
            f.write(error_report)

        print(f"  Report generated: {os.path.basename(pydantic_report_file)}")
        return False


def main():
    """Main entry point for master Phase A-B-C workflow."""

    # Setup command line argument parsing
    parser = argparse.ArgumentParser(
        description="SUEWS YAML Configuration Processor - Phase A, B, and/or C workflow",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python master_ABC_run.py user.yml                        # Run complete A→B workflow (default, user mode)
  python master_ABC_run.py user.yml --phase A              # Run Phase A only
  python master_ABC_run.py user.yml --phase C              # Run Phase C only (Pydantic validation)
  python master_ABC_run.py user.yml --phase BC             # Run complete B→C workflow
  python master_ABC_run.py user.yml --phase ABC            # Run complete A→B→C workflow
  python master_ABC_run.py user.yml --mode dev             # Run A→B workflow in dev mode (coming soon)
  python master_ABC_run.py user.yml --phase A --mode user  # Run Phase A in user mode (explicit)

Phases:
  Phase A: Parameter detection and YAML structure updates
  Phase B: Scientific validation and automatic adjustments  
  Phase C: Conditional Pydantic validation based on model physics options

Modes:
  user: Standard validation mode with user-friendly messaging (default)
  dev:  Developer mode with extended options (coming soon)
        """,
    )

    parser.add_argument("yaml_file", help="Input YAML configuration file")

    parser.add_argument(
        "--phase",
        "-p",
        choices=["A", "B", "C", "AB", "AC", "BC", "ABC"],
        default="AB",
        help="Phase to run: A (parameter detection), B (scientific validation), C (Pydantic validation), AB (A→B workflow, default), AC (A→C), BC (B→C), or ABC (complete workflow)",
    )

    parser.add_argument(
        "--mode",
        "-m",
        choices=["user", "dev"],
        default="user",
        help="Processing mode: user (standard validation mode, default) or dev (developer mode with extended options - coming soon)",
    )

    args = parser.parse_args()
    user_yaml_file = args.yaml_file
    phase = args.phase
    mode = args.mode

    # Print workflow header
    phase_desc = {
        "A": "Phase A Only",
        "B": "Phase B Only",
        "C": "Phase C Only",
        "AB": "Complete A→B Workflow",
        "AC": "Complete A→C Workflow",
        "BC": "Complete B→C Workflow",
        "ABC": "Complete A→B→C Workflow",
    }
    print(f"==================================")
    print(f"SUEWS YAML Configuration Processor")
    print(f"==================================")
    print(f"YAML user file: {os.path.basename(user_yaml_file)}")
    print(f"Processor Selected Mode: {phase_desc[phase]}")
    print(f"Processing Mode: {mode.title()}")
    print(f"==================================")
    print()

    # Check if dev mode is requested (not yet implemented)
    if mode.lower() == "dev":
        print("✗ Developer mode is not yet implemented")
        print("  Available mode: user (default)")
        print("  Coming soon: dev mode with extended validation options")
        return 1

    try:
        # Step 1: Validate input file
        user_yaml_file = validate_input_file(user_yaml_file)

        # Step 2: Setup paths
        standard_yaml_file = "src/supy/sample_data/sample_config.yml"
        if not os.path.exists(standard_yaml_file):
            print()
            print(f"✗ Standard YAML file not found: {standard_yaml_file}")
            print("Make sure you're running from the SUEWS root directory")
            return 1

        (
            uptodate_file,
            report_file,
            science_yaml_file,
            science_report_file,
            pydantic_yaml_file,
            pydantic_report_file,
            dirname,
        ) = setup_output_paths(user_yaml_file, phase)

        # Phase-specific execution
        if phase == "A":
            # Phase A only
            phase_a_success = run_phase_a(
                user_yaml_file, standard_yaml_file, uptodate_file, report_file, mode
            )
            if phase_a_success:
                print()
                print(f" Phase A completed: {os.path.basename(uptodate_file)}")
                print(f" Report: {os.path.basename(report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_a_success else 1

        elif phase == "B":
            # Phase B only - always run on original user YAML for pure Phase B validation
            input_yaml_file = user_yaml_file
            phase_a_report = None

            print("Running Phase B directly on user YAML...")
            print("(Phase B only mode - ignoring any existing Phase A output files)")
            # Phase B will validate the original user YAML and detect missing parameters

            phase_b_success = run_phase_b(
                user_yaml_file,
                input_yaml_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                phase_a_report,
                phase_a_performed=False,  # Phase B only mode
                mode=mode,
            )
            if phase_b_success:
                print()
                print(f" Phase B completed: {os.path.basename(science_yaml_file)}")
                print(f" Report: {os.path.basename(science_report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_b_success else 1

        elif phase == "C":
            # Phase C only - run Pydantic validation on original user YAML
            print("Running Phase C directly on user YAML...")
            print("(Phase C only mode - Pydantic conditional validation)")

            phase_c_success = run_phase_c(
                user_yaml_file,
                pydantic_yaml_file,
                pydantic_report_file,
                mode,
            )
            if phase_c_success:
                print()
                print(f" Phase C completed: {os.path.basename(pydantic_yaml_file)}")
                print(f" Report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")
            else:
                print()
                print(f" Phase C failed: {os.path.basename(pydantic_yaml_file)}")
                print(f" Report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_c_success else 1

        elif phase == "AB":
            # Complete A→B workflow (existing logic)
            phase_a_success = run_phase_a(
                user_yaml_file, standard_yaml_file, uptodate_file, report_file, mode
            )

            if not phase_a_success:
                # Phase A failed in AB workflow - rename output files to match selected phase (AB)
                try:
                    import shutil

                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, science_report_file
                        )  # reportA → reportAB
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, science_yaml_file
                        )  # updatedA → updatedAB
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print()
                print(f" Phase A failed - AB workflow halted")
                print(f" AB report: {os.path.basename(science_report_file)}")
                print(f" AB YAML: {os.path.basename(science_yaml_file)}")
                print(f" File locations: {dirname}")
                return 1

            phase_b_success = run_phase_b(
                user_yaml_file,
                uptodate_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                report_file,
                phase_a_performed=True,  # A→B workflow mode
                mode=mode,
            )

            # Clean up intermediate files when complete workflow succeeds
            workflow_success = phase_a_success and phase_b_success
            if workflow_success:
                try:
                    if os.path.exists(report_file):
                        os.remove(report_file)  # Remove Phase A report
                    if os.path.exists(uptodate_file):
                        os.remove(uptodate_file)  # Remove Phase A YAML
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print()
                print(
                    f" Ready for SUEWS simulation: {os.path.basename(science_yaml_file)}"
                )
                print(f" Report: {os.path.basename(science_report_file)}")
                print(f" File locations: {dirname}")

            return 0 if workflow_success else 1

        elif phase == "AC":
            # Complete A→C workflow (similar to AB)
            phase_a_success = run_phase_a(
                user_yaml_file, standard_yaml_file, uptodate_file, report_file, mode
            )

            if not phase_a_success:
                # Phase A failed in AC workflow - rename output files to match selected phase (AC)
                try:
                    import shutil

                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, pydantic_report_file
                        )  # reportA → reportAC
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updatedAC
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print()
                print(f" Phase A failed - AC workflow halted")
                print(f" AC report: {os.path.basename(pydantic_report_file)}")
                print(f" AC YAML: {os.path.basename(pydantic_yaml_file)}")
                print(f" File locations: {dirname}")
                return 1

            phase_c_success = run_phase_c(
                uptodate_file,  # Use Phase A output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode,
                phase_a_report_file=report_file,  # Pass Phase A report for consolidation
            )

            # Clean up intermediate files when complete workflow succeeds
            workflow_success = phase_a_success and phase_c_success
            if workflow_success:
                try:
                    if os.path.exists(report_file):
                        os.remove(report_file)  # Remove Phase A report
                    if os.path.exists(uptodate_file):
                        os.remove(uptodate_file)  # Remove Phase A YAML
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print()
                print(
                    f" Ready for SUEWS simulation: {os.path.basename(pydantic_yaml_file)}"
                )
                print(f" Report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")

            return 0 if workflow_success else 1

        elif phase == "BC":
            # Complete B→C workflow (following AC pattern)
            phase_b_success = run_phase_b(
                user_yaml_file,
                user_yaml_file,  # Phase B runs directly on user YAML
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                None,  # No Phase A report available
                phase_a_performed=False,  # B→C workflow mode
                mode=mode,
            )

            if not phase_b_success:
                # Phase B failed in BC workflow - rename output files to match selected phase (BC)
                try:
                    import shutil

                    if os.path.exists(science_report_file):
                        shutil.move(
                            science_report_file, pydantic_report_file
                        )  # reportB → reportBC
                    if os.path.exists(science_yaml_file):
                        shutil.move(
                            science_yaml_file, pydantic_yaml_file
                        )  # updatedB → updatedBC
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print()
                print(f" Phase B failed - BC workflow halted")
                print(f" BC report: {os.path.basename(pydantic_report_file)}")
                print(f" BC YAML: {os.path.basename(pydantic_yaml_file)}")
                print(f" File locations: {dirname}")
                return 1

            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode,
                phase_a_report_file=science_report_file,  # Pass Phase B report for consolidation
            )

            # Clean up intermediate files when complete workflow succeeds
            workflow_success = phase_b_success and phase_c_success
            if workflow_success:
                try:
                    if os.path.exists(science_report_file):
                        os.remove(science_report_file)  # Remove Phase B report
                    if os.path.exists(science_yaml_file):
                        os.remove(science_yaml_file)  # Remove Phase B YAML
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print()
                print(
                    f" Ready for SUEWS simulation: {os.path.basename(pydantic_yaml_file)}"
                )
                print(f" Report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")

            return 0 if workflow_success else 1

        elif phase == "ABC":
            # Complete A→B→C workflow with proper halt logic
            # Step 1: Run Phase A
            phase_a_success = run_phase_a(
                user_yaml_file, standard_yaml_file, uptodate_file, report_file, mode
            )

            if not phase_a_success:
                # Phase A failed in ABC workflow - rename output files to match selected phase (ABC)
                try:
                    import shutil

                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, pydantic_report_file
                        )  # reportA → reportABC
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updatedABC
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print()
                print(f" Phase A failed - ABC workflow halted")
                print(f" ABC report: {os.path.basename(pydantic_report_file)}")
                print(f" ABC YAML: {os.path.basename(pydantic_yaml_file)}")
                print(f" File locations: {dirname}")
                return 1

            # Step 2: Run Phase B (A passed, try B)
            phase_b_success = run_phase_b(
                user_yaml_file,
                uptodate_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                report_file,
                phase_a_performed=True,  # A→B→C workflow mode
                mode=mode,
            )

            if not phase_b_success:
                # Phase B failed in ABC workflow - rename to ABC and clean up intermediate A files
                try:
                    import shutil

                    # Rename B outputs to ABC (to match selected phase)
                    if os.path.exists(science_report_file):
                        shutil.move(
                            science_report_file, pydantic_report_file
                        )  # reportB → reportABC
                    if os.path.exists(science_yaml_file):
                        shutil.move(
                            science_yaml_file, pydantic_yaml_file
                        )  # updatedB → updatedABC
                    # Clean up intermediate Phase A files
                    if os.path.exists(report_file):
                        os.remove(report_file)  # Remove Phase A report
                    if os.path.exists(uptodate_file):
                        os.remove(uptodate_file)  # Remove Phase A YAML
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print()
                print(f" Phase B failed - ABC workflow halted")
                print(f" ABC report: {os.path.basename(pydantic_report_file)}")
                print(f" ABC YAML: {os.path.basename(pydantic_yaml_file)}")
                print(f" File locations: {dirname}")
                return 1

            # Step 3: Run Phase C (both A and B passed)
            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode,
                phase_a_report_file=science_report_file,  # Pass Phase B report for consolidation
            )

            # Clean up intermediate files when complete ABC workflow completes
            # (regardless of Phase C success - we always provide final ABC outputs)
            try:
                if os.path.exists(report_file):
                    os.remove(report_file)  # Remove Phase A report
                if os.path.exists(uptodate_file):
                    os.remove(uptodate_file)  # Remove Phase A YAML
                if os.path.exists(science_report_file):
                    os.remove(science_report_file)  # Remove Phase B report
                if os.path.exists(science_yaml_file):
                    os.remove(science_yaml_file)  # Remove Phase B YAML
            except Exception:
                pass  # Don't fail if cleanup doesn't work

            # Always provide ABC outputs (success or failure)
            if phase_c_success:
                print()
                print(
                    f" Ready for SUEWS simulation: {os.path.basename(pydantic_yaml_file)}"
                )
                print(f" ABC report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")
            else:
                print()
                print(f" Phase C failed - ABC workflow completed with errors")
                print(f" ABC report: {os.path.basename(pydantic_report_file)}")
                print(f" ABC YAML: {os.path.basename(pydantic_yaml_file)}")
                print(f" File locations: {dirname}")

            # Return success if all phases passed
            workflow_success = phase_a_success and phase_b_success and phase_c_success
            return 0 if workflow_success else 1

        else:
            # Fallback for unknown phase combinations
            print(f"✗ Unknown phase combination '{phase}'")
            print("Available phases: A, B, C, AB, AC, BC, ABC")
            return 1

    except FileNotFoundError as e:
        print()
        print(f"✗ File error: {e}")
        return 1
    except ValueError as e:
        print()
        print(f"✗ Input error: {e}")
        return 1
    except Exception as e:
        print()
        print(f"✗ Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    exit(main())
