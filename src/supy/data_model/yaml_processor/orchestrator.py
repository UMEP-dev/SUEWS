"""SUEWS YAML Configuration Processor

Three-phase validation workflow:
- Phase A: Up-to-date YAML check and parameter detection
- Phase B: Scientific validation and automatic adjustments
- Phase C: Conditional Pydantic validation based on physics options

Supports individual phases (A, B, C) or combined workflows (AB, AC, BC, ABC).
"""

import sys
import os
import argparse
import yaml
from typing import Tuple, Optional
import shutil
import io
from contextlib import redirect_stdout, redirect_stderr

# Import Phase A and B functions
try:
    from .phase_a_parameter_update import annotate_missing_parameters
    from .phase_b_science_check import run_science_check
except ImportError as e:
    print(f"Error importing required modules: {e}")
    print("Make sure phase modules are in the same directory")
    sys.exit(1)


def detect_pydantic_defaults(
    original_data: dict,
    processed_data: dict,
    path: str = "",
    standard_data: dict = None,
) -> tuple:
    """Detect where Pydantic applied defaults and separate critical nulls from normal defaults."""
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
        """Check if parameter exists in standard config at given path."""
        if not standard_data:
            return True

        # Navigate to the parameter using the path
        path_parts = param_path.split(".") if param_path else []
        current = standard_data

        for part in path_parts:
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

    if original_data is None:
        original_data = {}
    if processed_data is None:
        processed_data = {}

    if isinstance(processed_data, dict) and isinstance(original_data, dict):
        for key, processed_value in processed_data.items():
            current_path = f"{path}.{key}" if path else key
            original_value = original_data.get(key)

            if isinstance(processed_value, dict) and "value" in processed_value:
                original_ref_value = (
                    original_value.get("value")
                    if isinstance(original_value, dict)
                    else None
                )
                processed_ref_value = processed_value.get("value")

                if original_ref_value is None and processed_ref_value is not None:
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    was_missing = key not in original_data
                    status_text = "found missing" if was_missing else "found null"

                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": original_ref_value,
                        "default_value": processed_ref_value,
                        "field_name": key,
                        "status": status_text,
                    })
                elif (
                    original_ref_value is None
                    and processed_ref_value is None
                    and original_value is not None
                ):
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    if key in CRITICAL_PHYSICS_PARAMS:
                        critical_nulls.append({
                            "field_path": current_path,
                            "original_value": original_ref_value,
                            "field_name": key,
                            "parameter_type": "physics",
                        })
            elif (
                not isinstance(processed_value, (dict, list))
                and processed_value is not None
            ):
                if key in INTERNAL_UNUSED_PARAMS:
                    continue

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
                nested_critical, nested_defaults = detect_pydantic_defaults(
                    original_value, processed_value, current_path, standard_data
                )
                critical_nulls.extend(nested_critical)
                normal_defaults.extend(nested_defaults)
            elif isinstance(processed_value, list) and isinstance(original_value, list):
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
                        "default_value": str(processed_value),
                        "field_name": key,
                        "status": "found missing",
                    })

    return critical_nulls, normal_defaults


def validate_input_file(user_yaml_file: str) -> str:
    """Validate input YAML file exists and is readable."""
    if not os.path.exists(user_yaml_file):
        raise FileNotFoundError(f"Input file not found: {user_yaml_file}")

    if not user_yaml_file.lower().endswith((".yml", ".yaml")):
        raise ValueError(
            f"Input file must be a YAML file (.yml or .yaml): {user_yaml_file}"
        )

    try:
        with open(user_yaml_file, "r") as f:
            f.read(1)
    except PermissionError:
        raise PermissionError(f"Cannot read input file: {user_yaml_file}")

    return os.path.abspath(user_yaml_file)


def setup_output_paths(
    user_yaml_file: str, phase: str
) -> Tuple[str, str, str, str, str, str, str]:
    """Generate output file paths based on input file and phase."""
    basename = os.path.basename(user_yaml_file)
    dirname = os.path.dirname(user_yaml_file)
    name_without_ext = os.path.splitext(basename)[0]

    uptodate_file = None
    report_file = None
    science_yaml_file = None
    science_report_file = None
    pydantic_yaml_file = None
    pydantic_report_file = None

    if phase == "A":
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
    elif phase == "B":
        science_yaml_file = os.path.join(dirname, f"updatedB_{basename}")
        science_report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
    elif phase == "C":
        pydantic_yaml_file = os.path.join(dirname, f"updatedC_{basename}")
        pydantic_report_file = os.path.join(dirname, f"reportC_{name_without_ext}.txt")
    elif phase == "AB":
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
        science_yaml_file = os.path.join(dirname, f"updatedAB_{basename}")
        science_report_file = os.path.join(dirname, f"reportAB_{name_without_ext}.txt")
    elif phase == "AC":
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
        pydantic_yaml_file = os.path.join(dirname, f"updatedAC_{basename}")
        pydantic_report_file = os.path.join(dirname, f"reportAC_{name_without_ext}.txt")
    elif phase == "BC":
        science_yaml_file = os.path.join(dirname, f"updatedB_{basename}")
        science_report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
        pydantic_yaml_file = os.path.join(dirname, f"updatedBC_{basename}")
        pydantic_report_file = os.path.join(dirname, f"reportBC_{name_without_ext}.txt")
    elif phase == "ABC":
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
        science_yaml_file = os.path.join(dirname, f"updatedB_{basename}")
        science_report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
        pydantic_yaml_file = os.path.join(dirname, f"updatedABC_{basename}")
        pydantic_report_file = os.path.join(
            dirname, f"reportABC_{name_without_ext}.txt"
        )

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
    mode: str = "public",
    phase: str = "A",
    silent: bool = False,
) -> bool:
    """Execute Phase A: Parameter detection and YAML structure updates."""
    if not silent:
        print("Phase A: Up-to-date YAML check...")

    try:
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            annotate_missing_parameters(
                user_file=user_yaml_file,
                standard_file=standard_yaml_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
                mode=mode,
                phase=phase,
            )

        if not os.path.exists(uptodate_file):
            if not silent:
                print()
                print("✗ Phase A failed: No YAML file generated!")
            return False

        if not os.path.exists(report_file):
            if not silent:
                print()
                print("✗ Phase A failed: No report file generated!")
            return False

        with open(uptodate_file, "r") as f:
            content = f.read()
            if "Updated YAML" not in content:
                if not silent:
                    print()
                    print("✗ Phase A failed: Missing Phase A completion header!")
                return False

        with open(report_file, "r") as f:
            report_content = f.read()

        if "## ACTION NEEDED" in report_content:
            if not silent:
                print("✗ Phase A failed!")
                print(f"Report: {report_file}")
                print(f"Updated YAML: {uptodate_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run Phase A again."
                )
            return False

        if not silent:
            print("✓ Phase A completed")
        return True

    except Exception as e:
        if not silent:
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
    mode: str = "public",
    phase: str = "B",
    silent: bool = False,
) -> bool:
    """Execute Phase B: Scientific validation and automatic adjustments."""
    try:
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
                phase=phase,
            )

        # Check if Phase B produced output files
        if not os.path.exists(science_yaml_file):
            if not silent:
                print()
                print("✗ Phase B failed: No YAML file generated!")
            return False

        if not os.path.exists(science_report_file):
            if not silent:
                print()
                print("✗ Phase B failed: No report file generated!")
            return False

        # Check if Phase B report indicates critical issues
        with open(science_report_file, "r") as f:
            report_content = f.read()

        if "CRITICAL ISSUES DETECTED" in report_content or "URGENT" in report_content:
            if not silent:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                )
            return False

        if not silent:
            print("✓ Phase B completed")
        return True

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            if not silent:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                )
            return False
        else:
            if not silent:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                )
            return False
    except Exception as e:
        if not silent:
            print()
            print(f"✗ Phase B failed with unexpected error: {e}")
        return False


def run_phase_c(
    input_yaml_file: str,
    pydantic_yaml_file: str,
    pydantic_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
    silent: bool = False,
) -> bool:
    """Execute Phase C: Conditional Pydantic validation based on physics options."""
    try:
        current_dir = os.path.dirname(os.path.abspath(__file__))
        supy_root = os.path.abspath(os.path.join(current_dir, "../../"))
        if supy_root not in sys.path:
            sys.path.insert(0, supy_root)

        try:
            from supy.data_model import SUEWSConfig
            import logging

            supy_logger = logging.getLogger("SuPy")
            original_level = supy_logger.level
            supy_logger.setLevel(logging.CRITICAL)

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

                # Pydantic validation passed - create updatedC YAML with standardized header
                copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)

                # Build passed phases header based on which phases ran successfully
                if phases_run:
                    passed_phases = [f"PHASE {phase} - PASSED" for phase in phases_run]
                else:
                    passed_phases = ["PHASE C - PASSED"]  # Default for Phase C only

                phases_header = ", ".join(passed_phases)

                # Build consolidation info for previous phases
                phase_a_info = ""
                if phase_a_report_file and os.path.exists(phase_a_report_file):
                    try:
                        with open(phase_a_report_file, "r") as f:
                            phase_a_content = f.read()

                        # Add consolidation info
                        phase_a_info = "\n\n## PREVIOUS PHASES INFORMATION CONSOLIDATED\nSee below for prior phase results.\n"
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
                        action_needed += f"   Suggested fix: Set to appropriate non-null value - see documentation at https://suews.readthedocs.io/en/latest\n"

                    failure_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================
{action_needed}
# =================================================="""

                    with open(pydantic_report_file, "w") as f:
                        f.write(failure_report)

                    if not silent:
                        print("✗ Phase C failed!")
                        print(f"Report: {pydantic_report_file}")
                        print(f"Updated YAML: {pydantic_yaml_file}")
                        print(
                            f"Suggestion: Fix issues in updated YAML and consider to run either phase AB or complete processor ABC."
                        )
                    return False

                # Build NO ACTION NEEDED section if any defaults were detected
                no_action_info = ""
                if normal_defaults:
                    no_action_info = "\n\n## NO ACTION NEEDED\n"
                    for default_app in normal_defaults:
                        field_name = default_app.get("field_name", "unknown")
                        default_value = default_app.get("default_value", "unknown")
                        field_path = default_app.get("field_path", "unknown")
                        status = default_app.get(
                            "status", "found null"
                        )  # Default to old behaviour
                        no_action_info += f"- {field_name} {status} in user YAML at level {field_path}.\n  Pydantic will interpret that as default value: {default_value} - check doc for info on this parameter: https://suews.readthedocs.io/en/latest\n"

                # Generate phase-specific title for success report
                if phases_run:
                    phase_str = "".join(phases_run)
                else:
                    phase_str = "C"  # Default to Phase C only

                phase_titles = {
                    "A": "SUEWS - Phase A (Up-to-date YAML check) Report",
                    "B": "SUEWS - Phase B (Scientific Validation) Report",
                    "C": "SUEWS - Phase C (Pydantic Validation) Report",
                    "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
                    "AC": "SUEWS - Phase AC (Up-to-date YAML check and Pydantic Validation) Report",
                    "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
                    "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report",
                }

                title = phase_titles.get(
                    phase_str, "SUEWS Phase C (Pydantic Validation) Report"
                )

                # Extract NO ACTION NEEDED content from previous phases to consolidate properly
                consolidated_no_action = []

                # Add any default values detected
                if no_action_info:
                    # Remove the leading newlines and header, parse the content
                    no_action_content = no_action_info.strip()
                    if no_action_content.startswith("## NO ACTION NEEDED"):
                        no_action_content = no_action_content.replace(
                            "## NO ACTION NEEDED", "", 1
                        )

                    consolidated_no_action.extend([
                        line.strip()
                        for line in no_action_content.split("\n")
                        if line.strip()
                    ])

                # Extract content from previous phase reports without duplicating headers
                if phase_a_info:
                    # Extract only the content, not the headers
                    phase_content = phase_a_info.replace(
                        "## PREVIOUS PHASES INFORMATION CONSOLIDATED\nSee below for prior phase results.\n",
                        "",
                    )
                    # Remove redundant headers and extract only NO ACTION NEEDED content
                    lines = phase_content.split("\n")
                    in_no_action = False
                    for line in lines:
                        if line.strip().startswith("## NO ACTION NEEDED"):
                            in_no_action = True
                            continue
                        elif line.strip().startswith("##"):
                            in_no_action = False
                            continue
                        elif (
                            in_no_action
                            and line.strip()
                            and not line.strip().startswith("#")
                        ):
                            consolidated_no_action.append(line.strip())

                # Only add NO ACTION NEEDED section if there are items to show
                if consolidated_no_action:
                    success_report = f"""# {title}
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

## NO ACTION NEEDED
{chr(10).join(consolidated_no_action)}

# =================================================="""
                else:
                    success_report = f"""# {title}
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

Phase {phase_str} passed

# =================================================="""

                with open(pydantic_report_file, "w") as f:
                    f.write(success_report)

                # Restore logging level before return
                supy_logger.setLevel(original_level)
                if not silent:
                    print("✓ Phase C completed")
                return True

            except Exception as validation_error:
                # Restore logging level on validation error
                supy_logger.setLevel(original_level)
                # Pydantic validation failed - still create updatedC YAML with standardized header
                copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)

                # Generate structured ACTION NEEDED report
                try:
                    from .phase_c_pydantic_report import generate_phase_c_report

                    generate_phase_c_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                        phases_run,
                    )

                except Exception as report_error:
                    # Fallback to simple error report if structured report generation fails
                    from .phase_c_pydantic_report import generate_fallback_report

                    generate_fallback_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                        phases_run,
                    )

                if not silent:
                    print("✗ Phase C failed!")
                    print(f"Report: {pydantic_report_file}")
                    print(f"Updated YAML: {pydantic_yaml_file}")
                    print(
                        f"Suggestion: Fix issues in updated YAML and consider to run either phase AB or complete processor ABC."
                    )
                return False

        except ImportError as import_error:
            if not silent:
                print(f"✗ Phase C failed - Cannot import SUEWSConfig: {import_error}")

            # Import error report
            error_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
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
   python suews_yaml_processor.py user.yml --phase AB

## Error Details:
{str(import_error)}
"""

            with open(pydantic_report_file, "w") as f:
                f.write(error_report)

            print(f"  Report generated: {os.path.basename(pydantic_report_file)}")
            return False

    except Exception as e:
        if not silent:
            print(f"✗ Phase C failed: {e}")

        # General error report
        error_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

## PHASE C - FAILED

Error running Phase C validation: {e}

Input file: {input_yaml_file}

## Status:
Phase C validation could not be executed due to system errors.

## Recommended Actions:
1. Use Phase A + B validation instead:
   python suews_yaml_processor.py user.yml --phase AB

2. Check system configuration and try again

## Error Details:
{str(e)}
"""

        with open(pydantic_report_file, "w") as f:
            f.write(error_report)

        print(f"  Report generated: {os.path.basename(pydantic_report_file)}")
        return False


def copy_yaml_with_standard_header(source_file: str, dest_file: str) -> None:
    """Copy YAML file and add standardised header."""
    with open(source_file, "r") as f:
        original_content = f.read()

    lines = original_content.split("\n")
    content_start_idx = 0

    for i, line in enumerate(lines):
        if line.strip() == "" or line.strip().startswith("#"):
            continue
        else:
            content_start_idx = i
            break

    clean_content = "\n".join(lines[content_start_idx:])

    standard_header = """# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

"""

    with open(dest_file, "w") as f:
        f.write(standard_header + clean_content)


def main():
    """Main entry point for Phase A-B-C workflow."""
    parser = argparse.ArgumentParser(
        description="SUEWS YAML Configuration Processor - Phase A, B, and/or C workflow",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python suews_yaml_processor.py user.yml                        # Run complete A→B→C workflow (default, public mode)
  python suews_yaml_processor.py user.yml --phase A              # Run Phase A only
  python suews_yaml_processor.py user.yml --phase AB             # Run A→B workflow
  python suews_yaml_processor.py user.yml --phase C              # Run Phase C only (Pydantic validation)
  python suews_yaml_processor.py user.yml --phase BC             # Run complete B→C workflow
  python suews_yaml_processor.py user.yml --mode dev             # Run ABC workflow in dev mode (available)
  python suews_yaml_processor.py user.yml --phase A --mode public  # Run Phase A in public mode (explicit)

Phases:
  Phase A: Up-to-date YAML check and structure updates
  Phase B: Scientific validation and automatic adjustments  
  Phase C: Conditional Pydantic validation based on model physics options

Modes:
  public: Standard validation mode with user-friendly messaging (default)
  dev:    Developer mode with extended options (available)
        """,
    )

    parser.add_argument("yaml_file", help="Input YAML configuration file")

    parser.add_argument(
        "--phase",
        "-p",
        choices=["A", "B", "C", "AB", "AC", "BC", "ABC"],
        default="ABC",
        help="Phase to run: A (Up-to-date YAML check), B (scientific validation), C (Pydantic validation), AB (A→B workflow), AC (A→C), BC (B→C), or ABC (complete workflow, default)",
    )

    parser.add_argument(
        "--mode",
        "-m",
        choices=["public", "dev"],
        default="public",
        help="Processing mode: public (standard validation mode, default) or dev (developer mode with extended options - available)",
    )

    args = parser.parse_args()
    user_yaml_file = args.yaml_file
    phase = args.phase
    mode = args.mode

    # Use mode directly - no mapping needed
    internal_mode = mode

    # Dev mode is now available

    try:
        # Step 1: Validate input file
        user_yaml_file = validate_input_file(user_yaml_file)

        # Step 2: Setup paths
        standard_yaml_file = "src/supy/sample_data/sample_config.yml"

        # Print workflow header (after variables are defined)
        phase_desc = {
            "A": "Phase A",
            "B": "Phase B",
            "C": "Phase C",
            "AB": "Phase AB",
            "AC": "Phase AC",
            "BC": "Phase BC",
            "ABC": "Phase ABC",
        }
        print(f"==================================")
        print(f"SUEWS YAML Configuration Processor")
        print(f"==================================")
        print(f"YAML user file: {user_yaml_file}")
        print(f"Standard file: {standard_yaml_file}")
        print(f"Processor Selected Mode: {phase_desc[phase]}")
        print(
            f"User Mode: {'Developer' if internal_mode.lower() == 'dev' else 'Public'}"
        )
        print(f"==================================")
        print()

        # Step 3: Pre-validation check for public mode restrictions
        if internal_mode.lower() == "public":
            try:
                import yaml

                with open(user_yaml_file, "r") as f:
                    user_yaml_data = yaml.safe_load(f)

                # Check public mode restrictions
                restrictions_violated = []

                # Check STEBBS method restriction
                stebbs_method = None
                if (
                    user_yaml_data
                    and isinstance(user_yaml_data, dict)
                    and "model" in user_yaml_data
                    and isinstance(user_yaml_data["model"], dict)
                    and "physics" in user_yaml_data["model"]
                    and isinstance(user_yaml_data["model"]["physics"], dict)
                    and "stebbsmethod" in user_yaml_data["model"]["physics"]
                ):
                    stebbs_entry = user_yaml_data["model"]["physics"]["stebbsmethod"]
                    # Handle both direct values and RefValue format
                    if isinstance(stebbs_entry, dict) and "value" in stebbs_entry:
                        stebbs_method = stebbs_entry["value"]
                    else:
                        stebbs_method = stebbs_entry

                if stebbs_method is not None and stebbs_method != 0:
                    restrictions_violated.append(
                        "STEBBS method is enabled (stebbsmethod != 0)"
                    )

                # Add more restriction checks here as needed
                # if other_dev_feature_enabled:
                #     restrictions_violated.append("Other developer feature is enabled")

                # If any restrictions are violated, halt execution
                if restrictions_violated:
                    print(
                        "Warning: You selected mode 'public' but your configuration contains developer-only features:"
                    )
                    for restriction in restrictions_violated:
                        print(f"  - {restriction}")
                    print()
                    print("Options to resolve:")
                    print("  1. Switch to dev mode: --mode dev (when available)")
                    print(
                        "  2. Disable stebbsmethod in your YAML file (stebbsmethod = 0) and rerun processor"
                    )
                    print()
                    print("Processor halted due to mode restrictions")
                    return 1

            except Exception as e:
                # If we can't read the YAML, let the normal phases handle the error
                print(
                    f"Warning: Could not pre-validate YAML file for mode restrictions: {e}"
                )
                print("Continuing with normal validation...")
                print()

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
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="A",
            )
            if phase_a_success:
                print("Report:", report_file)
                print("Updated YAML:", uptodate_file)
            return 0 if phase_a_success else 1

        elif phase == "B":
            # Phase B only - always run on original user YAML for pure Phase B validation
            input_yaml_file = user_yaml_file
            phase_a_report = None

            print("Phase B: Scientific validation check...")
            phase_b_success = run_phase_b(
                user_yaml_file,
                input_yaml_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                phase_a_report,
                phase_a_performed=False,  # Phase B only mode
                mode=internal_mode,
                phase="B",
                silent=True,  # Suppress phase function output, main function handles terminal output
            )
            if phase_b_success:
                print("✓ Phase B completed")
                print("Report:", science_report_file)
                print("Updated YAML:", science_yaml_file)
            else:
                # Phase B failed - remove YAML file if it was created during failure (COMMIT 3 requirement)
                try:
                    if os.path.exists(science_yaml_file):
                        os.remove(science_yaml_file)
                except Exception:
                    pass  # Don't fail if removal doesn't work
                # Phase B standalone failure: only show Report and Suggestion (no Updated YAML)
                print("✗ Phase B failed!")
                print("Report:", science_report_file)
                print(
                    "Suggestion: Fix issues in report and consider to run phase B again."
                )
            return 0 if phase_b_success else 1

        elif phase == "C":
            # Phase C only - run Pydantic validation on original user YAML
            print("Phase C: Pydantic validation check...")

            phase_c_success = run_phase_c(
                user_yaml_file,
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phases_run=["C"],
                silent=True,  # Suppress phase function output, main function handles terminal output
            )
            if phase_c_success:
                print("✓ Phase C completed")
                print("Report:", pydantic_report_file)
                print("Updated YAML:", pydantic_yaml_file)
            else:
                # Phase C failed - remove YAML file if it was created during failure (COMMIT 3 requirement)
                try:
                    if os.path.exists(pydantic_yaml_file):
                        os.remove(pydantic_yaml_file)
                except Exception:
                    pass  # Don't fail if removal doesn't work
                # Phase C standalone failure: only show Report and Suggestion (no Updated YAML)
                print("✗ Phase C failed!")
                print("Report:", pydantic_report_file)
                print(
                    "Suggestion: Fix issues in report and consider to run phase C again."
                )
            return 0 if phase_c_success else 1

        elif phase == "AB":
            # Complete A→B workflow
            print("Phase A: Up-to-date YAML check...")
            phase_a_success = run_phase_a(
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="AB",
                silent=True,
            )

            if not phase_a_success:
                # Phase A failed in AB workflow - preserve Phase A outputs as AB outputs (when Phase A passes, we have updated from A)
                try:
                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, science_report_file
                        )  # reportA → reportAB
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, science_yaml_file
                        )  # updatedA → updatedAB (preserve Phase A updated YAML)
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print("✗ Phase A failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun AB."
                )
                return 1

            print("✓ Phase A completed")
            print("Phase B: Scientific validation check...")
            phase_b_success = run_phase_b(
                user_yaml_file,
                uptodate_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                report_file,
                phase_a_performed=True,  # A→B workflow mode
                mode=internal_mode,
                phase="AB",
                silent=True,
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

                print("✓ Phase B completed")
                print("Report:", science_report_file)
                print("Updated YAML:", science_yaml_file)
                return 0
            else:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun AB."
                )
                return 1

        elif phase == "AC":
            # Complete A→C workflow
            print("Phase A: Up-to-date YAML check...")
            phase_a_success = run_phase_a(
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="AC",
                silent=True,
            )

            if not phase_a_success:
                # Phase A failed in AC workflow - preserve Phase A outputs as AC outputs (when Phase A passes, we have updated from A)
                try:
                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, pydantic_report_file
                        )  # reportA → reportAC
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updatedAC (preserve Phase A updated YAML)
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print("✗ Phase A failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun AC or complete processor ABC."
                )
                return 1

            print("✓ Phase A completed")
            print("Phase C: Pydantic validation check...")
            phase_c_success = run_phase_c(
                uptodate_file,  # Use Phase A output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phase_a_report_file=report_file,  # Pass Phase A report for consolidation
                phases_run=["A", "C"],
                silent=True,
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

                print("✓ Phase C completed")
                print("Report:", pydantic_report_file)
                print("Updated YAML:", pydantic_yaml_file)
                return 0
            else:
                print("✗ Phase C failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun AC or complete processor ABC."
                )
                return 1

        elif phase == "BC":
            # Complete B→C workflow
            print("Phase B: Scientific validation check...")
            phase_b_success = run_phase_b(
                user_yaml_file,
                user_yaml_file,  # Phase B runs directly on user YAML
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                None,  # No Phase A report available
                phase_a_performed=False,  # B→C workflow mode
                mode=internal_mode,
                phase="BC",
                silent=True,
            )

            if not phase_b_success:
                # Phase B failed in BC workflow - preserve Phase B outputs as BC outputs (when Phase B passes, we have updated from B)
                try:
                    if os.path.exists(science_report_file):
                        shutil.move(
                            science_report_file, pydantic_report_file
                        )  # reportB → reportBC
                    if os.path.exists(science_yaml_file):
                        shutil.move(
                            science_yaml_file, pydantic_yaml_file
                        )  # updatedB → updatedBC (preserve Phase B updated YAML)
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print("✗ Phase B failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun BC."
                )
                return 1

            print("✓ Phase B completed")
            print("Phase C: Pydantic validation check...")
            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phase_a_report_file=science_report_file,  # Pass Phase B report for consolidation
                phases_run=["B", "C"],
                silent=True,
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

                print("✓ Phase C completed")
                print("Report:", pydantic_report_file)
                print("Updated YAML:", pydantic_yaml_file)
                return 0
            else:
                print("✗ Phase C failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun BC."
                )
                return 1

        elif phase == "ABC":
            # Complete A→B→C workflow with proper halt logic
            # Step 1: Run Phase A
            print("Phase A: Up-to-date YAML check...")
            phase_a_success = run_phase_a(
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="ABC",
                silent=True,
            )

            if not phase_a_success:
                # Phase A failed in ABC workflow - preserve Phase A outputs as ABC outputs (when Phase A passes, we have updated from A)
                try:
                    if os.path.exists(report_file):
                        shutil.move(
                            report_file, pydantic_report_file
                        )  # reportA → reportABC
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updatedABC (preserve Phase A updated YAML)
                except Exception:
                    pass  # Don't fail if rename doesn't work

                print("✗ Phase A failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun complete processor ABC."
                )
                return 1

            print("✓ Phase A completed")

            # Step 2: Run Phase B (A passed, try B)
            print("Phase B: Scientific validation check...")
            phase_b_success = run_phase_b(
                user_yaml_file,
                uptodate_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                report_file,
                phase_a_performed=True,  # A→B→C workflow mode
                mode=internal_mode,
                phase="ABC",
                silent=True,
            )

            if not phase_b_success:
                # Phase B failed in ABC workflow - preserve Phase AB outputs as ABC outputs (if ABC halts at B, we have updated from A)
                try:
                    # Rename B outputs to ABC (to match selected phase)
                    if os.path.exists(science_report_file):
                        shutil.move(
                            science_report_file, pydantic_report_file
                        )  # reportB → reportABC

                    # CRITICAL: Preserve Phase A updated YAML since that's what we have when AB halts at B
                    # Phase A passed and created uptodate_file, Phase B failed, so we preserve Phase A result
                    if os.path.exists(uptodate_file):
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updatedABC (preserve Phase A updated YAML)

                    # Remove failed Phase B YAML if it exists (failed phase output not needed)
                    if os.path.exists(science_yaml_file):
                        os.remove(science_yaml_file)  # Remove failed Phase B YAML

                    # Clean up intermediate Phase A report (consolidated into final report)
                    if os.path.exists(report_file):
                        os.remove(report_file)  # Remove Phase A report
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print("✗ Phase B failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun complete processor ABC."
                )
                return 1

            print("✓ Phase B completed")

            # Step 3: Run Phase C (both A and B passed)
            print("Phase C: Pydantic validation check...")
            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phase_a_report_file=science_report_file,  # Pass Phase B report for consolidation
                phases_run=["A", "B", "C"],
                silent=True,
            )

            if not phase_c_success:
                # Phase C failed in ABC workflow - preserve Phase A+B outputs as ABC outputs
                try:
                    # Preserve the A+B combined YAML (science_yaml_file contains both A and B updates)
                    if os.path.exists(science_yaml_file):
                        shutil.move(
                            science_yaml_file, pydantic_yaml_file
                        )  # updatedAB → updatedABC

                    # Clean up intermediate files (A and B reports/YAML already consolidated into C outputs)
                    if os.path.exists(report_file):
                        os.remove(report_file)  # Remove Phase A report
                    if os.path.exists(uptodate_file):
                        os.remove(uptodate_file)  # Remove Phase A YAML
                    if os.path.exists(science_report_file):
                        os.remove(science_report_file)  # Remove Phase B report
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print("✗ Phase C failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to rerun complete processor ABC."
                )
                return 1

            # Phase C succeeded - clean up intermediate files
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

            print("✓ Phase C completed")
            print("Report:", pydantic_report_file)
            print("Updated YAML:", pydantic_yaml_file)
            return 0

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
