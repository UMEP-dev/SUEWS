"""SUEWS YAML Configuration Processor - Refactored with JSON reporting

Three-phase validation workflow with switch between old and new systems:
- Phase A: Up-to-date YAML check and parameter detection
- Phase B: Scientific validation and automatic adjustments
- Phase C: Conditional Pydantic validation based on physics options

Supports individual phases (A, B, C) or combined workflows (AB, AC, BC, ABC).
Can use either original or refactored (JSON-based) validation system.
"""

import sys
import os
import argparse
import yaml
from typing import Tuple, Optional
import shutil
import io
from contextlib import redirect_stdout, redirect_stderr
from pathlib import Path

# Import phase functions for original system
try:
    from .phase_a_parameter_update import annotate_missing_parameters
    from .phase_b_science_check import run_science_check
except ImportError as e:
    try:
        # Fallback to absolute imports for testing
        from phase_a_parameter_update import annotate_missing_parameters
        from phase_b_science_check import run_science_check
    except ImportError:
        print(f"Error importing original modules: {e}")
        # Don't exit, continue with refactored-only functionality

# Import refactored phase functions
try:
    from .phase_a_reporter import annotate_missing_parameters_refactored
    from .phase_b_reporter import run_science_check_refactored
    from .phase_c_reporter import (
        generate_phase_c_report_refactored,
        generate_fallback_report_refactored,
    )
    from .validation_reporter import ValidationReporter
except ImportError as e:
    try:
        # Fallback to absolute imports for testing
        from phase_a_reporter import annotate_missing_parameters_refactored
        from phase_b_reporter import run_science_check_refactored
        from phase_c_reporter import (
            generate_phase_c_report_refactored,
            generate_fallback_report_refactored,
        )
        from validation_reporter import ValidationReporter
    except ImportError:
        print(f"Error importing refactored modules: {e}")
        # Don't exit, continue with original-only functionality


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


def parse_phase(phase: str) -> list:
    """Parse phase string into list of individual phases."""
    phase = phase.upper()
    valid_phases = ["A", "B", "C"]

    # Handle combined phases like "ABC", "AB", etc.
    if len(phase) > 1:
        return [p for p in phase if p in valid_phases]
    elif phase in valid_phases:
        return [phase]
    else:
        raise ValueError(
            f"Invalid phase: {phase}. Must be one of A, B, C or combinations like AB, BC, ABC"
        )


def run_phase_a(
    user_yaml_file: str,
    standard_yaml_file: str,
    uptodate_file: str,
    report_file: str,
    mode: str = "public",
    phase: str = "A",
    silent: bool = False,
    use_refactored: bool = False,
) -> Tuple[bool, str]:
    """Execute Phase A: Parameter detection and YAML structure updates."""
    if not silent:
        print("Phase A: Up-to-date YAML check...")

    try:
        if use_refactored:
            # Use refactored version that returns ValidationReporter
            reporter = annotate_missing_parameters_refactored(
                user_file=user_yaml_file,
                standard_file=standard_yaml_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
                mode=mode,
                phase=phase,
            )
            # Check if Phase A passed
            json_data = reporter.get_json_report()
            success = not any(
                e.get("type") == "missing_required_parameter"
                for e in json_data.get("errors", [])
            )
            if not success:
                if not silent:
                    print("✗ Phase A failed!")
                    print(f"Report: {report_file}")
                    print(f"Updated YAML: {uptodate_file}")
                    print(
                        f"Suggestion: Fix issues in updated YAML and consider to run Phase A again."
                    )
                return False, report_file
        else:
            # Use original version with silent output capture
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
                return False, None

            if not os.path.exists(report_file):
                if not silent:
                    print()
                    print("✗ Phase A failed: No report file generated!")
                return False, None

            with open(uptodate_file, "r") as f:
                content = f.read()
                if "Updated YAML" not in content:
                    if not silent:
                        print()
                        print("✗ Phase A failed: Missing Phase A completion header!")
                    return False, report_file

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
                return False, report_file

        if not silent:
            print("✓ Phase A completed")
        return True, report_file

    except Exception as e:
        if not silent:
            print()
            print(f"✗ Phase A failed with error: {e}")
        return False, None


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
    use_refactored: bool = False,
) -> Tuple[bool, str, dict]:
    """Execute Phase B: Scientific validation and automatic adjustments."""
    try:
        if use_refactored:
            # Use refactored version
            science_data, reporter = run_science_check_refactored(
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
            # Check if Phase B passed
            json_data = reporter.get_json_report()
            success = json_data.get("summary", {}).get("validation_passed", False)
            if not success:
                if not silent:
                    print("✗ Phase B failed!")
                    print(f"Report: {science_report_file}")
                    print(f"Updated YAML: {science_yaml_file}")
                    print(
                        f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                    )
                return False, science_report_file, {}
        else:
            # Use original version with silent output capture
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
                return False, None, {}

            if not os.path.exists(science_report_file):
                if not silent:
                    print()
                    print("✗ Phase B failed: No report file generated!")
                return False, None, {}

            # Check if Phase B report indicates critical issues
            with open(science_report_file, "r") as f:
                report_content = f.read()

            if (
                "CRITICAL ISSUES DETECTED" in report_content
                or "URGENT" in report_content
            ):
                if not silent:
                    print("✗ Phase B failed!")
                    print(f"Report: {science_report_file}")
                    print(f"Updated YAML: {science_yaml_file}")
                    print(
                        f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                    )
                return False, science_report_file, {}

        if not silent:
            print("✓ Phase B completed")
        return (
            True,
            science_report_file,
            science_checked_data if "science_checked_data" in locals() else {},
        )

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            if not silent:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                )
            return False, science_report_file, {}
        else:
            if not silent:
                print("✗ Phase B failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
                print(
                    f"Suggestion: Fix issues in updated YAML and consider to run phase AB."
                )
            return False, science_report_file, {}
    except Exception as e:
        import traceback

        if not silent:
            print()
            print(f"✗ Phase B failed with unexpected error: {e}")
            traceback.print_exc()
        return False, None, {}


def run_phase_c(
    input_yaml_file: str,
    pydantic_yaml_file: str,
    pydantic_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
    silent: bool = False,
    use_refactored: bool = False,
) -> Tuple[bool, str]:
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

            if use_refactored:
                # Use refactored version
                try:
                    # Load original YAML data for comparison
                    with open(input_yaml_file, "r") as f:
                        original_data = yaml.safe_load(f)

                    config = SUEWSConfig.from_yaml(input_yaml_file)
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

                    # Create updated YAML with standardized header
                    copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)

                    # Generate refactored report using existing reporter functions
                    from .phase_c_reporter import (
                        generate_phase_c_report_refactored,
                        generate_fallback_report_refactored,
                    )

                    # If validation passes, create success report
                    if not critical_nulls:
                        # Use the refactored success report generator
                        reporter = ValidationReporter()
                        reporter.set_metadata("phase", "C")
                        reporter.set_metadata("mode", mode)
                        reporter.add_phase_results(
                            "C",
                            {
                                "pydantic_errors": 0,
                                "validation_mode": mode,
                                "validation_passed": True,
                                "defaults_applied": len(normal_defaults),
                            },
                        )

                        # Generate text report
                        phase_str = "".join(phases_run) if phases_run else "C"
                        text_report = f"""# SUEWS Phase {phase_str} (Pydantic Validation) Report
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

Phase {phase_str} passed

# =================================================="""

                        with open(pydantic_report_file, "w") as f:
                            f.write(text_report)

                        # Save JSON report
                        json_file = pydantic_report_file.replace(".txt", ".json")
                        reporter.save_json_report(Path(json_file))

                        supy_logger.setLevel(original_level)
                        if not silent:
                            print("✓ Phase C completed")
                        return True, pydantic_report_file
                    else:
                        # Generate failure report with critical nulls
                        generate_phase_c_report_refactored(
                            validation_error=ValueError(
                                f"Found {len(critical_nulls)} critical physics parameters set to null"
                            ),
                            input_yaml_file=input_yaml_file,
                            output_report_file=pydantic_report_file,
                            mode=mode,
                            phase_a_report_file=phase_a_report_file,
                            phases_run=phases_run,
                        )

                        supy_logger.setLevel(original_level)
                        if not silent:
                            print("✗ Phase C failed!")
                            print(f"Report: {pydantic_report_file}")
                            print(f"Updated YAML: {pydantic_yaml_file}")
                            print(
                                f"Suggestion: Fix issues in updated YAML and consider to run either phase AB or complete processor ABC."
                            )
                        return False, pydantic_report_file

                except Exception as validation_error:
                    # Restore logging level on validation error
                    supy_logger.setLevel(original_level)
                    # Generate error report using refactored system
                    generate_phase_c_report_refactored(
                        validation_error=validation_error,
                        input_yaml_file=input_yaml_file,
                        output_report_file=pydantic_report_file,
                        mode=mode,
                        phase_a_report_file=phase_a_report_file,
                        phases_run=phases_run,
                    )

                    if not silent:
                        print("✗ Phase C failed!")
                        print(f"Report: {pydantic_report_file}")
                        print(f"Updated YAML: {pydantic_yaml_file}")
                        print(
                            f"Suggestion: Fix issues in updated YAML and consider to run either phase AB or complete processor ABC."
                        )
                    return False, pydantic_report_file

            else:
                # Use original Phase C implementation
                try:
                    # Load original YAML data for comparison
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
                        passed_phases = [
                            f"PHASE {phase} - PASSED" for phase in phases_run
                        ]
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
                        return False, pydantic_report_file

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
                    return True, pydantic_report_file

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
                    return False, pydantic_report_file

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
            return False, pydantic_report_file

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
        return False, pydantic_report_file


def run_workflow(
    user_file: str,
    standard_file: str,
    phases: str,
    mode: str = "public",
    use_refactored: bool = False,
) -> bool:
    """
    Run the complete workflow with specified phases.

    Args:
        user_file: Path to user YAML
        standard_file: Path to standard YAML
        phases: Phases to run (e.g., "A", "AB", "ABC")
        mode: Validation mode
        use_refactored: Whether to use the refactored (JSON-based) system

    Returns:
        True if workflow completed successfully
    """
    # Parse phases
    phase_list = parse_phase(phases)

    # Set up file paths
    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]

    # Track outputs from each phase
    current_input = user_file
    phase_a_report = None
    phase_b_report = None
    existing_reporter = None if use_refactored else None

    # Print workflow header
    system_type = "Refactored (JSON-based)" if use_refactored else "Original"
    print(f"\n{'=' * 60}")
    print(f" SUEWS YAML Processor - {system_type} System")
    print(f" Phases: {phases} | Mode: {mode}")
    print(f"{'=' * 60}\n")

    # Run Phase A if requested
    if "A" in phase_list:
        print("Running Phase A (Up-to-date YAML check)...")
        output_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")

        success, phase_a_report = run_phase_a(
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            uptodate_file=output_file,
            report_file=report_file,
            mode=mode,
            phase=phases,
            use_refactored=use_refactored,
        )

        if not success and use_refactored:
            print("Phase A detected critical errors. Workflow halted.")
            return False

        current_input = output_file
        print(f"  ✓ Phase A complete: {output_file}")

        # Load reporter if refactored
        if use_refactored and phase_a_report:
            json_file = phase_a_report.replace(".txt", ".json")
            if os.path.exists(json_file):
                import json

                with open(json_file, "r") as f:
                    json_data = json.load(f)
                existing_reporter = ValidationReporter()
                existing_reporter.json_data = json_data

    # Run Phase B if requested
    if "B" in phase_list:
        print("Running Phase B (Scientific validation)...")
        output_file = os.path.join(dirname, f"updatedB_{basename}")
        report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")

        success, phase_b_report, science_data = run_phase_b(
            user_yaml_file=user_file,
            uptodate_file=current_input,
            standard_yaml_file=standard_file,
            science_yaml_file=output_file,
            science_report_file=report_file,
            phase_a_report_file=phase_a_report,
            phase_a_performed="A" in phase_list,
            mode=mode,
            phase=phases,
            use_refactored=use_refactored,
        )

        if not success:
            print("Phase B detected critical errors. Workflow halted.")
            return False

        current_input = output_file
        print(f"  ✓ Phase B complete: {output_file}")

        # Update reporter if refactored
        if use_refactored and phase_b_report:
            json_file = phase_b_report.replace(".txt", ".json")
            if os.path.exists(json_file):
                import json

                with open(json_file, "r") as f:
                    json_data = json.load(f)
                if existing_reporter:
                    new_reporter = ValidationReporter()
                    new_reporter.json_data = json_data
                    existing_reporter.merge(new_reporter)
                else:
                    existing_reporter = ValidationReporter()
                    existing_reporter.json_data = json_data

    # Run Phase C if requested
    if "C" in phase_list:
        print("Running Phase C (Pydantic validation)...")
        report_file = os.path.join(dirname, f"reportC_{name_without_ext}.txt")

        output_file = os.path.join(dirname, f"updatedC_{basename}")

        success, report = run_phase_c(
            input_yaml_file=current_input,
            pydantic_yaml_file=output_file,
            pydantic_report_file=report_file,
            mode=mode,
            phase_a_report_file=phase_a_report,
            phases_run=phase_list,
            use_refactored=use_refactored,
        )

        if not success:
            print("Phase C validation failed.")
            return False

        print(f"  ✓ Phase C complete")

    # Generate combined report if multiple phases
    if len(phase_list) > 1 and use_refactored and existing_reporter:
        combined_report = os.path.join(
            dirname, f"report{phases}_{name_without_ext}.txt"
        )
        combined_json = combined_report.replace(".txt", ".json")

        # Save combined JSON report
        existing_reporter.save_json_report(Path(combined_json))
        print(f"\n  ✓ Combined JSON report: {combined_json}")

    print(f"\n{'=' * 60}")
    print(f" Workflow Complete - {system_type} System")
    print(f"{'=' * 60}\n")

    return True


def main_original():
    """Main entry point for original Phase A-B-C workflow (for backward compatibility)."""
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

        # Use the original system by calling functions with use_refactored=False
        # This maintains full backward compatibility

        # Phase-specific execution using original system logic
        if phase == "A":
            # Phase A only
            phase_a_success = run_phase_a(
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="A",
                use_refactored=False,
            )
            if phase_a_success:
                print("Report:", report_file)
                print("Updated YAML:", uptodate_file)
            return 0 if phase_a_success else 1

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
                use_refactored=False,
            )
            if phase_c_success:
                print("✓ Phase C completed")
                print("Report:", pydantic_report_file)
                print("Updated YAML:", pydantic_yaml_file)
            else:
                # Phase C failed - remove YAML file if it was created during failure
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

        # For other phases (B, AB, AC, BC, ABC), use run_workflow with original system
        else:
            success = run_workflow(
                user_file=user_yaml_file,
                standard_file=standard_yaml_file,
                phases=phase,
                mode=internal_mode,
                use_refactored=False,
            )
            return 0 if success else 1

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


def main():
    """Main entry point for the orchestrator."""
    parser = argparse.ArgumentParser(
        description="SUEWS YAML Configuration Processor with JSON reporting"
    )
    parser.add_argument("user_yaml", help="Path to user YAML configuration file")
    parser.add_argument(
        "--standard",
        help="Path to standard YAML configuration file",
        default="src/supy/sample_data/sample_config.yml",
    )
    parser.add_argument(
        "--phase",
        choices=["A", "B", "C", "AB", "AC", "BC", "ABC"],
        default="ABC",
        help="Which phase(s) to run (default: ABC)",
    )
    parser.add_argument(
        "--mode",
        choices=["public", "dev"],
        default="public",
        help="Validation mode (default: public)",
    )
    parser.add_argument(
        "--use-refactored",
        action="store_true",
        help="Use refactored JSON-based validation system (default: use original)",
    )
    parser.add_argument(
        "--compare",
        action="store_true",
        help="Run both original and refactored systems and compare results",
    )

    args = parser.parse_args()

    # Check if user file exists
    if not os.path.exists(args.user_yaml):
        print(f"Error: User YAML file not found: {args.user_yaml}")
        return 1

    # Check if standard file exists
    if not os.path.exists(args.standard):
        print(f"Error: Standard YAML file not found: {args.standard}")
        return 1

    if args.compare:
        # Run both systems and compare
        print("\n" + "=" * 60)
        print(" COMPARISON MODE: Running both systems")
        print("=" * 60)

        # Run original system
        print("\n1. Running ORIGINAL system...")
        success_orig = run_workflow(
            user_file=args.user_yaml,
            standard_file=args.standard,
            phases=args.phase,
            mode=args.mode,
            use_refactored=False,
        )

        # Run refactored system
        print("\n2. Running REFACTORED system...")
        success_refactored = run_workflow(
            user_file=args.user_yaml,
            standard_file=args.standard,
            phases=args.phase,
            mode=args.mode,
            use_refactored=True,
        )

        # Compare results
        print("\n" + "=" * 60)
        print(" COMPARISON RESULTS")
        print("=" * 60)
        print(f"Original system:   {'✓ PASSED' if success_orig else '✗ FAILED'}")
        print(f"Refactored system: {'✓ PASSED' if success_refactored else '✗ FAILED'}")

        # Compare report files
        dirname = os.path.dirname(args.user_yaml)
        name_without_ext = os.path.splitext(os.path.basename(args.user_yaml))[0]

        for phase_char in parse_phase(args.phase):
            orig_report = os.path.join(
                dirname, f"report{phase_char}_{name_without_ext}.txt"
            )

            if os.path.exists(orig_report):
                print(f"\nPhase {phase_char} reports:")
                print(f"  Original:   {orig_report}")

                # Check for JSON report
                json_report = orig_report.replace(".txt", ".json")
                if os.path.exists(json_report):
                    print(f"  JSON:       {json_report}")

        print("\nYou can compare the text reports to verify they are identical.")
        print("The JSON reports provide additional structured information.")

        return 0 if success_orig and success_refactored else 1
    else:
        # Run single system
        success = run_workflow(
            user_file=args.user_yaml,
            standard_file=args.standard,
            phases=args.phase,
            mode=args.mode,
            use_refactored=args.use_refactored,
        )

        return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())

