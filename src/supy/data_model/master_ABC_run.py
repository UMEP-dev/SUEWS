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
    user_yaml_file: str, standard_yaml_file: str, uptodate_file: str, report_file: str
) -> bool:
    """
    Execute Phase A: Parameter detection and YAML structure updates.

    Args:
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        uptodate_file: Path for Phase A output YAML
        report_file: Path for Phase A report

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

        # Phase A should halt workflow if critical parameters are missing
        if (
            "ACTION NEEDED" in report_content
            and "critical missing parameter" in report_content
        ):
            print()
            print("✗ Phase A halted: Critical parameters missing")
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
                config = SUEWSConfig.from_yaml(input_yaml_file)

                # If we get here, Pydantic validation passed
                success_report = f"""# SUEWS Phase C (Pydantic Validation) Report
# ============================================

## PHASE C - PASSED

All conditional Pydantic validation checks completed successfully:
- Model physics method compatibility validated
- Conditional parameter requirements satisfied  
- RefValue wrapper validation passed
- Physical constraint validation passed

Input file: {input_yaml_file}
Validation result: Pydantic validation completed successfully

No further action required.
"""

                with open(pydantic_report_file, "w") as f:
                    f.write(success_report)

                print("✓ Phase C completed - Pydantic validation passed")
                return True

            except Exception as validation_error:
                # Pydantic validation failed - generate structured ACTION NEEDED report
                try:
                    from phase_c_reports import generate_phase_c_report

                    generate_phase_c_report(
                        validation_error, input_yaml_file, pydantic_report_file
                    )

                except Exception as report_error:
                    # Fallback to simple error report if structured report generation fails
                    from phase_c_reports import generate_fallback_report

                    generate_fallback_report(
                        validation_error, input_yaml_file, pydantic_report_file
                    )

                print("✗ Phase C failed - Pydantic validation errors detected: \n")
                print(f"  Error: {validation_error}")
                print(f"  \nReport generated: {os.path.basename(pydantic_report_file)}")
                print(f"  Check ACTION NEEDED section in report for required fixes")
                return False

        except ImportError as import_error:
            print(f"✗ Phase C failed - Cannot import SUEWSConfig: {import_error}")

            # Import error report
            error_report = f"""# SUEWS Phase C (Pydantic Validation) Report
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
  python master_ABC_run.py user.yml                    # Run complete A→B workflow (default)
  python master_ABC_run.py user.yml --phase A          # Run Phase A only
  python master_ABC_run.py user.yml --phase B          # Run Phase B only
  python master_ABC_run.py user.yml --phase C          # Run Phase C only (Pydantic validation)
  python master_ABC_run.py user.yml --phase AB         # Run complete A→B workflow (explicit)
  python master_ABC_run.py user.yml --phase ABC        # Run complete A→B→C workflow

Phases:
  Phase A: Parameter detection and YAML structure updates
  Phase B: Scientific validation and automatic adjustments
  Phase C: Conditional Pydantic validation based on model physics options
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

    args = parser.parse_args()
    user_yaml_file = args.yaml_file
    phase = args.phase

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
    print(f"==================================")
    print()

    try:
        # Step 1: Validate input file
        user_yaml_file = validate_input_file(user_yaml_file)

        # Step 2: Setup paths
        standard_yaml_file = "src/supy/sample_run/sample_config.yml"
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
                user_yaml_file, standard_yaml_file, uptodate_file, report_file
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
            )
            if phase_c_success:
                print()
                print(f" Phase C completed: Pydantic validation passed")
                print(f" Report: {os.path.basename(pydantic_report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_c_success else 1

        elif phase == "AB":
            # Complete A→B workflow (existing logic)
            phase_a_success = run_phase_a(
                user_yaml_file, standard_yaml_file, uptodate_file, report_file
            )

            if not phase_a_success:
                return 1

            phase_b_success = run_phase_b(
                user_yaml_file,
                uptodate_file,
                standard_yaml_file,
                science_yaml_file,
                science_report_file,
                report_file,
                phase_a_performed=True,  # A→B workflow mode
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

        else:
            # Placeholder for other phase combinations: AC, BC, ABC
            print(f"✗ Phase combination '{phase}' not yet implemented")
            print("Available phases: A, B, C, AB")
            print("Coming soon: AC, BC, ABC workflows")
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
