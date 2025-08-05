"""
SUEWS Master Phase A-B-C Runner

This script provides a complete workflow for SUEWS YAML configuration processing:
- Phase A: Parameter detection and YAML structure updates (uptodate_yaml.py)
- Phase B: Scientific validation and automatic adjustments (science_check.py)
- Phase C: [Future] Final conditional Pydantic validation.

Usage:
    python master_ABC_run.py <user_yaml_file>
    
Example:
    python master_ABC_run.py my_config.yml
    
The script will:
1. Run Phase A to detect missing parameters and update YAML structure
2. If Phase A succeeds, run Phase B for scientific validation and adjustments  
3. Provide clear reporting of all phases and any issues encountered
4. Generate final science-checked YAML ready for SUEWS simulation

Input: Original user YAML configuration file
Output: Science-checked YAML configuration + comprehensive reports
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
    if not user_yaml_file.lower().endswith(('.yml', '.yaml')):
        raise ValueError(f"Input file must be a YAML file (.yml or .yaml): {user_yaml_file}")
    
    # Check if file is readable
    try:
        with open(user_yaml_file, 'r') as f:
            f.read(1)  # Try to read first character
    except PermissionError:
        raise PermissionError(f"Cannot read input file: {user_yaml_file}")
    
    return os.path.abspath(user_yaml_file)


def setup_output_paths(user_yaml_file: str, phase: str) -> Tuple[str, str, str, str, str]:
    """
    Generate all output file paths based on input file and phase.
    
    Args:
        user_yaml_file: Path to input user YAML file
        phase: Phase mode ('A', 'B', or 'AB')
        
    Returns:
        Tuple of (uptodate_file, report_file, science_yaml_file, science_report_file, dirname)
    """
    basename = os.path.basename(user_yaml_file)
    dirname = os.path.dirname(user_yaml_file)
    name_without_ext = os.path.splitext(basename)[0]
    
    if phase == 'AB':
        # Complete A→B workflow - use AB naming
        uptodate_file = os.path.join(dirname, f"updatedA_{basename}")  # Intermediate A file
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")  # Intermediate A report
        science_yaml_file = os.path.join(dirname, f"updatedAB_{basename}")  # Final AB file
        science_report_file = os.path.join(dirname, f"reportAB_{name_without_ext}.txt")  # Final AB report
    else:
        # Individual phases - use phase-specific naming
        uptodate_file = os.path.join(dirname, f"updated{phase}_{basename}")
        report_file = os.path.join(dirname, f"report{phase}_{name_without_ext}.txt")
        
        # Phase B outputs (used when phase == 'B')
        science_yaml_file = os.path.join(dirname, f"updatedB_{basename}")
        science_report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
    
    return uptodate_file, report_file, science_yaml_file, science_report_file, dirname


def run_phase_a(user_yaml_file: str, standard_yaml_file: str, 
                uptodate_file: str, report_file: str) -> bool:
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
                report_file=report_file
            )
        
        # Check if Phase A produced output files
        if not os.path.exists(uptodate_file):
            print("✗ Phase A failed: No uptodate YAML file generated")
            return False
            
        if not os.path.exists(report_file):
            print("✗ Phase A failed: No report file generated")
            return False
        
        # Check if uptodate file has Phase A header
        with open(uptodate_file, 'r') as f:
            content = f.read()
            if "UP TO DATE YAML" not in content:
                print("✗ Phase A failed: Missing Phase A completion header")
                return False
        
        # Check Phase A report for critical issues
        with open(report_file, 'r') as f:
            report_content = f.read()
            
        # Phase A should halt workflow if critical parameters are missing
        if "ACTION NEEDED" in report_content and "critical missing parameter" in report_content:
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
        print(f"✗ Phase A failed with error: {e}")
        return False


def run_phase_b(user_yaml_file: str, uptodate_file: str, standard_yaml_file: str,
                science_yaml_file: str, science_report_file: str, phase_a_report_file: str) -> bool:
    """
    Execute Phase B: Scientific validation and automatic adjustments.
    
    Args:
        user_yaml_file: Path to original user YAML
        uptodate_file: Path to Phase A output YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for Phase B output YAML
        science_report_file: Path for Phase B report
        
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
                phase_a_report_file=phase_a_report_file
            )
        
        # Check if Phase B produced output files
        if not os.path.exists(science_yaml_file):
            print("✗ Phase B failed: No science-checked YAML file generated")
            return False
            
        if not os.path.exists(science_report_file):
            print("✗ Phase B failed: No science report file generated") 
            return False
        
        # Check if Phase B report indicates critical issues
        with open(science_report_file, 'r') as f:
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
            print("  Suggestion: Fix the critical issues or run Phase A first if parameters are missing.")
            return False
        else:
            print()
            print(f"✗ Phase B failed: Validation error - {e}")
            print(f"  Check reportB file for details: {science_report_file}")
            return False
    except Exception as e:
        print(f"✗ Phase B failed with unexpected error: {e}")
        return False



def main():
    """Main entry point for master Phase A-B-C workflow."""
    
    # Setup command line argument parsing
    parser = argparse.ArgumentParser(
        description="SUEWS Configuration Processor - Phase A and/or Phase B workflow",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python master_ABC_run.py user.yml                    # Run complete A→B workflow (default)
  python master_ABC_run.py user.yml --phase A          # Run Phase A only
  python master_ABC_run.py user.yml --phase B          # Run Phase B only (requires updatedA_user.yml)
  python master_ABC_run.py user.yml --phase AB         # Run complete A→B workflow (explicit)

Phases:
  Phase A: Up to date YAML processor and missing parameter handling
  Phase B: Scientific validation and updating
        """
    )
    
    parser.add_argument('yaml_file', 
                       help='Input YAML configuration file')
    
    parser.add_argument('--phase', '-p',
                       choices=['A', 'B', 'AB'], 
                       default='AB',
                       help='Phase to run: A (up to date YAML processor), B (scientific check), or AB (complete workflow, default)')
    
    args = parser.parse_args()
    user_yaml_file = args.yaml_file
    phase = args.phase
    
    # Print workflow header
    phase_desc = {"A": "Phase A Only", "B": "Phase B Only", "AB": "Complete A→B Workflow"}
    print(f"=============================")
    print(f"SUEWS Configuration Processor")
    print(f"=============================")
    print(f"YAML user file: {os.path.basename(user_yaml_file)}")
    print(f"Processor Selected Mode: {phase_desc[phase]}")
    print(f"=============================")
    print()
    
    try:
        # Step 1: Validate input file
        user_yaml_file = validate_input_file(user_yaml_file)
        
        # Step 2: Setup paths
        standard_yaml_file = "src/supy/sample_run/sample_config.yml"
        if not os.path.exists(standard_yaml_file):
            print(f"✗ Standard YAML file not found: {standard_yaml_file}")
            print("Make sure you're running from the SUEWS root directory")
            return 1
        
        uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(user_yaml_file, phase)
        
        # Phase-specific execution
        if phase == 'A':
            # Phase A only
            phase_a_success = run_phase_a(user_yaml_file, standard_yaml_file, uptodate_file, report_file)
            if phase_a_success:
                print()
                print(f" Phase A completed: {os.path.basename(uptodate_file)}")
                print(f" Report: {os.path.basename(report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_a_success else 1
            
        elif phase == 'B':
            # Phase B only - can run on user YAML directly or use Phase A output if available
            input_yaml_file = user_yaml_file
            phase_a_report = None
            
            # Check if Phase A output exists and use it, otherwise use original user YAML
            if os.path.exists(uptodate_file):
                print("Using existing Phase A output...")
                input_yaml_file = uptodate_file
                phase_a_report = report_file if os.path.exists(report_file) else None
            else:
                print("Running Phase B directly on user YAML...")
                # Phase B will handle parameter detection internally
            
            phase_b_success = run_phase_b(user_yaml_file, input_yaml_file, standard_yaml_file,
                                         science_yaml_file, science_report_file, phase_a_report)
            if phase_b_success:
                print()
                print(f" Phase B completed: {os.path.basename(science_yaml_file)}")
                print(f" Report: {os.path.basename(science_report_file)}")
                print(f" File locations: {dirname}")
            return 0 if phase_b_success else 1
            
        else:  # phase == 'AB'
            # Complete A→B workflow (existing logic)
            phase_a_success = run_phase_a(user_yaml_file, standard_yaml_file, uptodate_file, report_file)
            
            if not phase_a_success:
                return 1
            
            phase_b_success = run_phase_b(user_yaml_file, uptodate_file, standard_yaml_file,
                                         science_yaml_file, science_report_file, report_file)
            
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
                print(f" Ready for SUEWS simulation: {os.path.basename(science_yaml_file)}")
                print(f" Report: {os.path.basename(science_report_file)}")
                print(f" File locations: {dirname}")
            
            return 0 if workflow_success else 1
        
    except FileNotFoundError as e:
        print(f"✗ File error: {e}")
        return 1
    except ValueError as e:
        print(f"✗ Input error: {e}")
        return 1
    except Exception as e:
        print(f"✗ Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    exit(main())