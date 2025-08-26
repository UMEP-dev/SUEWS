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

# Import original phase functions
try:
    from .phase_a_parameter_update import annotate_missing_parameters
    from .phase_b_science_check import run_science_check
    from .orchestrator import (
        detect_pydantic_defaults,
        run_phase_c as run_phase_c_original,
        parse_phase,
    )
except ImportError as e:
    print(f"Error importing original modules: {e}")
    sys.exit(1)

# Import refactored phase functions
try:
    from .phase_a_refactored import annotate_missing_parameters_refactored
    from .phase_b_refactored import run_science_check_refactored
    from .phase_c_refactored import (
        generate_phase_c_report_refactored,
        generate_fallback_report_refactored,
    )
    from .validation_reporter import ValidationReporter
except ImportError as e:
    print(f"Error importing refactored modules: {e}")
    sys.exit(1)


def run_phase_a(
    user_file: str,
    standard_file: str,
    output_file: str,
    report_file: str,
    mode: str = "public",
    phase: str = "A",
    use_refactored: bool = False,
) -> Tuple[bool, str]:
    """
    Run Phase A validation with switch for old/new system.
    
    Args:
        user_file: Path to user YAML
        standard_file: Path to standard YAML
        output_file: Path for updated YAML output
        report_file: Path for report output
        mode: Validation mode
        phase: Phase identifier
        use_refactored: Whether to use the refactored (JSON-based) system
    
    Returns:
        Tuple of (success, report_path)
    """
    try:
        if use_refactored:
            # Use refactored version that returns ValidationReporter
            reporter = annotate_missing_parameters_refactored(
                user_file=user_file,
                standard_file=standard_file,
                uptodate_file=output_file,
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
        else:
            # Use original version
            annotate_missing_parameters(
                user_file=user_file,
                standard_file=standard_file,
                uptodate_file=output_file,
                report_file=report_file,
                mode=mode,
                phase=phase,
            )
            success = True  # Original doesn't return success status
        
        return success, report_file
    except Exception as e:
        print(f"Phase A failed: {e}")
        return False, None


def run_phase_b(
    uptodate_file: str,
    user_file: str,
    standard_file: str,
    output_file: str,
    report_file: str,
    phase_a_report: str = None,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
    use_refactored: bool = False,
) -> Tuple[bool, str, dict]:
    """
    Run Phase B validation with switch for old/new system.
    
    Args:
        uptodate_file: Path to Phase A output
        user_file: Path to original user YAML
        standard_file: Path to standard YAML
        output_file: Path for science-checked output
        report_file: Path for report output
        phase_a_report: Path to Phase A report
        phase_a_performed: Whether Phase A was performed
        mode: Validation mode
        phase: Phase identifier
        use_refactored: Whether to use the refactored system
    
    Returns:
        Tuple of (success, report_path, science_data)
    """
    try:
        if use_refactored:
            # Use refactored version
            science_data, reporter = run_science_check_refactored(
                uptodate_yaml_file=uptodate_file,
                user_yaml_file=user_file,
                standard_yaml_file=standard_file,
                science_yaml_file=output_file,
                science_report_file=report_file,
                phase_a_report_file=phase_a_report,
                phase_a_performed=phase_a_performed,
                mode=mode,
                phase=phase,
            )
            # Check if Phase B passed
            json_data = reporter.get_json_report()
            success = json_data.get("summary", {}).get("validation_passed", False)
        else:
            # Use original version
            science_data = run_science_check(
                uptodate_yaml_file=uptodate_file,
                user_yaml_file=user_file,
                standard_yaml_file=standard_file,
                science_yaml_file=output_file,
                science_report_file=report_file,
                phase_a_report_file=phase_a_report,
                phase_a_performed=phase_a_performed,
                mode=mode,
                phase=phase,
            )
            success = True  # Original doesn't fail on warnings
        
        return success, report_file, science_data
    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            print("\nPhase B halted due to critical errors.")
            return False, report_file, None
        else:
            print(f"Phase B failed: {e}")
            return False, None, None
    except Exception as e:
        print(f"Phase B failed: {e}")
        return False, None, None


def run_phase_c(
    science_file: str,
    standard_file: str,
    report_file: str,
    mode: str = "public",
    phase_a_report: str = None,
    phases_run: list = None,
    use_refactored: bool = False,
    existing_reporter: ValidationReporter = None,
) -> Tuple[bool, str]:
    """
    Run Phase C validation with switch for old/new system.
    
    Args:
        science_file: Path to Phase B output
        standard_file: Path to standard YAML
        report_file: Path for report output
        mode: Validation mode
        phase_a_report: Path to Phase A report
        phases_run: List of phases run
        use_refactored: Whether to use the refactored system
        existing_reporter: Existing reporter from previous phases (for refactored)
    
    Returns:
        Tuple of (success, report_path)
    """
    if use_refactored:
        # Import Pydantic model for validation
        try:
            # Try to import the Pydantic model
            from supy.data_model.core.model import ModelConfig
            
            # Load the science-checked YAML
            with open(science_file, "r") as f:
                science_data = yaml.safe_load(f)
            
            with open(standard_file, "r") as f:
                standard_data = yaml.safe_load(f)
            
            try:
                # Try Pydantic validation
                config = ModelConfig(**science_data)
                
                # If validation passes, generate success report
                if existing_reporter:
                    reporter = existing_reporter
                else:
                    reporter = ValidationReporter()
                
                reporter.set_metadata("phase", "C")
                reporter.set_metadata("mode", mode)
                reporter.add_phase_results("C", {
                    "pydantic_errors": 0,
                    "validation_mode": mode,
                    "validation_passed": True
                })
                
                # Generate text report
                phase_str = "".join(phases_run) if phases_run else "C"
                from .phase_c_refactored import generate_phase_c_text_report
                text_report = generate_phase_c_text_report(
                    reporter.get_json_report(), phase_str, mode
                )
                
                with open(report_file, "w") as f:
                    f.write(text_report)
                
                # Save JSON report
                json_file = report_file.replace('.txt', '.json')
                reporter.save_json_report(Path(json_file))
                
                return True, report_file
                
            except Exception as validation_error:
                # Validation failed - generate error report
                reporter = generate_phase_c_report_refactored(
                    validation_error=validation_error,
                    input_yaml_file=science_file,
                    output_report_file=report_file,
                    mode=mode,
                    phase_a_report_file=phase_a_report,
                    phases_run=phases_run,
                    existing_reporter=existing_reporter,
                )
                return False, report_file
                
        except ImportError as e:
            print(f"Cannot import Pydantic model: {e}")
            print("Skipping Phase C validation")
            return True, None
        except Exception as e:
            # Generate fallback report
            generate_fallback_report_refactored(
                validation_error=e,
                input_yaml_file=science_file,
                output_report_file=report_file,
                mode=mode,
                phase_a_report_file=phase_a_report,
                phases_run=phases_run,
            )
            return False, report_file
    else:
        # Use original Phase C
        return run_phase_c_original(
            science_file=science_file,
            standard_file=standard_file,
            report_file=report_file,
            mode=mode,
            phase_a_report=phase_a_report,
            phases_run=phases_run,
        )


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
    print(f"\n{'='*60}")
    print(f" SUEWS YAML Processor - {system_type} System")
    print(f" Phases: {phases} | Mode: {mode}")
    print(f"{'='*60}\n")
    
    # Run Phase A if requested
    if "A" in phase_list:
        print("Running Phase A (Up-to-date YAML check)...")
        output_file = os.path.join(dirname, f"updatedA_{basename}")
        report_file = os.path.join(dirname, f"reportA_{name_without_ext}.txt")
        
        success, phase_a_report = run_phase_a(
            user_file=user_file,
            standard_file=standard_file,
            output_file=output_file,
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
            json_file = phase_a_report.replace('.txt', '.json')
            if os.path.exists(json_file):
                import json
                with open(json_file, 'r') as f:
                    json_data = json.load(f)
                existing_reporter = ValidationReporter()
                existing_reporter.json_data = json_data
    
    # Run Phase B if requested
    if "B" in phase_list:
        print("Running Phase B (Scientific validation)...")
        output_file = os.path.join(dirname, f"updatedB_{basename}")
        report_file = os.path.join(dirname, f"reportB_{name_without_ext}.txt")
        
        success, phase_b_report, science_data = run_phase_b(
            uptodate_file=current_input,
            user_file=user_file,
            standard_file=standard_file,
            output_file=output_file,
            report_file=report_file,
            phase_a_report=phase_a_report,
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
            json_file = phase_b_report.replace('.txt', '.json')
            if os.path.exists(json_file):
                import json
                with open(json_file, 'r') as f:
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
        
        success, report = run_phase_c(
            science_file=current_input,
            standard_file=standard_file,
            report_file=report_file,
            mode=mode,
            phase_a_report=phase_a_report,
            phases_run=phase_list,
            use_refactored=use_refactored,
            existing_reporter=existing_reporter,
        )
        
        if not success:
            print("Phase C validation failed.")
            return False
        
        print(f"  ✓ Phase C complete")
    
    # Generate combined report if multiple phases
    if len(phase_list) > 1 and use_refactored and existing_reporter:
        combined_report = os.path.join(dirname, f"report{phases}_{name_without_ext}.txt")
        combined_json = combined_report.replace('.txt', '.json')
        
        # Save combined JSON report
        existing_reporter.save_json_report(Path(combined_json))
        print(f"\n  ✓ Combined JSON report: {combined_json}")
    
    print(f"\n{'='*60}")
    print(f" Workflow Complete - {system_type} System")
    print(f"{'='*60}\n")
    
    return True


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
        print("\n" + "="*60)
        print(" COMPARISON MODE: Running both systems")
        print("="*60)
        
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
        print("\n" + "="*60)
        print(" COMPARISON RESULTS")
        print("="*60)
        print(f"Original system:   {'✓ PASSED' if success_orig else '✗ FAILED'}")
        print(f"Refactored system: {'✓ PASSED' if success_refactored else '✗ FAILED'}")
        
        # Compare report files
        dirname = os.path.dirname(args.user_yaml)
        name_without_ext = os.path.splitext(os.path.basename(args.user_yaml))[0]
        
        for phase_char in parse_phase(args.phase):
            orig_report = os.path.join(dirname, f"report{phase_char}_{name_without_ext}.txt")
            
            if os.path.exists(orig_report):
                print(f"\nPhase {phase_char} reports:")
                print(f"  Original:   {orig_report}")
                
                # Check for JSON report
                json_report = orig_report.replace('.txt', '.json')
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