"""SUEWS YAML Configuration Processor

Three-phase validation workflow:
- Configuration structure check and parameter detection
- Physics validation and automatic adjustments
- Configuration consistency validation based on physics options

Supports individual phases (A, B, C) or combined workflows (AB, AC, BC, ABC).
"""

import sys
import os
import argparse
import yaml
from typing import Tuple, Optional
import importlib.resources
import shutil
import io
from contextlib import redirect_stdout, redirect_stderr
import supy
from pathlib import Path

# Import Phase A and B functions
try:
    from .phase_a import annotate_missing_parameters
    from .phase_b import run_science_check
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
    """Detect where the validation system applied defaults and separate critical nulls from normal defaults."""
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
                    # Use GRIDID for sites array instead of numeric index
                    if (
                        current_path == "sites"
                        and isinstance(orig_item, dict)
                        and "gridiv" in orig_item
                    ):
                        gridiv = orig_item["gridiv"]
                        # Handle RefValue objects
                        if isinstance(gridiv, dict) and "value" in gridiv:
                            gridid = gridiv["value"]
                        else:
                            gridid = gridiv
                        list_path = f"{current_path}.{gridid}"
                    else:
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
    yaml_path = Path(user_yaml_file)
    if not yaml_path.exists():
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

    return str(yaml_path.resolve())


def setup_output_paths(
    user_yaml_file: str, phase: str
) -> Tuple[str, str, str, str, str, str, str]:
    """Generate output file paths based on input file and phase."""
    user_path = Path(user_yaml_file)
    basename = user_path.name
    dirname = user_path.parent
    name_without_ext = user_path.stem

    uptodate_file = None
    report_file = None
    science_yaml_file = None
    science_report_file = None
    pydantic_yaml_file = None
    pydantic_report_file = None

    # Final report and YAML paths
    final_yaml = str(dirname / f"updated_{basename}")
    final_report = str(dirname / f"report_{name_without_ext}.txt")

    # Temporary intermediate files (will be consolidated and removed)
    temp_report_a = str(dirname / f"temp_reportA_{name_without_ext}.txt")
    temp_report_b = str(dirname / f"temp_reportB_{name_without_ext}.txt")

    if phase == "A":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = final_report  # Phase A only -> direct to final
    elif phase == "B":
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = final_report  # Phase B only -> direct to final
    elif phase == "C":
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Phase C only -> direct to final
    elif phase == "AB":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        science_yaml_file = final_yaml
        science_report_file = final_report  # Final report will consolidate A+B
    elif phase == "AC":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate A+C
    elif phase == "BC":
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = temp_report_b  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate B+C
    elif phase == "ABC":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = temp_report_b  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate A+B+C

    return (
        uptodate_file,
        report_file,
        science_yaml_file,
        science_report_file,
        pydantic_yaml_file,
        pydantic_report_file,
        dirname,
    )


def extract_no_action_messages_from_report(report_file: str) -> list:
    """Extract NO ACTION NEEDED messages from a report file."""
    messages = []

    if not report_file or not os.path.exists(report_file):
        return messages

    try:
        with open(report_file, "r") as f:
            content = f.read()

        # Extract NO ACTION NEEDED section
        lines = content.split("\n")
        in_no_action = False

        for line in lines:
            if line.strip().startswith("## NO ACTION NEEDED"):
                in_no_action = True
                continue
            elif line.strip().startswith("##") and in_no_action:
                # Hit another section, stop collecting
                break
            elif line.strip().startswith("#") and in_no_action:
                # Skip any separator lines (like # ==================================================)
                continue
            elif in_no_action and line.strip():
                messages.append(line)

    except Exception:
        pass  # Skip if can't read file

    return messages


def create_consolidated_report(
    phases_run: list,
    no_action_messages: list,
    final_report_file: str,
    mode: str = "public",
) -> None:
    """Create final consolidated report with all NO ACTION NEEDED messages."""

    phase_str = "".join(phases_run) if phases_run else "Combined"

    # Use unified report title for all validation phases
    title = "SUEWS Validation Report"

    # Deduplicate messages while preserving order
    # Also filter out the generic "All validations passed" message if there are other messages
    seen = set()
    deduplicated_messages = []
    for message in no_action_messages:
        # Skip the generic "all passed" message - it will be added later if needed
        if message.strip() == "- All validations passed with no issues detected":
            continue
        if message not in seen:
            seen.add(message)
            deduplicated_messages.append(message)

    # Remove orphaned headers (headers ending with : that have no following detail lines starting with --)
    filtered_messages = []
    i = 0
    while i < len(deduplicated_messages):
        msg = deduplicated_messages[i]
        # Check if this is a header line (starts with - but not --, ends with :)
        if (
            msg.strip().startswith("- ")
            and not msg.strip().startswith("-- ")
            and msg.strip().endswith(":")
        ):
            # Check if next line exists and is a detail line (starts with --)
            if i + 1 < len(deduplicated_messages) and deduplicated_messages[
                i + 1
            ].strip().startswith("-- "):
                # Has following details, keep it
                filtered_messages.append(msg)
            # else: orphaned header, skip it
        else:
            # Not a header, keep it
            filtered_messages.append(msg)
        i += 1

    # Build final report content
    report_content = f"""# {title}
# {"=" * 50}
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# {"=" * 50}

## NO ACTION NEEDED"""

    if filtered_messages:
        for message in filtered_messages:
            report_content += f"\n{message}"
    else:
        report_content += "\n- All validations passed with no issues detected"

    report_content += f"\n\n# {'=' * 50}"

    # Write final consolidated report
    with open(final_report_file, "w") as f:
        f.write(report_content)


def create_final_user_files(user_yaml_file: str, source_yaml: str, source_report: str):
    """Create final user-facing files with simple names from intermediate files."""
    import shutil

    dirname = os.path.dirname(user_yaml_file)
    basename = os.path.basename(user_yaml_file)
    name_without_ext = os.path.splitext(basename)[0]

    final_yaml = os.path.join(dirname, f"updated_{basename}")
    final_report = os.path.join(dirname, f"report_{name_without_ext}.txt")

    # Move/copy intermediate files to final user-facing names
    try:
        if Path(source_yaml).exists():
            shutil.move(source_yaml, final_yaml)  # Move instead of copy

        if Path(source_report).exists():
            shutil.move(source_report, final_report)  # Move instead of copy
    except Exception as e:
        # Fallback to copy if move fails
        try:
            if Path(source_yaml).exists():
                shutil.copy2(source_yaml, final_yaml)
            if Path(source_report).exists():
                shutil.copy2(source_report, final_report)
        except Exception:
            pass  # If all fails, at least return the expected paths

    return final_yaml, final_report


def run_phase_a(
    user_yaml_file: str,
    standard_yaml_file: str,
    uptodate_file: str,
    report_file: str,
    mode: str = "public",
    phase: str = "A",
    silent: bool = False,
) -> bool:
    """Execute Phase A: Configuration structure checks and parameter updates."""
    if not silent:
        print("Configuration structure check...")

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
                print("✗ Validation failed - check report for details")
            return False

        if not os.path.exists(report_file):
            if not silent:
                print()
                print("✗ Validation failed - unable to generate report")
            return False

        with open(uptodate_file, "r") as f:
            content = f.read()
            if "Updated YAML" not in content:
                if not silent:
                    print()
                    print("✗ Validation failed - check report for details")
                return False

        with open(report_file, "r") as f:
            report_content = f.read()

        if "## ACTION NEEDED" in report_content:
            if not silent:
                print("✗ Validation failed!")
                print(f"Report: {report_file}")
                print(f"Updated YAML: {uptodate_file}")
            return False

        if not silent:
            print("[OK] Validation completed")
        return True

    except Exception as e:
        if not silent:
            print()
            print(f"✗ Validation failed with error: {e}")
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
    """Execute Phase B: Physics validation checks and automatic adjustments."""
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
                print("✗ Validation failed - check report for details")
            return False

        if not os.path.exists(science_report_file):
            if not silent:
                print()
                print("✗ Validation failed - unable to generate report")
            return False

        # Check if Phase B report indicates critical issues
        with open(science_report_file, "r") as f:
            report_content = f.read()

        if "CRITICAL ISSUES DETECTED" in report_content or "URGENT" in report_content:
            if not silent:
                print("✗ Validation failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
            return False

        if not silent:
            print("[OK] Phase B completed")
        return True

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            if not silent:
                print("✗ Validation failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
            return False
        else:
            if not silent:
                print("✗ Validation failed!")
                print(f"Report: {science_report_file}")
                print(f"Updated YAML: {science_yaml_file}")
            return False
    except Exception as e:
        if not silent:
            print()
            print(f"✗ Validation failed with unexpected error: {e}")
        return False


def run_phase_c(
    input_yaml_file: str,
    pydantic_yaml_file: str,
    pydantic_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    science_report_file: str = None,
    phases_run: list = None,
    no_action_messages: list = None,
    silent: bool = False,
) -> bool:
    """Execute Phase C: Configuration consistency validation based on physics options."""
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
                    # Use importlib.resources for robust package resource access
                    sample_data_files = importlib.resources.files(supy) / "sample_data"
                    with importlib.resources.as_file(
                        sample_data_files / "sample_config.yml"
                    ) as standard_yaml_path:
                        with open(standard_yaml_path, "r") as f:
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

                    failure_report = f"""# SUEWS Validation Report
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================
{action_needed}
# =================================================="""

                    with open(pydantic_report_file, "w") as f:
                        f.write(failure_report)

                    if not silent:
                        print("✗ Validation failed!")
                        print(f"Report: {pydantic_report_file}")
                        print(f"Updated YAML: {pydantic_yaml_file}")
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
                        no_action_info += f"- {field_name} {status} in user YAML at level {field_path}.\n  The validation system will interpret that as default value: {default_value} - check doc for info on this parameter: https://suews.readthedocs.io/en/latest\n"

                # Generate phase-specific title for success report
                if phases_run:
                    phase_str = "".join(phases_run)
                else:
                    phase_str = "C"  # Default to Phase C only

                # Use unified report title for all validation phases
                title = "SUEWS Validation Report"

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
                    # Map phase strings to descriptive messages
                    def get_phase_message(phase_str):
                        if phase_str == "A":
                            return "Configuration structure check passed"
                        elif phase_str == "B":
                            return "Physics validation check passed"
                        elif phase_str == "C":
                            return "Configuration consistency check passed"
                        elif phase_str == "AB":
                            return "Configuration structure check and Physics validation check passed"
                        elif phase_str == "BC":
                            return "Physics validation check and Configuration consistency check passed"
                        elif phase_str == "AC":
                            return "Configuration structure check and Configuration consistency check passed"
                        elif phase_str == "ABC":
                            return "All validation checks passed"
                        else:
                            return f"Phase {phase_str} passed"  # fallback

                    # Check if this is a multi-phase call that needs consolidation
                    if phases_run and len(phases_run) > 1:
                        # Multi-phase consolidation: use passed messages or extract from files
                        if no_action_messages is not None:
                            # Use passed messages (variable-based approach)
                            all_messages = no_action_messages
                        else:
                            # Fallback: extract from files (file-based approach)
                            all_messages = []
                            if phase_a_report_file and os.path.exists(
                                phase_a_report_file
                            ):
                                all_messages.extend(
                                    extract_no_action_messages_from_report(
                                        phase_a_report_file
                                    )
                                )
                            if science_report_file and os.path.exists(
                                science_report_file
                            ):
                                all_messages.extend(
                                    extract_no_action_messages_from_report(
                                        science_report_file
                                    )
                                )

                        # Create consolidated final report
                        create_consolidated_report(
                            phases_run=phases_run,
                            no_action_messages=all_messages,
                            final_report_file=pydantic_report_file,
                            mode=mode,
                        )

                        # Clean up any remaining intermediate files
                        if phase_a_report_file and Path(phase_a_report_file).exists():
                            Path(phase_a_report_file).unlink()
                        if science_report_file and Path(science_report_file).exists():
                            Path(science_report_file).unlink()
                    else:
                        # Single phase: use regular report
                        success_report = f"""# {title}
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

{get_phase_message(phase_str)}

# =================================================="""

                        with open(pydantic_report_file, "w") as f:
                            f.write(success_report)

                # Ensure report is always generated for successful validation
                if not os.path.exists(pydantic_report_file):
                    # Fallback: create a simple success report
                    simple_success_report = f"""# {title}
# ============================================
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# ============================================

Validation passed

# =================================================="""
                    with open(pydantic_report_file, "w") as f:
                        f.write(simple_success_report)

                # Restore logging level before return
                supy_logger.setLevel(original_level)
                if not silent:
                    print("[OK] Validation completed")
                return True

            except Exception as validation_error:
                # Restore logging level on validation error
                supy_logger.setLevel(original_level)
                # Pydantic validation failed - still create updatedC YAML with standardized header
                copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)

                # Generate structured ACTION NEEDED report
                try:
                    from .phase_c import generate_phase_c_report

                    generate_phase_c_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                        phases_run,
                        no_action_messages,
                    )

                except Exception as report_error:
                    # Fallback to simple error report if structured report generation fails
                    from .phase_c import generate_fallback_report

                    generate_fallback_report(
                        validation_error,
                        input_yaml_file,
                        pydantic_report_file,
                        mode,
                        phase_a_report_file,
                        phases_run,
                        no_action_messages,
                    )

                if not silent:
                    print("✗ Validation failed!")
                    print(f"Report: {pydantic_report_file}")
                    print(f"Updated YAML: {pydantic_yaml_file}")
                return False

        except ImportError as import_error:
            if not silent:
                print(
                    f"✗ Validation failed - Cannot import SUEWSConfig: {import_error}"
                )

            # Import error report
            error_report = f"""# SUEWS Validation Report
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
            print(f"✗ Validation failed: {e}")

        # General error report
        error_report = f"""# SUEWS Validation Report
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
  python suews_yaml_processor.py user.yml --phase C              # Run Phase C only (consistency checks)
  python suews_yaml_processor.py user.yml --phase BC             # Run complete B→C workflow
  python suews_yaml_processor.py user.yml --mode dev             # Run full validation in dev mode (available)
  python suews_yaml_processor.py user.yml --phase A --mode public  # Run Phase A in public mode (explicit)

Phases:
  Phase A: Configuration structure checks and parameter updates
  Phase B: Physics validation checks and automatic adjustments
  Phase C: Configuration consistency checks based on model physics options

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
        help="Phase to run: A (structure check), B (physics validation), C (consistency check), AB (A→B workflow), AC (A→C), BC (B→C), or ABC (complete workflow, default)",
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
        # Use importlib.resources for robust package resource access
        sample_data_files = importlib.resources.files(supy) / "sample_data"
        with importlib.resources.as_file(
            sample_data_files / "sample_config.yml"
        ) as standard_yaml_path:
            standard_yaml_file = str(standard_yaml_path)

        # Print workflow header (after variables are defined)
        phase_desc = {
            "A": "Phase A",
            "B": "Phase B",
            "C": "Phase C",
            "AB": "Phase AB",
            "AC": "Phase AC",
            "BC": "Phase BC",
            "ABC": "Full Validation",
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

                # Check Snowuse restriction
                snowuse = None
                if (
                    user_yaml_data
                    and "model" in user_yaml_data
                    and "physics" in user_yaml_data["model"]
                    and "snowuse" in user_yaml_data["model"]["physics"]
                ):
                    snowuse_entry = user_yaml_data["model"]["physics"]["snowuse"]
                    # Handle both direct values and RefValue format
                    if isinstance(snowuse_entry, dict) and "value" in snowuse_entry:
                        snowuse = snowuse_entry["value"]
                    else:
                        snowuse = snowuse_entry

                if snowuse is not None and snowuse != 0:
                    restrictions_violated.append(
                        "Snow calculations are enabled (snowuse != 0)"
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
                        "  2. Disable developer features in your YAML file and rerun processor:"
                    )
                    print("     - Set stebbsmethod = 0 (if enabled)")
                    print("     - Set snowuse = 0 (if enabled)")
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
        print(f"DEBUG: Executing phase '{phase}'")
        if phase == "A":
            # Phase A only
            phase_a_success = run_phase_a(
                user_yaml_file,
                standard_yaml_file,
                uptodate_file,
                report_file,
                mode=internal_mode,
                phase="A",
                silent=True,  # Suppress phase function output, main function handles terminal output
            )

            # Always create final user files with simple names
            final_yaml, final_report = create_final_user_files(
                user_yaml_file, uptodate_file, report_file
            )

            if phase_a_success:
                print("[OK] Validation completed")
                print("Report:", final_report)
                print("Updated YAML:", final_yaml)
            else:
                print("✗ Validation failed!")
                print("Report:", final_report)
                print("Updated YAML:", final_yaml)
            return 0 if phase_a_success else 1

        elif phase == "B":
            # Phase B only - always run on original user YAML for pure Phase B validation
            input_yaml_file = user_yaml_file
            phase_a_report = None

            print("Physics validation check...")
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

            # Always create final user files with simple names
            final_yaml, final_report = create_final_user_files(
                user_yaml_file, science_yaml_file, science_report_file
            )

            if phase_b_success:
                print("[OK] Validation completed")
                print("Report:", final_report)
                print("Updated YAML:", final_yaml)
            else:
                # Phase B failed - remove YAML file if it was created during failure (COMMIT 3 requirement)
                try:
                    if os.path.exists(science_yaml_file):
                        os.remove(science_yaml_file)
                        # Also remove the final yaml since source was removed
                        if os.path.exists(final_yaml):
                            os.remove(final_yaml)
                except Exception:
                    pass  # Don't fail if removal doesn't work
                # Phase B standalone failure: only show Report and Suggestion (no Updated YAML)
                print("✗ Validation failed!")
                print("Report:", final_report)
            return 0 if phase_b_success else 1

        elif phase == "C":
            # Phase C only - run Pydantic validation on original user YAML
            print("Configuration consistency check...")

            phase_c_success = run_phase_c(
                user_yaml_file,
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phases_run=["C"],
                silent=True,  # Suppress phase function output, main function handles terminal output
            )

            # Always create final user files with simple names
            final_yaml, final_report = create_final_user_files(
                user_yaml_file, pydantic_yaml_file, pydantic_report_file
            )

            if phase_c_success:
                print("[OK] Validation completed")
                print("Report:", final_report)
                print("Updated YAML:", final_yaml)
            else:
                # Phase C failed - remove YAML file if it was created during failure (COMMIT 3 requirement)
                try:
                    if os.path.exists(pydantic_yaml_file):
                        os.remove(pydantic_yaml_file)
                        # Also remove the final yaml since source was removed
                        if os.path.exists(final_yaml):
                            os.remove(final_yaml)
                except Exception:
                    pass  # Don't fail if removal doesn't work
                # Phase C standalone failure: only show Report and Suggestion (no Updated YAML)
                print("✗ Validation failed!")
                print("Report:", final_report)
            return 0 if phase_c_success else 1

        elif phase == "AB":
            # Complete A→B workflow
            print("Configuration structure check...")
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
                # Phase A failed in AB workflow - create final user files from Phase A outputs
                final_yaml, final_report = create_final_user_files(
                    user_yaml_file, uptodate_file, report_file
                )

                print("✗ Validation failed!")
                print(f"Report: {final_report}")
                print(f"Updated YAML: {final_yaml}")
                return 1

            print("[OK] Validation completed")
            print("Physics validation check...")
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

            if not phase_b_success:
                # Phase B failed in AB workflow - create final user files from Phase B error report and Phase A YAML
                dirname = os.path.dirname(user_yaml_file)
                basename = os.path.basename(user_yaml_file)
                name_without_ext = os.path.splitext(basename)[0]
                final_yaml = os.path.join(dirname, f"updated_{basename}")
                final_report = os.path.join(dirname, f"report_{name_without_ext}.txt")

                try:
                    # Use Phase A YAML as final (last successful phase)
                    if os.path.exists(uptodate_file):
                        shutil.move(uptodate_file, final_yaml)

                    # Use Phase B report as final (contains the errors)
                    if os.path.exists(science_report_file):
                        shutil.move(science_report_file, final_report)

                    # Clean up intermediate Phase A report
                    if os.path.exists(report_file):
                        os.remove(report_file)

                    # Remove failed Phase B YAML if it exists (only if different from final_yaml)
                    if (
                        os.path.exists(science_yaml_file)
                        and science_yaml_file != final_yaml
                    ):
                        os.remove(science_yaml_file)
                except Exception:
                    pass

                print("✗ Validation failed!")
                print(f"Report: {final_report}")
                print(f"Updated YAML: {final_yaml}")
                return 1

            # Both A and B succeeded - consolidate reports and clean up intermediate files
            try:
                # Extract NO ACTION NEEDED messages from both phases
                all_messages = []
                if os.path.exists(report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(report_file)
                    )
                if os.path.exists(science_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(science_report_file)
                    )

                # Create consolidated final report
                create_consolidated_report(
                    phases_run=["A", "B"],
                    no_action_messages=all_messages,
                    final_report_file=science_report_file,
                    mode=internal_mode,
                )

                # Clean up intermediate files
                if os.path.exists(report_file):
                    os.remove(report_file)  # Remove Phase A report
                if os.path.exists(uptodate_file):
                    os.remove(uptodate_file)  # Remove Phase A YAML
            except Exception:
                pass  # Don't fail if cleanup doesn't work

            print("[OK] Validation completed")
            print("Report:", science_report_file)
            print("Updated YAML:", science_yaml_file)
            return 0

        elif phase == "AC":
            # Complete A→C workflow
            print("Configuration structure check...")
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

                print("✗ Validation failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                return 1

            print("[OK] Validation completed")
            print("Configuration consistency check...")
            phase_c_success = run_phase_c(
                uptodate_file,  # Use Phase A output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phase_a_report_file=report_file,  # Pass Phase A report for consolidation
                phases_run=["A", "C"],
                silent=True,
            )

            if not phase_c_success:
                # Phase C failed in AC workflow - create final user files from Phase C error report and Phase A YAML
                dirname = os.path.dirname(user_yaml_file)
                basename = os.path.basename(user_yaml_file)
                name_without_ext = os.path.splitext(basename)[0]
                final_yaml = os.path.join(dirname, f"updated_{basename}")
                final_report = os.path.join(dirname, f"report_{name_without_ext}.txt")

                try:
                    # Use Phase A YAML as final (last successful phase)
                    if os.path.exists(uptodate_file):
                        shutil.move(uptodate_file, final_yaml)

                    # Phase C report should already be at pydantic_report_file (final name)
                    # Clean up intermediate Phase A report
                    if os.path.exists(report_file):
                        os.remove(report_file)

                    # Remove failed Phase C YAML if it exists (only if different from final_yaml)
                    if (
                        os.path.exists(pydantic_yaml_file)
                        and pydantic_yaml_file != final_yaml
                    ):
                        os.remove(pydantic_yaml_file)
                except Exception:
                    pass

                print("✗ Validation failed!")
                print(f"Report: {final_report}")
                print(f"Updated YAML: {final_yaml}")
                return 1

            # Both A and C succeeded - consolidate reports and clean up intermediate files
            try:
                # Extract NO ACTION NEEDED messages from both phases
                all_messages = []
                if os.path.exists(report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(report_file)
                    )
                if os.path.exists(pydantic_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(pydantic_report_file)
                    )

                # Create consolidated final report
                create_consolidated_report(
                    phases_run=["A", "C"],
                    no_action_messages=all_messages,
                    final_report_file=pydantic_report_file,
                    mode=internal_mode,
                )

                # Clean up intermediate files
                if os.path.exists(report_file):
                    os.remove(report_file)  # Remove Phase A report
                if os.path.exists(uptodate_file):
                    os.remove(uptodate_file)  # Remove Phase A YAML
            except Exception:
                pass  # Don't fail if cleanup doesn't work

            print("[OK] Validation completed")
            print("Report:", pydantic_report_file)
            print("Updated YAML:", pydantic_yaml_file)
            return 0

        elif phase == "BC":
            # Complete B→C workflow
            print("Physics validation check...")
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

                print("✗ Validation failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                return 1

            print("[OK] Phase B completed")
            print("Configuration consistency check...")
            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                pydantic_report_file,
                mode=internal_mode,
                phase_a_report_file=science_report_file,  # Pass Phase B report for consolidation
                phases_run=["B", "C"],
                silent=True,
            )

            if not phase_c_success:
                # Phase C failed in BC workflow - consolidate Phase B messages into Phase C error report
                dirname = os.path.dirname(user_yaml_file)
                basename = os.path.basename(user_yaml_file)
                name_without_ext = os.path.splitext(basename)[0]
                final_yaml = os.path.join(dirname, f"updated_{basename}")
                final_report = os.path.join(dirname, f"report_{name_without_ext}.txt")

                try:
                    # Extract NO ACTION NEEDED messages from Phase B
                    phase_b_messages = []
                    if os.path.exists(science_report_file):
                        phase_b_messages = extract_no_action_messages_from_report(
                            science_report_file
                        )

                    # Read Phase C error report and append Phase B messages
                    if os.path.exists(pydantic_report_file):
                        with open(pydantic_report_file, "r") as f:
                            phase_c_content = f.read()

                        # Append Phase B NO ACTION NEEDED messages to Phase C report
                        if phase_b_messages:
                            # Remove the closing separator and any trailing separators from Phase C report
                            lines = phase_c_content.rstrip().split("\n")
                            while lines and lines[-1].strip() == f"# {'=' * 50}":
                                lines.pop()
                            phase_c_content = "\n".join(lines)

                            # Ensure proper spacing before NO ACTION NEEDED section
                            if not phase_c_content.endswith("\n\n"):
                                phase_c_content += "\n"

                            # Add NO ACTION NEEDED section
                            phase_c_content += "\n## NO ACTION NEEDED"

                            # Add Phase B messages
                            for msg in phase_b_messages:
                                phase_c_content += f"\n{msg}"

                            # Add closing separator
                            phase_c_content += f"\n\n# {'=' * 50}\n"

                            # Write consolidated report
                            with open(pydantic_report_file, "w") as f:
                                f.write(phase_c_content)

                    # Use Phase B YAML as final (last successful phase)
                    if os.path.exists(science_yaml_file):
                        shutil.move(science_yaml_file, final_yaml)

                    # Clean up intermediate Phase B report (now that we've extracted messages)
                    if os.path.exists(science_report_file):
                        os.remove(science_report_file)

                    # Remove failed Phase C YAML if it exists (only if different from final_yaml)
                    if (
                        os.path.exists(pydantic_yaml_file)
                        and pydantic_yaml_file != final_yaml
                    ):
                        os.remove(pydantic_yaml_file)
                except Exception:
                    pass

                print("✗ Validation failed!")
                print(f"Report: {final_report}")
                print(f"Updated YAML: {final_yaml}")
                return 1

            # Both B and C succeeded - consolidate reports and clean up intermediate files
            try:
                # Extract NO ACTION NEEDED messages from both phases
                all_messages = []
                if os.path.exists(science_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(science_report_file)
                    )
                if os.path.exists(pydantic_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(pydantic_report_file)
                    )

                # Create consolidated final report
                create_consolidated_report(
                    phases_run=["B", "C"],
                    no_action_messages=all_messages,
                    final_report_file=pydantic_report_file,
                    mode=internal_mode,
                )

                # Clean up intermediate files
                if os.path.exists(science_report_file):
                    os.remove(science_report_file)  # Remove Phase B report
                if os.path.exists(science_yaml_file):
                    os.remove(science_yaml_file)  # Remove Phase B YAML
            except Exception:
                pass

            print("[OK] Validation completed")
            print("Report:", pydantic_report_file)
            print("Updated YAML:", pydantic_yaml_file)
            return 0

        elif phase == "ABC":
            # Complete A→B→C workflow with proper halt logic
            # Step 1: Run Phase A
            print("DEBUG: Starting ABC workflow")
            print("Configuration structure check...")
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
                print("DEBUG: Phase A failed, moving files to final names")
                print(f"DEBUG: uptodate_file: {uptodate_file}")
                print(f"DEBUG: report_file: {report_file}")
                print(f"DEBUG: pydantic_yaml_file: {pydantic_yaml_file}")
                print(f"DEBUG: pydantic_report_file: {pydantic_report_file}")
                try:
                    if os.path.exists(report_file):
                        print(f"DEBUG: Moving {report_file} to {pydantic_report_file}")
                        shutil.move(
                            report_file, pydantic_report_file
                        )  # reportA → report
                    else:
                        print(f"DEBUG: {report_file} does not exist")

                    if os.path.exists(uptodate_file):
                        print(f"DEBUG: Moving {uptodate_file} to {pydantic_yaml_file}")
                        shutil.move(
                            uptodate_file, pydantic_yaml_file
                        )  # updatedA → updated (preserve Phase A updated YAML)
                    else:
                        print(f"DEBUG: {uptodate_file} does not exist")
                except Exception as e:
                    print(f"DEBUG: Exception during file move: {e}")

                print("✗ Validation failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                return 1

            print("[OK] Validation completed")

            # Step 2: Run Phase B (A passed, try B)
            print("Physics validation check...")
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
                # Phase B failed in ABC workflow - create final files from Phase A (last successful phase)
                # Keep Phase A files as intermediate, create final files from Phase A content
                try:
                    # Create final files from Phase A outputs (last successful phase)
                    if os.path.exists(report_file):
                        shutil.copy2(
                            report_file, pydantic_report_file
                        )  # Copy reportA → report (keep intermediate)

                    if os.path.exists(uptodate_file):
                        shutil.copy2(
                            uptodate_file, pydantic_yaml_file
                        )  # Copy updatedA → updated (keep intermediate)

                    # Remove failed Phase B files (failed phase output not needed)
                    if Path(science_yaml_file).exists():
                        Path(science_yaml_file).unlink()  # Remove failed Phase B YAML
                    if Path(science_report_file).exists():
                        Path(
                            science_report_file
                        ).unlink()  # Remove failed Phase B report
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print("✗ Validation failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                return 1

            print("[OK] Phase B completed")

            # Step 3: Run Phase C (both A and B passed)
            print("Configuration consistency check...")

            # Create temporary file for Phase C report (will be consolidated later)
            temp_report_c = os.path.join(
                dirname,
                f"temp_reportC_{os.path.splitext(os.path.basename(user_yaml_file))[0]}.txt",
            )

            phase_c_success = run_phase_c(
                science_yaml_file,  # Use Phase B output as input to Phase C
                pydantic_yaml_file,
                temp_report_c,  # Temporary Phase C report
                mode=internal_mode,
                phase_a_report_file=None,  # No consolidation in Phase C anymore
                phases_run=["C"],  # Only Phase C content
                silent=True,
            )

            if not phase_c_success:
                # Phase C failed in ABC workflow - create final files from Phase B (last successful phase)
                # Keep Phase A and B files as intermediate
                try:
                    # Create final files from Phase B outputs (last successful phase)
                    if os.path.exists(science_yaml_file):
                        shutil.copy2(
                            science_yaml_file, pydantic_yaml_file
                        )  # Copy updatedB → updated (keep intermediate)

                    if os.path.exists(science_report_file):
                        shutil.copy2(
                            science_report_file, pydantic_report_file
                        )  # Copy reportB → report (keep intermediate)
                except Exception:
                    pass  # Don't fail if cleanup doesn't work

                print("✗ Validation failed!")
                print(f"Report: {pydantic_report_file}")
                print(f"Updated YAML: {pydantic_yaml_file}")
                return 1

            # Phase C succeeded - consolidate reports from all phases
            try:
                # Collect NO ACTION NEEDED messages from all phase reports
                all_messages = []

                # Extract messages from Phase A report
                if phase_a_report_file and os.path.exists(phase_a_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(phase_a_report_file)
                    )

                # Extract messages from Phase B report
                if science_report_file and os.path.exists(science_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(science_report_file)
                    )

                # Extract messages from Phase C report
                if temp_report_c and os.path.exists(temp_report_c):
                    all_messages.extend(
                        extract_no_action_messages_from_report(temp_report_c)
                    )

                # Create consolidated final report
                create_consolidated_report(
                    phases_run=["A", "B", "C"] if phases_run is None else phases_run,
                    no_action_messages=all_messages,
                    final_report_file=pydantic_report_file,
                    mode=internal_mode,
                )

                # Clean up intermediate files
                if phase_a_report_file and Path(phase_a_report_file).exists():
                    Path(phase_a_report_file).unlink()  # Remove Phase A report
                if science_report_file and Path(science_report_file).exists():
                    Path(science_report_file).unlink()  # Remove Phase B report
                if temp_report_c and Path(temp_report_c).exists():
                    Path(temp_report_c).unlink()  # Remove Phase C temp report
            except Exception:
                pass  # Don't fail if cleanup doesn't work

            print("[OK] Validation completed")
            print("Report:", pydantic_report_file)
            print("Updated YAML:", pydantic_yaml_file)
            return 0

        else:
            # Fallback for unknown phase combinations
            print(f"✗ Unknown validation option '{phase}'")
            print("For help: suews-validate --help")
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
