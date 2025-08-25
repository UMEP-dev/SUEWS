"""
Integration module to connect the wizard with the YAML processor from PR #580.
This provides real-time validation using the three-phase validation system.
"""

from typing import Dict, Any, Tuple, List, Optional
from pathlib import Path
import tempfile
import yaml
import io
from contextlib import redirect_stdout, redirect_stderr

from ....data_model.validation.pipeline.orchestrator import (
    detect_pydantic_defaults,
    run_phase_abc_workflow,
)
from ....data_model.validation.pipeline.phase_a_parameter_update import (
    annotate_missing_parameters,
)
from ....data_model.validation.pipeline.phase_b_science_check import run_science_check
from ....data_model.validation.pipeline.phase_c_pydantic_report import (
    generate_phase_c_report,
)
from ....data_model.core.config import SUEWSConfig


class YAMLProcessorValidator:
    """
    Integrates the three-phase YAML processor validation into the wizard.

    This class provides methods to:
    - Run individual validation phases (A, B, C)
    - Run combined validation workflows
    - Extract meaningful error messages for the wizard UI
    - Suggest fixes for common issues
    """

    def __init__(self, mode: str = "public"):
        """
        Initialize the validator.

        Args:
            mode: Validation mode ("public" or "dev")
        """
        self.mode = mode

    def validate_phase_a(
        self, config_dict: Dict[str, Any]
    ) -> Tuple[bool, List[str], Dict[str, Any]]:
        """
        Run Phase A validation: Parameter detection and up-to-date check.

        Args:
            config_dict: Configuration dictionary from wizard

        Returns:
            Tuple of (is_valid, messages, updated_config)
        """
        messages = []
        updated_config = config_dict.copy()

        # Create temporary YAML file for processing
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml.dump(config_dict, f, default_flow_style=False)
            temp_yaml = f.name

        try:
            # Run Phase A with output capture
            output_yaml = temp_yaml.replace(".yml", "_phaseA.yml")
            report_file = temp_yaml.replace(".yml", "_phaseA_report.txt")

            # Capture stdout/stderr
            stdout_buffer = io.StringIO()
            stderr_buffer = io.StringIO()

            with redirect_stdout(stdout_buffer), redirect_stderr(stderr_buffer):
                result = annotate_missing_parameters(
                    temp_yaml, output_yaml, report_file, mode=self.mode
                )

            # Parse report for messages
            if Path(report_file).exists():
                with open(report_file, "r") as f:
                    report_content = f.read()

                # Extract critical and optional missing parameters
                if "critical missing parameter(s)" in report_content:
                    messages.append(
                        "⚠️ Critical parameters missing - these must be provided"
                    )

                if "optional missing parameter(s)" in report_content:
                    messages.append(
                        "ℹ️ Optional parameters missing - defaults will be used"
                    )

                if "renamed parameter(s)" in report_content:
                    messages.append(
                        "🔄 Some parameters have been renamed to match current version"
                    )

            # Load updated config
            if Path(output_yaml).exists():
                with open(output_yaml, "r") as f:
                    updated_config = yaml.safe_load(f)

            # Clean up temp files
            Path(temp_yaml).unlink(missing_ok=True)
            Path(output_yaml).unlink(missing_ok=True)
            Path(report_file).unlink(missing_ok=True)

            # Phase A is typically informational, not a hard failure
            return True, messages, updated_config

        except Exception as e:
            messages.append(f"Phase A validation error: {str(e)}")
            return False, messages, config_dict
        finally:
            # Ensure temp file is cleaned up
            Path(temp_yaml).unlink(missing_ok=True)

    def validate_phase_b(
        self, config_dict: Dict[str, Any]
    ) -> Tuple[bool, List[str], Dict[str, Any]]:
        """
        Run Phase B validation: Scientific validation.

        Args:
            config_dict: Configuration dictionary from wizard

        Returns:
            Tuple of (is_valid, messages, updated_config)
        """
        messages = []
        updated_config = config_dict.copy()

        # Create temporary YAML file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml.dump(config_dict, f, default_flow_style=False)
            temp_yaml = f.name

        try:
            output_yaml = temp_yaml.replace(".yml", "_phaseB.yml")
            report_file = temp_yaml.replace(".yml", "_phaseB_report.txt")

            # Run Phase B
            stdout_buffer = io.StringIO()
            stderr_buffer = io.StringIO()

            with redirect_stdout(stdout_buffer), redirect_stderr(stderr_buffer):
                result = run_science_check(
                    temp_yaml, output_yaml, report_file, mode=self.mode
                )

            # Check for critical errors
            has_critical_errors = False

            if Path(report_file).exists():
                with open(report_file, "r") as f:
                    report_content = f.read()

                if "CRITICAL ERROR" in report_content:
                    has_critical_errors = True
                    messages.append("❌ Critical scientific validation errors found")

                    # Extract specific errors
                    for line in report_content.split("\n"):
                        if "CRITICAL ERROR" in line:
                            # Extract the error message
                            error_msg = line.split("CRITICAL ERROR:")[-1].strip()
                            if error_msg:
                                messages.append(f"  • {error_msg}")

                if "scientific warning(s) found" in report_content:
                    messages.append("⚠️ Scientific warnings found - review recommended")

            # Load updated config
            if Path(output_yaml).exists():
                with open(output_yaml, "r") as f:
                    updated_config = yaml.safe_load(f)

            # Clean up
            Path(temp_yaml).unlink(missing_ok=True)
            Path(output_yaml).unlink(missing_ok=True)
            Path(report_file).unlink(missing_ok=True)

            return not has_critical_errors, messages, updated_config

        except Exception as e:
            messages.append(f"Phase B validation error: {str(e)}")
            return False, messages, config_dict
        finally:
            Path(temp_yaml).unlink(missing_ok=True)

    def validate_phase_c(self, config_dict: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """
        Run Phase C validation: Pydantic conditional validation.

        Args:
            config_dict: Configuration dictionary from wizard

        Returns:
            Tuple of (is_valid, messages)
        """
        messages = []

        try:
            # Direct Pydantic validation
            config = SUEWSConfig(**config_dict)
            messages.append("✅ Configuration passes Pydantic validation")
            return True, messages

        except Exception as e:
            # Parse Pydantic errors
            if hasattr(e, "errors"):
                messages.append("❌ Pydantic validation errors:")
                for err in e.errors():
                    loc = " → ".join(str(x) for x in err["loc"])
                    msg = err["msg"]
                    messages.append(f"  • {loc}: {msg}")
            else:
                messages.append(f"❌ Validation error: {str(e)}")

            return False, messages

    def validate_all_phases(
        self, config_dict: Dict[str, Any]
    ) -> Tuple[bool, List[str], Dict[str, Any]]:
        """
        Run all three validation phases in sequence.

        Args:
            config_dict: Configuration dictionary from wizard

        Returns:
            Tuple of (is_valid, all_messages, final_config)
        """
        all_messages = []
        current_config = config_dict

        # Phase A
        all_messages.append("📋 Phase A: Parameter Detection")
        valid_a, messages_a, updated_config_a = self.validate_phase_a(current_config)
        all_messages.extend(messages_a)
        current_config = updated_config_a

        if not valid_a:
            return False, all_messages, current_config

        # Phase B
        all_messages.append("\n📊 Phase B: Scientific Validation")
        valid_b, messages_b, updated_config_b = self.validate_phase_b(current_config)
        all_messages.extend(messages_b)
        current_config = updated_config_b

        if not valid_b:
            return False, all_messages, current_config

        # Phase C
        all_messages.append("\n🔍 Phase C: Pydantic Validation")
        valid_c, messages_c = self.validate_phase_c(current_config)
        all_messages.extend(messages_c)

        return valid_c, all_messages, current_config

    def get_quick_validation(
        self, field_name: str, value: Any, context: Dict[str, Any] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Quick validation for individual fields during wizard input.

        Args:
            field_name: Name of the field being validated
            value: Value to validate
            context: Current configuration context

        Returns:
            Tuple of (is_valid, error_message)
        """
        # Map common field names to validation rules
        field_rules = {
            "latitude": {"min": -90, "max": 90, "type": float},
            "longitude": {"min": -180, "max": 180, "type": float},
            "timezone": {"min": -12, "max": 14, "type": (int, float)},
            "timestep": {"min": 1, "max": 3600, "type": int},
            "fr_paved": {"min": 0, "max": 1, "type": float},
            "fr_bldg": {"min": 0, "max": 1, "type": float},
            "fr_grass": {"min": 0, "max": 1, "type": float},
            "fr_deciduous": {"min": 0, "max": 1, "type": float},
            "fr_evergreen": {"min": 0, "max": 1, "type": float},
            "fr_baresoil": {"min": 0, "max": 1, "type": float},
            "fr_water": {"min": 0, "max": 1, "type": float},
        }

        if field_name in field_rules:
            rules = field_rules[field_name]

            # Type check
            if "type" in rules:
                expected_type = rules["type"]
                if not isinstance(value, expected_type):
                    try:
                        # Try to convert
                        if expected_type == float:
                            value = float(value)
                        elif expected_type == int:
                            value = int(value)
                    except (ValueError, TypeError):
                        return False, f"Must be {expected_type.__name__}"

            # Range check
            if "min" in rules and value < rules["min"]:
                return False, f"Must be >= {rules['min']}"
            if "max" in rules and value > rules["max"]:
                return False, f"Must be <= {rules['max']}"

        # Check surface fractions sum to 1 if this is a fraction field
        if field_name.startswith("fr_") and context:
            # Get all fraction fields from context
            total = 0
            for key, val in context.items():
                if key.startswith("fr_") and key != field_name:
                    total += float(val)
            total += float(value)

            if abs(total - 1.0) > 0.01:  # Allow small tolerance
                return (
                    False,
                    f"Surface fractions must sum to 1.0 (current: {total:.2f})",
                )

        return True, None

    def suggest_fixes(self, error_messages: List[str]) -> List[str]:
        """
        Suggest fixes for common validation errors.

        Args:
            error_messages: List of error messages from validation

        Returns:
            List of suggested fixes
        """
        suggestions = []

        for error in error_messages:
            if "surface fractions" in error.lower():
                suggestions.append(
                    "💡 Ensure all surface fractions (paved, buildings, vegetation, etc.) sum to exactly 1.0"
                )
            elif "missing parameter" in error.lower():
                suggestions.append(
                    "💡 Use the wizard to fill in all required parameters"
                )
            elif "renamed parameter" in error.lower():
                suggestions.append(
                    "💡 Parameter names have been automatically updated to match the current version"
                )
            elif "physics option" in error.lower():
                suggestions.append(
                    "💡 Check that selected physics options are compatible with each other"
                )
            elif "timezone" in error.lower():
                suggestions.append(
                    "💡 Timezone should be an offset from UTC (e.g., -6 for CST, 0 for GMT)"
                )

        return suggestions
