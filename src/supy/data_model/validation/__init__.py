"""
SUEWS Data Model Validation Module

This module provides comprehensive validation for SUEWS configurations,
including both core validation infrastructure and the three-phase validation pipeline.

Structure:
- core/: Core validation infrastructure (controller, utilities, feedback)
- pipeline/: Three-phase validation pipeline (Phase A, B, C and orchestrator)
"""

# Core validation exports
from .core.controller import (
    ValidationController,
    ValidationResult,
    validate_suews_config_conditional,
)
from .core.utils import (
    check_missing_params,
    warn_missing_params,
    validate_only_when_complete,
)
from .core.feedback import (
    ValidatedConfig,
    emit_validation_feedback,
)
from .core.yaml_helpers import (
    run_precheck,
)

# Pipeline exports (if needed by external modules)
from .pipeline.orchestrator import (
    validate_input_file,
    setup_output_paths,
    run_phase_a,
    run_phase_b,
    run_phase_c,
)

__all__ = [
    # Core validation
    "ValidationController",
    "ValidationResult",
    "validate_suews_config_conditional",
    "check_missing_params",
    "warn_missing_params",
    "validate_only_when_complete",
    "ValidatedConfig",
    "emit_validation_feedback",
    "run_precheck",
    # Pipeline functions
    "validate_input_file",
    "setup_output_paths",
    "run_phase_a",
    "run_phase_b",
    "run_phase_c",
]
