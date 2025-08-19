"""
SUEWS Data Model Validation Module

This module consolidates all validation logic for SUEWS configurations.
"""

# Controller and results
from .controller import (
    ValidationController,
    ValidationResult,
    validate_suews_config_conditional,
)

# Utilities
from .utils import (
    check_missing_params,
    warn_missing_params,
    validate_only_when_complete,
)

# Feedback
from .feedback import (
    ValidatedConfig,
    emit_validation_feedback,
)

# YAML helpers
from .yaml_helpers import (
    run_precheck,
)

__all__ = [
    # Controller
    "ValidationController",
    "ValidationResult",
    "validate_suews_config_conditional",
    # Utils
    "check_missing_params",
    "warn_missing_params",
    "validate_only_when_complete",
    # Feedback
    "ValidatedConfig",
    "emit_validation_feedback",
    # YAML helpers
    "run_precheck",
]
