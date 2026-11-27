"""SUEWS Data Model Validation Module.

Provides the unified :class:`ValidationEngine` / :func:`validate_config` API,
shared validation rules, core utilities, and the three-phase validation
pipeline (Phase A, B, C, and orchestrator).
"""

# Re-export package-level utilities for use by submodules
# This avoids deep relative imports like '...._env' in pipeline modules
from ..._env import logger_supy

# Core validation utilities
from .core.utils import (
    check_missing_params,
    validate_only_when_complete,
    warn_missing_params,
)

# Public validation engine & report schema
from .engine import (
    ValidationEngine,
    check_public_mode_restrictions,
    validate_config,
)

# Pipeline facades (retained for backward compatibility)
from .pipeline.orchestrator import (
    run_phase_a,
    run_phase_b,
    run_phase_c,
    setup_output_paths,
    validate_input_file,
)
from .pipeline.report_schema import (
    Issue,
    PhaseReport,
    ValidationReport,
)

__all__ = [
    "Issue",
    "PhaseReport",
    "ValidationEngine",
    "ValidationReport",
    "check_missing_params",
    "check_public_mode_restrictions",
    "logger_supy",
    "run_phase_a",
    "run_phase_b",
    "run_phase_c",
    "setup_output_paths",
    "validate_config",
    "validate_input_file",
    "validate_only_when_complete",
    "warn_missing_params",
]
