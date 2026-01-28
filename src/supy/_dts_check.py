"""Shared DTS availability check utility.

This module provides a single source of truth for checking whether DTS
(Derived Type Structure) features are available in the current build.

DTS features require a full build with type wrappers (wrap_dts_types=true).
Fast builds (make dev) do not include DTS support.
"""

from . import supy_driver as _supy_driver

# Check if DTS type classes exist (only present in full build with wrap_dts_types=true)
# The module_type_heat module exists in both builds, but the HEATSTATE class
# is only generated when DTS type wrappers are enabled
DTS_AVAILABLE = (
    hasattr(_supy_driver, "module_type_heat")
    and hasattr(_supy_driver.module_type_heat, "HEATSTATE")
)

DTS_ERROR_MSG = (
    "DTS features not available in this build.\n"
    "This build was compiled with 'make dev' (fast build without DTS support).\n"
    "To use DTS features, rebuild with: make clean && make dev-dts"
)


def check_dts_available():
    """Raise RuntimeError if DTS features are not available."""
    if not DTS_AVAILABLE:
        raise RuntimeError(DTS_ERROR_MSG)
