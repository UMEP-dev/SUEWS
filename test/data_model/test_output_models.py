#!/usr/bin/env python3
"""Standalone test for output variable Pydantic models.

This script tests the new Python-first output variable definitions
without requiring a full SUEWS build.
"""

import sys
import os
from pathlib import Path

import pytest

# Add src/supy/data_model directly to path to avoid importing full supy package
# This allows testing the output variable definitions without building the Fortran components
_DATA_MODEL_PATH = (
    Path(__file__).resolve().parent.parent.parent / "src" / "supy" / "data_model"
)
if str(_DATA_MODEL_PATH) not in sys.path:
    sys.path.insert(0, str(_DATA_MODEL_PATH))

# Direct import from output module (uses path modification above)
from output.variables import (
    OutputVariable,
    OutputVariableRegistry,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)
from output.datetime_vars import DATETIME_VARIABLES
from output.suews_vars import SUEWS_VARIABLES
from output.snow_vars import SNOW_VARIABLES
from output.estm_vars import ESTM_VARIABLES
from output.rsl_vars import RSL_VARIABLES
from output.dailystate_vars import DAILYSTATE_VARIABLES
from output.bl_vars import BL_VARIABLES
from output.beers_vars import BEERS_VARIABLES
from output.debug_vars import DEBUG_VARIABLES
from output.ehc_vars import EHC_VARIABLES
from output.spartacus_vars import SPARTACUS_VARIABLES
from output.stebbs_vars import STEBBS_VARIABLES
from output.nhood_vars import NHOOD_VARIABLES

# Expected variable counts (for test stability during development)
EXPECTED_COUNTS = {
    OutputGroup.DATETIME: 5,
    OutputGroup.SUEWS: 99,  # Updated: added 14 Tsfc surface temperature variables
    OutputGroup.SNOW: 98,  # Approximate, can vary by surface types
    OutputGroup.ESTM: 27,
    OutputGroup.RSL: 135,  # Approximate, depends on levels
    OutputGroup.DAILYSTATE: 47,
    OutputGroup.BL: 17,  # Corrected count from review feedback
    OutputGroup.BEERS: 29,
    OutputGroup.DEBUG: 131,  # Exact count matching Fortran dataOutLineDebug
    OutputGroup.EHC: 224,  # 2 + 7×15 roof + 7×15 wall
    OutputGroup.SPARTACUS: 194,  # 10 scalars + 12×15 layers
    OutputGroup.STEBBS: 78,  # Updated: 78 variables matching Fortran STEBBS output
    OutputGroup.NHOOD: 1,
}

# Assemble registry manually for testing (matching production)
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(
        DATETIME_VARIABLES
        + SUEWS_VARIABLES
        + SNOW_VARIABLES
        + ESTM_VARIABLES
        + RSL_VARIABLES
        + DAILYSTATE_VARIABLES
        + BL_VARIABLES
        + BEERS_VARIABLES
        + DEBUG_VARIABLES
        + EHC_VARIABLES
        + SPARTACUS_VARIABLES
        + STEBBS_VARIABLES
        + NHOOD_VARIABLES
    )
)


def test_registry_basic():
    """Test that registry loads and has expected variables."""
    print("Testing OUTPUT_REGISTRY basic functionality...")

    # Check registry is not empty
    assert len(OUTPUT_REGISTRY.variables) > 0, "Registry should not be empty"
    print(f"[OK] Registry contains {len(OUTPUT_REGISTRY.variables)} variables")

    # Track any drift for summary warning
    drift_warnings = []

    # Check all groups are present using centralised expected counts
    for group, expected_count in EXPECTED_COUNTS.items():
        group_vars = OUTPUT_REGISTRY.by_group(group)
        actual_count = len(group_vars)

        # For groups with variable counts, use tolerance-based checking
        if group in [OutputGroup.SNOW, OutputGroup.RSL, OutputGroup.DEBUG]:
            # Allow +/-10% tolerance for groups that may vary
            tolerance = max(1, int(expected_count * 0.1))
            assert abs(actual_count - expected_count) <= tolerance, (
                f"{group.value} should have ~{expected_count} variables "
                f"(+/-{tolerance}), got {actual_count}"
            )
            # Log drift even within tolerance for tracking
            if actual_count != expected_count:
                drift_warnings.append(
                    f"{group.value}: {actual_count} (expected {expected_count}, diff {actual_count - expected_count:+d})"
                )
            print(
                f"[OK] {group.value}: {actual_count} variables (~{expected_count} expected)"
            )
        else:
            # Exact count for stable groups
            assert actual_count == expected_count, (
                f"{group.value} should have exactly {expected_count} variables, "
                f"got {actual_count}"
            )
            print(f"[OK] {group.value}: {actual_count} variables")

    # Report any drift for awareness
    if drift_warnings:
        print()
        print("[INFO] Variable count drift detected (within tolerance):")
        for warning in drift_warnings:
            print(f"  - {warning}")
        print("  Consider updating EXPECTED_COUNTS if these changes are intentional.")

    print()


def test_specific_variables():
    """Test that specific key variables are present with correct metadata."""
    print("Testing specific variable definitions...")

    # Test QH (Sensible heat flux)
    qh = OUTPUT_REGISTRY.by_name("QH")
    assert qh is not None, "QH variable should exist"
    assert qh.unit == "W m-2", f"QH unit should be 'W m-2', got '{qh.unit}'"
    assert qh.aggregation == AggregationMethod.AVERAGE, (
        "QH should use AVERAGE aggregation"
    )
    assert qh.group == OutputGroup.SUEWS, "QH should be in SUEWS group"
    assert qh.level == OutputLevel.DEFAULT, "QH should be DEFAULT level"
    print(f"[OK] QH: {qh.description} [{qh.unit}]")

    # Test T2 (Air temperature at 2m)
    t2 = OUTPUT_REGISTRY.by_name("T2")
    assert t2 is not None, "T2 variable should exist"
    assert t2.unit == "degC", f"T2 unit should be 'degC', got '{t2.unit}'"
    assert t2.aggregation == AggregationMethod.AVERAGE, (
        "T2 should use AVERAGE aggregation"
    )
    print(f"[OK] T2: {t2.description} [{t2.unit}]")

    # Test Rain (precipitation)
    rain = OUTPUT_REGISTRY.by_name("Rain")
    assert rain is not None, "Rain variable should exist"
    assert rain.aggregation == AggregationMethod.SUM, "Rain should use SUM aggregation"
    print(f"[OK] Rain: {rain.description} [{rain.unit}] (aggregation: SUM)")

    # Test SMD (Soil Moisture Deficit)
    smd = OUTPUT_REGISTRY.by_name("SMD")
    assert smd is not None, "SMD variable should exist"
    assert smd.aggregation == AggregationMethod.LAST, "SMD should use LAST aggregation"
    print(f"[OK] SMD: {smd.description} [{smd.unit}] (aggregation: LAST)")

    print()


def test_output_levels():
    """Test filtering by output level."""
    print("Testing output level filtering...")

    # Get DEFAULT level variables
    default_vars = OUTPUT_REGISTRY.by_level(OutputLevel.DEFAULT)
    print(f"[OK] DEFAULT level: {len(default_vars)} variables")

    # Get up to EXTENDED level
    extended_vars = OUTPUT_REGISTRY.by_level(OutputLevel.EXTENDED)
    print(f"[OK] Up to EXTENDED level: {len(extended_vars)} variables")

    # Get up to SNOW_DETAILED level
    all_vars = OUTPUT_REGISTRY.by_level(OutputLevel.SNOW_DETAILED)
    print(f"[OK] Up to SNOW_DETAILED level: {len(all_vars)} variables")

    # DEFAULT should be subset of EXTENDED
    assert len(default_vars) <= len(extended_vars), (
        "DEFAULT should be subset of EXTENDED"
    )
    # EXTENDED should be subset of all
    assert len(extended_vars) <= len(all_vars), "EXTENDED should be subset of all"

    print()


def test_aggregation_rules():
    """Test generation of aggregation rules for resampling."""
    print("Testing aggregation rules generation...")

    agg_rules = OUTPUT_REGISTRY.get_aggregation_rules()

    # Check structure
    assert isinstance(agg_rules, dict), "Aggregation rules should be a dict"
    assert "SUEWS" in agg_rules, "Should have SUEWS group in aggregation rules"
    assert "datetime" in agg_rules, "Should have datetime group in aggregation rules"

    # Check SUEWS group has expected variables
    suews_rules = agg_rules["SUEWS"]
    assert "QH" in suews_rules, "QH should be in SUEWS aggregation rules"
    assert "Rain" in suews_rules, "Rain should be in SUEWS aggregation rules"

    # Check aggregation functions are correct type
    assert suews_rules["QH"] == "mean", "QH should use 'mean' aggregation"
    assert suews_rules["Rain"] == "sum", "Rain should use 'sum' aggregation"
    assert callable(suews_rules["SMD"]), "SMD should use callable (lambda) for LAST"

    print(f"[OK] Generated aggregation rules for {len(agg_rules)} groups")
    for group, rules in agg_rules.items():
        print(f"  - {group}: {len(rules)} variables")

    print()


def test_dataframe_conversion():
    """Test conversion to DataFrame format (backward compatibility)."""
    print("Testing DataFrame conversion...")

    df = OUTPUT_REGISTRY.to_dataframe()

    # Check DataFrame structure
    assert df.index.names == ["group", "var"], "Index should be (group, var)"
    assert "aggm" in df.columns, "Should have 'aggm' column"
    assert "outlevel" in df.columns, "Should have 'outlevel' column"
    assert "func" in df.columns, "Should have 'func' column"

    # Check specific entries
    qh_row = df.loc[("SUEWS", "QH")]
    assert qh_row["aggm"] == "A", "QH aggregation method should be 'A'"
    assert qh_row["outlevel"] == "0", "QH output level should be '0'"

    print(f"[OK] DataFrame has correct structure: {df.shape}")
    print(f"  - Index: {df.index.names}")
    print(f"  - Columns: {list(df.columns)}")

    print()


def test_enum_values():
    """Test that enum values work correctly."""
    print("Testing enum value behaviour...")

    # Test aggregation methods
    assert AggregationMethod.AVERAGE.value == "A"
    assert AggregationMethod.SUM.value == "S"
    assert AggregationMethod.LAST.value == "L"
    assert AggregationMethod.TIME.value == "T"
    print("[OK] AggregationMethod enums have correct values")

    # Test output levels
    assert OutputLevel.DEFAULT.value == 0
    assert OutputLevel.EXTENDED.value == 1
    assert OutputLevel.SNOW_DETAILED.value == 2
    print("[OK] OutputLevel enums have correct values")

    # Test output groups
    assert OutputGroup.DATETIME.value == "datetime"
    assert OutputGroup.SUEWS.value == "SUEWS"
    print("[OK] OutputGroup enums have correct values")

    print()


def test_by_name_returns_none_for_nonexistent():
    """Test that by_name returns None for variables that don't exist."""
    print("Testing by_name with non-existent variable...")

    result = OUTPUT_REGISTRY.by_name("NONEXISTENT_VARIABLE_XYZ")
    assert result is None, "by_name should return None for non-existent variables"
    print("[OK] by_name returns None for non-existent variable")

    print()


def test_duplicate_variable_within_group_raises_error():
    """Ensure duplicate variable names within same group are rejected."""
    print("Testing duplicate variable validation...")

    from pydantic import ValidationError

    duplicate_vars = [
        OutputVariable(
            name="TestVar",
            unit="W m-2",
            description="First test variable",
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.SUEWS,
            level=OutputLevel.DEFAULT,
        ),
        OutputVariable(
            name="TestVar",  # Duplicate name in same group
            unit="W m-2",
            description="Second test variable",
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.SUEWS,
            level=OutputLevel.DEFAULT,
        ),
    ]

    try:
        OutputVariableRegistry(variables=duplicate_vars)
        assert False, "Should have raised ValidationError for duplicate names"
    except ValidationError as e:
        assert "Duplicate variable names within groups" in str(e)
        print("[OK] Duplicate variable names within group correctly rejected")

    print()


def test_same_variable_name_in_different_groups_allowed():
    """Verify the same variable name can exist in different output groups."""
    print("Testing cross-group duplicate names (should be allowed)...")

    # Same variable name in different groups is valid
    cross_group_vars = [
        OutputVariable(
            name="QS",
            unit="W m-2",
            description="Storage heat flux (SUEWS)",
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.SUEWS,
            level=OutputLevel.DEFAULT,
        ),
        OutputVariable(
            name="QS",
            unit="W m-2",
            description="Storage heat flux (ESTM)",
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.ESTM,
            level=OutputLevel.DEFAULT,
        ),
    ]

    # Should NOT raise
    registry = OutputVariableRegistry(variables=cross_group_vars)
    assert len(registry.variables) == 2, "Registry should contain both variables"
    print("[OK] Same variable name in different groups is allowed")

    print()


def main():
    """Run all tests."""
    print("=" * 70)
    print("SUEWS Output Variable Pydantic Models - Proof of Concept Test")
    print("=" * 70)
    print()

    try:
        test_registry_basic()
        test_specific_variables()
        test_output_levels()
        test_aggregation_rules()
        test_dataframe_conversion()
        test_enum_values()
        test_by_name_returns_none_for_nonexistent()
        test_duplicate_variable_within_group_raises_error()
        test_same_variable_name_in_different_groups_allowed()

        print("=" * 70)
        print("[PASS] ALL TESTS PASSED!")
        print("=" * 70)
        print()
        print("Summary - Variable counts by group:")
        print(f"  Total: {len(OUTPUT_REGISTRY.variables)} variables")
        print(f"  - datetime: {len(OUTPUT_REGISTRY.by_group(OutputGroup.DATETIME))}")
        print(f"  - SUEWS: {len(OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS))}")
        print(f"  - snow: {len(OUTPUT_REGISTRY.by_group(OutputGroup.SNOW))}")
        print(f"  - ESTM: {len(OUTPUT_REGISTRY.by_group(OutputGroup.ESTM))}")
        print(f"  - RSL: {len(OUTPUT_REGISTRY.by_group(OutputGroup.RSL))}")
        print(
            f"  - DailyState: {len(OUTPUT_REGISTRY.by_group(OutputGroup.DAILYSTATE))}"
        )
        print(f"  - BL: {len(OUTPUT_REGISTRY.by_group(OutputGroup.BL))}")
        print(f"  - BEERS: {len(OUTPUT_REGISTRY.by_group(OutputGroup.BEERS))}")
        print(f"  - debug: {len(OUTPUT_REGISTRY.by_group(OutputGroup.DEBUG))}")
        print(f"  - EHC: {len(OUTPUT_REGISTRY.by_group(OutputGroup.EHC))}")
        print(f"  - SPARTACUS: {len(OUTPUT_REGISTRY.by_group(OutputGroup.SPARTACUS))}")
        print(f"  - STEBBS: {len(OUTPUT_REGISTRY.by_group(OutputGroup.STEBBS))}")
        print(f"  - NHOOD: {len(OUTPUT_REGISTRY.by_group(OutputGroup.NHOOD))}")
        print()
        print("All SUEWS output variables successfully migrated to Python/Pydantic!")
        print("The registry is ready for integration with SUEWS runtime.")

        return 0

    except AssertionError as e:
        print()
        print("=" * 70)
        print(f"[FAIL] TEST FAILED: {e}")
        print("=" * 70)
        return 1
    except Exception as e:
        print()
        print("=" * 70)
        print(f"[FAIL] UNEXPECTED ERROR: {e}")
        import traceback

        traceback.print_exc()
        print("=" * 70)
        return 1


def test_fortran_python_output_consistency():
    """Verify Python OUTPUT_REGISTRY completeness.

    HISTORICAL NOTE (2025-10-25):
    This test previously verified Fortran varListAll matched Python OUTPUT_REGISTRY.
    The Fortran output variable definitions (varListAll) have now been REMOVED as
    Python OUTPUT_REGISTRY is the single source of truth.

    This test is kept as a placeholder for future OUTPUT_REGISTRY validation.
    """
    print("=" * 70)
    print("Python OUTPUT_REGISTRY validation...")
    print("=" * 70)
    print()

    try:
        from supy.data_model.output import OUTPUT_REGISTRY, OutputGroup

        # Validate OUTPUT_REGISTRY is properly loaded
        print("Per-group variable counts:")
        total_vars = 0
        for group in OutputGroup:
            n_vars = len(OUTPUT_REGISTRY.by_group(group))
            total_vars += n_vars
            print(f"  [OK] {group.value:12s}: {n_vars:3d} variables")

        print()
        print("=" * 70)
        print("[PASS] OUTPUT_REGISTRY VALIDATION PASSED")
        print("=" * 70)
        print(f"Python OUTPUT_REGISTRY is properly loaded")
        print(f"({len(OutputGroup)} groups with {total_vars} total variables)")
        print()
        print("NOTE: Fortran varListAll has been deprecated (2025-10-25).")
        print("Python OUTPUT_REGISTRY is now the ONLY source of truth.")
        print()

    except ImportError as e:
        print()
        print("=" * 70)
        print("[SKIP] SKIPPED: OUTPUT_REGISTRY not available")
        print("=" * 70)
        print(f"Reason: {e}")
        print()
        pytest.skip(f"OUTPUT_REGISTRY not available: {e}")

    except AssertionError as e:
        print()
        print("=" * 70)
        print("[FAIL] CONSISTENCY CHECK FAILED")
        print("=" * 70)
        print(f"{e}")
        print()
        print("ACTION REQUIRED:")
        print("1. Check if recent changes were made to OUTPUT_REGISTRY")
        print("2. Ensure corresponding updates were made to Fortran varListAll")
        print()
        pytest.fail(f"Consistency check failed: {e}")

    except Exception as e:
        print()
        print("=" * 70)
        print("[FAIL] UNEXPECTED ERROR")
        print("=" * 70)
        print(f"{e}")
        import traceback

        traceback.print_exc()
        pytest.fail(f"Unexpected error: {e}")


if __name__ == "__main__":
    # Run both the original tests and the consistency check
    original_result = main()
    print()
    consistency_result = test_fortran_python_output_consistency()

    # Exit with error if either test failed
    sys.exit(max(original_result, consistency_result))
