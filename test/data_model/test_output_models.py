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

# Expected variable counts derived from Fortran ncolumnsDataOut* constants.
# Used for unit testing without Fortran build. The actual runtime verification
# against Fortran is done in test_fortran_python_output_consistency().
# Formula: ncolumnsDataOut* - 5 (subtract datetime columns)
EXPECTED_COUNTS = {
    OutputGroup.DATETIME: 5,  # Year, DOY, Hour, Min, Dectime
    OutputGroup.SUEWS: 99,  # Core SUEWS variables including Ts_* surface temps
    OutputGroup.SNOW: 98,  # Snow variables (7 surface types × 14 vars)
    OutputGroup.ESTM: 27,  # ESTM variables
    OutputGroup.RSL: 135,  # RSL profile variables (30 levels × 4 vars + 15)
    OutputGroup.DAILYSTATE: 47,  # Daily state variables
    OutputGroup.BL: 17,  # Boundary layer variables
    OutputGroup.BEERS: 29,  # BEERS radiation variables
    OutputGroup.DEBUG: 131,  # Debug variables (matches Fortran dataOutLineDebug)
    OutputGroup.EHC: 224,  # EHC variables (2 + 7×15 roof + 7×15 wall)
    OutputGroup.SPARTACUS: 194,  # SPARTACUS variables (10 scalars + 12×15 layers)
    OutputGroup.STEBBS: 80,  # STEBBS variables (matches Fortran truncated names)
    OutputGroup.NHOOD: 1,  # Neighbourhood variables
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
    """Test that registry loads and has expected variable counts.

    This unit test verifies Python registry against hardcoded EXPECTED_COUNTS
    (derived from Fortran ncolumnsDataOut* constants). It can run without
    building Fortran, providing quick feedback during development.

    For actual runtime verification against Fortran, see
    test_fortran_python_output_consistency() which calls Fortran directly.
    """
    print("Testing OUTPUT_REGISTRY basic functionality...")

    # Check registry is not empty
    assert len(OUTPUT_REGISTRY.variables) > 0, "Registry should not be empty"
    print(f"[OK] Registry contains {len(OUTPUT_REGISTRY.variables)} variables")

    # Check all groups match Fortran output EXACTLY
    for group, expected_count in EXPECTED_COUNTS.items():
        group_vars = OUTPUT_REGISTRY.by_group(group)
        actual_count = len(group_vars)

        # ALL groups must match Fortran output exactly - no tolerance allowed
        assert actual_count == expected_count, (
            f"{group.value} should have exactly {expected_count} variables "
            f"(matching Fortran output), got {actual_count}"
        )
        print(f"[OK] {group.value}: {actual_count} variables")

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
    """Verify Python OUTPUT_REGISTRY matches Fortran ncolumnsDataOut* constants.

    This test calls Fortran's output_ncolumns() function at runtime to get the
    expected number of columns for each output group, then verifies Python's
    OUTPUT_REGISTRY has the exact same count.

    This provides runtime verification that Python and Fortran stay in sync.
    If this test fails, either:
    - Fortran ncolumnsDataOut* constants were changed (update Python registry)
    - Python registry was changed (update Fortran constants)
    """
    print("=" * 70)
    print("Fortran/Python Output Consistency Verification...")
    print("=" * 70)
    print()

    try:
        from supy.data_model.output import OUTPUT_REGISTRY, OutputGroup
        from supy.supy_driver import suews_driver as sd

        # Mapping from Python OutputGroup to Fortran group name strings
        # These must match the CASE statements in output_ncolumns()
        GROUP_MAPPING = {
            OutputGroup.DATETIME: "datetime",
            OutputGroup.SUEWS: "SUEWS",
            OutputGroup.SNOW: "snow",
            OutputGroup.ESTM: "ESTM",
            OutputGroup.EHC: "EHC",
            OutputGroup.RSL: "RSL",
            OutputGroup.BL: "BL",
            OutputGroup.DEBUG: "debug",
            OutputGroup.BEERS: "BEERS",
            OutputGroup.DAILYSTATE: "DailyState",
            OutputGroup.SPARTACUS: "SPARTACUS",
            OutputGroup.STEBBS: "STEBBS",
            OutputGroup.NHOOD: "NHood",
        }

        print("Verifying Python registry against Fortran ncolumnsDataOut* constants:")
        mismatches = []
        for py_group, fortran_name in GROUP_MAPPING.items():
            python_count = len(OUTPUT_REGISTRY.by_group(py_group))
            fortran_count = sd.output_ncolumns(fortran_name)

            if python_count == fortran_count:
                print(
                    f"  [OK] {py_group.value:12s}: Python={python_count:3d}, Fortran={fortran_count:3d}"
                )
            else:
                print(
                    f"  [FAIL] {py_group.value:12s}: Python={python_count:3d}, Fortran={fortran_count:3d} MISMATCH!"
                )
                mismatches.append((py_group.value, python_count, fortran_count))

        print()
        if mismatches:
            print("=" * 70)
            print("[FAIL] CONSISTENCY CHECK FAILED")
            print("=" * 70)
            print("Mismatches found:")
            for group, py_count, f_count in mismatches:
                print(f"  - {group}: Python has {py_count}, Fortran expects {f_count}")
            print()
            print("ACTION REQUIRED:")
            print(
                "1. If Fortran changed: update Python registry in src/supy/data_model/output/"
            )
            print(
                "2. If Python changed: update ncolumnsDataOut* in src/suews/src/suews_ctrl_const.f95"
            )
            pytest.fail(f"Fortran/Python mismatch: {mismatches}")
        else:
            print("=" * 70)
            print("[PASS] CONSISTENCY CHECK PASSED")
            print("=" * 70)
            print(
                "Python OUTPUT_REGISTRY matches Fortran ncolumnsDataOut* constants exactly."
            )
            print()

    except ImportError as e:
        print()
        print("=" * 70)
        print("[SKIP] SKIPPED: supy not available")
        print("=" * 70)
        print(f"Reason: {e}")
        print("This test requires a full supy build with Fortran components.")
        print()
        pytest.skip(f"supy not available: {e}")

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
