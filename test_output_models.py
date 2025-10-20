#!/usr/bin/env python3
"""Standalone test for output variable Pydantic models.

This script tests the new Python-first output variable definitions
without requiring a full SUEWS build.
"""

import sys
import os

# Add src/supy/data_model directly to path to avoid importing full supy package
data_model_path = os.path.join(os.path.dirname(__file__), 'src', 'supy', 'data_model')
sys.path.insert(0, data_model_path)

# Direct import from output module
from output.variables import (
    OutputVariable,
    OutputVariableRegistry,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)
from output.datetime_vars import DATETIME_VARIABLES
from output.suews_vars import SUEWS_VARIABLES

# Assemble registry manually for testing
OUTPUT_REGISTRY = OutputVariableRegistry(
    variables=(DATETIME_VARIABLES + SUEWS_VARIABLES)
)


def test_registry_basic():
    """Test that registry loads and has expected variables."""
    print("Testing OUTPUT_REGISTRY basic functionality...")

    # Check registry is not empty
    assert len(OUTPUT_REGISTRY.variables) > 0, "Registry should not be empty"
    print(f"✓ Registry contains {len(OUTPUT_REGISTRY.variables)} variables")

    # Check we have datetime variables
    datetime_vars = OUTPUT_REGISTRY.by_group(OutputGroup.DATETIME)
    assert len(datetime_vars) == 5, "Should have 5 datetime variables"
    print(f"✓ Found {len(datetime_vars)} datetime variables")

    # Check we have SUEWS variables
    suews_vars = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)
    assert len(suews_vars) > 0, "Should have SUEWS variables"
    print(f"✓ Found {len(suews_vars)} SUEWS variables")

    print()


def test_specific_variables():
    """Test that specific key variables are present with correct metadata."""
    print("Testing specific variable definitions...")

    # Test QH (Sensible heat flux)
    qh = OUTPUT_REGISTRY.by_name("QH")
    assert qh is not None, "QH variable should exist"
    assert qh.unit == "W m-2", f"QH unit should be 'W m-2', got '{qh.unit}'"
    assert qh.aggregation == AggregationMethod.AVERAGE, "QH should use AVERAGE aggregation"
    assert qh.group == OutputGroup.SUEWS, "QH should be in SUEWS group"
    assert qh.level == OutputLevel.DEFAULT, "QH should be DEFAULT level"
    print(f"✓ QH: {qh.description} [{qh.unit}]")

    # Test T2 (Air temperature at 2m)
    t2 = OUTPUT_REGISTRY.by_name("T2")
    assert t2 is not None, "T2 variable should exist"
    assert t2.unit == "degC", f"T2 unit should be 'degC', got '{t2.unit}'"
    assert t2.aggregation == AggregationMethod.AVERAGE, "T2 should use AVERAGE aggregation"
    print(f"✓ T2: {t2.description} [{t2.unit}]")

    # Test Rain (precipitation)
    rain = OUTPUT_REGISTRY.by_name("Rain")
    assert rain is not None, "Rain variable should exist"
    assert rain.aggregation == AggregationMethod.SUM, "Rain should use SUM aggregation"
    print(f"✓ Rain: {rain.description} [{rain.unit}] (aggregation: SUM)")

    # Test SMD (Soil Moisture Deficit)
    smd = OUTPUT_REGISTRY.by_name("SMD")
    assert smd is not None, "SMD variable should exist"
    assert smd.aggregation == AggregationMethod.LAST, "SMD should use LAST aggregation"
    print(f"✓ SMD: {smd.description} [{smd.unit}] (aggregation: LAST)")

    print()


def test_output_levels():
    """Test filtering by output level."""
    print("Testing output level filtering...")

    # Get DEFAULT level variables
    default_vars = OUTPUT_REGISTRY.by_level(OutputLevel.DEFAULT)
    print(f"✓ DEFAULT level: {len(default_vars)} variables")

    # Get up to EXTENDED level
    extended_vars = OUTPUT_REGISTRY.by_level(OutputLevel.EXTENDED)
    print(f"✓ Up to EXTENDED level: {len(extended_vars)} variables")

    # Get up to SNOW_DETAILED level
    all_vars = OUTPUT_REGISTRY.by_level(OutputLevel.SNOW_DETAILED)
    print(f"✓ Up to SNOW_DETAILED level: {len(all_vars)} variables")

    # DEFAULT should be subset of EXTENDED
    assert len(default_vars) <= len(extended_vars), "DEFAULT should be subset of EXTENDED"
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

    print(f"✓ Generated aggregation rules for {len(agg_rules)} groups")
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

    print(f"✓ DataFrame has correct structure: {df.shape}")
    print(f"  - Index: {df.index.names}")
    print(f"  - Columns: {list(df.columns)}")

    print()


def test_enum_values():
    """Test that enum values work correctly."""
    print("Testing enum value behavior...")

    # Test aggregation methods
    assert AggregationMethod.AVERAGE.value == "A"
    assert AggregationMethod.SUM.value == "S"
    assert AggregationMethod.LAST.value == "L"
    assert AggregationMethod.TIME.value == "T"
    print("✓ AggregationMethod enums have correct values")

    # Test output levels
    assert OutputLevel.DEFAULT.value == 0
    assert OutputLevel.EXTENDED.value == 1
    assert OutputLevel.SNOW_DETAILED.value == 2
    print("✓ OutputLevel enums have correct values")

    # Test output groups
    assert OutputGroup.DATETIME.value == "datetime"
    assert OutputGroup.SUEWS.value == "SUEWS"
    print("✓ OutputGroup enums have correct values")

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

        print("=" * 70)
        print("✅ ALL TESTS PASSED!")
        print("=" * 70)
        print()
        print("Summary:")
        print(f"- Total variables in registry: {len(OUTPUT_REGISTRY.variables)}")
        datetime_count = len(OUTPUT_REGISTRY.by_group(OutputGroup.DATETIME))
        suews_count = len(OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS))
        print(f"- Datetime variables: {datetime_count}")
        print(f"- SUEWS variables: {suews_count}")
        print()
        print("The Pydantic models are working correctly and ready for integration!")

        return 0

    except AssertionError as e:
        print()
        print("=" * 70)
        print(f"❌ TEST FAILED: {e}")
        print("=" * 70)
        return 1
    except Exception as e:
        print()
        print("=" * 70)
        print(f"❌ UNEXPECTED ERROR: {e}")
        import traceback
        traceback.print_exc()
        print("=" * 70)
        return 1


if __name__ == "__main__":
    sys.exit(main())
