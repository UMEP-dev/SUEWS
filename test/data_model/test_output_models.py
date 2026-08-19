#!/usr/bin/env python3
"""Standalone test for output variable Pydantic models.

This script tests the new Python-first output variable definitions
without requiring a full SUEWS build.
"""


import pytest

pytestmark = pytest.mark.api

# Add src/supy/data_model directly to path to avoid importing full supy package
# Import the PRODUCTION registry -- the same objects _post.py uses --
# so these tests fail when the real output surface drifts.
from supy.data_model.output import OUTPUT_REGISTRY
from supy.data_model.output.variables import (
    AggregationMethod,
    OutputGroup,
    OutputLevel,
    OutputVariable,
    OutputVariableRegistry,
)


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


def _load_fortran_group_ncolumns():
    """Load per-group column counts from the compiled Fortran library via the Rust bridge.

    Returns a dict mapping group name to data column count (excluding datetime prefix,
    except 'datetime' which returns 5).
    """
    from importlib import import_module

    for module_name in ("supy.suews_bridge", "suews_bridge"):
        try:
            bridge = import_module(module_name)
            return dict(bridge.output_group_ncolumns())
        except Exception:
            pass
    pytest.skip("Rust bridge not available (output_group_ncolumns)")


def test_fortran_python_output_consistency():
    """Verify Python OUTPUT_REGISTRY matches compiled Fortran ncolumnsDataOut* constants.

    Calls the compiled Fortran library through the Rust C API bridge to get
    the ncolumnsDataOut* integer constants, then verifies Python's
    OUTPUT_REGISTRY has the exact same count for each output group.

    This provides runtime verification that Python and Fortran stay in sync,
    using the actual compiled library rather than source-level inspection.

    If this test fails, either:
    - Fortran ncolumnsDataOut* constants were changed (update Python registry)
    - Python registry was changed (update Fortran constants)
    """
    print("=" * 70)
    print("Fortran/Python Output Consistency Verification...")
    print("=" * 70)
    print()

    from supy.data_model.output import OUTPUT_REGISTRY, OutputGroup

    fortran_ncolumns = _load_fortran_group_ncolumns()

    # Mapping from Python OutputGroup to Fortran group name strings
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

    print("Verifying Python registry against compiled Fortran ncolumnsDataOut* constants:")
    mismatches = []
    for py_group, fortran_name in GROUP_MAPPING.items():
        python_count = len(OUTPUT_REGISTRY.by_group(py_group))
        fortran_count = fortran_ncolumns[fortran_name]

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
            "Python OUTPUT_REGISTRY matches compiled Fortran ncolumnsDataOut* constants exactly."
        )
        print()
