"""Tests for validation utility functions."""

import pytest
from supy.data_model.type import RefValue
from supy.data_model.validation_utils import check_missing_params, warn_missing_params


class TestParameterValidation:
    """Test parameter validation functions."""

    def test_check_missing_params_with_none(self):
        """Test that None values are detected as missing."""
        
        class TestObject:
            param1 = None
            param2 = 42.0
        
        test_obj = TestObject()
        params = {"param1": "First parameter", "param2": "Second parameter"}
        
        missing = check_missing_params(params, test_obj, "test", "test")
        assert len(missing) == 1
        assert "param1 (First parameter)" in missing

    def test_check_missing_params_with_refvalue(self):
        """Test that RefValue objects are handled correctly."""
        
        class TestObject:
            param1 = RefValue(value=25.0)  # Has value - not missing
            param2 = RefValue(value=None)  # No value - missing
            param3 = None  # Plain None - missing
        
        test_obj = TestObject()
        params = {
            "param1": "RefValue with value",
            "param2": "RefValue without value",
            "param3": "Plain None"
        }
        
        missing = check_missing_params(params, test_obj, "test", "test")
        assert len(missing) == 2
        assert "param2 (RefValue without value)" in missing
        assert "param3 (Plain None)" in missing
        assert "param1 (RefValue with value)" not in missing

    def test_check_missing_params_all_present(self):
        """Test when all parameters are present."""
        
        class TestObject:
            param1 = 10.0
            param2 = RefValue(value=20.0)
            param3 = {"value": 30.0}  # Dict format (as might come from YAML)
        
        test_obj = TestObject()
        params = {
            "param1": "First parameter",
            "param2": "Second parameter",
            "param3": "Third parameter"
        }
        
        missing = check_missing_params(params, test_obj, "test", "test")
        assert len(missing) == 0

    def test_warn_missing_params(self):
        """Test warning generation for missing parameters."""
        import warnings
        
        missing_params = [
            "param1 (Important parameter)",
            "param2 (Another parameter)"
        ]
        
        # Should produce warning
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            warn_missing_params(missing_params, "test category", "test impact", stacklevel=1)
            
            assert len(w) == 1
            assert "Missing critical test category parameters" in str(w[0].message)
            assert "test impact" in str(w[0].message)
            assert "param1 (Important parameter)" in str(w[0].message)
        
        # Should not produce warning when list is empty
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            warn_missing_params([], "test category", "test impact", stacklevel=1)
            assert len(w) == 0

    def test_refvalue_edge_cases(self):
        """Test edge cases for RefValue validation."""
        
        class TestObject:
            # Edge case: RefValue with 0.0 (should not be missing)
            param1 = RefValue(value=0.0)
            # Edge case: RefValue with empty string (should not trigger as missing for numeric params)
            param2 = RefValue(value="")
            # Edge case: RefValue with False (should not be missing)
            param3 = RefValue(value=False)
        
        test_obj = TestObject()
        params = {
            "param1": "Zero value",
            "param2": "Empty string",
            "param3": "False value"
        }
        
        missing = check_missing_params(params, test_obj, "test", "test")
        # None of these should be considered missing
        assert len(missing) == 0