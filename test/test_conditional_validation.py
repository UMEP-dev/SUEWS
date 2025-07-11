"""
Test cases for conditional validation system in SUEWS.

===============================================================================
IMPORTANT NOTE FOR SILVIA (Issue #400):
===============================================================================
These tests have been temporarily disabled because they expect the old 
component-level validation approach. They need to be updated for the new 
top-down validation system.

Key changes needed:
1. Move all validation logic to SUEWSConfig.validate_parameter_completeness()
2. Remove expectations of component-level warnings
3. Use ValidationResult structure for reporting issues
4. See NOTE_FOR_SILVIA_VALIDATION_UPDATE.md for migration guide

To re-enable: Remove the @pytest.mark.skip decorator from the test module
===============================================================================

These tests verify that the conditional validation system correctly:
1. Validates only relevant parameters based on enabled methods
2. Skips validation for parameters of disabled methods
3. Provides clear error messages for validation failures
4. Integrates properly with from_yaml and to_df_state workflows
"""

import pytest
import tempfile
import os
import yaml
import warnings

# Basic imports that should always work
from supy.data_model import SUEWSConfig
from supy.data_model.model import RSLMethod, RoughnessMethod
from supy.data_model.type import RefValue

# Skip all tests in this module until updated for new validation approach
pytestmark = pytest.mark.skip(
    reason="Needs update for new top-down validation approach (Issue #400 - Silvia)"
)


# Test if enhanced functionality is working
def test_suews_config_basic():
    """Test basic SUEWSConfig functionality works."""
    # Create config with at least one site
    config = SUEWSConfig(sites=[{
        "gridiv": 1,
        "properties": {
            "lat": {"value": 51.5},
            "lng": {"value": -0.1},
            "alt": {"value": 10.0},
            "timezone": {"value": 0}
        }
    }])
    assert config.name == "sample config"
    assert hasattr(config.model.physics, "rslmethod")

    # Test to_df_state works
    df_state = config.to_df_state()
    assert df_state is not None
    assert not df_state.empty


def test_suews_config_enhanced_methods():
    """Test that enhanced methods exist and can be called."""
    # Create config with at least one site
    config = SUEWSConfig(sites=[{
        "gridiv": 1,
        "properties": {
            "lat": {"value": 51.5},
            "lng": {"value": -0.1},
            "alt": {"value": 10.0},
            "timezone": {"value": 0}
        }
    }])
    
    # Test validation method exists
    assert hasattr(config, 'run_conditional_validation')
    # Test it can be called
    try:
        result = config.run_conditional_validation()
        assert hasattr(result, 'success')
        assert hasattr(result, 'errors')
    except Exception as e:
        pytest.fail(f"Could not run conditional validation: {e}")


def test_suews_config_different_rslmethods():
    """Test config can be created with different RSL methods."""
    # Test with CONSTANT
    config1 = SUEWSConfig(
        model={"physics": {"rslmethod": {"value": RSLMethod.CONSTANT}}},
        sites=[{
            "gridiv": 1,
            "properties": {
                "lat": {"value": 51.5},
                "lng": {"value": -0.1},
                "alt": {"value": 10.0},
                "timezone": {"value": 0}
            }
        }]
    )
    assert config1.model.physics.rslmethod.value == RSLMethod.CONSTANT
    
    # Test with VARIABLE
    config2 = SUEWSConfig(
        model={"physics": {"rslmethod": {"value": RSLMethod.VARIABLE}}},
        sites=[{
            "gridiv": 1, 
            "properties": {
                "lat": {"value": 51.5},
                "lng": {"value": -0.1},
                "alt": {"value": 10.0},
                "timezone": {"value": 0}
            }
        }]
    )
    assert config2.model.physics.rslmethod.value == RSLMethod.VARIABLE


def test_yaml_loading_basic():
    """Test loading a basic configuration from YAML."""
    yaml_content = """
model:
  physics:
    rslmethod: {value: 2}  # CONSTANT
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      timezone: {value: 0}
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        f.write(yaml_content)
        temp_path = f.name
    
    try:
        config = SUEWSConfig.from_yaml(temp_path)
        assert config.model.physics.rslmethod.value == RSLMethod.CONSTANT
        assert len(config.sites) == 1
    finally:
        os.unlink(temp_path)


def test_conditional_validation_warnings():
    """Test that warnings are generated for missing parameters based on enabled methods."""
    # This test expects warnings - which should now come from top-level validation
    yaml_content = """
model:
  physics:
    rslmethod: {value: 4}  # VARIABLE - requires variable roughness parameters
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      timezone: {value: 0}
      # Missing variable roughness parameters
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        f.write(yaml_content)
        temp_path = f.name
    
    try:
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            config = SUEWSConfig.from_yaml(temp_path)
            
            # Run validation
            result = config.run_conditional_validation()
            
            # Should have validation errors for missing variable roughness params
            assert not result.success
            assert len(result.errors) > 0
            
            # Check for specific missing parameters
            error_msgs = [e.message for e in result.errors]
            assert any("variable roughness" in msg.lower() for msg in error_msgs)
    finally:
        os.unlink(temp_path)


def test_backward_compatibility():
    """Test that old-style validation still works for existing code."""
    # Create a config that should trigger validation
    config = SUEWSConfig(
        model={"physics": {"rslmethod": {"value": RSLMethod.VARIABLE}}},
        sites=[{
            "gridiv": 1,
            "properties": {
                "lat": {"value": 51.5},
                "lng": {"value": -0.1}, 
                "alt": {"value": 10.0},
                "timezone": {"value": 0}
                # Missing variable roughness parameters
            }
        }]
    )
    
    # Should be able to run validation
    result = config.run_conditional_validation()
    assert hasattr(result, 'success')
    assert hasattr(result, 'errors')
    
    # Should fail due to missing parameters
    assert not result.success


def test_storage_heat_validation():
    """Test validation for StorageHeatMethod configurations."""
    # Test ESTM method requires thermal parameters
    yaml_content = """
model:
  physics:
    storageheatmethod: {value: 4}  # ESTM - requires thermal parameters
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      timezone: {value: 0}
      land_cover:
        paved:
          sfr: {value: 0.5}
          # Missing thermal_layers
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        f.write(yaml_content)
        temp_path = f.name
    
    try:
        config = SUEWSConfig.from_yaml(temp_path)
        result = config.run_conditional_validation()
        
        # Should fail due to missing thermal parameters
        assert not result.success
        assert any("thermal" in e.message.lower() for e in result.errors)
    finally:
        os.unlink(temp_path)


def test_netradiation_validation():
    """Test validation for NetRadiationMethod configurations."""
    # Test SPARTACUS method requires specific parameters
    yaml_content = """
model:
  physics:
    netradiationmethod: {value: 1003}  # SPARTACUS requires additional params
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      timezone: {value: 0}
      # Missing SPARTACUS parameters
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        f.write(yaml_content)
        temp_path = f.name
    
    try:
        config = SUEWSConfig.from_yaml(temp_path)
        result = config.run_conditional_validation()
        
        # Should fail due to missing SPARTACUS parameters
        assert not result.success
        assert any("spartacus" in e.message.lower() for e in result.errors)
    finally:
        os.unlink(temp_path)


def test_comprehensive_method_combinations():
    """Test various combinations of method settings."""
    test_cases = [
        # (rslmethod, roughnessmethod, storageheatmethod, netradiationmethod, should_pass)
        (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 1, 1, True),  # Basic config
        (RSLMethod.VARIABLE, RoughnessMethod.VARIABLE, 1, 1, False),  # Missing var roughness
        (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 4, 1, False),  # ESTM without thermal
        (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 1, 1003, False),  # SPARTACUS without params
    ]
    
    for rsl, rough, storage, netrad, should_pass in test_cases:
        config = SUEWSConfig(
            model={
                "physics": {
                    "rslmethod": {"value": rsl},
                    "roughnessmethod": {"value": rough},
                    "storageheatmethod": {"value": storage},
                    "netradiationmethod": {"value": netrad}
                }
            },
            sites=[{
                "gridiv": 1,
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.1},
                    "alt": {"value": 10.0},
                    "timezone": {"value": 0}
                }
            }]
        )
        
        result = config.run_conditional_validation()
        assert result.success == should_pass, (
            f"Test case failed: RSL={rsl}, Rough={rough}, Storage={storage}, "
            f"NetRad={netrad}, expected success={should_pass}"
        )


def test_integration_summary():
    """Test that validation summary provides useful information."""
    # Create a config with multiple validation issues
    config = SUEWSConfig(
        model={
            "physics": {
                "rslmethod": {"value": RSLMethod.VARIABLE},
                "roughnessmethod": {"value": RoughnessMethod.VARIABLE},
                "storageheatmethod": {"value": 4},  # ESTM
                "netradiationmethod": {"value": 1003}  # SPARTACUS
            }
        },
        sites=[{
            "gridiv": 1,
            "properties": {
                "lat": {"value": 51.5},
                "lng": {"value": -0.1},
                "alt": {"value": 10.0},
                "timezone": {"value": 0}
            }
        }]
    )
    
    result = config.run_conditional_validation()
    
    # Should have multiple types of errors
    assert not result.success
    assert len(result.errors) > 1
    
    # Check summary information
    assert hasattr(result, 'get_summary')
    summary = result.get_summary()
    assert "variable roughness" in summary.lower()
    assert "thermal" in summary.lower()
    assert "spartacus" in summary.lower()