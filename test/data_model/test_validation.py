"""
Consolidated tests for SUEWS validation logic.

This file combines:
- Conditional validation from test_conditional_validation.py
- Validation utilities from test_validation_utils.py
- Top-down validation from test_validation_topdown.py
- Essential migrated validators (from test_migrated_validators.py)
- Validator improvements from test_validator_improvements.py
"""

import io
import logging
from pathlib import Path
import tempfile
from types import SimpleNamespace
import warnings

import supy as sp
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.type import RefValue
from supy.data_model.validation.core.utils import check_missing_params


# A tiny “site” stub that only carries exactly the properties our validators look at
class DummySite:
    def __init__(self, properties, name="SiteX"):
        self.properties = properties
        self.name = name


# Helper to construct a SUEWSConfig without running Pydantic on sites,
# then inject the desired physics settings.
def make_cfg(**physics_kwargs):
    cfg = SUEWSConfig.model_construct()  # bypass validation
    # physics values wrapped in a simple .value holder
    phys = SimpleNamespace(**{
        k: SimpleNamespace(value=v) for k, v in physics_kwargs.items()
    })
    cfg.model = SimpleNamespace(physics=phys)
    return cfg


def test_needs_stebbs_validation_true_and_false():
    cfg = make_cfg(stebbsmethod=1)
    assert cfg._needs_stebbs_validation() is True
    cfg2 = make_cfg(stebbsmethod=0)
    assert cfg2._needs_stebbs_validation() is False


def test_validate_stebbs_missing_properties_block():
    cfg = make_cfg(stebbsmethod=1)
    site = DummySite(properties=None, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    assert msgs == ["Missing 'properties' section (required for STEBBS validation)"]


def test_validate_stebbs_missing_stebbs_section():
    cfg = make_cfg(stebbsmethod=1)
    props = SimpleNamespace(stebbs=None)
    site = DummySite(properties=props, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    assert msgs == ["Missing 'stebbs' section (required when stebbsmethod=1)"]


def test_validate_stebbs_missing_parameters():
    cfg = make_cfg(stebbsmethod=1)
    # Provide an empty stebbs object
    props = SimpleNamespace(stebbs=SimpleNamespace())
    site = DummySite(properties=props, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    # Should mention at least one of the required params
    assert msgs and msgs[0].startswith("Missing required STEBBS parameters:")


def test_needs_rsl_validation_true_and_false():
    # After conditional validation fix, validation is disabled by default
    # unless physics parameters are explicitly configured
    cfg = make_cfg(rslmethod=2)
    assert cfg._needs_rsl_validation() is False  # Disabled by default now
    cfg2 = make_cfg(rslmethod=1)
    assert cfg2._needs_rsl_validation() is False


def test_validate_rsl_no_land_cover_or_sfr():
    cfg = make_cfg(rslmethod=2)
    site = DummySite(properties=None)
    assert SUEWSConfig._validate_rsl(cfg, site, 0) == []
    # land_cover without bldgs
    site2 = DummySite(properties=SimpleNamespace(land_cover=None))
    assert SUEWSConfig._validate_rsl(cfg, site2, 0) == []


def test_validate_rsl_requires_faibldg():
    cfg = make_cfg(rslmethod=2)
    # build a land_cover.bldgs with sfr>0 but no faibldg
    bldgs = SimpleNamespace(sfr=SimpleNamespace(value=0.5), faibldg=None)
    lc = SimpleNamespace(bldgs=bldgs)
    site = DummySite(properties=SimpleNamespace(land_cover=lc), name="SiteR")
    msgs = SUEWSConfig._validate_rsl(cfg, site, 1)
    assert len(msgs) == 1
    assert "bldgs.faibldg must be set" in msgs[0]
    assert "SiteR" in msgs[0]


def test_needs_storage_validation_true_and_false():
    # After conditional validation fix, validation is disabled by default
    # unless physics parameters are explicitly configured
    cfg = make_cfg(storageheatmethod=6)
    assert cfg._needs_storage_validation() is False  # Disabled by default now
    cfg2 = make_cfg(storageheatmethod=1)
    assert cfg2._needs_storage_validation() is False


def test_validate_storage_requires_numeric_and_lambda():
    cfg = make_cfg(storageheatmethod=6)
    # stub thermal_layers: dz has one bad None, k OK, rho_cp missing
    th = SimpleNamespace(
        dz=SimpleNamespace(value=[None]),
        k=SimpleNamespace(value=[1.0, 2.0]),
        # rho_cp attribute not defined → treated missing
    )
    wall = SimpleNamespace(thermal_layers=th)
    props = SimpleNamespace(
        vertical_layers=SimpleNamespace(walls=[wall]),
        lambda_c=None,  # missing
    )
    site = DummySite(properties=props, name="SiteS")
    msgs = SUEWSConfig._validate_storage(cfg, site, 2)
    # must flag dz, rho_cp and lambda_c
    assert any("thermal_layers.dz" in m for m in msgs)
    assert any("thermal_layers.rho_cp" in m for m in msgs)
    assert any("properties.lambda_c must be set" in m for m in msgs)
    # should include the site name
    assert any("SiteS" in m for m in msgs)


def test_validate_lai_ranges_no_land_cover():
    """Test LAI validation with no land cover."""
    cfg = SUEWSConfig.model_construct()
    site = DummySite(properties=None)
    has_issues = cfg._check_lai_ranges(None, "TestSite")
    assert has_issues is False


def test_validate_lai_ranges_no_vegetation():
    """Test LAI validation with no vegetation surfaces."""
    cfg = SUEWSConfig.model_construct()
    # land_cover with no vegetation surfaces
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.5)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.5)),
    )
    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is False


def test_validate_lai_ranges_invalid_laimin_laimax():
    """Test LAI validation detects invalid laimin > laimax."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface with invalid LAI range
    lai = SimpleNamespace(
        laimin=SimpleNamespace(value=5.0), laimax=SimpleNamespace(value=3.0)
    )
    grass = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "LAI range validation" in cfg._validation_summary["issue_types"]
    assert any(
        "laimin (5.0) must be <= laimax (3.0)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_lai_ranges_invalid_baset_gddfull():
    """Test LAI validation detects invalid baset > gddfull."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface with invalid baset/gddfull range
    lai = SimpleNamespace(
        laimin=SimpleNamespace(value=1.0),
        laimax=SimpleNamespace(value=5.0),
        baset=SimpleNamespace(value=15.0),
        gddfull=SimpleNamespace(value=10.0),
    )
    dectr = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(dectr=dectr)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "LAI range validation" in cfg._validation_summary["issue_types"]
    assert any(
        "baset (15.0) must be <= gddfull (10.0)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_lai_ranges_multiple_vegetation_surfaces():
    """Test LAI validation checks all vegetation surfaces."""
    cfg = SUEWSConfig.model_construct()
    # Create multiple vegetation surfaces with invalid LAI ranges
    lai_invalid = SimpleNamespace(
        laimin=SimpleNamespace(value=5.0), laimax=SimpleNamespace(value=3.0)
    )

    grass = SimpleNamespace(lai=lai_invalid)
    dectr = SimpleNamespace(lai=lai_invalid)
    evetr = SimpleNamespace(lai=lai_invalid)

    lc = SimpleNamespace(grass=grass, dectr=dectr, evetr=evetr)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 3

    # Check that all surfaces were validated
    messages = cfg._validation_summary["detailed_messages"]
    assert any("grass" in msg for msg in messages)
    assert any("dectr" in msg for msg in messages)
    assert any("evetr" in msg for msg in messages)


def test_validate_lai_ranges_valid_ranges():
    """Test LAI validation passes with valid ranges."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface with valid LAI ranges
    lai = SimpleNamespace(
        laimin=SimpleNamespace(value=1.0),
        laimax=SimpleNamespace(value=5.0),
        baset=SimpleNamespace(value=5.0),
        gddfull=SimpleNamespace(value=200.0),
    )
    grass = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is False
    assert cfg._validation_summary["total_warnings"] == 0


def test_validate_lai_ranges_missing_lai_graceful():
    """Test LAI validation handles missing LAI gracefully."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface without LAI
    grass = SimpleNamespace()  # No lai attribute
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is False


def test_validate_lai_ranges_none_values():
    """Test LAI validation handles None values gracefully."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface with None LAI values
    lai = SimpleNamespace(laimin=None, laimax=None, baset=None, gddfull=None)
    grass = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is False


def test_validate_land_cover_fractions_no_land_cover():
    """Test land cover fraction validation with no land cover."""
    cfg = SUEWSConfig.model_construct()
    has_issues = cfg._check_land_cover_fractions(None, "TestSite")
    assert has_issues is False


def test_validate_land_cover_fractions_sum_to_one():
    """Test land cover fraction validation passes when fractions sum to 1.0."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with fractions that sum to 1.0
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.4)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.3)),
        grass=SimpleNamespace(sfr=SimpleNamespace(value=0.2)),
        dectr=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        evetr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        bsoil=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        water=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert has_issues is False
    assert cfg._validation_summary["total_warnings"] == 0


def test_validate_land_cover_fractions_sum_too_high():
    """Test land cover fraction validation detects fractions that sum > 1.0."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with fractions that sum to > 1.0
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.6)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.5)),
        grass=SimpleNamespace(sfr=SimpleNamespace(value=0.2)),
        dectr=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        evetr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        bsoil=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        water=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "Land cover fraction validation" in cfg._validation_summary["issue_types"]
    assert any(
        "must sum to 1.0 (got 1.400000)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_land_cover_fractions_sum_too_low():
    """Test land cover fraction validation detects fractions that sum < 1.0."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with fractions that sum to < 1.0
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.3)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.2)),
        grass=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        dectr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        evetr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        bsoil=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        water=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "Land cover fraction validation" in cfg._validation_summary["issue_types"]
    assert any(
        "must sum to 1.0 (got 0.600000)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_land_cover_fractions_handles_missing_surfaces():
    """Test land cover fraction validation handles missing surface types."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with only some surface types
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.5)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.5)),
        # Missing other surface types
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert (
        has_issues is False
    )  # Sum is 1.0 (0.5 + 0.5 + 0.0 + 0.0 + 0.0 + 0.0 + 0.0 = 1.0)


def test_validate_land_cover_fractions_handles_none_values():
    """Test land cover fraction validation handles None sfr values."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with None sfr values
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=None),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=1.0)),
        grass=SimpleNamespace(),  # No sfr attribute
        dectr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        evetr=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        bsoil=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
        water=SimpleNamespace(sfr=SimpleNamespace(value=0.0)),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert (
        has_issues is False
    )  # Sum is 1.0 (0.0 + 1.0 + 0.0 + 0.0 + 0.0 + 0.0 + 0.0 = 1.0)


def test_validate_land_cover_fractions_handles_direct_values():
    """Test land cover fraction validation handles direct (non-RefValue) values."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with direct values (not RefValue)
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=0.4),  # Direct value
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.3)),  # RefValue
        grass=SimpleNamespace(sfr=0.2),  # Direct value
        dectr=SimpleNamespace(sfr=0.1),  # Direct value
        evetr=SimpleNamespace(sfr=0.0),
        bsoil=SimpleNamespace(sfr=0.0),
        water=SimpleNamespace(sfr=0.0),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert has_issues is False  # Sum is 1.0


def test_validate_land_cover_fractions_detailed_message():
    """Test land cover fraction validation provides detailed error message."""
    cfg = SUEWSConfig.model_construct()
    # Create land cover with fractions that sum to != 1.0
    lc = SimpleNamespace(
        paved=SimpleNamespace(sfr=SimpleNamespace(value=0.3)),
        bldgs=SimpleNamespace(sfr=SimpleNamespace(value=0.2)),
        grass=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        dectr=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        evetr=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        bsoil=SimpleNamespace(sfr=SimpleNamespace(value=0.1)),
        water=SimpleNamespace(sfr=SimpleNamespace(value=0.05)),
    )

    has_issues = cfg._check_land_cover_fractions(lc, "TestSite")
    assert has_issues is True

    # Check detailed message contains all surface types
    messages = cfg._validation_summary["detailed_messages"]
    assert len(messages) >= 1
    message = messages[0]
    assert "TestSite" in message
    assert "paved=0.300" in message
    assert "bldgs=0.200" in message
    assert "grass=0.100" in message
    assert "got 0.950000" in message  # Sum is 0.95


# """
# Test cases for conditional validation system in SUEWS.

# ===============================================================================
# IMPORTANT NOTE FOR SILVIA (Issue #400):
# ===============================================================================
# These tests have been temporarily disabled because they expect the old
# component-level validation approach. They need to be updated for the new
# top-down validation system.

# Key changes needed:
# 1. Move all validation logic to SUEWSConfig.validate_parameter_completeness()
# 2. Remove expectations of component-level warnings
# 3. Use ValidationResult structure for reporting issues
# 4. See NOTE_FOR_SILVIA_VALIDATION_UPDATE.md for migration guide

# To re-enable: Remove the @pytest.mark.skip decorator from the test module
# ===============================================================================

# These tests verify that the conditional validation system correctly:
# 1. Validates only relevant parameters based on enabled methods
# 2. Skips validation for parameters of disabled methods
# 3. Provides clear error messages for validation failures
# 4. Integrates properly with from_yaml and to_df_state workflows
# """

# import pytest
# import tempfile
# import os
# import yaml
# import warnings

# # Basic imports that should always work
# from supy.data_model import SUEWSConfig
# from supy.data_model.model import RSLMethod, RoughnessMethod
# from supy.data_model.core.type import RefValue

# # Skip all tests in this module until updated for new validation approach
# pytestmark = pytest.mark.skip(
#     reason="Needs update for new top-down validation approach (Issue #400 - Silvia)"
# )


# # Test if enhanced functionality is working
# def test_suews_config_basic():
#     """Test basic SUEWSConfig functionality works."""
#     # Create config with at least one site
#     config = SUEWSConfig(sites=[{
#         "gridiv": 1,
#         "properties": {
#             "lat": {"value": 51.5},
#             "lng": {"value": -0.1},
#             "alt": {"value": 10.0},
#             "timezone": {"value": 0}
#         }
#     }])
#     assert config.name == "sample config"
#     assert hasattr(config.model.physics, "rslmethod")

#     # Test to_df_state works
#     df_state = config.to_df_state()
#     assert df_state is not None
#     assert not df_state.empty


# def test_suews_config_enhanced_methods():
#     """Test that enhanced methods exist and can be called."""
#     # Create config with at least one site
#     config = SUEWSConfig(sites=[{
#         "gridiv": 1,
#         "properties": {
#             "lat": {"value": 51.5},
#             "lng": {"value": -0.1},
#             "alt": {"value": 10.0},
#             "timezone": {"value": 0}
#         }
#     }])

#     # Test validation method exists
#     assert hasattr(config, 'run_conditional_validation')
#     # Test it can be called
#     try:
#         result = config.run_conditional_validation()
#         assert hasattr(result, 'success')
#         assert hasattr(result, 'errors')
#     except Exception as e:
#         pytest.fail(f"Could not run conditional validation: {e}")


# def test_suews_config_different_rslmethods():
#     """Test config can be created with different RSL methods."""
#     # Test with CONSTANT
#     config1 = SUEWSConfig(
#         model={"physics": {"rslmethod": {"value": RSLMethod.CONSTANT}}},
#         sites=[{
#             "gridiv": 1,
#             "properties": {
#                 "lat": {"value": 51.5},
#                 "lng": {"value": -0.1},
#                 "alt": {"value": 10.0},
#                 "timezone": {"value": 0}
#             }
#         }]
#     )
#     assert config1.model.physics.rslmethod.value == RSLMethod.CONSTANT

#     # Test with VARIABLE
#     config2 = SUEWSConfig(
#         model={"physics": {"rslmethod": {"value": RSLMethod.VARIABLE}}},
#         sites=[{
#             "gridiv": 1,
#             "properties": {
#                 "lat": {"value": 51.5},
#                 "lng": {"value": -0.1},
#                 "alt": {"value": 10.0},
#                 "timezone": {"value": 0}
#             }
#         }]
#     )
#     assert config2.model.physics.rslmethod.value == RSLMethod.VARIABLE


# def test_yaml_loading_basic():
#     """Test loading a basic configuration from YAML."""
#     yaml_content = """
# model:
#   physics:
#     rslmethod: {value: 2}  # CONSTANT
# sites:
#   - gridiv: 1
#     properties:
#       lat: {value: 51.5}
#       lng: {value: -0.1}
#       alt: {value: 10.0}
#       timezone: {value: 0}
# """

#     with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
#         f.write(yaml_content)
#         temp_path = f.name

#     try:
#         config = SUEWSConfig.from_yaml(temp_path)
#         assert config.model.physics.rslmethod.value == RSLMethod.CONSTANT
#         assert len(config.sites) == 1
#     finally:
#         os.unlink(temp_path)


# def test_conditional_validation_warnings():
#     """Test that warnings are generated for missing parameters based on enabled methods."""
#     # This test expects warnings - which should now come from top-level validation
#     yaml_content = """
# model:
#   physics:
#     rslmethod: {value: 4}  # VARIABLE - requires variable roughness parameters
# sites:
#   - gridiv: 1
#     properties:
#       lat: {value: 51.5}
#       lng: {value: -0.1}
#       alt: {value: 10.0}
#       timezone: {value: 0}
#       # Missing variable roughness parameters
# """

#     with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
#         f.write(yaml_content)
#         temp_path = f.name

#     try:
#         with warnings.catch_warnings(record=True) as w:
#             warnings.simplefilter("always")
#             config = SUEWSConfig.from_yaml(temp_path)

#             # Run validation
#             result = config.run_conditional_validation()

#             # Should have validation errors for missing variable roughness params
#             assert not result.success
#             assert len(result.errors) > 0

#             # Check for specific missing parameters
#             error_msgs = [e.message for e in result.errors]
#             assert any("variable roughness" in msg.lower() for msg in error_msgs)
#     finally:
#         os.unlink(temp_path)


# def test_backward_compatibility():
#     """Test that old-style validation still works for existing code."""
#     # Create a config that should trigger validation
#     config = SUEWSConfig(
#         model={"physics": {"rslmethod": {"value": RSLMethod.VARIABLE}}},
#         sites=[{
#             "gridiv": 1,
#             "properties": {
#                 "lat": {"value": 51.5},
#                 "lng": {"value": -0.1},
#                 "alt": {"value": 10.0},
#                 "timezone": {"value": 0}
#                 # Missing variable roughness parameters
#             }
#         }]
#     )

#     # Should be able to run validation
#     result = config.run_conditional_validation()
#     assert hasattr(result, 'success')
#     assert hasattr(result, 'errors')

#     # Should fail due to missing parameters
#     assert not result.success


# def test_storage_heat_validation():
#     """Test validation for StorageHeatMethod configurations."""
#     # Test ESTM method requires thermal parameters
#     yaml_content = """
# model:
#   physics:
#     storageheatmethod: {value: 4}  # ESTM - requires thermal parameters
# sites:
#   - gridiv: 1
#     properties:
#       lat: {value: 51.5}
#       lng: {value: -0.1}
#       alt: {value: 10.0}
#       timezone: {value: 0}
#       land_cover:
#         paved:
#           sfr: {value: 0.5}
#           # Missing thermal_layers
# """

#     with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
#         f.write(yaml_content)
#         temp_path = f.name

#     try:
#         config = SUEWSConfig.from_yaml(temp_path)
#         result = config.run_conditional_validation()

#         # Should fail due to missing thermal parameters
#         assert not result.success
#         assert any("thermal" in e.message.lower() for e in result.errors)
#     finally:
#         os.unlink(temp_path)


# def test_netradiation_validation():
#     """Test validation for NetRadiationMethod configurations."""
#     # Test SPARTACUS method requires specific parameters
#     yaml_content = """
# model:
#   physics:
#     netradiationmethod: {value: 1003}  # SPARTACUS requires additional params
# sites:
#   - gridiv: 1
#     properties:
#       lat: {value: 51.5}
#       lng: {value: -0.1}
#       alt: {value: 10.0}
#       timezone: {value: 0}
#       # Missing SPARTACUS parameters
# """

#     with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
#         f.write(yaml_content)
#         temp_path = f.name

#     try:
#         config = SUEWSConfig.from_yaml(temp_path)
#         result = config.run_conditional_validation()

#         # Should fail due to missing SPARTACUS parameters
#         assert not result.success
#         assert any("spartacus" in e.message.lower() for e in result.errors)
#     finally:
#         os.unlink(temp_path)


# def test_comprehensive_method_combinations():
#     """Test various combinations of method settings."""
#     test_cases = [
#         # (rslmethod, roughnessmethod, storageheatmethod, netradiationmethod, should_pass)
#         (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 1, 1, True),  # Basic config
#         (RSLMethod.VARIABLE, RoughnessMethod.VARIABLE, 1, 1, False),  # Missing var roughness
#         (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 4, 1, False),  # ESTM without thermal
#         (RSLMethod.CONSTANT, RoughnessMethod.FIXED, 1, 1003, False),  # SPARTACUS without params
#     ]

#     for rsl, rough, storage, netrad, should_pass in test_cases:
#         config = SUEWSConfig(
#             model={
#                 "physics": {
#                     "rslmethod": {"value": rsl},
#                     "roughnessmethod": {"value": rough},
#                     "storageheatmethod": {"value": storage},
#                     "netradiationmethod": {"value": netrad}
#                 }
#             },
#             sites=[{
#                 "gridiv": 1,
#                 "properties": {
#                     "lat": {"value": 51.5},
#                     "lng": {"value": -0.1},
#                     "alt": {"value": 10.0},
#                     "timezone": {"value": 0}
#                 }
#             }]
#         )

#         result = config.run_conditional_validation()
#         assert result.success == should_pass, (
#             f"Test case failed: RSL={rsl}, Rough={rough}, Storage={storage}, "
#             f"NetRad={netrad}, expected success={should_pass}"
#         )


# def test_integration_summary():
#     """Test that validation summary provides useful information."""
#     # Create a config with multiple validation issues
#     config = SUEWSConfig(
#         model={
#             "physics": {
#                 "rslmethod": {"value": RSLMethod.VARIABLE},
#                 "roughnessmethod": {"value": RoughnessMethod.VARIABLE},
#                 "storageheatmethod": {"value": 4},  # ESTM
#                 "netradiationmethod": {"value": 1003}  # SPARTACUS
#             }
#         },
#         sites=[{
#             "gridiv": 1,
#             "properties": {
#                 "lat": {"value": 51.5},
#                 "lng": {"value": -0.1},
#                 "alt": {"value": 10.0},
#                 "timezone": {"value": 0}
#             }
#         }]
#     )

#     result = config.run_conditional_validation()

#     # Should have multiple types of errors
#     assert not result.success
#     assert len(result.errors) > 1

#     # Check summary information
#     assert hasattr(result, 'get_summary')
#     summary = result.get_summary()
#     assert "variable roughness" in summary.lower()
#     assert "thermal" in summary.lower()
#     assert "spartacus" in summary.lower()


# From test_validation_topdown.py
class TestTopDownValidation:
    """Test the new top-down validation approach."""

    def test_no_warnings_during_import(self):
        """Verify importing doesn't generate spurious warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            import importlib

            import supy.data_model

            importlib.reload(supy.data_model)

        assert len(w) == 0, f"Import generated {len(w)} warnings"

    def test_no_warnings_during_component_creation(self):
        """Verify component creation doesn't generate warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # Create various components that used to warn
            from supy.data_model.core.human_activity import CO2Params
            from supy.data_model.core.site import Conductance
            from supy.data_model.core.surface import BldgsProperties

            co2 = CO2Params()
            cond = Conductance()
            bldgs = BldgsProperties(sfr=RefValue(0.3))

        # Should have no warnings at component level
        assert len(w) == 0, f"Component creation generated {len(w)} warnings"

    def test_validation_at_config_level(self):
        """Test that validation happens at config level with clear summary."""
        config_yaml = """
sites:
  - site_id: test_site
    properties:
      land_cover:
        bldgs:
          sfr: {value: 0.45}
          # Missing critical params
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(config_yaml)
            yaml_path = Path(f.name)

        try:
            # Capture validation output
            log_capture = io.StringIO()
            handler = logging.StreamHandler(log_capture)
            handler.setLevel(logging.WARNING)
            logger = logging.getLogger("SuPy")
            logger.addHandler(handler)

            # Load config
            config = SUEWSConfig.from_yaml(yaml_path)

            # Check validation summary was generated
            log_output = log_capture.getvalue()
            assert "VALIDATION SUMMARY" in log_output
            assert "Missing building parameters" in log_output
            assert "generate_annotated_yaml" in log_output

            logger.removeHandler(handler)

        finally:
            yaml_path.unlink()

    def test_annotated_yaml_generation(self):
        """Test annotated YAML shows missing parameters clearly."""
        config_yaml = """
sites:
  - site_id: test_site  
    properties:
      land_cover:
        bldgs:
          sfr: {value: 0.3}
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(config_yaml)
            yaml_path = Path(f.name)

        try:
            config = SUEWSConfig.from_yaml(yaml_path)
            annotated_path = config.generate_annotated_yaml(yaml_path)

            # Check annotated file exists and contains annotations
            annotated_path = Path(annotated_path)  # Convert string to Path
            assert annotated_path.exists()
            content = annotated_path.read_text()

            # Should have missing parameter annotations
            assert "[ERROR] MISSING:" in content
            assert "[TIP] ADD HERE:" in content
            assert "bldgh:" in content  # Missing building height
            assert "faibldg:" in content  # Missing frontal area

            # Check simplified syntax (no {value: ...} wrapper)
            assert "bldgh: 20.0" in content or "bldgh: " in content
            assert "bldgh: {value:" not in content

            annotated_path.unlink()

        finally:
            yaml_path.unlink()

    def test_no_validation_with_complete_config(self):
        """Test no warnings when all required parameters are provided."""
        # This would be a complex test - simplified for now
        # Key point: when all params provided, validation summary should be minimal
        pass

    def test_auto_generate_annotated_yaml_option(self):
        """Test auto_generate_annotated parameter works correctly."""
        config_yaml = """
sites:
  - name: Test Site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      land_cover:
        bldgs:
          sfr: {value: 0.4}
          # Missing bldgh and faibldg parameters
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(config_yaml)
            yaml_path = Path(f.name)

        try:
            # Test with auto_generate_annotated=True
            config = SUEWSConfig.from_yaml(yaml_path, auto_generate_annotated=True)

            # Check that annotated file was automatically generated
            annotated_path = yaml_path.parent / f"{yaml_path.stem}_annotated.yml"
            assert annotated_path.exists(), (
                "Annotated YAML should be generated when auto_generate_annotated=True"
            )

            # Verify content
            content = annotated_path.read_text()
            assert "ANNOTATED SUEWS CONFIGURATION" in content
            assert "bldgh" in content

            # Clean up
            annotated_path.unlink()

            # Test with auto_generate_annotated=False (default)
            config2 = SUEWSConfig.from_yaml(yaml_path, auto_generate_annotated=False)
            assert not annotated_path.exists(), (
                "Annotated YAML should not be generated when auto_generate_annotated=False"
            )

        finally:
            yaml_path.unlink()


class TestValidationUtils:
    """Test validation utility functions still work correctly."""

    def test_check_missing_params_with_refvalue(self):
        """Test RefValue handling in validation utils."""

        class TestObj:
            good = RefValue(value=10.0)
            bad = RefValue(value=None)
            also_bad = None

        params = {"good": "Good param", "bad": "Bad param", "also_bad": "Also bad"}

        missing = check_missing_params(params, TestObj(), "test", "test")
        assert len(missing) == 2
        assert "bad (Bad param)" in missing
        assert "also_bad (Also bad)" in missing
        assert "good" not in str(missing)

    def test_refvalue_zero_not_missing(self):
        """Test that RefValue(0.0) is not considered missing."""

        class TestObj:
            zero = RefValue(value=0.0)
            false_val = RefValue(value=False)

        params = {"zero": "Zero", "false_val": "False"}
        missing = check_missing_params(params, TestObj(), "test", "test")
        assert len(missing) == 0


# Optional: Integration test
def test_full_validation_workflow():
    """Test complete validation workflow from YAML to annotated output."""
    yaml_content = """
name: Test Config
sites:
  - site_id: site1
    properties:
      land_cover:
        paved:
          sfr: {value: 0.5}
        bldgs:
          sfr: {value: 0.3}
        grass:
          sfr: {value: 0.2}
"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        yaml_path = Path(f.name)

    try:
        # Load config (triggers validation)
        config = SUEWSConfig.from_yaml(yaml_path)

        # Generate annotated YAML
        annotated = config.generate_annotated_yaml(yaml_path)

        # Verify it was created (Windows compatibility check)
        if annotated is not None:
            annotated = Path(annotated)  # Convert string to Path
            assert annotated.exists()
            assert "annotated" in str(annotated)

            # Clean up
            annotated.unlink()
        else:
            # If annotation generation failed, we can still verify validation occurred
            # by checking that the config was loaded successfully
            assert config.name == "Test Config"

    finally:
        yaml_path.unlink()


def test_phase_b_storageheatmethod_ohmincqf_validation():
    """Test StorageHeatMethod-OhmIncQf validation in Phase B."""
    from supy.data_model.validation.pipeline.phase_b import (
        validate_model_option_dependencies,
    )

    # Test incompatible combination: StorageHeatMethod=1 requires OhmIncQf=0
    yaml_data_incompatible = {
        "model": {
            "physics": {
                "storageheatmethod": {"value": 1},  # OHM_WITHOUT_QF
                "ohmincqf": {"value": 1},  # INCLUDE - incompatible!
            }
        }
    }

    results = validate_model_option_dependencies(yaml_data_incompatible)

    # Should find the incompatible combination
    storage_results = [
        r for r in results if r.parameter == "storageheatmethod-ohmincqf"
    ]
    assert len(storage_results) == 1
    assert storage_results[0].status == "ERROR"
    assert (
        "StorageHeatMethod is set to 1 and OhmIncQf is set to 1"
        in storage_results[0].message
    )
    assert "You should switch to OhmIncQf=0" in storage_results[0].message

    # Test compatible combination: StorageHeatMethod=1 with OhmIncQf=0
    yaml_data_compatible = {
        "model": {
            "physics": {
                "storageheatmethod": {"value": 1},  # OHM_WITHOUT_QF
                "ohmincqf": {"value": 0},  # EXCLUDE - compatible!
            }
        }
    }

    results = validate_model_option_dependencies(yaml_data_compatible)

    # Should pass validation
    storage_results = [
        r for r in results if r.parameter == "storageheatmethod-ohmincqf"
    ]
    assert len(storage_results) == 1
    assert storage_results[0].status == "PASS"
    assert (
        "StorageHeatMethod-OhmIncQf compatibility validated"
        in storage_results[0].message
    )


def test_phase_b_rsl_stabilitymethod_validation():
    """Test that existing RSL-StabilityMethod validation still works in Phase B."""
    from supy.data_model.validation.pipeline.phase_b import (
        validate_model_option_dependencies,
    )

    # Test incompatible combination: rslmethod=2 requires stabilitymethod=3
    yaml_data_incompatible = {
        "model": {
            "physics": {
                "rslmethod": {"value": 2},
                "stabilitymethod": {"value": 1},  # Should be 3
            }
        }
    }

    results = validate_model_option_dependencies(yaml_data_incompatible)

    # Should find the incompatible combination
    rsl_results = [r for r in results if "rslmethod-stabilitymethod" in r.parameter]
    assert len(rsl_results) == 1
    assert rsl_results[0].status == "ERROR"
    assert "rslmethod == 2" in rsl_results[0].message
    assert "stabilitymethod must be 3" in rsl_results[0].message


def test_phase_b_model_option_dependencies_comprehensive():
    """Test validate_model_option_dependencies function with various configurations."""
    from supy.data_model.validation.pipeline.phase_b import (
        validate_model_option_dependencies,
    )

    # Test with minimal physics configuration (should all pass)
    yaml_data_minimal = {
        "model": {
            "physics": {
                "storageheatmethod": {"value": 0},  # OBSERVED
                "ohmincqf": {"value": 0},  # EXCLUDE
                "rslmethod": {"value": 0},
                "stabilitymethod": {"value": 1},
            }
        }
    }

    results = validate_model_option_dependencies(yaml_data_minimal)

    # All should pass
    error_results = [r for r in results if r.status == "ERROR"]
    assert len(error_results) == 0, (
        f"Unexpected errors: {[r.message for r in error_results]}"
    )

    # Should have validation results for key parameters
    storage_results = [
        r for r in results if r.parameter == "storageheatmethod-ohmincqf"
    ]
    assert len(storage_results) == 1
    assert storage_results[0].status == "PASS"

    # Test with mixed valid/invalid combinations
    yaml_data_mixed = {
        "model": {
            "physics": {
                "storageheatmethod": {"value": 1},  # OHM_WITHOUT_QF
                "ohmincqf": {"value": 0},  # EXCLUDE - compatible
                "rslmethod": {"value": 2},  # Should require stabilitymethod=3
                "stabilitymethod": {"value": 1},  # Wrong value - incompatible
            }
        }
    }

    results = validate_model_option_dependencies(yaml_data_mixed)

    # Should have one error (RSL) and one pass (storage heat)
    error_results = [r for r in results if r.status == "ERROR"]
    pass_results = [r for r in results if r.status == "PASS"]

    # Find RSL error
    rsl_errors = [
        r for r in error_results if "rslmethod-stabilitymethod" in r.parameter
    ]
    assert len(rsl_errors) == 1

    # Find storage heat pass
    storage_passes = [
        r for r in pass_results if "storageheatmethod-ohmincqf" in r.parameter
    ]
    assert len(storage_passes) == 1

    # Test with missing physics section (should handle gracefully)
    yaml_data_no_physics = {"model": {}}

    results = validate_model_option_dependencies(yaml_data_no_physics)

    # Should handle gracefully - may have default values or skip validation
    assert isinstance(results, list)  # Should return a list, not crash


def test_phase_b_outdoor_air_annual_temperature_from_cru():
    """Test that OutdoorAirAnnualTemperature is populated from CRU annual mean data."""
    from supy.data_model.validation.pipeline.phase_b import (
        adjust_surface_temperatures,
        get_mean_annual_air_temperature,
    )

    # Test coordinates (London)
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    # Verify CRU data is available for test coordinates
    annual_temp = get_mean_annual_air_temperature(test_lat, test_lon)
    if annual_temp is None:
        # Skip test if CRU data not available
        import pytest

        pytest.skip("CRU data not available")

    # Create test YAML data with STEBBS configuration
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "OutdoorAirAnnualTemperature": {
                            "value": 999.0
                        },  # Wrong value to be updated
                        "OutdoorAirStartTemperature": {
                            "value": 999.0
                        },  # Will be updated with monthly temp
                    },
                },
                "initial_states": {},
            }
        ]
    }

    # Run adjustment
    updated_data, adjustments = adjust_surface_temperatures(yaml_data, start_date)

    # Check that OutdoorAirAnnualTemperature was updated
    updated_annual_temp = updated_data["sites"][0]["properties"]["stebbs"][
        "OutdoorAirAnnualTemperature"
    ]["value"]
    assert updated_annual_temp == annual_temp, (
        f"Expected {annual_temp}, got {updated_annual_temp}"
    )

    # Check that adjustment was recorded
    annual_temp_adjustments = [
        adj
        for adj in adjustments
        if adj.parameter == "stebbs.OutdoorAirAnnualTemperature"
    ]
    assert len(annual_temp_adjustments) == 1
    adj = annual_temp_adjustments[0]
    assert adj.old_value == "999.0"
    assert f"{annual_temp}" in adj.new_value
    assert "CRU annual mean" in adj.reason
    assert "1991-2020" in adj.reason


def test_phase_b_outdoor_air_annual_temperature_no_update_if_same():
    """Test that OutdoorAirAnnualTemperature is not updated if already correct."""
    from supy.data_model.validation.pipeline.phase_b import (
        adjust_surface_temperatures,
        get_mean_annual_air_temperature,
    )

    # Test coordinates
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    # Get correct annual temp
    annual_temp = get_mean_annual_air_temperature(test_lat, test_lon)
    if annual_temp is None:
        # Skip test if CRU data not available
        import pytest

        pytest.skip("CRU data not available")

    # Create test YAML with already-correct value
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "OutdoorAirAnnualTemperature": {
                            "value": annual_temp
                        },  # Already correct
                    },
                },
                "initial_states": {},
            }
        ]
    }

    # Run adjustment
    updated_data, adjustments = adjust_surface_temperatures(yaml_data, start_date)

    # Check that NO adjustment was made (value already correct)
    annual_temp_adjustments = [
        adj
        for adj in adjustments
        if adj.parameter == "stebbs.OutdoorAirAnnualTemperature"
    ]
    assert len(annual_temp_adjustments) == 0, (
        "Should not adjust if value already correct"
    )


def test_phase_b_outdoor_air_annual_temperature_missing_stebbs():
    """Test graceful handling when OutdoorAirAnnualTemperature is not in stebbs."""
    from supy.data_model.validation.pipeline.phase_b import adjust_surface_temperatures

    # Test coordinates
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    # Create test YAML without OutdoorAirAnnualTemperature
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "OutdoorAirStartTemperature": {
                            "value": 10.0
                        },  # Other param present
                        # OutdoorAirAnnualTemperature NOT present
                    },
                },
                "initial_states": {},
            }
        ]
    }

    # Run adjustment - should not crash
    updated_data, adjustments = adjust_surface_temperatures(yaml_data, start_date)

    # Check that no OutdoorAirAnnualTemperature adjustment was attempted
    annual_temp_adjustments = [
        adj
        for adj in adjustments
        if adj.parameter == "stebbs.OutdoorAirAnnualTemperature"
    ]
    assert len(annual_temp_adjustments) == 0


# =====================================================================
# Phase A: nlayer dimension validation tests
# =====================================================================


def test_nlayer_height_array_dimension_error():
    """Test that height array dimension mismatch is detected."""
    from supy.data_model.validation.pipeline.phase_a import validate_nlayer_dimensions
    import yaml

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    with open(config_path, "r") as f:
        user_data = yaml.safe_load(f)

    # Set nlayer=3 but truncate height array to 2 elements
    vl = user_data["sites"][0]["properties"]["vertical_layers"]
    vl["nlayer"]["value"] = 3
    if isinstance(vl["height"], dict) and "value" in vl["height"]:
        vl["height"]["value"] = vl["height"]["value"][:2]
    else:
        vl["height"] = vl["height"][:2]

    # Run validation
    modified_data, dimension_errors = validate_nlayer_dimensions(user_data, nlayer=3)

    # Check that error was recorded
    assert len(dimension_errors) > 0
    height_errors = [err for err in dimension_errors if "height" in err[0]]
    assert len(height_errors) == 1
    path, expected_len, actual_len, nulls_added = height_errors[0]
    assert expected_len == 4  # nlayer+1 = 3+1 = 4
    assert actual_len == 2
    assert nulls_added == 2  # Should have added 2 nulls

    # Check that array WAS modified with nulls
    height_arr = modified_data["sites"][0]["properties"]["vertical_layers"]["height"]
    if isinstance(height_arr, dict) and "value" in height_arr:
        height_arr = height_arr["value"]
    assert len(height_arr) == 4  # Now padded to 4
    # Check that nulls were added at the end
    assert height_arr[2] is None
    assert height_arr[3] is None


def test_nlayer_veg_frac_array_dimension_error():
    """Test that veg_frac array dimension mismatch is detected."""
    from supy.data_model.validation.pipeline.phase_a import validate_nlayer_dimensions
    import yaml

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    with open(config_path, "r") as f:
        user_data = yaml.safe_load(f)

    # Set nlayer=5 but truncate veg_frac to 3 elements
    vl = user_data["sites"][0]["properties"]["vertical_layers"]
    vl["nlayer"]["value"] = 5
    if isinstance(vl["veg_frac"], dict) and "value" in vl["veg_frac"]:
        vl["veg_frac"]["value"] = vl["veg_frac"]["value"][:3]
    else:
        vl["veg_frac"] = vl["veg_frac"][:3]

    # Run validation
    modified_data, dimension_errors = validate_nlayer_dimensions(user_data, nlayer=5)

    # Check that veg_frac error was detected
    veg_frac_errors = [err for err in dimension_errors if "veg_frac" in err[0]]
    assert len(veg_frac_errors) == 1
    path, expected_len, actual_len, nulls_added = veg_frac_errors[0]
    assert expected_len == 5
    assert actual_len == 3
    assert nulls_added == 2  # Should have added 2 nulls

    # Check that array WAS modified with nulls
    veg_frac_arr = modified_data["sites"][0]["properties"]["vertical_layers"][
        "veg_frac"
    ]
    if isinstance(veg_frac_arr, dict) and "value" in veg_frac_arr:
        veg_frac_arr = veg_frac_arr["value"]
    assert len(veg_frac_arr) == 5  # Now padded to 5
    assert veg_frac_arr[3] is None
    assert veg_frac_arr[4] is None


def test_nlayer_multiple_arrays_dimension_errors():
    """Test detecting dimension errors in multiple arrays."""
    from supy.data_model.validation.pipeline.phase_a import validate_nlayer_dimensions
    import yaml

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    with open(config_path, "r") as f:
        user_data = yaml.safe_load(f)

    # Set nlayer=4 but truncate multiple arrays
    vl = user_data["sites"][0]["properties"]["vertical_layers"]
    vl["nlayer"]["value"] = 4

    if isinstance(vl["height"], dict) and "value" in vl["height"]:
        vl["height"]["value"] = vl["height"]["value"][:2]
    else:
        vl["height"] = vl["height"][:2]

    if isinstance(vl["veg_frac"], dict) and "value" in vl["veg_frac"]:
        vl["veg_frac"]["value"] = vl["veg_frac"]["value"][:2]
    else:
        vl["veg_frac"] = vl["veg_frac"][:2]

    # Run validation
    modified_data, dimension_errors = validate_nlayer_dimensions(user_data, nlayer=4)

    # Check that both height and veg_frac errors were detected
    error_arrays = [err[0].split(".")[-1] for err in dimension_errors]
    assert "height" in error_arrays
    assert "veg_frac" in error_arrays
    assert len(dimension_errors) > 2


def test_nlayer_no_errors_when_correct():
    """Test that correctly sized arrays produce no errors."""
    from supy.data_model.validation.pipeline.phase_a import validate_nlayer_dimensions
    import yaml

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    with open(config_path, "r") as f:
        user_data = yaml.safe_load(f)

    # Detect nlayer from the config
    vl = user_data["sites"][0]["properties"]["vertical_layers"]
    if "nlayer" in vl:
        if isinstance(vl["nlayer"], dict) and "value" in vl["nlayer"]:
            nlayer = vl["nlayer"]["value"]
        else:
            nlayer = vl["nlayer"]
    else:
        nlayer = 3

    # Run validation with correct nlayer
    modified_data, dimension_errors = validate_nlayer_dimensions(
        user_data, nlayer=nlayer
    )

    # No errors should be found
    assert len(dimension_errors) == 0


def test_nlayer_roofs_walls_dimension_error_with_templates():
    """Test that roofs/walls arrays get proper null template structures."""
    from supy.data_model.validation.pipeline.phase_a import validate_nlayer_dimensions
    import yaml

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    with open(config_path, "r") as f:
        user_data = yaml.safe_load(f)

    # Set nlayer to 2 but keep only 1 roof and 1 wall element
    vl = user_data["sites"][0]["properties"]["vertical_layers"]
    vl["nlayer"]["value"] = 2
    vl["roofs"] = [vl["roofs"][0]]  # Keep only first element
    vl["walls"] = [vl["walls"][0]]  # Keep only first element

    # Run validation
    modified_data, dimension_errors = validate_nlayer_dimensions(user_data, nlayer=2)

    # Check that roofs and walls errors were detected
    roof_errors = [
        err
        for err in dimension_errors
        if "roofs" in err[0] and "vertical_layers" in err[0]
    ]
    wall_errors = [
        err
        for err in dimension_errors
        if "walls" in err[0] and "vertical_layers" in err[0]
    ]

    assert len(roof_errors) == 1
    assert len(wall_errors) == 1

    # Verify nulls were added
    path, expected_len, actual_len, nulls_added = roof_errors[0]
    assert expected_len == 2
    assert actual_len == 1
    assert nulls_added == 1

    # Check that roofs array now has 2 elements with proper structure
    vl_modified = modified_data["sites"][0]["properties"]["vertical_layers"]
    roofs_arr = vl_modified["roofs"]
    assert len(roofs_arr) == 2

    # Check that second roof element has null template structure
    roof2 = roofs_arr[1]
    assert roof2["alb"]["value"] is None
    assert roof2["emis"]["value"] is None
    assert roof2["statelimit"]["value"] is None

    # Check thermal_layers structure
    thermal = roof2["thermal_layers"]
    assert isinstance(thermal["dz"]["value"], list)
    assert len(thermal["dz"]["value"]) == 5  # Same length as reference
    assert all(v is None for v in thermal["dz"]["value"])
    assert all(v is None for v in thermal["k"]["value"])
    assert all(v is None for v in thermal["rho_cp"]["value"])

    # Check walls similarly
    walls_arr = vl_modified["walls"]
    assert len(walls_arr) == 2
    wall2 = walls_arr[1]
    assert wall2["alb"]["value"] is None
    assert wall2["emis"]["value"] is None


# =====================================================================
# Phase A: Forcing data validation tests
# =====================================================================


def test_forcing_validation_missing_file():
    """Test forcing validation detects missing forcing file."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml

    # Create test config with non-existent forcing file
    test_config = {
        "model": {"control": {"forcing_file": {"value": "nonexistent_forcing.txt"}}}
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, forcing_path = validate_forcing_data(config_path)

        # Should detect missing file
        assert len(forcing_errors) > 0
        assert any("not found" in err.lower() for err in forcing_errors)
        assert forcing_path == "nonexistent_forcing.txt"
    finally:
        Path(config_path).unlink()


def test_forcing_validation_valid_forcing_file():
    """Test forcing validation passes with valid forcing data."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml
    import pandas as pd

    # Create valid forcing file
    forcing_data = {
        "iy": [2011] * 5,
        "id": [1] * 5,
        "it": [0] * 5,
        "imin": [0, 5, 10, 15, 20],
        "qn": [100.0] * 5,
        "qh": [50.0] * 5,
        "qe": [30.0] * 5,
        "qs": [20.0] * 5,
        "qf": [10.0] * 5,
        "U": [2.5] * 5,
        "RH": [70.0] * 5,
        "Tair": [15.0] * 5,
        "pres": [100.0]
        * 5,  # Will be converted to 1000 hPa (valid range: 680-1300 hPa)
        "rain": [0.0] * 5,
        "kdown": [200.0] * 5,
        "snow": [0.0] * 5,
        "ldown": [300.0] * 5,
        "fcld": [0.5] * 5,
        "wuh": [10.0] * 5,
        "xsmd": [0.2] * 5,
        "lai": [2.0] * 5,
        "kdiff": [100.0] * 5,
        "kdir": [100.0] * 5,
        "wdir": [180.0] * 5,
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df = pd.DataFrame(forcing_data)
        df.to_csv(f, sep=" ", index=False)
        forcing_path = f.name

    # Create test config
    test_config = {"model": {"control": {"forcing_file": {"value": forcing_path}}}}

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, returned_path = validate_forcing_data(config_path)

        # Should have no errors for valid data
        assert len(forcing_errors) == 0
        assert returned_path == forcing_path
    finally:
        Path(config_path).unlink()
        Path(forcing_path).unlink()


def test_forcing_validation_invalid_ranges():
    """Test forcing validation detects out-of-range values."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml
    import pandas as pd

    # Create forcing file with invalid values
    forcing_data = {
        "iy": [2011] * 5,
        "id": [1] * 5,
        "it": [0] * 5,
        "imin": [0, 5, 10, 15, 20],
        "qn": [100.0] * 5,
        "qh": [50.0] * 5,
        "qe": [30.0] * 5,
        "qs": [20.0] * 5,
        "qf": [10.0] * 5,
        "U": [2.5] * 5,
        "RH": [70.0] * 5,
        "Tair": [15.0] * 5,
        "pres": [100.0, 100.0, 100.0, 100.0, 50.0],  # Bad: 50*10=500 hPa < 680
        "rain": [0.0, 0.0, 0.0, 0.0, -1.0],  # Bad: negative rain
        "kdown": [200.0, 200.0, 200.0, 200.0, 2000.0],  # Bad: 2000 > 1400
        "snow": [0.0] * 5,
        "ldown": [300.0] * 5,
        "fcld": [0.5] * 5,
        "wuh": [10.0] * 5,
        "xsmd": [0.2] * 5,
        "lai": [2.0] * 5,
        "kdiff": [100.0] * 5,
        "kdir": [100.0] * 5,
        "wdir": [180.0] * 5,
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df = pd.DataFrame(forcing_data)
        df.to_csv(f, sep=" ", index=False)
        forcing_path = f.name

    # Create test config
    test_config = {"model": {"control": {"forcing_file": {"value": forcing_path}}}}

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, returned_path = validate_forcing_data(config_path)

        # Should detect range violations
        assert len(forcing_errors) == 3  # pres, rain, kdown
        assert any("pres" in err for err in forcing_errors)
        assert any("rain" in err for err in forcing_errors)
        assert any("kdown" in err for err in forcing_errors)

        # Check error messages are properly formatted (no extra newlines)
        for err in forcing_errors:
            assert "\n" not in err  # Should be single-line messages
    finally:
        Path(config_path).unlink()
        Path(forcing_path).unlink()


def test_forcing_validation_no_forcing_file_in_config():
    """Test forcing validation handles missing forcing_file in config."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml

    # Create config without forcing_file
    test_config = {"model": {"control": {}}}

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, forcing_path = validate_forcing_data(config_path)

        # Should detect missing forcing_file path
        assert len(forcing_errors) > 0
        assert any("not found" in err for err in forcing_errors)
        assert forcing_path is None
    finally:
        Path(config_path).unlink()


def test_forcing_validation_report_integration():
    """Test forcing errors are properly integrated into Phase A report."""
    from supy.data_model.validation.pipeline.phase_a import create_analysis_report

    # Create test forcing errors
    forcing_errors = [
        "`pres` should be between [680, 1300] but 1 outliers are found at: [9]",
        "`rain` should be between [0, inf] but 1 outliers are found at: [9]",
    ]

    # Generate report with forcing errors
    report = create_analysis_report(
        missing_params=[],
        renamed_replacements=[],
        extra_params=[],
        uptodate_filename="test.yml",
        mode="public",
        phase="A",
        dimension_errors=[],
        forcing_errors=forcing_errors,
    )

    # Verify forcing errors appear in ACTION NEEDED section
    assert "## ACTION NEEDED" in report
    assert "Found (2) forcing data validation error(s):" in report
    assert "`pres` should be between [680, 1300]" in report
    assert "`rain` should be between [0, inf]" in report
    assert "Required fix: Review and correct forcing data file." in report
    assert (
        "Suggestion: You may want to plot the time series of your input data." in report
    )

    # Check single-line formatting
    lines = report.split("\n")
    for line in lines:
        if "`pres`" in line or "`rain`" in line:
            # Error message should be on one line (starting with --)
            if line.strip().startswith("--"):
                # Check it includes the complete message
                assert "[9]" in line  # Index should be on same line


def test_forcing_validation_can_be_disabled():
    """Test that forcing validation can be disabled via parameter."""
    from supy.data_model.validation.pipeline.phase_a import (
        annotate_missing_parameters,
    )

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    config_path = sample_data_dir / "sample_config.yml"

    # Create temporary output files
    with tempfile.NamedTemporaryFile(suffix=".yml", delete=False) as uptodate_f:
        uptodate_path = uptodate_f.name
    with tempfile.NamedTemporaryFile(suffix=".txt", delete=False) as report_f:
        report_path = report_f.name

    try:
        # Run Phase A validation with forcing="off"
        annotate_missing_parameters(
            user_file=str(config_path),
            standard_file=str(config_path),
            uptodate_file=uptodate_path,
            report_file=report_path,
            mode="public",
            phase="A",
            nlayer=3,
            forcing="off",
        )

        # Verify report does not contain forcing validation errors
        with open(report_path, "r") as f:
            report_content = f.read()

        assert "forcing data validation error" not in report_content.lower()
    finally:
        # Cleanup temporary files
        Path(uptodate_path).unlink(missing_ok=True)
        Path(report_path).unlink(missing_ok=True)


def test_forcing_validation_line_number_accuracy():
    """Test that line numbers in error messages match actual file line numbers."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml
    import pandas as pd

    # Create forcing file with invalid value at specific rows
    # Row 0 (DataFrame index) = Line 2 (file line: header=1, first data=2)
    # Row 5 (DataFrame index) = Line 7 (file line)
    forcing_data = {
        "iy": [2011] * 10,
        "id": [1] * 10,
        "it": [0] * 10,
        "imin": list(range(0, 50, 5)),
        "qn": [100.0] * 10,
        "qh": [50.0] * 10,
        "qe": [30.0] * 10,
        "qs": [20.0] * 10,
        "qf": [10.0] * 10,
        "U": [2.5] * 10,
        "RH": [70.0] * 10,
        "Tair": [15.0] * 10,
        "pres": [100.0] * 10,
        "rain": [0.0, 0.0, 0.0, 0.0, 0.0, -5.0, 0.0, 0.0, 0.0, 0.0],  # Error at index 5
        "kdown": [200.0] * 10,
        "snow": [0.0] * 10,
        "ldown": [300.0] * 10,
        "fcld": [0.5] * 10,
        "wuh": [10.0] * 10,
        "xsmd": [0.2] * 10,
        "lai": [2.0] * 10,
        "kdiff": [100.0] * 10,
        "kdir": [100.0] * 10,
        "wdir": [180.0] * 10,
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df = pd.DataFrame(forcing_data)
        df.to_csv(f, sep=" ", index=False)
        forcing_path = f.name

    # Create test config
    test_config = {"model": {"control": {"forcing_file": {"value": forcing_path}}}}

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, returned_path = validate_forcing_data(config_path)

        # Should detect rain error at index 5 -> line 7 (header=1, index 0=line 2, so index 5=line 7)
        assert len(forcing_errors) > 0
        rain_errors = [err for err in forcing_errors if "rain" in err.lower()]
        assert len(rain_errors) > 0

        # Verify line number in error message
        # DataFrame index 5 should map to file line 7 (1 header + 6 data rows)
        rain_error = rain_errors[0]
        assert "[7]" in rain_error, f"Expected line 7, but got: {rain_error}"

        # Verify filename is included in error message
        filename = Path(forcing_path).name
        assert filename in rain_error, (
            f"Expected filename '{filename}' in error: {rain_error}"
        )

    finally:
        Path(config_path).unlink()
        Path(forcing_path).unlink()


def test_forcing_validation_refvalue_format():
    """Test forcing validation handles RefValue format correctly."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml
    import pandas as pd

    # Create valid forcing file
    forcing_data = {
        "iy": [2011] * 3,
        "id": [1] * 3,
        "it": [0] * 3,
        "imin": [0, 5, 10],
        "qn": [100.0] * 3,
        "qh": [50.0] * 3,
        "qe": [30.0] * 3,
        "qs": [20.0] * 3,
        "qf": [10.0] * 3,
        "U": [2.5] * 3,
        "RH": [70.0] * 3,
        "Tair": [15.0] * 3,
        "pres": [100.0] * 3,
        "rain": [0.0] * 3,
        "kdown": [200.0] * 3,
        "snow": [0.0] * 3,
        "ldown": [300.0] * 3,
        "fcld": [0.5] * 3,
        "wuh": [10.0] * 3,
        "xsmd": [0.2] * 3,
        "lai": [2.0] * 3,
        "kdiff": [100.0] * 3,
        "kdir": [100.0] * 3,
        "wdir": [180.0] * 3,
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df = pd.DataFrame(forcing_data)
        df.to_csv(f, sep=" ", index=False)
        forcing_path = f.name

    # Create test config with RefValue format (dict with "value" key)
    test_config = {
        "model": {
            "control": {
                "forcing_file": {
                    "value": forcing_path,
                    "source": "user",
                    "description": "Test forcing file",
                }
            }
        }
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, returned_path = validate_forcing_data(config_path)

        # Should have no errors for valid data
        assert len(forcing_errors) == 0
        assert returned_path == forcing_path

    finally:
        Path(config_path).unlink()
        Path(forcing_path).unlink()


def test_forcing_validation_multiple_files():
    """Test that all forcing files in a list are validated."""
    from supy.data_model.validation.pipeline.phase_a import validate_forcing_data
    import yaml
    import pandas as pd

    # Create first forcing file (valid)
    forcing_data1 = {
        "iy": [2011] * 3,
        "id": [1] * 3,
        "it": [0] * 3,
        "imin": [0, 5, 10],
        "qn": [100.0] * 3,
        "qh": [50.0] * 3,
        "qe": [30.0] * 3,
        "qs": [20.0] * 3,
        "qf": [10.0] * 3,
        "U": [2.5] * 3,
        "RH": [70.0] * 3,
        "Tair": [15.0] * 3,
        "pres": [100.0] * 3,
        "rain": [0.0] * 3,
        "kdown": [200.0] * 3,
        "snow": [0.0] * 3,
        "ldown": [300.0] * 3,
        "fcld": [0.5] * 3,
        "wuh": [10.0] * 3,
        "xsmd": [0.2] * 3,
        "lai": [2.0] * 3,
        "kdiff": [100.0] * 3,
        "kdir": [100.0] * 3,
        "wdir": [180.0] * 3,
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df1 = pd.DataFrame(forcing_data1)
        df1.to_csv(f, sep=" ", index=False)
        forcing_path1 = f.name

    # Create second forcing file (invalid - negative rain)
    forcing_data2 = forcing_data1.copy()
    forcing_data2["rain"] = [-1.0, -2.0, -3.0]

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        df2 = pd.DataFrame(forcing_data2)
        df2.to_csv(f, sep=" ", index=False)
        forcing_path2 = f.name

    # Create test config with list of forcing files
    test_config = {
        "model": {"control": {"forcing_file": [forcing_path1, forcing_path2]}}
    }

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(test_config, f)
        config_path = f.name

    try:
        forcing_errors, returned_paths = validate_forcing_data(config_path)

        # Should detect errors in second file only
        assert len(forcing_errors) > 0

        # Check that errors mention the second file
        file2_name = Path(forcing_path2).name
        file2_errors = [err for err in forcing_errors if file2_name in err]
        assert len(file2_errors) > 0, "Errors from second file should be detected"

        # Verify returned paths is a list
        assert returned_paths == [forcing_path1, forcing_path2]

    finally:
        Path(config_path).unlink()
        Path(forcing_path1).unlink()
        Path(forcing_path2).unlink()


def test_forcing_validation_cli_integration():
    """Test CLI integration: forcing validation can be enabled/disabled via --forcing flag."""
    import subprocess
    import yaml
    import pandas as pd

    sample_data_dir = Path(sp.__file__).parent / "sample_data"
    sample_config = sample_data_dir / "sample_config.yml"

    # Read sample config and get forcing file path
    with open(sample_config, "r") as f:
        config_data = yaml.safe_load(f)

    # Create a temporary config with invalid forcing data
    forcing_data = {
        "iy": [2011] * 3,
        "id": [1] * 3,
        "it": [0] * 3,
        "imin": [0, 5, 10],
        "qn": [100.0] * 3,
        "qh": [50.0] * 3,
        "qe": [30.0] * 3,
        "qs": [20.0] * 3,
        "qf": [10.0] * 3,
        "U": [2.5] * 3,
        "RH": [70.0] * 3,
        "Tair": [15.0] * 3,
        "pres": [100.0] * 3,
        "rain": [-1.0, -2.0, -3.0],  # Invalid: negative rain
        "kdown": [200.0] * 3,
        "snow": [0.0] * 3,
        "ldown": [300.0] * 3,
        "fcld": [0.5] * 3,
        "wuh": [10.0] * 3,
        "xsmd": [0.2] * 3,
        "lai": [2.0] * 3,
        "kdiff": [100.0] * 3,
        "kdir": [100.0] * 3,
        "wdir": [180.0] * 3,
    }

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir_path = Path(tmpdir)

        # Create forcing file with invalid data in tmpdir
        bad_forcing_path = tmpdir_path / "forcing_bad.txt"
        df = pd.DataFrame(forcing_data)
        df.to_csv(bad_forcing_path, sep=" ", index=False)

        # Create test config with bad forcing file in tmpdir
        test_config_data = config_data.copy()
        test_config_data["model"]["control"]["forcing_file"] = {
            "value": str(bad_forcing_path)
        }

        test_config_path = tmpdir_path / "test_config.yml"
        with open(test_config_path, "w") as f:
            yaml.dump(test_config_data, f)

        try:
            # Test 1: Default behavior (forcing validation enabled)
            result = subprocess.run(
                ["suews-validate", str(test_config_path)],
                capture_output=True,
                text=True,
            )

            # Should detect forcing errors in report
            report_files = list(tmpdir_path.glob("report_*.txt"))
            assert len(report_files) == 1, (
                f"Expected 1 report file, found {len(report_files)}: {list(tmpdir_path.iterdir())}"
            )
            with open(report_files[0], "r") as f:
                report_content = f.read()
            assert "forcing data validation error" in report_content.lower(), (
                f"Expected forcing validation error in report, but got:\n{report_content[:500]}"
            )

            # Clean up for next test
            for f in tmpdir_path.glob("report_*.txt"):
                f.unlink()
            for f in tmpdir_path.glob("updated_*.yml"):
                f.unlink()

            # Test 2: Disable forcing validation with --forcing off
            result = subprocess.run(
                ["suews-validate", "--forcing", "off", str(test_config_path)],
                capture_output=True,
                text=True,
            )

            # Should NOT detect forcing errors
            report_files = list(tmpdir_path.glob("report_*.txt"))
            assert len(report_files) == 1, (
                f"Expected 1 report file, found {len(report_files)}"
            )
            with open(report_files[0], "r") as f:
                report_content = f.read()
            assert "forcing data validation error" not in report_content.lower(), (
                f"Expected no forcing validation errors when disabled, but got:\n{report_content[:500]}"
            )

        finally:
            # Cleanup happens automatically with TemporaryDirectory context manager
            pass
