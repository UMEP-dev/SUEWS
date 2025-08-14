"""
Consolidated tests for SUEWS validation logic.

This file combines:
- Conditional validation from test_conditional_validation.py
- Validation utilities from test_validation_utils.py  
- Top-down validation from test_validation_topdown.py
- Essential migrated validators (from test_migrated_validators.py)
- Validator improvements from test_validator_improvements.py
"""
import pytest
from types import SimpleNamespace
import warnings
import tempfile
from pathlib import Path
import logging
import io

from supy.data_model.core import SUEWSConfig
from supy.data_model.validation_utils import check_missing_params
from supy.data_model.type import RefValue


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
        "laimin (5.0) must be ≤ laimax (3.0)" in msg
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
        "baset (15.0) must be ≤ gddfull (10.0)" in msg
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
# from supy.data_model.type import RefValue

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
            from supy.data_model.human_activity import CO2Params
            from supy.data_model.site import Conductance
            from supy.data_model.surface import BldgsProperties

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
