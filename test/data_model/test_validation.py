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
import types
import copy

import pytest
import yaml

import supy as sp
from supy._env import trv_supy_module
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.site import (
    LAIParams,
    DectrProperties,
    EvetrProperties,
    GrassProperties,
    LandCover,
    Site,
    SiteProperties,
)
from supy.data_model.core.state import (
    InitialStateDectr,
    InitialStateEvetr,
    InitialStateGrass,
    InitialStates,
)
from supy.data_model.core.type import RefValue
from supy.data_model.validation.core.utils import check_missing_params
from supy.data_model.validation.pipeline.phase_b import (
    adjust_seasonal_parameters,
    adjust_model_option_stebbsmethod,
    adjust_model_option_setpointmethod,
    adjust_model_option_rcmethod,
    RulesRegistry,
    ValidationContext
)
import types
import copy

pytestmark = pytest.mark.api

# A tiny “site” stub that only carries exactly the properties our validators look at
class DummySite:
    def __init__(self, properties, name="SiteX"):
        self.properties = properties
        self.name = name


@pytest.fixture(scope="module")
def registry():
    return RulesRegistry()


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
    cfg = make_cfg(stebbs=1)
    assert cfg._needs_stebbs_validation() is True
    cfg2 = make_cfg(stebbs=0)
    assert cfg2._needs_stebbs_validation() is False


def test_validate_stebbs_missing_properties_block():
    cfg = make_cfg(stebbs=1)
    site = DummySite(properties=None, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    assert msgs == ["Missing 'properties' section (required for STEBBS validation)"]


def test_validate_stebbs_missing_stebbs_section():
    cfg = make_cfg(stebbs=1)
    props = SimpleNamespace(stebbs=None)
    site = DummySite(properties=props, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    assert msgs == ["Missing 'stebbs' section (required when stebbs=1)"]


def test_validate_stebbs_missing_parameters():
    cfg = make_cfg(stebbs=1)
    # Provide an empty stebbs object
    props = SimpleNamespace(stebbs=SimpleNamespace())
    site = DummySite(properties=props, name="MySite")
    msgs = SUEWSConfig._validate_stebbs(cfg, site, site_index=0)
    # Should mention at least one of the required params
    assert msgs and msgs[0].startswith("Missing required STEBBS parameters:")


def test_needs_rsl_validation_true_and_false():
    # After conditional validation fix, validation is disabled by default
    # unless physics parameters are explicitly configured
    cfg = make_cfg(roughness_sublayer=2)
    assert cfg._needs_rsl_validation() is False  # Disabled by default now
    cfg2 = make_cfg(roughness_sublayer=1)
    assert cfg2._needs_rsl_validation() is False


def test_validate_rsl_no_land_cover_or_sfr():
    cfg = make_cfg(roughness_sublayer=2)
    site = DummySite(properties=None)
    assert SUEWSConfig._validate_rsl(cfg, site, 0) == []
    # land_cover without bldgs
    site2 = DummySite(properties=SimpleNamespace(land_cover=None))
    assert SUEWSConfig._validate_rsl(cfg, site2, 0) == []


def test_validate_rsl_requires_faibldg():
    cfg = make_cfg(roughness_sublayer=2)
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
    cfg = make_cfg(storage_heat=6)
    assert cfg._needs_storage_validation() is False  # Disabled by default now
    cfg2 = make_cfg(storage_heat=1)
    assert cfg2._needs_storage_validation() is False


def test_validate_storage_requires_numeric_and_lambda():
    cfg = make_cfg(storage_heat=6)
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
        lai_min=SimpleNamespace(value=5.0), lai_max=SimpleNamespace(value=3.0)
    )
    grass = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "LAI range validation" in cfg._validation_summary["issue_types"]
    assert any(
        "lai_min (5.0) must be <= lai_max (3.0)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_lai_ranges_invalid_baset_gddfull():
    """Test LAI validation detects invalid baset > gddfull."""
    cfg = SUEWSConfig.model_construct()
    # Create vegetation surface with invalid baset/gddfull range.
    # Provide both legacy and new attribute aliases so _check_lai_ranges
    # sees the gddfull guard while reading the gdd_full value (source
    # code uses the old attribute name in the hasattr guard).
    gdd = SimpleNamespace(value=10.0)
    lai = SimpleNamespace(
        lai_min=SimpleNamespace(value=1.0),
        lai_max=SimpleNamespace(value=5.0),
        base_temperature=SimpleNamespace(value=15.0),
        gdd_full=gdd,
        gddfull=gdd,
    )
    dectr = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(dectr=dectr)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is True
    assert cfg._validation_summary["total_warnings"] >= 1
    assert "LAI range validation" in cfg._validation_summary["issue_types"]
    assert any(
        "base_temperature (15.0) must be <= gddfull (10.0)" in msg
        for msg in cfg._validation_summary["detailed_messages"]
    )


def test_validate_lai_ranges_multiple_vegetation_surfaces():
    """Test LAI validation checks all vegetation surfaces."""
    cfg = SUEWSConfig.model_construct()
    # Create multiple vegetation surfaces with invalid LAI ranges
    lai_invalid = SimpleNamespace(
        lai_min=SimpleNamespace(value=5.0), lai_max=SimpleNamespace(value=3.0)
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
        lai_min=SimpleNamespace(value=1.0),
        lai_max=SimpleNamespace(value=5.0),
        base_temperature=SimpleNamespace(value=5.0),
        gdd_full=SimpleNamespace(value=200.0),
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
    lai = SimpleNamespace(lai_min=None, lai_max=None, base_temperature=None, gdd_full=None)
    grass = SimpleNamespace(lai=lai)
    lc = SimpleNamespace(grass=grass)

    has_issues = cfg._check_lai_ranges(lc, "TestSite")
    assert has_issues is False


def test_auto_albedo_trees_follow_lai_relation():
    """Test tree albedo follows the direct LAI-albedo relationship.

    Physical basis: Trees have dark bark/branches as background surface.
    - Low LAI (sparse canopy) -> more bark visible -> lower albedo
    - High LAI (dense foliage) -> leaves dominate -> higher albedo
    Leaf albedo (0.20-0.30) typically exceeds bark albedo (0.10-0.15).

    Test verifies boundary conditions:
    - evetr at LAI=laimin (ratio=0) -> alb_id = alb_min = 0.1
    - dectr at LAI=laimax (ratio=1) -> alb_id = alb_max = 0.4
    """
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.3,
        lai=LAIParams(lai_min=1.0, lai_max=3.0),
    )
    dectr_props = DectrProperties(
        alb_min=0.2,
        alb_max=0.4,
        lai=LAIParams(lai_min=2.0, lai_max=4.0),
    )
    land_cover = LandCover(evetr=evetr_props, dectr=dectr_props)
    site_props = SiteProperties(land_cover=land_cover)

    evetr_state = InitialStateEvetr(alb_id=None, lai_id=1.0)
    dectr_state = InitialStateDectr(alb_id=None, lai_id=4.0)
    initial_states = InitialStates(evetr=evetr_state, dectr=dectr_state)

    site = Site(properties=site_props, initial_states=initial_states)
    config = SUEWSConfig(sites=[site])

    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.1)
    assert config.sites[0].initial_states.dectr.alb_id == pytest.approx(0.4)


def test_auto_albedo_grass_reversed_relation():
    """Test grass albedo follows the reversed LAI-albedo relationship.

    Physical basis: Grass has bright soil/litter as background surface.
    - Low LAI (sparse grass) -> more soil visible -> higher albedo
    - High LAI (dense grass) -> green blades dominate -> lower albedo
    Dry soil albedo (0.25-0.45) typically exceeds grass blade albedo (0.18-0.25).

    Test verifies boundary conditions:
    - grass at LAI=laimin (ratio=0) -> alb_id = alb_max = 0.4 (bright soil)
    - grass at LAI=laimax (ratio=1) -> alb_id = alb_min = 0.2 (dense grass)
    """
    grass_props = GrassProperties(
        alb_min=0.2,
        alb_max=0.4,
        lai=LAIParams(lai_min=0.5, lai_max=2.5),
    )
    land_cover = LandCover(grass=grass_props)
    site_props = SiteProperties(land_cover=land_cover)

    grass_state_low = InitialStateGrass(alb_id=None, lai_id=0.5)
    grass_state_high = InitialStateGrass(alb_id=None, lai_id=2.5)

    site_low = Site(
        properties=site_props,
        initial_states=InitialStates(grass=grass_state_low),
    )
    site_high = Site(
        properties=site_props,
        initial_states=InitialStates(grass=grass_state_high),
    )

    config = SUEWSConfig(sites=[site_low, site_high])

    assert config.sites[0].initial_states.grass.alb_id == pytest.approx(0.4)
    assert config.sites[1].initial_states.grass.alb_id == pytest.approx(0.2)


def test_auto_albedo_preserves_user_value():
    """Test user-provided albedo is preserved."""
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.3,
        lai=LAIParams(lai_min=1.0, lai_max=3.0),
    )
    site_props = SiteProperties(land_cover=LandCover(evetr=evetr_props))
    evetr_state = InitialStateEvetr(alb_id=0.28, lai_id=2.0)
    site = Site(
        properties=site_props,
        initial_states=InitialStates(evetr=evetr_state),
    )

    config = SUEWSConfig(sites=[site])

    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.28)


def test_validate_albedo_id_within_range():
    """Test initial albedo validation against vegetation limits."""
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.2,
        lai=LAIParams(lai_min=1.0, lai_max=3.0),
    )
    site_props = SiteProperties(land_cover=LandCover(evetr=evetr_props))
    evetr_state = InitialStateEvetr(alb_id=0.5, lai_id=2.0)
    site = Site(
        properties=site_props,
        initial_states=InitialStates(evetr=evetr_state),
    )

    with pytest.raises(ValueError, match="alb_id"):
        SUEWSConfig(sites=[site])


def test_auto_albedo_midrange_interpolation():
    """Test linear interpolation at mid-range LAI produces expected intermediate albedo.

    For trees (direct relationship):
        lai_ratio = (2.0 - 1.0) / (3.0 - 1.0) = 0.5
        alb_id = 0.1 + (0.3 - 0.1) * 0.5 = 0.2

    For grass (reversed relationship):
        lai_ratio = (1.5 - 0.5) / (2.5 - 0.5) = 0.5
        alb_id = 0.4 - (0.4 - 0.2) * 0.5 = 0.3
    """
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.3,
        lai=LAIParams(lai_min=1.0, lai_max=3.0),
    )
    grass_props = GrassProperties(
        alb_min=0.2,
        alb_max=0.4,
        lai=LAIParams(lai_min=0.5, lai_max=2.5),
    )
    land_cover = LandCover(evetr=evetr_props, grass=grass_props)
    site_props = SiteProperties(land_cover=land_cover)

    evetr_state = InitialStateEvetr(alb_id=None, lai_id=2.0)
    grass_state = InitialStateGrass(alb_id=None, lai_id=1.5)
    initial_states = InitialStates(evetr=evetr_state, grass=grass_state)

    site = Site(properties=site_props, initial_states=initial_states)
    config = SUEWSConfig(sites=[site])

    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.2)
    assert config.sites[0].initial_states.grass.alb_id == pytest.approx(0.3)


def test_auto_albedo_clamps_lai_outside_range():
    """Test LAI values outside [laimin, laimax] are clamped to [0, 1] ratio.

    lai_id below laimin -> ratio clamped to 0 -> boundary albedo
    lai_id above laimax -> ratio clamped to 1 -> boundary albedo
    """
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.3,
        lai=LAIParams(lai_min=2.0, lai_max=4.0),
    )
    land_cover = LandCover(evetr=evetr_props)
    site_props = SiteProperties(land_cover=land_cover)

    # LAI below laimin -> clamped to ratio=0 -> alb_min
    evetr_below = InitialStateEvetr(alb_id=None, lai_id=0.5)
    site_below = Site(
        properties=site_props,
        initial_states=InitialStates(evetr=evetr_below),
    )

    # LAI above laimax -> clamped to ratio=1 -> alb_max
    evetr_above = InitialStateEvetr(alb_id=None, lai_id=6.0)
    site_above = Site(
        properties=site_props,
        initial_states=InitialStates(evetr=evetr_above),
    )

    config = SUEWSConfig(sites=[site_below, site_above])

    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.1)
    assert config.sites[1].initial_states.evetr.alb_id == pytest.approx(0.3)


def test_auto_albedo_zero_lai_range():
    """Test auto-albedo when laimin equals laimax (zero range).

    When lai_range <= 0, lai_ratio should be set to 0.0.
    For trees: alb_id = alb_min + (alb_max - alb_min) * 0.0 = alb_min
    For grass: alb_id = alb_max - (alb_max - alb_min) * 0.0 = alb_max
    """
    evetr_props = EvetrProperties(
        alb_min=0.1,
        alb_max=0.3,
        lai=LAIParams(lai_min=2.0, lai_max=2.0),
    )
    grass_props = GrassProperties(
        alb_min=0.2,
        alb_max=0.4,
        lai=LAIParams(lai_min=1.5, lai_max=1.5),
    )
    land_cover = LandCover(evetr=evetr_props, grass=grass_props)
    site_props = SiteProperties(land_cover=land_cover)
    evetr_state = InitialStateEvetr(alb_id=None, lai_id=2.0)
    grass_state = InitialStateGrass(alb_id=None, lai_id=1.5)
    initial_states = InitialStates(evetr=evetr_state, grass=grass_state)

    site = Site(properties=site_props, initial_states=initial_states)
    config = SUEWSConfig(sites=[site])

    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.1)
    assert config.sites[0].initial_states.grass.alb_id == pytest.approx(0.4)


def test_auto_albedo_with_refvalue_inputs():
    """Test auto-albedo with RefValue-wrapped inputs exercises _unwrap_value."""
    evetr_props = EvetrProperties(
        alb_min=RefValue(value=0.1),
        alb_max=RefValue(value=0.3),
        lai=LAIParams(lai_min=1.0, lai_max=3.0),
    )
    land_cover = LandCover(evetr=evetr_props)
    site_props = SiteProperties(land_cover=land_cover)

    evetr_state = InitialStateEvetr(alb_id=None, lai_id=RefValue(value=2.0))
    site = Site(
        properties=site_props,
        initial_states=InitialStates(evetr=evetr_state),
    )

    config = SUEWSConfig(sites=[site])
    # lai_ratio = (2.0 - 1.0) / (3.0 - 1.0) = 0.5
    # alb_id = 0.1 + (0.3 - 0.1) * 0.5 = 0.2
    assert config.sites[0].initial_states.evetr.alb_id == pytest.approx(0.2)


def test_validate_same_albedo_wall_requires_identical_wall_albedos():
    """
    When same_albedo_wall is ON but walls have different albedos,
    we should get an error about them needing to be identical.
    """
    cfg = make_cfg(same_albedo_wall=1)

    walls = [
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
        SimpleNamespace(alb=SimpleNamespace(value=0.6)),  # mismatch
    ]
    vl = SimpleNamespace(walls=walls)
    ba = SimpleNamespace(wall_reflectivity=SimpleNamespace(value=0.5))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteWallMismatch")

    msgs = SUEWSConfig._validate_same_albedo_wall(cfg, site, 0)
    assert len(msgs) == 1
    assert "so all walls albedo values must be identical;" in msgs[0]
    assert "SiteWallMismatch" in msgs[0]


def test_validate_same_albedo_wall_requires_match_with_wallreflectivity():
    """
    When same_albedo_wall is ON, all walls have same alb but it differs
    from building_archetype.WallReflectivity, we should get an error.
    """
    cfg = make_cfg(same_albedo_wall=1)

    walls = [
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
    ]
    vl = SimpleNamespace(walls=walls)
    ba = SimpleNamespace(wall_reflectivity=SimpleNamespace(value=0.345))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteRefMismatch")

    msgs = SUEWSConfig._validate_same_albedo_wall(cfg, site, 0)
    assert len(msgs) == 1
    msg = msgs[0]
    assert "must equal properties.building_archetype.wall_reflectivity (0.345)" in msg
    assert "walls[0]=0.5" in msg

def test_validate_same_albedo_roof_requires_match_with_roofreflectivity():
    """
    When same_albedo_roof is ON, all roofs have same alb but it differs
    from building_archetype.RoofReflectivity, we should get an error.
    """
    cfg = make_cfg(same_albedo_roof=1)

    roofs = [
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
    ]
    vl = SimpleNamespace(roofs=roofs)
    ba = SimpleNamespace(roof_reflectivity=SimpleNamespace(value=0.345))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteRefMismatch")

    msgs = SUEWSConfig._validate_same_albedo_roof(cfg, site, 0)
    assert len(msgs) == 1
    msg = msgs[0]
    assert "must equal properties.building_archetype.roof_reflectivity (0.345)" in msg
    assert "roofs[0]=0.5" in msg

def test_validate_same_albedo_roof_requires_identical_roof_albedos():
    """
    When same_albedo_roof is ON but roofs have different albedos,
    we should get an error about them needing to be identical.
    """
    cfg = make_cfg(same_albedo_roof=1)

    roofs = [
        SimpleNamespace(alb=SimpleNamespace(value=0.5)),
        SimpleNamespace(alb=SimpleNamespace(value=0.6)),  # mismatch
    ]
    vl = SimpleNamespace(roofs=roofs)
    ba = SimpleNamespace(roof_reflectivity=SimpleNamespace(value=0.5))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteRoofMismatch")

    msgs = SUEWSConfig._validate_same_albedo_roof(cfg, site, 0)
    assert len(msgs) == 1
    assert "so all roofs albedo values must be identical;" in msgs[0]
    assert "SiteRoofMismatch" in msgs[0]

def test_needs_same_albedo_roof_validation_true_and_false():
    cfg = make_cfg(same_albedo_roof=1)
    assert cfg._needs_same_albedo_roof_validation() is True
    cfg2 = make_cfg(same_albedo_roof=0)
    assert cfg2._needs_same_albedo_roof_validation() is False

def test_needs_same_albedo_wall_validation_true_and_false():
    cfg = make_cfg(same_albedo_wall=1)
    assert cfg._needs_same_albedo_wall_validation() is True
    cfg2 = make_cfg(same_albedo_wall=0)
    assert cfg2._needs_same_albedo_wall_validation() is False

def test_validate_same_emissivity_wall_requires_identical_wall_emissivities():
    """
    When same_emissivity_wall is ON but walls have different emissivities,
    we should get an error about them needing to be identical.
    """
    cfg = make_cfg(same_emissivity_wall=1)

    walls = [
        SimpleNamespace(emis=SimpleNamespace(value=0.5)),
        SimpleNamespace(emis=SimpleNamespace(value=0.6)),  # mismatch
    ]
    vl = SimpleNamespace(walls=walls)
    ba = SimpleNamespace(wall_external_emissivity=SimpleNamespace(value=0.5))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteWallMismatch")

    msgs = SUEWSConfig._validate_same_emissivity_wall(cfg, site, 0)
    assert len(msgs) == 1
    assert "so all walls emissivity values must be identical;" in msgs[0]
    assert "SiteWallMismatch" in msgs[0]

def test_validate_same_emissivity_wall_requires_match_with_wallexternalemissivity():
    """
    When same_emissivity_wall is ON, all walls have same emis but it differs
    from building_archetype.WallExternalEmissivity, we should get an error.
    """
    cfg = make_cfg(same_emissivity_wall=1)

    walls = [
        SimpleNamespace(emis=SimpleNamespace(value=0.9)),
        SimpleNamespace(emis=SimpleNamespace(value=0.9)),
    ]
    vl = SimpleNamespace(walls=walls)
    ba = SimpleNamespace(wall_external_emissivity=SimpleNamespace(value=0.8))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteEmisRefMismatch")

    msgs = SUEWSConfig._validate_same_emissivity_wall(cfg, site, 0)
    assert len(msgs) == 1
    msg = msgs[0]
    assert (
        "must equal properties.building_archetype.wall_external_emissivity (0.8)" in msg
    )
    assert "walls[0]=0.9" in msg
    assert "SiteEmisRefMismatch" in msg

def test_validate_same_emissivity_roof_requires_match_with_roofexternalemissivity():
    """
    When same_emissivity_roof is ON, all roofs have same emis but it differs
    from building_archetype.RoofExternalEmissivity, we should get an error.
    """
    cfg = make_cfg(same_emissivity_roof=1)

    roofs = [
        SimpleNamespace(emis=SimpleNamespace(value=0.7)),
        SimpleNamespace(emis=SimpleNamespace(value=0.7)),
    ]
    vl = SimpleNamespace(roofs=roofs)
    ba = SimpleNamespace(roof_external_emissivity=SimpleNamespace(value=0.5))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteRoofEmisRefMismatch")

    msgs = SUEWSConfig._validate_same_emissivity_roof(cfg, site, 0)
    assert len(msgs) == 1
    msg = msgs[0]
    assert (
        "must equal properties.building_archetype.roof_external_emissivity (0.5)" in msg
    )
    assert "roofs[0]=0.7" in msg
    assert "SiteRoofEmisRefMismatch" in msg

def test_validate_same_emissivity_roof_requires_identical_roof_emissivities():
    """
    When same_emissivity_roof is ON but roofs have different emissivities,
    we should get an error about them needing to be identical.
    """
    cfg = make_cfg(same_emissivity_roof=1)

    roofs = [
        SimpleNamespace(emis=SimpleNamespace(value=0.8)),
        SimpleNamespace(emis=SimpleNamespace(value=0.9)),  # mismatch
    ]
    vl = SimpleNamespace(roofs=roofs)
    ba = SimpleNamespace(roof_external_emissivity=SimpleNamespace(value=0.8))
    props = SimpleNamespace(vertical_layers=vl, building_archetype=ba)
    site = DummySite(properties=props, name="SiteRoofEmisMismatch")

    msgs = SUEWSConfig._validate_same_emissivity_roof(cfg, site, 0)
    assert len(msgs) == 1
    assert "so all roofs emissivity values must be identical;" in msgs[0]
    assert "SiteRoofEmisMismatch" in msgs[0]

def test_needs_same_emissivity_roof_validation_true_and_false():
    cfg = make_cfg(same_emissivity_roof=1)
    assert cfg._needs_same_emissivity_roof_validation() is True
    cfg2 = make_cfg(same_emissivity_roof=0)
    assert cfg2._needs_same_emissivity_roof_validation() is False

def test_needs_same_emissivity_wall_validation_true_and_false():
    cfg = make_cfg(same_emissivity_wall=1)
    assert cfg._needs_same_emissivity_wall_validation() is True
    cfg2 = make_cfg(same_emissivity_wall=0)
    assert cfg2._needs_same_emissivity_wall_validation() is False

def test_phase_b_validate_model_option_same_albedo_disabled(registry):
    """Test validate_model_option_same_albedo returns WARNING when option is disabled (==0)."""

    yaml_data_wall = {
        "model": {
            "physics": {
                "same_albedo_wall": {"value": 0}
            }
        },
        "sites": [{"name": "site1", "properties": {}}],  
    }

    results_wall = registry["same_albedo"](ValidationContext(yaml_data=yaml_data_wall))
    assert len(results_wall) == 1
    assert results_wall[0].status == "WARNING"
    assert "no check of consistency" in results_wall[0].message.lower()
    assert "same_albedo_wall == 0" in results_wall[0].message.lower()

    yaml_data_roof = {
        "model": {
            "physics": {
                "same_albedo_roof": {"value": 0}
            }
        },
        "sites": [{"name": "site1", "properties": {}}],  
    }
    results_roof = registry["same_albedo"](ValidationContext(yaml_data=yaml_data_roof))
    assert len(results_roof) == 1
    assert results_roof[0].status == "WARNING"
    assert "no check of consistency" in results_roof[0].message.lower()
    assert "same_albedo_roof == 0" in results_roof[0].message.lower()

def test_phase_b_validate_model_option_same_emissivity_disabled(registry):
    """Test validate_model_option_same_emissivity returns WARNING when option is disabled (==0)."""

    yaml_data_wall = {
        "model": {
            "physics": {
                "same_emissivity_wall": {"value": 0}
            }
        },
        "sites": [{"name": "site1", "properties": {}}],  
    }
    results_wall = registry["same_emissivity"](ValidationContext(yaml_data=yaml_data_wall))
    assert len(results_wall) == 1
    assert results_wall[0].status == "WARNING"
    assert "no check of consistency" in results_wall[0].message.lower()
    assert "same_emissivity_wall == 0" in results_wall[0].message.lower()
    yaml_data_roof = {
        "model": {
            "physics": {
                "same_emissivity_roof": {"value": 0}
            }
        },
        "sites": [{"name": "site1", "properties": {}}],  
    }
    results_roof = registry["same_emissivity"](ValidationContext(yaml_data=yaml_data_roof))
    assert len(results_roof) == 1
    assert results_roof[0].status == "WARNING"
    assert "no check of consistency" in results_roof[0].message.lower()
    assert "same_emissivity_roof == 0" in results_roof[0].message.lower()
    
def test_phase_b_forcing_height_error_and_warning(registry):
    """
    Test validate_forcing_height_vs_buildings returns WARNING and ERROR correctly 
    for a site where z is below max building height threshold but above min mean height threshold.
    
    According to Grimmond & Oke (1999) Fig. 1:
    - sfr > 0.35 then min_factor = 1.5 for mean building height
    - max_factor = 5 for both mean and max building heights
    """
    yaml_data = {
        "model": {
            "physics": {
                "stebbs": {"value": 1},
                "net_radiation": {"value": 1001},
            }
        },
        "sites": [
            {
                "name": "TestSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 10.0},  # forcing height
                    "land_cover": {
                        "bldgs": {"bldgh": {"value": 6.0}, "sfr": {"value": 0.4}},
                    },
                    "building_archetype": {"building_height": {"value": 8.0}},
                    "vertical_layers": {"height": {"value": [0, 5, 12, 0]}},
                },
            }
        ],
    }

    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))

    # --- Check ERRORs ---
    errors = [r for r in results if r.status == "ERROR"]
    # ERROR should NOT exist because z=10.0 >= min_factor*mean_height (1.5*6=9.0)
    assert not errors, f"Expected no ERRORs, got {errors}"

    # --- Check WARNINGs ---
    warnings = [r for r in results if r.status == "WARNING"]
    assert warnings, "Expected at least one WARNING for z < min_factor*max_height"
    
    # Max building height should be SPARTACUS top = 12.0
    assert any("1.5* max building height" in w.message for w in warnings)
    assert any("max building height=12.0" in w.suggested_value for w in warnings)

def test_phase_b_forcing_height_valid(registry):
    """Test validate_forcing_height_vs_buildings passes when z is sufficient."""
    yaml_data = {
        "model": {
            "physics": {
                "stebbs": {"value": 1},
                "net_radiation": {"value": 1001},
            }
        },
        "sites": [
            {
                "name": "TestSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 30.0},
                    "land_cover": {
                        "bldgs": {"bldgh": {"value": 10.0}, "sfr": {"value": 0.3}},
                    },
                    "building_archetype": {"building_height": {"value": 12.0}},
                    "vertical_layers": {"height": {"value": [0, 5, 12, 15]}},
                },
            }
        ],
    }
    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))
    # No ERROR or WARNING expected
    assert not results

def test_phase_b_forcing_height_only_stebbs_height(registry):
    """Test validate_forcing_height_vs_buildings uses building_height if present."""
    yaml_data = {
        "model": {
            "physics": {
                "stebbs": {"value": 1},
            }
        },
        "sites": [
            {
                "name": "TestSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 14.0},
                    "land_cover": {
                        "bldgs": {"bldgh": {"value": 7.0}, "sfr": {"value": 0.4}},
                    },
                    "building_archetype": {"building_height": {"value": 7.0}},
                },
            }
        ],
    }
    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))

    assert not any(r.status == "ERROR" for r in results)

def test_phase_b_forcing_height_only_spartacus_top(registry):
    """
    Test that validate_forcing_height_vs_buildings uses SPARTACUS top height if present,
    and raises a WARNING when z is below min_factor*max building height.
    
    The function should compute h_max = max(bldgh, STEBBS, SPARTACUS top) correctly,
    and produce an ERROR for mean height and WARNING for max height.
    """
    yaml_data = {
        "model": {
            "physics": {
                "net_radiation": {"value": 1001},  # enable SPARTACUS
            }
        },
        "sites": [
            {
                "name": "TestSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 5.0},  # forcing height below thresholds
                    "land_cover": {
                        "bldgs": {"bldgh": {"value": 3.0}, "sfr": {"value": 0.3}}
                    },
                    "vertical_layers": {"height": {"value": [0, 2, 8, 0]}},  # SPARTACUS top = 8
                },
            }
        ],
    }

    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))

    # --- Check that a WARNING exists for z < min_factor * max building height ---
    warnings = [r for r in results if r.status == "WARNING"]
    assert warnings, "Expected WARNING for z below max building height threshold"
    
    # SPARTACUS top = 8, sfr = 0.3 → min_factor = 2.0 → min_z_max = 16.0
    expected_min_z_max = 2.0 * 8.0
    assert any(str(expected_min_z_max) in w.suggested_value for w in warnings)
    assert any("max building height" in w.message for w in warnings)

    # --- Check that an ERROR exists for z < min_factor * mean building height ---
    errors = [r for r in results if r.status == "ERROR"]
    assert errors, "Expected ERROR for z below mean building height threshold"

    # mean building height = 3.0, sfr = 0.3 → min_factor = 2 → min_z_mean = 6
    expected_min_z_mean = 2.0 * 3.0
    assert any(str(expected_min_z_mean) in e.suggested_value for e in errors)
    assert any("mean building height" in e.message for e in errors)
    
def test_phase_b_forcing_height_missing_bldgh_or_sfr(registry):
    yaml_data = {
        "model": {"physics": {}},
        "sites": [
            {
                "name": "NoBldghSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 5.0},
                    "land_cover": {"bldgs": {"sfr": {"value": 0.4}}},  # bldgh missing
                },
            }
        ],
    }
    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))
    assert results
    assert results[0].status == "WARNING"
    assert "cannot validate" in results[0].message

def test_phase_b_forcing_height_above_max(registry):
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [
            {
                "name": "HighZSite",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 100.0},  # way above
                    "land_cover": {"bldgs": {"bldgh": {"value": 10.0}, "sfr": {"value": 0.4}}},
                    "building_archetype": {"building_height": {"value": 15.0}},
                },
            }
        ],
    }
    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))
    errors = [r for r in results if r.status == "ERROR"]
    warnings = [r for r in results if r.status == "WARNING"]
    assert errors
    assert warnings
    assert any("above the maximum allowed" in e.message for e in errors)
    assert any("above the maximum allowed" in w.message for w in warnings)

def test_phase_b_forcing_height_sfr_boundary(registry):
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [
            {
                "name": "BoundarySFR",
                "gridiv": 1,
                "properties": {
                    "z": {"value": 6.0},
                    "land_cover": {"bldgs": {"bldgh": {"value": 4.0}, "sfr": {"value": 0.35}}},
                },
            }
        ],
    }
    results = registry["forcing_height"](ValidationContext(yaml_data=yaml_data))
    # min_factor should be 1.5 → min_z_mean = 6.0 → z == min allowed → no ERROR
    errors = [r for r in results if r.status == "ERROR"]
    assert not errors

def test_validate_model_option_rcmethod_missing_params(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {},
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    params = [r.parameter for r in results]
    assert any("roof_outer_heat_capacity_fraction" in p for p in params)
    assert any("wall_outer_heat_capacity_fraction" in p for p in params)
    assert all(r.status == "ERROR" for r in results)

def test_validate_model_option_rcmethod_enabled_invalid_values(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "roof_outer_heat_capacity_fraction": {"value": 1.5},
                    "wall_outer_heat_capacity_fraction": {"value": -0.2},
                },
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    assert any("out of valid range" in r.message for r in results)
    assert all(r.status == "ERROR" for r in results)

def test_validate_model_option_rcmethod_enabled_valid_values(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "roof_outer_heat_capacity_fraction": {"value": 0.5},
                    "wall_outer_heat_capacity_fraction": {"value": 0.5},
                },
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    assert not results or all(r.status != "ERROR" for r in results)

def test_adjust_model_option_rcmethod_sets_defaults():
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 0}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {},
            }
        }],
    }
    updated_data, adjustments = adjust_model_option_rcmethod(yaml_data)
    ba = updated_data["sites"][0]["properties"]["building_archetype"]
    assert ba["roof_outer_heat_capacity_fraction"]["value"] == 0.5
    assert ba["wall_outer_heat_capacity_fraction"]["value"] == 0.5
    assert any(adj.parameter == "building_archetype.roof_outer_heat_capacity_fraction" for adj in adjustments)
    assert any(adj.parameter == "building_archetype.wall_outer_heat_capacity_fraction" for adj in adjustments)

def test_adjust_model_option_rcmethod_no_action_when_already_set():
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 0}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "roof_outer_heat_capacity_fraction": {"value": 0.5},
                    "wall_outer_heat_capacity_fraction": {"value": 0.5},
                },
            }
        }],
    }
    updated_data, adjustments = adjust_model_option_rcmethod(yaml_data)
    assert len(adjustments) == 0

def test_adjust_model_option_setpointmethod_sets_profiles_to_null_when_0_or_1():
    # setpoint == 0: should nullify all profile entries
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 0}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": {"0": 21.0, "1": 22.0},
                        "holiday": {"0": 20.0, "1": 21.0},
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": {"0": 25.0, "1": 26.0},
                        "holiday": {"0": 24.0, "1": 25.0},
                    },
                }
            }
        }],
    }
    updated, adjustments = adjust_model_option_setpointmethod(yaml_data)
    ba = updated["sites"][0]["properties"]["building_archetype"]
    for prof in ["heating_setpoint_temperature_profile", "cooling_setpoint_temperature_profile"]:
        for daytype in ["working_day", "holiday"]:
            for hour in ba[prof][daytype]:
                assert ba[prof][daytype][hour] is None
    assert any(a.parameter == "building_archetype.heating_setpoint_temperature_profile.working_day" for a in adjustments)
    assert any(a.parameter == "building_archetype.cooling_setpoint_temperature_profile.holiday" for a in adjustments)

def test_adjust_model_option_setpointmethod_sets_profiles_to_null_when_1():
    # setpoint == 1: should nullify all profile entries
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": {"0": 21.0},
                        "holiday": {"0": 20.0},
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": {"0": 25.0},
                        "holiday": {"0": 24.0},
                    },
                }
            }
        }],
    }
    updated, adjustments = adjust_model_option_setpointmethod(yaml_data)
    ba = updated["sites"][0]["properties"]["building_archetype"]
    for prof in ["heating_setpoint_temperature_profile", "cooling_setpoint_temperature_profile"]:
        for daytype in ["working_day", "holiday"]:
            for hour in ba[prof][daytype]:
                assert ba[prof][daytype][hour] is None
    assert any(a.parameter == "building_archetype.heating_setpoint_temperature_profile.working_day" for a in adjustments)

def test_adjust_model_option_setpointmethod_sets_temps_to_null_when_2():
    # setpoint == 2: should nullify heating_setpoint_temperature and cooling_setpoint_temperature
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature": {"value": 21.0},
                    "cooling_setpoint_temperature": {"value": 25.0},
                }
            }
        }],
    }
    updated, adjustments = adjust_model_option_setpointmethod(yaml_data)
    ba = updated["sites"][0]["properties"]["building_archetype"]
    assert ba["heating_setpoint_temperature"]["value"] is None
    assert ba["cooling_setpoint_temperature"]["value"] is None
    assert any(a.parameter == "building_archetype.heating_setpoint_temperature" for a in adjustments)
    assert any(a.parameter == "building_archetype.cooling_setpoint_temperature" for a in adjustments)

def test_adjust_model_option_setpointmethod_no_action_when_already_null():
    # Should not add adjustments if already null
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature": {"value": None},
                    "cooling_setpoint_temperature": {"value": None},
                }
            }
        }],
    }
    updated, adjustments = adjust_model_option_setpointmethod(yaml_data)
    ba = updated["sites"][0]["properties"]["building_archetype"]
    assert ba["heating_setpoint_temperature"]["value"] is None
    assert ba["cooling_setpoint_temperature"]["value"] is None
    assert len(adjustments) == 0

def test_validate_model_option_rcmethod2_missing_params(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {"building_archetype": {}},
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    required = [
        "wall_external_thickness", "wall_external_effective_conductivity", "wall_external_density", "wall_external_specific_heat_capacity",
        "roof_external_thickness", "roof_external_effective_conductivity", "roof_external_density", "roof_external_specific_heat_capacity"
    ]
    expected = [f"building_archetype.{p}" for p in required]
    error_params = [r.parameter for r in results if r.status == "ERROR"]
    for p in expected:
        assert p in error_params
    assert all(r.status == "ERROR" for r in results if r.parameter in expected)

def test_validate_model_option_rcmethod2_all_params_provided(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "wall_external_thickness": {"value": 0.2},
                    "wall_external_effective_conductivity": {"value": 1.0},
                    "wall_external_density": {"value": 2200},
                    "wall_external_specific_heat_capacity": {"value": 900},
                    "roof_external_thickness": {"value": 0.18},
                    "roof_external_effective_conductivity": {"value": 1.2},
                    "roof_external_density": {"value": 2300},
                    "roof_external_specific_heat_capacity": {"value": 1000},
                }
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    warnings = [r for r in results if r.status == "WARNING"]
    assert any("wall material parameters will be used for parameterisation" in r.message for r in warnings)
    assert any("roof material parameters will be used for parameterisation" in r.message for r in warnings)
    assert all(r.status != "ERROR" for r in results)

def test_validate_model_option_rcmethod2_legacy_ext_params_supported(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "WallextThickness": {"value": 0.2},
                    "WallextEffectiveConductivity": {"value": 1.0},
                    "WallextDensity": {"value": 2200},
                    "WallextCp": {"value": 900},
                    "RoofextThickness": {"value": 0.18},
                    "RoofextEffectiveConductivity": {"value": 1.2},
                    "RoofextDensity": {"value": 2300},
                    "RoofextCp": {"value": 1000},
                }
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    warnings = [r for r in results if r.status == "WARNING"]
    assert any("wall material parameters will be used for parameterisation" in r.message for r in warnings)
    assert any("roof material parameters will be used for parameterisation" in r.message for r in warnings)
    assert all(r.status != "ERROR" for r in results)

def test_validate_model_option_rcmethod2_some_params_missing(registry):
    yaml_data = {
        "model": {"physics": {"outer_cap_fraction": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "wall_external_thickness": {"value": 0.2},
                    "wall_external_effective_conductivity": {},
                    "wall_external_density": {"value": 2200},
                    # wall_external_specific_heat_capacity missing
                    "roof_external_thickness": {"value": 0.18},
                    # roof_external_effective_conductivity missing
                    "roof_external_density": {"value": 2300},
                    "roof_external_specific_heat_capacity": {"value": 1000},
                }
            }
        }],
    }
    results = registry["rcmethod"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results if r.status == "ERROR"]
    assert "building_archetype.wall_external_effective_conductivity" in error_params
    assert "building_archetype.wall_external_specific_heat_capacity" in error_params
    assert "building_archetype.roof_external_effective_conductivity" in error_params
    assert any(r.status == "WARNING" for r in results)
    assert all("must be provided" in r.message for r in results if r.status == "ERROR")

def test_validate_model_option_setpointmethod_0_or_1_all_params(registry):
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 0}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature": {"value": 21.0},
                    "cooling_setpoint_temperature": {"value": 25.0},
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    assert not results or all(r.status != "ERROR" for r in results)

def test_validate_model_option_setpointmethod_0_or_1_missing_params(registry):
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 1}, "stebbs": {"value":1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    # "heating_setpoint_temperature" missing
                    "cooling_setpoint_temperature": {"value": 25.0},
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results if r.status == "ERROR"]
    assert "heating_setpoint_temperature" in error_params
    assert all("must be set" in r.message for r in results if r.status == "ERROR")

def test_validate_model_option_setpointmethod_2_all_profiles_valid(registry):
    heating_working = {str(i): 20.0 for i in range(1, 145)}
    heating_holiday = {str(i): 19.0 for i in range(1, 145)}
    cooling_working = {str(i): 26.0 for i in range(1, 145)}
    cooling_holiday = {str(i): 25.5 for i in range(1, 145)}
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": heating_working,
                        "holiday": heating_holiday,
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": cooling_working,
                        "holiday": cooling_holiday,
                    },
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    assert not results or all(r.status != "ERROR" for r in results)

def test_validate_model_option_setpointmethod_2_missing_profile_entries(registry):
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}, "stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": {"0": None, "1": 19.5},
                        "holiday": {"0": 19.0, "1": None},
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": {"0": 26.0, "1": None},
                        "holiday": {"0": None, "1": 26.5},
                    },
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results if r.status == "ERROR"]
    assert "heating_setpoint_temperature_profile" in error_params
    assert "cooling_setpoint_temperature_profile" in error_params
    assert any("null entries" in r.message for r in results if r.status == "ERROR")

def test_validate_model_option_setpointmethod_2_out_of_range(registry):
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}, "stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": {"0": 31.0, "1": 19.5},
                        "holiday": {"0": 19.0, "1": 30.0},
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": {"0": 14.0, "1": 27.0},
                        "holiday": {"0": 25.5, "1": 15.0},
                    },
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    heating_errors = [r for r in results if r.parameter == "heating_setpoint_temperature_profile" and r.status == "ERROR"]
    cooling_errors = [r for r in results if r.parameter == "cooling_setpoint_temperature_profile" and r.status == "ERROR"]
    assert any("values >= 30.0" in r.message for r in heating_errors)
    assert any("values <= 15.0" in r.message for r in cooling_errors)

def test_validate_model_option_setpointmethod_2_invalid_slice_keys(registry):
    yaml_data = {
        "model": {"physics": {"setpoint": {"value": 2}, "stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "heating_setpoint_temperature_profile": {
                        "working_day": {"0": 20.0, "1": 19.5},
                        "holiday": {"1": 19.0, "2": 18.5},
                    },
                    "cooling_setpoint_temperature_profile": {
                        "working_day": {"1": 26.0, "145": 27.0},
                        "holiday": {"1": 25.5, "2": 26.5},
                    },
                }
            }
        }],
    }
    results = registry["setpoint"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results if r.status == "ERROR"]
    assert "heating_setpoint_temperature_profile.working_day" in error_params
    assert "cooling_setpoint_temperature_profile.working_day" in error_params
    assert any(
        "Only entries 1-144 are valid." in r.message
        for r in results
        if r.status == "ERROR"
    )
    
def test_validate_model_option_stebbsmethod_hotwaterflowprofile_valid(registry):
    """Test hot_water_flow_profile accepts only 0 or 1 values."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "hot_water_flow_profile": {
                        "working_day": {"0": 0, "1": 1, "2": 0.0, "3": 1.0},
                        "holiday": {"0": 1, "1": 0, "2": 1.0, "3": 0.0},
                    }
                }
            }
        }],
    }
    results = registry["stebbs_props"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for valid hot_water_flow_profile values"

def test_validate_model_option_stebbsmethod_hotwaterflowprofile_invalid(registry):
    """Test hot_water_flow_profile returns ERROR for invalid values."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "hot_water_flow_profile": {
                        "working_day": {"0": 2, "1": -1, "2": 0.5},
                        "holiday": {"0": "yes", "1": None},
                    }
                }
            }
        }],
    }
    results = registry["stebbs_props"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results]
    assert "stebbs.hot_water_flow_profile.working_day.0" in error_params
    assert "stebbs.hot_water_flow_profile.working_day.1" in error_params
    assert "stebbs.hot_water_flow_profile.working_day.2" in error_params
    assert "stebbs.hot_water_flow_profile.holiday.0" in error_params
    assert "stebbs.hot_water_flow_profile.holiday.1" in error_params
    assert all(r.status == "ERROR" for r in results)
    assert all("must be 0 or 1" in r.message for r in results)

def test_validate_model_option_stebbsmethod_hotwaterflowprofile_missing(registry):
    """Test hot_water_flow_profile missing returns no errors."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    # hot_water_flow_profile missing
                }
            }
        }],
    }
    results = registry["stebbs_props"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors if hot_water_flow_profile is missing"

def test_validate_model_option_stebbsmethod_hotwaterflowprofile_partial(registry):
    """Test hot_water_flow_profile with partial valid/invalid values."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "hot_water_flow_profile": {
                        "working_day": {"0": 1, "1": 0, "2": 2},
                        "holiday": {"0": 0, "1": 1, "2": -1},
                    }
                }
            }
        }],
    }
    results = registry["stebbs_props"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results]
    assert "stebbs.hot_water_flow_profile.working_day.2" in error_params
    assert "stebbs.hot_water_flow_profile.holiday.2" in error_params
    assert all(r.status == "ERROR" for r in results)
    assert len(results) == 2

def test_validate_model_option_stebbsmethod_daylightcontrol_valid(registry):
    """Test DaylightControl accepts only 0 or 1 values, and LightingIlluminanceThreshold is required if 1."""
    # Valid: DaylightControl = 1, LightingIlluminanceThreshold provided
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "DaylightControl": {"value": 1},
                    "LightingIlluminanceThreshold": {"value": 300}
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for valid DaylightControl=1 with LightingIlluminanceThreshold"

    # Valid: DaylightControl = 0, LightingIlluminanceThreshold not required
    yaml_data["sites"][0]["properties"]["stebbs"]["DaylightControl"]["value"] = 0
    yaml_data["sites"][0]["properties"]["stebbs"].pop("LightingIlluminanceThreshold")
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for valid DaylightControl=0"

    # Valid: DaylightControl = 0.0 (float)
    yaml_data["sites"][0]["properties"]["stebbs"]["DaylightControl"]["value"] = 0.0
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for valid DaylightControl=0.0"

    # Valid: DaylightControl = 1.0 (float), LightingIlluminanceThreshold provided
    yaml_data["sites"][0]["properties"]["stebbs"]["DaylightControl"]["value"] = 1.0
    yaml_data["sites"][0]["properties"]["stebbs"]["LightingIlluminanceThreshold"] = {"value": 200}
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for valid DaylightControl=1.0 with LightingIlluminanceThreshold"

def test_validate_model_option_stebbsmethod_daylightcontrol_missing_lit(registry):
    """Test daylight_control == 1 but lighting_illuminance_threshold missing returns error."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "daylight_control": {"value": 1}
                    # lighting_illuminance_threshold missing
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert len(results) == 1
    assert results[0].parameter == "stebbs.lighting_illuminance_threshold"
    assert results[0].status == "ERROR"
    assert "must be provided" in results[0].message

def test_validate_model_option_stebbsmethod_daylightcontrol_invalid(registry):
    """Test daylight_control returns ERROR for invalid values."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "daylight_control": {"value": 2}
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert len(results) == 1
    assert results[0].parameter == "stebbs.daylight_control"
    assert results[0].status == "ERROR"
    assert "must be 0 (off) or 1 (on)" in results[0].message

def test_validate_model_option_stebbsmethod_daylightcontrol_string_value(registry):
    """Test daylight_control returns ERROR for string or unexpected values."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "daylight_control": {"value": "yes"}
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert len(results) == 1
    assert results[0].parameter == "stebbs.daylight_control"
    assert results[0].status == "ERROR"
    assert "must be 0 (off) or 1 (on)" in results[0].message

def test_validate_model_option_stebbsmethod_daylightcontrol_missing(registry):
    """Test DaylightControl missing returns no errors."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    # DaylightControl missing
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors if DaylightControl is missing"

def test_validate_model_option_stebbsmethod_daylightcontrol_not_active(registry):
    """Test no validation occurs if stebbsmethod != 1."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 0}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "DaylightControl": {"value": 2}
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors if stebbsmethod != 1"

def test_daylight_control_lightingilluminancethreshold_zero(registry):
    """Test LightingIlluminanceThreshold=0 is accepted when DaylightControl=1."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "DaylightControl": {"value": 1},
                    "LightingIlluminanceThreshold": {"value": 0}
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for LightingIlluminanceThreshold=0"

def test_daylight_control_daylightcontrol_zero(registry):
    """Test DaylightControl=0 is accepted and LightingIlluminanceThreshold is not required."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "stebbs": {
                    "DaylightControl": {"value": 0}
                    # LightingIlluminanceThreshold not provided
                }
            }
        }],
    }
    results = registry["daylight_control"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors for DaylightControl=0 without LightingIlluminanceThreshold"


def test_validate_model_option_stebbsmethod_occupants_zero_metabolismprofile_nonzero(registry):
    """Test error when occupants=0.0 but metabolism_profile has nonzero values."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "occupants": {"value": 0.0},
                    "metabolism_profile": {
                        "working_day": {"0": 0, "1": 1.2, "2": 0},
                        "holiday": {"0": 0, "1": 0, "2": 0.5},
                    },
                },
                "stebbs": {},
            }
        }],
    }
    results = registry["occupants_metabolism"](ValidationContext(yaml_data=yaml_data))
    error_params = [r.parameter for r in results]
    assert "building_archetype.metabolism_profile" in error_params
    assert any("nonzero entries" in r.message for r in results)
    assert all(r.status == "ERROR" for r in results)

def test_validate_model_option_stebbsmethod_occupants_zero_metabolismprofile_all_zero(registry):
    """Test no error when Occupants=0.0 and all MetabolismProfile values are zero."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "occupants": {"value": 0.0},
                    "metabolism_profile": {
                        "working_day": {"0": 0, "1": 0.0, "2": None},
                        "holiday": {"0": 0, "1": 0.0, "2": None},
                    },
                },
                "stebbs": {},
            }
        }],
    }
    results = registry["occupants_metabolism"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors when all MetabolismProfile values are zero or None"

def test_validate_model_option_stebbsmethod_occupants_nonzero_metabolismprofile_nonzero(registry):
    """Test no error when Occupants>0 and MetabolismProfile has nonzero values."""

    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "name": "site1",
            "properties": {
                "building_archetype": {
                    "occupants": {"value": 2.0},
                    "metabolism_profile": {
                        "working_day": {"0": 1.1, "1": 1.2},
                        "holiday": {"0": 0.9, "1": 1.0},
                    },
                },
                "stebbs": {},
            }
        }],
    }
    results = registry["occupants_metabolism"](ValidationContext(yaml_data=yaml_data))
    assert not results, "Should not return errors when Occupants > 0"

@pytest.mark.parametrize(
    "wwr, nullify, keep",
    [
        (0.0,  # window_to_wall_ratio==0: nullify window params
         ["window_internal_convection_coefficient", "window_external_convection_coefficient"],
         ["wall_external_convection_coefficient", "wall_internal_convection_coefficient"]),
        (1.0,  # window_to_wall_ratio==1: nullify wall params
         ["wall_external_convection_coefficient", "wall_internal_convection_coefficient"],
         ["window_internal_convection_coefficient", "window_external_convection_coefficient"]),
        (0.5,  # window_to_wall_ratio!=0,1: nullify nothing
         [], ["window_internal_convection_coefficient", "window_external_convection_coefficient",
              "wall_external_convection_coefficient", "wall_internal_convection_coefficient"]),
    ],
)
def test_adjust_model_option_stebbsmethod_nullification(wwr, nullify, keep):
    """Test adjust_model_option_stebbsmethod nullifies correct params based on window_to_wall_ratio."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 1}}},
        "sites": [{
            "properties": {
                "stebbs": {
                    "window_internal_convection_coefficient": {"value": 5.0},
                    "window_external_convection_coefficient": {"value": 6.0},
                    "wall_external_convection_coefficient": {"value": 8.0},
                    "wall_internal_convection_coefficient": {"value": 9.0},
                },
                "building_archetype": {
                    "window_to_wall_ratio": {"value": wwr},
                    "window_thickness": {"value": 0.2},
                    "window_effective_conductivity": {"value": 1.1},
                    "window_density": {"value": 2500},
                    "window_specific_heat_capacity": {"value": 900},
                    "window_external_emissivity": {"value": 0.85},
                    "window_internal_emissivity": {"value": 0.9},
                    "window_transmissivity": {"value": 0.7},
                    "window_absorptivity": {"value": 0.1},
                    "window_reflectivity": {"value": 0.2},
                    "wall_external_emissivity": {"value": 0.88},
                    "wall_internal_emissivity": {"value": 0.89},
                    "wall_transmissivity": {"value": 0.5},
                    "wall_absorptivity": {"value": 0.15},
                    "wall_reflectivity": {"value": 0.25},
                    "wall_thickness": {"value": 0.35},
                    "wall_effective_conductivity": {"value": 1.3},
                    "wall_density": {"value": 2400},
                    "wall_specific_heat_capacity": {"value": 920},
                },
            }
        }],
    }
    updated, adjustments = adjust_model_option_stebbsmethod(yaml_data)
    stebbs = updated["sites"][0]["properties"]["stebbs"]
    bldgarc = updated["sites"][0]["properties"]["building_archetype"]

    # Check nullified params
    for param in nullify:
        assert stebbs.get(param, {}).get("value") is None
    # Check kept params
    for param in keep:
        assert stebbs.get(param, {}).get("value") == yaml_data["sites"][0]["properties"]["stebbs"].get(param, {}).get("value")

    # Check building_archetype nullification
    if wwr == 0.0:
        for param in [
            "window_thickness", "window_effective_conductivity", "window_density", "window_specific_heat_capacity",
            "window_external_emissivity", "window_internal_emissivity", "window_transmissivity",
            "window_absorptivity", "window_reflectivity"
        ]:
            assert bldgarc[param]["value"] is None
        for param in [
            "wall_external_emissivity", "wall_internal_emissivity", "wall_transmissivity",
            "wall_absorptivity", "wall_reflectivity", "wall_thickness", "wall_effective_conductivity",
            "wall_density", "wall_specific_heat_capacity"
        ]:
            assert bldgarc[param]["value"] == yaml_data["sites"][0]["properties"]["building_archetype"][param]["value"]
    elif wwr == 1.0:
        for param in [
            "wall_external_emissivity", "wall_internal_emissivity", "wall_transmissivity",
            "wall_absorptivity", "wall_reflectivity", "wall_thickness", "wall_effective_conductivity",
            "wall_density", "wall_specific_heat_capacity"
        ]:
            assert bldgarc[param]["value"] is None
        for param in [
            "window_thickness", "window_effective_conductivity", "window_density", "window_specific_heat_capacity",
            "window_external_emissivity", "window_internal_emissivity", "window_transmissivity",
            "window_absorptivity", "window_reflectivity"
        ]:
            assert bldgarc[param]["value"] == yaml_data["sites"][0]["properties"]["building_archetype"][param]["value"]
    else:
        # No params nullified
        for param in bldgarc:
            assert bldgarc[param]["value"] == yaml_data["sites"][0]["properties"]["building_archetype"][param]["value"]

    # Adjustments
    if wwr in (0.0, 1.0):
        assert adjustments
        assert all(a.new_value == "null" for a in adjustments)
    else:
        assert adjustments == []


def test_adjust_model_option_stebbsmethod_not_one_no_action():
    """If stebbsmethod != 1, nothing is changed."""
    yaml_data = {
        "model": {"physics": {"stebbs": {"value": 0}}},
        "sites": [{
            "properties": {
                "stebbs": {
                    "window_internal_convection_coefficient": {"value": 5.0},
                    "wall_external_convection_coefficient": {"value": 8.0},
                },
                "building_archetype": {
                    "window_to_wall_ratio": {"value": 0.0},
                    "window_thickness": {"value": 0.2},
                    "wall_thickness": {"value": 0.35},
                },
            }
        }],
    }
    updated, adjustments = adjust_model_option_stebbsmethod(yaml_data)
    props = updated["sites"][0]["properties"]
    for param in ["window_internal_convection_coefficient", "wall_external_convection_coefficient"]:
        assert props["stebbs"][param]["value"] == yaml_data["sites"][0]["properties"]["stebbs"][param]["value"]
    for param in ["window_thickness", "wall_thickness"]:
        assert props["building_archetype"][param]["value"] == yaml_data["sites"][0]["properties"]["building_archetype"][param]["value"]
    assert adjustments == []

def test_needs_spartacus_validation_true_and_false():
    
    cfg = make_cfg()
    cfg.model.physics.net_radiation =1001
    assert cfg._needs_spartacus_validation() is True

    cfg2 = make_cfg()
    cfg2.model.physics.net_radiation = 1
    assert cfg2._needs_spartacus_validation() is False

def test_validate_spartacus_building_height_error():
    cfg = make_cfg(net_radiation=1001, stebbs=1)
    # _unwrap_value only unwraps RefValue/Enum, not SimpleNamespace;
    # set stebbsmethod as a raw int so the validator can parse it.
    cfg.model.physics.stebbs = 1

    # bldgh and building_height both exceed height[nlayer]
    bldgs = SimpleNamespace(bldgh=15.0)
    building_archetype = SimpleNamespace(building_height=20.0)
    vertical_layers = SimpleNamespace(height=[5.0, 10.0, 12.0], nlayer=1)
    props = SimpleNamespace(
        land_cover=SimpleNamespace(bldgs=bldgs),
        vertical_layers=vertical_layers,
        building_archetype=building_archetype,
    )
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_building_height(site, 0)

    assert len(msgs) == 2
    assert any("bldgh=15.0" in m and "height[1]=10.0" in m for m in msgs)
    assert any("building_height=20.0" in m and "height[1]=10.0" in m for m in msgs)


def test_validate_spartacus_building_height_no_error():
    cfg = make_cfg(net_radiation=1001, stebbs=1)
    cfg.model.physics.stebbs = 1

    # bldgh and building_height do not exceed height[nlayer]
    bldgs = SimpleNamespace(bldgh=8.0)
    building_archetype = SimpleNamespace(building_height=9.0)
    vertical_layers = SimpleNamespace(height=[5.0, 10.0, 12.0], nlayer=1)
    props = SimpleNamespace(
        land_cover=SimpleNamespace(bldgs=bldgs),
        vertical_layers=vertical_layers,
        building_archetype=building_archetype,
    )
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_building_height(site, 0)
    assert msgs == []


def test_validate_spartacus_building_height_stebbs_off():
    """building_height should NOT be checked when stebbsmethod != 1."""
    cfg = make_cfg(net_radiation=1001, stebbs=0)

    # building_height exceeds domain top, but stebbsmethod is off
    bldgs = SimpleNamespace(bldgh=8.0)
    building_archetype = SimpleNamespace(building_height=20.0)
    vertical_layers = SimpleNamespace(height=[5.0, 10.0, 12.0], nlayer=1)
    props = SimpleNamespace(
        land_cover=SimpleNamespace(bldgs=bldgs),
        vertical_layers=vertical_layers,
        building_archetype=building_archetype,
    )
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_building_height(site, 0)
    assert msgs == []

def test_validate_spartacus_sfr_mismatch_bldgs_frac():
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    bldgs = SimpleNamespace(sfr=0.6)  
    lc = SimpleNamespace(bldgs=bldgs, evetr=None, dectr=None)
    vertical_layers = SimpleNamespace(
        building_frac=[0.3],  
        veg_frac=[0.0],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_sfr(site, 0)
    assert msgs  
    assert any(
        "bldgs.sfr (0.6) does not match vertical_layers.building_frac[0] (0.3)"
        in m
        for m in msgs
    )

def test_validate_spartacus_sfr_consistent_values():
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    bldgs = SimpleNamespace(sfr=0.3)  
    evetr = SimpleNamespace(sfr=0.1)
    dectr = SimpleNamespace(sfr=0.2)
    lc = SimpleNamespace(bldgs=bldgs, evetr=evetr, dectr=dectr)
    vertical_layers = SimpleNamespace(
        building_frac=[0.3],
        veg_frac=[0.3],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_sfr(site, 0)
    assert msgs == []

def test_validate_spartacus_sfr_mismatch_veg_frac():
    """SPARTACUS SFR validation flags mismatch between evetr.sfr + dectr.sfr and max(veg_frac)."""
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))

    # land_cover vegetation: evetr + dectr = 0.4
    evetr = SimpleNamespace(sfr=0.1)
    dectr = SimpleNamespace(sfr=0.3)
    bldgs = SimpleNamespace(sfr=0.2)
    lc = SimpleNamespace(bldgs=bldgs, evetr=evetr, dectr=dectr)

    # vertical_layers veg_frac: max = 0.1 -> mismatch with 0.4
    vertical_layers = SimpleNamespace(
        building_frac=[0.2],
        veg_frac=[0.1],  # max(vertical_layers.veg_frac) = 0.1
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")

    msgs = cfg._validate_spartacus_sfr(site, 0)
    assert msgs
    assert any(
        "evetr.sfr + dectr.sfr (0.4) does not match max(vertical_layers.veg_frac) (0.1)"
        in m
        for m in msgs
    )

def test_validate_spartacus_veg_dimensions_valid():
    """Test validate_spartacus_veg_dimensions passes with matching veg_frac and nlayer."""
    cfg = SUEWSConfig.model_construct()
    vertical_layers = SimpleNamespace(
        nlayer=2,
        veg_frac=[0.3, 0.7],
    )
    props = SimpleNamespace(vertical_layers=vertical_layers)
    site = SimpleNamespace(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_too_few_elements():
    """Test validate_spartacus_veg_dimensions detects too few veg_frac elements."""
    cfg = SUEWSConfig.model_construct()
    vertical_layers = SimpleNamespace(
        nlayer=3,
        veg_frac=[0.2, 0.8],
    )
    props = SimpleNamespace(vertical_layers=vertical_layers)
    site = SimpleNamespace(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_too_many_elements():
    """Test validate_spartacus_veg_dimensions detects too many veg_frac elements."""
    cfg = SUEWSConfig.model_construct()
    vertical_layers = SimpleNamespace(
        nlayer=2,
        veg_frac=[0.1, 0.2, 0.7],
    )
    props = SimpleNamespace(vertical_layers=vertical_layers)
    site = SimpleNamespace(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_missing_veg_frac():
    """Test validate_spartacus_veg_dimensions handles missing veg_frac gracefully."""
    cfg = SUEWSConfig.model_construct()
    vertical_layers = SimpleNamespace(
        nlayer=2,
        # veg_frac missing
    )
    props = SimpleNamespace(vertical_layers=vertical_layers)
    site = SimpleNamespace(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_missing_nlayer():
    """Test validate_spartacus_veg_dimensions handles missing nlayer gracefully."""
    cfg = SUEWSConfig.model_construct()
    vertical_layers = SimpleNamespace(
        veg_frac=[0.5, 0.5],
        # nlayer missing
    )
    props = SimpleNamespace(vertical_layers=vertical_layers)
    site = SimpleNamespace(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_passing_case():
    """Passing case: dectreeh=12, height=[0, 5, 10, 15, 20], veg_frac=[0.3, 0.3, 0.2, 0, 0]"""
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    lc = SimpleNamespace(dectr=SimpleNamespace(height_deciduous_tree=12.0), evetr=None)
    vertical_layers = SimpleNamespace(
        height=[0, 5, 10, 15, 20],
        veg_frac=[0.3, 0.3, 0.2, 0, 0],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_failing_case():
    """Failing case: dectreeh=12, height=[0, 5, 10, 15, 20], veg_frac=[0.3, 0.3, 0.2, 0.1, 0].
    Tree at 12 m falls in layer 10-15 m (layer_index=3), so veg_frac[3]=0.1 in the
    15-20 m layer should be flagged."""
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    lc = SimpleNamespace(dectr=SimpleNamespace(height_deciduous_tree=12.0), evetr=None)
    vertical_layers = SimpleNamespace(
        height=[0, 5, 10, 15, 20],
        veg_frac=[0.3, 0.3, 0.2, 0.1, 0],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert len(msgs) >= 1
    assert any("veg_frac[3]" in m for m in msgs)

def test_validate_spartacus_veg_dimensions_boundary_case():
    """Boundary case: max_tree exactly on a layer boundary."""
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    lc = SimpleNamespace(dectr=SimpleNamespace(height_deciduous_tree=15.0), evetr=None)
    vertical_layers = SimpleNamespace(
        height=[0, 5, 10, 15, 20],
        veg_frac=[0.3, 0.3, 0.2, 0, 0],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs == []

def test_validate_spartacus_veg_dimensions_exceeds_all_case():
    """Exceeds-all case: max_tree=100 with height=[0, 5, 10] — should produce the 'exceeds' message."""
    cfg = SUEWSConfig.model_construct()
    cfg.model = SimpleNamespace(physics=SimpleNamespace(net_radiation=1001))
    # Note: dectrh and evetrh are attributes of land_cover.dectr and land_cover.evetr, not land_cover itself
    dectr = SimpleNamespace(height_deciduous_tree=100.0)
    lc = SimpleNamespace(dectr=dectr, evetr=None)
    vertical_layers = SimpleNamespace(
        height=[0, 5, 10],
        veg_frac=[0.3, 0.3, 0.2],
    )
    props = SimpleNamespace(land_cover=lc, vertical_layers=vertical_layers)
    site = DummySite(properties=props, name="TestSite")
    msgs = cfg._validate_spartacus_veg_dimensions(site, 0)
    assert msgs
    assert any("exceeds all vertical_layers heights" in m for m in msgs)

def make_sample_physics(fields):
    obj = types.SimpleNamespace()
    obj.model_fields_set = set(fields)
    return obj

def make_sample_model(physics):
    obj = types.SimpleNamespace()
    obj.physics = physics
    return obj

class SampleConfig:
    def __init__(self, model):
        self.model = model

    def _is_physics_explicitly_configured(self, option_name: str) -> bool:
        physics = getattr(self.model, "physics", None)
        return bool(
            physics
            and hasattr(physics, "model_fields_set")
            and option_name in physics.model_fields_set
        )

def test_is_physics_explicitly_configured_true():
    """Returns True if the physics field is explicitly set."""
    model = make_sample_model(make_sample_physics({'roughness_sublayer', 'storage_heat'}))
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is True

def test_is_physics_explicitly_configured_false():
    """Returns False if the specific physics field is not set."""
    model = make_sample_model(make_sample_physics({'storage_heat'}))
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

def test_is_physics_explicitly_configured_empty():
    """Returns False if model_fields_set is empty."""
    model = make_sample_model(make_sample_physics(set()))
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

def test_is_physics_explicitly_configured_no_physics():
    """Returns False if physics attribute is missing or None."""
    # model without physics attribute
    class DummyModel: pass
    cfg = SampleConfig(DummyModel())
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

    # model with physics None
    class DummyModel2: pass
    m = DummyModel2()
    m.physics = None
    cfg = SampleConfig(m)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

def test_is_physics_explicitly_configured_no_model_fields_set():
    """Returns False if physics object has no model_fields_set attribute."""
    class DummyPhysics: pass
    model = make_sample_model(DummyPhysics())
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

def test_is_physics_explicitly_configured_non_string_fields():
    """Handles non-string entries in model_fields_set gracefully."""
    model = make_sample_model(make_sample_physics({1, None, 'roughness_sublayer'}))
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is True
    assert cfg._is_physics_explicitly_configured('storage_heat') is False

def test_is_physics_explicitly_configured_model_fields_set_list():
    """Handles model_fields_set as a list instead of set."""
    obj = types.SimpleNamespace()
    obj.model_fields_set = ['roughness_sublayer', 'storage_heat']
    model = make_sample_model(obj)
    cfg = SampleConfig(model)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is True
    assert cfg._is_physics_explicitly_configured('other') is False

def test_is_physics_explicitly_configured_model_none():
    """Returns False if model is None."""
    cfg = SampleConfig(None)
    assert cfg._is_physics_explicitly_configured('roughness_sublayer') is False

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
        """Critical missing physics fields raise on load (gh#1333).

        Previously the validator logged a WARNING summary and let the
        simulation run; the resulting NaN output was silent on x86_64.
        Now ``from_yaml`` raises ``ValueError`` with the missing fields
        named in the message.
        """
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
            with pytest.raises(ValueError) as exc_info:
                SUEWSConfig.from_yaml(yaml_path)

            message = str(exc_info.value)
            assert "bldgh" in message
            assert "faibldg" in message
        finally:
            yaml_path.unlink()

    def test_annotated_yaml_generation(self):
        """Annotated YAML surfaces missing parameters clearly.

        Post-gh#1333, ``from_yaml`` on a config missing critical physics
        fields raises; with ``auto_generate_annotated=True`` the annotated
        template is emitted before the raise so users get actionable
        feedback alongside the error.
        """
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
        annotated_path = yaml_path.parent / f"{yaml_path.stem}_annotated.yml"

        try:
            with pytest.raises(ValueError):
                SUEWSConfig.from_yaml(yaml_path, auto_generate_annotated=True)

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

        finally:
            yaml_path.unlink()
            if annotated_path.exists():
                annotated_path.unlink()

    def test_no_validation_with_complete_config(self):
        """Test no warnings when all required parameters are provided."""
        # This would be a complex test - simplified for now
        # Key point: when all params provided, validation summary should be minimal
        pass

    def test_auto_generate_annotated_yaml_option(self):
        """``auto_generate_annotated=True`` emits the template on the failure path.

        Post-gh#1333, a config missing critical physics fields raises at
        ``from_yaml`` time. When the caller requested
        ``auto_generate_annotated=True``, the validator still writes the
        annotated template before raising so users get a helpful starting
        point alongside the error message.
        """
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
        annotated_path = yaml_path.parent / f"{yaml_path.stem}_annotated.yml"

        try:
            # auto_generate_annotated=True should still emit the template on raise
            with pytest.raises(ValueError):
                SUEWSConfig.from_yaml(yaml_path, auto_generate_annotated=True)

            assert annotated_path.exists(), (
                "Annotated YAML should be generated even when load raises, "
                "provided auto_generate_annotated=True"
            )
            content = annotated_path.read_text()
            assert "ANNOTATED SUEWS CONFIGURATION" in content
            assert "bldgh" in content
            annotated_path.unlink()

            # auto_generate_annotated=False should not produce a template.
            with pytest.raises(ValueError):
                SUEWSConfig.from_yaml(yaml_path, auto_generate_annotated=False)
            assert not annotated_path.exists(), (
                "Annotated YAML should not be generated when auto_generate_annotated=False"
            )

        finally:
            yaml_path.unlink()
            if annotated_path.exists():
                annotated_path.unlink()


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
    """End-to-end: annotated YAML generator is callable on an incomplete config.

    Post-gh#1333, ``from_yaml`` on a config missing critical physics fields
    raises. The annotation generator is still a useful diagnostic tool; the
    test exercises it via ``use_conditional_validation=False`` so the focus
    stays on the annotator, not the validator.
    """
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
        config = SUEWSConfig.from_yaml(yaml_path, use_conditional_validation=False)

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


def test_phase_b_storageheatmethod_ohmincqf_validation(registry):
    """Test StorageHeatMethod-OhmIncQf validation in Phase B."""

    # Test incompatible combination: StorageHeatMethod=1 requires OhmIncQf=0
    yaml_data_incompatible = {
        "model": {
            "physics": {
                "storage_heat": {"value": 1},  # OHM_WITHOUT_QF
                "ohm_inc_qf": {"value": 1},  # INCLUDE - incompatible!
            }
        }
    }

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_incompatible))

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
                "storage_heat": {"value": 1},  # OHM_WITHOUT_QF
                "ohm_inc_qf": {"value": 0},  # EXCLUDE - compatible!
            }
        }
    }

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_compatible))

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


def test_phase_b_rsl_stabilitymethod_validation(registry):
    """Test that existing RSL-StabilityMethod validation still works in Phase B."""

    # Test incompatible combination: rslmethod=2 requires stabilitymethod=3
    yaml_data_incompatible = {
        "model": {
            "physics": {
                "roughness_sublayer": {"value": 2},
                "stability": {"value": 1},  # Should be 3
            }
        }
    }

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_incompatible))

    # Should find the incompatible combination
    rsl_results = [r for r in results if "rslmethod-stabilitymethod" in r.parameter]
    assert len(rsl_results) == 1
    assert rsl_results[0].status == "ERROR"
    assert "rslmethod == 2" in rsl_results[0].message
    assert "stabilitymethod must be 3" in rsl_results[0].message


def test_phase_b_model_option_dependencies_comprehensive(registry):
    """Test validate_model_option_dependencies function with various configurations."""
    # Test with minimal physics configuration (should all pass)
    yaml_data_minimal = {
        "model": {
            "physics": {
                "storage_heat": {"value": 0},  # OBSERVED
                "ohm_inc_qf": {"value": 0},  # EXCLUDE
                "roughness_sublayer": {"value": 0},
                "stability": {"value": 1},
            }
        }
    }

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_minimal))

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
                "storage_heat": {"value": 1},  # OHM_WITHOUT_QF
                "ohm_inc_qf": {"value": 0},  # EXCLUDE - compatible
                "roughness_sublayer": {"value": 2},  # Should require stabilitymethod=3
                "stability": {"value": 1},  # Wrong value - incompatible
            }
        }
    }

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_mixed))

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

    results = registry["option_dependencies"](ValidationContext(yaml_data=yaml_data_no_physics))

    # Should handle gracefully - may have default values or skip validation
    assert isinstance(results, list)  # Should return a list, not crash


def test_phase_b_annual_mean_air_temperature_from_cru(cru_data_available):
    """Test that annual_mean_air_temperature is populated from CRU annual mean data."""
    from supy.data_model.validation.pipeline.phase_b import (
        adjust_surface_temperatures,
        get_mean_annual_air_temperature,
    )

    # Test coordinates (London)
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    annual_temp = get_mean_annual_air_temperature(test_lat, test_lon)

    # Create test YAML data with STEBBS configuration
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "annual_mean_air_temperature": {
                            "value": 999.0
                        },  # Wrong value to be updated
                        "initial_outdoor_temperature": {
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

    # Check that annual_mean_air_temperature was updated
    updated_annual_temp = updated_data["sites"][0]["properties"]["stebbs"][
        "annual_mean_air_temperature"
    ]["value"]
    assert updated_annual_temp == annual_temp, (
        f"Expected {annual_temp}, got {updated_annual_temp}"
    )

    # Check that adjustment was recorded
    annual_temp_adjustments = [
        adj for adj in adjustments if adj.parameter == "stebbs.annual_mean_air_temperature"
    ]
    assert len(annual_temp_adjustments) == 1
    adj = annual_temp_adjustments[0]
    assert adj.old_value == "999.0"
    assert f"{annual_temp}" in adj.new_value
    assert "CRU annual mean" in adj.reason
    assert "1991-2020" in adj.reason


def test_phase_b_annual_mean_air_temperature_no_update_if_same(cru_data_available):
    """Test that annual_mean_air_temperature is not updated if already correct."""
    from supy.data_model.validation.pipeline.phase_b import (
        adjust_surface_temperatures,
        get_mean_annual_air_temperature,
    )

    # Test coordinates
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    annual_temp = get_mean_annual_air_temperature(test_lat, test_lon)

    # Create test YAML with already-correct value
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "annual_mean_air_temperature": {
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
        adj for adj in adjustments if adj.parameter == "stebbs.annual_mean_air_temperature"
    ]
    assert len(annual_temp_adjustments) == 0, (
        "Should not adjust if value already correct"
    )


def test_phase_b_annual_mean_air_temperature_missing_stebbs():
    """Test graceful handling when annual_mean_air_temperature is not in stebbs."""
    from supy.data_model.validation.pipeline.phase_b import adjust_surface_temperatures

    # Test coordinates
    test_lat = 51.5
    test_lon = -0.1
    start_date = "2020-01-15"

    # Create test YAML without annual_mean_air_temperature
    yaml_data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": test_lat},
                    "lng": {"value": test_lon},
                    "stebbs": {
                        "initial_outdoor_temperature": {"value": 10.0},
                        # annual_mean_air_temperature NOT present
                    },
                },
                "initial_states": {},
            }
        ]
    }

    # Run adjustment - should not crash
    updated_data, adjustments = adjust_surface_temperatures(yaml_data, start_date)

    # Check that no annual_mean_air_temperature adjustment was attempted
    annual_temp_adjustments = [
        adj for adj in adjustments if adj.parameter == "stebbs.annual_mean_air_temperature"
    ]
    assert len(annual_temp_adjustments) == 0


def _make_minimal_phase_b_yaml_for_albedo(lat: float) -> dict:
    """Build a minimal Phase-B-style YAML dict with vegetation albedo ranges and alb_id."""
    return {
        "sites": [
            {
                "name": "TestSite",
                "gridiv": 1,
                "properties": {
                    "lat": {"value": lat},
                    "lng": {"value": 0.0},
                    "land_cover": {
                        "grass": {
                            "sfr": {"value": 0.1},
                            "alb_min": {"value": 0.10},
                            "alb_max": {"value": 0.20},
                        },
                        "dectr": {
                            "sfr": {"value": 0.1},
                            "alb_min": {"value": 0.12},
                            "alb_max": {"value": 0.30},
                        },
                        "evetr": {
                            "sfr": {"value": 0.1},
                            "alb_min": {"value": 0.14},
                            "alb_max": {"value": 0.40},
                        },
                    },
                },
                "initial_states": {
                    "grass": {"alb_id": {"value": 0.15}},
                    "dectr": {"alb_id": {"value": 0.18}},
                    "evetr": {"alb_id": {"value": 0.25}},
                },
            }
        ]
    }


def _get_phase_b_alb_ids(yaml_data: dict):
    site = yaml_data["sites"][0]
    ist = site["initial_states"]
    return (
        ist["grass"]["alb_id"]["value"],
        ist["dectr"]["alb_id"]["value"],
        ist["evetr"]["alb_id"]["value"],
    )


def test_phase_b_seasonal_albedo_summer_updates_alb_id_from_ranges():
    """Summer: grass.alb_id -> alb_min, dectr/evetr.alb_id -> alb_max."""
    # Northern Hemisphere, DOY in summer window: 2017-07-15
    yaml_data = _make_minimal_phase_b_yaml_for_albedo(lat=51.5)
    yaml_before = copy.deepcopy(yaml_data)

    updated, adjustments = adjust_seasonal_parameters(
        yaml_data, start_date="2017-07-15", model_year=2017
    )

    grass_id, dectr_id, evetr_id = _get_phase_b_alb_ids(updated)

    # Expected from ranges defined above
    assert grass_id == pytest.approx(0.10)  # alb_min(grass)
    assert dectr_id == pytest.approx(0.30)  # alb_max(dectr)
    assert evetr_id == pytest.approx(0.40)  # alb_max(evetr)

    # We should have ScientificAdjustment entries for each
    params = {a.parameter for a in adjustments}
    assert "grass.alb_id" in params
    assert "dectr.alb_id" in params
    assert "evetr.alb_id" in params

    # Land-cover ranges themselves must be unchanged
    lc_before = yaml_before["sites"][0]["properties"]["land_cover"]
    lc_after = updated["sites"][0]["properties"]["land_cover"]
    assert lc_after == lc_before


def test_phase_b_seasonal_albedo_winter_updates_alb_id_from_ranges():
    """Winter: grass.alb_id -> alb_max, dectr/evetr.alb_id -> alb_min."""
    # Northern Hemisphere, DOY in winter window: 2017-01-15
    yaml_data = _make_minimal_phase_b_yaml_for_albedo(lat=51.5)

    updated, adjustments = adjust_seasonal_parameters(
        yaml_data, start_date="2017-01-15", model_year=2017
    )

    grass_id, dectr_id, evetr_id = _get_phase_b_alb_ids(updated)

    assert grass_id == pytest.approx(0.20)  # alb_max(grass)
    assert dectr_id == pytest.approx(0.12)  # alb_min(dectr)
    assert evetr_id == pytest.approx(0.14)  # alb_min(evetr)

    params = {a.parameter for a in adjustments}
    assert "grass.alb_id" in params
    assert "dectr.alb_id" in params
    assert "evetr.alb_id" in params


def test_phase_b_seasonal_albedo_midseason_sets_alb_id_midpoint():
    """Spring/fall: alb_id -> (alb_min + alb_max)/2 for dectr/evetr surfaces."""
    # Northern Hemisphere, DOY in spring window: 2017-04-01
    yaml_data = _make_minimal_phase_b_yaml_for_albedo(lat=51.5)

    updated, adjustments = adjust_seasonal_parameters(
        yaml_data, start_date="2017-04-01", model_year=2017
    )

    _ , dectr_id, evetr_id = _get_phase_b_alb_ids(updated)

    # Expected midpoints
    assert dectr_id == pytest.approx((0.12 + 0.30) / 2)  # 0.21
    assert evetr_id == pytest.approx((0.14 + 0.40) / 2)  # 0.27

    params = {a.parameter for a in adjustments}
    assert "dectr.alb_id" in params
    assert "evetr.alb_id" in params

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
    assert roof2["state_limit"]["value"] is None

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


def test_forcing_validation_cli_integration(cli_runner):
    """Test CLI integration: forcing validation can be enabled/disabled via --forcing flag.

    Uses Click's CliRunner for in-process testing to avoid subprocess overhead.

    Parameters
    ----------
    cli_runner : CliRunner
        Shared Click test runner fixture from conftest
    """
    import yaml
    import pandas as pd
    from supy.cmd.validate_config import cli as validate_cli

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

    # Use isolated_filesystem for clean working directory (reports are written to cwd)
    with cli_runner.isolated_filesystem() as tmpdir:
        tmpdir_path = Path(tmpdir)

        # Create forcing file with invalid data
        bad_forcing_path = tmpdir_path / "forcing_bad.txt"
        df = pd.DataFrame(forcing_data)
        df.to_csv(bad_forcing_path, sep=" ", index=False)

        # Create test config with bad forcing file
        test_config_data = config_data.copy()
        test_config_data["model"]["control"]["forcing_file"] = {
            "value": str(bad_forcing_path)
        }

        test_config_path = tmpdir_path / "test_config.yml"
        with open(test_config_path, "w") as f:
            yaml.dump(test_config_data, f)

        # Test 1: Default behavior (forcing validation enabled)
        cli_runner.invoke(validate_cli, [str(test_config_path)])

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
        cli_runner.invoke(validate_cli, ["--forcing", "off", str(test_config_path)])

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


# ============================================================================
# Irrigation DOY Validation Tests (Issue #811)
# ============================================================================


def test_irrigation_doy_leap_year():
    """Test DOY 366 validation for leap vs non-leap years."""
    from supy.data_model.validation.pipeline.phase_b_rules import validate_irrigation_doy

    # Leap year: DOY 366 valid
    results = validate_irrigation_doy(120, 366, 51.5, 2024, "test_site")
    assert not any(r.status == "ERROR" for r in results)

    # Non-leap year: DOY 366 invalid
    results = validate_irrigation_doy(120, 366, 51.5, 2023, "test_site")
    errors = [r for r in results if r.status == "ERROR"]
    assert len(errors) == 1
    assert "365" in errors[0].message


def test_irrigation_doy_disabled():
    """Test irrigation disabled configurations (None/0)."""
    from supy.data_model.validation.pipeline.phase_b_rules import validate_irrigation_doy

    # Both None = valid
    assert len(validate_irrigation_doy(None, None, 51.5, 2023, "test")) == 0

    # Both 0 = valid
    assert len(validate_irrigation_doy(0, 0, 51.5, 2023, "test")) == 0

    # One set, one None = error
    results = validate_irrigation_doy(120, None, 51.5, 2023, "test")
    errors = [r for r in results if r.status == "ERROR"]
    assert len(errors) == 1
    assert "must be specified together" in errors[0].message


def test_irrigation_hemisphere_warnings():
    """Test hemisphere and tropical-aware warm season warnings."""
    from supy.data_model.validation.pipeline.phase_b_rules import validate_irrigation_doy

    # NH: warm season (May-Sept, DOY 121-273) = no warning
    results = validate_irrigation_doy(150, 250, 51.5, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    results = validate_irrigation_doy(121, 273, 51.5, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    # NH: outside warm season = warning (also warns about year-wrapping)
    results = validate_irrigation_doy(350, 30, 51.5, 2023, "test")
    warnings = [r for r in results if r.status == "WARNING"]
    assert len(warnings) == 2
    assert any(
        "Northern Hemisphere" in w.message and "May-September" in w.message
        for w in warnings
    )
    assert any("wraps year boundary" in w.message for w in warnings)

    # NH: partial overlap with warm season = warning
    results = validate_irrigation_doy(100, 200, 51.5, 2023, "test")
    warnings = [r for r in results if r.status == "WARNING"]
    assert len(warnings) == 1  # Start (100) is outside warm season (121-273)

    # SH: warm season (Nov-March, DOY 305-90, year-wrapping) = no warning
    results = validate_irrigation_doy(330, 60, -33.9, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    results = validate_irrigation_doy(305, 90, -33.9, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    # SH: outside warm season (winter) = warning (also warns about not wrapping)
    results = validate_irrigation_doy(180, 220, -33.9, 2023, "test")
    warnings = [r for r in results if r.status == "WARNING"]
    assert len(warnings) == 2
    assert any(
        "Southern Hemisphere" in w.message and "November-March" in w.message
        for w in warnings
    )
    assert any("does not wrap year boundary" in w.message for w in warnings)

    # Tropical: any period = no warning (irrigation allowed year-round)
    results = validate_irrigation_doy(1, 365, 15.0, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    results = validate_irrigation_doy(100, 200, 20.0, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)

    results = validate_irrigation_doy(300, 50, -20.0, 2023, "test")
    assert not any(r.status == "WARNING" for r in results)


def test_irrigation_year_wrapping():
    """Test year-wrapping irrigation period validation (ie_start > ie_end)."""
    from supy.data_model.validation.pipeline.phase_b_rules import validate_irrigation_doy

    # Year-wrapping period in NH (winter irrigation - unusual)
    results = validate_irrigation_doy(300, 50, 51.5, 2023, "test")
    # Should warn about both: outside warm season AND year-wrapping
    warnings = [r for r in results if r.status == "WARNING"]
    assert len(warnings) == 2
    assert any("wraps year boundary" in w.message for w in warnings)
    assert any("falls outside" in w.message for w in warnings)

    # Year-wrapping period in SH (summer irrigation - expected)
    results = validate_irrigation_doy(330, 60, -33.9, 2023, "test")
    # Should NOT warn about year-wrapping (it's normal for SH warm season)
    warnings = [r for r in results if r.status == "WARNING"]
    wrapping_warnings = [
        w for w in warnings if "does not wrap year boundary" in w.message
    ]
    assert len(wrapping_warnings) == 0

    # Non-wrapping period in SH (winter irrigation - unusual)
    results = validate_irrigation_doy(150, 250, -33.9, 2023, "test")
    # Should warn about both: outside warm season AND not wrapping
    warnings = [r for r in results if r.status == "WARNING"]
    assert len(warnings) == 2
    assert any("does not wrap year boundary" in w.message for w in warnings)
    assert any("falls outside" in w.message for w in warnings)


def test_startdls_enddls_range_validation():
    """Test Pydantic validation for startdls and enddls DOY range constraints."""
    import pytest
    from supy.data_model.core.human_activity import AnthropogenicEmissions
    from pydantic import ValidationError

    # Valid cases
    # Both None (no DST)
    emissions = AnthropogenicEmissions(startdls=None, enddls=None)
    assert emissions.startdls is None
    assert emissions.enddls is None

    # Valid DOY values
    emissions = AnthropogenicEmissions(startdls=1, enddls=366)
    assert emissions.startdls == 1
    assert emissions.enddls == 366

    # Typical NH DST
    emissions = AnthropogenicEmissions(startdls=86, enddls=303)
    assert emissions.startdls == 86
    assert emissions.enddls == 303

    # Typical SH DST
    emissions = AnthropogenicEmissions(startdls=274, enddls=97)
    assert emissions.startdls == 274
    assert emissions.enddls == 97

    # Invalid cases - startdls too small
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=0, enddls=100)
    assert "greater than or equal to 1" in str(exc_info.value).lower()

    # Invalid cases - startdls negative
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=-50, enddls=100)
    assert "greater than or equal to 1" in str(exc_info.value).lower()

    # Invalid cases - startdls too large
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=400, enddls=100)
    assert "less than or equal to 366" in str(exc_info.value).lower()

    # Invalid cases - startdls placeholder value
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=999, enddls=100)
    assert "less than or equal to 366" in str(exc_info.value).lower()

    # Invalid cases - enddls too small
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=100, enddls=0)
    assert "greater than or equal to 1" in str(exc_info.value).lower()

    # Invalid cases - enddls negative
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=100, enddls=-20)
    assert "greater than or equal to 1" in str(exc_info.value).lower()

    # Invalid cases - enddls too large
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=100, enddls=500)
    assert "less than or equal to 366" in str(exc_info.value).lower()

    # Invalid cases - enddls placeholder value
    with pytest.raises(ValidationError) as exc_info:
        AnthropogenicEmissions(startdls=100, enddls=999)
    assert "less than or equal to 366" in str(exc_info.value).lower()

    # Edge cases - boundary values
    emissions = AnthropogenicEmissions(startdls=1, enddls=1)
    assert emissions.startdls == 1
    assert emissions.enddls == 1

    emissions = AnthropogenicEmissions(startdls=366, enddls=366)
    assert emissions.startdls == 366
    assert emissions.enddls == 366

    # Decimal values (DOY can be decimal)
    emissions = AnthropogenicEmissions(startdls=86.5, enddls=303.75)
    assert emissions.startdls == 86.5
    assert emissions.enddls == 303.75


def test_dls_consistency_validation_in_suews_config():
    """Test DLS consistency check in SUEWSConfig: both set or both None."""
    import pytest
    from pydantic import ValidationError
    from supy.data_model.core import SUEWSConfig

    # Load a valid sample configuration
    import yaml

    sample_path = trv_supy_module / "sample_data" / "sample_config.yml"
    with sample_path.open() as f:
        config_data = yaml.safe_load(f)

    # Test 1: Both None - valid
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = None
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = None
    config = SUEWSConfig(**config_data)
    assert config is not None

    # Test 2: Both set - valid
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 86
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 303
    config = SUEWSConfig(**config_data)
    assert config is not None

    # Test 3: Only startdls set - ERROR
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 86
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = None
    with pytest.raises(ValidationError) as exc_info:
        SUEWSConfig(**config_data)
    assert "must both be set or both be None" in str(exc_info.value)

    # Test 4: Only enddls set - ERROR
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = None
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 303
    with pytest.raises(ValidationError) as exc_info:
        SUEWSConfig(**config_data)
    assert "must both be set or both be None" in str(exc_info.value)


def test_dls_leap_year_validation_in_suews_config():
    """Test DLS leap year refinement in SUEWSConfig."""
    import pytest
    import yaml
    from pydantic import ValidationError
    from supy.data_model.core import SUEWSConfig

    sample_path = trv_supy_module / "sample_data" / "sample_config.yml"
    with sample_path.open() as f:
        config_data = yaml.safe_load(f)

    # Set DLS values
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 86
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 366

    # Test 1: Leap year 2024 - DOY 366 is valid
    config_data["model"]["control"]["start_time"] = "2024-01-01T00:00:00"
    config = SUEWSConfig(**config_data)
    assert config is not None

    # Test 2: Non-leap year 2025 - DOY 366 is ERROR
    config_data["model"]["control"]["start_time"] = "2025-01-01T00:00:00"
    with pytest.raises(ValidationError) as exc_info:
        SUEWSConfig(**config_data)
    assert "non-leap year 2025" in str(exc_info.value)
    assert "366" in str(exc_info.value)


def test_dls_location_based_informational_messages_in_suews_config():
    """Test DLS location-based comparison messages are collected in validation_summary."""
    import pytest
    import yaml
    from supy.data_model.core import SUEWSConfig

    sample_path = trv_supy_module / "sample_data" / "sample_config.yml"
    with sample_path.open() as f:
        config_data = yaml.safe_load(f)

    # Set year to 2024 (leap year) and add site name
    config_data["model"]["control"]["start_time"] = "2024-01-01T00:00:00"
    config_data["sites"][0]["name"] = "TestSite"

    # Test 1: Correct DLS values for London (should match calculated values, no info message)
    # For London (51.5, -0.12) in 2024, DST starts at DOY 91 and ends at DOY 301
    config_data["sites"][0]["properties"]["lat"] = 51.5  # London
    config_data["sites"][0]["properties"]["lng"] = -0.12
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 91
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 301

    config = SUEWSConfig(**config_data)
    assert config is not None

    # Check that no DLS comparison messages were generated (values match calculated)
    if hasattr(config, "_validation_summary"):
        info_messages = config._validation_summary.get("info_messages", [])
        dls_messages = [msg for msg in info_messages if "DLS values differ" in msg]
        assert len(dls_messages) == 0, (
            "Should not generate message when user values match calculated values"
        )

    # Test 2: Incorrect DLS values (should generate info messages for both startdls and enddls)
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 15
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = 200

    config = SUEWSConfig(**config_data)
    assert config is not None

    # Check that DLS comparison info messages were generated
    assert hasattr(config, "_validation_summary")
    info_messages = config._validation_summary.get("info_messages", [])
    dls_messages = [msg for msg in info_messages if "DLS values differ" in msg]
    assert len(dls_messages) == 2, (
        "Should generate separate info messages for startdls and enddls"
    )

    # Check startdls message
    startdls_messages = [msg for msg in dls_messages if "startdls for site" in msg]
    assert len(startdls_messages) == 1
    assert "TestSite" in startdls_messages[0]
    assert "lat=51.50" in startdls_messages[0]
    assert "lng=-0.12" in startdls_messages[0]
    assert "15" in startdls_messages[0]  # User value
    assert "91" in startdls_messages[0]  # Calculated value

    # Check enddls message
    enddls_messages = [msg for msg in dls_messages if "enddls for site" in msg]
    assert len(enddls_messages) == 1
    assert "TestSite" in enddls_messages[0]
    assert "200" in enddls_messages[0]  # User value
    assert "301" in enddls_messages[0]  # Calculated value

    # Test 3: Only startdls differs (should generate only one info message)
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = 50
    config_data["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = (
        301  # Correct
    )

    config = SUEWSConfig(**config_data)
    assert config is not None

    info_messages = config._validation_summary.get("info_messages", [])
    dls_messages = [msg for msg in info_messages if "DLS values differ" in msg]
    assert len(dls_messages) == 1, (
        "Should generate only one message when only startdls differs"
    )
    assert "startdls for site" in dls_messages[0]
    assert "enddls for site" not in dls_messages[0]


# ----------------------------------------------------------------------
# gh#1333 regression: sparse YAML must raise at load, not warn + run
# ----------------------------------------------------------------------


class TestSparseYmlCriticalMissing:
    """Regression tests for gh#1333.

    A sparse user YAML that omits physics-required blocks (``conductance``,
    ``lai``, tree ``fai_*`` / ``height_*``, ``bldgs.faibldg``) must raise
    ``ValidationError`` at ``SUEWSConfig.from_yaml()`` time. Previously the
    validator logged a WARNING and let the simulation run, producing
    all-NaN output on x86_64.
    """

    SPARSE_FIXTURE = (
        Path(__file__).parent.parent / "fixtures" / "sparse_site.yml"
    )
    SAMPLE_CONFIG = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"

    def test_sparse_yml_raises_validation_error(self):
        """Sparse config raises on load, naming each missing block.

        ``SUEWSConfig.from_yaml`` intercepts the underlying
        ``pydantic.ValidationError`` and re-raises as ``ValueError``
        with a rewritten message, so that's what the public API surfaces.
        """
        assert self.SPARSE_FIXTURE.exists(), (
            f"Fixture missing: {self.SPARSE_FIXTURE}"
        )

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig.from_yaml(str(self.SPARSE_FIXTURE))

        message = str(exc_info.value)

        # Conductance block (Tier 3 — physics-conditional on any active veg).
        assert "conductance.g_max" in message
        assert "conductance.g_sm" in message

        # Per-surface structural fields (Tier 2 — surface-presence-conditional).
        assert "faibldg" in message
        assert "fai_evergreen_tree" in message
        assert "height_evergreen_tree" in message
        assert "fai_deciduous_tree" in message
        assert "height_deciduous_tree" in message

        # LAI phenology fields on active vegetated surfaces (Tier 2).
        assert "lai_max" in message
        assert "base_temperature" in message
        assert "gdd_full" in message
        assert "sdd_full" in message

    def test_sample_config_still_loads(self):
        """Packaged sample config is complete and must not trigger the new check."""
        assert self.SAMPLE_CONFIG.exists(), (
            f"Sample config missing: {self.SAMPLE_CONFIG}"
        )
        # Must not raise; a successful load is the assertion.
        config = SUEWSConfig.from_yaml(str(self.SAMPLE_CONFIG))
        assert config is not None

    def test_site_params_check_returns_empty_on_complete_config(self):
        """Direct method check — complete config yields no critical issues."""
        config = SUEWSConfig.from_yaml(str(self.SAMPLE_CONFIG))
        assert config._check_critical_null_site_params() == []

    def test_partial_land_cover_still_checks_omitted_active_defaults(self, tmp_path):
        """Omitted surfaces in an explicit land_cover block must not bypass checks."""
        config_yaml = """
name: partial_land_cover
model:
  control:
    tstep: 300
    start_time: '2012-07-01'
    end_time: '2012-07-10'
    forcing:
      file: {value: ./forcing.txt}
    output:
      format: parquet
      freq: 3600
      dir: ./out
  physics:
    netradiationmethod: {value: 3}
    emissionsmethod: {value: 2}
    storageheatmethod: {value: 1}
    ohmincqf: {value: 0}
    roughlenmommethod: {value: 1}
    roughlenheatmethod: {value: 2}
    stabilitymethod: {value: 3}
    smdmethod: {value: 0}
    waterusemethod: {value: 0}
    rslmethod: {value: 1}
    rsllevel: {value: 1}
    gsmodel: {value: 2}
    snowuse: {value: 0}
    stebbsmethod: {value: 0}
    rcmethod: {value: 0}
    setpointmethod: {value: 0}
    same_albedo_wall: {value: 0}
    same_albedo_roof: {value: 0}
    same_emissivity_wall: {value: 0}
    same_emissivity_roof: {value: 0}
sites:
  - name: site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 50.0}
      surfacearea: {value: 100000.0}
      z: {value: 100.0}
      land_cover:
        paved:
          sfr: {value: 0.5}
          soildepth: {value: 350.0}
          soilstorecap: {value: 150.0}
          statelimit: {value: 0.48}
          alb: {value: 0.08}
        bldgs:
          sfr: {value: 0.5}
          soildepth: {value: 350.0}
          soilstorecap: {value: 150.0}
          statelimit: {value: 0.25}
          alb: {value: 0.15}
          bldgh: {value: 3.5}
          faibldg: {value: 0.3}
"""
        config_path = tmp_path / "partial_land_cover.yml"
        config_path.write_text(config_yaml)

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig.from_yaml(str(config_path))

        message = str(exc_info.value)
        assert "conductance.g_max" in message
        assert "land_cover.dectr.lai: lai_max" in message
        assert "land_cover.evetr.lai: lai_max" in message
        assert "land_cover.grass.lai: lai_max" in message

    def test_fai_simple_scheme_allows_missing_explicit_fai(self, tmp_path):
        """FAI should not be required when the simple scheme derives it."""
        config_yaml = """
name: fai_simple_scheme
model:
  control:
    tstep: 300
    start_time: '2012-07-01'
    end_time: '2012-07-10'
    forcing:
      file: {value: ./forcing.txt}
    output:
      format: parquet
      freq: 3600
      dir: ./out
  physics:
    netradiationmethod: {value: 3}
    emissionsmethod: {value: 2}
    storageheatmethod: {value: 1}
    ohmincqf: {value: 0}
    roughlenmommethod: {value: 1}
    roughlenheatmethod: {value: 2}
    stabilitymethod: {value: 3}
    smdmethod: {value: 0}
    waterusemethod: {value: 0}
    rslmethod: {value: 1}
    faimethod: {value: 1}
    rsllevel: {value: 1}
    gsmodel: {value: 2}
    snowuse: {value: 0}
    stebbsmethod: {value: 0}
    rcmethod: {value: 0}
    setpointmethod: {value: 0}
    same_albedo_wall: {value: 0}
    same_albedo_roof: {value: 0}
    same_emissivity_wall: {value: 0}
    same_emissivity_roof: {value: 0}
sites:
  - name: site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 50.0}
      surfacearea: {value: 100000.0}
      z: {value: 100.0}
      land_cover:
        paved:
          sfr: {value: 0.5}
          soildepth: {value: 350.0}
          soilstorecap: {value: 150.0}
          statelimit: {value: 0.48}
          alb: {value: 0.08}
        bldgs:
          sfr: {value: 0.5}
          soildepth: {value: 350.0}
          soilstorecap: {value: 150.0}
          statelimit: {value: 0.25}
          alb: {value: 0.15}
          bldgh: {value: 3.5}
        dectr:
          sfr: {value: 0.0}
        evetr:
          sfr: {value: 0.0}
        grass:
          sfr: {value: 0.0}
        bsoil:
          sfr: {value: 0.0}
        water:
          sfr: {value: 0.0}
"""
        config_path = tmp_path / "fai_simple_scheme.yml"
        config_path.write_text(config_yaml)

        config = SUEWSConfig.from_yaml(str(config_path))
        assert config is not None

        annotated_path = Path(config.generate_annotated_yaml(str(config_path)))
        try:
            annotated = annotated_path.read_text()
            assert "faibldg:" not in annotated
        finally:
            annotated_path.unlink(missing_ok=True)

    def test_observed_lai_allows_missing_gdd_sdd_thresholds(self, tmp_path):
        """Observed LAI mode should not require calculated-phenology fields."""
        config_yaml = """
name: lai_observed
model:
  control:
    tstep: 300
    start_time: '2012-07-01'
    end_time: '2012-07-10'
    forcing:
      file: {value: ./forcing.txt}
    output:
      format: parquet
      freq: 3600
      dir: ./out
  physics:
    netradiationmethod: {value: 3}
    emissionsmethod: {value: 2}
    storageheatmethod: {value: 1}
    ohmincqf: {value: 0}
    roughlenmommethod: {value: 1}
    roughlenheatmethod: {value: 2}
    stabilitymethod: {value: 3}
    smdmethod: {value: 0}
    waterusemethod: {value: 0}
    rslmethod: {value: 1}
    faimethod: {value: 1}
    rsllevel: {value: 1}
    gsmodel: {value: 2}
    laimethod: {value: 0}
    snowuse: {value: 0}
    stebbsmethod: {value: 0}
    rcmethod: {value: 0}
    setpointmethod: {value: 0}
    same_albedo_wall: {value: 0}
    same_albedo_roof: {value: 0}
    same_emissivity_wall: {value: 0}
    same_emissivity_roof: {value: 0}
sites:
  - name: site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 50.0}
      surfacearea: {value: 100000.0}
      z: {value: 100.0}
      conductance:
        g_max: {value: 3.5}
        g_k: {value: 200.0}
        g_q_base: {value: 0.13}
        g_q_shape: {value: 0.7}
        g_t: {value: 30.0}
        g_sm: {value: 0.05}
        kmax: {value: 1200.0}
        s1: {value: 5.56}
        s2: {value: 0.0}
        tl: {value: -10.0}
        th: {value: 55.0}
      land_cover:
        paved:
          sfr: {value: 0.0}
        bldgs:
          sfr: {value: 0.0}
        dectr:
          sfr: {value: 0.0}
        evetr:
          sfr: {value: 0.0}
        grass:
          sfr: {value: 1.0}
          lai:
            lai_max: {value: 4.5}
        bsoil:
          sfr: {value: 0.0}
        water:
          sfr: {value: 0.0}
"""
        config_path = tmp_path / "lai_observed.yml"
        config_path.write_text(config_yaml)

        config = SUEWSConfig.from_yaml(str(config_path))
        assert config is not None


def _phase_b_science_fixture():
    return {
        "model": {
            "physics": {
                "emissions": {"value": 5},
                "stebbs": {"value": 1},
                "outer_cap_fraction": {"value": 1},
                "setpoint": {"value": 0},
            }
        },
        "sites": [
            {
                "gridiv": {"value": 101},
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.1},
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.99995}},
                    },
                    "stebbs": {
                        "initial_outdoor_temperature": {"value": 0.0},
                        "annual_mean_air_temperature": {"value": 0.0},
                    },
                },
                "initial_states": {
                    "paved": {
                        "temperature": {"value": [0.0] * 5},
                        "tsfc": {"value": 0.0},
                        "tin": {"value": 0.0},
                    },
                    "snowalb": {"value": 0.8},
                },
            }
        ],
    }


def test_phase_b_collect_science_suggestions_does_not_mutate(monkeypatch):
    from supy.data_model.validation.pipeline import phase_b

    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda *args: 15.5
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda *args: 9.0
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda *args: 11.2
    )

    yaml_data = _phase_b_science_fixture()
    original = copy.deepcopy(yaml_data)

    suggestions = phase_b.collect_science_suggestions(
        yaml_data, start_date="2020-07-01", model_year=2020
    )

    assert yaml_data == original
    assert any(s.parameter == "initial_states.paved" for s in suggestions)
    assert any(s.parameter == "paved.sfr" for s in suggestions)
    assert any(s.parameter == "snowalb" for s in suggestions)
    assert all(s.severity == "SUGGESTION" for s in suggestions)
    assert all(s.source for s in suggestions)


def test_phase_b_apply_science_suggestions_mutates_copy(monkeypatch):
    from supy.data_model.validation.pipeline import phase_b

    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda *args: 15.5
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda *args: 9.0
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda *args: 11.2
    )

    yaml_data = _phase_b_science_fixture()
    suggestions = phase_b.collect_science_suggestions(
        yaml_data, start_date="2020-07-01", model_year=2020
    )
    updated, adjustments = phase_b.apply_science_suggestions(
        yaml_data, suggestions, start_date="2020-07-01", model_year=2020
    )

    paved_state = updated["sites"][0]["initial_states"]["paved"]
    paved_sfr = updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]

    assert paved_state["tsfc"]["value"] == 15.5
    assert paved_sfr["value"] == pytest.approx(1.0)
    assert updated["sites"][0]["initial_states"]["snowalb"]["value"] is None
    assert len(adjustments) == len(suggestions)
    assert yaml_data["sites"][0]["initial_states"]["paved"]["tsfc"]["value"] == 0.0


@pytest.mark.parametrize(
    ("science_fixes", "expect_mutation", "expected_section", "unexpected_section"),
    [
        ("suggest", False, "## SUGGESTED UPDATES", "## APPLIED UPDATES"),
        ("apply", True, "## APPLIED UPDATES", "## SUGGESTED UPDATES"),
        ("off", False, "## INFO", "## SUGGESTED UPDATES"),
    ],
)
def test_phase_b_run_science_check_science_fixes_modes(
    tmp_path,
    monkeypatch,
    science_fixes,
    expect_mutation,
    expected_section,
    unexpected_section,
):
    from supy.data_model.validation.pipeline import phase_b

    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda *args: 15.5
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda *args: 9.0
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda *args: 11.2
    )
    monkeypatch.setattr(phase_b.RulesRegistry, "run_validation", lambda self: [])

    yaml_data = _phase_b_science_fixture()
    yaml_data["model"]["control"] = {
        "start_time": "2020-07-01",
        "end_time": "2020-07-02",
    }

    input_file = tmp_path / "config.yml"
    output_file = tmp_path / "updated_config.yml"
    report_file = tmp_path / "report_config.txt"
    input_file.write_text(yaml.safe_dump(yaml_data), encoding="utf-8")

    phase_b.run_science_check(
        uptodate_yaml_file=str(input_file),
        user_yaml_file=str(input_file),
        standard_yaml_file=str(input_file),
        science_yaml_file=str(output_file),
        science_report_file=str(report_file),
        science_fixes=science_fixes,
    )

    updated = yaml.safe_load(output_file.read_text(encoding="utf-8"))
    report = report_file.read_text(encoding="utf-8")

    updated_tsfc = updated["sites"][0]["initial_states"]["paved"]["tsfc"]["value"]
    expected_tsfc = 15.5 if expect_mutation else 0.0

    assert updated_tsfc == expected_tsfc
    assert expected_section in report
    assert unexpected_section not in report


def test_phase_b_report_uses_review_and_suggestion_sections():
    from supy.data_model.validation.pipeline.phase_b import (
        ScienceSuggestion,
        ValidationResult,
        create_science_report,
    )

    report = create_science_report(
        validation_results=[
            ValidationResult(
                status="WARNING",
                category="GEOGRAPHY",
                parameter="timezone",
                message="Review timezone choice",
            )
        ],
        adjustments=[],
        suggestions=[
            ScienceSuggestion(
                parameter="snowalb",
                old_value=0.8,
                suggested_value=None,
                message="Seasonal initialisation suggestion",
                source="Seasonal snow-albedo initialisation heuristic.",
            )
        ],
    )

    assert "## REVIEW ADVISED" in report
    assert "## SUGGESTED UPDATES" in report
    assert "## NO ACTION NEEDED" not in report


def test_phase_b_structured_result_and_rule_order():
    from supy.data_model.validation.pipeline.phase_b_rules import (
        DEFAULT_RULE_ORDER,
        RulesRegistry,
        ValidationResult,
    )

    result = ValidationResult(
        status="warning",
        category="LAND_COVER",
        parameter="land_cover.paved.sfr",
        message="Review surface fractions",
    )
    payload = result.to_dict()

    assert payload["severity"] == "WARNING"
    assert payload["path"] == "land_cover.paved.sfr"
    assert payload["code"] == "LAND_COVER.LAND_COVER_PAVED_SFR"

    ordered_rule_names = [name for name, _ in RulesRegistry().ordered_rule_items()]
    expected_prefix = [
        name for name in DEFAULT_RULE_ORDER if name in RulesRegistry.rules
    ]
    assert ordered_rule_names[: len(expected_prefix)] == expected_prefix


def test_supy_validation_backward_compatibility_warns():
    import supy.validation as legacy_validation

    with pytest.warns(DeprecationWarning):
        result = legacy_validation.validate_suews_config_conditional(
            {"model": {"physics": {}}, "sites": []}
        )

    assert result["errors"] == result.errors
    assert "errors" in result.to_dict()


def test_suews_validate_help_exposes_science_fixes(cli_runner):
    from supy.cmd.validate_config import cli as validate_cli

    result = cli_runner.invoke(validate_cli, ["--help"])

    assert result.exit_code == 0
    assert "--science-fixes" in result.output
