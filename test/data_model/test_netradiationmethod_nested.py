"""Tests for nested netradiationmethod configuration (Issue #972).

This module tests the orthogonal decomposition of netradiationmethod into
physics (obs/narp/spartacus) and longwave (obs/cloud/air) dimensions.
"""

import pytest
import yaml
import tempfile
from pathlib import Path

from supy.data_model.core.model import ModelPhysics, NetRadiationMethod
from supy.data_model.core.physics_options import (
    NetRadiationMethodConfig,
    RadiationPhysics,
    LongwaveSource,
    code_to_dimensions,
    dimensions_to_code,
)


class TestDimensionMappings:
    """Test mapping between dimensions and numeric codes."""

    @pytest.mark.parametrize(
        "code,expected_physics,expected_longwave",
        [
            (0, "obs", None),
            (1, "narp", "obs"),
            (2, "narp", "cloud"),
            (3, "narp", "air"),
            (1001, "spartacus", "obs"),
            (1002, "spartacus", "cloud"),
            (1003, "spartacus", "air"),
            # Deprecated codes should map to narp equivalents
            (11, "narp", "obs"),
            (12, "narp", "cloud"),
            (13, "narp", "air"),
            (100, "narp", "obs"),
            (200, "narp", "cloud"),
            (300, "narp", "air"),
        ],
    )
    def test_code_to_dimensions(self, code, expected_physics, expected_longwave):
        """Test converting numeric codes to dimension tuples."""
        physics, longwave = code_to_dimensions(code)
        assert physics == expected_physics
        assert longwave == expected_longwave

    @pytest.mark.parametrize(
        "physics,longwave,expected_code",
        [
            ("obs", None, 0),
            ("narp", "obs", 1),
            ("narp", "cloud", 2),
            ("narp", "air", 3),
            ("spartacus", "obs", 1001),
            ("spartacus", "cloud", 1002),
            ("spartacus", "air", 1003),
        ],
    )
    def test_dimensions_to_code(self, physics, longwave, expected_code):
        """Test converting dimension tuples to numeric codes."""
        code = dimensions_to_code(physics, longwave)
        assert code == expected_code

    def test_invalid_code_raises(self):
        """Test that invalid codes raise ValueError."""
        with pytest.raises(ValueError, match="Unknown netradiationmethod code"):
            code_to_dimensions(999)

    def test_invalid_combination_raises(self):
        """Test that invalid combinations raise ValueError."""
        with pytest.raises(ValueError, match="Invalid combination"):
            dimensions_to_code("narp", None)  # narp requires longwave


class TestNetRadiationMethodConfig:
    """Test NetRadiationMethodConfig container."""

    def test_create_obs_config(self):
        """Test creating obs physics config."""
        config = NetRadiationMethodConfig(physics=RadiationPhysics.OBS)
        assert config.physics == RadiationPhysics.OBS
        assert config.longwave is None
        assert config.int_value == 0

    def test_create_narp_config(self):
        """Test creating narp physics config."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.NARP, longwave=LongwaveSource.AIR
        )
        assert config.physics == RadiationPhysics.NARP
        assert config.longwave == LongwaveSource.AIR
        assert config.int_value == 3

    def test_create_spartacus_config(self):
        """Test creating spartacus physics config."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.SPARTACUS, longwave=LongwaveSource.CLOUD
        )
        assert config.physics == RadiationPhysics.SPARTACUS
        assert config.longwave == LongwaveSource.CLOUD
        assert config.int_value == 1002

    def test_obs_clears_longwave(self):
        """Test that obs physics clears longwave if provided."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.OBS, longwave=LongwaveSource.AIR
        )
        assert config.longwave is None

    def test_narp_requires_longwave(self):
        """Test that narp requires longwave."""
        with pytest.raises(ValueError, match="'longwave' is required"):
            NetRadiationMethodConfig(physics=RadiationPhysics.NARP, longwave=None)

    def test_spartacus_requires_longwave(self):
        """Test that spartacus requires longwave."""
        with pytest.raises(ValueError, match="'longwave' is required"):
            NetRadiationMethodConfig(physics=RadiationPhysics.SPARTACUS, longwave=None)

    def test_from_code(self):
        """Test creating config from legacy numeric code."""
        config = NetRadiationMethodConfig.from_code(3)
        assert config.physics == RadiationPhysics.NARP
        assert config.longwave == LongwaveSource.AIR
        assert config.int_value == 3

    def test_from_code_spartacus(self):
        """Test creating config from spartacus code."""
        config = NetRadiationMethodConfig.from_code(1001)
        assert config.physics == RadiationPhysics.SPARTACUS
        assert config.longwave == LongwaveSource.OBS
        assert config.int_value == 1001

    def test_int_conversion(self):
        """Test int() conversion."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.NARP, longwave=LongwaveSource.CLOUD
        )
        assert int(config) == 2

    def test_model_dump_json_mode(self):
        """Test model_dump outputs clean dimension form in JSON mode."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.NARP, longwave=LongwaveSource.AIR
        )
        dumped = config.model_dump(mode="json")
        assert dumped == {"physics": "narp", "longwave": "air"}

    def test_model_dump_json_mode_obs(self):
        """Test model_dump for obs physics (no longwave)."""
        config = NetRadiationMethodConfig(physics=RadiationPhysics.OBS)
        dumped = config.model_dump(mode="json")
        assert dumped == {"physics": "obs"}


class TestModelPhysicsBackwardCompatibility:
    """Test backward compatibility with legacy input forms."""

    def test_legacy_refvalue_form(self):
        """Test {'value': N} form is accepted."""
        physics = ModelPhysics(netradiationmethod={"value": 3})
        assert physics.netradiationmethod.int_value == 3
        assert physics.netradiationmethod.physics == RadiationPhysics.NARP
        assert physics.netradiationmethod.longwave == LongwaveSource.AIR

    def test_legacy_plain_int(self):
        """Test plain integer is accepted."""
        physics = ModelPhysics(netradiationmethod=3)
        assert physics.netradiationmethod.int_value == 3

    def test_legacy_enum_member(self):
        """Test enum member is accepted."""
        physics = ModelPhysics(netradiationmethod=NetRadiationMethod.LDOWN_AIR)
        assert physics.netradiationmethod.int_value == 3

    def test_dimension_based_form(self):
        """Test dimension-based form is accepted."""
        physics = ModelPhysics(
            netradiationmethod={"physics": "narp", "longwave": "air"}
        )
        assert physics.netradiationmethod.int_value == 3
        assert physics.netradiationmethod.physics == RadiationPhysics.NARP

    def test_dimension_based_obs(self):
        """Test dimension-based obs form."""
        physics = ModelPhysics(netradiationmethod={"physics": "obs"})
        assert physics.netradiationmethod.int_value == 0
        assert physics.netradiationmethod.physics == RadiationPhysics.OBS

    def test_dimension_based_spartacus(self):
        """Test dimension-based spartacus form."""
        physics = ModelPhysics(
            netradiationmethod={"physics": "spartacus", "longwave": "cloud"}
        )
        assert physics.netradiationmethod.int_value == 1002

    def test_config_instance_passthrough(self):
        """Test NetRadiationMethodConfig instance is passed through."""
        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.NARP, longwave=LongwaveSource.OBS
        )
        physics = ModelPhysics(netradiationmethod=config)
        assert physics.netradiationmethod.int_value == 1

    def test_deprecated_code_mapping(self):
        """Test deprecated codes are mapped correctly."""
        physics = ModelPhysics(netradiationmethod={"value": 100})
        assert physics.netradiationmethod.int_value == 1  # Maps to narp+obs


class TestEnumDimensionProperties:
    """Test dimension properties on NetRadiationMethod enum."""

    @pytest.mark.parametrize(
        "member,expected_physics",
        [
            (NetRadiationMethod.OBSERVED, "obs"),
            (NetRadiationMethod.LDOWN_OBSERVED, "narp"),
            (NetRadiationMethod.LDOWN_CLOUD, "narp"),
            (NetRadiationMethod.LDOWN_AIR, "narp"),
            (NetRadiationMethod.LDOWN_SS_OBSERVED, "spartacus"),
            (NetRadiationMethod.LDOWN_SS_CLOUD, "spartacus"),
            (NetRadiationMethod.LDOWN_SS_AIR, "spartacus"),
        ],
    )
    def test_physics_property(self, member, expected_physics):
        """Test enum physics property."""
        assert member.physics == expected_physics

    @pytest.mark.parametrize(
        "member,expected_longwave",
        [
            (NetRadiationMethod.OBSERVED, None),
            (NetRadiationMethod.LDOWN_OBSERVED, "obs"),
            (NetRadiationMethod.LDOWN_CLOUD, "cloud"),
            (NetRadiationMethod.LDOWN_AIR, "air"),
            (NetRadiationMethod.LDOWN_SS_OBSERVED, "obs"),
            (NetRadiationMethod.LDOWN_SS_CLOUD, "cloud"),
            (NetRadiationMethod.LDOWN_SS_AIR, "air"),
        ],
    )
    def test_longwave_property(self, member, expected_longwave):
        """Test enum longwave property."""
        assert member.longwave == expected_longwave


class TestDataFrameConversion:
    """Test DataFrame conversion preserves integer values."""

    @pytest.mark.smoke
    def test_to_df_state_extracts_int(self):
        """Test that to_df_state extracts integer value."""
        physics = ModelPhysics(
            netradiationmethod={"physics": "narp", "longwave": "air"}
        )
        df = physics.to_df_state(grid_id=1)
        assert df.loc[1, ("netradiationmethod", "0")] == 3

    @pytest.mark.smoke
    def test_to_df_state_spartacus(self):
        """Test to_df_state with spartacus."""
        physics = ModelPhysics(
            netradiationmethod={"physics": "spartacus", "longwave": "cloud"}
        )
        df = physics.to_df_state(grid_id=1)
        assert df.loc[1, ("netradiationmethod", "0")] == 1002

    @pytest.mark.smoke
    def test_from_df_state_creates_config(self):
        """Test that from_df_state creates NetRadiationMethodConfig."""
        # Create a physics object and convert to DataFrame
        physics = ModelPhysics(netradiationmethod={"physics": "narp", "longwave": "air"})
        df = physics.to_df_state(grid_id=1)

        # Reconstruct from DataFrame
        physics_reconst = ModelPhysics.from_df_state(df, grid_id=1)

        # Verify type and value
        assert isinstance(
            physics_reconst.netradiationmethod, NetRadiationMethodConfig
        )
        assert physics_reconst.netradiationmethod.int_value == 3
        assert physics_reconst.netradiationmethod.physics == RadiationPhysics.NARP

    @pytest.mark.smoke
    def test_roundtrip_preserves_value(self):
        """Test DataFrame roundtrip preserves netradiationmethod value."""
        original = ModelPhysics(
            netradiationmethod={"physics": "spartacus", "longwave": "obs"}
        )
        df = original.to_df_state(grid_id=1)
        reconst = ModelPhysics.from_df_state(df, grid_id=1)
        assert reconst.netradiationmethod.int_value == original.netradiationmethod.int_value
