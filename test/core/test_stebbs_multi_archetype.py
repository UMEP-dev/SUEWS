"""Tests for multi-building-archetype support in STEBBS (GH#360).

This module tests the multi-archetype functionality including:
- Python/Pydantic data model for multiple archetypes
- DataFrame serialisation and round-trip
- Validation of sfr (surface fraction) sum
- Per-archetype output variable generation
- Backwards compatibility with single archetype configuration
"""

import pytest

from supy.data_model.core.site import (
    ArchetypeProperties,
    SiteProperties,
)
from supy.data_model.core.type import RefValue
from supy.data_model.output.stebbs_vars import (
    STEBBS_ARCHETYPE_KEY_VARS,
    generate_archetype_variables,
)
from supy.data_model.output.variables import OutputGroup


def get_value(field):
    """Extract value from field, handling both RefValue and raw values."""
    if isinstance(field, RefValue):
        return field.value
    return field


class TestMultiArchetypeDataModel:
    """Test Python/Pydantic data model for multiple building archetypes."""

    def test_single_archetype_backwards_compatible(self):
        """Single archetype config should work unchanged (backwards compatibility)."""
        site = SiteProperties()

        assert site.nbtypes == 1
        assert list(site.effective_archetypes.keys()) == ["default"]

    def test_single_archetype_with_building_archetype_field(self):
        """Explicit single archetype via building_archetype field."""
        site = SiteProperties(
            building_archetype=ArchetypeProperties(
                stebbs_Height=15.0,
                BuildingName="single_test",
            )
        )

        assert site.nbtypes == 1
        archetype = site.effective_archetypes["default"]
        assert get_value(archetype.stebbs_Height) == 15.0
        assert archetype.BuildingName == "single_test"

    def test_two_archetypes_creation(self):
        """Create site with two building archetypes."""
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(sfr=0.6, stebbs_Height=8.5),
                "commercial": ArchetypeProperties(sfr=0.4, stebbs_Height=25.0),
            }
        )

        assert site.nbtypes == 2
        assert set(site.effective_archetypes.keys()) == {"residential", "commercial"}

    def test_archetype_sfr_values(self):
        """Verify sfr values are correctly stored."""
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(sfr=0.6),
                "commercial": ArchetypeProperties(sfr=0.4),
            }
        )

        archetypes = site.effective_archetypes
        assert get_value(archetypes["residential"].sfr) == 0.6
        assert get_value(archetypes["commercial"].sfr) == 0.4


class TestSfrValidation:
    """Test validation of surface fraction (sfr) sum."""

    def test_sfr_sum_equals_one_valid(self):
        """Valid sfr values summing to 1.0 should be accepted."""
        # Should not raise
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(sfr=0.6),
                "commercial": ArchetypeProperties(sfr=0.4),
            }
        )
        assert site.nbtypes == 2

    def test_sfr_sum_exceeds_one_invalid(self):
        """sfr values summing to > 1.0 should raise ValueError."""
        with pytest.raises(ValueError, match=r"must sum to 1\.0"):
            SiteProperties(
                building_archetypes={
                    "residential": ArchetypeProperties(sfr=0.6),
                    "commercial": ArchetypeProperties(sfr=0.5),  # Sum = 1.1
                }
            )

    def test_sfr_sum_less_than_one_invalid(self):
        """sfr values summing to < 1.0 should raise ValueError."""
        with pytest.raises(ValueError, match=r"must sum to 1\.0"):
            SiteProperties(
                building_archetypes={
                    "residential": ArchetypeProperties(sfr=0.5),
                    "commercial": ArchetypeProperties(sfr=0.3),  # Sum = 0.8
                }
            )

    def test_max_archetypes_exceeded(self):
        """More than 10 archetypes should raise ValueError."""
        with pytest.raises(ValueError, match="Maximum 10"):
            archetypes = {
                f"arch_{i}": ArchetypeProperties(sfr=1.0 / 11)
                for i in range(11)
            }
            SiteProperties(building_archetypes=archetypes)

    def test_sfr_auto_distribution_default_values(self):
        """Archetypes with default sfr should be auto-distributed equally."""
        # Create archetypes without explicit sfr (all use default 1.0)
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(),  # sfr defaults to 1.0
                "commercial": ArchetypeProperties(),   # sfr defaults to 1.0
            }
        )

        # Should auto-distribute to 0.5 each
        archetypes = site.effective_archetypes
        assert get_value(archetypes["residential"].sfr) == pytest.approx(0.5)
        assert get_value(archetypes["commercial"].sfr) == pytest.approx(0.5)

    def test_sfr_auto_distribution_three_archetypes(self):
        """Three archetypes with default sfr should get 1/3 each."""
        site = SiteProperties(
            building_archetypes={
                "a": ArchetypeProperties(),
                "b": ArchetypeProperties(),
                "c": ArchetypeProperties(),
            }
        )

        expected = 1.0 / 3.0
        for archetype in site.effective_archetypes.values():
            assert get_value(archetype.sfr) == pytest.approx(expected)

    def test_sfr_no_auto_distribution_with_explicit_values(self):
        """Explicit sfr values should not be changed by auto-distribution."""
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(sfr=0.7),
                "commercial": ArchetypeProperties(sfr=0.3),
            }
        )

        archetypes = site.effective_archetypes
        assert get_value(archetypes["residential"].sfr) == pytest.approx(0.7)
        assert get_value(archetypes["commercial"].sfr) == pytest.approx(0.3)


class TestDataFrameSerialization:
    """Test DataFrame serialisation and round-trip for multi-archetype."""

    def test_single_archetype_dataframe(self):
        """Single archetype should serialise to DataFrame correctly."""
        site = SiteProperties(
            building_archetype=ArchetypeProperties(stebbs_Height=10.0)
        )

        df = site.to_df_state(grid_id=1)

        # Should have nbtypes column
        assert ("nbtypes", "0") in df.columns
        nbtypes_val = df.loc[1, ("nbtypes", "0")]
        # Handle Series or scalar
        if hasattr(nbtypes_val, "iloc"):
            nbtypes_val = nbtypes_val.iloc[0]
        assert int(nbtypes_val) == 1

    def test_multi_archetype_dataframe_columns(self):
        """Multiple archetypes should have indexed columns."""
        site = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(sfr=0.6, stebbs_Height=8.5),
                "commercial": ArchetypeProperties(sfr=0.4, stebbs_Height=25.0),
            }
        )

        df = site.to_df_state(grid_id=1)

        # Should have nbtypes = 2
        nbtypes_val = df.loc[1, ("nbtypes", "0")]
        if hasattr(nbtypes_val, "iloc"):
            nbtypes_val = nbtypes_val.iloc[0]
        assert int(nbtypes_val) == 2

        # Should have archetype names stored
        names = df.loc[1, ("archetype_names", "0")]
        if hasattr(names, "iloc"):
            names = names.iloc[0]
        assert "residential" in str(names)
        assert "commercial" in str(names)

        # Should have indexed columns
        assert ("stebbs_height", "arch_0") in df.columns
        assert ("stebbs_height", "arch_1") in df.columns

    def test_archetype_indexed_serialization_roundtrip(self):
        """ArchetypeProperties indexed serialisation should round-trip correctly."""
        original = ArchetypeProperties(
            sfr=0.6,
            stebbs_Height=8.5,
            BuildingName="test_residential",
        )

        # Serialise to indexed DataFrame
        df = original.to_df_state_indexed(grid_id=1, arch_idx=0)

        # Round-trip
        rebuilt = ArchetypeProperties.from_df_state_indexed(df, grid_id=1, arch_idx=0)

        assert get_value(rebuilt.sfr) == get_value(original.sfr)
        assert get_value(rebuilt.stebbs_Height) == get_value(original.stebbs_Height)
        assert rebuilt.BuildingName == original.BuildingName


class TestPerArchetypeOutputVariables:
    """Test per-archetype output variable generation."""

    def test_generate_variables_count(self):
        """Should generate correct number of variables."""
        archetype_names = ["residential", "commercial"]
        vars = generate_archetype_variables(archetype_names)

        expected_count = len(STEBBS_ARCHETYPE_KEY_VARS) * len(archetype_names)
        assert len(vars) == expected_count

    def test_variable_naming_format(self):
        """Generated variables should follow naming convention."""
        archetype_names = ["residential", "commercial"]
        vars = generate_archetype_variables(archetype_names)

        # Check naming pattern
        var_names = [v.name for v in vars]
        assert "Tair_ind_residential" in var_names
        assert "Tair_ind_commercial" in var_names
        assert "Qload_heating_F_residential" in var_names
        assert "Qload_heating_F_commercial" in var_names

    def test_variable_output_group(self):
        """All generated variables should be in STEBBS_ARCHETYPE group."""
        archetype_names = ["residential", "commercial"]
        vars = generate_archetype_variables(archetype_names)

        for v in vars:
            assert v.group == OutputGroup.STEBBS_ARCHETYPE

    def test_variable_descriptions(self):
        """Generated variables should have archetype-specific descriptions."""
        archetype_names = ["residential"]
        vars = generate_archetype_variables(archetype_names)

        tair_var = next(v for v in vars if v.name == "Tair_ind_residential")
        assert "residential" in tair_var.description.lower()

    def test_archetype_name_normalisation(self):
        """Archetype names with spaces/dashes should be normalised."""
        archetype_names = ["high rise", "mixed-use"]
        vars = generate_archetype_variables(archetype_names)

        var_names = [v.name for v in vars]
        # Spaces and dashes should become underscores
        assert "Tair_ind_high_rise" in var_names
        assert "Tair_ind_mixed_use" in var_names


class TestSitePropertiesRoundTrip:
    """Test archetype serialization within SiteProperties."""

    @pytest.mark.core
    def test_multi_archetype_to_df_state(self):
        """Multi-archetype serialization to DataFrame works correctly."""
        # Create site with multiple archetypes
        original = SiteProperties(
            building_archetypes={
                "residential": ArchetypeProperties(
                    sfr=0.6,
                    stebbs_Height=8.5,
                    BuildingName="residential_block",
                ),
                "commercial": ArchetypeProperties(
                    sfr=0.4,
                    stebbs_Height=25.0,
                    BuildingName="office_tower",
                ),
            }
        )

        # Serialize to DataFrame
        df = original.to_df_state(grid_id=1)

        # Verify nbtypes is stored
        nbtypes_val = df.loc[1, ("nbtypes", "0")]
        if hasattr(nbtypes_val, "iloc"):
            nbtypes_val = nbtypes_val.iloc[0]
        assert int(nbtypes_val) == 2

        # Verify archetype names stored
        names_val = df.loc[1, ("archetype_names", "0")]
        if hasattr(names_val, "iloc"):
            names_val = names_val.iloc[0]
        assert "residential" in str(names_val)
        assert "commercial" in str(names_val)

        # Verify indexed columns exist
        assert ("stebbs_height", "arch_0") in df.columns
        assert ("stebbs_height", "arch_1") in df.columns

    @pytest.mark.core
    def test_auto_distributed_sfr_values(self):
        """Auto-distributed sfr values are correctly assigned."""
        # Create site with auto-distributed sfr
        original = SiteProperties(
            building_archetypes={
                "a": ArchetypeProperties(),
                "b": ArchetypeProperties(),
            }
        )

        # Verify sfr was auto-distributed
        assert get_value(original.effective_archetypes["a"].sfr) == pytest.approx(0.5)
        assert get_value(original.effective_archetypes["b"].sfr) == pytest.approx(0.5)

        # Serialize to DataFrame and verify sfr values stored
        df = original.to_df_state(grid_id=1)
        sfr_0 = df.loc[1, ("sfr", "arch_0")]
        sfr_1 = df.loc[1, ("sfr", "arch_1")]
        if hasattr(sfr_0, "iloc"):
            sfr_0 = sfr_0.iloc[0]
            sfr_1 = sfr_1.iloc[0]
        assert float(sfr_0) == pytest.approx(0.5)
        assert float(sfr_1) == pytest.approx(0.5)


class TestBackwardsCompatibility:
    """Test backwards compatibility with single-archetype configurations."""

    def test_default_site_is_single_archetype(self):
        """Default SiteProperties should have single archetype."""
        site = SiteProperties()
        assert site.nbtypes == 1
        assert site.building_archetype is not None

    def test_effective_archetypes_with_single(self):
        """effective_archetypes should work with single archetype."""
        site = SiteProperties(
            building_archetype=ArchetypeProperties(stebbs_Height=12.0)
        )

        archetypes = site.effective_archetypes
        assert len(archetypes) == 1
        assert "default" in archetypes
        assert get_value(archetypes["default"].stebbs_Height) == 12.0

    def test_nbtypes_property(self):
        """nbtypes property should return correct count."""
        single = SiteProperties()
        assert single.nbtypes == 1

        multi = SiteProperties(
            building_archetypes={
                "a": ArchetypeProperties(sfr=0.5),
                "b": ArchetypeProperties(sfr=0.5),
            }
        )
        assert multi.nbtypes == 2
