"""Tests for field renames (issues #1256, #1321).

Verifies:
- New field names are accessible as Pydantic attributes.
- Old field names still work via ``@model_validator(mode='before')`` (backward compat).
- Deprecation warnings are emitted when old names are used.
- ``to_df_state()`` emits DataFrame columns under legacy names (Fortran bridge).
- Providing both old and new names raises ``ValueError``.
- Category 1 intermediate spellings (Schema 2026.5 shape) still load with a
  deprecation warning via MODELPHYSICS_SUFFIX_RENAMES (#1321).
"""

from pathlib import Path
import warnings

import pytest
import yaml

from supy.validation import analyze_config_methods

pytestmark = pytest.mark.api

from supy.data_model.core.field_renames import (
    ALL_FIELD_RENAMES,
    ANTHRO_RENAMES,
    ARCHETYPEPROPERTIES_DEV6_RENAMES,
    ARCHETYPEPROPERTIES_DEV7_RENAMES,
    ARCHETYPEPROPERTIES_DEV12_RENAMES,
    ARCHETYPEPROPERTIES_RENAMES,
    ATMOSPHERE_RENAMES,
    DECTRPROPERTIES_RENAMES,
    EHC_RENAMES,
    EVETRPROPERTIES_RENAMES,
    FORTRAN_INTERNAL_RENAMES,
    HEATSTATE_RENAMES,
    HYDROSTATE_RENAMES,
    LAIPARAMS_RENAMES,
    LANDCOVER_RENAMES,
    MODELPHYSICS_RENAMES,
    PHENOLOGYSTATE_RENAMES,
    RAW_YAML_FIELD_RENAMES,
    SNOWPARAMS_RENAMES,
    SNOWSTATE_RENAMES,
    STEBBSPROPERTIES_RENAMES,
    STEBBSSTATE_RENAMES,
    SURFACE_RENAMES,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
    WATERDIST_RENAMES,
    normalise_yaml_renames,
    read_physics_key,
    rename_keys_recursive,
)
from supy.data_model.core.model import ModelPhysics, StebbsPhysics
from supy.data_model.core.site import (
    ArchetypeProperties,
    DectrProperties,
    EvetrProperties,
    LAIParams,
    SnowParams,
    StebbsProperties,
)
from supy.data_model.core.surface import SurfaceProperties
from supy.data_model.validation.core.controller import ValidationController

# (ModelClass, rename_dict) pairs covering every class that got renames.
_RENAMED_CLASSES = [
    (ModelPhysics, MODELPHYSICS_RENAMES),
    (SurfaceProperties, SURFACEPROPERTIES_RENAMES),
    (LAIParams, LAIPARAMS_RENAMES),
    (EvetrProperties, EVETRPROPERTIES_RENAMES),
    (DectrProperties, DECTRPROPERTIES_RENAMES),
    (SnowParams, SNOWPARAMS_RENAMES),
    (ArchetypeProperties, ARCHETYPEPROPERTIES_RENAMES),
    (StebbsProperties, STEBBSPROPERTIES_RENAMES),
    # VEGETATEDSURFACEPROPERTIES_RENAMES is covered by subclasses EvetrProperties
    # and DectrProperties (both inherit from VegetatedSurfaceProperties).
]


def _unwrap(value):
    """Return underlying value regardless of RefValue wrapping."""
    return getattr(value, "value", value)


class TestRegistryIntegrity:
    def test_all_renames_combines_per_class_dicts(self):
        expected = (
            # User-facing (Pydantic YAML surface). gh#1326 Tier D
            # Fortran-only internals live in FORTRAN_INTERNAL_RENAMES
            # instead — see that registry's dedicated test below.
            len(MODELPHYSICS_RENAMES)
            + len(SURFACEPROPERTIES_RENAMES)
            + len(LAIPARAMS_RENAMES)
            + len(VEGETATEDSURFACEPROPERTIES_RENAMES)
            + len(EVETRPROPERTIES_RENAMES)
            + len(DECTRPROPERTIES_RENAMES)
            + len(ARCHETYPEPROPERTIES_RENAMES)
            + len(STEBBSPROPERTIES_RENAMES)
            + len(SNOWPARAMS_RENAMES)
        )
        assert len(ALL_FIELD_RENAMES) == expected

    def test_fortran_internal_renames_combines_per_class_dicts(self):
        expected = (
            len(EHC_RENAMES)
            + len(SNOWSTATE_RENAMES)
            + len(WATERDIST_RENAMES)
            + len(PHENOLOGYSTATE_RENAMES)
            + len(ANTHRO_RENAMES)
            + len(ATMOSPHERE_RENAMES)
            + len(SURFACE_RENAMES)
            + len(HEATSTATE_RENAMES)
            + len(HYDROSTATE_RENAMES)
            + len(LANDCOVER_RENAMES)
            + len(STEBBSSTATE_RENAMES)
        )
        assert len(FORTRAN_INTERNAL_RENAMES) == expected

    def test_no_duplicate_new_names(self):
        values = list(ALL_FIELD_RENAMES.values())
        assert len(set(values)) == len(values), "Duplicate new names detected"

    def test_legacy_and_new_sets_disjoint(self):
        """No string can simultaneously be a legacy key and a final name.

        The Rust bridge reverse lookup inverts ``ALL_FIELD_RENAMES`` into
        a ``{new_name: old_name}`` map and the raw-YAML preflight runs the
        forward map recursively. An identifier that plays both roles
        (e.g. ``occupants`` as both the Archetype final name and a
        STEBBS-state legacy key) causes silent value rewrites — gh#1326
        surfaced the failure mode, this test locks it down.
        """
        legacy = set(ALL_FIELD_RENAMES.keys())
        new = set(ALL_FIELD_RENAMES.values())
        overlap = sorted(legacy & new)
        assert not overlap, (
            f"Legacy and new name sets in ALL_FIELD_RENAMES overlap: {overlap}"
        )

    def test_snake_case_outputs(self):
        # gh#1334 retires the STEBBS PascalCase exception — every user-facing
        # YAML field is snake_case throughout.
        for new in ALL_FIELD_RENAMES.values():
            assert new.islower(), f"New name must be lowercase: {new!r}"
            assert "_" in new or new.isalpha(), (
                f"New name expected to be snake_case: {new!r}"
            )


class TestNewNamesAccepted:
    @pytest.mark.parametrize("model_cls, renames", _RENAMED_CLASSES)
    def test_new_names_resolve_to_attributes(self, model_cls, renames):
        # ArchetypeProperties and StebbsProperties have downstream
        # dev-cycle rename passes. Accept either a direct Pydantic field
        # or a key that chains to one.
        from supy.data_model.core.field_renames import (
            STEBBSPROPERTIES_DEV8_RENAMES,
            STEBBSPROPERTIES_DEV12_RENAMES,
        )
        if model_cls.__name__ == "ArchetypeProperties":
            downstream_chain = {
                **ARCHETYPEPROPERTIES_DEV6_RENAMES,
                **ARCHETYPEPROPERTIES_DEV7_RENAMES,
                **ARCHETYPEPROPERTIES_DEV12_RENAMES,
            }
        elif model_cls.__name__ == "StebbsProperties":
            downstream_chain = {
                **STEBBSPROPERTIES_DEV8_RENAMES,
                **STEBBSPROPERTIES_DEV12_RENAMES,
            }
        else:
            downstream_chain = {}

        # gh#1456: the STEBBS-scoped ModelPhysics renames resolve into the
        # nested `stebbs` (StebbsPhysics) object rather than a flat field.
        # `stebbs` (the master toggle) maps to the nested object itself; the
        # other STEBBS targets map to a member of StebbsPhysics.
        stebbs_targets = {
            "stebbs": None,  # the nested object itself
            # dev12 Column D (gh#1452) lands the former rcmethod / rc_method /
            # outer_cap_fraction on the flat name `capacitance`; gh#1456 then
            # folds it into the nested stebbs.capacitance member.
            "capacitance": "capacitance",
            "outer_cap_fraction": "capacitance",
            "setpoint": "setpoint",
            "same_albedo_wall": "same_albedo_wall",
            "same_albedo_roof": "same_albedo_roof",
            "same_emissivity_wall": "same_emissivity_wall",
            "same_emissivity_roof": "same_emissivity_roof",
        }
        from supy.data_model.core.model import StebbsPhysics

        for new_name in renames.values():
            field_name = new_name
            seen = {field_name}
            # Stop as soon as we hit a real model field — a dev12 final name
            # may itself appear as a *key* in an earlier dev table (e.g.
            # `daylight_control` is the dev12 final but is also the dev8 key
            # `daylight_control -> control_daylight`, reverted at dev12), so
            # blindly chasing the chain would oscillate. The canonical name is
            # the terminal model field.
            while field_name not in model_cls.model_fields and field_name in downstream_chain:
                field_name = downstream_chain[field_name]
                assert field_name not in seen, (
                    f"{model_cls.__name__} cyclic downstream rename from "
                    f"{new_name!r} via {field_name!r}"
                )
                seen.add(field_name)
            if field_name in model_cls.model_fields:
                continue
            if model_cls.__name__ == "ModelPhysics" and field_name in stebbs_targets:
                member = stebbs_targets[field_name]
                if member is None or member in StebbsPhysics.model_fields:
                    continue
            raise AssertionError(
                f"{model_cls.__name__} missing renamed field {field_name!r} "
                f"(from {new_name!r})"
            )


class TestBackwardCompat:
    def test_old_name_constructor_populates_new_attribute(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            m = ModelPhysics(netradiationmethod=3)
        assert _unwrap(m.net_radiation) == 3

    def test_cat1_intermediate_name_populates_new_attribute(self):
        """Users on Schema 2026.5 supply net_radiation_method (Cat 1 intermediate);
        the Cat 2+3 suffix shim (#1321) maps it to the final `net_radiation`.
        """
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            m = ModelPhysics(net_radiation_method=3, rsl_method=2, gs_model=2)
        assert _unwrap(m.net_radiation) == 3
        assert _unwrap(m.roughness_sublayer) == 2
        assert _unwrap(m.surface_conductance) == 2

    def test_old_lai_names_populate_new_attributes(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            lai = LAIParams(baset=5.0, laimax=6.0)
        assert _unwrap(lai.base_temperature) == 5.0
        assert _unwrap(lai.lai_max) == 6.0

    def test_old_snow_names_populate_new_attributes(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            snow = SnowParams(crwmax=0.2, tempmeltfact=0.15)
        assert _unwrap(snow.water_holding_capacity_max) == 0.2
        # tempmeltfact -> temperature_melt_factor (post-gh#1334: skipped
        # the 2026.5 intermediate `temp_melt_factor`).
        assert _unwrap(snow.temperature_melt_factor) == 0.15

    def test_snow_intermediate_names_populate_new_attributes(self):
        """Schema 2026.5.dev2 intermediate snake_case -> gh#1334 final."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            snow = SnowParams(
                precip_limit=2.5,
                snow_limit_building=0.1,
                tau_a=0.02,
                temp_melt_factor=0.15,
            )
        assert _unwrap(snow.temperature_rain_snow_threshold) == 2.5
        assert _unwrap(snow.snow_depth_limit_building) == 0.1
        assert _unwrap(snow.tau_cold_snow) == 0.02
        assert _unwrap(snow.temperature_melt_factor) == 0.15

    def test_old_stebbs_ext_names_populate_new_attributes(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            archetype = ArchetypeProperties(
                WallextThickness=0.25,
                WallextDensity=1800.0,
                RoofextCp=920.0,
            )
        # Post-gh#1334: fused Wallext/Roofext skip the gh#1329 PascalCase
        # intermediate and land on the snake_case dev6 form. The dev6 -> dev7
        # Rule 2 reorder then renames external -> outer (layer-to-insulation
        # qualifier) and reorders quantity -> component -> sub-class.
        assert _unwrap(archetype.thickness_wall_outer) == 0.25
        assert _unwrap(archetype.density_wall_outer) == 1800.0
        assert _unwrap(archetype.specific_heat_capacity_roof_outer) == 920.0

    def test_stebbs_pascal_names_populate_new_attributes(self):
        """Full STEBBS PascalCase -> current snake_case naming convention."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            archetype = ArchetypeProperties(
                WWR=0.4,
                WallThickness=0.3,
                WallOuterCapFrac=0.6,
        )
        assert _unwrap(archetype.ratio_window_to_wall) == 0.4
        # dev6 wall_thickness -> dev7 thickness_wall (Rule 2: quantity leads).
        assert _unwrap(archetype.thickness_wall) == 0.3
        # dev6 wall_outer_heat_capacity_fraction ->
        # dev7 fraction_heat_capacity_wall_external (Rule 2 non-physical:
        # fraction_* category prefix leads).
        assert _unwrap(archetype.fraction_heat_capacity_wall_external) == 0.6

    def test_stebbs_properties_pascal_names_populate_new_attributes(self):
        """Full StebbsProperties PascalCase -> current snake_case naming.

        Chains gh#1334 (PascalCase -> snake_case) plus the dev8 -> dev9
        Rule 2 reorder for StebbsProperties.
        """
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            stebbs = StebbsProperties(
                DHWWaterVolume=0.25,
                CoolingSystemCOP=3.5,
                MonthMeanAirTemperature_diffmax=14.0,
                HotWaterTankWallEmissivity=0.85,
            )
        # dev8 hot_water_volume -> dev9 volume_hot_water (Rule 2).
        assert _unwrap(stebbs.volume_hot_water) == 0.25
        # dev8 cooling_system_cop -> dev9 efficiency_cooling_system_air
        # (Rule 2 + air_ qualifier; COP folded into efficiency).
        assert _unwrap(stebbs.efficiency_cooling_system_air) == 3.5
        # dev9 month_mean_air_temperature_diffmax -> dev12
        # temperature_air_month_mean_diffmax (gh#1392 follow-up straggler
        # reorder, Rule 2: temperature quantity leads).
        assert _unwrap(stebbs.temperature_air_month_mean_diffmax) == 14.0
        # dev8 hot_water_tank_wall_emissivity -> dev9
        # emissivity_hot_water_tank_wall (Rule 2: quantity leads).
        assert _unwrap(stebbs.emissivity_hot_water_tank_wall) == 0.85


class TestDeprecationWarnings:
    @pytest.mark.parametrize(
        "model_cls, old_name, new_name, sample_value",
        [
            # Fused -> final (Cat 1 + Cat 2+3 consolidated in MODELPHYSICS_RENAMES).
            (ModelPhysics, "netradiationmethod", "net_radiation", 3),
            (ModelPhysics, "setpointmethod", "setpoint", 2),
            # Cat 1 intermediate -> final (MODELPHYSICS_SUFFIX_RENAMES, #1321).
            (ModelPhysics, "net_radiation_method", "net_radiation", 3),
            (ModelPhysics, "rsl_method", "roughness_sublayer", 2),
            (ModelPhysics, "gs_model", "surface_conductance", 2),
            (ModelPhysics, "smd_method", "soil_moisture_deficit", 0),
            # dev12 Column D: rcmethod / rc_method / dev11 outer_cap_fraction
            # all resolve to the final `capacitance` (gh#1392 follow-up).
            (ModelPhysics, "rc_method", "capacitance", 1),
            (ModelPhysics, "rcmethod", "capacitance", 1),
            (ModelPhysics, "outer_cap_fraction", "capacitance", 1),
            # Other classes (Cat 1 only).
            (SurfaceProperties, "soildepth", "soil_depth", 150.0),
            (LAIParams, "baset", "base_temperature", 5.0),
            (SnowParams, "crwmax", "water_holding_capacity_max", 0.2),
            (EvetrProperties, "evetreeh", "height_evergreen_tree", 12.0),
            (DectrProperties, "capmax_dec", "capacity_max_deciduous", 90.0),
            # gh#1334 fused + PascalCase legacy + gh#1390 dev7 reorder ->
            # current final names.
            (ArchetypeProperties, "WallextThickness", "thickness_wall_outer", 0.25),
            (ArchetypeProperties, "RoofextDensity", "density_roof_outer", 1900.0),
            (ArchetypeProperties, "WallThickness", "thickness_wall", 0.3),
            (ArchetypeProperties, "WWR", "ratio_window_to_wall", 0.4),
            (ArchetypeProperties, "WallOuterCapFrac", "fraction_heat_capacity_wall_external", 0.6),
            (StebbsProperties, "DHWWaterVolume", "hot_water_volume", 0.25),
            (StebbsProperties, "CoolingSystemCOP", "cooling_system_cop", 3.5),
            (StebbsProperties, "MonthMeanAirTemperature_diffmax", "month_mean_air_temperature_diffmax", 14.0),
            # dev11 -> dev12 STEBBS straggler reorder (gh#1392 follow-up): the
            # dev9-era snake names are accepted and remapped quantity-first.
            (StebbsProperties, "ground_depth", "depth_ground", 3.0),
            (StebbsProperties, "ventilation_rate", "rate_ventilation", 0.5),
            (StebbsProperties, "lighting_power_density", "power_density_lighting", 2.0),
            (StebbsProperties, "month_mean_air_temperature_diffmax", "temperature_air_month_mean_diffmax", 14.0),
            # dev11 -> dev12 Column D alignment (gh#1392 follow-up): the
            # dev9-era snake names are accepted and remapped. Scalar fields
            # only here; the three profile-typed renames are covered by
            # test_column_d_profile_renames_emit_deprecation below.
            # ArchetypeProperties scalar setpoints / powers.
            (ArchetypeProperties, "power_air_heating_max", "max_power_heating_system_air", 8000.0),
            (ArchetypeProperties, "power_water_heating_max", "max_power_heating_system_water", 4000.0),
            (ArchetypeProperties, "temperature_air_heating_setpoint", "setpoint_temperature_heating_air", 19.0),
            (ArchetypeProperties, "temperature_air_cooling_setpoint", "setpoint_temperature_cooling_air", 24.0),
            # StebbsProperties scalars.
            (StebbsProperties, "power_air_cooling_max", "max_power_cooling_system_air", 6000.0),
            (StebbsProperties, "temperature_water_heating_setpoint", "setpoint_temperature_heating_water", 55.0),
            (StebbsProperties, "temperature_water_mains", "temperature_mains_water", 10.0),
            (StebbsProperties, "area_hot_water_tank_surface", "surface_area_hot_water_tank", 2.0),
            (StebbsProperties, "area_hot_water_surface", "surface_area_hot_water", 0.5),
            (StebbsProperties, "rate_hot_water_flow", "rate_flow_hot_water", 0.0),
            (StebbsProperties, "control_daylight", "daylight_control", 1),
            (
                StebbsProperties,
                "convection_coefficient_hot_water_vessel_wall_internal",
                "convection_coefficient_hot_water_tank_vessel_internal",
                7.7,
            ),
            (
                StebbsProperties,
                "convection_coefficient_hot_water_vessel_wall_external",
                "convection_coefficient_hot_water_tank_vessel_external",
                7.7,
            ),
            (SnowParams, "tau_a", "tau_cold_snow", 0.02),
            (SnowParams, "narp_emis_snow", "narp_emissivity_snow", 0.98),
            (SnowParams, "precip_limit", "temperature_rain_snow_threshold", 2.5),
        ],
    )
    def test_old_name_emits_deprecation_warning(
        self, model_cls, old_name, new_name, sample_value
    ):
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            model_cls(**{old_name: sample_value})
        messages = [str(w.message) for w in captured if issubclass(w.category, DeprecationWarning)]
        assert any(old_name in msg and new_name in msg for msg in messages), (
            f"Expected deprecation warning for {old_name} -> {new_name}, got {messages}"
        )

    @pytest.mark.parametrize(
        "model_cls, old_name, new_name",
        [
            # dev11 -> dev12 Column D alignment, profile-typed renames. These
            # take default_factory-built profile objects, so we let the field
            # default carry through and only assert the deprecation warning.
            (
                ArchetypeProperties,
                "profile_temperature_air_heating_setpoint",
                "profile_setpoint_temperature_heating_air",
            ),
            (
                ArchetypeProperties,
                "profile_temperature_air_cooling_setpoint",
                "profile_setpoint_temperature_cooling_air",
            ),
            (
                StebbsProperties,
                "profile_hot_water_flow",
                "profile_flow_hot_water",
            ),
        ],
    )
    def test_column_d_profile_renames_emit_deprecation(
        self, model_cls, old_name, new_name
    ):
        """The three dev12 profile-typed renames accept the old name and warn.

        Pass a None value: ``apply_field_renames`` rewrites the key in the
        ``mode='before'`` validator (emitting the warning) and the Optional
        profile field tolerates ``None`` at construction.
        """
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            instance = model_cls(**{old_name: None})
        messages = [
            str(w.message)
            for w in captured
            if issubclass(w.category, DeprecationWarning)
        ]
        assert any(old_name in msg and new_name in msg for msg in messages), (
            f"Expected deprecation warning for {old_name} -> {new_name}, got {messages}"
        )
        # The new attribute exists on the model after the rename.
        assert hasattr(instance, new_name)

    def test_new_name_does_not_warn(self):
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            ModelPhysics(net_radiation=3)
        messages = [str(w.message) for w in captured if issubclass(w.category, DeprecationWarning)]
        # Allow unrelated Pydantic deprecations; assert none mention our renames.
        assert not any("netradiationmethod" in m for m in messages)
        assert not any("net_radiation_method" in m for m in messages)


class TestStebbsPhysicsFold:
    """gh#1456: the flat STEBBS physics switches fold into model.physics.stebbs.

    Covers both public paths: the ModelPhysics before-validator (Pydantic) and
    the yaml_upgrade migrator. Legacy flat YAMLs must still load, the master
    toggle must decompose, and capacitance must land in stebbs.capacitance.
    """

    def test_flat_master_toggle_decomposes_pydantic(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp = ModelPhysics(
                stebbs={"value": 2},
                outer_cap_fraction={"value": 1},
                setpoint={"value": 1},
                same_albedo_wall={"value": 1},
            )
        # Master toggle 2 -> enabled + provided parameters.
        assert _unwrap(mp.stebbs.enabled) is True
        assert int(_unwrap(mp.stebbs.parameters)) == 2
        # outer_cap_fraction folds to capacitance.
        assert int(_unwrap(mp.stebbs.capacitance)) == 1
        assert int(_unwrap(mp.stebbs.setpoint)) == 1
        assert int(_unwrap(mp.stebbs.same_albedo_wall)) == 1

    def test_flat_fold_composes_back_to_legacy_columns(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp = ModelPhysics(stebbs={"value": 2}, outer_cap_fraction={"value": 2})
        df = mp.to_df_state(grid_id=1)
        assert int(df.loc[1, ("stebbsmethod", "0")]) == 2
        assert int(df.loc[1, ("rcmethod", "0")]) == 2

    def test_disabled_master_composes_to_zero(self):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp = ModelPhysics(stebbs={"value": 0})
        df = mp.to_df_state(grid_id=1)
        assert int(df.loc[1, ("stebbsmethod", "0")]) == 0
        assert _unwrap(mp.stebbs.enabled) is False

    def test_already_nested_passes_through_without_warning(self):
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            mp = ModelPhysics(
                stebbs={
                    "enabled": {"value": True},
                    "parameters": {"value": 2},
                    "capacitance": {"value": 1},
                }
            )
        fold_msgs = [
            str(w.message)
            for w in captured
            if issubclass(w.category, DeprecationWarning) and "gh#1456" in str(w.message)
        ]
        assert fold_msgs == [], f"nested form must not warn: {fold_msgs}"
        assert _unwrap(mp.stebbs.enabled) is True
        assert int(_unwrap(mp.stebbs.capacitance)) == 1

    def test_nested_legacy_aliases_populate_stebbs_leaves(self):
        """Legacy aliases inside the nested block match raw Phase B behaviour."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp = ModelPhysics(
                stebbs={
                    "enabled": {"value": True},
                    "parameters": {"value": 2},
                    "outer_cap_fraction": {"value": 2},
                    "setpointmethod": {"value": 2},
                }
            )

        assert int(_unwrap(mp.stebbs.capacitance)) == 2
        assert int(_unwrap(mp.stebbs.setpoint)) == 2
        df = mp.to_df_state(grid_id=1)
        assert int(df.loc[1, ("stebbsmethod", "0")]) == 2
        assert int(df.loc[1, ("rcmethod", "0")]) == 2
        assert int(df.loc[1, ("setpointmethod", "0")]) == 2

    @pytest.mark.parametrize(
        "left,right",
        [
            ("capacitance", "outer_cap_fraction"),
            ("outer_cap_fraction", "rcmethod"),
            ("rcmethod", "rc_method"),
            ("setpoint", "setpointmethod"),
        ],
    )
    def test_duplicate_nested_stebbs_leaf_aliases_are_rejected(self, left, right):
        """Nested aliases to the same STEBBS leaf are ambiguous."""
        payload = {
            "stebbs": {
                "enabled": {"value": True},
                left: {"value": 1},
                right: {"value": 2},
            }
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            with pytest.raises(ValueError, match="Multiple nested STEBBS"):
                ModelPhysics(**payload)

    @pytest.mark.parametrize(
        "flat_key",
        ["rcmethod", "capacitance", "outer_cap_fraction"],
    )
    def test_nested_and_flat_stebbs_leaves_are_rejected(self, flat_key):
        """Match the Rust bridge: ambiguous flat+nested STEBBS input fails."""
        payload = {
            "stebbs": {
                "enabled": {"value": True},
                "capacitance": {"value": 1},
            },
            flat_key: {"value": 0},
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            with pytest.raises(ValueError, match="nested 'stebbs'.*flat STEBBS"):
                ModelPhysics(**payload)

    @pytest.mark.parametrize(
        "flat_key",
        ["rcmethod", "capacitance", "outer_cap_fraction"],
    )
    def test_built_stebbs_object_and_flat_leaves_are_rejected(self, flat_key):
        """Programmatic nested objects follow the same ambiguity rule as YAML."""
        stebbs = StebbsPhysics(
            enabled={"value": True},
            capacitance={"value": 1},
        )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            with pytest.raises(ValueError, match="nested 'stebbs'.*flat STEBBS"):
                ModelPhysics(stebbs=stebbs, **{flat_key: {"value": 0}})

    @pytest.mark.parametrize(
        "left,right",
        [
            ("outer_cap_fraction", "rcmethod"),
            ("outer_cap_fraction", "rc_method"),
            ("capacitance", "rcmethod"),
            ("setpoint", "setpointmethod"),
        ],
    )
    def test_duplicate_flat_stebbs_leaf_aliases_are_rejected(self, left, right):
        """Flat aliases to the same nested STEBBS leaf are ambiguous."""
        payload = {
            "stebbs": {"value": 1},
            left: {"value": 1},
            right: {"value": 2},
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            with pytest.raises(ValueError, match="Use only"):
                ModelPhysics(**payload)

    def test_single_fold_deprecation_warning(self):
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            ModelPhysics(
                stebbs={"value": 1},
                outer_cap_fraction={"value": 1},
                setpoint={"value": 2},
            )
        fold_msgs = [
            str(w.message)
            for w in captured
            if issubclass(w.category, DeprecationWarning) and "gh#1456" in str(w.message)
        ]
        assert len(fold_msgs) == 1, f"expected one fold warning, got {fold_msgs}"

    def test_legacy_master_toggle_with_ref_stays_legacy(self):
        """gh#1456 regression: a legacy master toggle carrying provenance.

        ``model.physics.stebbs: {value: 1, ref: {...}}`` is a RefValue scalar
        (its only keys are ``value``/``ref``), NOT the new nested object.
        Before the fix, ``ref`` discriminated it as nested, so its ``value``
        was ignored and ``enabled`` defaulted to false -- silently flipping
        ``stebbsmethod`` from 1 (enabled) to 0 (disabled).
        """
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp_enabled = ModelPhysics(
                stebbs={"value": 1, "ref": {"desc": "STEBBS on", "DOI": "10.0/x"}}
            )
        # value 1 -> enabled + default parameters; composes to stebbsmethod 1.
        assert _unwrap(mp_enabled.stebbs.enabled) is True
        assert mp_enabled.stebbs.enabled.ref.desc == "STEBBS on"
        assert int(_unwrap(mp_enabled.stebbs.parameters)) == 1
        df_enabled = mp_enabled.to_df_state(grid_id=1)
        assert int(df_enabled.loc[1, ("stebbsmethod", "0")]) == 1

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp_provided = ModelPhysics(
                stebbs={"value": 2, "ref": {"desc": "STEBBS provided"}}
            )
        # value 2 -> enabled + provided parameters; composes to stebbsmethod 2.
        assert _unwrap(mp_provided.stebbs.enabled) is True
        assert int(_unwrap(mp_provided.stebbs.parameters)) == 2
        df_provided = mp_provided.to_df_state(grid_id=1)
        assert int(df_provided.loc[1, ("stebbsmethod", "0")]) == 2

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mp_off = ModelPhysics(stebbs={"value": 0})
        # value 0 -> disabled; composes to stebbsmethod 0.
        assert _unwrap(mp_off.stebbs.enabled) is False
        df_off = mp_off.to_df_state(grid_id=1)
        assert int(df_off.loc[1, ("stebbsmethod", "0")]) == 0

    def test_malformed_legacy_stebbs_mapping_is_rejected(self):
        with pytest.raises(ValueError, match="Legacy 'stebbs' mappings"):
            ModelPhysics(stebbs={"foo": 1})

    def test_invalid_legacy_stebbs_master_value_is_rejected(self):
        with pytest.raises(ValueError, match="expects integer 0, 1, or 2"):
            ModelPhysics(stebbs={"value": 5})

    def test_null_stebbs_master_switch_values_are_rejected(self):
        with pytest.raises(ValueError, match="enabled/parameters"):
            ModelPhysics(stebbs={"enabled": {"value": None}})
        with pytest.raises(ValueError, match="enabled/parameters"):
            ModelPhysics(
                stebbs={"enabled": {"value": True}, "parameters": {"value": None}}
            )

    def test_from_df_state_rejects_invalid_stebbsmethod_code(self):
        df = ModelPhysics().to_df_state(grid_id=1)
        df.loc[1, ("stebbsmethod", "0")] = 99

        with pytest.raises(ValueError, match="Invalid stebbsmethod"):
            ModelPhysics.from_df_state(df, grid_id=1)

    def test_migrator_folds_flat_physics(self):
        """The yaml_upgrade migrator folds flat physics STEBBS keys to nested."""
        from supy.util.converter.yaml_upgrade import _apply_stebbs_physics_fold

        cfg = {
            "model": {
                "physics": {
                    "stebbs": {"value": 2},
                    "outer_cap_fraction": {"value": 1},
                    "setpoint": {"value": 1},
                    "same_albedo_wall": {"value": 1},
                }
            }
        }
        _apply_stebbs_physics_fold(cfg)
        stebbs = cfg["model"]["physics"]["stebbs"]
        # Flat siblings removed from top-level physics; folded under stebbs.
        assert "outer_cap_fraction" not in cfg["model"]["physics"]
        assert "setpoint" not in cfg["model"]["physics"]
        assert stebbs["enabled"] == {"value": True}
        assert stebbs["parameters"] == {"value": 2}
        assert stebbs["capacitance"] == {"value": 1}
        assert stebbs["setpoint"] == {"value": 1}
        assert stebbs["same_albedo_wall"] == {"value": 1}

        cfg_with_ref = {"model": {"physics": {"stebbs": {"value": 1, "ref": {"desc": "toggle"}}}}}
        _apply_stebbs_physics_fold(cfg_with_ref)
        assert cfg_with_ref["model"]["physics"]["stebbs"]["enabled"] == {
            "value": True,
            "ref": {"desc": "toggle"},
        }

        cfg_nested_alias = {
            "model": {
                "physics": {
                    "stebbs": {
                        "enabled": {"value": True},
                        "outer_cap_fraction": {"value": 2},
                        "setpointmethod": {"value": 2},
                    }
                }
            }
        }
        _apply_stebbs_physics_fold(cfg_nested_alias)
        stebbs_alias = cfg_nested_alias["model"]["physics"]["stebbs"]
        assert "outer_cap_fraction" not in stebbs_alias
        assert "setpointmethod" not in stebbs_alias
        assert stebbs_alias["capacitance"] == {"value": 2}
        assert stebbs_alias["setpoint"] == {"value": 2}


class TestConflictDetection:
    def test_both_fused_and_final_raises(self):
        with pytest.raises(ValueError, match="both .* and .* are present"):
            ModelPhysics(netradiationmethod=3, net_radiation=2)

    def test_both_cat1_intermediate_and_final_raises(self):
        """Schema 2026.5 spelling clashes with Schema 2026.5.dev2 spelling (#1321)."""
        with pytest.raises(ValueError, match="both .* and .* are present"):
            ModelPhysics(net_radiation_method=3, net_radiation=2)


class TestRawDictCompatibility:
    def test_analyze_config_methods_accepts_final_names(self):
        """Schema 2026.5.dev2 (#1321) final names resolve via `read_physics_key`."""
        config = {
            "model": {
                "physics": {
                    "roughness_sublayer": {"value": 2},
                    "roughness_length_momentum": {"value": 2},
                    "net_radiation": {"value": 1001},
                    "emissions": {"value": 4},
                    "storage_heat": {"value": 4},
                }
            }
        }

        methods = analyze_config_methods(config)

        assert methods["rslmethod_variable"] is True
        assert methods["roughness_variable"] is True
        assert methods["netradiation_spartacus"] is True
        assert methods["emissions_advanced"] is True
        assert methods["storage_estm"] is True

    def test_analyze_config_methods_accepts_cat1_intermediate(self):
        """Schema 2026.5 (Cat 1 intermediate) names still resolve via the rename registry."""
        config = {
            "model": {
                "physics": {
                    "rsl_method": {"value": 2},
                    "roughness_length_momentum_method": {"value": 2},
                    "net_radiation_method": {"value": 1001},
                    "emissions_method": {"value": 4},
                    "storage_heat_method": {"value": 4},
                }
            }
        }

        methods = analyze_config_methods(config)

        assert methods["rslmethod_variable"] is True
        assert methods["roughness_variable"] is True
        assert methods["netradiation_spartacus"] is True
        assert methods["emissions_advanced"] is True
        assert methods["storage_estm"] is True

    def test_analyze_config_methods_accepts_nested_family_form(self):
        config = {
            "model": {
                "physics": {
                    "roughness_sublayer": {"value": 2},
                    "roughness_length_momentum": {"value": 2},
                    "net_radiation": {"spartacus": {"value": 1001}},
                    "emissions": {"simple": {"value": 4}},
                    "storage_heat": {"estm": {"value": 4}},
                }
            }
        }

        methods = analyze_config_methods(config)

        assert methods["rslmethod_variable"] is True
        assert methods["roughness_variable"] is True
        assert methods["netradiation_spartacus"] is True
        assert methods["emissions_advanced"] is True
        assert methods["storage_estm"] is True

    def test_analyze_config_methods_accepts_orthogonal_net_radiation(self):
        config = {
            "model": {
                "physics": {
                    "roughness_sublayer": {"value": 2},
                    "roughness_length_momentum": {"value": 2},
                    "net_radiation": {"scheme": "spartacus", "ldown": "air"},
                    "emissions": {"simple": {"value": 4}},
                    "storage_heat": {"estm": {"value": 4}},
                }
            }
        }

        methods = analyze_config_methods(config)

        assert methods["roughness_variable"] is True
        assert methods["netradiation_spartacus"] is True
        assert methods["emissions_advanced"] is True
        assert methods["storage_estm"] is True

    def test_read_physics_key_accepts_orthogonal_legacy_name(self):
        physics = {"netradiationmethod": {"scheme": "narp", "ldown": "air"}}

        assert read_physics_key(physics, "net_radiation") == 3

    @pytest.mark.parametrize(
        "storage_key",
        ["storage_heat", "storage_heat_method", "storageheatmethod"],
    )
    def test_read_physics_key_accepts_storage_heat_owned_qf_axis(self, storage_key):
        physics = {storage_key: {"ohm": {"include_qf": False}}}

        assert read_physics_key(physics, "storage_heat") == 1
        assert read_physics_key(physics, "ohm_inc_qf") == 0

    def test_read_physics_key_rejects_storage_value_plus_nested_qf(self):
        physics = {"storage_heat": {"ohm": {"value": 1, "include_qf": False}}}

        with pytest.raises(ValueError, match="storage_heat\\.ohm.*inner keys"):
            read_physics_key(physics, "storage_heat")

    def test_legacy_analyze_config_methods_accepts_sample_config(self):
        path = Path(__file__).resolve().parents[2] / "src/supy/sample_data/sample_config.yml"
        config = yaml.safe_load(path.read_text(encoding="utf-8"))

        methods = analyze_config_methods(config)

        assert methods
        assert methods["storage_estm"] is False
        assert methods["emissions_advanced"] is False

    def test_analyze_config_methods_accepts_orthogonal_emissions(self):
        config = {
            "model": {
                "physics": {
                    "emissions": {
                        "heat": "j11",
                        "co2": {
                            "anthropogenic": "detailed",
                            "biogenic": "conductance",
                        },
                    },
                }
            }
        }

        methods = analyze_config_methods(config)

        assert methods["emissions_advanced"] is True

    def test_validation_controller_accepts_legacy_physics_names(self):
        config = {
            "model": {
                "physics": {
                    "roughlenmommethod": {"value": 2},
                    "netradiationmethod": {"value": 1001},
                    "emissionsmethod": {"value": 4},
                    "storageheatmethod": {"value": 4},
                }
            }
        }

        controller = ValidationController(config_data=config)
        active_methods = controller.get_active_methods()

        assert active_methods["roughness_variable"] is True
        assert active_methods["netradiation_spartacus"] is True
        assert active_methods["emissions_advanced"] is True
        assert active_methods["storage_estm"] is True

    def test_validation_controller_accepts_nested_legacy_physics_names(self):
        config = {
            "model": {
                "physics": {
                    "roughlenmommethod": {"value": 2},
                    "netradiationmethod": {"spartacus": {"value": 1001}},
                    "emissions_method": {"simple": {"value": 4}},
                    "storage_heat_method": {"estm": {"value": 4}},
                }
            }
        }

        controller = ValidationController(config_data=config)
        active_methods = controller.get_active_methods()

        assert active_methods["roughness_variable"] is True
        assert active_methods["netradiation_spartacus"] is True
        assert active_methods["emissions_advanced"] is True
        assert active_methods["storage_estm"] is True

    def test_raw_yaml_archetype_renames_reach_current_names(self):
        payload = {
            "sites": [
                {
                    "properties": {
                        "building_archetype": {
                            "WallextThickness": {"value": 0.25},
                            "WallThickness": {"value": 0.3},
                            "wall_external_emissivity": {"value": 0.85},
                            "WWR": {"value": 0.4},
                        }
                    }
                }
            ]
        }

        normalised = rename_keys_recursive(payload, RAW_YAML_FIELD_RENAMES)
        archetype = normalised["sites"][0]["properties"]["building_archetype"]

        assert archetype["thickness_wall_outer"]["value"] == 0.25
        assert archetype["thickness_wall"]["value"] == 0.3
        assert archetype["emissivity_wall_external"]["value"] == 0.85
        assert archetype["ratio_window_to_wall"]["value"] == 0.4
        assert "wall_external_thickness" not in archetype
        assert "wall_thickness" not in archetype
        assert "window_to_wall_ratio" not in archetype

    def test_public_physics_aliases_are_scoped_to_model_physics(self):
        payload = {
            "model": {"physics": {"leaf_area_index": "modelled", "snow": "disabled"}},
            "sites": [{"properties": {"snow": {"initially": {"value": 0}}}}],
        }

        normalised = normalise_yaml_renames(payload)

        physics = normalised["model"]["physics"]
        assert physics["laimethod"]["value"] == 1
        assert physics["snow_use"]["value"] == 0
        properties = normalised["sites"][0]["properties"]
        assert "snow" in properties
        assert "snow_use" not in properties

    def test_raw_yaml_stebbs_straggler_renames_reach_current_names(self):
        """dev11 -> dev12 STEBBS straggler reorder must resolve in the raw-YAML
        precheck path (Phase A / RENAMED_PARAMS), not only the Pydantic shim.

        Regression for the gap where STEBBSPROPERTIES_DEV12_RENAMES was wired
        into the model_validator and the migrator but omitted from
        RAW_YAML_FIELD_RENAMES, so a dev11 YAML using the old keys was
        mis-reported in validation (old key as unknown extra, new name as
        missing) instead of being normalised.
        """
        payload = {
            "sites": [
                {
                    "properties": {
                        "stebbs": {
                            "ground_depth": {"value": 2.0},
                            "ventilation_rate": {"value": 0.6},
                            "lighting_power_density": {"value": 2.0},
                            "month_mean_air_temperature_diffmax": {"value": 10.0},
                        }
                    }
                }
            ]
        }

        normalised = rename_keys_recursive(payload, RAW_YAML_FIELD_RENAMES)
        stebbs = normalised["sites"][0]["properties"]["stebbs"]

        assert stebbs["depth_ground"]["value"] == 2.0
        assert stebbs["rate_ventilation"]["value"] == 0.6
        assert stebbs["power_density_lighting"]["value"] == 2.0
        assert stebbs["temperature_air_month_mean_diffmax"]["value"] == 10.0
        assert "ground_depth" not in stebbs
        assert "ventilation_rate" not in stebbs
        assert "lighting_power_density" not in stebbs
        assert "month_mean_air_temperature_diffmax" not in stebbs

    def test_raw_yaml_column_d_renames_reach_current_names(self):
        """dev11 -> dev12 Column D alignment must resolve in the raw-YAML
        precheck path (Phase A / RENAMED_PARAMS) for BOTH the stebbs and
        building_archetype containers.

        Regression for the gap where a DEV12 rename dict was wired into the
        model_validator and the migrator but omitted from
        RAW_YAML_FIELD_RENAMES (Codex P2 on the original straggler PR), so a
        dev11 YAML using the old keys was mis-reported in validation.
        """
        payload = {
            "sites": [
                {
                    "properties": {
                        "stebbs": {
                            "power_air_cooling_max": {"value": 6000.0},
                            "temperature_water_heating_setpoint": {"value": 55.0},
                            "temperature_water_mains": {"value": 10.0},
                            "area_hot_water_tank_surface": {"value": 2.0},
                            "area_hot_water_surface": {"value": 0.5},
                            "rate_hot_water_flow": {"value": 0.0},
                            "profile_hot_water_flow": {"working_day": {}},
                            "control_daylight": {"value": 1},
                            "convection_coefficient_hot_water_vessel_wall_internal": {"value": 7.7},
                            "convection_coefficient_hot_water_vessel_wall_external": {"value": 7.7},
                        },
                        "building_archetype": {
                            "power_air_heating_max": {"value": 8000.0},
                            "power_water_heating_max": {"value": 4000.0},
                            "temperature_air_heating_setpoint": {"value": 19.0},
                            "temperature_air_cooling_setpoint": {"value": 24.0},
                            "profile_temperature_air_heating_setpoint": {"working_day": {}},
                            "profile_temperature_air_cooling_setpoint": {"working_day": {}},
                        },
                    }
                }
            ]
        }

        normalised = rename_keys_recursive(payload, RAW_YAML_FIELD_RENAMES)
        props = normalised["sites"][0]["properties"]
        stebbs = props["stebbs"]
        archetype = props["building_archetype"]

        # Stebbs (10)
        assert stebbs["max_power_cooling_system_air"]["value"] == 6000.0
        assert stebbs["setpoint_temperature_heating_water"]["value"] == 55.0
        assert stebbs["temperature_mains_water"]["value"] == 10.0
        assert stebbs["surface_area_hot_water_tank"]["value"] == 2.0
        assert stebbs["surface_area_hot_water"]["value"] == 0.5
        assert stebbs["rate_flow_hot_water"]["value"] == 0.0
        assert "working_day" in stebbs["profile_flow_hot_water"]
        assert stebbs["daylight_control"]["value"] == 1
        assert stebbs["convection_coefficient_hot_water_tank_vessel_internal"]["value"] == 7.7
        assert stebbs["convection_coefficient_hot_water_tank_vessel_external"]["value"] == 7.7
        for old in (
            "power_air_cooling_max",
            "temperature_water_heating_setpoint",
            "temperature_water_mains",
            "area_hot_water_tank_surface",
            "area_hot_water_surface",
            "rate_hot_water_flow",
            "profile_hot_water_flow",
            "control_daylight",
            "convection_coefficient_hot_water_vessel_wall_internal",
            "convection_coefficient_hot_water_vessel_wall_external",
        ):
            assert old not in stebbs

        # Archetype (6)
        assert archetype["max_power_heating_system_air"]["value"] == 8000.0
        assert archetype["max_power_heating_system_water"]["value"] == 4000.0
        assert archetype["setpoint_temperature_heating_air"]["value"] == 19.0
        assert archetype["setpoint_temperature_cooling_air"]["value"] == 24.0
        assert "working_day" in archetype["profile_setpoint_temperature_heating_air"]
        assert "working_day" in archetype["profile_setpoint_temperature_cooling_air"]
        for old in (
            "power_air_heating_max",
            "power_water_heating_max",
            "temperature_air_heating_setpoint",
            "temperature_air_cooling_setpoint",
            "profile_temperature_air_heating_setpoint",
            "profile_temperature_air_cooling_setpoint",
        ):
            assert old not in archetype

    def test_raw_yaml_modelphysics_capacitance_rename_reaches_current_name(self):
        """dev12 model.physics rename (outer_cap_fraction -> capacitance) and
        its legacy ancestry (rcmethod / rc_method) must resolve in the raw-YAML
        precheck path, not only the Pydantic shim (gh#1392 Column D).
        """
        for old_key in ("outer_cap_fraction", "rcmethod", "rc_method"):
            payload = {"model": {"physics": {old_key: {"value": 1}}}}
            normalised = rename_keys_recursive(payload, RAW_YAML_FIELD_RENAMES)
            physics = normalised["model"]["physics"]
            assert physics["capacitance"]["value"] == 1, (
                f"{old_key} did not resolve to capacitance: {physics}"
            )
            assert old_key not in physics or old_key == "capacitance"

    def test_raw_value_reader_accepts_archetype_pre_current_names(self):
        from supy.data_model.validation.core.yaml_helpers import get_value_safe

        archetype = {
            "WallThickness": {"value": 0.3},
            "wall_external_thickness": {"value": 0.25},
            "wall_reflectivity": {"value": 0.2},
            "WWR": {"value": 0.4},
        }

        assert get_value_safe(archetype, "thickness_wall") == 0.3
        assert get_value_safe(archetype, "thickness_wall_outer") == 0.25
        assert get_value_safe(archetype, "reflectivity_wall_external") == 0.2
        assert get_value_safe(archetype, "ratio_window_to_wall") == 0.4


class TestDataFrameColumnsPreserveLegacyNames:
    """DataFrame column names must stay as the old (fused) names so the Rust /
    Fortran bridge keeps working. Only the Python attribute names change.
    """

    def test_model_physics_columns(self):
        df = ModelPhysics().to_df_state(grid_id=1)
        flat_cols = {col[0] for col in df.columns}
        for old_name in MODELPHYSICS_RENAMES:
            assert old_name in flat_cols, f"Missing legacy column {old_name!r}"

    def test_lai_params_columns(self):
        df = LAIParams(base_temperature=5.0).to_df_state(grid_id=1, surf_idx=2)
        flat_cols = {col[0] for col in df.columns}
        # LAIPowerCoefficients column is separate; only top-level LAI scalars here.
        for old_name in ("baset", "gddfull", "basete", "sddfull", "laimin", "laimax", "laitype"):
            assert old_name in flat_cols, f"Missing legacy column {old_name!r}"

    def test_snow_params_columns(self):
        df = SnowParams().to_df_state(grid_id=1)
        flat_cols = {col[0] for col in df.columns}
        scalar_old_names = {
            old for old in SNOWPARAMS_RENAMES if old not in {"snowprof_24hr"}
        }
        for old_name in scalar_old_names:
            assert old_name in flat_cols, f"Missing legacy column {old_name!r}"

    def test_archetype_ext_columns(self):
        # The Fortran bridge (src/suews_bridge/src/building_archetype_prm.rs)
        # reads these columns by their pre-gh#1334 spellings; the
        # to_df_state path must preserve them even though the Python
        # attributes are now snake_case (wall_external_thickness etc.).
        df = ArchetypeProperties().to_df_state(grid_id=1)
        flat_cols = {col[0] for col in df.columns}
        for old_name in ARCHETYPEPROPERTIES_RENAMES:
            assert old_name.lower() in flat_cols, (
                f"Missing legacy column {old_name.lower()!r}"
            )
        for old_name, new_name in ARCHETYPEPROPERTIES_RENAMES.items():
            # When lower(old) happens to equal new (e.g. `Occupants` ->
            # `occupants`), the legacy column and the snake_case attribute
            # share a name and there is nothing to check — the legacy
            # assertion above already covered it.
            if old_name.lower() == new_name:
                continue
            assert new_name not in flat_cols, (
                f"Unexpected new-name column {new_name!r} in bridge output"
            )
