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

import warnings

import pytest
from supy.validation import analyze_config_methods

pytestmark = pytest.mark.api

from supy.data_model.core.field_renames import (
    ALL_FIELD_RENAMES,
    ARCHETYPEPROPERTIES_RENAMES,
    ARCHETYPEPROPERTIES_PASCAL_RENAMES,
    DECTRPROPERTIES_RENAMES,
    EVETRPROPERTIES_RENAMES,
    LAIPARAMS_RENAMES,
    MODELPHYSICS_RENAMES,
    MODELPHYSICS_SUFFIX_RENAMES,
    SNOWPARAMS_RENAMES,
    SNOWPARAMS_INTERMEDIATE_RENAMES,
    STEBBSPROPERTIES_RENAMES,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
)
from supy.data_model.core.model import ModelPhysics
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

    def test_no_duplicate_new_names(self):
        values = list(ALL_FIELD_RENAMES.values())
        assert len(set(values)) == len(values), "Duplicate new names detected"

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
        for new_name in renames.values():
            assert new_name in model_cls.model_fields, (
                f"{model_cls.__name__} missing renamed field {new_name!r}"
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
        # Post-gh#1334: fused Wallext/Roofext now skip the gh#1329
        # PascalCase intermediate and land on the snake_case final.
        assert _unwrap(archetype.wall_external_thickness) == 0.25
        assert _unwrap(archetype.wall_external_density) == 1800.0
        assert _unwrap(archetype.roof_external_specific_heat_capacity) == 920.0

    def test_stebbs_pascal_names_populate_new_attributes(self):
        """Full STEBBS PascalCase -> snake_case (gh#1334)."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            archetype = ArchetypeProperties(
                BuildingType="Office",
                WWR=0.4,
                WallThickness=0.3,
                WallOuterCapFrac=0.6,
            )
        assert archetype.building_type == "Office"
        assert _unwrap(archetype.window_to_wall_ratio) == 0.4
        assert _unwrap(archetype.wall_thickness) == 0.3
        assert _unwrap(archetype.wall_outer_heat_capacity_fraction) == 0.6

    def test_stebbs_properties_pascal_names_populate_new_attributes(self):
        """Full StebbsProperties PascalCase -> snake_case (gh#1334)."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            stebbs = StebbsProperties(
                DHWWaterVolume=0.25,
                CoolingSystemCOP=3.5,
                MonthMeanAirTemperature_diffmax=14.0,
                HotWaterTankWallEmissivity=0.85,
            )
        assert _unwrap(stebbs.hot_water_volume) == 0.25
        assert _unwrap(stebbs.cooling_system_cop) == 3.5
        assert _unwrap(stebbs.month_mean_air_temperature_diffmax) == 14.0
        assert _unwrap(stebbs.hot_water_tank_wall_emissivity) == 0.85


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
            (ModelPhysics, "rc_method", "outer_cap_fraction", 1),
            # Other classes (Cat 1 only).
            (SurfaceProperties, "soildepth", "soil_depth", 150.0),
            (LAIParams, "baset", "base_temperature", 5.0),
            (SnowParams, "crwmax", "water_holding_capacity_max", 0.2),
            (EvetrProperties, "evetreeh", "height_evergreen_tree", 12.0),
            (DectrProperties, "capmax_dec", "capacity_max_deciduous", 90.0),
            # gh#1334: fused + PascalCase legacy -> snake_case final.
            (ArchetypeProperties, "WallextThickness", "wall_external_thickness", 0.25),
            (ArchetypeProperties, "RoofextDensity", "roof_external_density", 1900.0),
            (ArchetypeProperties, "WallThickness", "wall_thickness", 0.3),
            (ArchetypeProperties, "BuildingType", "building_type", "Office"),
            (ArchetypeProperties, "WWR", "window_to_wall_ratio", 0.4),
            (ArchetypeProperties, "WallOuterCapFrac", "wall_outer_heat_capacity_fraction", 0.6),
            (StebbsProperties, "DHWWaterVolume", "hot_water_volume", 0.25),
            (StebbsProperties, "CoolingSystemCOP", "cooling_system_cop", 3.5),
            (StebbsProperties, "MonthMeanAirTemperature_diffmax", "month_mean_air_temperature_diffmax", 14.0),
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

    def test_new_name_does_not_warn(self):
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            ModelPhysics(net_radiation=3)
        messages = [str(w.message) for w in captured if issubclass(w.category, DeprecationWarning)]
        # Allow unrelated Pydantic deprecations; assert none mention our renames.
        assert not any("netradiationmethod" in m for m in messages)
        assert not any("net_radiation_method" in m for m in messages)


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
