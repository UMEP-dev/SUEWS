"""Tests for Category 1 field renames (issue #1256).

Verifies:
- New field names are accessible as Pydantic attributes.
- Old field names still work via ``@model_validator(mode='before')`` (backward compat).
- Deprecation warnings are emitted when old names are used.
- ``to_df_state()`` emits DataFrame columns under legacy names (Fortran bridge).
- Providing both old and new names raises ``ValueError``.
"""

import warnings

import pytest
from supy.validation import analyze_config_methods

pytestmark = pytest.mark.api

from supy.data_model.core.field_renames import (
    ALL_FIELD_RENAMES,
    DECTRPROPERTIES_RENAMES,
    EVETRPROPERTIES_RENAMES,
    LAIPARAMS_RENAMES,
    MODELPHYSICS_RENAMES,
    SNOWPARAMS_RENAMES,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
)
from supy.data_model.core.model import ModelPhysics
from supy.data_model.core.site import (
    DectrProperties,
    EvetrProperties,
    LAIParams,
    SnowParams,
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
            + len(SNOWPARAMS_RENAMES)
        )
        assert len(ALL_FIELD_RENAMES) == expected

    def test_no_duplicate_new_names(self):
        values = list(ALL_FIELD_RENAMES.values())
        assert len(set(values)) == len(values), "Duplicate new names detected"

    def test_snake_case_outputs(self):
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
        assert _unwrap(m.net_radiation_method) == 3

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
        assert _unwrap(snow.temp_melt_factor) == 0.15


class TestDeprecationWarnings:
    @pytest.mark.parametrize(
        "model_cls, old_name, new_name, sample_value",
        [
            (ModelPhysics, "netradiationmethod", "net_radiation_method", 3),
            (SurfaceProperties, "soildepth", "soil_depth", 150.0),
            (LAIParams, "baset", "base_temperature", 5.0),
            (SnowParams, "crwmax", "water_holding_capacity_max", 0.2),
            (EvetrProperties, "evetreeh", "height_evergreen_tree", 12.0),
            (DectrProperties, "capmax_dec", "capacity_max_deciduous", 90.0),
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
            ModelPhysics(net_radiation_method=3)
        messages = [str(w.message) for w in captured if issubclass(w.category, DeprecationWarning)]
        # Allow unrelated Pydantic deprecations; assert none mention our rename.
        assert not any("netradiationmethod" in m for m in messages)


class TestConflictDetection:
    def test_both_old_and_new_raises(self):
        with pytest.raises(ValueError, match="both .* and .* are present"):
            ModelPhysics(netradiationmethod=3, net_radiation_method=2)


class TestRawDictCompatibility:
    def test_analyze_config_methods_accepts_snake_case(self):
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
