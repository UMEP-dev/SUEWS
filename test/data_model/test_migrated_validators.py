"""
Test module for validators migrated from Model class to SUEWSConfig.

This module contains tests for validators that have been migrated from
individual data model classes (Model, ModelPhysics, etc.) to the top-level
SUEWSConfig class for better centralized validation.

Each migrated validator should have comprehensive tests covering:
- Valid configurations that should pass
- Invalid configurations that should raise ValidationError
- Edge cases and boundary conditions
- Integration with existing SUEWSConfig validation
"""

import pytest
from pydantic import ValidationError

from supy.data_model import SUEWSConfig
from supy.data_model.model import OutputConfig


class TestMigratedValidators:
    """Test cases for validators migrated to SUEWSConfig."""

    def test_validate_model_output_config_valid_freq(self):
        """Test that valid output frequency configurations pass validation."""
        # Test case: freq is multiple of tstep
        config = SUEWSConfig(
            name="test_valid_output_config",
            description="Test valid output configuration",
            model={
                "control": {
                    "tstep": 300,
                    "output_file": {
                        "format": "parquet",
                        "freq": 3600,  # 3600 % 300 = 0, should pass
                    },
                }
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )
        # Should not raise any validation errors
        assert config.model.control.tstep == 300
        assert config.model.control.output_file.freq == 3600

    def test_validate_model_output_config_invalid_freq(self):
        """Test that invalid output frequency configurations raise ValidationError."""
        # Test case: freq is not multiple of tstep
        with pytest.raises(
            ValidationError, match="Output frequency.*multiple of timestep"
        ):
            SUEWSConfig(
                name="test_invalid_output_config",
                description="Test invalid output configuration",
                model={
                    "control": {
                        "tstep": 300,
                        "output_file": {
                            "format": "parquet",
                            "freq": 3601,  # 3601 % 300 = 1, should fail
                        },
                    }
                },
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            )

    def test_validate_model_output_config_none_freq(self):
        """Test that output config with None freq passes validation."""
        config = SUEWSConfig(
            name="test_none_freq",
            description="Test output config with None freq",
            model={
                "control": {
                    "tstep": 300,
                    "output_file": {
                        "format": "parquet",
                        "freq": None,  # Should pass validation
                    },
                }
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )
        assert config.model.control.output_file.freq is None

    def test_validate_model_output_config_deprecated_string(self):
        """Test that deprecated string output_file raises deprecation warning."""
        import warnings

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            config = SUEWSConfig(
                name="test_deprecated_string",
                description="Test deprecated string output_file",
                model={
                    "control": {
                        "tstep": 300,
                        "output_file": "custom_output.txt",  # Deprecated format
                    }
                },
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            )

            # Should have issued deprecation warning
            deprecation_warnings = [
                warning
                for warning in w
                if issubclass(warning.category, DeprecationWarning)
            ]
            assert len(deprecation_warnings) > 0
            assert "deprecated and was never used" in str(
                deprecation_warnings[0].message
            )

    def test_validate_model_output_config_default_string_no_warning(self):
        """Test that default 'output.txt' doesn't raise deprecation warning."""
        import warnings

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            config = SUEWSConfig(
                name="test_default_string",
                description="Test default string output_file",
                model={
                    "control": {
                        "tstep": 300,
                        "output_file": "output.txt",  # Default - should not warn
                    }
                },
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            )

            # Should not have issued deprecation warning for default value
            deprecation_warnings = [
                warning
                for warning in w
                if issubclass(warning.category, DeprecationWarning)
            ]
            output_file_warnings = [
                w for w in deprecation_warnings if "output_file" in str(w.message)
            ]
            assert len(output_file_warnings) == 0

    def test_validate_model_output_config_multiple_tstep_values(self):
        """Test output frequency validation with different tstep values."""
        test_cases = [
            (300, 600, True),  # 600 % 300 = 0 ✓
            (300, 900, True),  # 900 % 300 = 0 ✓
            (300, 1800, True),  # 1800 % 300 = 0 ✓
            (300, 3600, True),  # 3600 % 300 = 0 ✓
            (300, 301, False),  # 301 % 300 = 1 ✗
            (600, 1200, True),  # 1200 % 600 = 0 ✓
            (600, 1201, False),  # 1201 % 600 = 1 ✗
        ]

        for tstep, freq, should_pass in test_cases:
            config_data = {
                "name": f"test_tstep_{tstep}_freq_{freq}",
                "description": f"Test tstep={tstep}, freq={freq}",
                "model": {
                    "control": {
                        "tstep": tstep,
                        "output_file": {"format": "parquet", "freq": freq},
                    }
                },
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                assert config.model.control.tstep == tstep
                assert config.model.control.output_file.freq == freq
            else:
                with pytest.raises(
                    ValidationError, match="Output frequency.*multiple of timestep"
                ):
                    SUEWSConfig(**config_data)


class TestValidatorMigrationFramework:
    """Test the validator migration framework itself."""

    def test_migrated_validator_runs_after_base_validation(self):
        """Test that migrated validators run after basic model validation."""
        # This test ensures the migration framework works correctly
        # by confirming that model_validator(mode="after") runs after
        # the base model is fully constructed

        # Create minimal valid config
        config = SUEWSConfig(
            name="framework_test",
            description="Test validator migration framework",
            model={
                "control": {"tstep": 300},
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )

        # Confirm the config was created successfully
        assert config.name == "framework_test"
        assert config.model.control.tstep == 300

    def test_multiple_migrated_validators_coexist(self):
        """Test that multiple migrated validators can coexist in SUEWSConfig."""
        # This will become relevant as we migrate more validators
        # For now, test that the existing migrated validators work together

        config = SUEWSConfig(
            name="multiple_validators_test",
            description="Test multiple migrated validators",
            model={
                "control": {
                    "tstep": 300,
                    "output_file": {
                        "format": "parquet",
                        "freq": 3600,  # Valid freq
                    },
                },
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )

        # Both parameter completeness and output config validation should pass
        assert config.model.control.tstep == 300
        assert config.model.control.output_file.freq == 3600


class TestRadiationMethodValidator:
    """Test cases for the validate_radiation_method validator migrated to SUEWSConfig."""

    def test_validate_radiation_method_compatible_config(self):
        """Test that compatible radiation method configurations pass validation."""
        # Test case: netradiationmethod=3 with any forcing file (should pass)
        config = SUEWSConfig(
            name="test_compatible_radiation_method",
            description="Test compatible radiation method configuration",
            model={
                "control": {"forcing_file": "forcing.txt"},
                "physics": {
                    "netradiationmethod": 3  # LDOWN_AIR method
                },
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )
        # Should not raise any validation errors
        assert config.model.physics.netradiationmethod.value == 3

    def test_validate_radiation_method_incompatible_config(self):
        """Test that incompatible radiation method configurations trigger warning."""
        # Test case: netradiationmethod=1 with forcing.txt (should warn)
        with pytest.warns(
            UserWarning, match="NetRadiationMethod is set to 1.*observed Ldown"
        ):
            SUEWSConfig(
                name="test_incompatible_radiation_method",
                description="Test incompatible radiation method configuration",
                model={
                    "control": {"forcing_file": "forcing.txt"},
                    "physics": {
                        "netradiationmethod": 1  # LDOWN_OBSERVED method
                    },
                },
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            )

    def test_validate_radiation_method_with_refvalue_wrapper(self):
        """Test radiation method validation with RefValue wrapper objects."""
        # Test case: RefValue wrapped values should work correctly
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_radiation_method",
            description="Test radiation method with RefValue wrappers",
            model={
                "control": {"forcing_file": RefValue("custom_forcing.txt")},
                "physics": {
                    "netradiationmethod": RefValue(1)  # LDOWN_OBSERVED with custom file
                },
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )
        # Should pass because forcing file is not "forcing.txt"
        # Note: RefValue(1) gets converted to NetRadiationMethod.LDOWN_OBSERVED enum
        method_val = config.model.physics.netradiationmethod.value
        assert int(method_val) == 1
        assert config.model.control.forcing_file.value == "custom_forcing.txt"

    def test_validate_radiation_method_with_enum_values(self):
        """Test radiation method validation with Enum values."""
        from supy.data_model.model import NetRadiationMethod

        # Test case: Direct Enum value should be handled correctly
        config = SUEWSConfig(
            name="test_enum_radiation_method",
            description="Test radiation method with Enum values",
            model={
                "control": {"forcing_file": "custom_forcing.txt"},
                "physics": {
                    "netradiationmethod": NetRadiationMethod.LDOWN_OBSERVED  # Enum value = 1
                },
            },
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                    },
                }
            ],
        )
        # Should pass because forcing file is not "forcing.txt"
        method_val = config.model.physics.netradiationmethod.value
        assert int(method_val) == 1

    def test_validate_radiation_method_edge_case_forcing_files(self):
        """Test radiation method validation with various forcing file configurations."""
        test_cases = [
            ("forcing.txt", 1, False),  # Should fail: method=1 + forcing.txt
            ("forcing.txt", 3, True),  # Should pass: method=3 + any file
            ("custom.txt", 1, True),  # Should pass: method=1 + custom file
            ("observed_ldown.txt", 1, True),  # Should pass: method=1 + custom file
        ]

        for forcing_file, method, should_pass in test_cases:
            config_data = {
                "name": f"test_forcing_{forcing_file.replace('.', '_')}_method_{method}",
                "description": f"Test forcing_file={forcing_file}, method={method}",
                "model": {
                    "control": {"forcing_file": forcing_file},
                    "physics": {"netradiationmethod": method},
                },
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                method_val = config.model.physics.netradiationmethod.value
                assert int(method_val) == method
                forcing_file_val = getattr(
                    config.model.control.forcing_file,
                    "value",
                    config.model.control.forcing_file,
                )
                assert forcing_file_val == forcing_file
            else:
                with pytest.warns(
                    UserWarning,
                    match="NetRadiationMethod is set to 1.*observed Ldown",
                ):
                    SUEWSConfig(**config_data)

    def test_validate_radiation_method_mixed_types(self):
        """Test radiation method validation with mixed RefValue and direct value types."""
        from supy.data_model.type import RefValue
        from supy.data_model.model import NetRadiationMethod

        test_cases = [
            # (forcing_file_value, netradiationmethod_value, should_pass)
            (RefValue("forcing.txt"), 1, False),  # RefValue + int: should fail
            ("forcing.txt", RefValue(1), False),  # str + RefValue: should fail
            (
                RefValue("custom.txt"),
                RefValue(1),
                True,
            ),  # RefValue + RefValue: should pass
            (
                "custom.txt",
                NetRadiationMethod.LDOWN_OBSERVED,
                True,
            ),  # str + Enum: should pass
        ]

        for i, (forcing_file_val, method_val, should_pass) in enumerate(test_cases):
            config_data = {
                "name": f"test_mixed_types_{i}",
                "description": f"Test mixed types case {i}",
                "model": {
                    "control": {"forcing_file": forcing_file_val},
                    "physics": {"netradiationmethod": method_val},
                },
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                # Verify the values are properly accessed
                # Note: All values get converted to Enum, so we expect the .value to be 1
                method_val = config.model.physics.netradiationmethod.value
                assert int(method_val) == 1
            else:
                with pytest.warns(
                    UserWarning,
                    match="NetRadiationMethod is set to 1.*observed Ldown",
                ):
                    SUEWSConfig(**config_data)


class TestSiteRequiredFieldsValidator:
    """Test cases for the validate_site_required_fields validator migrated to SUEWSConfig."""

    def test_validate_site_complete_required_fields_passes(self):
        """Test that sites with all required fields pass validation."""
        config = SUEWSConfig(
            name="test_complete_site_fields",
            description="Test site with all required fields",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        # Critical geographic fields
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                        "timezone": {"value": 0},
                        # Basic physical parameters
                        "surfacearea": {"value": 10000},
                        "z": {"value": 10},
                        "z0m_in": {"value": 1.0},  # Must be < zdm_in
                        "zdm_in": {"value": 5.0},  # Must be > z0m_in
                        # System parameters
                        "pipecapacity": {"value": 100},
                        "runofftowater": {"value": 0},
                        "narp_trans_site": {"value": 0.2},
                        # Required complex objects (minimal valid configs)
                        "lumps": {},
                        "spartacus": {},
                        "conductance": {},
                        "irrigation": {},
                        "anthropogenic_emissions": {},
                        "snow": {},
                        "land_cover": {},
                        "vertical_layers": {},
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.lat.value == 51.5
        assert config.sites[0].properties.lng.value == -0.1

    def test_validate_site_missing_properties_fails(self):
        """Test that sites without properties section fail validation."""
        # This test is no longer valid since SiteProperties has default_factory
        # All sites automatically get a properties object with default values
        # Instead test that default properties are created correctly
        config = SUEWSConfig(
            name="test_missing_properties",
            description="Test site with default properties",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    # Properties will be auto-created with defaults
                }
            ],
        )
        # Should have properties with default values
        assert config.sites[0].properties is not None

        # Fields can be either RefValue or plain values depending on how they're set
        lat_val = config.sites[0].properties.lat
        if hasattr(lat_val, "value"):
            assert lat_val.value == 51.5  # RefValue case
        else:
            assert lat_val == 51.5  # Plain value case

    def test_validate_site_fields_have_defaults(self):
        """Test that sites automatically get default values for required fields."""
        # Since all "required" fields actually have defaults in SiteProperties,
        # this validator primarily serves as a safety check for programmatic usage
        config = SUEWSConfig(
            name="test_defaults_present",
            description="Test that all required fields have defaults",
            sites=[{"name": "test_site", "gridiv": 1}],
        )

        # All "required" fields should be present with default values
        props = config.sites[0].properties
        assert props.lat is not None
        assert props.lng is not None
        assert props.alt is not None
        assert props.timezone is not None
        assert props.surfacearea is not None
        assert props.z is not None
        assert props.z0m_in is not None
        assert props.zdm_in is not None
        assert props.pipecapacity is not None
        assert props.runofftowater is not None
        assert props.narp_trans_site is not None

        # Complex objects should also be present
        assert props.lumps is not None
        assert props.spartacus is not None
        assert props.conductance is not None
        assert props.irrigation is not None
        assert props.anthropogenic_emissions is not None
        assert props.snow is not None
        assert props.land_cover is not None
        assert props.vertical_layers is not None

    def test_validate_site_null_refvalue_fails(self):
        """Test that RefValue fields with null values fail validation."""
        from supy.data_model.type import RefValue

        with pytest.raises(
            ValidationError, match="test_site.*Required field 'alt' has no value"
        ):
            SUEWSConfig(
                name="test_null_refvalue",
                description="Test site with null RefValue",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": RefValue(None),  # Null RefValue should fail
                            "timezone": {"value": 0},
                            "surfacearea": {"value": 10000},
                            "z": {"value": 10},
                            "z0m_in": {"value": 1.0},
                            "zdm_in": {"value": 5.0},
                            "pipecapacity": {"value": 100},
                            "runofftowater": {"value": 0},
                            "narp_trans_site": {"value": 0.2},
                            "lumps": {},
                            "spartacus": {},
                            "conductance": {},
                            "irrigation": {},
                            "anthropogenic_emissions": {},
                            "snow": {},
                            "land_cover": {},
                            "vertical_layers": {},
                        },
                    }
                ],
            )

    def test_validate_site_z0m_zdm_constraint_violation(self):
        """Test that z0m_in >= zdm_in constraint violation fails validation."""
        with pytest.raises(
            ValidationError, match="test_site.*z0m_in.*must be less than zdm_in"
        ):
            SUEWSConfig(
                name="test_z0m_zdm_violation",
                description="Test z0m_in >= zdm_in constraint violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10},
                            "timezone": {"value": 0},
                            "surfacearea": {"value": 10000},
                            "z": {"value": 10},
                            "z0m_in": {"value": 5.0},  # Equal to zdm_in - should fail
                            "zdm_in": {"value": 5.0},
                            "pipecapacity": {"value": 100},
                            "runofftowater": {"value": 0},
                            "narp_trans_site": {"value": 0.2},
                            "lumps": {},
                            "spartacus": {},
                            "conductance": {},
                            "irrigation": {},
                            "anthropogenic_emissions": {},
                            "snow": {},
                            "land_cover": {},
                            "vertical_layers": {},
                        },
                    }
                ],
            )

    def test_validate_site_z0m_zdm_constraint_passes(self):
        """Test that valid z0m_in < zdm_in constraint passes validation."""
        config = SUEWSConfig(
            name="test_z0m_zdm_valid",
            description="Test valid z0m_in < zdm_in constraint",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10},
                        "timezone": {"value": 0},
                        "surfacearea": {"value": 10000},
                        "z": {"value": 10},
                        "z0m_in": {"value": 1.0},  # Less than zdm_in - should pass
                        "zdm_in": {"value": 5.0},
                        "pipecapacity": {"value": 100},
                        "runofftowater": {"value": 0},
                        "narp_trans_site": {"value": 0.2},
                        "lumps": {},
                        "spartacus": {},
                        "conductance": {},
                        "irrigation": {},
                        "anthropogenic_emissions": {},
                        "snow": {},
                        "land_cover": {},
                        "vertical_layers": {},
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.z0m_in.value == 1.0
        assert config.sites[0].properties.zdm_in.value == 5.0

    def test_validate_multiple_sites_with_z0m_zdm_constraint_violations(self):
        """Test z0m_in < zdm_in constraint violations across multiple sites."""
        # Test the constraint that can actually be violated
        with pytest.raises(
            ValueError,
            match="site1.*z0m_in.*must be less than zdm_in.*site2.*z0m_in.*must be less than zdm_in",
        ):
            SUEWSConfig(
                name="test_multi_site_constraint_violations",
                description="Test multiple sites with z0m_in >= zdm_in violations",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "z0m_in": {"value": 5.0},  # Equal to zdm_in
                            "zdm_in": {"value": 5.0},  # Should fail: z0m_in >= zdm_in
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "z0m_in": {"value": 6.0},  # Greater than zdm_in
                            "zdm_in": {"value": 5.0},  # Should fail: z0m_in >= zdm_in
                        },
                    },
                ],
            )

    def test_validate_site_with_refvalue_wrappers(self):
        """Test site required fields validation with RefValue wrappers."""
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_site_fields",
            description="Test site fields with RefValue wrappers",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "lat": RefValue(51.5),
                        "lng": RefValue(-0.1),
                        "alt": RefValue(10),
                        "timezone": RefValue(0),
                        "surfacearea": RefValue(10000),
                        "z": RefValue(10),
                        "z0m_in": RefValue(1.0),
                        "zdm_in": RefValue(5.0),
                        "pipecapacity": RefValue(100),
                        "runofftowater": RefValue(0),
                        "narp_trans_site": RefValue(0.2),
                        "lumps": {},
                        "spartacus": {},
                        "conductance": {},
                        "irrigation": {},
                        "anthropogenic_emissions": {},
                        "snow": {},
                        "land_cover": {},
                        "vertical_layers": {},
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.lat.value == 51.5
        assert config.sites[0].properties.lng.value == -0.1

    def test_validate_site_with_multiple_z0m_zdm_violations(self):
        """Test that multiple z0m_in >= zdm_in violations are reported together."""
        # Test multiple constraint violations in a single site by using edge cases
        with pytest.raises(
            ValueError, match="test_site.*z0m_in.*must be less than zdm_in"
        ):
            SUEWSConfig(
                name="test_multiple_z0m_zdm_violations",
                description="Test site with z0m_in >= zdm_in constraint violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "z0m_in": {"value": 5.0},  # Equal to zdm_in - should fail
                            "zdm_in": {"value": 5.0},  # z0m_in must be < zdm_in
                            # Other fields use defaults
                        },
                    }
                ],
            )


class TestSnowParametersValidator:
    """Test cases for the validate_snow_parameters validator migrated to SUEWSConfig."""

    def test_validate_snow_parameters_valid_ranges_pass(self):
        """Test that snow parameters with valid ranges pass validation."""
        config = SUEWSConfig(
            name="test_valid_snow_ranges",
            description="Test snow parameters with valid ranges",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "snow": {
                            "crwmin": {"value": 0.1},  # < crwmax ✓
                            "crwmax": {"value": 0.3},
                            "snowalbmin": {"value": 0.4},  # < snowalbmax ✓
                            "snowalbmax": {"value": 0.8},
                        }
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.snow.crwmin.value == 0.1
        assert config.sites[0].properties.snow.crwmax.value == 0.3
        assert config.sites[0].properties.snow.snowalbmin.value == 0.4
        assert config.sites[0].properties.snow.snowalbmax.value == 0.8

    def test_validate_snow_parameters_crw_range_violation(self):
        """Test that crwmin >= crwmax raises ValidationError."""
        with pytest.raises(
            ValidationError, match="test_site.*crwmin.*must be less than crwmax"
        ):
            SUEWSConfig(
                name="test_crw_range_violation",
                description="Test crwmin >= crwmax violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "snow": {
                                "crwmin": {
                                    "value": 0.3
                                },  # Equal to crwmax - should fail
                                "crwmax": {"value": 0.3},
                                "snowalbmin": {"value": 0.4},
                                "snowalbmax": {"value": 0.8},
                            }
                        },
                    }
                ],
            )

    def test_validate_snow_parameters_snowalb_range_violation(self):
        """Test that snowalbmin >= snowalbmax raises ValidationError."""
        with pytest.raises(
            ValidationError, match="test_site.*snowalbmin.*must be less than snowalbmax"
        ):
            SUEWSConfig(
                name="test_snowalb_range_violation",
                description="Test snowalbmin >= snowalbmax violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "snow": {
                                "crwmin": {"value": 0.1},
                                "crwmax": {"value": 0.3},
                                "snowalbmin": {
                                    "value": 0.8
                                },  # Greater than snowalbmax - should fail
                                "snowalbmax": {"value": 0.4},
                            }
                        },
                    }
                ],
            )

    def test_validate_snow_parameters_multiple_violations(self):
        """Test that multiple snow parameter violations are reported together."""
        with pytest.raises(
            ValidationError,
            match="test_site.*crwmin.*must be less than crwmax.*snowalbmin.*must be less than snowalbmax",
        ):
            SUEWSConfig(
                name="test_multiple_snow_violations",
                description="Test multiple snow parameter violations",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "snow": {
                                "crwmin": {"value": 0.5},  # > crwmax (0.3) ✗
                                "crwmax": {"value": 0.3},
                                "snowalbmin": {"value": 0.9},  # > snowalbmax (0.4) ✗
                                "snowalbmax": {"value": 0.4},
                            }
                        },
                    }
                ],
            )

    def test_validate_snow_parameters_with_refvalue_wrappers(self):
        """Test snow parameter validation with RefValue wrapper objects."""
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_snow_params",
            description="Test snow parameters with RefValue wrappers",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "snow": {
                            "crwmin": RefValue(0.1),
                            "crwmax": RefValue(0.3),
                            "snowalbmin": RefValue(0.4),
                            "snowalbmax": RefValue(0.8),
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.snow.crwmin.value == 0.1
        assert config.sites[0].properties.snow.crwmax.value == 0.3

    def test_validate_snow_parameters_multiple_sites(self):
        """Test snow parameter validation across multiple sites."""
        config = SUEWSConfig(
            name="test_multi_site_snow_params",
            description="Test snow parameter validation across multiple sites",
            sites=[
                {
                    "name": "site1_valid",
                    "gridiv": 0,
                    "properties": {
                        "snow": {
                            "crwmin": {"value": 0.1},
                            "crwmax": {"value": 0.3},
                            "snowalbmin": {"value": 0.4},
                            "snowalbmax": {"value": 0.8},
                        }
                    },
                },
                {
                    "name": "site2_valid",
                    "gridiv": 1,
                    "properties": {
                        "snow": {
                            "crwmin": {"value": 0.05},
                            "crwmax": {"value": 0.25},
                            "snowalbmin": {"value": 0.3},
                            "snowalbmax": {"value": 0.7},
                        }
                    },
                },
            ],
        )

        # Both sites should pass validation
        assert len(config.sites) == 2
        assert config.sites[0].properties.snow.crwmin.value == 0.1
        assert config.sites[1].properties.snow.crwmin.value == 0.05

    def test_validate_snow_parameters_multiple_sites_with_errors(self):
        """Test snow parameter validation with errors across multiple sites."""
        with pytest.raises(
            ValidationError,
            match="site1.*crwmin.*must be less than crwmax.*site2.*snowalbmin.*must be less than snowalbmax",
        ):
            SUEWSConfig(
                name="test_multi_site_snow_errors",
                description="Test multiple sites with different snow parameter violations",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "snow": {
                                "crwmin": {"value": 0.4},  # > crwmax (0.3) ✗
                                "crwmax": {"value": 0.3},
                                "snowalbmin": {"value": 0.4},
                                "snowalbmax": {"value": 0.8},
                            }
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "snow": {
                                "crwmin": {"value": 0.1},
                                "crwmax": {"value": 0.3},
                                "snowalbmin": {"value": 0.9},  # > snowalbmax (0.4) ✗
                                "snowalbmax": {"value": 0.4},
                            }
                        },
                    },
                ],
            )

    def test_validate_snow_parameters_edge_case_equal_values(self):
        """Test snow parameter validation with edge case equal values."""
        # Test that equal values (>=) are properly caught
        test_cases = [
            # (crwmin, crwmax, snowalbmin, snowalbmax, should_pass, expected_error)
            (0.1, 0.3, 0.4, 0.8, True, None),  # Valid ranges ✓
            (0.3, 0.3, 0.4, 0.8, False, "crwmin"),  # crwmin = crwmax ✗
            (0.1, 0.3, 0.8, 0.8, False, "snowalbmin"),  # snowalbmin = snowalbmax ✗
            (0.3, 0.3, 0.8, 0.8, False, "crwmin.*snowalbmin"),  # Both equal ✗
            (0.4, 0.1, 0.4, 0.8, False, "crwmin"),  # crwmin > crwmax ✗
            (0.1, 0.3, 0.9, 0.4, False, "snowalbmin"),  # snowalbmin > snowalbmax ✗
        ]

        for i, (
            crwmin,
            crwmax,
            snowalbmin,
            snowalbmax,
            should_pass,
            expected_error,
        ) in enumerate(test_cases):
            config_data = {
                "name": f"test_edge_case_{i}",
                "description": f"Edge case test {i}",
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "snow": {
                                "crwmin": {"value": crwmin},
                                "crwmax": {"value": crwmax},
                                "snowalbmin": {"value": snowalbmin},
                                "snowalbmax": {"value": snowalbmax},
                            }
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                assert config.sites[0].properties.snow.crwmin.value == crwmin
            else:
                with pytest.raises(ValidationError, match=expected_error):
                    SUEWSConfig(**config_data)


class TestAlbedoRangesValidator:
    """Test cases for the validate_albedo_ranges validator migrated to SUEWSConfig."""

    def test_validate_albedo_ranges_valid_ranges_pass(self):
        """Test that vegetated surfaces with valid albedo ranges pass validation."""
        config = SUEWSConfig(
            name="test_valid_albedo_ranges",
            description="Test vegetated surfaces with valid albedo ranges",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "evetr": {
                                "alb_min": {"value": 0.1},  # < alb_max ✓
                                "alb_max": {"value": 0.3},
                            },
                            "dectr": {
                                "alb_min": {"value": 0.15},  # < alb_max ✓
                                "alb_max": {"value": 0.35},
                            },
                            "grass": {
                                "alb_min": {"value": 0.2},  # < alb_max ✓
                                "alb_max": {"value": 0.4},
                            },
                        }
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.land_cover.evetr.alb_min.value == 0.1
        assert config.sites[0].properties.land_cover.evetr.alb_max.value == 0.3
        assert config.sites[0].properties.land_cover.dectr.alb_min.value == 0.15
        assert config.sites[0].properties.land_cover.dectr.alb_max.value == 0.35
        assert config.sites[0].properties.land_cover.grass.alb_min.value == 0.2
        assert config.sites[0].properties.land_cover.grass.alb_max.value == 0.4

    def test_validate_albedo_ranges_evetr_violation(self):
        """Test that alb_min > alb_max for evergreen trees raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="test_site evergreen trees.*alb_min.*must be less than or equal to alb_max",
        ):
            SUEWSConfig(
                name="test_evetr_albedo_violation",
                description="Test alb_min > alb_max for evergreen trees",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "evetr": {
                                    "alb_min": {
                                        "value": 0.5
                                    },  # > alb_max - should fail
                                    "alb_max": {"value": 0.3},
                                }
                            }
                        },
                    }
                ],
            )

    def test_validate_albedo_ranges_dectr_violation(self):
        """Test that alb_min > alb_max for deciduous trees raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="test_site deciduous trees.*alb_min.*must be less than or equal to alb_max",
        ):
            SUEWSConfig(
                name="test_dectr_albedo_violation",
                description="Test alb_min > alb_max for deciduous trees",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "alb_min": {
                                        "value": 0.6
                                    },  # > alb_max - should fail
                                    "alb_max": {"value": 0.4},
                                }
                            }
                        },
                    }
                ],
            )

    def test_validate_albedo_ranges_grass_violation(self):
        """Test that alb_min > alb_max for grass raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="test_site grass.*alb_min.*must be less than or equal to alb_max",
        ):
            SUEWSConfig(
                name="test_grass_albedo_violation",
                description="Test alb_min > alb_max for grass",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "grass": {
                                    "alb_min": {
                                        "value": 0.7
                                    },  # > alb_max - should fail
                                    "alb_max": {"value": 0.5},
                                }
                            }
                        },
                    }
                ],
            )

    def test_validate_albedo_ranges_multiple_violations(self):
        """Test that multiple albedo range violations are reported together."""
        with pytest.raises(
            ValidationError,
            match="test_site evergreen trees.*alb_min.*must be less than or equal to alb_max.*test_site grass.*alb_min.*must be less than or equal to alb_max",
        ):
            SUEWSConfig(
                name="test_multiple_albedo_violations",
                description="Test multiple albedo range violations",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "evetr": {
                                    "alb_min": {"value": 0.6},  # > alb_max ✗
                                    "alb_max": {"value": 0.4},
                                },
                                "dectr": {
                                    "alb_min": {"value": 0.2},  # < alb_max ✓
                                    "alb_max": {"value": 0.3},
                                },
                                "grass": {
                                    "alb_min": {"value": 0.8},  # > alb_max ✗
                                    "alb_max": {"value": 0.6},
                                },
                            }
                        },
                    }
                ],
            )

    def test_validate_albedo_ranges_with_refvalue_wrappers(self):
        """Test albedo range validation with RefValue wrapper objects."""
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_albedo_ranges",
            description="Test albedo ranges with RefValue wrappers",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "evetr": {
                                "alb_min": RefValue(0.15),
                                "alb_max": RefValue(0.35),
                            },
                            "dectr": {
                                "alb_min": RefValue(0.2),
                                "alb_max": RefValue(0.4),
                            },
                            "grass": {
                                "alb_min": RefValue(0.25),
                                "alb_max": RefValue(0.45),
                            },
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.land_cover.evetr.alb_min.value == 0.15
        assert config.sites[0].properties.land_cover.evetr.alb_max.value == 0.35

    def test_validate_albedo_ranges_multiple_sites(self):
        """Test albedo range validation across multiple sites."""
        config = SUEWSConfig(
            name="test_multi_site_albedo_ranges",
            description="Test albedo range validation across multiple sites",
            sites=[
                {
                    "name": "site1_valid",
                    "gridiv": 0,
                    "properties": {
                        "land_cover": {
                            "evetr": {
                                "alb_min": {"value": 0.1},
                                "alb_max": {"value": 0.3},
                            },
                            "grass": {
                                "alb_min": {"value": 0.2},
                                "alb_max": {"value": 0.4},
                            },
                        }
                    },
                },
                {
                    "name": "site2_valid",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "alb_min": {"value": 0.15},
                                "alb_max": {"value": 0.35},
                            },
                            "grass": {
                                "alb_min": {"value": 0.25},
                                "alb_max": {"value": 0.45},
                            },
                        }
                    },
                },
            ],
        )

        # Both sites should pass validation
        assert len(config.sites) == 2
        assert config.sites[0].properties.land_cover.evetr.alb_min.value == 0.1
        assert config.sites[1].properties.land_cover.dectr.alb_min.value == 0.15

    def test_validate_albedo_ranges_multiple_sites_with_errors(self):
        """Test albedo range validation with errors across multiple sites."""
        with pytest.raises(
            ValidationError,
            match="site1 evergreen trees.*alb_min.*must be less than or equal to alb_max.*site2 grass.*alb_min.*must be less than or equal to alb_max",
        ):
            SUEWSConfig(
                name="test_multi_site_albedo_errors",
                description="Test multiple sites with different albedo range violations",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "land_cover": {
                                "evetr": {
                                    "alb_min": {"value": 0.5},  # > alb_max ✗
                                    "alb_max": {"value": 0.3},
                                }
                            }
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "grass": {
                                    "alb_min": {"value": 0.8},  # > alb_max ✗
                                    "alb_max": {"value": 0.6},
                                }
                            }
                        },
                    },
                ],
            )

    def test_validate_albedo_ranges_equal_values_pass(self):
        """Test that alb_min = alb_max passes validation (edge case)."""
        # The original validation allowed equal values (<=), so this should pass
        config = SUEWSConfig(
            name="test_equal_albedo_values",
            description="Test alb_min = alb_max edge case",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "evetr": {
                                "alb_min": {"value": 0.3},  # = alb_max - should pass
                                "alb_max": {"value": 0.3},
                            },
                            "grass": {
                                "alb_min": {"value": 0.4},  # = alb_max - should pass
                                "alb_max": {"value": 0.4},
                            },
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.land_cover.evetr.alb_min.value == 0.3
        assert config.sites[0].properties.land_cover.evetr.alb_max.value == 0.3

    def test_validate_albedo_ranges_edge_cases(self):
        """Test albedo range validation with various edge case values."""
        test_cases = [
            # (alb_min, alb_max, should_pass, surface_type, expected_error)
            (0.1, 0.3, True, "evetr", None),  # Valid range ✓
            (0.3, 0.3, True, "evetr", None),  # Equal values ✓ (original allowed <=)
            (0.4, 0.2, False, "evetr", "evergreen"),  # alb_min > alb_max ✗
            (0.0, 1.0, True, "grass", None),  # Full range ✓
            (0.5, 0.4, False, "dectr", "deciduous"),  # alb_min > alb_max ✗
        ]

        for i, (
            alb_min,
            alb_max,
            should_pass,
            surface_type,
            expected_error,
        ) in enumerate(test_cases):
            config_data = {
                "name": f"test_edge_case_{i}",
                "description": f"Edge case test {i} for {surface_type}",
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                surface_type: {
                                    "alb_min": {"value": alb_min},
                                    "alb_max": {"value": alb_max},
                                }
                            }
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                surface_props = getattr(
                    config.sites[0].properties.land_cover, surface_type
                )
                assert surface_props.alb_min.value == alb_min
                assert surface_props.alb_max.value == alb_max
            else:
                with pytest.raises(ValidationError, match=expected_error):
                    SUEWSConfig(**config_data)


class TestDeciduousPorosityRangesValidator:
    """Test cases for the validate_deciduous_porosity_ranges validator migrated to SUEWSConfig."""

    def test_validate_deciduous_porosity_ranges_valid_range_passes(self):
        """Test that deciduous trees with valid porosity ranges pass validation."""
        config = SUEWSConfig(
            name="test_valid_deciduous_porosity",
            description="Test deciduous trees with valid porosity range",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "pormin_dec": {"value": 0.3},  # < pormax_dec ✓
                                "pormax_dec": {"value": 0.7},
                            }
                        }
                    },
                }
            ],
        )

        # Should not raise any validation errors
        assert config.sites[0].properties.land_cover.dectr.pormin_dec.value == 0.3
        assert config.sites[0].properties.land_cover.dectr.pormax_dec.value == 0.7

    def test_validate_deciduous_porosity_ranges_violation_equal_values(self):
        """Test that pormin_dec = pormax_dec raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="test_site deciduous trees.*pormin_dec.*must be less than pormax_dec",
        ):
            SUEWSConfig(
                name="test_equal_porosity_violation",
                description="Test pormin_dec = pormax_dec violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {
                                        "value": 0.5
                                    },  # = pormax_dec - should fail
                                    "pormax_dec": {"value": 0.5},
                                }
                            }
                        },
                    }
                ],
            )

    def test_validate_deciduous_porosity_ranges_violation_greater_than(self):
        """Test that pormin_dec > pormax_dec raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="test_site deciduous trees.*pormin_dec.*must be less than pormax_dec",
        ):
            SUEWSConfig(
                name="test_greater_than_porosity_violation",
                description="Test pormin_dec > pormax_dec violation",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {
                                        "value": 0.8
                                    },  # > pormax_dec - should fail
                                    "pormax_dec": {"value": 0.6},
                                }
                            }
                        },
                    }
                ],
            )

    def test_validate_deciduous_porosity_ranges_with_refvalue_wrappers(self):
        """Test deciduous porosity range validation with RefValue wrapper objects."""
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_deciduous_porosity",
            description="Test deciduous porosity ranges with RefValue wrappers",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "pormin_dec": RefValue(0.25),
                                "pormax_dec": RefValue(0.75),
                            }
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.land_cover.dectr.pormin_dec.value == 0.25
        assert config.sites[0].properties.land_cover.dectr.pormax_dec.value == 0.75

    def test_validate_deciduous_porosity_ranges_multiple_sites(self):
        """Test deciduous porosity range validation across multiple sites."""
        config = SUEWSConfig(
            name="test_multi_site_deciduous_porosity",
            description="Test deciduous porosity range validation across multiple sites",
            sites=[
                {
                    "name": "site1_valid",
                    "gridiv": 0,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "pormin_dec": {"value": 0.2},
                                "pormax_dec": {"value": 0.6},
                            }
                        }
                    },
                },
                {
                    "name": "site2_valid",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "pormin_dec": {"value": 0.3},
                                "pormax_dec": {"value": 0.8},
                            }
                        }
                    },
                },
            ],
        )

        # Both sites should pass validation
        assert len(config.sites) == 2
        assert config.sites[0].properties.land_cover.dectr.pormin_dec.value == 0.2
        assert config.sites[1].properties.land_cover.dectr.pormin_dec.value == 0.3

    def test_validate_deciduous_porosity_ranges_multiple_sites_with_errors(self):
        """Test deciduous porosity range validation with errors across multiple sites."""
        with pytest.raises(
            ValidationError,
            match="site1 deciduous trees.*pormin_dec.*must be less than pormax_dec.*site2 deciduous trees.*pormin_dec.*must be less than pormax_dec",
        ):
            SUEWSConfig(
                name="test_multi_site_deciduous_porosity_errors",
                description="Test multiple sites with deciduous porosity violations",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {"value": 0.7},  # > pormax_dec ✗
                                    "pormax_dec": {"value": 0.5},
                                }
                            }
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {"value": 0.9},  # > pormax_dec ✗
                                    "pormax_dec": {"value": 0.6},
                                }
                            }
                        },
                    },
                ],
            )

    def test_validate_deciduous_porosity_ranges_sites_without_dectr(self):
        """Test that sites without deciduous trees are skipped gracefully."""
        # This should pass because sites without dectr properties are skipped
        config = SUEWSConfig(
            name="test_no_dectr_sites",
            description="Test sites without deciduous trees",
            sites=[
                {
                    "name": "site_with_dectr",
                    "gridiv": 0,
                    "properties": {
                        "land_cover": {
                            "dectr": {
                                "pormin_dec": {"value": 0.3},
                                "pormax_dec": {"value": 0.7},
                            }
                        }
                    },
                },
                {
                    "name": "site_without_dectr",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "evetr": {  # Only evergreen trees, no deciduous
                                "alb_min": {"value": 0.1},
                                "alb_max": {"value": 0.3},
                            }
                        }
                    },
                },
            ],
        )

        # Should pass - only site with dectr is validated
        assert config.sites[0].properties.land_cover.dectr.pormin_dec.value == 0.3
        assert config.sites[1].properties.land_cover.evetr.alb_min.value == 0.1

    def test_validate_deciduous_porosity_ranges_edge_cases(self):
        """Test deciduous porosity range validation with edge case values."""
        test_cases = [
            # (pormin_dec, pormax_dec, should_pass, expected_error)
            (0.1, 0.9, True, None),  # Valid range ✓
            (0.2, 0.8, True, None),  # Valid range ✓
            (0.4, 0.6, True, None),  # Valid mid-range ✓
            (0.5, 0.5, False, "deciduous"),  # Equal values ✗
            (0.7, 0.3, False, "deciduous"),  # pormin > pormax ✗
            (0.8, 0.2, False, "deciduous"),  # Large difference but wrong order ✗
        ]

        for i, (pormin_dec, pormax_dec, should_pass, expected_error) in enumerate(
            test_cases
        ):
            config_data = {
                "name": f"test_porosity_edge_case_{i}",
                "description": f"Porosity edge case test {i}",
                "sites": [
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {"value": pormin_dec},
                                    "pormax_dec": {"value": pormax_dec},
                                }
                            }
                        },
                    }
                ],
            }

            if should_pass:
                config = SUEWSConfig(**config_data)
                assert (
                    config.sites[0].properties.land_cover.dectr.pormin_dec.value
                    == pormin_dec
                )
                assert (
                    config.sites[0].properties.land_cover.dectr.pormax_dec.value
                    == pormax_dec
                )
            else:
                with pytest.raises(ValidationError, match=expected_error):
                    SUEWSConfig(**config_data)

    def test_validate_deciduous_porosity_mixed_valid_invalid_sites(self):
        """Test validation with mix of valid and invalid deciduous porosity across sites."""
        # Test that one valid site and one invalid site correctly reports only the invalid one
        with pytest.raises(
            ValidationError,
            match="site2 deciduous trees.*pormin_dec.*must be less than pormax_dec",
        ):
            SUEWSConfig(
                name="test_mixed_deciduous_porosity",
                description="Test mixed valid/invalid deciduous porosity sites",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {"value": 0.2},  # Valid ✓
                                    "pormax_dec": {"value": 0.8},
                                }
                            }
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "land_cover": {
                                "dectr": {
                                    "pormin_dec": {"value": 0.9},  # Invalid ✗
                                    "pormax_dec": {"value": 0.4},
                                }
                            }
                        },
                    },
                ],
            )


class TestBuildingLayersValidator:
    """Test class for validate_building_layers validator migration."""

    def test_validate_building_layers_valid_configuration(self):
        """Test that valid building layer configuration passes validation."""
        # Create a config with valid building layer setup
        config = SUEWSConfig(
            name="test_valid_building_layers",
            description="Test valid building layer configuration",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": 3,
                            "height": [0.0, 10.0, 20.0, 30.0],  # nlayer+1 = 4 elements
                            "building_frac": [0.4, 0.3, 0.3],  # nlayer = 3 elements
                            "building_scale": [1.0, 1.0, 1.0],  # nlayer = 3 elements
                            "roofs": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 3 elements
                            ],
                            "walls": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 3 elements
                            ],
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.vertical_layers.nlayer == 3
        assert len(config.sites[0].properties.vertical_layers.height) == 4
        assert len(config.sites[0].properties.vertical_layers.building_frac) == 3

    def test_validate_building_layers_invalid_height_array(self):
        """Test that height array length != nlayer+1 raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="Site 1.*Building heights array length.*must be nlayer\\+1",
        ):
            SUEWSConfig(
                name="test_invalid_height_array",
                description="Test invalid height array length",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [
                                    0.0,
                                    10.0,
                                    20.0,
                                ],  # Should be 4 elements (nlayer+1)
                                "building_frac": [0.4, 0.3, 0.3],
                                "building_scale": [1.0, 1.0, 1.0],
                            }
                        },
                    }
                ],
            )

    def test_validate_building_layers_invalid_fractions_array(self):
        """Test that building_frac array length != nlayer raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="Site 1.*Building fractions array length.*must match nlayer",
        ):
            SUEWSConfig(
                name="test_invalid_fractions_array",
                description="Test invalid building fractions array length",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [0.0, 10.0, 20.0, 30.0],
                                "building_frac": [
                                    0.4,
                                    0.3,
                                ],  # Should be 3 elements (nlayer)
                                "building_scale": [1.0, 1.0, 1.0],
                            }
                        },
                    }
                ],
            )

    def test_validate_building_layers_invalid_scales_array(self):
        """Test that building_scale array length != nlayer raises ValidationError."""
        with pytest.raises(
            ValidationError,
            match="Site 1.*Building scales array length.*must match nlayer",
        ):
            SUEWSConfig(
                name="test_invalid_scales_array",
                description="Test invalid building scales array length",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [0.0, 10.0, 20.0, 30.0],
                                "building_frac": [0.4, 0.3, 0.3],
                                "building_scale": [
                                    1.0,
                                    1.0,
                                    1.0,
                                    1.0,
                                ],  # Should be 3 elements (nlayer)
                            }
                        },
                    }
                ],
            )

    def test_validate_building_layers_invalid_roof_layers_count(self):
        """Test that roof layers count != nlayer raises ValidationError."""
        with pytest.raises(
            ValidationError, match="Site 1.*Roof layers count.*must match nlayer"
        ):
            SUEWSConfig(
                name="test_invalid_roof_layers",
                description="Test invalid roof layers count",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [0.0, 10.0, 20.0, 30.0],
                                "building_frac": [0.4, 0.3, 0.3],
                                "building_scale": [1.0, 1.0, 1.0],
                                "roofs": [
                                    {"layers": [], "properties": {}},
                                    {
                                        "layers": [],
                                        "properties": {},
                                    },  # Should be 3 elements (nlayer)
                                ],
                            }
                        },
                    }
                ],
            )

    def test_validate_building_layers_invalid_wall_layers_count(self):
        """Test that wall layers count != nlayer raises ValidationError."""
        with pytest.raises(
            ValidationError, match="Site 1.*Wall layers count.*must match nlayer"
        ):
            SUEWSConfig(
                name="test_invalid_wall_layers",
                description="Test invalid wall layers count",
                sites=[
                    {
                        "name": "test_site",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [0.0, 10.0, 20.0, 30.0],
                                "building_frac": [0.4, 0.3, 0.3],
                                "building_scale": [1.0, 1.0, 1.0],
                                "walls": [
                                    {"layers": [], "properties": {}},
                                    {"layers": [], "properties": {}},
                                    {"layers": [], "properties": {}},
                                    {
                                        "layers": [],
                                        "properties": {},
                                    },  # Should be 3 elements (nlayer)
                                ],
                            }
                        },
                    }
                ],
            )

    def test_validate_building_layers_with_refvalue_wrappers(self):
        """Test building layer validation with RefValue wrapper objects."""
        from supy.data_model.type import RefValue

        config = SUEWSConfig(
            name="test_refvalue_building_layers",
            description="Test building layer validation with RefValue wrappers",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": RefValue(2),
                            "height": RefValue([
                                0.0,
                                15.0,
                                30.0,
                            ]),  # nlayer+1 = 3 elements
                            "building_frac": RefValue([
                                0.6,
                                0.4,
                            ]),  # nlayer = 2 elements
                            "building_scale": RefValue([
                                1.2,
                                0.8,
                            ]),  # nlayer = 2 elements
                            "roofs": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                            "walls": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.vertical_layers.nlayer.value == 2
        assert len(config.sites[0].properties.vertical_layers.height.value) == 3
        assert len(config.sites[0].properties.vertical_layers.building_frac.value) == 2

    def test_validate_building_layers_multi_site_validation(self):
        """Test building layer validation across multiple sites."""
        config = SUEWSConfig(
            name="test_multi_site_building_layers",
            description="Test building layer validation with multiple sites",
            sites=[
                {
                    "name": "site1",
                    "gridiv": 0,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": 2,
                            "height": [0.0, 12.0, 24.0],  # Valid: nlayer+1 = 3
                            "building_frac": [0.7, 0.3],  # Valid: nlayer = 2
                            "building_scale": [1.0, 1.0],  # Valid: nlayer = 2
                            "roofs": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                            "walls": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                        }
                    },
                },
                {
                    "name": "site2",
                    "gridiv": 1,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": 4,
                            "height": [
                                0.0,
                                5.0,
                                10.0,
                                15.0,
                                20.0,
                            ],  # Valid: nlayer+1 = 5
                            "building_frac": [
                                0.25,
                                0.25,
                                0.25,
                                0.25,
                            ],  # Valid: nlayer = 4
                            "building_scale": [1.0, 1.0, 1.0, 1.0],  # Valid: nlayer = 4
                            "roofs": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 4 elements
                            ],
                            "walls": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 4 elements
                            ],
                        }
                    },
                },
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.vertical_layers.nlayer == 2
        assert config.sites[1].properties.vertical_layers.nlayer == 4

    def test_validate_building_layers_multi_site_with_error(self):
        """Test building layer validation with one invalid site among multiple."""
        # Test that one valid site and one invalid site correctly reports only the invalid one
        with pytest.raises(
            ValidationError,
            match="Site 2.*Building heights array length.*must be nlayer\\+1",
        ):
            SUEWSConfig(
                name="test_mixed_building_layers",
                description="Test mixed valid/invalid building layer sites",
                sites=[
                    {
                        "name": "site1",
                        "gridiv": 0,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 2,
                                "height": [0.0, 12.0, 24.0],  # Valid: nlayer+1 = 3
                                "building_frac": [0.7, 0.3],  # Valid: nlayer = 2
                                "building_scale": [1.0, 1.0],  # Valid: nlayer = 2
                                "roofs": [
                                    {"layers": [], "properties": {}},
                                    {
                                        "layers": [],
                                        "properties": {},
                                    },  # nlayer = 2 elements
                                ],
                                "walls": [
                                    {"layers": [], "properties": {}},
                                    {
                                        "layers": [],
                                        "properties": {},
                                    },  # nlayer = 2 elements
                                ],
                            }
                        },
                    },
                    {
                        "name": "site2",
                        "gridiv": 1,
                        "properties": {
                            "vertical_layers": {
                                "nlayer": 3,
                                "height": [
                                    0.0,
                                    10.0,
                                ],  # Invalid: should be nlayer+1 = 4
                                "building_frac": [0.4, 0.3, 0.3],
                                "building_scale": [1.0, 1.0, 1.0],
                            }
                        },
                    },
                ],
            )

    def test_validate_building_layers_skip_sites_without_bldg(self):
        """Test that sites without building surface are skipped."""
        config = SUEWSConfig(
            name="test_skip_no_bldg",
            description="Test sites without building surface are skipped",
            sites=[
                {
                    "name": "site_with_bldgs",
                    "gridiv": 0,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": 2,
                            "height": [0.0, 12.0, 24.0],
                            "building_frac": [0.7, 0.3],
                            "building_scale": [1.0, 1.0],
                            "roofs": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                            "walls": [
                                {"layers": [], "properties": {}},
                                {"layers": [], "properties": {}},  # nlayer = 2 elements
                            ],
                        }
                    },
                },
                {
                    "name": "site_without_bldgs",
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {"grass": {"lai": {"laimax": 5.0}}}
                        # No vertical_layers specified - will use defaults
                    },
                },
            ],
        )

        # Should not raise validation errors, both sites have valid vertical_layers
        assert config.sites[0].properties.vertical_layers.nlayer == 2
        # Site without explicit vertical_layers still has default VerticalLayers object
        assert config.sites[1].properties.vertical_layers is not None

    def test_validate_building_layers_edge_case_nlayer_1(self):
        """Test building layer validation with nlayer=1 (minimum case)."""
        config = SUEWSConfig(
            name="test_nlayer_1",
            description="Test building layer validation with nlayer=1",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "vertical_layers": {
                            "nlayer": 1,
                            "height": [0.0, 20.0],  # nlayer+1 = 2 elements
                            "building_frac": [1.0],  # nlayer = 1 element
                            "building_scale": [1.0],  # nlayer = 1 element
                            "roofs": [
                                {"layers": [], "properties": {}}  # nlayer = 1 element
                            ],
                            "walls": [
                                {"layers": [], "properties": {}}  # nlayer = 1 element
                            ],
                        }
                    },
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].properties.vertical_layers.nlayer == 1
        assert len(config.sites[0].properties.vertical_layers.height) == 2


class TestSurfaceStatesValidator:
    """Test class for validate_surface_states validator migration."""

    def test_validate_surface_states_valid_configuration(self):
        """Test that valid surface state configuration passes validation."""
        from supy.data_model.type import SurfaceType

        # Create initial state objects with proper surface types
        class MockInitialStateVeg:
            def __init__(self, surface_type):
                self._surface_type = surface_type

        class MockInitialStates:
            def __init__(self):
                self.evetr = MockInitialStateVeg(SurfaceType.EVETR)
                self.dectr = MockInitialStateVeg(SurfaceType.DECTR)
                self.grass = MockInitialStateVeg(SurfaceType.GRASS)

        # Note: We'll create a minimal config that bypasses the complex initial state structure
        # The actual test will rely on the fact that surface states have default surface types
        config = SUEWSConfig(
            name="test_valid_surface_states",
            description="Test valid surface state configuration",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {
                        # Default initial states should have correct surface types
                    },
                }
            ],
        )

        # Should not raise validation errors - default surface types are correct
        assert config.sites[0].initial_states is not None
        assert hasattr(config.sites[0].initial_states, "evetr")
        assert hasattr(config.sites[0].initial_states, "dectr")
        assert hasattr(config.sites[0].initial_states, "grass")

    def test_validate_surface_states_skip_missing_initial_states(self):
        """Test that sites without initial_states are skipped."""
        config = SUEWSConfig(
            name="test_skip_missing_initial_states",
            description="Test skipping sites without initial states",
            sites=[
                {
                    "name": "site_without_initial_states",
                    "gridiv": 1,
                    # No initial_states specified - should be skipped in validation
                }
            ],
        )

        # Should not raise validation errors, even without initial_states
        assert config.sites[0].initial_states is not None  # Has defaults

    def test_validate_surface_states_skip_missing_surface_type(self):
        """Test that surface states without _surface_type are skipped."""
        # This test validates the skip logic in the validator
        # Since we can't easily mock the complex structure, we rely on the default behavior
        config = SUEWSConfig(
            name="test_skip_missing_surface_type",
            description="Test skipping surface states without surface type",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    # Will use default initial states which should have proper surface types
                }
            ],
        )

        # Should not raise validation errors
        assert config.sites[0].initial_states.evetr._surface_type.value == "evetr"
        assert config.sites[0].initial_states.dectr._surface_type.value == "dectr"
        assert config.sites[0].initial_states.grass._surface_type.value == "grass"

    def test_validate_surface_states_multi_site_validation(self):
        """Test surface state validation across multiple sites."""
        config = SUEWSConfig(
            name="test_multi_site_surface_states",
            description="Test surface state validation with multiple sites",
            sites=[
                {
                    "name": "site1",
                    "gridiv": 0,
                    # Will use defaults
                },
                {
                    "name": "site2",
                    "gridiv": 1,
                    # Will use defaults
                },
            ],
        )

        # Should not raise validation errors for either site
        assert config.sites[0].initial_states is not None
        assert config.sites[1].initial_states is not None

        # Both sites should have correct surface types
        for site_index in [0, 1]:
            site = config.sites[site_index]
            assert site.initial_states.evetr._surface_type.value == "evetr"
            assert site.initial_states.dectr._surface_type.value == "dectr"
            assert site.initial_states.grass._surface_type.value == "grass"

    def test_validate_surface_states_comprehensive_coverage(self):
        """Test that the validator covers all vegetated surface types."""
        # This test ensures the validator checks evetr, dectr, and grass
        config = SUEWSConfig(
            name="test_comprehensive_coverage",
            description="Test comprehensive surface state validation coverage",
            sites=[{"name": "test_site", "gridiv": 1}],
        )

        # Verify all vegetated surfaces are present and have correct types
        initial_states = config.sites[0].initial_states

        # Check that all expected surface types exist
        assert hasattr(initial_states, "evetr")
        assert hasattr(initial_states, "dectr")
        assert hasattr(initial_states, "grass")

        # Check surface types are set correctly
        assert initial_states.evetr._surface_type.value == "evetr"
        assert initial_states.dectr._surface_type.value == "dectr"
        assert initial_states.grass._surface_type.value == "grass"

        # Also check non-vegetated surfaces exist (they should be skipped by validator)
        assert hasattr(initial_states, "paved")
        assert hasattr(initial_states, "bldgs")
        assert hasattr(initial_states, "bsoil")
        assert hasattr(initial_states, "water")

    def test_validate_surface_states_integration_with_other_validators(self):
        """Test surface state validation works alongside other validators."""
        config = SUEWSConfig(
            name="test_integration",
            description="Test integration with other validators",
            sites=[
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "properties": {
                        "snow": {
                            "crwmin": 0.15,  # Valid snow parameters
                            "crwmax": 0.25,
                        }
                    },
                }
            ],
        )

        # Should pass both surface state validation and snow validation
        assert config.sites[0].initial_states is not None
        # Note: When passed as dict values, they're not RefValue wrapped
        assert config.sites[0].properties.snow.crwmin == 0.15
        assert config.sites[0].properties.snow.crwmax == 0.25


class TestHDDIDConverterValidator:
    """Test class for convert_legacy_hdd_formats validator migration."""

    def test_convert_legacy_hdd_formats_valid_list_conversion(self):
        """Test that legacy HDD_ID list format converts to dictionary."""
        # Create config with legacy list format
        config_data = {
            "name": "test_hdd_conversion",
            "description": "Test HDD ID conversion from list format",
            "sites": [
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {
                        "hdd_id": [
                            1.0,  # hdd_accum
                            2.0,  # cdd_accum
                            3.0,  # temp_accum
                            4.0,  # temp_5day_accum
                            5.0,  # precip_accum
                            6.0,  # days_since_rain_accum
                            7.0,  # hdd_daily
                            8.0,  # cdd_daily
                            9.0,  # temp_daily_mean
                            10.0,  # temp_5day_mean
                            11.0,  # precip_daily_total
                            12.0,  # days_since_rain
                        ]
                    },
                }
            ],
        }

        config = SUEWSConfig(**config_data)

        # Should convert list to dictionary structure
        hdd_id = config.sites[0].initial_states.hdd_id
        assert hdd_id.hdd_accum == 1.0
        assert hdd_id.cdd_accum == 2.0
        assert hdd_id.temp_accum == 3.0
        assert hdd_id.temp_5day_accum == 4.0
        assert hdd_id.precip_accum == 5.0
        assert hdd_id.days_since_rain_accum == 6.0
        assert hdd_id.hdd_daily == 7.0
        assert hdd_id.cdd_daily == 8.0
        assert hdd_id.temp_daily_mean == 9.0
        assert hdd_id.temp_5day_mean == 10.0
        assert hdd_id.precip_daily_total == 11.0
        assert hdd_id.days_since_rain == 12.0

    def test_convert_legacy_hdd_formats_short_list_defaults(self):
        """Test that short HDD_ID list creates default empty dictionary."""
        config_data = {
            "name": "test_hdd_short_list",
            "description": "Test HDD ID with short list",
            "sites": [
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {
                        "hdd_id": [
                            1.0,
                            2.0,
                            3.0,
                        ]  # Only 3 elements, should create defaults
                    },
                }
            ],
        }

        config = SUEWSConfig(**config_data)

        # Should create default HDD_ID object (all zeros)
        hdd_id = config.sites[0].initial_states.hdd_id
        assert hdd_id.hdd_accum == 0.0
        assert hdd_id.cdd_accum == 0.0
        assert hdd_id.temp_accum == 0.0

    def test_convert_legacy_hdd_formats_dict_unchanged(self):
        """Test that existing dictionary format is left unchanged."""
        config_data = {
            "name": "test_hdd_dict_format",
            "description": "Test HDD ID with existing dict format",
            "sites": [
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {
                        "hdd_id": {
                            "hdd_accum": 100.0,
                            "cdd_accum": 200.0,
                            "temp_accum": 300.0,
                        }
                    },
                }
            ],
        }

        config = SUEWSConfig(**config_data)

        # Dictionary format should be preserved
        hdd_id = config.sites[0].initial_states.hdd_id
        assert hdd_id.hdd_accum == 100.0
        assert hdd_id.cdd_accum == 200.0
        assert hdd_id.temp_accum == 300.0

    def test_convert_legacy_hdd_formats_no_hdd_id_field(self):
        """Test sites without hdd_id field are handled gracefully."""
        config_data = {
            "name": "test_no_hdd_id",
            "description": "Test site without HDD ID field",
            "sites": [
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {
                        # No hdd_id field - should use defaults
                    },
                }
            ],
        }

        config = SUEWSConfig(**config_data)

        # Should create default HDD_ID object
        hdd_id = config.sites[0].initial_states.hdd_id
        assert hdd_id.hdd_accum == 0.0
        assert hdd_id.cdd_accum == 0.0

    def test_convert_legacy_hdd_formats_multi_site_conversion(self):
        """Test HDD ID conversion across multiple sites."""
        config_data = {
            "name": "test_multi_site_hdd_conversion",
            "description": "Test HDD ID conversion with multiple sites",
            "sites": [
                {
                    "name": "site1",
                    "gridiv": 0,
                    "initial_states": {
                        "hdd_id": [
                            10.0,
                            20.0,
                            30.0,
                            40.0,
                            50.0,
                            60.0,
                            70.0,
                            80.0,
                            90.0,
                            100.0,
                            110.0,
                            120.0,
                        ]
                    },
                },
                {
                    "name": "site2",
                    "gridiv": 1,
                    "initial_states": {
                        "hdd_id": {"hdd_accum": 999.0, "cdd_accum": 888.0}
                    },
                },
            ],
        }

        config = SUEWSConfig(**config_data)

        # Site1 should have converted list to dict
        hdd_id1 = config.sites[0].initial_states.hdd_id
        assert hdd_id1.hdd_accum == 10.0
        assert hdd_id1.cdd_accum == 20.0
        assert hdd_id1.days_since_rain == 120.0

        # Site2 should preserve existing dict
        hdd_id2 = config.sites[1].initial_states.hdd_id
        assert hdd_id2.hdd_accum == 999.0
        assert hdd_id2.cdd_accum == 888.0

    def test_convert_legacy_hdd_formats_mixed_sites_with_without_hdd(self):
        """Test conversion with mix of sites that have/don't have HDD ID."""
        config_data = {
            "name": "test_mixed_hdd_sites",
            "description": "Test mixed sites with and without HDD ID",
            "sites": [
                {
                    "name": "site_with_hdd_list",
                    "gridiv": 0,
                    "initial_states": {
                        "hdd_id": [
                            1.0,
                            2.0,
                            3.0,
                            4.0,
                            5.0,
                            6.0,
                            7.0,
                            8.0,
                            9.0,
                            10.0,
                            11.0,
                            12.0,
                        ]
                    },
                },
                {
                    "name": "site_without_hdd",
                    "gridiv": 1,
                    "initial_states": {
                        # No hdd_id field
                    },
                },
                {
                    "name": "site_with_short_list",
                    "gridiv": 2,
                    "initial_states": {
                        "hdd_id": [100.0, 200.0]  # Short list
                    },
                },
            ],
        }

        config = SUEWSConfig(**config_data)

        # Site 1: Full list conversion
        assert config.sites[0].initial_states.hdd_id.hdd_accum == 1.0
        assert config.sites[0].initial_states.hdd_id.days_since_rain == 12.0

        # Site 2: Default HDD_ID object
        assert config.sites[1].initial_states.hdd_id.hdd_accum == 0.0

        # Site 3: Short list creates defaults
        assert config.sites[2].initial_states.hdd_id.hdd_accum == 0.0

    def test_convert_legacy_hdd_formats_backward_compatibility(self):
        """Test that conversion maintains full backward compatibility."""
        # This test validates the exact same field mapping as the original
        legacy_list = [
            1.5,  # hdd_accum
            2.5,  # cdd_accum
            3.5,  # temp_accum
            4.5,  # temp_5day_accum
            5.5,  # precip_accum
            6.5,  # days_since_rain_accum
            7.5,  # hdd_daily
            8.5,  # cdd_daily
            9.5,  # temp_daily_mean
            10.5,  # temp_5day_mean
            11.5,  # precip_daily_total
            12.5,  # days_since_rain
        ]

        config_data = {
            "name": "test_backward_compatibility",
            "description": "Test exact backward compatibility",
            "sites": [
                {
                    "name": "test_site",
                    "gridiv": 1,
                    "initial_states": {"hdd_id": legacy_list},
                }
            ],
        }

        config = SUEWSConfig(**config_data)
        hdd_id = config.sites[0].initial_states.hdd_id

        # Verify exact field mapping matches original converter
        field_mapping = [
            ("hdd_accum", 0),
            ("cdd_accum", 1),
            ("temp_accum", 2),
            ("temp_5day_accum", 3),
            ("precip_accum", 4),
            ("days_since_rain_accum", 5),
            ("hdd_daily", 6),
            ("cdd_daily", 7),
            ("temp_daily_mean", 8),
            ("temp_5day_mean", 9),
            ("precip_daily_total", 10),
            ("days_since_rain", 11),
        ]

        for field_name, index in field_mapping:
            expected_value = legacy_list[index]
            actual_value = getattr(hdd_id, field_name)
            assert actual_value == expected_value, (
                f"Field {field_name} mismatch: expected {expected_value}, got {actual_value}"
            )


class TestSurfaceTypesValidator:
    """Test surface types validation migrated from LandCover.set_surface_types"""

    def test_set_surface_types_validation_basic(self):
        """Test basic surface type setting functionality"""
        config_data = {
            "sites": [
                {
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.2},
                            "bldgs": {"sfr": 0.3},
                            "dectr": {"sfr": 0.1},
                            "evetr": {"sfr": 0.1},
                            "grass": {"sfr": 0.2},
                            "bsoil": {"sfr": 0.05},
                            "water": {"sfr": 0.05},
                        }
                    }
                }
            ]
        }

        # Should create successfully - validator sets surface types
        result = SUEWSConfig(**config_data)

        # Verify surface types were set correctly
        land_cover = result.sites[0].properties.land_cover
        from supy.data_model.type import SurfaceType

        # Each surface property should have its surface type set
        assert hasattr(land_cover.paved, "_surface_type")
        assert land_cover.paved._surface_type == SurfaceType.PAVED
        assert land_cover.bldgs._surface_type == SurfaceType.BLDGS
        assert land_cover.dectr._surface_type == SurfaceType.DECTR
        assert land_cover.evetr._surface_type == SurfaceType.EVETR
        assert land_cover.grass._surface_type == SurfaceType.GRASS
        assert land_cover.bsoil._surface_type == SurfaceType.BSOIL
        assert land_cover.water._surface_type == SurfaceType.WATER

    def test_set_surface_types_validation_multi_site(self):
        """Test surface types setting across multiple sites"""
        config_data = {
            "sites": [
                {
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.4},
                            "bldgs": {"sfr": 0.4},
                            "grass": {"sfr": 0.2},
                        }
                    }
                },
                {
                    "properties": {
                        "land_cover": {
                            "dectr": {"sfr": 0.3},
                            "evetr": {"sfr": 0.3},
                            "water": {"sfr": 0.4},
                        }
                    }
                },
            ]
        }

        result = SUEWSConfig(**config_data)
        from supy.data_model.type import SurfaceType

        # Check first site
        land_cover_1 = result.sites[0].properties.land_cover
        assert land_cover_1.paved._surface_type == SurfaceType.PAVED
        assert land_cover_1.bldgs._surface_type == SurfaceType.BLDGS
        assert land_cover_1.grass._surface_type == SurfaceType.GRASS

        # Check second site
        land_cover_2 = result.sites[1].properties.land_cover
        assert land_cover_2.dectr._surface_type == SurfaceType.DECTR
        assert land_cover_2.evetr._surface_type == SurfaceType.EVETR
        assert land_cover_2.water._surface_type == SurfaceType.WATER

    def test_set_surface_types_validation_missing_land_cover(self):
        """Test validator handles missing land_cover gracefully"""
        config_data = {
            "sites": [
                {
                    # No land_cover specified
                }
            ]
        }

        # Should create successfully - validator handles missing land_cover
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1
        # land_cover should be created with defaults
        assert result.sites[0].properties.land_cover is not None

    def test_set_surface_types_validation_partial_surfaces(self):
        """Test validator with only some surfaces defined"""
        config_data = {
            "sites": [
                {
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.5},
                            "grass": {"sfr": 0.3},
                            "water": {"sfr": 0.2},
                            # Only 3 surfaces defined
                        }
                    }
                }
            ]
        }

        result = SUEWSConfig(**config_data)
        from supy.data_model.type import SurfaceType

        land_cover = result.sites[0].properties.land_cover

        # Defined surfaces should have correct types
        assert land_cover.paved._surface_type == SurfaceType.PAVED
        assert land_cover.grass._surface_type == SurfaceType.GRASS
        assert land_cover.water._surface_type == SurfaceType.WATER

        # Default surfaces should also have types set
        assert land_cover.bldgs._surface_type == SurfaceType.BLDGS
        assert land_cover.dectr._surface_type == SurfaceType.DECTR
        assert land_cover.evetr._surface_type == SurfaceType.EVETR
        assert land_cover.bsoil._surface_type == SurfaceType.BSOIL

    def test_set_surface_types_validation_surface_mapping(self):
        """Test surface type mapping is complete and correct"""
        config_data = {
            "sites": [
                {
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.1},
                            "bldgs": {"sfr": 0.1},
                            "dectr": {"sfr": 0.1},
                            "evetr": {"sfr": 0.1},
                            "grass": {"sfr": 0.1},
                            "bsoil": {"sfr": 0.1},
                            "water": {"sfr": 0.4},
                        }
                    }
                }
            ]
        }

        result = SUEWSConfig(**config_data)
        from supy.data_model.type import SurfaceType

        land_cover = result.sites[0].properties.land_cover

        # Test all expected surface type mappings
        surface_mappings = {
            "paved": SurfaceType.PAVED,
            "bldgs": SurfaceType.BLDGS,
            "dectr": SurfaceType.DECTR,
            "evetr": SurfaceType.EVETR,
            "grass": SurfaceType.GRASS,
            "bsoil": SurfaceType.BSOIL,
            "water": SurfaceType.WATER,
        }

        for surface_name, expected_type in surface_mappings.items():
            surface_prop = getattr(land_cover, surface_name)
            assert hasattr(surface_prop, "_surface_type")
            assert surface_prop._surface_type == expected_type, (
                f"Surface {surface_name} should have type {expected_type}, "
                f"got {surface_prop._surface_type}"
            )

    def test_set_surface_types_validation_no_error_on_empty_site(self):
        """Test validator handles empty site configuration"""
        config_data = {
            "sites": [{}]  # Completely empty site
        }

        # Should not raise errors - validator is defensive
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1


class TestModelPhysicsValidator:
    """Test model physics compatibility validation migrated from ModelPhysics.check_all"""

    def test_validate_model_physics_compatibility_valid_config(self):
        """Test valid model physics configuration passes validation"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 1,  # OHM_WITHOUT_QF
                    "ohmincqf": 0,  # EXCLUDE (compatible)
                    "snowuse": 0,  # DISABLED
                }
            },
            "sites": [{"name": "test_site"}],
        }

        # Should create successfully with compatible settings
        result = SUEWSConfig(**config_data)
        assert result.model.physics.storageheatmethod.value == 1
        assert result.model.physics.ohmincqf.value == 0
        assert result.model.physics.snowuse.value == 0

    def test_validate_model_physics_compatibility_storage_heat_method_1_error(self):
        """Test StorageHeatMethod=1 with incompatible OhmIncQf raises error"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 1,  # OHM_WITHOUT_QF
                    "ohmincqf": 1,  # INCLUDE (incompatible!)
                    "snowuse": 0,
                }
            },
            "sites": [{"name": "test_site"}],
        }

        with pytest.raises(
            ValueError, match="StorageHeatMethod is set to 1 and OhmIncQf is set to 1"
        ):
            SUEWSConfig(**config_data)

    def test_validate_model_physics_compatibility_storage_heat_method_invalid_enum(
        self,
    ):
        """Test invalid StorageHeatMethod value raises Pydantic error"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 2,  # Invalid - not in enum
                    "ohmincqf": 0,
                    "snowuse": 0,
                }
            },
            "sites": [{"name": "test_site"}],
        }

        # Should raise ValidationError for invalid enum value, not our custom error
        from pydantic_core import ValidationError as CoreValidationError

        with pytest.raises(
            CoreValidationError, match="Input should be 0, 1, 3, 4, 5 or 6"
        ):
            SUEWSConfig(**config_data)

    def test_validate_model_physics_compatibility_snow_use_enabled_error(self):
        """Test SnowUse=1 raises error (experimental feature)"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 1,
                    "ohmincqf": 0,
                    "snowuse": 1,  # ENABLED (experimental!)
                }
            },
            "sites": [{"name": "test_site"}],
        }

        with pytest.raises(
            ValueError, match="SnowUse is set to 1.*There are no checks implemented"
        ):
            SUEWSConfig(**config_data)

    def test_validate_model_physics_compatibility_multiple_errors(self):
        """Test multiple physics errors are reported together"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 1,  # Incompatible with ohmincqf=1
                    "ohmincqf": 1,  # Error 1: Wrong OhmIncQf for method 1
                    "snowuse": 1,  # Error 2: Experimental feature
                }
            },
            "sites": [{"name": "test_site"}],
        }

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig(**config_data)

        error_msg = str(exc_info.value)
        # Should contain both errors
        assert "StorageHeatMethod is set to 1 and OhmIncQf is set to 1" in error_msg
        assert "SnowUse is set to 1" in error_msg

    def test_validate_model_physics_compatibility_refvalue_handling(self):
        """Test validator handles RefValue wrappers correctly"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": {"value": 1},  # RefValue format
                    "ohmincqf": {"value": 1},  # RefValue format (incompatible!)
                    "snowuse": {"value": 0},
                }
            },
            "sites": [{"name": "test_site"}],
        }

        with pytest.raises(
            ValueError, match="StorageHeatMethod is set to 1 and OhmIncQf is set to 1"
        ):
            SUEWSConfig(**config_data)

    def test_validate_model_physics_compatibility_error_message_format(self):
        """Test error message format for physics compatibility errors"""
        config_data = {
            "model": {
                "physics": {
                    "storageheatmethod": 1,
                    "ohmincqf": 1,  # Error condition
                    "snowuse": 0,
                }
            },
            "sites": [{"name": "test_site"}],
        }

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig(**config_data)

        # Error should contain specific information about the incompatible settings
        error_msg = str(exc_info.value)
        assert "StorageHeatMethod is set to 1 and OhmIncQf is set to 1" in error_msg
        assert "You should switch to OhmIncQf=0" in error_msg

    def test_validate_model_physics_compatibility_missing_model_graceful(self):
        """Test validator handles missing model configuration gracefully"""
        config_data = {
            # No model section
            "sites": [{"name": "test_site"}]
        }

        # Should create successfully - validator is defensive
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1

    def test_validate_model_physics_compatibility_valid_combinations(self):
        """Test all valid storage heat method combinations"""
        valid_combinations = [
            (1, 0),  # OHM_WITHOUT_QF + EXCLUDE (compatible)
            (0, 0),  # OBSERVED + EXCLUDE
            (0, 1),  # OBSERVED + INCLUDE
            (3, 0),  # ANOHM + EXCLUDE
            (3, 1),  # ANOHM + INCLUDE
        ]

        for storageheat, ohmincqf in valid_combinations:
            config_data = {
                "model": {
                    "physics": {
                        "storageheatmethod": storageheat,
                        "ohmincqf": ohmincqf,
                        "snowuse": 0,
                    }
                },
                "sites": [{"name": f"test_{storageheat}_{ohmincqf}"}],
            }

            # Should create successfully
            result = SUEWSConfig(**config_data)
            assert result.model.physics.storageheatmethod.value == storageheat
            assert result.model.physics.ohmincqf.value == ohmincqf


class TestHourlyProfileValidator:
    """Test hourly profile validation migrated from HourlyProfile.validate_hours"""

    def test_validate_hourly_profile_hours_valid_profiles(self):
        """Test valid hourly profiles pass validation"""
        # Create valid 24-hour profiles
        valid_profile = {str(h): 1.0 / 24.0 for h in range(1, 25)}

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            }
                        },
                        "irrigation": {
                            "wuprofa_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            },
                            "wuprofm_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            },
                        },
                    }
                }
            ]
        }

        # Should create successfully with valid profiles
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1

        # Verify the profiles were created correctly
        snow_profile = result.sites[0].properties.snow.snowprof_24hr
        assert len(snow_profile.working_day) == 24
        assert len(snow_profile.holiday) == 24

    def test_validate_hourly_profile_hours_missing_hours_error(self):
        """Test missing hours in profile raises error"""
        # Create profile missing hour 24
        invalid_profile = {str(h): 1.0 / 23.0 for h in range(1, 24)}  # Missing hour 24

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": invalid_profile,
                                "holiday": {str(h): 1.0 / 24.0 for h in range(1, 25)},
                            }
                        }
                    }
                }
            ]
        }

        with pytest.raises(ValueError, match="missing hours: \\[24\\]"):
            SUEWSConfig(**config_data)

    def test_validate_hourly_profile_hours_extra_hours_error(self):
        """Test extra hours in profile raises error"""
        # Create profile with extra hour 25
        invalid_profile = {str(h): 1.0 / 25.0 for h in range(1, 26)}  # Extra hour 25

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": invalid_profile,
                                "holiday": {str(h): 1.0 / 24.0 for h in range(1, 25)},
                            }
                        }
                    }
                }
            ]
        }

        with pytest.raises(ValueError, match="extra hours: \\[25\\]"):
            SUEWSConfig(**config_data)

    def test_validate_hourly_profile_hours_out_of_range_error(self):
        """Test hour values outside 1-24 range raise error"""
        # Create profile with hour 0 (invalid)
        invalid_profile = {
            str(h): 1.0 / 24.0 for h in range(0, 24)
        }  # Hour 0 instead of 24

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": invalid_profile,
                                "holiday": {str(h): 1.0 / 24.0 for h in range(1, 25)},
                            }
                        }
                    }
                }
            ]
        }

        with pytest.raises(ValueError, match="hour values outside range 1-24"):
            SUEWSConfig(**config_data)

    def test_validate_hourly_profile_hours_invalid_hour_keys_error(self):
        """Test non-numeric hour keys raise error"""
        # Create profile with invalid hour keys
        invalid_profile = {"morning": 0.5, "afternoon": 0.3, "evening": 0.2}

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": invalid_profile,
                                "holiday": {str(h): 1.0 / 24.0 for h in range(1, 25)},
                            }
                        }
                    }
                }
            ]
        }

        with pytest.raises(ValueError, match="invalid hour keys"):
            SUEWSConfig(**config_data)

    def test_validate_hourly_profile_hours_multiple_profile_types(self):
        """Test validation across different profile types"""
        valid_profile = {str(h): 1.0 / 24.0 for h in range(1, 25)}
        invalid_profile = {str(h): 1.0 / 23.0 for h in range(2, 25)}  # Missing hour 1

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            }
                        },
                        "irrigation": {
                            "wuprofa_24hr": {
                                "working_day": invalid_profile,  # Error here
                                "holiday": valid_profile.copy(),
                            }
                        },
                    }
                }
            ]
        }

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig(**config_data)

        error_msg = str(exc_info.value)
        assert "irrigation.wuprofa_24hr.working_day" in error_msg
        assert "missing hours: [1]" in error_msg

    def test_validate_hourly_profile_hours_multi_site_error_naming(self):
        """Test proper site naming in multi-site configurations"""
        valid_profile = {str(h): 1.0 / 24.0 for h in range(1, 25)}
        invalid_profile = {str(h): 1.0 / 23.0 for h in range(1, 24)}  # Missing hour 24

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            }
                        }
                    }
                },
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": invalid_profile,  # Error in site 2
                                "holiday": valid_profile.copy(),
                            }
                        }
                    }
                },
            ]
        }

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig(**config_data)

        error_msg = str(exc_info.value)
        assert "Site 2:" in error_msg  # Multi-site should use "Site N:"
        assert "missing hours: [24]" in error_msg

    def test_validate_hourly_profile_hours_missing_profiles_graceful(self):
        """Test validator handles missing profile configurations gracefully"""
        config_data = {
            "sites": [
                {
                    # No properties specified - should handle gracefully
                }
            ]
        }

        # Should create successfully - validator is defensive
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1

    def test_validate_hourly_profile_hours_all_profile_types(self):
        """Test validation works for all supported profile types"""
        valid_profile = {str(h): 1.0 / 24.0 for h in range(1, 25)}

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            }
                        },
                        "irrigation": {
                            "wuprofa_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            },
                            "wuprofm_24hr": {
                                "working_day": valid_profile.copy(),
                                "holiday": valid_profile.copy(),
                            },
                        },
                        "anthropogenic_emissions": {
                            "heat": {
                                "ahprof_24hr": {
                                    "working_day": valid_profile.copy(),
                                    "holiday": valid_profile.copy(),
                                },
                                "popprof_24hr": {
                                    "working_day": valid_profile.copy(),
                                    "holiday": valid_profile.copy(),
                                },
                            },
                            "co2": {
                                "traffprof_24hr": {
                                    "working_day": valid_profile.copy(),
                                    "holiday": valid_profile.copy(),
                                },
                                "humactivity_24hr": {
                                    "working_day": valid_profile.copy(),
                                    "holiday": valid_profile.copy(),
                                },
                            },
                        },
                    }
                }
            ]
        }

        # Should create successfully with all profile types valid
        result = SUEWSConfig(**config_data)
        assert len(result.sites) == 1

        # Verify all profiles were created
        props = result.sites[0].properties
        assert props.snow.snowprof_24hr is not None
        assert props.irrigation.wuprofa_24hr is not None
        assert props.irrigation.wuprofm_24hr is not None
        assert props.anthropogenic_emissions.heat.ahprof_24hr is not None
        assert props.anthropogenic_emissions.heat.popprof_24hr is not None
        assert props.anthropogenic_emissions.co2.traffprof_24hr is not None
        assert props.anthropogenic_emissions.co2.humactivity_24hr is not None

    def test_validate_hourly_profile_hours_mixed_errors_reporting(self):
        """Test multiple errors are reported together with details"""
        valid_profile = {str(h): 1.0 / 24.0 for h in range(1, 25)}
        missing_hours_profile = {
            str(h): 1.0 / 22.0 for h in range(1, 23)
        }  # Missing 23, 24
        extra_hours_profile = {
            str(h): 1.0 / 26.0 for h in range(0, 26)
        }  # Hour 0, 25 invalid

        config_data = {
            "sites": [
                {
                    "properties": {
                        "snow": {
                            "snowprof_24hr": {
                                "working_day": missing_hours_profile,
                                "holiday": valid_profile.copy(),
                            }
                        },
                        "irrigation": {
                            "wuprofa_24hr": {
                                "working_day": extra_hours_profile,
                                "holiday": valid_profile.copy(),
                            }
                        },
                    }
                }
            ]
        }

        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig(**config_data)

        error_msg = str(exc_info.value)
        # Should contain both errors
        assert "snow.snowprof_24hr.working_day" in error_msg
        assert "missing hours: [23, 24]" in error_msg
        assert "irrigation.wuprofa_24hr.working_day" in error_msg
        assert "hour values outside range 1-24" in error_msg


class TestThermalLayersValidator:
    """Test cases for thermal layer validation bug fixes."""

    def test_thermal_layers_valid_config_no_false_positive(self):
        """Test that valid thermal layer config doesn't trigger false validation errors.

        Regression test for issue where valid thermal layer configurations
        were incorrectly flagged as "Missing thermal layer parameters".
        """

        # Configuration with valid thermal layers for bldgs and paved
        config = SUEWSConfig(
            name="thermal layers test",
            description="Test thermal layer validation",
            model={"control": {"start_time": "2011-01-01", "end_time": "2011-01-02"}},
            sites=[
                {
                    "properties": {
                        "lat": 51.5,
                        "lng": -0.1,
                        "alt": 10,
                        "timezone": 0,
                        "surfacearea": 10000,
                        "z": 10,
                        "z0m_in": 1.0,
                        "zdm_in": 5.0,
                        "land_cover": {
                            "bldgs": {
                                "sfr": 0.4,
                                "faibldg": 0.7,
                                "thermal_layers": {
                                    "dz": [0.2, 0.1, 0.1, 0.2, 1.6],
                                    "k": [1.2, 1.1, 1.1, 1.5, 1.6],
                                    "rho_cp": [
                                        1200000.0,
                                        1100000.0,
                                        1100000.0,
                                        1500000.0,
                                        1600000.0,
                                    ],
                                },
                            },
                            "paved": {
                                "sfr": 0.3,
                                "thermal_layers": {
                                    "dz": [0.2, 0.1, 0.1, 0.2, 1.6],
                                    "k": [1.2, 1.1, 1.1, 1.5, 1.6],
                                    "rho_cp": [
                                        1200000.0,
                                        1100000.0,
                                        1100000.0,
                                        1500000.0,
                                        1600000.0,
                                    ],
                                },
                            },
                            "water": {"sfr": 0.3},
                        },
                    }
                }
            ],
        )

        # Check that no thermal layer validation errors occurred
        validation_summary = getattr(config, "_validation_summary", {})
        issue_types = validation_summary.get("issue_types", set())

        assert "Missing thermal layer parameters" not in issue_types, (
            "Valid thermal layer configuration should not trigger missing parameter warnings"
        )

        # Verify the thermal layers are accessible and correct
        bldgs = config.sites[0].properties.land_cover.bldgs
        paved = config.sites[0].properties.land_cover.paved

        assert bldgs.thermal_layers is not None
        assert bldgs.thermal_layers.dz == [0.2, 0.1, 0.1, 0.2, 1.6]
        assert bldgs.thermal_layers.k == [1.2, 1.1, 1.1, 1.5, 1.6]
        assert bldgs.thermal_layers.rho_cp == [
            1200000.0,
            1100000.0,
            1100000.0,
            1500000.0,
            1600000.0,
        ]

        assert paved.thermal_layers is not None
        assert paved.thermal_layers.dz == [0.2, 0.1, 0.1, 0.2, 1.6]
        assert paved.thermal_layers.k == [1.2, 1.1, 1.1, 1.5, 1.6]
        assert paved.thermal_layers.rho_cp == [
            1200000.0,
            1100000.0,
            1100000.0,
            1500000.0,
            1600000.0,
        ]

    def test_thermal_layers_validation_logic_handles_both_formats(self):
        """Test that validation logic handles both RefValue and plain list formats.

        Ensures the _is_valid_layer_array logic works for both RefValue wrappers
        and plain Python lists.
        """

        config = SUEWSConfig(
            name="format test",
            description="Test both thermal layer formats",
            model={"control": {"start_time": "2011-01-01", "end_time": "2011-01-02"}},
            sites=[
                {
                    "properties": {
                        "lat": 51.5,
                        "lng": -0.1,
                        "alt": 10,
                        "timezone": 0,
                        "surfacearea": 10000,
                        "z": 10,
                        "z0m_in": 1.0,
                        "zdm_in": 5.0,
                        "land_cover": {
                            "bldgs": {
                                "sfr": 0.4,
                                "faibldg": 0.7,
                                "thermal_layers": {
                                    "dz": [
                                        0.2,
                                        0.1,
                                        0.1,
                                        0.2,
                                        1.6,
                                    ],  # Plain list format
                                    "k": [1.2, 1.1, 1.1, 1.5, 1.6],
                                    "rho_cp": [
                                        1200000.0,
                                        1100000.0,
                                        1100000.0,
                                        1500000.0,
                                        1600000.0,
                                    ],
                                },
                            },
                            "water": {"sfr": 0.6},
                        },
                    }
                }
            ],
        )

        # Should not trigger thermal layer validation errors
        validation_summary = getattr(config, "_validation_summary", {})
        issue_types = validation_summary.get("issue_types", set())

        assert "Missing thermal layer parameters" not in issue_types, (
            "Plain list format should be properly validated"
        )

    def test_thermal_layers_only_validates_when_explicitly_provided(self):
        """Test that thermal layer validation only occurs for surfaces with explicit thermal data.

        Surfaces like water, grass that don't have thermal layers explicitly set
        should not trigger thermal layer validation errors.
        """

        config = SUEWSConfig(
            name="selective validation test",
            description="Test thermal layer validation scope",
            model={"control": {"start_time": "2011-01-01", "end_time": "2011-01-02"}},
            sites=[
                {
                    "properties": {
                        "lat": 51.5,
                        "lng": -0.1,
                        "alt": 10,
                        "timezone": 0,
                        "surfacearea": 10000,
                        "z": 10,
                        "z0m_in": 1.0,
                        "zdm_in": 5.0,
                        "land_cover": {
                            "water": {
                                "sfr": 0.5
                            },  # No thermal layers - should not trigger validation
                            "grass": {
                                "sfr": 0.3
                            },  # No thermal layers - should not trigger validation
                            "dectr": {
                                "sfr": 0.2
                            },  # No thermal layers - should not trigger validation
                        },
                    }
                }
            ],
        )

        # Should not trigger thermal layer validation errors for surfaces without explicit thermal data
        validation_summary = getattr(config, "_validation_summary", {})
        issue_types = validation_summary.get("issue_types", set())

        assert "Missing thermal layer parameters" not in issue_types, (
            "Surfaces without explicit thermal layers should not trigger thermal validation"
        )
