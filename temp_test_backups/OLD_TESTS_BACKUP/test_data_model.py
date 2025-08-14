import unittest
import pandas as pd
import supy as sp
from pathlib import Path
import numpy as np
from supy.data_model import (
    SUEWSConfig,
    Model,
    Site,
    SiteProperties,
    InitialStates,
    RefValue,
)


class TestSUEWSConfig(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures."""
        self.path_sample_config = (
            Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
        )
        self.config = SUEWSConfig.from_yaml(self.path_sample_config)

    def test_config_conversion_cycle(self):
        """Test if SUEWS configuration can be correctly converted between YAML and DataFrame formats."""
        print("\n========================================")
        print("Testing YAML-DataFrame-YAML conversion cycle for SUEWS configuration...")

        # Convert to DataFrame
        df_state = self.config.to_df_state()

        # Convert back to config object
        config_reconst = SUEWSConfig.from_df_state(df_state)

        # Compare the two configs
        # We'll compare a few key attributes as a basic test
        self.assertEqual(self.config.name, config_reconst.name)
        self.assertEqual(self.config.description, config_reconst.description)
        self.assertEqual(
            self.config.model.control.tstep, config_reconst.model.control.tstep
        )
        self.assertEqual(
            self.config.model.physics.netradiationmethod.value,
            config_reconst.model.physics.netradiationmethod.value,
        )
        self.assertEqual(
            self.config.sites[0].properties.lat.value,
            config_reconst.sites[0].properties.lat.value,
        )

        # Test if DataFrame conversion preserves structure
        df_state_2 = config_reconst.to_df_state()

        pd.testing.assert_frame_equal(df_state, df_state_2, check_dtype=False)

    def test_df_state_conversion_cycle(self):
        """Test conversion cycle starting from a DataFrame state."""
        print("\n========================================")
        print(
            "Testing DataFrame-YAML-DataFrame conversion cycle for SUEWS configuration..."
        )
        # TODO: Fix loopholes for bad sample data

        # Load initial DataFrame state
        df_state_init = sp.load_sample_data()[0]
        df_state_init2 = df_state_init.copy()

        # Fix sample data to pass validation
        for i in range(1, 7):
            if df_state_init2[("soilstore_surf", f"({i},)")].values[0] < 10:
                df_state_init2[("soilstore_surf", f"({i},)")] = 10

        # Create config object from DataFrame
        config_from_df = SUEWSConfig.from_df_state(df_state_init2)

        # Convert back to DataFrame
        df_state_reconst = config_from_df.to_df_state()

        # Reset ohm_coef 7th surface parameter as not used or changed
        for x in range(4):
            for y in range(3):
                df_state_reconst[("ohm_coef", f"(7, {x}, {y})")] = df_state_init[
                    ("ohm_coef", f"(7, {x}, {y})")
                ]

        df_state_reconst[("ohm_threshsw", f"(7,)")] = df_state_init[
            ("ohm_threshsw", f"(7,)")
        ]
        df_state_reconst[("ohm_threshwd", f"(7,)")] = df_state_init[
            ("ohm_threshwd", f"(7,)")
        ]

        for i in range(1, 7):
            if df_state_init[("soilstore_surf", f"({i},)")].values[0] < 10:
                df_state_reconst[("soilstore_surf", f"({i},)")] = 0

        # Compare the initial and reconstructed DataFrame states
        pd.testing.assert_frame_equal(
            df_state_init, df_state_reconst, check_dtype=False, check_like=True
        )

    def test_model_physics_validation(self):
        """Test model physics validation rules."""
        # Test storageheatmethod and ohmincqf validation in SUEWSConfig
        with self.assertRaises(ValueError):
            SUEWSConfig(
                sites=[{}],
                model={
                    "physics": {
                        "storageheatmethod": {"value": 1},
                        "ohmincqf": {"value": 1},
                    }
                },
            )

        with self.assertRaises(ValueError):
            SUEWSConfig(sites=[{}], model={"physics": {"snowuse": {"value": 1}}})

    def test_site_properties(self):
        """Test site properties data model."""
        # Test latitude bounds
        with self.assertRaises(ValueError):
            properties = SiteProperties(lat=RefValue(91.0))  # Invalid latitude

        with self.assertRaises(ValueError):
            properties = SiteProperties(lat=RefValue(-91.0))  # Invalid latitude

        # Test valid latitude
        properties = SiteProperties(lat=RefValue(51.5))  # London's latitude
        self.assertEqual(properties.lat.value, 51.5)

    def test_initial_states(self):
        """Test initial states data model."""
        states = InitialStates()

        # Test snow albedo bounds
        with self.assertRaises(ValueError):
            states.snowalb = RefValue(1.5)  # Invalid albedo > 1
            InitialStates.model_validate(states.model_dump())

        with self.assertRaises(ValueError):
            states.snowalb = RefValue(-0.1)  # Invalid albedo < 0
            InitialStates.model_validate(states.model_dump())

        # Test valid snow albedo
        states.snowalb = RefValue(0.8)
        validated_states = InitialStates.model_validate(states.model_dump())
        self.assertEqual(validated_states.snowalb.value, 0.8)

    def test_multi_site_config(self):
        """Test configuration with multiple sites."""
        config = SUEWSConfig(
            name="Multi-site test",
            description="Test configuration with multiple sites",
            sites=[
                Site(gridiv=0),
                Site(gridiv=1),
                Site(gridiv=2),
            ],
        )

        # Convert to DataFrame
        df_state = config.to_df_state()

        # Check if all sites are present
        self.assertEqual(len(df_state.index), 3)
        self.assertTrue(all(idx in df_state.index for idx in [0, 1, 2]))

        # Convert back and check if sites are preserved
        config_reconst = SUEWSConfig.from_df_state(df_state)
        self.assertEqual(len(config_reconst.sites), 3)
        self.assertEqual([site.gridiv for site in config_reconst.sites], [0, 1, 2])

    def test_wrap_numeric(self):
        """Test that wrap creates a RefValue from a plain numeric."""
        rv = RefValue.wrap(5.0)
        self.assertIsInstance(rv, RefValue)
        self.assertEqual(rv.value, 5.0)

    def test_wrap_existing_refvalue(self):
        """Test that wrap leaves an existing RefValue unchanged."""
        original = RefValue(10.0)
        wrapped = RefValue.wrap(original)
        self.assertIs(wrapped, original)

    def test_equality_between_refvalues(self):
        """Test equality and inequality between RefValues."""
        a = RefValue(5.0)
        b = RefValue(5.0)
        c = RefValue(10.0)
        self.assertEqual(a, b)
        self.assertNotEqual(a, c)

    def test_equality_with_plain_value(self):
        """Test equality comparison between RefValue and plain value."""
        a = RefValue(5.0)
        self.assertEqual(a, 5.0)
        self.assertNotEqual(a, 10.0)

    def test_comparison_with_none_returns_true(self):
        """Test that comparison with None always returns True."""
        a = RefValue(None)
        b = RefValue(5.0)

        # Less than
        self.assertTrue(a < b)
        self.assertTrue(b < a)

        # Less than or equal
        self.assertTrue(a <= b)
        self.assertTrue(b <= a)

        # Greater than
        self.assertTrue(a > b)
        self.assertTrue(b > a)

        # Greater than or equal
        self.assertTrue(a >= b)
        self.assertTrue(b >= a)


if __name__ == "__main__":
    unittest.main()
