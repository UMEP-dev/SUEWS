"""Integration tests for public API wrappers.

These tests ensure that the deprecated functional API wrappers still work correctly
and emit proper deprecation warnings.

Note: conftest.py monkeypatches public functions to private implementations for
performance. These tests explicitly import the saved deprecated versions to test
the actual public wrappers including their deprecation warnings.
"""

import warnings
from pathlib import Path

import pytest
import pandas as pd

# Import the module to access both versions
import supy as sp
from supy.suews_sim import SUEWSSimulation

# The conftest.py saves deprecated versions before monkeypatching
# We'll access those to test the actual public API wrappers


class TestPublicAPIFunctionality:
    """Test that deprecated public API functions still work correctly."""

    def test_load_sample_data_works(self):
        """Test load_sample_data returns correct data structures."""
        # Use the saved deprecated version from conftest.py
        func = sp._deprecated_load_sample_data

        # Capture deprecation warning but test functionality
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            df_state, df_forcing = func()

            # Verify deprecation warning was emitted
            assert any(
                issubclass(warning.category, DeprecationWarning) for warning in w
            ), "load_sample_data should emit DeprecationWarning"

            # Verify return types and shapes
            assert isinstance(df_state, pd.DataFrame), (
                "Should return DataFrame for state"
            )
            assert isinstance(df_forcing, pd.DataFrame), (
                "Should return DataFrame for forcing"
            )
            assert df_state.shape[0] >= 1, "Should have at least one site"
            assert df_forcing.shape[0] > 1000, "Should have substantial forcing data"

    def test_init_supy_works(self, tmp_path):
        """Test init_supy loads configuration correctly."""
        func = sp._deprecated_init_supy

        # Use sample config
        from supy._env import trv_supy_module

        config_path = trv_supy_module / "sample_data" / "sample_config.yml"

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            df_state = func(str(config_path))

            # Verify deprecation warning
            assert any(
                issubclass(warning.category, DeprecationWarning) for warning in w
            ), "init_supy should emit DeprecationWarning"

            # Verify result
            assert isinstance(df_state, pd.DataFrame)
            assert len(df_state) >= 1

    def test_run_supy_works(self):
        """Test run_supy executes simulation correctly."""
        run_func = sp._deprecated_run_supy

        # Get sample data (use monkeypatched version for speed)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_state, df_forcing = sp.load_sample_data()

        # Run simulation with small subset
        df_forcing_subset = df_forcing.iloc[:24]  # First 24 timesteps

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            df_output, df_state_final = run_func(df_forcing_subset, df_state)

            # Verify deprecation warning
            assert any(
                issubclass(warning.category, DeprecationWarning) for warning in w
            ), "run_supy should emit DeprecationWarning"

            # Verify results
            assert isinstance(df_output, pd.DataFrame)
            assert isinstance(df_state_final, pd.DataFrame)
            assert len(df_output) == 24, "Should have 24 output timesteps"
            assert ("SUEWS", "QH") in df_output.columns, "Should have SUEWS/QH column"

    def test_save_supy_works(self, tmp_path):
        """Test save_supy writes files correctly."""
        save_func = sp._deprecated_save_supy

        # Run quick simulation (use monkeypatched for speed)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_state, df_forcing = sp.load_sample_data()
            df_output, df_state_final = sp.run_supy(df_forcing.iloc[:24], df_state)

        # Save results
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            paths = save_func(df_output, df_state_final, path_dir_save=str(tmp_path))

            # Verify deprecation warning
            assert any(
                issubclass(warning.category, DeprecationWarning) for warning in w
            ), "save_supy should emit DeprecationWarning"

            # Verify files were created
            assert isinstance(paths, list)
            assert len(paths) > 0
            for path in paths:
                assert Path(path).exists(), f"Output file should exist: {path}"


class TestPublicAPIEquivalence:
    """Test that public API produces same results as OOP interface."""

    def test_functional_matches_oop(self):
        """Test functional API produces equivalent results to OOP interface."""
        # Use deprecated versions to test actual public API
        load_func = sp._deprecated_load_sample_data
        run_func = sp._deprecated_run_supy

        # Run using functional API (with warnings suppressed for comparison)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_state_func, df_forcing_func = load_func()
            df_output_func, df_state_final_func = run_func(
                df_forcing_func.iloc[:24], df_state_func
            )

        # Run using OOP API
        sim = SUEWSSimulation.from_sample_data()
        df_output_oop = sim.run(end_date=sim.forcing.index[23])
        df_state_final_oop = sim.state_final

        # Compare results - should be nearly identical
        # Use pd.testing for robust comparison allowing small numerical differences
        pd.testing.assert_frame_equal(
            df_output_func,
            df_output_oop,
            check_exact=False,
            rtol=1e-6,  # Relative tolerance for floating point
            check_names=False,
        )

        # State comparison (allowing for small numerical differences)
        pd.testing.assert_frame_equal(
            df_state_final_func,
            df_state_final_oop,
            check_exact=False,
            rtol=1e-6,
            check_names=False,
        )


class TestPublicAPIDeprecationMessages:
    """Test that deprecation messages are clear and helpful."""

    def test_deprecation_message_includes_migration_path(self):
        """Test deprecation messages include migration guidance."""
        func = sp._deprecated_load_sample_data

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            func()

            # Find the deprecation warning
            dep_warnings = [
                warning
                for warning in w
                if issubclass(warning.category, DeprecationWarning)
            ]
            assert len(dep_warnings) > 0, "Should emit deprecation warning"

            # Check message is helpful
            message = str(dep_warnings[0].message)
            assert "deprecated" in message.lower()
            assert "SUEWSSimulation" in message or "migration" in message.lower(), (
                "Should mention migration path"
            )

    @pytest.mark.parametrize(
        "func_name",
        [
            "load_sample_data",
            "init_supy",
            "run_supy",
            "save_supy",
        ],
    )
    def test_all_functions_emit_warnings(self, func_name, tmp_path):
        """Test each deprecated function emits a warning."""
        # Setup minimal data for testing (use monkeypatched for speed)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            df_state, df_forcing = sp.load_sample_data()
            df_output, df_state_final = sp.run_supy(df_forcing.iloc[:24], df_state)

        # Get the deprecated function
        func_map = {
            "load_sample_data": sp._deprecated_load_sample_data,
            "init_supy": sp._deprecated_init_supy,
            "run_supy": sp._deprecated_run_supy,
            "save_supy": sp._deprecated_save_supy,
        }
        func = func_map[func_name]

        # Test the specific function
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            if func_name == "load_sample_data":
                func()
            elif func_name == "init_supy":
                from supy._env import trv_supy_module

                config_path = trv_supy_module / "sample_data" / "sample_config.yml"
                func(str(config_path))
            elif func_name == "run_supy":
                func(df_forcing.iloc[:24], df_state)
            elif func_name == "save_supy":
                func(df_output, df_state_final, path_dir_save=str(tmp_path))

            # Verify warning was emitted
            assert any(
                issubclass(warning.category, DeprecationWarning) for warning in w
            ), f"{func_name} should emit DeprecationWarning"
