"""Concise test suite for SUEWSSimulation class using sample data."""

from pathlib import Path

import pytest

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

import supy as sp
from supy.suews_sim import SUEWSSimulation


class TestInit:
    """Test initialization."""

    def test_empty_init(self):
        """Test empty initialization."""
        sim = SUEWSSimulation()
        assert sim.config is None
        assert sim.forcing is None
        assert sim.results is None

    def test_yaml_init(self):
        """Test initialization from YAML."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))
        assert sim.config is not None
        assert sim._df_state_init is not None

    def test_from_sample_data(self):
        """Test initialization from sample data factory method."""
        sim = SUEWSSimulation.from_sample_data()
        assert sim._df_state_init is not None
        assert sim._df_forcing is not None
        # Check structure without hardcoding exact column counts (fragile as model evolves)
        assert sim._df_state_init.shape[0] == 1  # One site
        assert sim._df_state_init.shape[1] > 1000  # Many parameters (sanity check)
        assert sim._df_forcing.shape[0] == 105408  # Expected timesteps
        assert sim._df_forcing.shape[1] > 20  # Expected forcing variables

    def test_from_sample_data_matches_load_sample_data(self):
        """Test that from_sample_data() produces equivalent data to load_sample_data()."""
        # Factory method approach
        sim = SUEWSSimulation.from_sample_data()

        # Functional approach
        df_state, df_forcing = sp.load_sample_data()

        # Should have same shapes
        assert sim._df_state_init.shape == df_state.shape
        assert sim._df_forcing.shape == df_forcing.shape

    def test_from_sample_data_can_run(self):
        """Test that simulation created from sample data can run successfully."""
        sim = SUEWSSimulation.from_sample_data()

        # Run with first 24 timesteps (2 hours)
        results = sim.run(end_date=sim._df_forcing.index[23])

        assert results is not None
        assert len(results) == 24
        assert sim._run_completed is True


class TestConfig:
    """Test configuration updates."""

    def test_update_config_yaml(self):
        """Test updating config from another YAML file."""
        # Create initial simulation
        sim = SUEWSSimulation()
        assert sim.config is None

        # Update with YAML config
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim.update_config(str(yaml_path))

        assert sim.config is not None
        assert sim._df_state_init is not None
        assert len(sim._df_state_init) > 0

    def test_invalid_config_path(self):
        """Test invalid config path."""
        with pytest.raises(FileNotFoundError):
            SUEWSSimulation("nonexistent.yml")

    def test_update_config_nested_dict(self):
        """Test nested dictionary update preserves Pydantic models (regression test for #756)."""
        # Initialize with YAML config
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))

        # Update with nested dictionary (the problematic case from documentation)
        sim.update_config({"model": {"control": {"tstep": 600}}})

        # Verify the update worked
        assert sim.config.model.control.tstep == 600

        # Critical: verify control is still a Pydantic model, not a dict
        assert hasattr(sim.config.model.control, "__dict__")
        assert not isinstance(sim.config.model.control, dict)

        # Verify other attributes on control are preserved
        assert hasattr(sim.config.model.control, "forcing_file")
        assert hasattr(sim.config.model.control, "diagnose")

    def test_update_config_deeply_nested_dict(self):
        """Test deeply nested updates work with arbitrary depth."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))

        # Test multi-level nested update
        sim.update_config({"model": {"control": {"tstep": 900, "diagnose": 1}}})

        assert sim.config.model.control.tstep == 900
        assert sim.config.model.control.diagnose == 1

        # Verify model.control is still a proper object
        assert hasattr(sim.config.model.control, "__dict__")

    def test_update_config_in_sites_length_1(self):
        """Test changes in the sites parameters with only one site."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))

        # Test multi-level nested update
        sim.update_config({"sites": {"name": "test"}})

        assert sim.config.sites[0].name == "test"

        # Verify sites is still a proper object
        assert hasattr(sim.config.sites[0], "__dict__")

    def test_update_config_in_sites_name(self):
        """Test changes in the sites parameters with only one site."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))

        # Test multi-level nested update
        sim.update_config({"sites": {"KCL": {"name": "test"}}})
        changed_site = next(
            (item for item in sim.config.sites if item.name == "test"), None
        )

        assert changed_site.name == "test"

        # Verify sites is still a proper object
        assert hasattr(changed_site, "__dict__")

    def test_update_config_in_sites_index(self):
        """Test changes in the sites parameters with only one site."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        sim = SUEWSSimulation(str(yaml_path))

        # Test multi-level nested update
        sim.update_config({"sites": {0: {"name": "test"}}})

        assert sim.config.sites[0].name == "test"

        # Verify sites is still a proper object
        assert hasattr(sim.config.sites[0], "__dict__")


class TestConfigSitesUpdate:
    """Test site-specific configuration updates with enhanced coverage.

    This test class provides comprehensive coverage for the three site access patterns:
    1. Single-site shorthand (no key)
    2. Site name lookup (string key)
    3. Site index access (integer key)

    Tests cover both shallow and deeply nested updates to ensure robust behaviour.
    """

    @pytest.fixture
    def sim_single_site(self):
        """Fixture providing a simulation with single site (KCL)."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        return SUEWSSimulation(str(yaml_path))

    @pytest.fixture
    def sim_multi_site(self, sim_single_site, tmp_path):
        """Fixture providing a simulation with multiple sites.

        Creates a temporary config with two sites for testing multi-site scenarios.
        """
        # Add a second site programmatically
        import copy

        site2 = copy.deepcopy(sim_single_site.config.sites[0])
        site2.name = "Swindon"
        sim_single_site.config.sites.append(site2)
        return sim_single_site

    # ===== Test 1: Single-site shorthand access =====
    def test_single_site_shorthand_shallow(self, sim_single_site):
        """Test updating top-level site attribute using single-site shorthand."""
        sim_single_site.update_config({"sites": {"name": "NewName"}})
        assert sim_single_site.config.sites[0].name == "NewName"

    def test_single_site_shorthand_deep_nested(self, sim_single_site):
        """Test updating deeply nested site attribute using single-site shorthand."""
        original_lat = sim_single_site.config.sites[0].properties.lat.value

        sim_single_site.update_config({
            "sites": {"properties": {"lat": {"value": 52.5}}}
        })

        assert sim_single_site.config.sites[0].properties.lat.value == 52.5
        assert sim_single_site.config.sites[0].properties.lat.value != original_lat
        # Verify other properties unchanged
        assert hasattr(sim_single_site.config.sites[0].properties, "lng")

    def test_single_site_shorthand_multiple_attrs(self, sim_single_site):
        """Test updating multiple attributes at once using single-site shorthand."""
        sim_single_site.update_config({"sites": {"name": "UpdatedSite", "gridiv": 2}})

        assert sim_single_site.config.sites[0].name == "UpdatedSite"
        assert sim_single_site.config.sites[0].gridiv == 2

    # ===== Test 2: Site name lookup =====
    def test_site_name_shallow(self, sim_single_site):
        """Test updating top-level site attribute by site name."""
        sim_single_site.update_config({
            "sites": {"KCL": {"name": "KingsCollegeLondon"}}
        })
        assert sim_single_site.config.sites[0].name == "KingsCollegeLondon"

    def test_site_name_deep_nested(self, sim_single_site):
        """Test updating deeply nested site attribute by site name."""
        sim_single_site.update_config({
            "sites": {
                "KCL": {
                    "properties": {"lat": {"value": 51.52}, "lng": {"value": -0.13}}
                }
            }
        })

        assert sim_single_site.config.sites[0].properties.lat.value == 51.52
        assert sim_single_site.config.sites[0].properties.lng.value == -0.13

    def test_site_name_nonexistent_multi_sites(self, sim_multi_site):
        """Test that non-matching site name is skipped gracefully when multiple sites exist."""
        original_name1 = sim_multi_site.config.sites[0].name
        original_name2 = sim_multi_site.config.sites[1].name

        # Update with non-existent site name should be skipped
        sim_multi_site.update_config({
            "sites": {"NonExistent": {"name": "ShouldNotApply"}}
        })

        # Both sites should remain unchanged
        assert sim_multi_site.config.sites[0].name == original_name1
        assert sim_multi_site.config.sites[1].name == original_name2

    def test_site_name_multi_site_selective(self, sim_multi_site):
        """Test updating only one site by name when multiple sites exist."""
        sim_multi_site.update_config({"sites": {"Swindon": {"gridiv": 5}}})

        # Only second site should be updated
        assert sim_multi_site.config.sites[0].gridiv == 1  # unchanged
        assert sim_multi_site.config.sites[1].gridiv == 5  # updated

    # ===== Test 3: Site index access =====
    def test_site_index_shallow(self, sim_single_site):
        """Test updating top-level site attribute by integer index."""
        sim_single_site.update_config({"sites": {0: {"name": "IndexedName"}}})
        assert sim_single_site.config.sites[0].name == "IndexedName"

    def test_site_index_deep_nested(self, sim_single_site):
        """Test updating deeply nested site attribute by integer index."""
        sim_single_site.update_config({
            "sites": {0: {"properties": {"alt": {"value": 25.0}}}}
        })

        assert sim_single_site.config.sites[0].properties.alt.value == 25.0

    def test_site_index_multi_site_selective(self, sim_multi_site):
        """Test updating specific site by index when multiple sites exist."""
        sim_multi_site.update_config({"sites": {1: {"name": "Site2Updated"}}})

        # Only second site should be updated
        assert sim_multi_site.config.sites[0].name == "KCL"  # unchanged
        assert sim_multi_site.config.sites[1].name == "Site2Updated"  # updated

    # ===== Test 4: Type preservation and structure integrity =====
    def test_site_update_preserves_pydantic_models(self, sim_single_site):
        """Test that site updates preserve Pydantic model structure."""
        sim_single_site.update_config({"sites": {0: {"name": "Test"}}})

        # Site should still be a proper Pydantic model, not a dict
        assert hasattr(sim_single_site.config.sites[0], "__dict__")
        assert not isinstance(sim_single_site.config.sites[0], dict)

        # Other attributes should be preserved
        assert hasattr(sim_single_site.config.sites[0], "properties")
        assert hasattr(sim_single_site.config.sites[0], "gridiv")

    def test_site_update_preserves_nested_models(self, sim_single_site):
        """Test that nested structure remains intact after partial update."""
        sim_single_site.update_config({
            "sites": {0: {"properties": {"lat": {"value": 50.0}}}}
        })

        # Properties should still be a model, not replaced by dict
        assert hasattr(sim_single_site.config.sites[0].properties, "__dict__")
        # Other nested properties should be preserved
        assert hasattr(sim_single_site.config.sites[0].properties, "lng")
        assert hasattr(sim_single_site.config.sites[0].properties, "alt")

    # ===== Test 5: Edge cases and robustness =====
    @pytest.mark.parametrize(
        "access_method,update_dict",
        [
            ("shorthand", {"sites": {"name": "A"}}),
            ("name", {"sites": {"KCL": {"name": "B"}}}),
            ("index", {"sites": {0: {"name": "C"}}}),
        ],
    )
    def test_all_access_methods_equivalent_single_site(
        self, sim_single_site, access_method, update_dict
    ):
        """Test that all three access methods work for single-site configs."""
        # Should not raise any exception
        sim_single_site.update_config(update_dict)
        assert sim_single_site.config.sites[0].name in ["A", "B", "C"]

    def test_single_site_shorthand_fails_gracefully_multi_site(self, sim_multi_site):
        """Test that single-site shorthand is safely ignored with multiple sites.

        When multiple sites exist and shorthand syntax is used (e.g., {"name": "value"}
        without site key), the update should be skipped rather than applied ambiguously.
        This prevents accidental updates to the wrong site.
        """
        # Record original names
        original_name_site0 = sim_multi_site.config.sites[0].name
        original_name_site1 = sim_multi_site.config.sites[1].name

        # Shorthand syntax with multiple sites - should be skipped
        sim_multi_site.update_config({"sites": {"name": "AmbiguousName"}})

        # ASSERTION: Both sites should remain unchanged (shorthand skipped)
        # Rationale: With multiple sites, shorthand is ambiguous and should not apply
        assert sim_multi_site.config.sites[0].name == original_name_site0, (
            "Site 0 name should not change when shorthand used with multiple sites"
        )
        assert sim_multi_site.config.sites[1].name == original_name_site1, (
            "Site 1 name should not change when shorthand used with multiple sites"
        )

    def test_nonexistent_site_name_single_site_fallback(self, sim_single_site):
        """Test fallback behaviour when site name doesn't match but only one site exists.

        This is the edge case where:
        1. User provides site name that doesn't match
        2. Only one site exists
        3. Implementation should fall back to updating that single site

        **CRITICAL BUG:** Line 148 uses `value` instead of `site_value`,
        which will fail to update because it tries to set wrong attribute names.
        """
        original_gridiv = sim_single_site.config.sites[0].gridiv

        # Provide non-existent site name with single site
        # Should fall back to updating the only available site
        sim_single_site.update_config({"sites": {"NonExistentSite": {"gridiv": 99}}})

        # The fallback should work correctly
        # If line 148 bug exists, gridiv will still be original value
        assert sim_single_site.config.sites[0].gridiv == 99, (
            "Bug detected: Single-site fallback not working (line 148 uses 'value' instead of 'site_value')"
        )


class TestForcing:
    """Test forcing data loading."""

    def test_dataframe_forcing(self):
        """Test loading forcing from DataFrame."""
        _, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim.update_forcing(df_forcing.iloc[:24])  # 2 hours only
        assert len(sim.forcing) == 24

    def test_invalid_forcing_path(self):
        """Test invalid forcing path."""
        sim = SUEWSSimulation()
        with pytest.raises(FileNotFoundError):
            sim.update_forcing("nonexistent.txt")


class TestRun:
    """Test simulation execution."""

    def test_basic_run(self):
        """Test basic simulation run."""
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])  # 2 hours

        results = sim.run()
        assert results is not None
        assert len(results) > 0
        assert "QH" in results.columns.get_level_values("var")

    def test_run_without_forcing(self):
        """Test run fails without forcing."""
        sim = SUEWSSimulation()
        sim._df_state_init, _ = sp.load_SampleData()

        with pytest.raises(RuntimeError, match="No forcing"):
            sim.run()


class TestSave:
    """Test result saving."""

    def test_save_default(self, tmp_path):
        """Test saving results."""
        # Quick run
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])
        sim.run()

        # Save
        paths = sim.save(tmp_path)
        assert isinstance(paths, list)
        assert len(paths) > 0
        assert any(Path(p).exists() for p in paths)

    def test_save_without_results(self):
        """Test save fails without results."""
        sim = SUEWSSimulation()
        with pytest.raises(RuntimeError, match="No simulation results"):
            sim.save()


class TestReset:
    """Test reset functionality."""

    def test_reset_clears_results(self):
        """Test reset clears results."""
        # Run simulation
        df_state, df_forcing = sp.load_SampleData()
        sim = SUEWSSimulation()
        sim._df_state_init = df_state
        sim.update_forcing(df_forcing.iloc[:24])
        sim.run()

        # Reset
        sim.reset()
        assert sim._df_output is None
        assert sim._run_completed is False

        # Can run again
        results = sim.run()
        assert results is not None


class TestIntegration:
    """Test complete workflows."""

    def test_yaml_workflow(self, tmp_path):
        """Test YAML config → run → save workflow."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")

        # Load from YAML
        sim = SUEWSSimulation(str(yaml_path))

        # Override with short forcing
        _, df_forcing = sp.load_SampleData()
        sim.update_forcing(df_forcing.iloc[:48])  # 4 hours

        # Run and save
        results = sim.run()
        paths = sim.save(tmp_path)

        assert len(results) > 0
        assert len(paths) > 0


class TestEnhancements:
    """Test new enhancements to SUEWSSimulation class."""

    def test_repr_not_configured(self):
        """Test repr for unconfigured simulation."""
        sim = SUEWSSimulation()
        repr_str = repr(sim)
        assert "Not configured" in repr_str
        assert "SUEWSSimulation" in repr_str

    def test_repr_ready(self):
        """Test repr for configured simulation."""
        sim = SUEWSSimulation.from_sample_data()
        repr_str = repr(sim)
        assert "Ready" in repr_str
        assert "site" in repr_str
        assert "timesteps" in repr_str

    def test_repr_complete(self):
        """Test repr after running."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])  # Run only 24 timesteps
        repr_str = repr(sim)
        assert "Complete" in repr_str
        assert "results" in repr_str

    def test_state_properties(self):
        """Test state property access."""
        sim = SUEWSSimulation.from_sample_data()
        assert sim.state_init is not None
        assert sim.state_final is None

        sim.run(end_date=sim.forcing.index[23])
        assert sim.state_final is not None

    def test_validation_methods(self):
        """Test is_ready and is_complete."""
        sim = SUEWSSimulation()
        assert not sim.is_ready()
        assert not sim.is_complete()

        sim = SUEWSSimulation.from_sample_data()
        assert sim.is_ready()
        assert not sim.is_complete()

        sim.run(end_date=sim.forcing.index[23])
        assert sim.is_complete()


class TestMethodChaining:
    """Test method chaining support."""

    def test_update_config_returns_self(self):
        """Test update_config enables chaining."""
        sim = SUEWSSimulation()
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        result = sim.update_config(str(yaml_path))
        assert result is sim

    def test_update_forcing_returns_self(self):
        """Test update_forcing enables chaining."""
        sim = SUEWSSimulation()
        _, df_forcing = sp.load_sample_data()
        result = sim.update_forcing(df_forcing.iloc[:24])
        assert result is sim

    def test_reset_returns_self(self):
        """Test reset enables chaining."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])
        result = sim.reset()
        assert result is sim
        assert not sim.is_complete()

    def test_fluent_interface(self):
        """Test complete fluent workflow."""
        yaml_path = files("supy").joinpath("sample_data/sample_config.yml")
        _, df_forcing = sp.load_sample_data()

        sim = (
            SUEWSSimulation()
            .update_config(str(yaml_path))
            .update_forcing(df_forcing.iloc[:24])
        )

        assert sim.is_ready()

        # Now run
        sim.run()
        assert sim.is_complete()


class TestGetVariable:
    """Test variable extraction helper method."""

    def test_get_variable_with_group(self):
        """Test variable extraction with group specification."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])

        # QH appears in multiple groups, must specify which one
        qh = sim.get_variable("QH", group="SUEWS")
        assert qh is not None
        assert len(qh) == 24

    def test_get_variable_ambiguous_raises(self):
        """Test get_variable raises error for ambiguous variable without group."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])

        # QH appears in multiple groups - should raise error
        with pytest.raises(ValueError, match="appears in multiple groups"):
            sim.get_variable("QH")

    def test_get_variable_not_run(self):
        """Test get_variable fails gracefully if not run."""
        sim = SUEWSSimulation.from_sample_data()
        with pytest.raises(RuntimeError, match="No results available"):
            sim.get_variable("QH")

    def test_get_variable_invalid_name(self):
        """Test get_variable with invalid variable name."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])

        with pytest.raises(ValueError, match="not found"):
            sim.get_variable("INVALID_VAR")

    def test_get_variable_wrong_group(self):
        """Test get_variable with wrong group specification."""
        sim = SUEWSSimulation.from_sample_data()
        sim.run(end_date=sim.forcing.index[23])

        # QH exists but not in a non-existent group
        with pytest.raises(ValueError, match="not found in group"):
            sim.get_variable("QH", group="NONEXISTENT_GROUP")


class TestPathResolution:
    """Test forcing file path resolution."""

    def test_resolve_absolute_path_unchanged(self, tmp_path):
        """Test that absolute paths are returned unchanged."""
        # Create config file
        config_file = tmp_path / "config.yml"
        config_file.write_text("sites:\n  - name: test\n")

        sim = SUEWSSimulation()
        sim._config_path = config_file

        # Absolute path should be returned as-is
        abs_path = "/absolute/path/to/forcing.txt"
        result = sim._resolve_single_path(abs_path)
        assert result == abs_path

    def test_resolve_relative_path_to_config(self, tmp_path):
        """Test that relative paths are resolved relative to config file."""
        # Create config file and forcing file
        config_file = tmp_path / "config.yml"
        config_file.write_text("sites:\n  - name: test\n")
        forcing_file = tmp_path / "forcing.txt"
        forcing_file.write_text("# forcing data")

        sim = SUEWSSimulation()
        sim._config_path = config_file

        # Relative path should be resolved relative to config directory
        result = sim._resolve_single_path("forcing.txt")
        assert result == str(tmp_path / "forcing.txt")

    def test_resolve_relative_path_list(self, tmp_path):
        """Test that list of relative paths are all resolved."""
        config_file = tmp_path / "config.yml"
        config_file.write_text("sites:\n  - name: test\n")

        sim = SUEWSSimulation()
        sim._config_path = config_file

        paths = ["forcing1.txt", "forcing2.txt"]
        result = sim._resolve_forcing_paths(paths)

        assert isinstance(result, list)
        assert len(result) == 2
        assert result[0] == str(tmp_path / "forcing1.txt")
        assert result[1] == str(tmp_path / "forcing2.txt")

    def test_resolve_mixed_absolute_relative_list(self, tmp_path):
        """Test list with mixed absolute and relative paths."""
        config_file = tmp_path / "config.yml"
        config_file.write_text("sites:\n  - name: test\n")

        sim = SUEWSSimulation()
        sim._config_path = config_file

        paths = ["/absolute/path/forcing.txt", "relative_forcing.txt"]
        result = sim._resolve_forcing_paths(paths)

        assert result[0] == "/absolute/path/forcing.txt"  # unchanged
        assert result[1] == str(tmp_path / "relative_forcing.txt")  # resolved

    def test_resolve_path_with_parent_references(self, tmp_path):
        """Test that paths with '..' work correctly (intentionally allowed)."""
        # Create nested directory structure
        config_dir = tmp_path / "configs"
        config_dir.mkdir()
        forcing_dir = tmp_path / "forcing"
        forcing_dir.mkdir()

        config_file = config_dir / "config.yml"
        config_file.write_text("sites:\n  - name: test\n")

        sim = SUEWSSimulation()
        sim._config_path = config_file

        # Path using '..' to go up and into sibling directory
        result = sim._resolve_single_path("../forcing/data.txt")

        # Should resolve correctly (not blocked by security check)
        expected = (forcing_dir / "data.txt").resolve()
        assert Path(result).resolve() == expected
