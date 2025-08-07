"""Tests for the profile manager module."""

import pytest
from pathlib import Path
import tempfile
import pandas as pd
import numpy as np

from supy.util.profile_manager import ProfileManager


class TestProfileManager:
    """Test suite for ProfileManager class."""

    def test_init_without_profiles(self):
        """Test initializing ProfileManager without existing profiles."""
        manager = ProfileManager()
        assert len(manager.available_profiles) == 0
        assert manager.profile_data is None
        assert len(manager.missing_profiles) == 0

    def test_init_with_profiles(self, tmp_path):
        """Test initializing ProfileManager with existing profiles."""
        # Create a sample profiles file
        profiles_path = tmp_path / "SUEWS_Profiles.txt"
        with open(profiles_path, "w") as f:
            f.write(
                "Code    Hr00    Hr01    Hr02    Hr03    Hr04    Hr05    Hr06    Hr07    Hr08    Hr09    Hr10    Hr11    Hr12    Hr13    Hr14    Hr15    Hr16    Hr17    Hr18    Hr19    Hr20    Hr21    Hr22    Hr23\n"
            )
            f.write(
                "1    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0\n"
            )
            f.write(
                "999    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5    0.5\n"
            )

        manager = ProfileManager(profiles_path)
        assert len(manager.available_profiles) == 2
        assert 1 in manager.available_profiles
        assert 999 in manager.available_profiles

    def test_validate_profile_code_exists(self, tmp_path):
        """Test validation of existing profile codes."""
        profiles_path = tmp_path / "SUEWS_Profiles.txt"
        with open(profiles_path, "w") as f:
            f.write(
                "Code    Hr00    Hr01    Hr02    Hr03    Hr04    Hr05    Hr06    Hr07    Hr08    Hr09    Hr10    Hr11    Hr12    Hr13    Hr14    Hr15    Hr16    Hr17    Hr18    Hr19    Hr20    Hr21    Hr22    Hr23\n"
            )
            f.write(
                "100    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0\n"
            )

        manager = ProfileManager(profiles_path)
        is_valid, replacement = manager.validate_profile_code(100, "TestField")
        assert is_valid is True
        assert replacement == 100

    def test_validate_profile_code_missing(self):
        """Test validation of missing profile codes."""
        manager = ProfileManager()
        is_valid, replacement = manager.validate_profile_code(800, "TestField")
        assert is_valid is False
        assert replacement == 999  # Default profile
        assert 800 in manager.missing_profiles

    def test_validate_special_codes(self):
        """Test validation of special codes (-999, -9)."""
        manager = ProfileManager()

        # Test -999
        is_valid, replacement = manager.validate_profile_code(-999, "TestField")
        assert is_valid is True
        assert replacement == -999

        # Test -9
        is_valid, replacement = manager.validate_profile_code(-9, "TestField")
        assert is_valid is True
        assert replacement == -9

    def test_create_default_profile(self):
        """Test creation of default uniform profile."""
        manager = ProfileManager()
        profile = manager.create_default_profile(100)

        assert len(profile) == 24
        assert all(v == 1.0 for v in profile)
        assert profile.name == 100

    def test_ensure_required_profiles(self, tmp_path):
        """Test ensuring required profiles exist."""
        manager = ProfileManager()
        # Add some missing profiles
        manager.missing_profiles = {100, 200, 300}

        manager.ensure_required_profiles(tmp_path)

        # Check file was created
        profiles_file = tmp_path / "SUEWS_Profiles.txt"
        assert profiles_file.exists()

        # Check all default and missing profiles were created
        assert 999 in manager.available_profiles  # Default profile
        assert 100 in manager.available_profiles
        assert 200 in manager.available_profiles
        assert 300 in manager.available_profiles

    def test_process_conversion_rules(self):
        """Test processing conversion rules with profile references."""
        manager = ProfileManager()

        # Create sample rules DataFrame
        rules_data = {
            "From": ["2016a", "2016a", "2017a", "2017a"],
            "To": ["2017a", "2017a", "2018a", "2018a"],
            "Action": ["Add", "Add", "Add", "Delete"],
            "File": [
                "SUEWS_SiteSelect.txt",
                "SUEWS_SiteSelect.txt",
                "SUEWS_AnthropogenicHeat.txt",
                "SUEWS_SiteSelect.txt",
            ],
            "Variable": [
                "ActivityProfWD",
                "ActivityProfWE",
                "EnergyUseProfWD",
                "SomeOtherField",
            ],
            "Column": [54, 55, 19, 10],
            "Value": ["800", "801", "44", "-999"],
        }
        rules_df = pd.DataFrame(rules_data)

        # Process rules
        updated_rules = manager.process_conversion_rules(rules_df)

        # Check that invalid profile codes were replaced
        assert updated_rules.loc[0, "Value"] == "999"  # 800 -> 999
        assert updated_rules.loc[1, "Value"] == "999"  # 801 -> 999
        assert updated_rules.loc[2, "Value"] == "999"  # 44 -> 999
        assert updated_rules.loc[3, "Value"] == "-999"  # Unchanged (Delete action)

        # Check missing profiles were tracked
        assert 800 in manager.missing_profiles
        assert 801 in manager.missing_profiles
        assert 44 in manager.missing_profiles

    def test_get_profile_summary(self):
        """Test getting profile summary."""
        manager = ProfileManager()
        manager.available_profiles = {1, 999}
        manager.missing_profiles = {100, 200}

        summary = manager.get_profile_summary()

        assert summary["available_profiles"] == 2
        assert summary["missing_profiles"] == 2
        assert summary["missing_codes"] == [100, 200]
        assert "DEFAULT" in summary["default_profiles"]

    def test_profile_file_format(self, tmp_path):
        """Test that created profile files have correct format."""
        manager = ProfileManager()
        manager.missing_profiles = {100}

        manager.ensure_required_profiles(tmp_path)

        profiles_file = tmp_path / "SUEWS_Profiles.txt"
        with open(profiles_file, "r") as f:
            lines = f.readlines()

        # Check header
        assert lines[0].startswith("Code")
        assert "Hr00" in lines[0]
        assert "Hr23" in lines[0]

        # Check data lines
        for line in lines[1:]:
            parts = line.split()
            assert len(parts) == 25  # Code + 24 hours
            # First part should be a valid integer (profile code)
            assert parts[0].isdigit()
            # Rest should be valid floats
            for part in parts[1:]:
                float(part)  # Should not raise ValueError


class TestProfileValidation:
    """Test profile validation integration."""

    def test_validate_and_fix_profiles(self, tmp_path):
        """Test the main validation function."""
        from supy.util.profile_manager import validate_and_fix_profiles

        # Create a simple rules file with missing profile references
        rules_path = tmp_path / "rules.csv"
        rules_data = {
            "From": ["2016a", "2016a"],
            "To": ["2017a", "2017a"],
            "Action": ["Add", "Add"],
            "File": ["SUEWS_SiteSelect.txt", "SUEWS_SiteSelect.txt"],
            "Variable": ["ActivityProfWD", "ActivityProfWE"],
            "Column": [54, 55],
            "Value": ["800", "801"],
        }
        pd.DataFrame(rules_data).to_csv(rules_path, index=False)

        # Run validation
        manager = validate_and_fix_profiles(tmp_path, rules_path, debug=False)

        # Check that profiles were created
        profiles_file = tmp_path / "SUEWS_Profiles.txt"
        assert profiles_file.exists()

        # Check that missing profiles were identified
        assert len(manager.missing_profiles) > 0

        # Check that rules were backed up and updated
        backup_file = rules_path.with_suffix(".csv.bak")
        assert backup_file.exists()

        # Check updated rules have default values
        updated_rules = pd.read_csv(rules_path)
        assert all(v == "999" for v in updated_rules["Value"])
