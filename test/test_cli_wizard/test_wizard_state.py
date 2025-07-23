"""
Tests for wizard state management.
"""

import pytest
from supy.cli.wizard.utils.state import WizardSession, ConfigState


class TestWizardSession:
    """Test the WizardSession state management"""

    def test_session_initialization(self):
        """Test session initializes correctly"""
        session = WizardSession()
        assert session.current_step == 0
        assert session.configuration == {}
        assert session.history == []
        assert session.validation_errors == {}

    def test_set_value(self):
        """Test setting configuration values"""
        session = WizardSession()

        # Test simple value
        session.set_value("site.name", "TestSite")
        assert session.configuration["site"]["name"] == "TestSite"

        # Test nested value
        session.set_value("site.location.latitude", 51.5)
        assert session.configuration["site"]["location"]["latitude"] == 51.5

    def test_get_value(self):
        """Test getting configuration values"""
        session = WizardSession()

        # Set some values
        session.set_value("site.name", "TestSite")
        session.set_value("site.latitude", 51.5)

        # Test getting existing values
        assert session.get_value("site.name") == "TestSite"
        assert session.get_value("site.latitude") == 51.5

        # Test getting non-existent value with default
        assert session.get_value("site.altitude", 0) == 0

        # Test getting non-existent value without default
        assert session.get_value("site.altitude") is None

    def test_undo_redo(self):
        """Test undo/redo functionality"""
        session = WizardSession()

        # Make some changes
        session.set_value("step1", "value1")
        session.set_value("step2", "value2")
        session.set_value("step3", "value3")

        # Test undo
        assert session.undo()
        assert session.get_value("step3") is None
        assert session.get_value("step2") == "value2"

        # Test another undo
        assert session.undo()
        assert session.get_value("step2") is None
        assert session.get_value("step1") == "value1"

        # Test redo
        assert session.redo()
        assert session.get_value("step2") == "value2"

        # Test redo again
        assert session.redo()
        assert session.get_value("step3") == "value3"

        # Test no more redo
        assert not session.redo()

    def test_validation_errors(self):
        """Test validation error management"""
        session = WizardSession()

        # Add errors
        session.add_validation_error("latitude", "Invalid latitude")
        session.add_validation_error("latitude", "Out of range")
        session.add_validation_error("longitude", "Invalid longitude")

        # Check errors
        assert session.has_validation_errors()
        assert len(session.validation_errors["latitude"]) == 2
        assert len(session.validation_errors["longitude"]) == 1

        # Clear errors
        session.clear_validation_errors()
        assert not session.has_validation_errors()
        assert session.validation_errors == {}

    def test_history_management(self):
        """Test history is properly maintained"""
        session = WizardSession()

        # Initial state should have no history
        assert len(session.history) == 0

        # Make changes
        session.set_value("value1", 1)
        assert len(session.history) == 1

        session.set_value("value2", 2)
        assert len(session.history) == 2

        # Undo should not remove from history
        session.undo()
        assert len(session.history) == 1

        # New change after undo should clear redo stack
        session.set_value("value3", 3)
        assert len(session.history) == 2
        assert len(session.redo_stack) == 0
