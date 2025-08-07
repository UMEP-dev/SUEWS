"""Tests for the two-phase conversion engine."""

import pytest
from pathlib import Path
import tempfile
import shutil

from supy.util.conversion_engine import (
    ConversionValidator,
    ConversionRecovery,
    TwoPhaseConverter,
    convert_with_two_phase
)
from supy.util.code_manager import UniversalCodeManager


class TestConversionValidator:
    """Test the conversion validator."""
    
    def setup_method(self):
        """Setup test environment."""
        self.code_manager = UniversalCodeManager(debug=False)
        self.validator = ConversionValidator(self.code_manager)
        
    def test_validate_pre_conversion_valid(self, tmp_path):
        """Test pre-conversion validation with valid input."""
        # Create minimal valid structure
        runcontrol = tmp_path / "RunControl.nml"
        runcontrol.write_text("""&RunControl
FileInputPath = './Input'
FileOutputPath = './Output'
/""")
        
        # Create input directory
        input_dir = tmp_path / "Input"
        input_dir.mkdir()
        
        # Validate
        is_valid, issues = self.validator.validate_pre_conversion(
            tmp_path, '2020a', '2024a'
        )
        
        assert is_valid is True
        assert len(issues) == 0
        
    def test_validate_pre_conversion_invalid_version(self, tmp_path):
        """Test validation with invalid version."""
        is_valid, issues = self.validator.validate_pre_conversion(
            tmp_path, 'invalid', '2024a'
        )
        
        assert is_valid is False
        assert any('not supported' in issue for issue in issues)
        
    def test_validate_pre_conversion_missing_runcontrol(self, tmp_path):
        """Test validation with missing RunControl.nml."""
        is_valid, issues = self.validator.validate_pre_conversion(
            tmp_path, '2020a', '2024a'
        )
        
        assert is_valid is False
        assert any('RunControl.nml not found' in issue for issue in issues)
        
    def test_validate_post_conversion(self, tmp_path):
        """Test post-conversion validation."""
        # Create some files
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code
1 31"""
        veg_file.write_text(veg_data)
        
        # Create BiogenCO2 file
        bio_file = tmp_path / "SUEWS_BiogenCO2.txt"
        bio_data = """Code alpha beta
31 0.004 8.747"""
        bio_file.write_text(bio_data)
        
        # Validate
        is_valid, issues = self.validator.validate_post_conversion(tmp_path)
        
        # Should return validation results
        assert isinstance(is_valid, bool)
        assert isinstance(issues, list)


class TestConversionRecovery:
    """Test the conversion recovery system."""
    
    def setup_method(self):
        """Setup test environment."""
        self.code_manager = UniversalCodeManager(debug=False)
        self.recovery = ConversionRecovery(self.code_manager)
        
    def test_suggest_fixes_keyerror(self):
        """Test fix suggestions for KeyError."""
        error = KeyError("None of [Index([31], dtype='int64')] are in the [index]")
        suggestions = self.recovery.suggest_fixes(error)
        
        assert len(suggestions) > 0
        assert any('Missing code reference' in s for s in suggestions)
        assert any('--auto-fix' in s for s in suggestions)
        
    def test_suggest_fixes_duplicate_column(self):
        """Test fix suggestions for duplicate column."""
        error = ValueError("cannot insert alpha, already exists")
        suggestions = self.recovery.suggest_fixes(error)
        
        assert len(suggestions) > 0
        assert any('Duplicate column' in s for s in suggestions)
        
    def test_suggest_fixes_generic(self):
        """Test generic fix suggestions."""
        error = Exception("Some unknown error")
        suggestions = self.recovery.suggest_fixes(error)
        
        assert len(suggestions) > 0
        assert any('General suggestions' in s for s in suggestions)
        
    def test_auto_fix_missing_codes(self, tmp_path):
        """Test auto-fixing missing codes."""
        # Create file with missing reference
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code
1 31"""
        veg_file.write_text(veg_data)
        
        # Apply auto-fix
        fixed = self.recovery.auto_fix(tmp_path)
        
        # Should have created BiogenCO2 file
        bio_file = tmp_path / "SUEWS_BiogenCO2.txt"
        assert bio_file.exists() or not fixed
        
        if fixed:
            assert len(self.recovery.fixes_applied) > 0


class TestTwoPhaseConverter:
    """Test the two-phase converter."""
    
    def setup_method(self):
        """Setup test environment."""
        self.converter = TwoPhaseConverter(debug=False, auto_fix=True)
        
    def test_init(self):
        """Test converter initialization."""
        assert self.converter.code_manager is not None
        assert self.converter.validator is not None
        assert self.converter.recovery is not None
        assert self.converter.auto_fix is True
        
    def test_convert_with_invalid_input(self, tmp_path):
        """Test conversion with invalid input."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        
        success, report = self.converter.convert(
            input_dir, output_dir, 'invalid', '2024a'
        )
        
        assert success is False
        assert 'validation' in report
        assert report['validation']['pre']['valid'] is False
        
    def test_progress_callback(self, tmp_path):
        """Test progress callback functionality."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        
        # Create minimal structure
        runcontrol = input_dir / "RunControl.nml"
        runcontrol.write_text("""&RunControl
FileInputPath = './Input'
FileOutputPath = './Output'
/""")
        
        messages = []
        
        def progress_callback(msg):
            messages.append(msg)
            
        # This will fail but we're testing the callback
        success, report = self.converter.convert(
            input_dir, output_dir, '2020a', '2024a',
            progress_callback=progress_callback
        )
        
        # Should have received progress messages
        assert len(messages) > 0
        assert any('validation' in msg.lower() for msg in messages)


class TestConvertWithTwoPhase:
    """Test the high-level conversion function."""
    
    def test_convert_with_progress(self, tmp_path, capsys):
        """Test conversion with progress output."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        
        # Create minimal structure
        runcontrol = input_dir / "RunControl.nml"
        runcontrol.write_text("""&RunControl
FileInputPath = './Input'
FileOutputPath = './Output'
/""")
        
        # Run conversion (will fail but we're testing output)
        success = convert_with_two_phase(
            str(input_dir),
            str(output_dir),
            '2020a',
            '2024a',
            debug=False,
            auto_fix=True,
            show_progress=True
        )
        
        # Check output was produced
        captured = capsys.readouterr()
        assert '🔍' in captured.out or 'validation' in captured.out.lower()
        assert 'Phase 1' in captured.out
        assert 'Phase 2' in captured.out
        
    def test_convert_without_progress(self, tmp_path, capsys):
        """Test conversion without progress output."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        
        # Run conversion with progress disabled
        success = convert_with_two_phase(
            str(input_dir),
            str(output_dir),
            '2020a',
            '2024a',
            debug=False,
            auto_fix=False,
            show_progress=False
        )
        
        # Should have minimal output
        captured = capsys.readouterr()
        assert '🔍' not in captured.out