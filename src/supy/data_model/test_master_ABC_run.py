"""
Test suite for master_ABC_run.py

This test suite validates the master workflow script for SUEWS configuration processing,
covering all three execution modes:
- Phase A only: Parameter detection and YAML structure updates  
- Phase B only: Scientific validation and automatic adjustments
- Phase A→B: Complete workflow with consolidated output

Test scenarios include:
- File naming conventions for each mode
- Error handling and validation
- Workflow integration between phases
- Output file generation and cleanup
- Critical error detection and reporting
"""

import os
import tempfile
import shutil
import sys
from pathlib import Path
import yaml
from unittest.mock import patch, MagicMock
import pytest

# Add current directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from master_ABC_run import (
    validate_input_file, 
    setup_output_paths, 
    run_phase_a, 
    run_phase_b,
    main
)


@pytest.fixture
def test_env():
    """Set up test environment with temporary directory and test files."""
    test_dir = tempfile.mkdtemp()
    test_yaml_file = os.path.join(test_dir, "test_config.yml")
    standard_yaml_file = os.path.join(test_dir, "standard_config.yml")
    
    # Create minimal test YAML configurations
    create_test_yaml_files(test_dir, test_yaml_file, standard_yaml_file)
    
    yield {
        'test_dir': test_dir,
        'test_yaml_file': test_yaml_file,
        'standard_yaml_file': standard_yaml_file
    }
    
    # Cleanup
    shutil.rmtree(test_dir)


def create_test_yaml_files(test_dir, test_yaml_file, standard_yaml_file):
    """Create test YAML files for testing."""
    
    # Basic user YAML with missing physics parameters (will trigger Phase A)
    user_config = {
        'name': 'test config',
        'model': {
            'control': {
                'tstep': 300,
                'start_time': '2011-01-01',
                'end_time': '2011-01-02'
            },
            'physics': {
                'emissionsmethod': {'value': 2},
                'storageheatmethod': {'value': 1},
                # Missing netradiationmethod - will be detected by Phase A
            }
        },
        'sites': [{
            'name': 'test_site',
            'gridiv': 1,
            'properties': {
                'lat': {'value': 51.5},
                'lng': {'value': -0.1},
                'alt': {'value': 10.0},
                'timezone': {'value': 0},
                'land_cover': {
                    'paved': {'sfr': {'value': 0.4}},
                    'bldgs': {'sfr': {'value': 0.3}},
                    'grass': {'sfr': {'value': 0.3}},
                    'dectr': {'sfr': {'value': 0.0}},
                    'evetr': {'sfr': {'value': 0.0}},
                    'bsoil': {'sfr': {'value': 0.0}},
                    'water': {'sfr': {'value': 0.0}}
                }
            },
            'initial_states': {}
        }]
    }
    
    # Standard YAML with complete physics parameters
    standard_config = {
        'name': 'standard config',
        'model': {
            'control': {
                'tstep': 300,
                'start_time': '2011-01-01',
                'end_time': '2011-01-02'
            },
            'physics': {
                'netradiationmethod': {'value': 3},
                'emissionsmethod': {'value': 2},
                'storageheatmethod': {'value': 1},
                'stabilitymethod': {'value': 3},
                'rslmethod': {'value': 2}
            }
        },
        'sites': [{
            'name': 'standard_site',
            'gridiv': 1,
            'properties': {
                'lat': {'value': 51.5},
                'lng': {'value': -0.1},
                'alt': {'value': 10.0},
                'timezone': {'value': 0},
                'land_cover': {
                    'paved': {'sfr': {'value': 0.4}},
                    'bldgs': {'sfr': {'value': 0.3}},
                    'grass': {'sfr': {'value': 0.3}},
                    'dectr': {'sfr': {'value': 0.0}},
                    'evetr': {'sfr': {'value': 0.0}},
                    'bsoil': {'sfr': {'value': 0.0}},
                    'water': {'sfr': {'value': 0.0}}
                }
            },
            'initial_states': {}
        }]
    }
    
    # Write test files
    with open(test_yaml_file, 'w') as f:
        yaml.dump(user_config, f, default_flow_style=False)
        
    with open(standard_yaml_file, 'w') as f:
        yaml.dump(standard_config, f, default_flow_style=False)


# Input validation tests
def test_validate_existing_yaml_file(test_env):
    """Test validation of existing YAML file."""
    result = validate_input_file(test_env['test_yaml_file'])
    assert result == os.path.abspath(test_env['test_yaml_file'])


def test_validate_nonexistent_file(test_env):
    """Test validation fails for nonexistent file."""
    nonexistent_file = os.path.join(test_env['test_dir'], "nonexistent.yml")
    with pytest.raises(FileNotFoundError):
        validate_input_file(nonexistent_file)


def test_validate_non_yaml_file(test_env):
    """Test validation fails for non-YAML file."""
    txt_file = os.path.join(test_env['test_dir'], "test.txt")
    with open(txt_file, 'w') as f:
        f.write("not a yaml file")
    
    with pytest.raises(ValueError):
        validate_input_file(txt_file)


def test_validate_unreadable_file(test_env):
    """Test validation fails for unreadable file."""
    # Create file and make it unreadable (Unix only)
    if os.name != 'nt':  # Skip on Windows
        unreadable_file = os.path.join(test_env['test_dir'], "unreadable.yml")
        with open(unreadable_file, 'w') as f:
            f.write("test: content")
        os.chmod(unreadable_file, 0o000)
        
        try:
            with pytest.raises(PermissionError):
                validate_input_file(unreadable_file)
        finally:
            os.chmod(unreadable_file, 0o644)  # Restore permissions for cleanup


# Output path tests
def test_setup_output_paths_phase_a(test_env):
    """Test output path generation for Phase A only."""
    uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
        test_env['test_yaml_file'], 'A'
    )
    
    basename = os.path.basename(test_env['test_yaml_file'])
    name_without_ext = os.path.splitext(basename)[0]
    
    assert uptodate_file == os.path.join(test_env['test_dir'], f"updatedA_{basename}")
    assert report_file == os.path.join(test_env['test_dir'], f"reportA_{name_without_ext}.txt")
    assert science_yaml_file == os.path.join(test_env['test_dir'], f"updatedB_{basename}")
    assert science_report_file == os.path.join(test_env['test_dir'], f"reportB_{name_without_ext}.txt")
    assert dirname == test_env['test_dir']


def test_setup_output_paths_phase_b(test_env):
    """Test output path generation for Phase B only."""
    uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
        test_env['test_yaml_file'], 'B'
    )
    
    basename = os.path.basename(test_env['test_yaml_file'])
    name_without_ext = os.path.splitext(basename)[0]
    
    assert uptodate_file == os.path.join(test_env['test_dir'], f"updatedB_{basename}")
    assert report_file == os.path.join(test_env['test_dir'], f"reportB_{name_without_ext}.txt")
    assert science_yaml_file == os.path.join(test_env['test_dir'], f"updatedB_{basename}")
    assert science_report_file == os.path.join(test_env['test_dir'], f"reportB_{name_without_ext}.txt")


def test_setup_output_paths_phase_ab(test_env):
    """Test output path generation for complete A→B workflow."""
    uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
        test_env['test_yaml_file'], 'AB'
    )
    
    basename = os.path.basename(test_env['test_yaml_file'])
    name_without_ext = os.path.splitext(basename)[0]
    
    # AB mode should use AB naming for final outputs
    assert uptodate_file == os.path.join(test_env['test_dir'], f"updatedA_{basename}")  # Intermediate
    assert report_file == os.path.join(test_env['test_dir'], f"reportA_{name_without_ext}.txt")  # Intermediate
    assert science_yaml_file == os.path.join(test_env['test_dir'], f"updatedAB_{basename}")  # Final
    assert science_report_file == os.path.join(test_env['test_dir'], f"reportAB_{name_without_ext}.txt")  # Final


# Phase A execution tests
@patch('master_ABC_run.annotate_missing_parameters')
def test_run_phase_a_success(mock_annotate, test_env):
    """Test successful Phase A execution."""
    # Setup mock to create expected output files
    def create_mock_files(*args, **kwargs):
        uptodate_file = kwargs.get('uptodate_file') or args[2]
        report_file = kwargs.get('report_file') or args[3]
        
        # Create mock Phase A output
        with open(uptodate_file, 'w') as f:
            f.write("# UP TO DATE YAML\ntest: content")
        
        # Create mock report without critical errors
        with open(report_file, 'w') as f:
            f.write("# Report\n## NO ACTION NEEDED\n- All parameters found")
    
    mock_annotate.side_effect = create_mock_files
    
    uptodate_file = os.path.join(test_env['test_dir'], "updatedA_test.yml")
    report_file = os.path.join(test_env['test_dir'], "reportA_test.txt")
    
    result = run_phase_a(test_env['test_yaml_file'], test_env['standard_yaml_file'], uptodate_file, report_file)
    
    assert result is True
    assert os.path.exists(uptodate_file)
    assert os.path.exists(report_file)
    mock_annotate.assert_called_once()


@patch('master_ABC_run.annotate_missing_parameters')
def test_run_phase_a_critical_error(mock_annotate, test_env):
    """Test Phase A execution with critical errors."""
    def create_mock_files_with_critical_error(*args, **kwargs):
        uptodate_file = kwargs.get('uptodate_file') or args[2]
        report_file = kwargs.get('report_file') or args[3]
        
        with open(uptodate_file, 'w') as f:
            f.write("# UP TO DATE YAML\ntest: content")
        
        # Create report with critical error
        with open(report_file, 'w') as f:
            f.write("# Report\n## ACTION NEEDED\n- Found (1) critical missing parameter(s):")
    
    mock_annotate.side_effect = create_mock_files_with_critical_error
    
    uptodate_file = os.path.join(test_env['test_dir'], "updatedA_test.yml")
    report_file = os.path.join(test_env['test_dir'], "reportA_test.txt")
    
    result = run_phase_a(test_env['test_yaml_file'], test_env['standard_yaml_file'], uptodate_file, report_file)
    
    assert result is False


@patch('master_ABC_run.annotate_missing_parameters')
def test_run_phase_a_exception(mock_annotate, test_env):
    """Test Phase A execution with exception."""
    mock_annotate.side_effect = Exception("Test exception")
    
    uptodate_file = os.path.join(test_env['test_dir'], "updatedA_test.yml")
    report_file = os.path.join(test_env['test_dir'], "reportA_test.txt")
    
    result = run_phase_a(test_env['test_yaml_file'], test_env['standard_yaml_file'], uptodate_file, report_file)
    
    assert result is False


# Phase B execution tests
@patch('master_ABC_run.run_science_check')
def test_run_phase_b_success(mock_science_check, test_env):
    """Test successful Phase B execution."""
    def create_mock_files(*args, **kwargs):
        science_yaml_file = kwargs.get('science_yaml_file') or args[3]
        science_report_file = kwargs.get('science_report_file') or args[4]
        
        # Create mock Phase B outputs
        with open(science_yaml_file, 'w') as f:
            f.write("# Science checked YAML\ntest: content")
        
        with open(science_report_file, 'w') as f:
            f.write("# Science Report\n## NO ACTION NEEDED\n- All validations passed")
        
        return {}  # Mock return value
    
    mock_science_check.side_effect = create_mock_files
    
    uptodate_file = test_env['test_yaml_file']  # Use test file as input
    science_yaml_file = os.path.join(test_env['test_dir'], "updatedB_test.yml")
    science_report_file = os.path.join(test_env['test_dir'], "reportB_test.txt")
    
    result = run_phase_b(
        test_env['test_yaml_file'], uptodate_file, test_env['standard_yaml_file'],
        science_yaml_file, science_report_file, None
    )
    
    assert result is True
    assert os.path.exists(science_yaml_file)
    assert os.path.exists(science_report_file)


@patch('master_ABC_run.run_science_check')
def test_run_phase_b_critical_error(mock_science_check, test_env):
    """Test Phase B execution with critical scientific errors."""
    mock_science_check.side_effect = ValueError("Critical scientific errors detected")
    
    uptodate_file = test_env['test_yaml_file']
    science_yaml_file = os.path.join(test_env['test_dir'], "updatedB_test.yml")
    science_report_file = os.path.join(test_env['test_dir'], "reportB_test.txt")
    
    result = run_phase_b(
        test_env['test_yaml_file'], uptodate_file, test_env['standard_yaml_file'],
        science_yaml_file, science_report_file, None
    )
    
    assert result is False


# Workflow integration tests
def test_command_line_parsing(test_env):
    """Test command line argument parsing."""
    # Test with Phase A
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file'], '--phase', 'A']
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            with patch('os.path.exists', return_value=True):
                with patch('master_ABC_run.run_phase_a', return_value=True):
                    result = main()
                    assert result == 0


@patch('master_ABC_run.run_phase_b')
@patch('master_ABC_run.run_phase_a')
@patch('os.path.exists')
@patch('os.remove')
def test_file_cleanup_on_successful_ab_workflow(mock_remove, mock_exists, mock_phase_a, mock_phase_b, test_env):
    """Test that intermediate files are cleaned up on successful AB workflow."""
    # Setup mocks for successful workflow
    mock_exists.return_value = True
    mock_phase_a.return_value = True
    mock_phase_b.return_value = True
    
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file'], '--phase', 'AB']
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            result = main()
            
            # Should succeed
            assert result == 0
            
            # Should attempt to clean up intermediate files
            assert mock_remove.called
            # Should call remove at least twice (for reportA and updatedA files)
            assert mock_remove.call_count >= 2


def test_error_handling_missing_standard_file(test_env):
    """Test error handling when standard file is missing."""
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file']]
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            with patch('os.path.exists', return_value=False):  # Standard file doesn't exist
                result = main()
                assert result == 1


def test_phase_b_with_existing_phase_a_output(test_env):
    """Test Phase B execution when Phase A output already exists."""
    # Create mock Phase A output files
    uptodate_file = os.path.join(test_env['test_dir'], "updatedA_test_config.yml")
    phase_a_report = os.path.join(test_env['test_dir'], "reportA_test_config.txt")
    
    with open(uptodate_file, 'w') as f:
        f.write("# UP TO DATE YAML\ntest: content")
    with open(phase_a_report, 'w') as f:
        f.write("# Phase A Report\ntest: content")
    
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file'], '--phase', 'B']
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            with patch('os.path.exists', return_value=True):
                with patch('master_ABC_run.run_phase_b', return_value=True) as mock_phase_b:
                    result = main()
                    
                    assert result == 0
                    # Should have called run_phase_b successfully
                    assert mock_phase_b.called


def test_phase_b_without_phase_a_output(test_env):
    """Test Phase B execution when no Phase A output exists."""
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file'], '--phase', 'B']
    
    def mock_exists(path):
        # Mock standard file exists, but no Phase A outputs
        if 'sample_config.yml' in path:
            return True
        return False
    
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            with patch('os.path.exists', side_effect=mock_exists):
                with patch('master_ABC_run.run_phase_b', return_value=True) as mock_phase_b:
                    result = main()
                    
                    assert result == 0
                    # Should have called run_phase_b with original user YAML
                    mock_phase_b.assert_called_once()
                    call_args = mock_phase_b.call_args
                    assert call_args[0][1] == test_env['test_yaml_file']  # Should use user file directly


def test_failed_ab_workflow_no_cleanup(test_env):
    """Test that cleanup doesn't happen when AB workflow fails."""
    test_args = ['test_master_ABC_run.py', test_env['test_yaml_file'], '--phase', 'AB']
    
    with patch('sys.argv', test_args):
        with patch('master_ABC_run.validate_input_file', return_value=test_env['test_yaml_file']):
            with patch('os.path.exists', return_value=True):
                with patch('master_ABC_run.run_phase_a', return_value=True):
                    with patch('master_ABC_run.run_phase_b', return_value=False):  # Phase B fails
                        with patch('os.remove') as mock_remove:
                            result = main()
                            
                            # Should fail
                            assert result == 1
                            
                            # Should NOT attempt cleanup when workflow fails
                            mock_remove.assert_not_called()


# File operation tests
def test_file_naming_conventions():
    """Test that files are named correctly for each phase."""
    basename = "test_config.yml"
    test_dir = "/test/dir"
    
    # Test Phase A naming
    uptodate_file, report_file, _, _, _ = setup_output_paths(
        os.path.join(test_dir, basename), 'A'
    )
    assert uptodate_file.endswith("updatedA_test_config.yml")
    assert report_file.endswith("reportA_test_config.txt")
    
    # Test Phase B naming
    uptodate_file, report_file, science_yaml_file, science_report_file, _ = setup_output_paths(
        os.path.join(test_dir, basename), 'B'
    )
    assert uptodate_file.endswith("updatedB_test_config.yml")
    assert report_file.endswith("reportB_test_config.txt")
    
    # Test AB naming
    uptodate_file, report_file, science_yaml_file, science_report_file, _ = setup_output_paths(
        os.path.join(test_dir, basename), 'AB'
    )
    assert uptodate_file.endswith("updatedA_test_config.yml")  # Intermediate
    assert report_file.endswith("reportA_test_config.txt")      # Intermediate
    assert science_yaml_file.endswith("updatedAB_test_config.yml")  # Final
    assert science_report_file.endswith("reportAB_test_config.txt")  # Final


def test_file_existence_checks(test_env):
    """Test file existence validation in workflow functions."""
    # Create a temporary file to test with
    temp_file = os.path.join(test_env['test_dir'], "temp_test.yml")
    with open(temp_file, 'w') as f:
        f.write("test: content")
    
    # Test that validate_input_file works with existing file
    result = validate_input_file(temp_file)
    assert os.path.exists(result)
    
    # Test with non-existent file
    non_existent = os.path.join(test_env['test_dir'], "non_existent.yml")
    with pytest.raises(FileNotFoundError):
        validate_input_file(non_existent)