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

import unittest
import os
import tempfile
import shutil
import subprocess
import sys
from pathlib import Path
import yaml
from unittest.mock import patch, MagicMock

# Add current directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from master_ABC_run import (
    validate_input_file, 
    setup_output_paths, 
    run_phase_a, 
    run_phase_b,
    main
)


class TestMasterABCRun(unittest.TestCase):
    """Test suite for the master ABC run workflow."""
    
    def setUp(self):
        """Set up test fixtures before each test method."""
        self.test_dir = tempfile.mkdtemp()
        self.test_yaml_file = os.path.join(self.test_dir, "test_config.yml")
        self.standard_yaml_file = os.path.join(self.test_dir, "standard_config.yml")
        
        # Create minimal test YAML configurations
        self.create_test_yaml_files()
        
    def tearDown(self):
        """Clean up test fixtures after each test method."""
        shutil.rmtree(self.test_dir)
        
    def create_test_yaml_files(self):
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
        with open(self.test_yaml_file, 'w') as f:
            yaml.dump(user_config, f, default_flow_style=False)
            
        with open(self.standard_yaml_file, 'w') as f:
            yaml.dump(standard_config, f, default_flow_style=False)

    def create_complete_test_yaml(self):
        """Create a complete test YAML that should pass both Phase A and B."""
        complete_config = {
            'name': 'complete test config',
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
                    'rslmethod': {'value': 2},
                    'roughlenmommethod': {'value': 1},
                    'roughlenheatmethod': {'value': 2}
                }
            },
            'sites': [{
                'name': 'complete_site',
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
                'initial_states': {
                    'paved': {'tsfc': {'value': 15.0}},
                    'bldgs': {'tsfc': {'value': 15.0}},
                    'grass': {'tsfc': {'value': 15.0}}
                }
            }]
        }
        
        complete_yaml_file = os.path.join(self.test_dir, "complete_config.yml")
        with open(complete_yaml_file, 'w') as f:
            yaml.dump(complete_config, f, default_flow_style=False)
        
        return complete_yaml_file


class TestInputValidation(TestMasterABCRun):
    """Test input file validation functionality."""
    
    def test_validate_existing_yaml_file(self):
        """Test validation of existing YAML file."""
        result = validate_input_file(self.test_yaml_file)
        self.assertEqual(result, os.path.abspath(self.test_yaml_file))
    
    def test_validate_nonexistent_file(self):
        """Test validation fails for nonexistent file."""
        nonexistent_file = os.path.join(self.test_dir, "nonexistent.yml")
        with self.assertRaises(FileNotFoundError):
            validate_input_file(nonexistent_file)
    
    def test_validate_non_yaml_file(self):
        """Test validation fails for non-YAML file."""
        txt_file = os.path.join(self.test_dir, "test.txt")
        with open(txt_file, 'w') as f:
            f.write("not a yaml file")
        
        with self.assertRaises(ValueError):
            validate_input_file(txt_file)
    
    def test_validate_unreadable_file(self):
        """Test validation fails for unreadable file."""
        # Create file and make it unreadable (Unix only)
        if os.name != 'nt':  # Skip on Windows
            unreadable_file = os.path.join(self.test_dir, "unreadable.yml")
            with open(unreadable_file, 'w') as f:
                f.write("test: content")
            os.chmod(unreadable_file, 0o000)
            
            try:
                with self.assertRaises(PermissionError):
                    validate_input_file(unreadable_file)
            finally:
                os.chmod(unreadable_file, 0o644)  # Restore permissions for cleanup


class TestOutputPaths(TestMasterABCRun):
    """Test output path generation for different phases."""
    
    def test_setup_output_paths_phase_a(self):
        """Test output path generation for Phase A only."""
        uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
            self.test_yaml_file, 'A'
        )
        
        basename = os.path.basename(self.test_yaml_file)
        name_without_ext = os.path.splitext(basename)[0]
        
        self.assertEqual(uptodate_file, os.path.join(self.test_dir, f"updatedA_{basename}"))
        self.assertEqual(report_file, os.path.join(self.test_dir, f"reportA_{name_without_ext}.txt"))
        self.assertEqual(science_yaml_file, os.path.join(self.test_dir, f"updatedB_{basename}"))
        self.assertEqual(science_report_file, os.path.join(self.test_dir, f"reportB_{name_without_ext}.txt"))
        self.assertEqual(dirname, self.test_dir)
    
    def test_setup_output_paths_phase_b(self):
        """Test output path generation for Phase B only."""
        uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
            self.test_yaml_file, 'B'
        )
        
        basename = os.path.basename(self.test_yaml_file)
        name_without_ext = os.path.splitext(basename)[0]
        
        self.assertEqual(uptodate_file, os.path.join(self.test_dir, f"updatedB_{basename}"))
        self.assertEqual(report_file, os.path.join(self.test_dir, f"reportB_{name_without_ext}.txt"))
        self.assertEqual(science_yaml_file, os.path.join(self.test_dir, f"updatedB_{basename}"))
        self.assertEqual(science_report_file, os.path.join(self.test_dir, f"reportB_{name_without_ext}.txt"))
    
    def test_setup_output_paths_phase_ab(self):
        """Test output path generation for complete A→B workflow."""
        uptodate_file, report_file, science_yaml_file, science_report_file, dirname = setup_output_paths(
            self.test_yaml_file, 'AB'
        )
        
        basename = os.path.basename(self.test_yaml_file)
        name_without_ext = os.path.splitext(basename)[0]
        
        # AB mode should use AB naming for final outputs
        self.assertEqual(uptodate_file, os.path.join(self.test_dir, f"updatedA_{basename}"))  # Intermediate
        self.assertEqual(report_file, os.path.join(self.test_dir, f"reportA_{name_without_ext}.txt"))  # Intermediate
        self.assertEqual(science_yaml_file, os.path.join(self.test_dir, f"updatedAB_{basename}"))  # Final
        self.assertEqual(science_report_file, os.path.join(self.test_dir, f"reportAB_{name_without_ext}.txt"))  # Final


class TestPhaseAExecution(TestMasterABCRun):
    """Test Phase A execution functionality."""
    
    @patch('master_ABC_run.annotate_missing_parameters')
    def test_run_phase_a_success(self, mock_annotate):
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
        
        uptodate_file = os.path.join(self.test_dir, "updatedA_test.yml")
        report_file = os.path.join(self.test_dir, "reportA_test.txt")
        
        result = run_phase_a(self.test_yaml_file, self.standard_yaml_file, uptodate_file, report_file)
        
        self.assertTrue(result)
        self.assertTrue(os.path.exists(uptodate_file))
        self.assertTrue(os.path.exists(report_file))
        mock_annotate.assert_called_once()
    
    @patch('master_ABC_run.annotate_missing_parameters')
    def test_run_phase_a_critical_error(self, mock_annotate):
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
        
        uptodate_file = os.path.join(self.test_dir, "updatedA_test.yml")
        report_file = os.path.join(self.test_dir, "reportA_test.txt")
        
        result = run_phase_a(self.test_yaml_file, self.standard_yaml_file, uptodate_file, report_file)
        
        self.assertFalse(result)
    
    @patch('master_ABC_run.annotate_missing_parameters')
    def test_run_phase_a_exception(self, mock_annotate):
        """Test Phase A execution with exception."""
        mock_annotate.side_effect = Exception("Test exception")
        
        uptodate_file = os.path.join(self.test_dir, "updatedA_test.yml")
        report_file = os.path.join(self.test_dir, "reportA_test.txt")
        
        result = run_phase_a(self.test_yaml_file, self.standard_yaml_file, uptodate_file, report_file)
        
        self.assertFalse(result)


class TestPhaseBExecution(TestMasterABCRun):
    """Test Phase B execution functionality."""
    
    @patch('master_ABC_run.run_science_check')
    def test_run_phase_b_success(self, mock_science_check):
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
        
        uptodate_file = self.test_yaml_file  # Use test file as input
        science_yaml_file = os.path.join(self.test_dir, "updatedB_test.yml")
        science_report_file = os.path.join(self.test_dir, "reportB_test.txt")
        
        result = run_phase_b(
            self.test_yaml_file, uptodate_file, self.standard_yaml_file,
            science_yaml_file, science_report_file, None
        )
        
        self.assertTrue(result)
        self.assertTrue(os.path.exists(science_yaml_file))
        self.assertTrue(os.path.exists(science_report_file))
    
    @patch('master_ABC_run.run_science_check')
    def test_run_phase_b_critical_error(self, mock_science_check):
        """Test Phase B execution with critical scientific errors."""
        mock_science_check.side_effect = ValueError("Critical scientific errors detected")
        
        uptodate_file = self.test_yaml_file
        science_yaml_file = os.path.join(self.test_dir, "updatedB_test.yml")
        science_report_file = os.path.join(self.test_dir, "reportB_test.txt")
        
        result = run_phase_b(
            self.test_yaml_file, uptodate_file, self.standard_yaml_file,
            science_yaml_file, science_report_file, None
        )
        
        self.assertFalse(result)


class TestWorkflowIntegration(TestMasterABCRun):
    """Test complete workflow integration scenarios."""
    
    def test_command_line_parsing(self):
        """Test command line argument parsing."""
        # Test with Phase A
        test_args = ['test_master_ABC_run.py', self.test_yaml_file, '--phase', 'A']
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                with patch('os.path.exists', return_value=True):
                    with patch('master_ABC_run.run_phase_a', return_value=True):
                        result = main()
                        self.assertEqual(result, 0)
    
    @patch('master_ABC_run.run_phase_b')
    @patch('master_ABC_run.run_phase_a')
    @patch('os.path.exists')
    @patch('os.remove')
    def test_file_cleanup_on_successful_ab_workflow(self, mock_remove, mock_exists, mock_phase_a, mock_phase_b):
        """Test that intermediate files are cleaned up on successful AB workflow."""
        # Setup mocks for successful workflow
        mock_exists.return_value = True
        mock_phase_a.return_value = True
        mock_phase_b.return_value = True
        
        test_args = ['test_master_ABC_run.py', self.test_yaml_file, '--phase', 'AB']
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                result = main()
                
                # Should succeed
                self.assertEqual(result, 0)
                
                # Should attempt to clean up intermediate files
                self.assertTrue(mock_remove.called)
                # Should call remove at least twice (for reportA and updatedA files)
                self.assertGreaterEqual(mock_remove.call_count, 2)
    
    def test_error_handling_missing_standard_file(self):
        """Test error handling when standard file is missing."""
        test_args = ['test_master_ABC_run.py', self.test_yaml_file]
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                with patch('os.path.exists', return_value=False):  # Standard file doesn't exist
                    result = main()
                    self.assertEqual(result, 1)
    
    def test_phase_b_with_existing_phase_a_output(self):
        """Test Phase B execution when Phase A output already exists."""
        # Create mock Phase A output files
        uptodate_file = os.path.join(self.test_dir, "updatedA_test_config.yml")
        phase_a_report = os.path.join(self.test_dir, "reportA_test_config.txt")
        
        with open(uptodate_file, 'w') as f:
            f.write("# UP TO DATE YAML\ntest: content")
        with open(phase_a_report, 'w') as f:
            f.write("# Phase A Report\ntest: content")
        
        test_args = ['test_master_ABC_run.py', self.test_yaml_file, '--phase', 'B']
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                with patch('os.path.exists', return_value=True):
                    with patch('master_ABC_run.run_phase_b', return_value=True) as mock_phase_b:
                        result = main()
                        
                        self.assertEqual(result, 0)
                        # Should have called run_phase_b with the existing Phase A output
                        mock_phase_b.assert_called_once()
                        call_args = mock_phase_b.call_args
                        # The second parameter (uptodate_file) should be the Phase A output when it exists
                        # But since we mocked os.path.exists to always return True, it uses the updatedA file
                        # Check that it called run_phase_b successfully
                        self.assertTrue(mock_phase_b.called)
    
    def test_phase_b_without_phase_a_output(self):
        """Test Phase B execution when no Phase A output exists."""
        test_args = ['test_master_ABC_run.py', self.test_yaml_file, '--phase', 'B']
        
        def mock_exists(path):
            # Mock standard file exists, but no Phase A outputs
            if 'sample_config.yml' in path:
                return True
            return False
        
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                with patch('os.path.exists', side_effect=mock_exists):
                    with patch('master_ABC_run.run_phase_b', return_value=True) as mock_phase_b:
                        result = main()
                        
                        self.assertEqual(result, 0)
                        # Should have called run_phase_b with original user YAML
                        mock_phase_b.assert_called_once()
                        call_args = mock_phase_b.call_args
                        self.assertEqual(call_args[0][1], self.test_yaml_file)  # Should use user file directly
    
    def test_failed_ab_workflow_no_cleanup(self):
        """Test that cleanup doesn't happen when AB workflow fails."""
        test_args = ['test_master_ABC_run.py', self.test_yaml_file, '--phase', 'AB']
        
        with patch('sys.argv', test_args):
            with patch('master_ABC_run.validate_input_file', return_value=self.test_yaml_file):
                with patch('os.path.exists', return_value=True):
                    with patch('master_ABC_run.run_phase_a', return_value=True):
                        with patch('master_ABC_run.run_phase_b', return_value=False):  # Phase B fails
                            with patch('os.remove') as mock_remove:
                                result = main()
                                
                                # Should fail
                                self.assertEqual(result, 1)
                                
                                # Should NOT attempt cleanup when workflow fails
                                mock_remove.assert_not_called()


class TestFileOperations(TestMasterABCRun):
    """Test file operations and naming conventions."""
    
    def test_file_naming_conventions(self):
        """Test that files are named correctly for each phase."""
        basename = "test_config.yml"
        name_without_ext = "test_config"
        test_dir = "/test/dir"
        
        # Test Phase A naming
        uptodate_file, report_file, _, _, _ = setup_output_paths(
            os.path.join(test_dir, basename), 'A'
        )
        self.assertTrue(uptodate_file.endswith("updatedA_test_config.yml"))
        self.assertTrue(report_file.endswith("reportA_test_config.txt"))
        
        # Test Phase B naming
        uptodate_file, report_file, science_yaml_file, science_report_file, _ = setup_output_paths(
            os.path.join(test_dir, basename), 'B'
        )
        self.assertTrue(uptodate_file.endswith("updatedB_test_config.yml"))
        self.assertTrue(report_file.endswith("reportB_test_config.txt"))
        
        # Test AB naming
        uptodate_file, report_file, science_yaml_file, science_report_file, _ = setup_output_paths(
            os.path.join(test_dir, basename), 'AB'
        )
        self.assertTrue(uptodate_file.endswith("updatedA_test_config.yml"))  # Intermediate
        self.assertTrue(report_file.endswith("reportA_test_config.txt"))      # Intermediate
        self.assertTrue(science_yaml_file.endswith("updatedAB_test_config.yml"))  # Final
        self.assertTrue(science_report_file.endswith("reportAB_test_config.txt"))  # Final
    
    def test_file_existence_checks(self):
        """Test file existence validation in workflow functions."""
        # Create a temporary file to test with
        temp_file = os.path.join(self.test_dir, "temp_test.yml")
        with open(temp_file, 'w') as f:
            f.write("test: content")
        
        # Test that validate_input_file works with existing file
        result = validate_input_file(temp_file)
        self.assertTrue(os.path.exists(result))
        
        # Test with non-existent file
        non_existent = os.path.join(self.test_dir, "non_existent.yml")
        with self.assertRaises(FileNotFoundError):
            validate_input_file(non_existent)


if __name__ == '__main__':
    # Run the test suite
    unittest.main(verbosity=2)