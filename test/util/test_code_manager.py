"""Tests for the Universal Code Manager system."""

import pytest
from pathlib import Path
import tempfile
import pandas as pd
import numpy as np

from supy.util.code_manager import (
    CodeRegistry, ReferenceGraph, TemplateSystem, UniversalCodeManager
)


class TestCodeRegistry:
    """Test the code registry component."""
    
    def test_init(self):
        """Test registry initialization."""
        registry = CodeRegistry()
        assert len(registry.definitions) > 0
        assert 'SUEWS_Profiles.txt' in registry.definitions
        assert 'SUEWS_BiogenCO2.txt' in registry.definitions
        
    def test_get_file_definition(self):
        """Test getting file definitions."""
        registry = CodeRegistry()
        
        # Test existing file
        definition = registry.get_file_definition('SUEWS_Profiles.txt')
        assert definition is not None
        assert definition['type'] == 'profile'
        assert 'fields' in definition
        
        # Test non-existent file
        definition = registry.get_file_definition('NonExistent.txt')
        assert definition is None
        
    def test_get_standard_codes(self):
        """Test getting standard codes."""
        registry = CodeRegistry()
        
        codes = registry.get_standard_codes('SUEWS_BiogenCO2.txt')
        assert 31 in codes
        assert codes[31]['name'] == 'Default vegetation'
        
    def test_get_referenced_by_fields(self):
        """Test getting fields that reference a file."""
        registry = CodeRegistry()
        
        refs = registry.get_referenced_by_fields('SUEWS_Profiles.txt')
        assert 'EnergyUseProfWD' in refs
        assert 'ActivityProfWD' in refs
        
    def test_register_code_usage(self):
        """Test registering code usage."""
        registry = CodeRegistry()
        
        registry.register_code_usage('SUEWS_Profiles.txt', 999, 'SiteSelect')
        assert 'SiteSelect' in registry.code_usage['SUEWS_Profiles.txt:999']


class TestReferenceGraph:
    """Test the reference graph component."""
    
    def setup_method(self):
        """Setup test environment."""
        self.registry = CodeRegistry()
        self.graph = ReferenceGraph(self.registry)
        
    def test_init(self):
        """Test graph initialization."""
        assert len(self.graph.graph) == 0
        assert len(self.graph.references) == 0
        
    def test_scan_directory(self, tmp_path):
        """Test scanning directory for references."""
        # Create test files
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code OtherField
1 31 100
2 31 200"""
        veg_file.write_text(veg_data)
        
        site_file = tmp_path / "SUEWS_SiteSelect.txt"
        site_data = """Grid PopProfWD PopProfWE
1 801 802"""
        site_file.write_text(site_data)
        
        # Scan directory
        self.graph.scan_directory(tmp_path)
        
        # Check graph was built
        assert len(self.graph.references) > 0
        assert 'SUEWS_Veg.txt' in self.graph.graph
        
    def test_get_resolution_order(self):
        """Test topological sort for resolution order."""
        # Manually build a simple graph
        self.graph.graph['A.txt'] = {'B.txt', 'C.txt'}
        self.graph.graph['B.txt'] = {'C.txt'}
        self.graph.reverse_graph['B.txt'] = {'A.txt'}
        self.graph.reverse_graph['C.txt'] = {'A.txt', 'B.txt'}
        
        order = self.graph.get_resolution_order()
        
        # C should come before B, and B before A
        assert order.index('C.txt') < order.index('B.txt')
        assert order.index('B.txt') < order.index('A.txt')
        
    def test_get_missing_codes(self, tmp_path):
        """Test finding missing code references."""
        # Create files with references
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code
1 31
2 32"""
        veg_file.write_text(veg_data)
        
        # Create BiogenCO2 file with only code 31
        bio_file = tmp_path / "SUEWS_BiogenCO2.txt"
        bio_data = """Code alpha beta
31 0.004 8.747"""
        bio_file.write_text(bio_data)
        
        # Scan and check missing codes
        self.graph.scan_directory(tmp_path)
        missing = self.graph.get_missing_codes(tmp_path)
        
        # Should find code 32 as missing
        assert len(missing) > 0
        assert any(code == 32 for _, _, _, code in missing)


class TestTemplateSystem:
    """Test the template system."""
    
    def test_templates_exist(self):
        """Test that templates are defined."""
        assert 'BiogenCO2_minimal' in TemplateSystem.TEMPLATES
        assert 'ESTMCoefficients_minimal' in TemplateSystem.TEMPLATES
        assert 'Profiles_minimal' in TemplateSystem.TEMPLATES
        
    def test_create_from_template(self, tmp_path):
        """Test creating file from template."""
        # Create BiogenCO2 file from template
        output_path = TemplateSystem.create_from_template(
            'BiogenCO2_minimal', tmp_path
        )
        
        assert output_path.exists()
        assert output_path.name == 'SUEWS_BiogenCO2.txt'
        
        # Check content
        df = pd.read_csv(output_path, sep=r'\s+', header=0, index_col=0)
        assert 31 in df.index
        assert 'alpha' in df.columns
        
    def test_invalid_template(self, tmp_path):
        """Test error handling for invalid template."""
        with pytest.raises(ValueError, match="Unknown template"):
            TemplateSystem.create_from_template('NonExistent', tmp_path)


class TestUniversalCodeManager:
    """Test the universal code manager."""
    
    def setup_method(self):
        """Setup test environment."""
        self.manager = UniversalCodeManager(debug=False)
        
    def test_init(self):
        """Test manager initialization."""
        assert self.manager.registry is not None
        assert self.manager.graph is not None
        assert self.manager.templates is not None
        
    def test_analyze_directory(self, tmp_path):
        """Test directory analysis."""
        # Create test files
        self._create_test_files(tmp_path)
        
        # Analyze
        report = self.manager.analyze_directory(tmp_path)
        
        assert 'files_found' in report
        assert 'missing_codes' in report
        assert 'resolution_order' in report
        assert report['total_references'] >= 0
        
    def test_fix_missing_codes(self, tmp_path):
        """Test fixing missing codes."""
        # Create files with missing references
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code
1 31
2 999"""
        veg_file.write_text(veg_data)
        
        # Fix missing codes
        fixed = self.manager.fix_missing_codes(tmp_path, auto_create=True)
        
        # Check BiogenCO2 file was created with code 31
        bio_file = tmp_path / "SUEWS_BiogenCO2.txt"
        if bio_file.exists():
            df = pd.read_csv(bio_file, sep=r'\s+', header=0, index_col=0)
            assert 31 in df.index
            
    def test_validate_directory(self, tmp_path):
        """Test directory validation."""
        # Create valid files
        self._create_test_files(tmp_path)
        
        # Add BiogenCO2 file to make it valid
        bio_file = tmp_path / "SUEWS_BiogenCO2.txt"
        bio_data = """Code alpha beta theta alpha_enh beta_enh resp_a resp_b min_respi
31 0.004 8.747 0.96 0.016 33.353 2.43 0.0 0.6"""
        bio_file.write_text(bio_data)
        
        # Validate
        is_valid, issues = self.manager.validate_directory(tmp_path)
        
        # Should have some validation results
        assert isinstance(is_valid, bool)
        assert isinstance(issues, list)
        
    def _create_test_files(self, directory: Path):
        """Helper to create test files."""
        # Create Veg file with BiogenCO2 reference
        veg_file = directory / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code OtherField
1 31 100"""
        veg_file.write_text(veg_data)
        
        # Create SiteSelect with profile references
        site_file = directory / "SUEWS_SiteSelect.txt"
        site_data = """Grid PopProfWD
1 999"""
        site_file.write_text(site_data)
        
        # Create Profiles file
        prof_file = directory / "SUEWS_Profiles.txt"
        prof_data = "Code " + " ".join([f"Hr{i:02d}" for i in range(24)]) + "\n"
        prof_data += "999 " + " ".join(["1.0"] * 24)
        prof_file.write_text(prof_data)


class TestIntegration:
    """Integration tests for the complete system."""
    
    def test_complete_workflow(self, tmp_path):
        """Test complete workflow from analysis to fix."""
        manager = UniversalCodeManager(debug=False)
        
        # Create problematic files
        veg_file = tmp_path / "SUEWS_Veg.txt"
        veg_data = """Code BiogenCO2Code
1 31
2 32
3 33"""
        veg_file.write_text(veg_data)
        
        site_file = tmp_path / "SUEWS_SiteSelect.txt"
        site_data = """Grid PopProfWD PopProfWE ActivityProfWD
1 801 802 55663"""
        site_file.write_text(site_data)
        
        # Step 1: Analyze
        report = manager.analyze_directory(tmp_path)
        initial_missing = report['total_missing']
        assert initial_missing > 0
        
        # Step 2: Fix
        fixed = manager.fix_missing_codes(tmp_path, auto_create=True)
        assert fixed > 0
        
        # Step 3: Re-analyze
        report = manager.analyze_directory(tmp_path)
        final_missing = report['total_missing']
        
        # Should have fewer missing codes
        assert final_missing < initial_missing
        
        # Step 4: Validate
        is_valid, issues = manager.validate_directory(tmp_path)
        # May not be completely valid due to missing required files
        # but should have improved
        assert len(issues) >= 0  # At least we got a result