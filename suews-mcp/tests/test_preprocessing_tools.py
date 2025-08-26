"""Tests for MCP server preprocessing tool integration."""

import pytest
import asyncio
import pandas as pd
import numpy as np
import tempfile
import yaml
from pathlib import Path
from unittest.mock import AsyncMock

from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.config import MCPServerConfig


class TestPreprocessingToolsIntegration:
    """Test integration of preprocessing tools with MCP server."""
    
    @pytest.fixture
    def config(self):
        """Create test MCP server configuration."""
        return MCPServerConfig(
            server_name="SUEWS MCP Test Server",
            server_version="1.0.0-test",
            max_concurrent_simulations=2,
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True
        )
    
    @pytest.fixture
    def handlers(self, config):
        """Create MCP handlers instance."""
        return SUEWSMCPHandlers(config)
    
    @pytest.fixture
    def sample_forcing_data(self, tmp_path):
        """Create sample forcing data file."""
        n_rows = 24
        data = {
            'iy': [2023] * n_rows,
            'id': [1] * n_rows,
            'it': list(range(n_rows)),
            'imin': [0] * n_rows,
            'qn': np.random.normal(100, 50, n_rows),
            'qh': np.random.normal(50, 30, n_rows),
            'qe': np.random.normal(30, 20, n_rows),
            'qs': np.random.normal(20, 10, n_rows),
            'qf': np.random.normal(10, 5, n_rows),
            'U': np.random.uniform(1, 10, n_rows),
            'RH': np.random.uniform(30, 90, n_rows),
            'Tair': np.random.normal(15, 5, n_rows),
            'pres': np.random.uniform(98, 102, n_rows),
            'rain': np.random.exponential(0.1, n_rows),
            'kdown': np.maximum(0, np.random.normal(200, 100, n_rows)),
        }
        
        df = pd.DataFrame(data)
        file_path = tmp_path / "sample_forcing.txt"
        df.to_csv(file_path, sep=" ", index=False, float_format="%.2f")
        return file_path
    
    @pytest.fixture 
    def sample_config_file(self, tmp_path):
        """Create sample SUEWS configuration file."""
        config = {
            "name": "test_config",
            "description": "Test configuration for MCP server",
            "model": {
                "control": {
                    "tstep": 300,
                    "forcing_file": {"value": "forcing/"}
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1}
                }
            },
            "sites": [
                {
                    "name": "test_site",
                    "properties": {
                        "lat": {"value": 51.5},
                        "lng": {"value": -0.1},
                        "alt": {"value": 10.0}
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.3}}
                    }
                }
            ]
        }
        
        file_path = tmp_path / "test_config.yml"
        with open(file_path, 'w') as f:
            yaml.dump(config, f)
        return file_path
    
    @pytest.fixture
    def sample_csv_data(self, tmp_path):
        """Create sample CSV data file for format conversion."""
        data = pd.DataFrame({
            'year': [2023] * 24,
            'day_of_year': [1] * 24,
            'hour': list(range(24)),
            'minute': [0] * 24,
            'temperature': np.random.normal(15, 5, 24),
            'humidity': np.random.uniform(30, 90, 24),
            'wind_speed': np.random.uniform(1, 10, 24),
            'solar_radiation': np.maximum(0, np.random.normal(200, 100, 24))
        })
        
        file_path = tmp_path / "sample_data.csv"
        data.to_csv(file_path, index=False)
        return file_path

    @pytest.mark.asyncio
    async def test_list_tools_includes_preprocessing(self, handlers):
        """Test that list_tools includes preprocessing tools."""
        result = await handlers.handle_list_tools()
        
        tool_names = [tool.name for tool in result.tools]
        
        # Check that preprocessing tools are included
        assert "preprocess_forcing" in tool_names
        assert "validate_config" in tool_names
        assert "convert_data_format" in tool_names
        
        # Check tool descriptions
        preprocess_tool = next(tool for tool in result.tools if tool.name == "preprocess_forcing")
        assert "meteorological forcing data" in preprocess_tool.description.lower()
        
        validate_tool = next(tool for tool in result.tools if tool.name == "validate_config") 
        assert "configuration" in validate_tool.description.lower()
        
        convert_tool = next(tool for tool in result.tools if tool.name == "convert_data_format")
        assert "format" in convert_tool.description.lower()

    @pytest.mark.asyncio
    async def test_preprocess_forcing_tool_success(self, handlers, sample_forcing_data, tmp_path):
        """Test successful forcing data preprocessing."""
        output_file = tmp_path / "processed_output.txt"
        
        arguments = {
            "input_file": str(sample_forcing_data),
            "output_file": str(output_file),
            "validate_energy_balance": True,
            "auto_fix_issues": True
        }
        
        result = await handlers.handle_call_tool("preprocess_forcing", arguments)
        
        assert not result.is_error
        assert len(result.content) == 1
        
        content_text = result.content[0].text
        assert "Preprocessing Results" in content_text
        assert "SUCCESS" in content_text or "Processing Steps" in content_text
        assert output_file.exists()
    
    @pytest.mark.asyncio
    async def test_preprocess_forcing_tool_missing_input(self, handlers):
        """Test preprocessing tool with missing input file parameter."""
        arguments = {}  # Missing input_file
        
        result = await handlers.handle_call_tool("preprocess_forcing", arguments)
        
        assert result.is_error
        assert "input_file parameter is required" in result.content[0].text
    
    @pytest.mark.asyncio
    async def test_preprocess_forcing_tool_invalid_file(self, handlers):
        """Test preprocessing tool with invalid input file."""
        arguments = {
            "input_file": "/nonexistent/file.txt"
        }
        
        result = await handlers.handle_call_tool("preprocess_forcing", arguments)
        
        # Should handle error gracefully
        content_text = result.content[0].text
        assert "Error" in content_text or "FAILED" in content_text

    @pytest.mark.asyncio
    async def test_validate_config_tool_success(self, handlers, sample_config_file):
        """Test successful configuration validation."""
        arguments = {
            "config_file": str(sample_config_file),
            "strict_mode": False,
            "check_file_paths": False  # Skip file path checks for test
        }
        
        result = await handlers.handle_call_tool("validate_config", arguments)
        
        assert not result.is_error
        content_text = result.content[0].text
        assert "Validation Results" in content_text
        assert "PASSED" in content_text or "Validation Checklist" in content_text
    
    @pytest.mark.asyncio
    async def test_validate_config_tool_strict_mode(self, handlers, sample_config_file):
        """Test configuration validation in strict mode."""
        arguments = {
            "config_file": str(sample_config_file),
            "strict_mode": True,
            "check_file_paths": False
        }
        
        result = await handlers.handle_call_tool("validate_config", arguments)
        
        content_text = result.content[0].text
        assert "Strict" in content_text
        assert "Validation Results" in content_text
    
    @pytest.mark.asyncio
    async def test_validate_config_tool_missing_file(self, handlers):
        """Test configuration validation with missing file."""
        arguments = {
            "config_file": "/nonexistent/config.yml"
        }
        
        result = await handlers.handle_call_tool("validate_config", arguments)
        
        # Should handle error gracefully  
        content_text = result.content[0].text
        assert "Error" in content_text or "FAILED" in content_text

    @pytest.mark.asyncio
    async def test_convert_data_format_tool_success(self, handlers, sample_csv_data, tmp_path):
        """Test successful data format conversion."""
        output_file = tmp_path / "converted_output.txt"
        
        arguments = {
            "input_file": str(sample_csv_data),
            "output_file": str(output_file),
            "input_format": "csv",
            "output_format": "txt",
            "column_mapping": {
                "temperature": "Tair",
                "humidity": "RH"
            }
        }
        
        result = await handlers.handle_call_tool("convert_data_format", arguments)
        
        assert not result.is_error
        content_text = result.content[0].text
        assert "Conversion Results" in content_text
        assert "SUCCESS" in content_text
        assert output_file.exists()
        
        # Check that column mapping was mentioned
        assert "Column Mapping" in content_text
    
    @pytest.mark.asyncio
    async def test_convert_data_format_tool_suews_format(self, handlers, sample_csv_data, tmp_path):
        """Test conversion to SUEWS format."""
        output_file = tmp_path / "suews_format.txt"
        
        arguments = {
            "input_file": str(sample_csv_data),
            "output_file": str(output_file), 
            "input_format": "csv",
            "output_format": "suews_txt",
            "column_mapping": {
                "year": "iy",
                "day_of_year": "id",
                "hour": "it",
                "minute": "imin",
                "temperature": "Tair",
                "humidity": "RH",
                "wind_speed": "U",
                "solar_radiation": "kdown"
            }
        }
        
        result = await handlers.handle_call_tool("convert_data_format", arguments)
        
        assert not result.is_error
        content_text = result.content[0].text
        assert "SUCCESS" in content_text
        assert "SUEWS" in content_text
        assert output_file.exists()
        
        # Verify the output format is space-separated
        df_output = pd.read_csv(output_file, sep=" ")
        assert "iy" in df_output.columns
        assert "Tair" in df_output.columns
    
    @pytest.mark.asyncio
    async def test_convert_data_format_tool_missing_params(self, handlers):
        """Test format conversion with missing parameters."""
        arguments = {
            "input_file": "test.csv"
            # Missing output_file, input_format, output_format
        }
        
        result = await handlers.handle_call_tool("convert_data_format", arguments)
        
        assert result.is_error
        assert "Missing required parameters" in result.content[0].text

    @pytest.mark.asyncio
    async def test_tool_error_handling(self, handlers):
        """Test that tool errors are handled gracefully."""
        # Test with completely invalid tool name
        result = await handlers.handle_call_tool("nonexistent_tool", {})
        
        assert result.is_error
        assert "Unknown tool" in result.content[0].text
        
        # Test preprocessing tools with various error conditions already covered above

    @pytest.mark.asyncio
    async def test_preprocessing_tools_workflow(self, handlers, tmp_path):
        """Test complete workflow using preprocessing tools."""
        
        # Step 1: Create raw CSV data
        raw_data = pd.DataFrame({
            'year': [2023] * 12,
            'doy': [1] * 12,
            'hour': list(range(12)),
            'min': [0] * 12,
            'temp_air': np.random.normal(15, 3, 12),
            'rel_hum': np.random.uniform(40, 80, 12),
            'wind_spd': np.random.uniform(2, 8, 12),
            'pressure': np.random.uniform(99, 101, 12),
        })
        
        raw_file = tmp_path / "raw_data.csv"
        raw_data.to_csv(raw_file, index=False)
        
        # Step 2: Convert format to SUEWS
        suews_file = tmp_path / "suews_data.txt"
        
        convert_args = {
            "input_file": str(raw_file),
            "output_file": str(suews_file),
            "input_format": "csv",
            "output_format": "suews_txt",
            "column_mapping": {
                "year": "iy",
                "doy": "id",
                "hour": "it", 
                "min": "imin",
                "temp_air": "Tair",
                "rel_hum": "RH",
                "wind_spd": "U",
                "pressure": "pres"
            }
        }
        
        convert_result = await handlers.handle_call_tool("convert_data_format", convert_args)
        assert not convert_result.is_error
        assert suews_file.exists()
        
        # Step 3: Preprocess the converted data
        processed_file = tmp_path / "processed_data.txt"
        
        preprocess_args = {
            "input_file": str(suews_file),
            "output_file": str(processed_file),
            "validate_energy_balance": False,  # Don't have energy balance variables
            "auto_fix_issues": True
        }
        
        preprocess_result = await handlers.handle_call_tool("preprocess_forcing", preprocess_args)
        # May have warnings but should not error completely
        assert processed_file.exists()
        
        # Step 4: Create and validate config
        config = {
            "name": "workflow_test",
            "description": "Workflow test config",
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": str(processed_file)}
                },
                "physics": {
                    "netradiationmethod": {"value": 1}
                }
            },
            "sites": [{
                "name": "test",
                "properties": {
                    "lat": {"value": 50.0},
                    "lng": {"value": 0.0},
                    "alt": {"value": 100.0}
                }
            }]
        }
        
        config_file = tmp_path / "workflow_config.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        validate_args = {
            "config_file": str(config_file),
            "strict_mode": False,
            "check_file_paths": True
        }
        
        validate_result = await handlers.handle_call_tool("validate_config", validate_args)
        # Should validate without critical errors
        validate_text = validate_result.content[0].text
        assert "Validation Results" in validate_text
        
        print("✅ Complete preprocessing workflow test passed!")

    @pytest.mark.asyncio 
    async def test_tool_schema_validation(self, handlers):
        """Test that tool schemas are properly defined."""
        result = await handlers.handle_list_tools()
        
        preprocessing_tools = [
            tool for tool in result.tools 
            if tool.name in ["preprocess_forcing", "validate_config", "convert_data_format"]
        ]
        
        # Should have all 3 preprocessing tools
        assert len(preprocessing_tools) == 3
        
        # Check schema structure for each tool
        for tool in preprocessing_tools:
            assert "input_schema" in tool.__dict__
            schema = tool.input_schema
            assert "type" in schema
            assert schema["type"] == "object"
            assert "properties" in schema
            assert "required" in schema
            
            # Each tool should have required parameters
            assert len(schema["required"]) > 0
            
            # Check that required parameters are in properties
            for required_param in schema["required"]:
                assert required_param in schema["properties"]


if __name__ == "__main__":
    pytest.main([__file__, "-v"])