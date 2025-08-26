"""Integration tests for complete data processing workflows in SUEWS MCP Server."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch
import tempfile
import yaml
import json
from datetime import datetime, timedelta
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.preprocessing import (
    ForcingDataPreprocessor,
    ConfigValidator, 
    DataFormatConverter,
    PreprocessingResult,
    DataQualityIssue
)
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.config import MCPServerConfig


class TestCompleteDataWorkflows:
    """Test complete end-to-end data processing workflows."""

    @pytest.fixture
    def workflow_config(self):
        """Configuration for workflow testing."""
        return MCPServerConfig(
            server_name="workflow-test-server",
            server_version="0.1.0-test",
            log_level="INFO",
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
        )

    @pytest.fixture
    def raw_meteorological_data(self):
        """Raw meteorological data in various formats for testing workflows."""
        n_days = 3
        n_hours = n_days * 24
        base_date = pd.Timestamp("2024-06-15")  # Summer date
        
        # Generate realistic summer weather pattern
        hours = np.arange(n_hours)
        
        # Solar radiation with realistic diurnal cycle
        solar_angle = 2 * np.pi * (hours % 24) / 24 - np.pi/2
        solar_base = np.maximum(0, 800 * np.sin(solar_angle) * 
                               np.maximum(0, np.cos(solar_angle)))
        solar_noise = np.random.normal(0, 50, n_hours)
        solar_radiation = np.maximum(0, solar_base + solar_noise)
        
        # Temperature with diurnal cycle
        temp_cycle = 25 + 10 * np.sin(2 * np.pi * (hours % 24) / 24 - np.pi/2)
        temp_noise = np.random.normal(0, 1.5, n_hours)
        air_temperature = temp_cycle + temp_noise
        
        # Humidity inversely related to temperature
        rel_humidity = np.clip(80 - (air_temperature - 20) * 2 + 
                              np.random.normal(0, 8, n_hours), 30, 95)
        
        # Wind with some variability
        wind_speed = np.maximum(0.5, 4.0 + np.random.normal(0, 1.5, n_hours))
        
        # Pressure with realistic variation
        pressure_hpa = 1013.25 + np.random.normal(0, 5, n_hours)
        pressure_kpa = pressure_hpa / 10.0
        
        # Energy fluxes derived from solar radiation
        net_radiation = solar_radiation * 0.65 + np.random.normal(0, 20, n_hours)
        sensible_heat = net_radiation * 0.4 + np.random.normal(0, 15, n_hours)
        latent_heat = net_radiation * 0.35 + np.random.normal(0, 12, n_hours)
        storage_heat = net_radiation * 0.15 + np.random.normal(0, 8, n_hours)
        anthro_heat = np.full(n_hours, 25.0) + np.random.normal(0, 5, n_hours)
        
        # Precipitation (mostly zero with occasional rain)
        rain_prob = np.random.random(n_hours)
        precipitation = np.where(rain_prob < 0.95, 0, 
                                np.random.exponential(2.0, n_hours))
        
        return {
            'csv_format': pd.DataFrame({
                'timestamp': [base_date + pd.Timedelta(hours=h) for h in hours],
                'year': [2024] * n_hours,
                'day_of_year': [((base_date + pd.Timedelta(hours=h)).dayofyear) 
                               for h in hours],
                'hour': [h % 24 for h in hours],
                'minute': [0] * n_hours,
                'temperature_celsius': air_temperature,
                'humidity_percent': rel_humidity,
                'wind_ms': wind_speed,
                'pressure_kpa': pressure_kpa,
                'solar_wm2': solar_radiation,
                'net_rad_wm2': net_radiation,
                'sens_heat_wm2': sensible_heat,
                'lat_heat_wm2': latent_heat,
                'stor_heat_wm2': storage_heat,
                'anth_heat_wm2': anthro_heat,
                'rainfall_mm': precipitation
            }),
            
            'suews_format': pd.DataFrame({
                'iy': [2024] * n_hours,
                'id': [((base_date + pd.Timedelta(hours=h)).dayofyear) 
                      for h in hours],
                'it': [h % 24 for h in hours],
                'imin': [0] * n_hours,
                'qn': net_radiation,
                'qh': sensible_heat,
                'qe': latent_heat,
                'qs': storage_heat,
                'qf': anthro_heat,
                'U': wind_speed,
                'RH': rel_humidity,
                'Tair': air_temperature,
                'pres': pressure_kpa,
                'rain': precipitation,
                'kdown': solar_radiation
            })
        }

    async def test_csv_to_suews_workflow(self, raw_meteorological_data, tmp_path):
        """Test complete workflow: CSV input -> SUEWS format -> validation -> preprocessing."""
        
        # Step 1: Save raw CSV data
        csv_data = raw_meteorological_data['csv_format']
        input_csv = tmp_path / "raw_weather_data.csv"
        csv_data.to_csv(input_csv, index=False)
        
        # Step 2: Convert CSV to SUEWS format
        converter = DataFormatConverter()
        converted_file = tmp_path / "converted_forcing.txt"
        
        column_mapping = {
            'year': 'iy',
            'day_of_year': 'id', 
            'hour': 'it',
            'minute': 'imin',
            'temperature_celsius': 'Tair',
            'humidity_percent': 'RH',
            'wind_ms': 'U',
            'pressure_kpa': 'pres',
            'solar_wm2': 'kdown',
            'net_rad_wm2': 'qn',
            'sens_heat_wm2': 'qh',
            'lat_heat_wm2': 'qe',
            'stor_heat_wm2': 'qs',
            'anth_heat_wm2': 'qf',
            'rainfall_mm': 'rain'
        }
        
        convert_result = converter.convert_format(
            input_path=input_csv,
            output_path=converted_file,
            input_format="csv",
            output_format="suews_txt",
            column_mapping=column_mapping
        )
        
        assert convert_result.success is True
        assert converted_file.exists()
        assert convert_result.data is not None
        assert len(convert_result.data) == 72  # 3 days of hourly data
        
        # Verify column mapping worked
        suews_data = pd.read_csv(converted_file, sep=' ')
        expected_columns = ['iy', 'id', 'it', 'imin', 'Tair', 'RH', 'U', 'pres', 
                           'kdown', 'qn', 'qh', 'qe', 'qs', 'qf', 'rain']
        for col in expected_columns:
            assert col in suews_data.columns
        
        # Step 3: Preprocess the converted data
        preprocessor = ForcingDataPreprocessor()
        processed_file = tmp_path / "processed_forcing.txt"
        
        preprocess_result = preprocessor.preprocess_forcing_file(
            file_path=converted_file,
            output_path=processed_file,
            validate_energy_balance=True,
            auto_fix_issues=True
        )
        
        assert preprocess_result.success is True
        assert processed_file.exists()
        assert preprocess_result.data is not None
        
        # Check preprocessing results
        assert 'detected_timestep_seconds' in preprocess_result.metadata
        assert preprocess_result.metadata['detected_timestep_seconds'] == 3600
        
        # Should have minimal errors with good data
        errors = [i for i in preprocess_result.issues if i.severity == 'error']
        assert len(errors) == 0
        
        # Step 4: Validate final data structure
        final_data = pd.read_csv(processed_file, sep=' ')
        assert len(final_data) == 72
        assert all(col in final_data.columns for col in expected_columns)
        
        print(f"✅ CSV to SUEWS workflow completed successfully:")
        print(f"   • Input CSV: {len(csv_data)} rows")
        print(f"   • Converted: {len(suews_data)} rows")
        print(f"   • Processed: {len(final_data)} rows")
        print(f"   • Issues found: {len(preprocess_result.issues)}")

    async def test_data_quality_workflow(self, tmp_path):
        """Test workflow with data quality issues and automatic fixes."""
        
        # Create problematic data
        n_hours = 48
        problematic_data = pd.DataFrame({
            'iy': [2024] * n_hours,
            'id': [1] * 24 + [2] * 24,
            'it': list(range(24)) * 2,
            'imin': [0] * n_hours,
            'qn': [-999] * 5 + list(np.random.normal(100, 30, n_hours - 5)),  # Missing values
            'qh': list(np.random.normal(50, 20, n_hours)),
            'qe': list(np.random.normal(30, 15, n_hours)), 
            'qs': list(np.random.normal(10, 10, n_hours)),
            'qf': [-50] + list(np.random.uniform(10, 30, n_hours - 1)),  # Invalid negative
            'U': [-2.0] + list(np.random.uniform(1, 8, n_hours - 1)),  # Invalid negative wind
            'RH': [150] + list(np.random.uniform(40, 85, n_hours - 1)),  # Out of range humidity
            'Tair': [-100] + list(np.random.normal(18, 4, n_hours - 1)),  # Invalid temperature
            'pres': [50] + list(np.random.normal(101.3, 2, n_hours - 1)),  # Invalid pressure
            'rain': list(np.random.exponential(0.1, n_hours)),
            'kdown': [-100] + list(np.maximum(0, np.random.normal(300, 100, n_hours - 1)))  # Negative solar
        })
        
        # Step 1: Save problematic data
        input_file = tmp_path / "problematic_data.txt"
        problematic_data.to_csv(input_file, sep=' ', index=False, float_format='%.2f')
        
        # Step 2: Process with preprocessing tools
        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "fixed_data.txt"
        
        result = preprocessor.preprocess_forcing_file(
            file_path=input_file,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=True  # Enable automatic fixing
        )
        
        # Should process despite issues
        assert result.data is not None
        assert output_file.exists()
        
        # Should find and report issues
        assert len(result.issues) > 0
        issue_types = [i.issue_type for i in result.issues]
        
        # Check specific issue types were detected
        assert any('missing_data' in itype for itype in issue_types)
        assert any('out_of_range' in itype for itype in issue_types)
        assert any('invalid' in itype for itype in issue_types)
        
        # Check that fixes were applied
        fixed_data = pd.read_csv(output_file, sep=' ')
        
        # Negative solar radiation should be fixed
        assert all(fixed_data['kdown'] >= 0)
        
        # Negative wind speed should be fixed
        assert all(fixed_data['U'] >= 0)
        
        # Out of range humidity should be clipped
        assert all(fixed_data['RH'] <= 100)
        assert all(fixed_data['RH'] >= 0)
        
        # Processing log should contain fix actions
        assert len(result.processing_log) > 0
        log_text = ' '.join(result.processing_log)
        assert 'fix' in log_text.lower() or 'correct' in log_text.lower()
        
        print(f"✅ Data quality workflow completed:")
        print(f"   • Issues detected: {len(result.issues)}")
        print(f"   • Fixes applied: {len(result.processing_log)}")

    async def test_multi_format_conversion_workflow(self, raw_meteorological_data, tmp_path):
        """Test workflow with multiple format conversions."""
        
        csv_data = raw_meteorological_data['csv_format']
        
        # Step 1: CSV -> TXT conversion
        csv_file = tmp_path / "input_data.csv"
        csv_data.to_csv(csv_file, index=False)
        
        converter = DataFormatConverter()
        txt_file = tmp_path / "intermediate_data.txt"
        
        csv_to_txt_result = converter.convert_format(
            input_path=csv_file,
            output_path=txt_file,
            input_format="csv",
            output_format="txt"
        )
        
        assert csv_to_txt_result.success is True
        assert txt_file.exists()
        
        # Step 2: TXT -> SUEWS format conversion with column mapping
        suews_file = tmp_path / "final_suews_data.txt"
        
        column_mapping = {
            'temperature_celsius': 'Tair',
            'humidity_percent': 'RH', 
            'wind_ms': 'U',
            'pressure_kpa': 'pres',
            'solar_wm2': 'kdown',
            'net_rad_wm2': 'qn',
            'sens_heat_wm2': 'qh',
            'lat_heat_wm2': 'qe',
            'stor_heat_wm2': 'qs',
            'anth_heat_wm2': 'qf',
            'rainfall_mm': 'rain'
        }
        
        txt_to_suews_result = converter.convert_format(
            input_path=txt_file,
            output_path=suews_file,
            input_format="txt",
            output_format="suews_txt",
            column_mapping=column_mapping
        )
        
        assert txt_to_suews_result.success is True
        assert suews_file.exists()
        
        # Step 3: Verify final format
        final_data = pd.read_csv(suews_file, sep=' ')
        suews_columns = ['iy', 'id', 'it', 'imin', 'Tair', 'RH', 'U', 'pres', 
                        'kdown', 'qn', 'qh', 'qe', 'qs', 'qf', 'rain']
        
        for col in suews_columns:
            if col in column_mapping.values() or col in ['iy', 'id', 'it', 'imin']:
                assert col in final_data.columns
        
        print(f"✅ Multi-format conversion workflow completed:")
        print(f"   • CSV: {len(csv_data)} rows")
        print(f"   • TXT: {len(pd.read_csv(txt_file, sep=' '))} rows")
        print(f"   • SUEWS: {len(final_data)} rows")

    async def test_configuration_workflow(self, tmp_path):
        """Test complete configuration creation and validation workflow."""
        
        # Step 1: Create configuration from template
        config_template = {
            'name': 'workflow_test',
            'description': 'Test configuration for workflow validation',
            'model': {
                'control': {
                    'tstep': 3600,
                    'forcing_file': {'value': 'forcing_data.txt'},
                    'start_doy': {'value': 1},
                    'end_doy': {'value': 3},
                    'year': {'value': 2024}
                },
                'physics': {
                    'netradiationmethod': {'value': 3},
                    'roughlenmommethod': {'value': 1},
                    'conductancemethod': {'value': 4},
                    'stabilitymethod': {'value': 3}
                }
            },
            'sites': [{
                'name': 'workflow_test_site',
                'properties': {
                    'lat': {'value': 52.0},
                    'lng': {'value': 0.0},
                    'alt': {'value': 100.0}
                },
                'land_cover': {
                    'paved': {'sfr': {'value': 0.4}},
                    'bldgs': {'sfr': {'value': 0.3}},
                    'grass': {'sfr': {'value': 0.3}}
                }
            }]
        }
        
        # Step 2: Save and validate basic configuration
        config_file = tmp_path / "test_config.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config_template, f)
        
        validator = ConfigValidator()
        basic_validation_result = validator.validate_config(
            config_path=config_file,
            strict_mode=False,
            check_file_paths=False
        )
        
        assert basic_validation_result.success is True
        
        # Step 3: Test strict validation (should find more issues)
        strict_validation_result = validator.validate_config(
            config_path=config_file,
            strict_mode=True,
            check_file_paths=False
        )
        
        # May have warnings but should still be valid
        errors = [i for i in strict_validation_result.issues if i.severity == 'error']
        assert len(errors) == 0  # No critical errors
        
        # Step 4: Test with file path checking
        # Create dummy forcing file
        dummy_forcing = pd.DataFrame({
            'iy': [2024] * 24,
            'id': [1] * 24,
            'it': list(range(24)),
            'imin': [0] * 24,
            'Tair': np.random.normal(15, 3, 24),
            'U': np.random.uniform(2, 8, 24),
            'RH': np.random.uniform(50, 80, 24)
        })
        
        forcing_file = tmp_path / "forcing_data.txt"
        dummy_forcing.to_csv(forcing_file, sep=' ', index=False)
        
        # Update config to point to actual file
        config_template['model']['control']['forcing_file']['value'] = str(forcing_file)
        with open(config_file, 'w') as f:
            yaml.dump(config_template, f)
        
        file_check_result = validator.validate_config(
            config_path=config_file,
            strict_mode=False,
            check_file_paths=True
        )
        
        assert file_check_result.success is True
        
        print(f"✅ Configuration workflow completed:")
        print(f"   • Basic validation: {basic_validation_result.success}")
        print(f"   • Strict validation: {strict_validation_result.success}") 
        print(f"   • File check validation: {file_check_result.success}")

    async def test_error_recovery_workflow(self, tmp_path):
        """Test workflow with error recovery and user guidance."""
        
        # Create severely problematic configuration
        broken_config = {
            'name': 'broken_config',
            'model': {
                'control': {
                    'tstep': -100,  # Invalid timestep
                    'forcing_file': {'value': '/nonexistent/path/file.txt'}
                },
                'physics': {
                    'netradiationmethod': {'value': 99}  # Invalid method
                }
            },
            'sites': [{
                'name': 'broken_site',
                'properties': {
                    'lat': {'value': 200.0},  # Invalid latitude
                    'lng': {'value': 400.0},  # Invalid longitude
                    'alt': {'value': -1000.0}  # Invalid altitude
                },
                'land_cover': {
                    'paved': {'sfr': {'value': 0.8}},  # Fractions don't sum to 1
                    'bldgs': {'sfr': {'value': 0.5}}
                }
            }]
        }
        
        config_file = tmp_path / "broken_config.yml"
        with open(config_file, 'w') as f:
            yaml.dump(broken_config, f)
        
        # Test validation and error reporting
        validator = ConfigValidator()
        result = validator.validate_config(
            config_path=config_file,
            strict_mode=True,
            check_file_paths=True
        )
        
        # Should fail validation
        assert result.success is False
        assert len(result.issues) > 0
        
        # Should have multiple error types
        error_types = {i.issue_type for i in result.issues if i.severity == 'error'}
        assert len(error_types) > 1
        
        # Check specific error types were detected
        all_issue_types = [i.issue_type for i in result.issues]
        assert any('invalid' in itype for itype in all_issue_types)
        assert any('missing' in itype or 'file' in itype for itype in all_issue_types)
        
        # Should provide useful error messages
        error_messages = [i.message for i in result.issues if i.severity == 'error']
        assert len(error_messages) > 0
        
        # Messages should be descriptive
        combined_messages = ' '.join(error_messages).lower()
        assert any(word in combined_messages for word in ['invalid', 'missing', 'range', 'file'])
        
        print(f"✅ Error recovery workflow completed:")
        print(f"   • Total issues: {len(result.issues)}")
        print(f"   • Error types: {len(error_types)}")
        print(f"   • Validation failed as expected: {not result.success}")


class TestWorkflowIntegration:
    """Test integration between different workflow components."""

    async def test_preprocessing_to_simulation_workflow(self, workflow_config, tmp_path):
        """Test workflow from data preprocessing to simulation execution."""
        
        # Step 1: Create and preprocess forcing data
        forcing_data = pd.DataFrame({
            'iy': [2024] * 24,
            'id': [180] * 24,  # Day 180 (summer)
            'it': list(range(24)),
            'imin': [0] * 24,
            'qn': np.maximum(0, 400 * np.sin(np.pi * np.arange(24) / 24) + 
                           np.random.normal(0, 30, 24)),
            'qh': np.maximum(0, 200 * np.sin(np.pi * np.arange(24) / 24) + 
                           np.random.normal(0, 20, 24)),
            'qe': np.maximum(0, 150 * np.sin(np.pi * np.arange(24) / 24) + 
                           np.random.normal(0, 15, 24)),
            'qs': 50 + np.random.normal(0, 10, 24),
            'qf': np.full(24, 25.0) + np.random.normal(0, 5, 24),
            'U': np.maximum(0.5, 4 + np.random.normal(0, 1.5, 24)),
            'RH': np.clip(60 + np.random.normal(0, 10, 24), 30, 95),
            'Tair': 20 + 8 * np.sin(2 * np.pi * np.arange(24) / 24 - np.pi/2) + 
                   np.random.normal(0, 1, 24),
            'pres': 101.3 + np.random.normal(0, 1, 24),
            'rain': np.random.exponential(0.05, 24),
            'kdown': np.maximum(0, 600 * np.sin(np.pi * np.arange(24) / 24) + 
                              np.random.normal(0, 50, 24))
        })
        
        raw_forcing_file = tmp_path / "raw_forcing.txt"
        forcing_data.to_csv(raw_forcing_file, sep=' ', index=False, float_format='%.2f')
        
        # Preprocess the data
        preprocessor = ForcingDataPreprocessor()
        processed_forcing_file = tmp_path / "processed_forcing.txt"
        
        preprocess_result = preprocessor.preprocess_forcing_file(
            file_path=raw_forcing_file,
            output_path=processed_forcing_file,
            validate_energy_balance=True,
            auto_fix_issues=True
        )
        
        assert preprocess_result.success is True
        assert processed_forcing_file.exists()
        
        # Step 2: Create configuration referencing the processed data
        config = {
            'name': 'integration_workflow_test',
            'model': {
                'control': {
                    'tstep': 3600,
                    'forcing_file': {'value': str(processed_forcing_file)},
                    'start_doy': {'value': 180},
                    'end_doy': {'value': 180},
                    'year': {'value': 2024}
                },
                'physics': {
                    'netradiationmethod': {'value': 3},
                    'roughlenmommethod': {'value': 1}
                }
            },
            'sites': [{
                'name': 'integration_site',
                'properties': {
                    'lat': {'value': 51.5},
                    'lng': {'value': -0.1},
                    'alt': {'value': 50.0}
                },
                'land_cover': {
                    'paved': {'sfr': {'value': 0.4}},
                    'bldgs': {'sfr': {'value': 0.3}},
                    'grass': {'sfr': {'value': 0.3}}
                }
            }]
        }
        
        config_file = tmp_path / "integration_config.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        # Step 3: Validate the complete configuration
        validator = ConfigValidator()
        validation_result = validator.validate_config(
            config_path=config_file,
            strict_mode=False,
            check_file_paths=True
        )
        
        assert validation_result.success is True
        
        # Step 4: Test with MCP handlers
        handlers = SUEWSMCPHandlers(workflow_config)
        
        # Mock simulation call
        with patch.object(handlers, '_call_simulation_tool') as mock_simulation:
            mock_simulation.return_value = {
                'content': [{
                    'type': 'text',
                    'text': 'Integration test simulation completed successfully!'
                }]
            }
            
            simulation_args = {
                'config_file': str(config_file),
                'simulation_id': 'integration_workflow_001',
                'output_dir': str(tmp_path / 'output')
            }
            
            simulation_result = await handlers.handle_call_tool(
                'run_suews_simulation', simulation_args
            )
            
            assert simulation_result is not None
            mock_simulation.assert_called_once()
        
        print(f"✅ Preprocessing to simulation workflow completed:")
        print(f"   • Data preprocessing: {preprocess_result.success}")
        print(f"   • Configuration validation: {validation_result.success}")
        print(f"   • Simulation setup: Success")

    async def test_parallel_workflow_processing(self, tmp_path):
        """Test parallel processing of multiple workflow stages."""
        
        # Create multiple datasets for parallel processing
        datasets = []
        for i in range(3):
            data = pd.DataFrame({
                'iy': [2024] * 24,
                'id': [100 + i] * 24,  # Different days
                'it': list(range(24)),
                'imin': [0] * 24,
                'Tair': 15 + i * 5 + np.random.normal(0, 2, 24),
                'U': 3 + i + np.random.uniform(0, 2, 24),
                'RH': 70 - i * 5 + np.random.uniform(-10, 10, 24)
            })
            
            file_path = tmp_path / f"dataset_{i}.txt"
            data.to_csv(file_path, sep=' ', index=False)
            datasets.append(file_path)
        
        # Process datasets in parallel using asyncio
        async def process_dataset(file_path):
            preprocessor = ForcingDataPreprocessor()
            output_path = file_path.parent / f"processed_{file_path.name}"
            
            result = preprocessor.preprocess_forcing_file(
                file_path=file_path,
                output_path=output_path,
                validate_energy_balance=False,  # Faster for parallel test
                auto_fix_issues=True
            )
            
            return result, output_path
        
        # Run parallel processing
        results = await asyncio.gather(
            *[process_dataset(dataset) for dataset in datasets]
        )
        
        # Verify all processing completed successfully
        assert len(results) == 3
        for result, output_path in results:
            assert result.success is True
            assert output_path.exists()
        
        print(f"✅ Parallel workflow processing completed:")
        print(f"   • Datasets processed: {len(results)}")
        print(f"   • All successful: {all(r[0].success for r in results)}")

    async def test_workflow_performance_monitoring(self, tmp_path):
        """Test performance monitoring throughout workflow execution."""
        
        # Create larger dataset to monitor performance
        n_hours = 168  # One week of data
        large_dataset = pd.DataFrame({
            'iy': [2024] * n_hours,
            'id': [(i // 24) + 1 for i in range(n_hours)],
            'it': [i % 24 for i in range(n_hours)],
            'imin': [0] * n_hours,
            'Tair': np.random.normal(15, 8, n_hours),
            'U': np.random.uniform(1, 12, n_hours),
            'RH': np.random.uniform(30, 95, n_hours),
            'pres': np.random.normal(101.3, 3, n_hours),
            'qn': np.random.normal(200, 100, n_hours),
            'qh': np.random.normal(100, 50, n_hours),
            'qe': np.random.normal(80, 40, n_hours),
            'qs': np.random.normal(20, 15, n_hours),
            'qf': np.random.uniform(10, 40, n_hours),
            'rain': np.random.exponential(0.1, n_hours),
            'kdown': np.maximum(0, np.random.normal(300, 150, n_hours))
        })
        
        input_file = tmp_path / "large_dataset.txt"
        large_dataset.to_csv(input_file, sep=' ', index=False, float_format='%.2f')
        
        # Monitor preprocessing performance
        start_time = datetime.now()
        
        preprocessor = ForcingDataPreprocessor()
        output_file = tmp_path / "processed_large_dataset.txt"
        
        result = preprocessor.preprocess_forcing_file(
            file_path=input_file,
            output_path=output_file,
            validate_energy_balance=True,
            auto_fix_issues=True
        )
        
        processing_time = (datetime.now() - start_time).total_seconds()
        
        # Verify performance expectations
        assert result.success is True
        assert processing_time < 30  # Should complete within 30 seconds
        assert len(result.data) == n_hours
        
        # Check memory usage through data size
        processed_data_size = output_file.stat().st_size
        assert processed_data_size > 0
        assert processed_data_size < 50 * 1024 * 1024  # Less than 50MB
        
        print(f"✅ Workflow performance monitoring completed:")
        print(f"   • Dataset size: {n_hours} hours")
        print(f"   • Processing time: {processing_time:.2f} seconds")
        print(f"   • Output size: {processed_data_size / 1024:.1f} KB")
        print(f"   • Performance: {n_hours / processing_time:.1f} hours/second")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])