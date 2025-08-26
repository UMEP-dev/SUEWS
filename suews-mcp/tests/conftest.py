"""Test fixtures for SUEWS MCP Server tests."""

import pytest
import asyncio
import tempfile
import shutil
import os
import sys
from unittest.mock import Mock, AsyncMock, MagicMock
from typing import Dict, Any, List, Optional
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from suews_mcp.config import MCPServerConfig
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.server import SUEWSMCPServer


@pytest.fixture
def test_config():
    """Create a test configuration for SUEWS MCP server."""
    return MCPServerConfig(
        server_name="test-suews-mcp",
        server_version="0.1.0-test",
        log_level="DEBUG",
        enable_debug=True,
        suews_timeout=60,  # Shorter timeout for tests
        max_concurrent_simulations=2,  # Lower limit for tests
        enable_simulation_tool=True,
        enable_validation_tool=True,
        enable_analysis_tool=True,
        max_memory_mb=512,  # Lower memory limit for tests
        max_simulation_time_hours=1.0,  # Shorter time limit for tests
    )


@pytest.fixture
def minimal_config():
    """Create a minimal test configuration with reduced features."""
    return MCPServerConfig(
        server_name="minimal-test-server",
        server_version="0.1.0-test",
        log_level="WARNING",
        enable_debug=False,
        enable_simulation_tool=False,
        enable_validation_tool=True,
        enable_analysis_tool=False,
        max_concurrent_simulations=1,
    )


@pytest.fixture
def temp_directory():
    """Create a temporary directory for test files."""
    temp_dir = tempfile.mkdtemp(prefix="suews_mcp_test_")
    yield temp_dir
    # Cleanup
    shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def test_config_file(temp_directory):
    """Create a test SUEWS configuration file."""
    config_content = """
# Test SUEWS Configuration
site:
  lat: 51.5074
  lon: -0.1278
  alt: 50.0
  id: "test_site"

time:
  start: "2024-01-01 00:00"
  end: "2024-01-02 00:00"
  resolution: 3600

output:
  format: "txt"
  freq: 3600
"""
    config_file = os.path.join(temp_directory, "test_config.yml")
    with open(config_file, "w") as f:
        f.write(config_content)
    return config_file


@pytest.fixture
def test_forcing_file(temp_directory):
    """Create a test forcing data file."""
    forcing_data = """iy  id  it  imin  qn  qh  qe  qs  qf  wind  rh  ta  pa  rain  kd  snow  ldown  fcld  wuh  xsmd  lai_hp  lai_dp  albedo_hp  albedo_dp
2024  1  1  0  100.0  50.0  40.0  10.0  20.0  3.5  65.0  15.2  101325.0  0.0  250.0  0.0  300.0  0.3  5.0  0.2  2.1  1.8  0.15  0.12
2024  1  1  60  95.0  48.0  38.0  9.0  19.0  3.2  67.0  15.0  101320.0  0.1  240.0  0.0  295.0  0.4  4.8  0.2  2.1  1.8  0.15  0.12
"""
    forcing_file = os.path.join(temp_directory, "test_forcing.txt")
    with open(forcing_file, "w") as f:
        f.write(forcing_data)
    return forcing_file


@pytest.fixture
def test_output_file(temp_directory):
    """Create a test SUEWS output file."""
    output_data = """datetime,QN,QH,QE,QS,QF,T2,RH2,U10
2024-01-01T00:00,100.0,50.0,40.0,10.0,20.0,15.2,65.0,3.5
2024-01-01T01:00,95.0,48.0,38.0,9.0,19.0,15.0,67.0,3.2
2024-01-01T02:00,90.0,45.0,35.0,8.0,18.0,14.8,68.0,3.0
"""
    output_file = os.path.join(temp_directory, "test_output.csv")
    with open(output_file, "w") as f:
        f.write(output_data)
    return output_file


@pytest.fixture
def mock_mcp_client():
    """Create a mock MCP client for testing server interactions."""
    client = Mock()
    client.read_stream = AsyncMock()
    client.write_stream = AsyncMock()

    # Mock client methods
    client.initialize = AsyncMock(return_value={"protocol_version": "2024-11-05"})
    client.list_tools = AsyncMock(return_value={"tools": []})
    client.call_tool = AsyncMock(
        return_value={"content": [{"type": "text", "text": "mock result"}]}
    )
    client.list_prompts = AsyncMock(return_value={"prompts": []})
    client.get_prompt = AsyncMock(return_value={"messages": []})

    return client


@pytest.fixture
async def handlers(test_config):
    """Create SUEWS MCP handlers instance for testing."""
    return SUEWSMCPHandlers(test_config)


@pytest.fixture
async def minimal_handlers(minimal_config):
    """Create minimal SUEWS MCP handlers instance for testing."""
    return SUEWSMCPHandlers(minimal_config)


@pytest.fixture
async def server(test_config):
    """Create SUEWS MCP server instance for testing."""
    return SUEWSMCPServer(test_config)


@pytest.fixture
async def minimal_server(minimal_config):
    """Create minimal SUEWS MCP server instance for testing."""
    return SUEWSMCPServer(minimal_config)


class MockStreamPair:
    """Mock stream pair for stdio testing."""

    def __init__(self):
        self.read_stream = AsyncMock()
        self.write_stream = AsyncMock()
        self.read_data = []
        self.write_data = []

    async def __aenter__(self):
        return self.read_stream, self.write_stream

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        pass


@pytest.fixture
def mock_stdio_server():
    """Mock stdio_server for testing without actual stdio."""
    return MockStreamPair()


@pytest.fixture
def mock_supy_module():
    """Mock the supy module for testing without actual SUEWS installation."""
    mock_supy = Mock()

    # Mock common supy functions
    mock_supy.load_SampleData = Mock(
        return_value={"config": {"site": {"lat": 51.5, "lon": -0.1}}, "forcing": Mock()}
    )

    mock_supy.run_supy = AsyncMock(
        return_value={
            "output": Mock(),
            "state": Mock(),
            "metadata": {"simulation_time": "00:00:10"},
        }
    )

    mock_supy.validate_config = Mock(return_value={"valid": True, "errors": []})

    mock_supy.analyze_output = Mock(
        return_value={
            "statistics": {"mean_QH": 45.0, "mean_QE": 35.0, "mean_QN": 95.0},
            "time_series": {"QH": [50, 48, 45], "QE": [40, 38, 35]},
        }
    )

    return mock_supy


@pytest.fixture(autouse=True)
def capture_logs(caplog):
    """Automatically capture logs for all tests."""
    caplog.set_level("DEBUG")
    return caplog


@pytest.fixture
def sample_initialize_params():
    """Sample initialization parameters for MCP."""
    return {
        "protocol_version": "2024-11-05",
        "capabilities": {"roots": {"list_changed": False}, "sampling": {}},
        "client_info": {"name": "test-client", "version": "1.0.0"},
    }


@pytest.fixture
def sample_tool_arguments():
    """Sample tool arguments for testing different tools."""
    return {
        "run_suews_simulation": {
            "config_file": "/path/to/config.yml",
            "simulation_id": "test_sim_001",
            "output_dir": "/path/to/output",
        },
        "validate_suews_config": {"config_file": "/path/to/config.yml", "strict": True},
        "analyze_suews_output": {
            "output_file": "/path/to/output.csv",
            "metrics": ["QH", "QE", "QN"],
            "time_period": "daily",
        },
        "health_check": {},
    }


@pytest.fixture
def invalid_tool_arguments():
    """Invalid tool arguments for error testing."""
    return {
        "run_suews_simulation": {
            # Missing required config_file
            "simulation_id": "test_sim_001"
        },
        "validate_suews_config": {
            # Missing required config_file
            "strict": True
        },
        "analyze_suews_output": {
            # Missing required output_file
            "metrics": ["QH", "QE"]
        },
    }


@pytest.fixture
def cleanup_environment():
    """Clean up environment variables after tests."""
    original_env = os.environ.copy()
    yield
    # Restore original environment
    os.environ.clear()
    os.environ.update(original_env)


@pytest.fixture(scope="session")
def event_loop():
    """Create event loop for async tests."""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


class AsyncContextManager:
    """Helper class for testing async context managers."""

    def __init__(self, value):
        self.value = value

    async def __aenter__(self):
        return self.value

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        return False


@pytest.fixture
def async_context_manager():
    """Factory for creating async context managers in tests."""
    return AsyncContextManager


# Additional fixtures for e2e and performance tests

@pytest.fixture
def large_forcing_dataset():
    """Generate large forcing dataset for performance testing."""
    def generate_dataset(n_hours: int = 8760, n_variables: int = 15) -> pd.DataFrame:
        """Generate realistic large forcing dataset."""
        import pandas as pd
        import numpy as np
        
        np.random.seed(42)  # Reproducible data
        base_date = pd.Timestamp("2024-01-01")
        hours = np.arange(n_hours)
        
        # Efficient vectorized generation of realistic patterns
        solar_cycle = np.maximum(
            0, 
            1000 * np.sin(np.pi * (hours % 24) / 24) * 
            np.maximum(0, np.cos(np.pi * ((hours % 24) - 12) / 12))
        )
        
        temp_base = 15 + 10 * np.sin(2 * np.pi * (hours % 24) / 24 - np.pi/2)
        temp_seasonal = 5 * np.sin(2 * np.pi * hours / (365 * 24))
        
        data = {
            'iy': [2024] * n_hours,
            'id': [((base_date + pd.Timedelta(hours=h)).dayofyear) for h in hours],
            'it': [h % 24 for h in hours],
            'imin': [0] * n_hours,
            'qn': solar_cycle * 0.6 + np.random.normal(0, 20, n_hours),
            'qh': solar_cycle * 0.3 + np.random.normal(0, 15, n_hours),
            'qe': solar_cycle * 0.25 + np.random.normal(0, 12, n_hours),
            'qs': solar_cycle * 0.15 + np.random.normal(0, 8, n_hours),
            'qf': np.full(n_hours, 25.0) + np.random.normal(0, 3, n_hours),
            'U': np.maximum(0.5, 4.0 + np.random.normal(0, 1.8, n_hours)),
            'RH': np.clip(70 + np.random.normal(0, 12, n_hours), 30, 95),
            'Tair': temp_base + temp_seasonal + np.random.normal(0, 1.2, n_hours),
            'pres': 101.325 + np.random.normal(0, 0.6, n_hours),
            'rain': np.random.exponential(0.08, n_hours),
            'kdown': solar_cycle + np.random.normal(0, 40, n_hours),
        }
        
        # Add additional variables if requested
        extra_vars = ['ldown', 'fcld', 'wuh', 'xsmd', 'lai_hp', 'lai_dp', 'albedo_hp', 'albedo_dp']
        for i, var in enumerate(extra_vars[:max(0, n_variables - len(data))]):
            if var in ['ldown']:
                data[var] = 320 + 30 * np.sin(2 * np.pi * (hours % 24) / 24) + np.random.normal(0, 15, n_hours)
            elif var in ['fcld']:
                data[var] = np.clip(np.random.beta(2, 5, n_hours), 0, 1)
            elif var in ['lai_hp', 'lai_dp']:
                seasonal_lai = 2.0 + 2.0 * np.sin(2 * np.pi * hours / (365 * 24) + np.pi/2)
                data[var] = np.maximum(0.5, seasonal_lai + np.random.normal(0, 0.3, n_hours))
            else:
                data[var] = np.random.normal(0.5, 0.2, n_hours)
        
        return pd.DataFrame(data)
    
    return generate_dataset


@pytest.fixture
def comprehensive_suews_config():
    """Generate comprehensive SUEWS configuration template."""
    def create_config(
        name: str = "test_config",
        n_days: int = 7,
        n_sites: int = 1,
        complexity: str = "medium"
    ) -> Dict[str, Any]:
        """Create SUEWS configuration with specified parameters."""
        
        base_config = {
            'name': name,
            'description': f'Generated test configuration ({complexity} complexity)',
            'model': {
                'control': {
                    'tstep': 3600,  # 1-hour timestep
                    'forcing_file': {'value': 'forcing_data.txt'},
                    'start_doy': {'value': 1},
                    'end_doy': {'value': n_days},
                    'year': {'value': 2024},
                    'resolutionfilesout': {'value': 3600}
                },
                'physics': {
                    'netradiationmethod': {'value': 3},
                    'roughlenmommethod': {'value': 2},
                    'conductancemethod': {'value': 4},
                    'stabilitymethod': {'value': 3},
                    'storedrainprm': {'value': 2},
                    'anthropogenic': {'value': 1},
                    'snowuse': {'value': 1}
                },
                'output': {
                    'writefiles': {'value': 1},
                    'files_text': {'value': 1},
                    'files_rdf': {'value': 0}
                }
            },
            'sites': []
        }
        
        # Add sites
        for i in range(n_sites):
            site = {
                'name': f'{name}_site_{i+1}',
                'properties': {
                    'lat': {'value': 51.5074 + i * 0.01},
                    'lng': {'value': -0.1278 - i * 0.01},
                    'alt': {'value': 50.0 + i * 10},
                    'timezone': {'value': 0}
                },
                'land_cover': {
                    'paved': {
                        'sfr': {'value': 0.30 + i * 0.02},
                        'emis': {'value': 0.95},
                        'roughlenmmom': {'value': 0.1},
                        'storemin': {'value': 0.5},
                        'storemax': {'value': 2.0}
                    },
                    'bldgs': {
                        'sfr': {'value': 0.35},
                        'emis': {'value': 0.92},
                        'roughlenmmom': {'value': 1.0},
                        'storemin': {'value': 0.5},
                        'storemax': {'value': 2.5}
                    },
                    'grass': {
                        'sfr': {'value': 0.35 - i * 0.02},
                        'emis': {'value': 0.96},
                        'roughlenmmom': {'value': 0.05},
                        'storemin': {'value': 0.5},
                        'storemax': {'value': 3.0}
                    }
                },
                'initial_conditions': {
                    'soilstore_id': {'value': 150.0 + i * 25},
                    'soilstore_surf': {'value': 15.0 + i * 5}
                }
            }
            
            # Add more complex land cover for high complexity
            if complexity == "high":
                site['land_cover'].update({
                    'evetr': {
                        'sfr': {'value': 0.05},
                        'emis': {'value': 0.97},
                        'roughlenmmom': {'value': 2.0},
                        'lai': {'value': 4.5}
                    },
                    'dectr': {
                        'sfr': {'value': 0.08},
                        'emis': {'value': 0.96},
                        'roughlenmmom': {'value': 1.5},
                        'lai': {'value': 2.5}
                    }
                })
                
                site['anthropogenic'] = {
                    'qf_a': {'value': 25.0 + i * 5},
                    'qf_b': {'value': 0.5 + i * 0.1},
                    'qf_c': {'value': 0.0}
                }
            
            base_config['sites'].append(site)
        
        return base_config
    
    return create_config


@pytest.fixture
def performance_timers():
    """Provide timing utilities for performance tests."""
    class PerformanceTimer:
        def __init__(self):
            self.timings = {}
            self.active_timers = {}
        
        def start(self, name: str):
            """Start timing an operation."""
            import time
            self.active_timers[name] = time.perf_counter()
        
        def stop(self, name: str) -> float:
            """Stop timing and return duration."""
            import time
            if name not in self.active_timers:
                return 0.0
            
            duration = time.perf_counter() - self.active_timers[name]
            del self.active_timers[name]
            
            if name not in self.timings:
                self.timings[name] = []
            self.timings[name].append(duration)
            
            return duration
        
        def get_stats(self, name: str) -> Dict[str, float]:
            """Get timing statistics."""
            if name not in self.timings or not self.timings[name]:
                return {}
            
            import statistics
            times = self.timings[name]
            return {
                'count': len(times),
                'total': sum(times),
                'mean': statistics.mean(times),
                'min': min(times),
                'max': max(times),
                'std': statistics.stdev(times) if len(times) > 1 else 0.0
            }
        
        def reset(self):
            """Reset all timings."""
            self.timings.clear()
            self.active_timers.clear()
    
    return PerformanceTimer()


@pytest.fixture
def mock_large_simulation_results():
    """Generate mock results for large simulation testing."""
    def create_mock_results(n_hours: int = 8760, n_sites: int = 1) -> Dict[str, Any]:
        """Create realistic mock simulation results."""
        import pandas as pd
        import numpy as np
        
        # Generate output data
        output_data = pd.DataFrame({
            'datetime': pd.date_range('2024-01-01', periods=n_hours, freq='h'),
            'site_id': np.tile(range(1, n_sites + 1), n_hours // n_sites + 1)[:n_hours],
            'QN': np.random.normal(100, 50, n_hours),
            'QH': np.random.normal(50, 25, n_hours),
            'QE': np.random.normal(40, 20, n_hours),
            'QS': np.random.normal(20, 15, n_hours),
            'QF': np.full(n_hours, 25.0) + np.random.normal(0, 5, n_hours),
            'T2': np.random.normal(15, 8, n_hours),
            'RH2': np.clip(np.random.normal(70, 15, n_hours), 10, 100),
            'U10': np.maximum(0.1, np.random.normal(4, 2, n_hours))
        })
        
        # Generate state data
        state_vars = ['soilstore_id', 'soilstore_surf', 'state_id']
        state_data = pd.DataFrame({
            var: np.random.normal(100, 20, n_sites) for var in state_vars
        })
        
        return {
            'output': output_data,
            'state': state_data,
            'metadata': {
                'simulation_time': f'00:{n_hours//60:02d}:{n_hours%60:02d}',
                'total_timesteps': n_hours,
                'sites': n_sites,
                'start_time': '2024-01-01 00:00:00',
                'end_time': output_data['datetime'].iloc[-1].strftime('%Y-%m-%d %H:%M:%S'),
                'version': 'test-version',
                'config_hash': f'test_hash_{n_hours}_{n_sites}'
            }
        }
    
    return create_mock_results


@pytest.fixture
def workflow_test_data(tmp_path, large_forcing_dataset, comprehensive_suews_config):
    """Prepare complete test data for workflow testing."""
    def create_workflow_data(
        scenario: str = "standard",
        duration_days: int = 7,
        n_sites: int = 1
    ) -> Dict[str, Any]:
        """Create complete workflow test data."""
        
        # Generate forcing data
        n_hours = duration_days * 24
        forcing_data = large_forcing_dataset(n_hours, 15)
        
        # Save forcing data
        forcing_file = tmp_path / f"forcing_{scenario}.txt"
        forcing_data.to_csv(forcing_file, sep=' ', index=False, float_format='%.2f')
        
        # Generate configuration
        config_complexity = "high" if scenario == "complex" else "medium"
        config_data = comprehensive_suews_config(
            name=f"{scenario}_workflow_test",
            n_days=duration_days,
            n_sites=n_sites,
            complexity=config_complexity
        )
        
        # Update config to point to forcing file
        config_data['model']['control']['forcing_file'] = {'value': forcing_file.name}
        
        # Save configuration
        import yaml
        config_file = tmp_path / f"config_{scenario}.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config_data, f, default_flow_style=False)
        
        # Create output directory
        output_dir = tmp_path / f"output_{scenario}"
        output_dir.mkdir(exist_ok=True)
        
        return {
            'scenario': scenario,
            'forcing_file': forcing_file,
            'config_file': config_file,
            'output_dir': output_dir,
            'forcing_data': forcing_data,
            'config_data': config_data,
            'n_hours': n_hours,
            'n_sites': n_sites,
            'duration_days': duration_days
        }
    
    return create_workflow_data
