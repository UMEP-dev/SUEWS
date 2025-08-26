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
