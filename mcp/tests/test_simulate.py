"""Tests for MCP simulation tools."""

import pytest
from unittest.mock import patch, MagicMock

from suews_mcp.tools.simulate import (
    estimate_runtime,
    run_simulation,
)


@pytest.fixture
def sample_config_path():
    """Path to the sample configuration file."""
    return "src/supy/sample_data/sample_config.yml"


@pytest.mark.asyncio
async def test_estimate_runtime(sample_config_path):
    """Test runtime estimation."""
    result = await estimate_runtime(sample_config_path)

    assert "error" not in result
    assert "num_sites" in result
    assert result["num_sites"] == 1
    assert "estimated_runtime_seconds" in result
    assert "estimated_runtime_minutes" in result
    assert result["estimated_runtime_seconds"] > 0


@pytest.mark.asyncio
async def test_estimate_runtime_nonexistent():
    """Test runtime estimation for non-existent file."""
    result = await estimate_runtime("nonexistent.yml")

    assert "error" in result


@pytest.mark.asyncio
@patch('supy.run_supy')
async def test_run_simulation(mock_run_supy, sample_config_path, tmp_path):
    """Test running a simulation (mocked)."""
    # Mock the run_supy function from supy module
    mock_results = MagicMock()
    mock_run_supy.return_value = mock_results

    result = await run_simulation(
        config_path=sample_config_path,
        output_dir=str(tmp_path),
    )

    assert result["success"] is True
    assert result["config"] == "sample_config"
    assert "output_dir" in result


@pytest.mark.asyncio
async def test_run_simulation_nonexistent():
    """Test running simulation with non-existent config."""
    result = await run_simulation(config_path="nonexistent.yml")

    assert result["success"] is False
    assert "error" in result
