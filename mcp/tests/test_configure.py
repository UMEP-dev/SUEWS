"""Tests for MCP configuration tools."""

import pytest
import tempfile
from pathlib import Path
import yaml

from suews_mcp.tools.configure import (
    validate_config,
    create_config,
    get_config_info,
    update_config,
)


@pytest.fixture
def sample_config_path():
    """Path to the sample configuration file."""
    return "src/supy/sample_data/sample_config.yml"


@pytest.fixture
def temp_dir():
    """Temporary directory for test outputs."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.mark.asyncio
async def test_validate_config_valid(sample_config_path):
    """Test validating a valid configuration file."""
    result = await validate_config(sample_config_path)

    assert result["valid"] is True
    assert "sample_config" in result["message"]
    assert result["config"]["name"] == "sample_config"
    assert result["config"]["num_sites"] == 1


@pytest.mark.asyncio
async def test_validate_config_nonexistent():
    """Test validating a non-existent file."""
    result = await validate_config("nonexistent.yml")

    assert result["valid"] is False
    assert "error" in result


@pytest.mark.asyncio
async def test_validate_config_invalid_yaml(temp_dir):
    """Test validating an invalid YAML file."""
    invalid_file = temp_dir / "invalid.yml"
    invalid_file.write_text("invalid: yaml: content: [")

    result = await validate_config(str(invalid_file))

    assert result["valid"] is False
    assert "error" in result


@pytest.mark.asyncio
async def test_create_config_minimal(temp_dir):
    """Test creating a minimal configuration (requires template since sites are mandatory)."""
    output_path = temp_dir / "new_config.yml"

    # Note: SUEWSConfig requires at least one site, so minimal config without
    # template will fail validation. This is expected behavior.
    result = await create_config(
        name="test_config",
        description="Test configuration",
        output_path=str(output_path),
    )

    assert result["success"] is False
    assert "error" in result
    assert "sites" in result["error"].lower()


@pytest.mark.asyncio
async def test_create_config_from_template(sample_config_path, temp_dir):
    """Test creating a configuration from a template."""
    output_path = temp_dir / "templated_config.yml"

    result = await create_config(
        name="templated_config",
        description="Config from template",
        output_path=str(output_path),
        template=sample_config_path,
    )

    assert result["success"] is True
    assert output_path.exists()
    assert result["config"]["name"] == "templated_config"

    # Verify file was created and can be validated
    validation_result = await validate_config(str(output_path))
    assert validation_result["valid"] is True
    assert validation_result["config"]["name"] == "templated_config"


@pytest.mark.asyncio
async def test_get_config_info(sample_config_path):
    """Test getting configuration information."""
    result = await get_config_info(sample_config_path)

    assert "error" not in result
    assert result["name"] == "sample_config"
    assert result["num_sites"] == 1
    assert "site_names" in result
    assert len(result["site_names"]) == 1


@pytest.mark.asyncio
async def test_get_config_info_nonexistent():
    """Test getting info for non-existent file."""
    result = await get_config_info("nonexistent.yml")

    assert "error" in result


@pytest.mark.asyncio
async def test_update_config(sample_config_path, temp_dir):
    """Test updating a configuration file."""
    # Copy sample config to temp directory
    import shutil
    temp_config = temp_dir / "config_to_update.yml"
    shutil.copy(sample_config_path, temp_config)

    # Update the description
    result = await update_config(
        config_path=str(temp_config),
        updates={"description": "Updated description"},
    )

    assert result["success"] is True

    # Verify the update by getting config info
    info_result = await get_config_info(str(temp_config))
    assert "error" not in info_result
    assert info_result["description"] == "Updated description"


@pytest.mark.asyncio
async def test_update_config_nonexistent():
    """Test updating a non-existent file."""
    result = await update_config(
        config_path="nonexistent.yml",
        updates={"description": "New description"},
    )

    assert result["success"] is False
    assert "error" in result
