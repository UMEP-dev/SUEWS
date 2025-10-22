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
    # Try multiple paths to find the sample config
    import os
    candidates = [
        "src/supy/sample_data/sample_config.yml",  # From root
        "../src/supy/sample_data/sample_config.yml",  # From mcp/
    ]

    # Also try installed version
    try:
        import supy
        pkg_path = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
        if pkg_path.exists():
            return str(pkg_path)
    except ImportError:
        pass

    for candidate in candidates:
        if os.path.exists(candidate):
            return candidate

    # If none found, return the first candidate (will fail with clear error)
    return candidates[0]


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


@pytest.mark.asyncio
async def test_update_config_nested(sample_config_path, temp_dir):
    """Test nested configuration updates.

    Verifies that the recursive update functionality works correctly
    for nested configuration structures, matching the implementation
    in SUEWSSimulation._update_config_from_dict.
    """
    import shutil

    # Copy sample config to temp directory
    temp_config = temp_dir / "config_nested_update.yml"
    shutil.copy(sample_config_path, temp_config)

    # Test nested update: model.control.tstep
    result = await update_config(
        config_path=str(temp_config),
        updates={
            "model": {
                "control": {
                    "tstep": 600  # Change from 300 to 600
                }
            }
        },
    )

    assert result["success"] is True
    assert "updates_applied" in result

    # Verify the nested update by loading the config
    with open(temp_config) as f:
        updated_config = yaml.safe_load(f)

    assert updated_config["model"]["control"]["tstep"] == 600

    # Verify other fields are unchanged (this ensures we didn't replace the entire model dict)
    # The sample config should still have other control fields intact
    assert "start_time" in updated_config["model"]["control"]
    assert "end_time" in updated_config["model"]["control"]
    # Verify physics section is still there (wasn't replaced by control update)
    assert "physics" in updated_config["model"]


@pytest.mark.asyncio
async def test_update_config_nested_multiple_levels(sample_config_path, temp_dir):
    """Test deeply nested configuration updates at multiple levels."""
    import shutil

    # Copy sample config to temp directory
    temp_config = temp_dir / "config_multi_nested_update.yml"
    shutil.copy(sample_config_path, temp_config)

    # Test updating multiple nested fields simultaneously
    result = await update_config(
        config_path=str(temp_config),
        updates={
            "description": "Updated top level",
            "model": {
                "control": {
                    "tstep": 600
                }
            }
        },
    )

    assert result["success"] is True

    # Verify both updates applied
    with open(temp_config) as f:
        updated_config = yaml.safe_load(f)

    assert updated_config["description"] == "Updated top level"
    assert updated_config["model"]["control"]["tstep"] == 600
    # Verify other model sections still exist
    assert "physics" in updated_config["model"]
