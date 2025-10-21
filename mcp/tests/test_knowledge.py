"""Tests for MCP knowledge tools."""

import pytest
from suews_mcp.tools.knowledge import (
    get_config_schema,
    get_model_docs,
    list_available_models,
    get_variable_info,
    list_physics_schemes,
    get_physics_implementation,
)


def test_get_config_schema():
    """Test getting configuration schema."""
    result = get_config_schema()

    assert result["success"] is True
    assert "schema" in result
    assert "$defs" in result["schema"] or "definitions" in result["schema"]
    assert "guidance" in result
    assert "schema_url" in result


def test_get_model_docs_valid():
    """Test getting documentation for a valid model."""
    result = get_model_docs("Site")

    assert result["success"] is True
    assert result["model_name"] == "Site"
    assert "documentation" in result
    assert "guidance" in result


def test_get_model_docs_invalid():
    """Test getting documentation for an invalid model."""
    result = get_model_docs("NonExistentModel")

    assert result["success"] is False
    assert "error" in result
    assert "available_models" in result


def test_list_available_models():
    """Test listing all available models."""
    result = list_available_models()

    assert result["success"] is True
    assert "models" in result
    assert "count" in result
    assert result["count"] > 0
    assert "guidance" in result

    # Check some expected models exist
    assert "Site" in result["models"]
    assert "SurfaceProperties" in result["models"]
    assert "SUEWSConfig" in result["models"]


def test_get_variable_info_specific():
    """Test getting info for a specific variable."""
    result = get_variable_info("QH")

    assert result["success"] is True
    assert result["variable"] == "QH"
    assert "info" in result
    assert result["info"]["name"] == "Sensible Heat Flux"
    assert result["info"]["units"] == "W/m²"
    assert result["info"]["type"] == "energy_flux"
    assert "energy_balance_note" in result


def test_get_variable_info_all():
    """Test getting info for all variables."""
    result = get_variable_info()

    assert result["success"] is True
    assert "variables" in result
    assert "count" in result
    assert result["count"] > 0
    assert "guidance" in result

    # Check some expected variables
    assert "QH" in result["variables"]
    assert "QE" in result["variables"]
    assert "QS" in result["variables"]


def test_get_variable_info_invalid():
    """Test getting info for an invalid variable."""
    result = get_variable_info("InvalidVariable")

    assert result["success"] is False
    assert "error" in result
    assert "available_variables" in result


def test_list_physics_schemes():
    """Test listing physics schemes."""
    result = list_physics_schemes()

    assert result["success"] is True
    assert "schemes" in result
    assert "count" in result
    assert result["count"] > 0
    assert "guidance" in result

    # Check expected schemes
    assert "OHM" in result["schemes"]
    assert "water_balance" in result["schemes"]
    assert "evaporation" in result["schemes"]

    # Check scheme has expected fields
    ohm_scheme = result["schemes"]["OHM"]
    assert "name" in ohm_scheme
    assert "purpose" in ohm_scheme
    assert "file" in ohm_scheme
    assert "description" in ohm_scheme


def test_get_physics_implementation_valid():
    """Test getting physics implementation for a valid scheme."""
    result = get_physics_implementation("OHM")

    assert result["success"] is True
    assert result["scheme"] == "OHM"
    assert "source_code" in result
    assert "subroutines" in result
    assert "line_count" in result
    assert "guidance" in result

    # Check source code contains expected content
    assert len(result["source_code"]) > 0
    assert result["line_count"] > 0


def test_get_physics_implementation_invalid():
    """Test getting physics implementation for an invalid scheme."""
    result = get_physics_implementation("NonExistentScheme")

    assert result["success"] is False
    assert "error" in result
    assert "available_schemes" in result
