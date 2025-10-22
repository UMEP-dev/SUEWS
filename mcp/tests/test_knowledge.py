"""Tests for MCP knowledge tools."""

import pytest
from suews_mcp.tools.knowledge import (
    get_config_schema,
    get_config_docs,
    list_available_models,
    get_variable_info,
    list_physics_schemes,
    get_physics_implementation,
    get_forcing_format_guide,
)


def test_get_config_schema():
    """Test getting configuration schema overview."""
    result = get_config_schema()

    assert result["success"] is True
    assert "schema_overview" in result
    assert "navigation_guide" in result
    assert "guidance" in result
    assert "schema_url" in result
    # Should NOT return full schema (too large)
    assert "schema" not in result


def test_get_config_docs_valid():
    """Test getting configuration documentation for a valid model."""
    result = get_config_docs("Site")

    assert result["success"] is True
    assert result["config_name"] == "Site"
    assert "documentation" in result
    assert "guidance" in result


def test_get_config_docs_invalid():
    """Test getting configuration documentation for an invalid model."""
    result = get_config_docs("NonExistentModel")

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
    assert "concept" in result  # Energy balance concept for flux variables


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
    assert "size_info" in result
    assert "guidance" in result

    # Check source code contains expected content
    assert len(result["source_code"]) > 0
    assert result["size_info"]["total_lines"] > 0


def test_get_physics_implementation_invalid():
    """Test getting physics implementation for an invalid scheme."""
    result = get_physics_implementation("NonExistentScheme")

    assert result["success"] is False
    assert "error" in result
    assert "available_schemes" in result


def test_get_forcing_format_guide_basic():
    """Test getting forcing format guide without parameters."""
    result = get_forcing_format_guide()

    assert result["success"] is True
    assert "format_specification" in result
    assert "required_variables" in result
    assert "unit_conversions" in result
    assert "conversion_template" in result
    assert "validation_script" in result
    assert "guidance" in result

    # Check format specification
    format_spec = result["format_specification"]
    assert format_spec["file_type"] == "Space-separated text file (.txt)"
    assert format_spec["delimiter"] == "Single space character"
    assert len(format_spec["column_order"]) == 12  # 12 required columns

    # Check required variables
    required_vars = result["required_variables"]
    assert "kdown" in required_vars
    assert "Tair" in required_vars
    assert "wspeed" in required_vars
    assert required_vars["wspeed"]["special"] is not None  # Wind speed has special warning

    # Check conversion template
    template = result["conversion_template"]
    assert template["language"] == "Python"
    assert "import pandas" in template["code"]
    assert "clip(lower=0.001)" in template["code"]  # Wind speed clipping

    # Check validation script
    validation = result["validation_script"]
    assert validation["language"] == "Python"
    assert "def validate_forcing" in validation["code"]


def test_get_forcing_format_guide_with_source_format():
    """Test getting forcing format guide with source format hint."""
    result = get_forcing_format_guide(source_format="csv")

    assert result["success"] is True
    assert "context" in result
    assert result["context"]["source_format"] == "csv"


def test_get_forcing_format_guide_with_available_variables():
    """Test getting forcing format guide with available variables."""
    # Provide SUEWS variable names (what user would check against the format guide)
    available_vars = ["Tair", "RH", "wspeed", "pres"]
    result = get_forcing_format_guide(variables_available=available_vars)

    assert result["success"] is True
    assert "context" in result
    assert result["context"]["available_variables"] == available_vars
    assert "missing_variables" in result["context"]  # Should detect missing required vars
    assert "kdown" in result["context"]["missing_variables"]  # Radiation missing
    assert "wspeed" not in result["context"]["missing_variables"]  # wspeed was provided
    assert "Tair" not in result["context"]["missing_variables"]  # Tair was provided


def test_get_forcing_format_guide_unit_conversions():
    """Test that unit conversion formulas are provided."""
    result = get_forcing_format_guide()

    assert result["success"] is True
    conversions = result["unit_conversions"]

    # Check temperature conversions
    assert "temperature" in conversions
    assert "Kelvin_to_Celsius" in conversions["temperature"]

    # Check pressure conversions
    assert "pressure" in conversions
    assert "Pa_to_kPa" in conversions["pressure"]

    # Check wind conversions
    assert "wind" in conversions
    assert "u_v_to_speed" in conversions["wind"]
    assert "u_v_to_direction" in conversions["wind"]


def test_get_forcing_format_guide_required_variable_details():
    """Test that required variables have complete specifications."""
    result = get_forcing_format_guide()

    assert result["success"] is True
    required_vars = result["required_variables"]

    # Test a few key variables have all required fields
    for var_name in ["kdown", "Tair", "RH", "wspeed"]:
        var = required_vars[var_name]
        assert "name" in var
        assert "unit" in var
        assert "description" in var
        assert "required" in var
        assert "typical_range" in var

    # Wind speed should have special note
    assert "special" in required_vars["wspeed"]
    assert "0.001" in required_vars["wspeed"]["special"]


def test_get_forcing_format_guide_common_sources():
    """Test that common data sources are documented."""
    result = get_forcing_format_guide()

    assert result["success"] is True
    sources = result["common_sources"]

    # Check common sources are documented
    assert "weather_stations" in sources
    assert "reanalysis" in sources
    assert "mesoscale_models" in sources

    # ERA5 should mention the tool
    assert "get_era5_forcing" in sources["reanalysis"]["tool_available"]


def test_get_forcing_format_guide_guidance():
    """Test that practical guidance is provided."""
    result = get_forcing_format_guide()

    assert result["success"] is True
    guidance = result["guidance"]

    # Check priority checks are listed
    assert "priority_checks" in guidance
    assert len(guidance["priority_checks"]) > 0
    assert any("Wind speed" in check for check in guidance["priority_checks"])

    # Check alternative tools are mentioned
    assert "alternative_tools" in guidance
    assert "era5_retrieval" in guidance["alternative_tools"]
    assert "validation" in guidance["alternative_tools"]
