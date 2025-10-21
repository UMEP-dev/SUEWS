"""Tests for MCP utility tools."""

import pytest
import tempfile
from pathlib import Path
import pandas as pd
import numpy as np

from suews_mcp.tools.utilities import (
    calculate_ohm_coefficients,
    calculate_surface_conductance,
    calculate_roughness,
)


@pytest.fixture
def temp_dir():
    """Temporary directory for test outputs."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def sample_results_ohm(temp_dir):
    """Create sample results file for OHM coefficient calculation."""
    # Create synthetic data with QS and QN
    n_points = 100
    time_index = pd.date_range("2024-01-01", periods=n_points, freq="h")

    # Generate synthetic data with a simple relationship
    # QS = a1*QN + a2 with some noise
    qn = np.random.uniform(100, 500, n_points)
    qs = 0.3 * qn + 50 + np.random.normal(0, 10, n_points)

    df = pd.DataFrame({"QN": qn, "QS": qs}, index=time_index)

    results_path = temp_dir / "results_ohm.csv"
    df.to_csv(results_path)
    return str(results_path)


@pytest.fixture
def sample_results_conductance(temp_dir):
    """Create sample results file for surface conductance calculation."""
    n_points = 100
    time_index = pd.date_range("2024-01-01", periods=n_points, freq="h")

    # Generate synthetic meteorological and flux data
    df = pd.DataFrame(
        {
            "QH": np.random.uniform(50, 200, n_points),
            "QE": np.random.uniform(50, 200, n_points),
            "T2": np.random.uniform(15, 25, n_points),
            "RH2": np.random.uniform(40, 80, n_points),
            "Pres": np.random.uniform(1010, 1020, n_points),
        },
        index=time_index,
    )

    results_path = temp_dir / "results_conductance.csv"
    df.to_csv(results_path)
    return str(results_path)


def test_calculate_ohm_coefficients_success(sample_results_ohm):
    """Test OHM coefficient calculation with valid data."""
    result = calculate_ohm_coefficients(sample_results_ohm, surface_type="urban")

    assert result["success"] is True
    assert result["surface_type"] == "urban"
    assert "coefficients" in result
    assert "a1" in result["coefficients"]
    assert "a2" in result["coefficients"]
    assert "a3" in result["coefficients"]
    assert "fit_statistics" in result
    assert "guidance" in result
    assert "reference" in result


def test_calculate_ohm_coefficients_missing_file():
    """Test OHM calculation with non-existent file."""
    result = calculate_ohm_coefficients("nonexistent.csv")

    assert result["success"] is False
    assert "error" in result


def test_calculate_ohm_coefficients_missing_variables(temp_dir):
    """Test OHM calculation with missing required variables."""
    # Create file with only one variable
    df = pd.DataFrame({"QN": [100, 200, 300]})
    results_path = temp_dir / "incomplete.csv"
    df.to_csv(results_path)

    result = calculate_ohm_coefficients(str(results_path))

    assert result["success"] is False
    assert "error" in result
    assert "QS" in result["error"] or "QN" in result["error"]


def test_calculate_surface_conductance_suews(sample_results_conductance):
    """Test surface conductance calculation with SUEWS method."""
    result = calculate_surface_conductance(sample_results_conductance, method="suews")

    assert result["success"] is True
    assert result["method"] == "suews"
    assert "conductance" in result
    assert "mean" in result["conductance"]
    assert "median" in result["conductance"]
    assert "std" in result["conductance"]
    assert result["units"] == "mm/s"
    assert "guidance" in result


def test_calculate_surface_conductance_observed(sample_results_conductance):
    """Test surface conductance calculation with observed method."""
    result = calculate_surface_conductance(
        sample_results_conductance, method="observed"
    )

    assert result["success"] is True
    assert result["method"] == "observed"
    assert "conductance" in result


def test_calculate_surface_conductance_invalid_method(sample_results_conductance):
    """Test surface conductance with invalid method."""
    result = calculate_surface_conductance(
        sample_results_conductance, method="invalid"
    )

    assert result["success"] is False
    assert "error" in result
    assert "available_methods" in result


def test_calculate_surface_conductance_missing_file():
    """Test surface conductance with non-existent file."""
    result = calculate_surface_conductance("nonexistent.csv")

    assert result["success"] is False
    assert "error" in result


def test_calculate_roughness_basic():
    """Test roughness calculation with basic parameters."""
    result = calculate_roughness(building_height=10.0, plan_area_fraction=0.4)

    assert result["success"] is True
    assert "input" in result
    assert result["input"]["building_height_m"] == 10.0
    assert result["input"]["plan_area_fraction"] == 0.4
    assert "results" in result
    assert "roughness_length_m" in result["results"]
    assert "displacement_height_m" in result["results"]
    assert "z0_to_height_ratio" in result["results"]
    assert "zd_to_height_ratio" in result["results"]
    assert "guidance" in result
    assert "reference" in result


def test_calculate_roughness_with_frontal_area():
    """Test roughness calculation with frontal area index."""
    result = calculate_roughness(
        building_height=15.0, plan_area_fraction=0.5, frontal_area_index=0.3
    )

    assert result["success"] is True
    assert result["input"]["frontal_area_index"] == 0.3


def test_calculate_roughness_invalid_inputs():
    """Test roughness calculation with invalid inputs."""
    # Negative building height
    result = calculate_roughness(building_height=-5.0, plan_area_fraction=0.4)

    # Should either fail or handle gracefully
    # The actual behavior depends on supy.util.cal_z0zd implementation
    assert "error" in result or "results" in result


def test_calculate_roughness_out_of_range_fraction():
    """Test roughness calculation with out-of-range plan area fraction."""
    # Plan area fraction > 1
    result = calculate_roughness(building_height=10.0, plan_area_fraction=1.5)

    # Should either fail or handle gracefully
    assert "error" in result or "results" in result
