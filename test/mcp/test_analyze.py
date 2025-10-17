"""Tests for MCP analysis tools."""

import pytest
import tempfile
from pathlib import Path
import pandas as pd
import numpy as np

from supy.mcp.tools.analyze import (
    load_results,
    compute_statistics,
    create_plot,
    export_results,
)


@pytest.fixture
def sample_results_csv(tmp_path):
    """Create a sample CSV results file."""
    # Create sample data
    dates = pd.date_range("2012-01-01", periods=24, freq="h")
    data = {
        "temp": np.random.uniform(0, 30, 24),
        "humidity": np.random.uniform(30, 90, 24),
        "pressure": np.random.uniform(990, 1020, 24),
    }
    df = pd.DataFrame(data, index=dates)

    # Save to CSV
    csv_path = tmp_path / "results.csv"
    df.to_csv(csv_path)

    return csv_path


@pytest.mark.asyncio
async def test_load_results_csv(sample_results_csv):
    """Test loading results from CSV file."""
    result = await load_results(str(sample_results_csv))

    assert result["success"] is True
    assert result["num_rows"] == 24
    # CSV includes index column, so 4 columns total (index + 3 data columns)
    assert result["num_columns"] == 4
    assert "temp" in result["columns"]
    assert "humidity" in result["columns"]
    assert "pressure" in result["columns"]


@pytest.mark.asyncio
async def test_load_results_with_variables(sample_results_csv):
    """Test loading specific variables from results."""
    result = await load_results(
        str(sample_results_csv),
        variables=["temp", "humidity"],
    )

    assert result["success"] is True
    assert result["num_columns"] == 2
    assert "temp" in result["columns"]
    assert "humidity" in result["columns"]
    assert "pressure" not in result["columns"]


@pytest.mark.asyncio
async def test_load_results_nonexistent():
    """Test loading non-existent file."""
    result = await load_results("nonexistent.csv")

    assert "error" in result


@pytest.mark.asyncio
async def test_compute_statistics_mean(sample_results_csv):
    """Test computing mean statistics."""
    result = await compute_statistics(
        str(sample_results_csv),
        variables=["temp", "humidity"],
        aggregation="mean",
    )

    assert result["success"] is True
    assert result["aggregation"] == "mean"
    assert "temp" in result["statistics"]
    assert "humidity" in result["statistics"]
    assert isinstance(result["statistics"]["temp"], float)


@pytest.mark.asyncio
async def test_compute_statistics_sum(sample_results_csv):
    """Test computing sum statistics."""
    result = await compute_statistics(
        str(sample_results_csv),
        variables=["temp"],
        aggregation="sum",
    )

    assert result["success"] is True
    assert result["aggregation"] == "sum"
    assert "temp" in result["statistics"]


@pytest.mark.asyncio
async def test_compute_statistics_minmax(sample_results_csv):
    """Test computing min/max statistics."""
    result_min = await compute_statistics(
        str(sample_results_csv),
        variables=["temp"],
        aggregation="min",
    )

    result_max = await compute_statistics(
        str(sample_results_csv),
        variables=["temp"],
        aggregation="max",
    )

    assert result_min["success"] is True
    assert result_max["success"] is True
    assert result_min["statistics"]["temp"] < result_max["statistics"]["temp"]


@pytest.mark.asyncio
async def test_compute_statistics_missing_variable(sample_results_csv):
    """Test computing statistics with missing variable."""
    result = await compute_statistics(
        str(sample_results_csv),
        variables=["nonexistent_var"],
        aggregation="mean",
    )

    assert "error" in result


@pytest.mark.asyncio
async def test_create_plot_timeseries(sample_results_csv, tmp_path):
    """Test creating a timeseries plot."""
    output_path = tmp_path / "plot.png"

    result = await create_plot(
        str(sample_results_csv),
        variables=["temp", "humidity"],
        output_path=str(output_path),
        plot_type="timeseries",
    )

    assert result["success"] is True
    assert result["plot_type"] == "timeseries"
    assert output_path.exists()


@pytest.mark.asyncio
async def test_create_plot_scatter(sample_results_csv, tmp_path):
    """Test creating a scatter plot."""
    output_path = tmp_path / "scatter.png"

    result = await create_plot(
        str(sample_results_csv),
        variables=["temp", "humidity"],
        output_path=str(output_path),
        plot_type="scatter",
    )

    assert result["success"] is True
    assert result["plot_type"] == "scatter"
    assert output_path.exists()


@pytest.mark.asyncio
async def test_create_plot_histogram(sample_results_csv, tmp_path):
    """Test creating a histogram."""
    output_path = tmp_path / "histogram.png"

    result = await create_plot(
        str(sample_results_csv),
        variables=["temp"],
        output_path=str(output_path),
        plot_type="histogram",
    )

    assert result["success"] is True
    assert output_path.exists()


@pytest.mark.asyncio
async def test_create_plot_invalid_type(sample_results_csv, tmp_path):
    """Test creating plot with invalid type."""
    output_path = tmp_path / "plot.png"

    result = await create_plot(
        str(sample_results_csv),
        variables=["temp"],
        output_path=str(output_path),
        plot_type="invalid_type",
    )

    assert "error" in result


@pytest.mark.asyncio
async def test_export_results_csv(sample_results_csv, tmp_path):
    """Test exporting results to CSV."""
    output_path = tmp_path / "exported.csv"

    result = await export_results(
        str(sample_results_csv),
        str(output_path),
        format="csv",
    )

    assert result["success"] is True
    assert output_path.exists()


@pytest.mark.asyncio
async def test_export_results_json(sample_results_csv, tmp_path):
    """Test exporting results to JSON."""
    output_path = tmp_path / "exported.json"

    result = await export_results(
        str(sample_results_csv),
        str(output_path),
        format="json",
    )

    assert result["success"] is True
    assert output_path.exists()


@pytest.mark.asyncio
async def test_export_results_with_variables(sample_results_csv, tmp_path):
    """Test exporting specific variables."""
    output_path = tmp_path / "exported.csv"

    result = await export_results(
        str(sample_results_csv),
        str(output_path),
        format="csv",
        variables=["temp"],
    )

    assert result["success"] is True

    # Verify only selected variable was exported
    df = pd.read_csv(output_path, index_col=0)
    assert "temp" in df.columns
    assert "humidity" not in df.columns
