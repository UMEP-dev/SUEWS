"""Tests for MCP helper utilities."""

import pytest
import tempfile
from pathlib import Path
import yaml
from pydantic import ValidationError

from suews_mcp.utils.helpers import (
    load_yaml_file,
    save_yaml_file,
    format_validation_error,
    format_results_summary,
)


@pytest.fixture
def temp_dir():
    """Temporary directory for test outputs."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


def test_load_yaml_file(temp_dir):
    """Test loading a YAML file."""
    # Create test file
    test_file = temp_dir / "test.yml"
    data = {"key": "value", "number": 42}
    with open(test_file, "w") as f:
        yaml.dump(data, f)

    # Load and verify
    loaded_data = load_yaml_file(test_file)
    assert loaded_data == data


def test_load_yaml_file_nonexistent():
    """Test loading a non-existent file."""
    with pytest.raises(FileNotFoundError):
        load_yaml_file("nonexistent.yml")


def test_save_yaml_file(temp_dir):
    """Test saving a YAML file."""
    data = {"key": "value", "nested": {"a": 1, "b": 2}}
    output_file = temp_dir / "output.yml"

    save_yaml_file(data, output_file)

    assert output_file.exists()

    # Verify contents
    with open(output_file) as f:
        loaded_data = yaml.safe_load(f)
    assert loaded_data == data


def test_save_yaml_file_creates_directory(temp_dir):
    """Test that save_yaml_file creates parent directories."""
    output_file = temp_dir / "subdir" / "nested" / "output.yml"
    data = {"test": "data"}

    save_yaml_file(data, output_file)

    assert output_file.exists()
    assert output_file.parent.exists()


def test_format_validation_error():
    """Test formatting a Pydantic ValidationError."""
    from pydantic import BaseModel, Field

    class TestModel(BaseModel):
        name: str
        age: int = Field(gt=0)

    try:
        TestModel(name="Test", age=-5)
    except ValidationError as e:
        formatted = format_validation_error(e)

        assert "age" in formatted
        assert isinstance(formatted, str)


def test_format_validation_error_generic():
    """Test formatting a generic exception."""
    error = ValueError("Test error message")
    formatted = format_validation_error(error)

    assert formatted == "Test error message"


def test_format_results_summary_dataframe():
    """Test formatting results summary from DataFrame."""
    import pandas as pd
    import numpy as np

    # Create sample DataFrame
    dates = pd.date_range("2012-01-01", periods=100, freq="h")
    df = pd.DataFrame(
        {
            "temp": np.random.uniform(0, 30, 100),
            "humidity": np.random.uniform(30, 90, 100),
            "pressure": np.random.uniform(990, 1020, 100),
        },
        index=dates,
    )

    summary = format_results_summary(df)

    assert summary["shape"]["rows"] == 100
    assert summary["shape"]["columns"] == 3
    assert "temp" in summary["columns"]
    assert "humidity" in summary["columns"]
    assert "time_range" in summary
    assert "statistics" in summary


def test_format_results_summary_with_to_dataframe():
    """Test formatting results summary from object with to_dataframe method."""
    import pandas as pd
    import numpy as np

    class MockResults:
        def to_dataframe(self):
            return pd.DataFrame(
                {
                    "var1": np.random.randn(50),
                    "var2": np.random.randn(50),
                }
            )

    results = MockResults()
    summary = format_results_summary(results)

    assert summary["shape"]["rows"] == 50
    assert summary["shape"]["columns"] == 2
    assert "var1" in summary["columns"]
    assert "var2" in summary["columns"]
