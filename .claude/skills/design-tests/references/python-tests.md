# Python Unit Test Patterns

Test patterns for Python utility functions, CLI commands, and helper modules.

## Contents

- [Test Organisation](#test-organisation)
- [Pattern 1: Simple Function Test](#pattern-1-simple-function-test)
- [Pattern 2: Parameterised Tests](#pattern-2-parameterised-tests)
- [Pattern 3: CLI Command Test](#pattern-3-cli-command-test)
- [Pattern 4: Exception Testing](#pattern-4-exception-testing)
- [Pattern 5: Fixture Usage](#pattern-5-fixture-usage)
- [Pattern 6: Mocking](#pattern-6-mocking)
- [Pattern 7: DataFrame Testing](#pattern-7-dataframe-testing)
- [pytest Markers](#pytest-markers)
- [Coverage Targets](#coverage-targets)
- [Test Docstrings](#test-docstrings)

## Test Organisation

```
test/
├── core/              # Core functionality
│   ├── test_supy.py          # Main API tests
│   ├── test_cli_run.py       # CLI command tests
│   └── test_util_*.py        # Utility function tests
├── data_model/        # Configuration tests
├── physics/           # Physics validation
└── io_tests/          # Input/output tests
```

## Pattern 1: Simple Function Test

```python
def test_kelvin_to_celsius():
    """Test temperature conversion utility."""
    from supy.util import kelvin_to_celsius

    assert kelvin_to_celsius(273.15) == pytest.approx(0.0)
    assert kelvin_to_celsius(373.15) == pytest.approx(100.0)
    assert kelvin_to_celsius(0) == pytest.approx(-273.15)
```

## Pattern 2: Parameterised Tests

```python
import pytest

@pytest.mark.parametrize("input_val,expected", [
    (273.15, 0.0),
    (373.15, 100.0),
    (300.0, 26.85),
])
def test_kelvin_to_celsius_parametrised(input_val, expected):
    """Test temperature conversion with multiple values."""
    from supy.util import kelvin_to_celsius

    assert kelvin_to_celsius(input_val) == pytest.approx(expected, rel=1e-4)
```

## Pattern 3: CLI Command Test

```python
from click.testing import CliRunner

def test_cli_run_command():
    """Test supy CLI run command."""
    from supy.cmd import main

    runner = CliRunner()
    result = runner.invoke(main, ['run', 'config.yml', '--dry-run'])

    assert result.exit_code == 0
    assert 'Configuration validated' in result.output
```

## Pattern 4: Exception Testing

```python
import pytest

def test_invalid_input_raises():
    """Test that invalid inputs raise appropriate exceptions."""
    from supy.util import calculate_density

    with pytest.raises(ValueError, match="Temperature must be positive"):
        calculate_density(temperature=-10)

    with pytest.raises(TypeError):
        calculate_density(temperature="warm")
```

## Pattern 5: Fixture Usage

```python
import pytest

@pytest.fixture
def sample_config():
    """Provide sample configuration for tests."""
    from importlib.resources import files, as_file

    config_ref = files("supy") / "sample_data" / "sample_config.yml"
    with as_file(config_ref) as path:
        yield path

@pytest.fixture
def sample_forcing():
    """Provide sample forcing data."""
    import supy as sp
    _, df_forcing = sp.load_sample_data()
    return df_forcing

def test_simulation_with_fixtures(sample_config, sample_forcing):
    """Test simulation using fixtures."""
    import supy as sp

    df_state = sp.init_supy(sample_config)
    df_output = sp.run_supy(df_forcing, df_state)

    assert len(df_output) > 0
```

## Pattern 6: Mocking

```python
from unittest.mock import patch, MagicMock

def test_api_call_mocked():
    """Test function that makes external API calls."""
    from supy.util.era5 import download_era5_data

    mock_response = {'temperature': [20, 21, 22]}

    with patch('supy.util.era5.cdsapi.Client') as mock_client:
        mock_client.return_value.retrieve.return_value = MagicMock()
        mock_client.return_value.retrieve.return_value.download.return_value = None

        result = download_era5_data(
            start_date='2020-01-01',
            end_date='2020-01-02'
        )

        mock_client.return_value.retrieve.assert_called_once()
```

## Pattern 7: DataFrame Testing

```python
import pandas as pd
import numpy as np

def test_dataframe_processing():
    """Test DataFrame manipulation functions."""
    from supy.post import resample_output

    # Create test data
    df = pd.DataFrame({
        'T2': np.random.uniform(15, 25, 24),
        'QH': np.random.uniform(50, 150, 24),
    }, index=pd.date_range('2020-01-01', periods=24, freq='h'))

    # Test resampling
    df_daily = resample_output(df, 'D')

    assert len(df_daily) == 1
    assert df_daily.index[0].date() == pd.Timestamp('2020-01-01').date()
```

## pytest Markers

Use markers to categorise tests:

```python
import pytest

@pytest.mark.smoke
def test_basic_import():
    """Smoke test: basic import works."""
    import supy
    assert hasattr(supy, 'run_supy')

@pytest.mark.slow
def test_year_long_simulation():
    """Test full year simulation (takes >30s)."""
    # ... long running test

@pytest.mark.core
def test_energy_balance():
    """Core physics test."""
    # ... physics validation
```

## Coverage Targets

| Component | Target | Notes |
|-----------|--------|-------|
| Core functions | 85-90% | Critical paths |
| Utilities | 70-80% | Helper functions |
| CLI | 60-70% | Command interface |
| Critical paths | 95-100% | Must be covered |

## Test Docstrings

Include helpful docstrings:

```python
def test_wind_profile_calculation():
    """
    Test logarithmic wind profile calculation.

    Verifies equation 5.3 from Stull (1988) Boundary Layer
    Meteorology, which relates wind speed at measurement
    height to friction velocity and roughness length.

    Expected accuracy: +/- 0.1 m/s for typical conditions.
    """
```
