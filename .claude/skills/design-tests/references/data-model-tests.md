# Data Model Test Patterns

Test patterns for Pydantic configuration models and data validation.

## Contents

- [Core Test Types](#core-test-types)
- [Pattern 1: Configuration Round-Trip](#pattern-1-configuration-round-trip)
- [Pattern 2: Field Validation](#pattern-2-field-validation)
- [Pattern 3: Default Values](#pattern-3-default-values)
- [Pattern 4: Nested Model Validation](#pattern-4-nested-model-validation)
- [Pattern 5: Type Coercion](#pattern-5-type-coercion)
- [Pattern 6: Conditional Validation](#pattern-6-conditional-validation)
- [Pattern 7: Error Message Quality](#pattern-7-error-message-quality)
- [Package Data Access](#package-data-access)
- [Anti-Patterns](#anti-patterns)

## Core Test Types

1. **Schema Tests**: Validate field types, defaults, constraints
2. **Conversion Tests**: YAML-DataFrame-YAML round-trips
3. **Validation Tests**: Input validation logic
4. **Integration Tests**: Multi-component interactions

## Pattern 1: Configuration Round-Trip

```python
def test_yaml_dataframe_yaml_cycle():
    """Test configuration survives YAML -> DataFrame -> YAML conversion."""
    import yaml
    from supy import SUEWSConfig

    # Load original
    with open('test/fixtures/benchmark1/benchmark1.yml') as f:
        original = yaml.safe_load(f)

    # Convert to DataFrame
    config = SUEWSConfig.from_yaml('test/fixtures/benchmark1/benchmark1.yml')
    df_state = config.to_dataframe()

    # Convert back
    config2 = SUEWSConfig.from_dataframe(df_state)

    # Compare key fields
    assert config.simulation.start_time == config2.simulation.start_time
    assert config.site.latitude == config2.site.latitude
```

## Pattern 2: Field Validation

```python
import pytest
from pydantic import ValidationError

def test_latitude_bounds():
    """Test latitude validation rejects invalid values."""
    from supy.data_model import SiteConfig

    # Valid cases
    SiteConfig(latitude=51.5, longitude=-0.1)  # London
    SiteConfig(latitude=-33.9, longitude=151.2)  # Sydney

    # Invalid cases
    with pytest.raises(ValidationError) as exc_info:
        SiteConfig(latitude=91.0, longitude=0)  # Too high
    assert 'latitude' in str(exc_info.value)

    with pytest.raises(ValidationError):
        SiteConfig(latitude=-91.0, longitude=0)  # Too low
```

## Pattern 3: Default Values

```python
def test_default_values_applied():
    """Test that optional fields get sensible defaults."""
    from supy.data_model import OutputConfig

    config = OutputConfig()  # All defaults

    assert config.output_interval == 3600  # 1 hour default
    assert config.include_debug is False
    assert config.output_format == 'csv'
```

## Pattern 4: Nested Model Validation

```python
def test_nested_model_structure():
    """Test nested Pydantic model validation."""
    from supy.data_model import SUEWSConfig

    config = SUEWSConfig.from_yaml('config.yml')

    # Access nested models
    assert hasattr(config, 'simulation')
    assert hasattr(config.simulation, 'timestep')

    # Verify nested validation propagates
    assert config.simulation.timestep > 0
    assert config.simulation.timestep <= 3600
```

## Pattern 5: Type Coercion

```python
def test_type_coercion():
    """Test automatic type coercion in Pydantic models."""
    from supy.data_model import SimulationConfig

    # String to datetime
    config = SimulationConfig(
        start_time="2020-01-01 00:00:00",
        end_time="2020-12-31 23:59:59"
    )

    from datetime import datetime
    assert isinstance(config.start_time, datetime)

    # String to number
    config2 = SimulationConfig(timestep="300")  # String "300"
    assert config2.timestep == 300
    assert isinstance(config2.timestep, int)
```

## Pattern 6: Conditional Validation

```python
def test_conditional_validation():
    """Test physics option compatibility validation."""
    from supy.data_model import PhysicsOptions
    import pytest

    # Valid combination
    PhysicsOptions(
        stomatal_method='jarvis',
        soil_moisture_coupling=True
    )

    # Invalid combination (hypothetical)
    with pytest.raises(ValidationError) as exc_info:
        PhysicsOptions(
            stomatal_method='ball_berry',
            photosynthesis_model='disabled'  # Incompatible
        )
    assert 'incompatible' in str(exc_info.value).lower()
```

## Pattern 7: Error Message Quality

```python
def test_validation_error_messages():
    """Test that validation errors are user-friendly."""
    from supy.data_model import SiteConfig
    import pytest

    with pytest.raises(ValidationError) as exc_info:
        SiteConfig(latitude="not a number", longitude=0)

    error_msg = str(exc_info.value)

    # Should be helpful
    assert 'latitude' in error_msg
    assert 'type' in error_msg.lower() or 'number' in error_msg.lower()
```

## Package Data Access

Use `importlib.resources` for accessing package data in tests:

```python
from importlib.resources import files, as_file

# Read configuration file from package
sample_config = files("supy") / "sample_data" / "sample_config.yml"
with sample_config.open() as f:
    config_data = yaml.safe_load(f)

# Get path for functions expecting file paths
with as_file(files("supy") / "sample_data" / "sample_config.yml") as path:
    df_state = sp.init_supy(path)
```

## Anti-Patterns

Avoid:
- Relative paths from repository root
- Assuming specific installation directory
- Hardcoded paths to `src/` directory
- Testing implementation details rather than behaviour
