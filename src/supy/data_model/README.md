# SUEWS Data Model

This directory contains the Pydantic-based data model for the SUEWS (Surface Urban Energy and Water balance Scheme) urban climate model.

## Overview

The data model provides type-safe, validated Python classes that represent all the physical and configuration parameters needed to run SUEWS simulations. It uses Pydantic for automatic validation and serialization.

## Key Components

### Core Classes
- **`Site`**: Top-level container for a complete SUEWS site configuration
- **`SiteProperties`**: Physical and morphological properties of the urban site
- **`InitialStates`**: Initial conditions for all model state variables
- **`Model`**: Model physics methods and control parameters

### Surface Types
- **`PavedProperties`**: Roads, pavements, and other impervious surfaces
- **`BldgsProperties`**: Building surfaces (walls, roofs)
- **`EvetrProperties`**: Evergreen trees and vegetation
- **`DectrProperties`**: Deciduous trees and vegetation  
- **`GrassProperties`**: Grass and low vegetation surfaces
- **`BsoilProperties`**: Bare soil surfaces
- **`WaterProperties`**: Water bodies (lakes, ponds, rivers)

### Specialized Components
- **`SnowParams`**: Snow physics and albedo parameters
- **`Conductance`**: Surface conductance for vegetation
- **`AnthropogenicEmissions`**: Heat and CO2 emissions from human activities
- **`ArchetypeProperties`**: Building energy model (STEBBS) parameters
- **`SPARTACUSParams`**: 3D radiation modeling parameters
- **`VerticalLayers`**: Multi-layer urban canopy structure

## RefValue System

All physical parameters use the `RefValue` wrapper class, which allows associating literature references with parameter values:

```python
# Basic usage
temperature = RefValue(15.0)

# With reference
temperature = RefValue(
    value=15.0, 
    ref=Reference(
        ref="Smith et al. (2023)",
        DOI="10.1234/example",
        ID="param_123"
    )
)
```

### Unit Conventions

**All physical quantities in RefValue parameters include explicit units.** The unit system follows these conventions:

#### Base Units
- **Length**: `m` (meters)
- **Time**: `s` (seconds), `h` (hours), `d` (days)
- **Mass**: `kg` (kilograms)
- **Temperature**: `degC` (Celsius) for meteorological quantities, `K` (Kelvin) for temperature differences
- **Energy**: `J` (joules), `W` (watts)

#### Derived Units (using ASCII notation)
- **Area**: `m^2` 
- **Volume**: `m^3`
- **Density**: `kg m^-3`
- **Heat capacity**: `J kg^-1 K^-1` or `J m^-3 K^-1`
- **Thermal conductivity**: `W m^-1 K^-1`
- **Heat flux**: `W m^-2`
- **Velocity**: `mm s^-1` (small velocities), `m s^-1` (larger velocities)
- **Flow rate**: `m^3 s^-1`, `mm h^-1`
- **Convection coefficient**: `W m^-2 K^-1`

#### Special Cases
- **Dimensionless quantities**: `dimensionless` (ratios, fractions, albedos, emissivities)
- **Vegetation**: `m^2 m^-2` (LAI), `degC d` (degree days)
- **Snow**: `mm K^-1 h^-1` (temperature melt factor), `mm W^-1 m^2 h^-1` (radiation melt factor)
- **Atmospheric**: `kPa^-1` (inverse pressure for conductance parameters)

#### Configuration Parameters
Configuration parameters (model method selections, file paths, boolean flags, counts) appropriately do not have physical units:

```python
# Physical quantity - HAS units
building_height: RefValue[float] = Field(
    default=RefValue(10.0),
    description="Building height",
    unit="m"
)

# Configuration parameter - NO units needed
emissions_method: RefValue[EmissionsMethod] = Field(
    default=RefValue(EmissionsMethod.J11),
    description="Method used to calculate anthropogenic emissions"
)
```

## Data Validation

The data model provides comprehensive validation:

- **Type checking**: Automatic validation of parameter types
- **Range validation**: Physical constraints (e.g., albedo ∈ [0,1])
- **Cross-parameter validation**: Ensuring parameter combinations make physical sense
- **Unit consistency**: All physical quantities properly documented with units

## Usage Examples

### Creating a Simple Site
```python
from supy.data_model import Site, SiteProperties

# Create site with default parameters
site = Site(
    name="example_site",
    gridiv=1,
    properties=SiteProperties(
        lat=RefValue(51.5),     # degrees
        lng=RefValue(-0.13),    # degrees  
        alt=RefValue(40.0),     # m above sea level
    )
)
```

### Customizing Surface Properties
```python
from supy.data_model import EvetrProperties, RefValue

# Customize evergreen tree properties
evetr = EvetrProperties(
    alb=RefValue(0.15),           # dimensionless
    evetreeh=RefValue(20.0),      # m
    faievetree=RefValue(0.2),     # dimensionless
)
```

### Working with Building Energy Model
```python
from supy.data_model import ArchetypeProperties

# Define building archetype
archetype = ArchetypeProperties(
    stebbs_Height=RefValue(25.0),                    # m
    FootprintArea=RefValue(100.0),                   # m^2
    WallEffectiveConductivity=RefValue(1.5),         # W m^-1 K^-1
    HeatingSetpointTemperature=RefValue(20.0),       # degC
)
```

## File Organization

- **`core.py`**: Base classes and core functionality
- **`type.py`**: RefValue system and fundamental types
- **`site.py`**: Site-level properties and complex building energy parameters
- **`surface.py`**: Individual surface type properties
- **`state.py`**: Initial state definitions for all surfaces
- **`model.py`**: Model physics methods and control parameters
- **`hydro.py`**: Water distribution and storage parameters
- **`human_activity.py`**: Anthropogenic emissions and irrigation
- **`ohm.py`**: Objective Hysteresis Model coefficients
- **`profile.py`**: Temporal profiles (hourly, daily, weekly)

## Development Notes

### Unit Addition History
All RefValue parameters representing physical quantities were systematically reviewed and assigned appropriate units following these principles:

1. **Physical quantities**: All parameters representing measurable physical properties (temperatures, lengths, densities, etc.) have explicit units
2. **Dimensionless ratios**: Parameters like albedo, emissivity, porosity use `unit="dimensionless"`
3. **Configuration parameters**: Model selections, file paths, and boolean flags appropriately have no units

This ensures dimensional consistency throughout the model and helps users understand the expected scales and ranges for each parameter.

### Adding New Parameters
When adding new RefValue parameters:

1. **For physical quantities**: Always include appropriate `unit=` parameter
2. **For dimensionless ratios**: Use `unit="dimensionless"`
3. **For configuration parameters**: No unit parameter needed
4. **Follow existing naming conventions**: Use ASCII notation for compound units (e.g., `W m^-2 K^-1`)

## Conditional Validation and JSON Schema Export

### Validation Coverage Comparison

The data model includes comprehensive conditional validation that validates only parameters relevant to enabled physics methods. When exporting to JSON Schema for web interfaces, different validation approaches provide varying coverage:

| Validation Type | Python Implementation | Online JSON Validator | Static JSON Schema |
|----------------|----------------------|----------------------|-------------------|
| Basic field validation | ✅ Full | ✅ Full | ✅ Full |
| Method-specific fields | ✅ Full | ✅ ~70% | ❌ Limited |
| Storage heat dependencies | ✅ Full | ✅ Full | ✅ Full |
| Complex conditional logic | ✅ Full | ❌ Limited | ❌ None |
| Cross-site validation | ✅ Full | ❌ Very Limited | ❌ None |
| Dynamic error messages | ✅ Full | ✅ Partial | ❌ Basic |
| SUEWS domain knowledge | ✅ Full | ❌ None | ❌ None |

### JSON Schema Capabilities

**What CAN be implemented in JSON Schema:**
- ✅ Basic method dependencies (e.g., `if diagmethod=1 then require building_height`)
- ✅ Field relevance logic (show/hide parameters based on enabled methods)
- ✅ Simple parameter relationships (e.g., `storageheatmethod=1 requires ohmincqf=0`)
- ✅ Value range dependencies and type validation

**What CANNOT be implemented in JSON Schema:**
- ❌ Complex Python validation logic
- ❌ Multi-step validation chains
- ❌ SUEWS-specific domain rules (e.g., "Snow calculations enabled but no validation implemented")
- ❌ Advanced cross-field dependencies across multiple configuration sections

### Implementation Recommendations

**For static deployment (GitHub Pages/ReadTheDocs):**

1. **Enhanced JSON Schema** (~60-70% validation coverage):
   ```json
   {
     "if": {"properties": {"diagmethod": {"const": 0}}},
     "then": {
       "properties": {"z0m_in": {"type": "number", "minimum": 0}},
       "not": {"required": ["building_height"]}
     }
   }
   ```

2. **Smart UI Design** (Primary defence):
   - Conditional field visibility based on method selection
   - Dynamic form sections for enabled methods only
   - Helpful tooltips explaining parameter relevance

3. **Clear Documentation** (Secondary defence):
   - Method-specific examples and guidance
   - Warning messages about remaining validation requirements
   - "Validate in Python before use" recommendations

**For server-based deployment:**
- Use full Python conditional validation via API endpoints
- Implement real-time validation with the complete logic
- Provide 100% validation coverage

### Online JSON Validators

Popular validators like JSONSchemaValidator.net, AJV Online, and Hyperjump support JSON Schema Draft 2019-09 with conditional validation, providing significantly better coverage than basic schemas but still missing complex SUEWS-specific logic.

## Future Work

### High Priority Improvements

1. **Enforce Strict Pydantic Validation**
   - Migrate from `extra="allow"` to `extra="forbid"` to catch typos and invalid fields
   - Create `StrictBaseModel` with frozen configuration
   - Breaking change requiring major version bump

2. **Surface Fraction Validation** 
   - Enforce sum-to-one constraint for physical consistency
   - Add tolerance for floating-point (0.999-1.001)
   - Critical physics invariant currently not validated

3. **Type Aliases for Constrained Fields**
   - Create standardised type definitions (Frac, Albedo, Emissivity, etc.)
   - Move validation from model validators to field level
   - Improve code consistency and error messages

### Medium Priority Enhancements

4. **Property-Based Testing with Hypothesis**
   - Test edge cases and invariants systematically
   - Validate surface fractions, array lengths, physical ranges
   - Improve robustness of validation logic

5. **Deprecation Framework**
   - Systematic handling of field deprecation
   - Clear migration paths with warnings
   - Track deprecation timelines

6. **Migration Documentation**
   - Create cookbook with examples
   - Document breaking changes
   - Provide automated migration scripts

### Lower Priority Features

7. **CF Convention Metadata**
   - Add Climate and Forecast standard names
   - Enhance interoperability with climate tools
   - Use json_schema_extra for metadata

8. **Physics Sanity Checks**
   - Configurable warnings for unusual configurations
   - Energy balance constraints
   - Document typical parameter ranges

9. **Pint Integration for Units**
   - Optional unit-aware validation
   - Automatic SI conversion on input
   - Help users with unit conversions

10. **MECE Model Restructuring**
    - Reorganise into clear domain partitions
    - Separate concerns more clearly
    - Major refactoring effort

### References
- Järvi, L., et al. (2011). Development of the Surface Urban Energy and Water Balance Scheme (SUEWS) for cold climate conditions. *Geoscientific Model Development*, 4(4), 845-869.
- Ward, H. C., et al. (2016). Surface Urban Energy and Water Balance Scheme (SUEWS): Development and evaluation at two UK sites. *Urban Climate*, 18, 1-32.
- Sun, T., et al. (2017). A Python-enhanced urban land surface model SuPy (SUEWS in Python, v2019.2): development, deployment and demonstration. *Geoscientific Model Development*, 12(7), 2781-2795.