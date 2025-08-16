# SUEWS Schema Implementation - Refined Next Steps

## Status Summary

### ✅ Completed (from PR #603)
1. **Basic schema versioning** - Single `schema_version` field implemented
2. **Migration framework** - Basic migration structure in place
3. **JSON Schema export** - Publisher creates JSON schemas
4. **CLI validation tools** - suews-validate command available
5. **CI/CD automation** - GitHub Actions workflow configured

### 🔄 Current State Analysis

#### Gaps vs schema-plan.md

1. **Pydantic v2 Strictness** ❌
   - Current: `ConfigDict(extra="allow")` in ValidatedConfig
   - Required: `ConfigDict(extra='forbid', frozen=True)`
   - Impact: Allows typos and unintended fields to pass validation

2. **Type Aliases & Constraints** ⚠️
   - Missing standardised type aliases (Frac, Albedo, Emissivity, etc.)
   - No Annotated types with Field constraints
   - Validation scattered across model_validators instead of field-level

3. **Domain Partition** ⚠️
   - Partially covered but not MECE (Mutually Exclusive, Collectively Exhaustive)
   - Missing clear separation between:
     - Site metadata
     - Surface composition  
     - Surface properties
     - Vegetation phenology
     - Urban hydrology
     - Met forcing
     - Simulation control
     - Data provenance

4. **Units & CF Metadata** ❌
   - No pint integration
   - Missing json_schema_extra with CF standard names
   - No systematic units handling

5. **Cross-field Invariants** ⚠️
   - Some validators exist but not comprehensive
   - Missing fraction sum-to-one enforcement
   - No systematic conditional requirements

## 📋 Prioritised Next Steps

### Phase 1: Strengthen Foundation (Week 1)

#### 1.1 Tighten Pydantic Models
```python
# Create base configuration
class StrictBaseModel(BaseModel):
    model_config = ConfigDict(
        extra='forbid',
        frozen=True,
        str_min_length=1,
        validate_default=True
    )
```

**Tasks:**
- [ ] Create `src/supy/data_model/_base.py` with StrictBaseModel
- [ ] Migrate critical models to inherit from StrictBaseModel
- [ ] Add test suite for strictness enforcement

#### 1.2 Implement Type Aliases
```python
# src/supy/data_model/_types.py
from typing import Annotated
from pydantic import Field

Frac = Annotated[float, Field(ge=0.0, le=1.0)]
Albedo = Annotated[float, Field(ge=0.0, le=1.0)]
Emissivity = Annotated[float, Field(ge=0.8, le=1.0)]
Pos = Annotated[float, Field(gt=0.0)]
NonNeg = Annotated[float, Field(ge=0.0)]
MonthlyArray = Annotated[List[float], Field(min_length=12, max_length=12)]
DiurnalArray = Annotated[List[float], Field(min_length=24, max_length=24)]
```

**Tasks:**
- [ ] Create comprehensive type aliases module
- [ ] Replace existing float fields with constrained types
- [ ] Add field-level validation where appropriate

### Phase 2: Domain Model Refinement (Week 2)

#### 2.1 Implement MECE Domain Partition

Create clear separation:
```python
# src/supy/data_model/domain/
├── site_metadata.py      # ID, coords, timezone, CRS
├── surface_fractions.py  # Built, paved, veg, water (sum=1)
├── surface_properties.py # Albedo, emissivity, roughness
├── phenology.py          # LAI/SAI monthly arrays
├── hydrology.py          # Storage, infiltration, runoff
├── forcing.py            # Met data configuration
├── control.py            # Simulation settings
└── provenance.py         # Data lineage tracking
```

**Tasks:**
- [ ] Restructure models into domain modules
- [ ] Implement SurfaceFractions with sum-to-one validator
- [ ] Add comprehensive cross-field validators

#### 2.2 Add Invariant Enforcement
```python
class SurfaceFractions(StrictBaseModel):
    built: Frac
    paved: Frac
    vegetation: Frac
    water: Frac
    
    @model_validator(mode="after")
    def sum_to_one(self):
        total = self.built + self.paved + self.vegetation + self.water
        if not (0.999 <= total <= 1.001):
            raise ValueError(f"Fractions must sum to 1 (got {total:.6f})")
        return self
```

### Phase 3: Units & Metadata (Week 3)

#### 3.1 Integrate Pint for Units
```python
# src/supy/data_model/_units.py
from pint import UnitRegistry
ureg = UnitRegistry()

class UnitField(BaseModel):
    value: float
    unit: str
    
    def to_si(self):
        return (self.value * ureg(self.unit)).to_base_units().magnitude
```

**Tasks:**
- [ ] Add optional pint validation layer
- [ ] Create unit conversion utilities
- [ ] Document standard units for all fields

#### 3.2 Add CF Metadata
```python
albedo: Albedo = Field(
    description="Surface albedo",
    json_schema_extra={
        'standard_name': 'surface_albedo',
        'units': '1',
        'valid_range': [0.0, 1.0]
    }
)
```

### Phase 4: Advanced Validation (Week 4)

#### 4.1 Property-based Testing
```python
# test/test_property_based.py
from hypothesis import given, strategies as st

@given(
    built=st.floats(0, 1),
    paved=st.floats(0, 1),
    vegetation=st.floats(0, 1),
    water=st.floats(0, 1)
)
def test_fractions_validation(built, paved, vegetation, water):
    # Test fraction validation logic
    pass
```

#### 4.2 Physics Sanity Checks
```python
class PhysicsValidator:
    def validate_energy_balance(self, config):
        """Warning-level physics checks"""
        if config.albedo > 0.5 and config.urban_fraction > 0.8:
            warnings.warn("High albedo unusual for urban areas")
```

## 🎯 Quick Wins (Can do immediately)

1. **Switch ValidatedConfig to extra='forbid'**
   - One-line change with big impact
   - Catches typos and invalid fields

2. **Add fraction sum validation**
   - Critical invariant currently missing
   - Prevents invalid configurations

3. **Create type aliases module**
   - Foundation for consistent validation
   - Improves code readability

## 📊 Success Metrics

- [ ] All models use `extra='forbid'`
- [ ] 100% of numeric fields have range constraints
- [ ] Surface fractions enforced to sum to 1.0
- [ ] JSON Schema includes units and CF metadata
- [ ] Property-based tests pass with 1000+ examples
- [ ] Zero silent validation failures

## 🚀 Implementation Order

1. **Immediate** (Today)
   - Fix ConfigDict settings
   - Add basic type aliases

2. **This Week**
   - Restructure domain models
   - Add cross-field validators

3. **Next Week**
   - Units integration
   - CF metadata addition

4. **Following Week**
   - Property-based testing
   - Physics validation layer

## 📝 Migration Strategy

For existing configurations:
1. Run migration tool to detect issues
2. Auto-fix where possible (e.g., normalise fractions)
3. Generate migration report with manual fixes needed
4. Provide clear error messages with solutions

## 🔍 Testing Requirements

Each phase needs:
- Unit tests for new validators
- Integration tests with real configs
- Property-based fuzzing
- Migration round-trip tests
- Performance benchmarks (validation should be <100ms)

## 📚 Documentation Updates

- Update schema versioning guide
- Add validation best practices
- Create migration cookbook
- Document all type constraints
- Provide urban archetype examples

## Key Decision Points

1. **Strictness Level**: Start with warnings, move to errors in v2.0?
2. **Units**: Require SI at rest or allow unit specifications?
3. **Breaking Changes**: Bundle in major version or gradual deprecation?

## Next Actions

1. Review this plan with team
2. Create GitHub issues for each phase
3. Set up feature branch for Phase 1
4. Begin with ConfigDict strictness fix