# Schema Versioning Integration Recommendations

Based on analysis of the `src/supy/data_model/` structure, here are recommendations for integrating schema versioning more deeply into the SUEWS data model.

## Current State Analysis

### Strengths
1. **Well-organised module structure**: Clear separation between surface types, model physics, and configuration
2. **RefValue system**: Good abstraction for parameter values with references
3. **Validation framework**: Two-step validation with controller pattern
4. **Comprehensive documentation**: README with unit conventions and JSON Schema capabilities

### Integration Points Identified
1. **Core.py**: Already has `schema_version` field in `SUEWSConfig`
2. **Validation system**: Conditional validation controller could version-aware
3. **Type system**: RefValue could carry version metadata
4. **YAML processor**: Three-phase validation pipeline perfect for version handling

## Recommended Integration Strategy

### 1. Create Base Model with Version Awareness

**File**: `src/supy/data_model/_base.py` (new)

```python
from pydantic import BaseModel, ConfigDict, Field
from typing import Optional
from ._schema_version import CURRENT_SCHEMA_VERSION

class VersionedBaseModel(BaseModel):
    """Base model for all versioned SUEWS data models."""
    
    model_config = ConfigDict(
        # Start permissive, migrate to strict in v2.0
        extra="allow",  # Will change to "forbid" in v2.0
        validate_default=True,
        ser_json_inf_nan="constants"
    )
    
    # Optional version tracking at model level
    _schema_version: Optional[str] = None
    
    def get_schema_version(self) -> str:
        """Get the schema version this model was created with."""
        return self._schema_version or CURRENT_SCHEMA_VERSION

class StrictVersionedBaseModel(BaseModel):
    """Strict base model for future migration (v2.0)."""
    
    model_config = ConfigDict(
        extra="forbid",
        frozen=True,
        validate_default=True,
        str_min_length=1
    )
```

### 2. Enhance Type System with Version Metadata

**File**: `src/supy/data_model/_types.py` (new)

```python
from typing import Annotated, List
from pydantic import Field

# Type aliases aligned with schema-plan.md
Frac = Annotated[float, Field(ge=0.0, le=1.0, description="Fraction [0-1]")]
Albedo = Annotated[float, Field(ge=0.0, le=1.0, description="Albedo")]
Emissivity = Annotated[float, Field(ge=0.8, le=1.0, description="Emissivity")]
Pos = Annotated[float, Field(gt=0.0, description="Positive value")]
NonNeg = Annotated[float, Field(ge=0.0, description="Non-negative value")]

# Array types with fixed lengths
MonthlyArray = Annotated[List[float], Field(min_length=12, max_length=12)]
DiurnalArray = Annotated[List[float], Field(min_length=24, max_length=24)]

# Version-specific types (for future migration)
def get_type_for_version(type_name: str, version: str):
    """Get type definition for specific schema version."""
    type_registry = {
        "1.0": {
            "Frac": Frac,
            "Albedo": Albedo,
        },
        # Future versions may have different constraints
        "2.0": {
            "Frac": Annotated[float, Field(ge=0.0, le=1.0, 
                                          json_schema_extra={"strict": True})]
        }
    }
    return type_registry.get(version, {}).get(type_name)
```

### 3. Version-Aware Validation Controller

**Update**: `src/supy/data_model/validation_controller.py`

```python
class ValidationController(BaseModel):
    """Controller for conditional validation with version awareness."""
    
    # Add version tracking
    schema_version: str = Field(default=CURRENT_SCHEMA_VERSION)
    
    def _analyze_config(self) -> None:
        """Analyze configuration considering schema version."""
        # Extract schema version
        self.schema_version = self.config_data.get(
            "schema_version", CURRENT_SCHEMA_VERSION
        )
        
        # Apply version-specific validation rules
        if self.schema_version == "1.0":
            self._apply_v1_0_rules()
        elif self.schema_version == "2.0":
            self._apply_v2_0_rules()
    
    def _apply_v1_0_rules(self):
        """Validation rules for schema v1.0."""
        # Current validation logic
        pass
    
    def _apply_v2_0_rules(self):
        """Stricter validation for schema v2.0."""
        # Future stricter validation
        pass
```

### 4. Integrate with YAML Processor Pipeline

**Update**: `src/supy/data_model/yaml_processor/orchestrator.py`

Add version handling to the three-phase pipeline:

```python
class YAMLOrchestrator:
    def process(self, config_dict: dict) -> ProcessingResult:
        # Phase 0: Version detection and migration
        version = self._detect_version(config_dict)
        if version != CURRENT_SCHEMA_VERSION:
            config_dict = self._migrate_if_needed(config_dict, version)
        
        # Phase A: Parameter updates (version-aware)
        phase_a_result = self._phase_a_process(config_dict, version)
        
        # Phase B: Science checks (version-specific rules)
        phase_b_result = self._phase_b_validate(phase_a_result, version)
        
        # Phase C: Pydantic validation
        phase_c_result = self._phase_c_report(phase_b_result, version)
        
        return phase_c_result
```

### 5. Export Version Metadata in JSON Schema

**Update**: `src/supy/util/schema_publisher.py`

Enhance JSON Schema generation with version metadata:

```python
def generate_schema(version: str = None) -> dict:
    """Generate JSON Schema with version metadata."""
    schema = SUEWSConfig.model_json_schema()
    
    # Add version metadata
    schema["$id"] = f"https://suews.org/schemas/{version}/config.json"
    schema["version"] = version
    schema["x-schema-version"] = {
        "current": version,
        "compatible": COMPATIBLE_VERSIONS.get(version, []),
        "deprecated_fields": get_deprecated_fields(version)
    }
    
    # Add conditional validation based on version
    if version >= "2.0":
        schema["additionalProperties"] = False  # Strict mode
    
    return schema
```

## Migration Path

### Phase 1: Foundation (Current PR)
✅ Basic `schema_version` field in SUEWSConfig
✅ Version detection and compatibility checking
✅ Migration framework skeleton

### Phase 2: Type System (Next PR)
- [ ] Create `_types.py` with constrained type aliases
- [ ] Add `_base.py` with versioned base models
- [ ] Update existing models to use type aliases

### Phase 3: Validation Enhancement
- [ ] Make ValidationController version-aware
- [ ] Integrate version handling in YAML processor
- [ ] Add version-specific validation rules

### Phase 4: Strict Mode (v2.0)
- [ ] Switch to `StrictVersionedBaseModel`
- [ ] Enforce `extra="forbid"`
- [ ] Add surface fraction validation
- [ ] Major version bump

## Benefits of This Approach

1. **Gradual migration**: Can introduce improvements without breaking changes
2. **Version-aware validation**: Different rules for different schema versions
3. **Type safety**: Constrained types catch errors at field level
4. **Clear upgrade path**: Users know exactly what changes between versions
5. **Backward compatibility**: Old configs continue to work with warnings

## Testing Strategy

```python
# test/test_schema_integration.py
def test_version_aware_validation():
    """Test that validation rules change with version."""
    config_v1 = {"schema_version": "1.0", "extra_field": "allowed"}
    config_v2 = {"schema_version": "2.0", "extra_field": "forbidden"}
    
    # v1.0 allows extra fields
    assert SUEWSConfig(**config_v1)
    
    # v2.0 forbids extra fields
    with pytest.raises(ValidationError):
        StrictSUEWSConfig(**config_v2)

def test_type_constraints():
    """Test field-level type constraints."""
    # Fraction must be 0-1
    with pytest.raises(ValidationError):
        config = {"surface_fraction": 1.5}  # > 1.0
```

## Immediate Actions

1. **Keep current implementation** - It's a good foundation
2. **Create `_types.py`** - Quick win with immediate benefits
3. **Add fraction validation** - Critical physics constraint
4. **Document migration timeline** - Set expectations for v2.0

## Long-term Vision

The schema versioning system should evolve to:
- Support multiple configuration formats (YAML, JSON, TOML)
- Enable web-based configuration editors with real-time validation
- Provide automated migration tools for major updates
- Track parameter provenance through versions
- Support configuration inheritance and composition