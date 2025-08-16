# Data Model Restructuring Proposal

## Current Issues

The `src/supy/data_model/` directory has grown organically and now contains:
- Core models mixed with utilities
- Validation logic scattered across files
- YAML processing in a subdirectory
- No clear separation between public API and internal implementation

## Proposed Structure

```
src/supy/data_model/
├── __init__.py              # Public API exports only
├── README.md                # Documentation
│
├── _internal/               # Private implementation (prefix with _)
│   ├── __init__.py
│   ├── _schema_version.py   # Version management
│   ├── _base.py            # Base models (new)
│   ├── _types.py           # Type aliases (new)
│   └── _constants.py       # Constants (new)
│
├── core/                    # Core configuration models
│   ├── __init__.py
│   ├── config.py           # SUEWSConfig (from core.py)
│   ├── site.py             # Site, SiteProperties
│   └── model.py            # Model, ModelPhysics, ModelControl
│
├── surfaces/                # Surface-specific models
│   ├── __init__.py
│   ├── properties.py       # Surface properties (from surface.py)
│   ├── state.py           # Initial states
│   └── types.py           # SurfaceType enum
│
├── parameters/              # Physical parameters
│   ├── __init__.py
│   ├── hydro.py           # Water distribution
│   ├── human_activity.py  # Anthropogenic emissions
│   ├── ohm.py            # OHM coefficients
│   └── profile.py        # Temporal profiles
│
├── validation/             # All validation logic
│   ├── __init__.py
│   ├── controller.py      # ValidationController
│   ├── feedback.py        # ValidationResult, feedback
│   ├── utils.py          # Validation utilities
│   └── rules/            # Validation rules by category
│       ├── physics.py
│       ├── ranges.py
│       └── consistency.py
│
├── processors/            # Data processors
│   ├── __init__.py
│   ├── yaml/             # YAML processing pipeline
│   │   ├── __init__.py
│   │   ├── orchestrator.py
│   │   ├── phase_a_parameter.py
│   │   ├── phase_b_science.py
│   │   └── phase_c_pydantic.py
│   └── annotators/       # Annotation utilities
│       ├── yaml_annotator.py
│       └── json_annotator.py
│
└── types/                # Type system
    ├── __init__.py
    ├── ref_value.py      # RefValue, Reference
    └── enums.py          # All enums (timezone, methods, etc.)
```

## Migration Benefits

### 1. Clear Organisation
- **Core models** separate from utilities
- **Surface types** grouped together
- **Validation** consolidated in one place
- **Processing** clearly separated

### 2. Better Imports
```python
# Current (confusing)
from supy.data_model.core import SUEWSConfig
from supy.data_model.validation_controller import ValidationController
from supy.data_model.yaml_processor.orchestrator import YAMLOrchestrator

# Proposed (clear)
from supy.data_model.core import SUEWSConfig
from supy.data_model.validation import ValidationController
from supy.data_model.processors.yaml import YAMLOrchestrator
```

### 3. Easier Testing
```
test/data_model/
├── test_core/
├── test_surfaces/
├── test_validation/
└── test_processors/
```

### 4. Schema Versioning Integration
```python
# _internal/_schema_version.py - version definitions
# _internal/_types.py - version-aware type aliases
# validation/rules/ - version-specific validation rules
```

## Implementation Plan

### Phase 1: Create New Structure (Non-breaking)
1. Create new directories
2. Copy files to new locations
3. Update imports in new files
4. Keep old files with deprecation warnings

### Phase 2: Update Internal References
1. Update all internal imports
2. Run full test suite
3. Fix any broken references

### Phase 3: Update Public API
1. Update `__init__.py` exports
2. Add compatibility layer for old imports
3. Document migration in changelog

### Phase 4: Clean Up (Next major version)
1. Remove old files
2. Remove compatibility layer
3. Update documentation

## Alternative: Minimal Reorganisation

If full restructuring is too disruptive, consider minimal changes:

```
src/supy/data_model/
├── __init__.py
├── README.md
├── _internal/          # Move private stuff here
│   ├── _schema_version.py
│   ├── _types.py (new)
│   └── _base.py (new)
├── core.py            # Keep main models
├── site.py
├── model.py
├── surface.py
├── state.py
├── validation/        # Consolidate validation
│   ├── __init__.py
│   ├── controller.py
│   ├── feedback.py
│   └── utils.py
└── yaml_processor/    # Keep as is
```

## Recommendation

**For now**: Go with minimal reorganisation
- Move schema versioning to `_internal/`
- Consolidate validation into `validation/`
- Keep core models as they are

**For v2.0**: Do full restructuring
- Breaking change anyway
- Clean slate for better organisation
- Align with schema v2.0

## File Mapping

### Files to Move/Rename

| Current | Proposed (Full) | Proposed (Minimal) |
|---------|----------------|-------------------|
| `_schema_version.py` | `_internal/_schema_version.py` | `_internal/_schema_version.py` |
| `core.py` | `core/config.py` | Keep as is |
| `site.py` | `core/site.py` | Keep as is |
| `model.py` | `core/model.py` | Keep as is |
| `surface.py` | `surfaces/properties.py` | Keep as is |
| `state.py` | `surfaces/state.py` | Keep as is |
| `validation_controller.py` | `validation/controller.py` | `validation/controller.py` |
| `validation_feedback.py` | `validation/feedback.py` | `validation/feedback.py` |
| `validation_utils.py` | `validation/utils.py` | `validation/utils.py` |
| `yaml_annotator.py` | `processors/annotators/yaml_annotator.py` | Keep as is |
| `yaml_processor/*` | `processors/yaml/*` | Keep as is |

### New Files to Create

1. `_internal/_base.py` - Base models
2. `_internal/_types.py` - Type aliases
3. `_internal/_constants.py` - Shared constants

## Next Steps

1. **Decide on approach** (minimal vs full)
2. **Create GitHub issue** for tracking
3. **Implement in stages** to avoid disruption
4. **Update imports gradually** with deprecation warnings
5. **Document changes** in migration guide