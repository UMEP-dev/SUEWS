# Proposed Model Organization

## Current Issues
- 15 Python files at root level
- Mix of core models, utilities, and supporting types
- Hard to distinguish what's a main model vs helper

## Proposed Structure

```
src/supy/data_model/
├── __init__.py              # Public API exports
├── README.md                # Documentation
├── _schema_version.py       # Schema versioning (stays at root - version control)
│
├── models/                  # Core model definitions
│   ├── __init__.py         
│   ├── config.py           # SUEWSConfig (from core.py)
│   ├── site.py             # Site, SiteProperties
│   ├── model.py            # Model physics configuration
│   ├── surface.py          # Surface properties
│   ├── state.py            # Initial states
│   └── human_activity.py   # Anthropogenic emissions
│
├── parameters/              # Physical parameter definitions
│   ├── __init__.py
│   ├── hydro.py            # Water/hydrology parameters
│   ├── ohm.py              # OHM coefficients
│   └── profile.py          # Temporal profiles
│
├── types/                   # Base types and utilities
│   ├── __init__.py
│   ├── base.py             # RefValue, Reference (from type.py)
│   ├── enums.py            # SurfaceType, TimezoneOffset (from timezone_enum.py)
│   └── utils.py            # Type utilities
│
├── validation/              # [Already organized]
│   └── ...
│
├── processors/              # Data processors
│   ├── __init__.py
│   ├── yaml_annotator.py   # YAML annotation
│   ├── json_annotator.py   # JSON annotation (from yaml_annotator_json.py)
│   └── yaml/               # YAML pipeline (from yaml_processor/)
│       └── ...
```

## Alternative (Simpler) Structure

If that's too much restructuring, a simpler approach:

```
src/supy/data_model/
├── __init__.py
├── _schema_version.py
│
├── models/                  # All core model definitions
│   ├── __init__.py
│   ├── config.py           # Main config (from core.py)
│   ├── site.py            
│   ├── model.py           
│   ├── surface.py         
│   ├── state.py           
│   ├── human_activity.py   
│   ├── hydro.py           
│   ├── ohm.py             
│   └── profile.py         
│
├── types/                   # Types and enums
│   ├── __init__.py
│   ├── ref_value.py        # RefValue, Reference
│   ├── surface_type.py     # SurfaceType enum
│   └── timezone.py         # TimezoneOffset enum
│
├── validation/              # [Keep as is]
│   └── ...
│
├── yaml_processor/          # [Keep as is]
│   └── ...
│
├── yaml_annotator.py        # Could stay at root or move to processors/
└── yaml_annotator_json.py   # Could stay at root or move to processors/
```

## Benefits of Organization

### Option 1: Full Reorganization
✅ **Clear separation**: models vs parameters vs types
✅ **Logical grouping**: Related concepts together
✅ **Scalable**: Easy to add new categories
❌ **More directories**: Potentially over-engineered

### Option 2: Simple Models + Types
✅ **Simple**: Just two main directories
✅ **Clear core models**: All in `models/`
✅ **Type safety**: Types separated
✅ **Less disruption**: Fewer changes needed

## Recommendation

**Go with Option 2 (Simple)** because:
1. Moves 9 core model files into `models/`
2. Moves 3 type files into `types/`
3. Reduces root level from 15 to ~5 files
4. Clear distinction: models vs types vs validation
5. Not over-engineered

## Impact Assessment

### High Value (definitely move):
- `core.py` → `models/config.py`
- `site.py` → `models/site.py`
- `model.py` → `models/model.py`
- `surface.py` → `models/surface.py`
- `state.py` → `models/state.py`
- `type.py` → `types/ref_value.py`
- `timezone_enum.py` → `types/timezone.py`

### Medium Value (consider moving):
- `human_activity.py` → `models/`
- `hydro.py` → `models/`
- `ohm.py` → `models/`
- `profile.py` → `models/`

### Keep at Root (for now):
- `_schema_version.py` (version control)
- `yaml_annotator.py` (might move later)
- `yaml_annotator_json.py` (might move later)

## Implementation Steps

1. Create `models/` and `types/` directories
2. Move files maintaining git history
3. Update all imports
4. Update `__init__.py` files for re-exports
5. Test everything works

Would you like me to implement Option 2?