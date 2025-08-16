# Naming Alternatives for Model Definitions

## The Problem with "models"
- We're already in `data_model/` so `data_model/models/` is redundant
- "Model" is overloaded (data model, physics model, ML model)
- Doesn't clearly indicate these are configuration schemas

## Better Alternatives

### Option 1: `schemas/` ✓
```
data_model/
├── schemas/         # Configuration schemas
│   ├── config.py   # Main configuration
│   ├── site.py     # Site configuration
│   ├── surface.py  # Surface parameters
│   └── ...
```
**Pros**: Clear these are schema definitions
**Cons**: Might conflict with JSON schemas

### Option 2: `definitions/` or `defs/` ✓
```
data_model/
├── defs/           # Core definitions
│   ├── config.py
│   ├── site.py
│   └── ...
```
**Pros**: Short, clear, follows Python convention (like `typedefs`)
**Cons**: Maybe too abbreviated

### Option 3: `core/` ✓✓
```
data_model/
├── core/           # Core data structures
│   ├── config.py
│   ├── site.py
│   └── ...
```
**Pros**: Indicates these are the core/main definitions
**Cons**: "Core" is somewhat generic

### Option 4: `entities/`
```
data_model/
├── entities/       # Domain entities
│   ├── config.py
│   ├── site.py
│   └── ...
```
**Pros**: Domain-driven design term
**Cons**: Maybe too formal/enterprise-y

### Option 5: `structures/`
```
data_model/
├── structures/     # Data structures
│   ├── config.py
│   ├── site.py
│   └── ...
```
**Pros**: Clear these are data structures
**Cons**: A bit verbose

### Option 6: `config/` 
```
data_model/
├── config/         # Configuration structures
│   ├── main.py    # Main config (not config.py to avoid confusion)
│   ├── site.py
│   └── ...
```
**Pros**: Clear these are configuration-related
**Cons**: Might be confusing with actual config files

## Python Ecosystem Conventions

Looking at popular Python projects:

- **Pydantic**: Uses flat structure mostly
- **SQLAlchemy**: Uses `orm/`, `sql/`, `engine/`
- **Django**: Uses `models/` but it's at app level
- **FastAPI**: Uses `models/` or `schemas/`
- **Marshmallow**: Uses `schemas/`

## Recommendation

**Best options in order:**

1. **`core/`** - Clear, concise, indicates importance
2. **`defs/`** - Short, follows Python conventions
3. **`schemas/`** - Clear meaning, aligns with Pydantic/FastAPI

I'd go with **`core/`** because:
- It's clear these are the core data structures
- Not redundant with `data_model/`
- Short and easy to type
- Common in Python projects
- `from supy.data_model.core import SUEWSConfig` reads well

## Final Structure with `core/`

```
data_model/
├── __init__.py
├── _schema_version.py    # Version control
│
├── core/                 # Core data structures
│   ├── __init__.py
│   ├── config.py        # SUEWSConfig
│   ├── site.py         
│   ├── model.py        
│   ├── surface.py      
│   ├── state.py        
│   ├── human_activity.py
│   ├── hydro.py        
│   ├── ohm.py          
│   └── profile.py      
│
├── types/               # Type definitions
│   ├── __init__.py
│   ├── ref_value.py    
│   ├── enums.py        # All enums
│   └── base.py         # Base classes
│
├── validation/          # Validation logic
│   └── ...
│
├── processors/          # Data processors
│   ├── __init__.py
│   ├── yaml_annotator.py
│   ├── json_annotator.py
│   └── yaml_pipeline/   # From yaml_processor/
│       └── ...
│
└── README.md

```

This gives us:
```python
from supy.data_model.core import SUEWSConfig, Site
from supy.data_model.types import RefValue
from supy.data_model.validation import ValidationController
```

Clean and clear!