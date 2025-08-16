# Schema Version Storage Specification

## Overview

This document specifies how SUEWS schema versions are stored, accessed, and managed across different contexts: source code, configuration files, published schemas, and runtime.

## Storage Locations & Purposes

### 1. Source Code Definition
**Location**: `src/supy/data_model/_schema_version.py`

```python
# Primary version definition
CURRENT_SCHEMA_VERSION = "1.0"

# Version history with descriptions
SCHEMA_VERSIONS = {
    "1.0": "Initial YAML schema with full Pydantic data model (2025.8)",
    "1.1": "Added optional field X for feature Y (2025.10)",  # Future
    "2.0": "Breaking change: Renamed field A to B (2026.1)"   # Future
}

# Compatibility matrix
COMPATIBLE_VERSIONS = {
    "1.0": ["1.0"],
    "1.1": ["1.0", "1.1"],  # 1.1 accepts 1.0 configs
    "2.0": ["2.0"]          # 2.0 breaks compatibility
}
```

**Purpose**: 
- Single source of truth for version information
- Centralised compatibility rules
- Version history documentation

### 2. Configuration Files
**Location**: User YAML configuration files

```yaml
# Required field at top level
name: my_urban_config
schema_version: "1.0"  # Must match or be compatible with CURRENT_SCHEMA_VERSION
description: Urban climate simulation
```

**Purpose**:
- Explicitly declare which schema the config follows
- Enable migration detection
- Prevent version mismatch errors

### 3. Published JSON Schemas
**Location**: `schemas/` directory structure

```
schemas/
├── README.md
├── latest -> v1.0/  # Symlink to current
├── v1.0/
│   ├── schema.json
│   ├── schema.yaml
│   └── README.md
├── v1.1/  # Future
└── archive/
    └── v0.9/  # Deprecated versions
```

**Schema Metadata** (in schema.json):
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://suews.org/schemas/1.0/config.json",
  "version": "1.0",
  "title": "SUEWS Configuration Schema v1.0",
  "properties": {
    "schema_version": {
      "const": "1.0",
      "description": "Configuration schema version"
    }
  }
}
```

**Purpose**:
- External validation tools
- IDE integration
- API consumption
- Historical reference

### 4. Runtime Storage
**Location**: In-memory during execution

```python
class SUEWSConfig(BaseModel):
    schema_version: Optional[str] = Field(
        default=None,
        description="Configuration schema version"
    )
    
    @model_validator(mode="after")
    def set_schema_version(self):
        if self.schema_version is None:
            self.schema_version = CURRENT_SCHEMA_VERSION
        return self
```

**Purpose**:
- Version validation during loading
- Migration triggers
- Compatibility checks

## Version Storage Best Practices

### 1. Version Format
- **Format**: `"MAJOR.MINOR"` (string)
- **Examples**: `"1.0"`, `"1.1"`, `"2.0"`
- **NOT**: `"1.0.0"`, `1.0`, `"v1.0"`
- **Rationale**: Simple, consistent, no patch level needed

### 2. Default Behaviour
```python
# When no version specified
if config.get("schema_version") is None:
    # Assume current version for backward compatibility
    config["schema_version"] = CURRENT_SCHEMA_VERSION
    logger.info(f"No schema version specified, assuming {CURRENT_SCHEMA_VERSION}")
```

### 3. Version Comparison
```python
def compare_versions(v1: str, v2: str) -> int:
    """Compare schema versions.
    Returns: -1 if v1 < v2, 0 if equal, 1 if v1 > v2
    """
    v1_parts = tuple(map(int, v1.split('.')))
    v2_parts = tuple(map(int, v2.split('.')))
    return (v1_parts > v2_parts) - (v1_parts < v2_parts)
```

## Storage Update Workflow

### When to Update Schema Version

1. **Update `CURRENT_SCHEMA_VERSION`** when:
   - Adding/removing required fields
   - Changing field types
   - Modifying validation rules
   - Restructuring configuration hierarchy

2. **Update Process**:
   ```bash
   # 1. Edit _schema_version.py
   CURRENT_SCHEMA_VERSION = "1.1"  # Increment
   
   # 2. Add to SCHEMA_VERSIONS
   "1.1": "Added irrigation scheduling options (2025.10)"
   
   # 3. Update COMPATIBLE_VERSIONS
   "1.1": ["1.0", "1.1"]  # If backward compatible
   
   # 4. Generate new schema
   python -m supy.util.schema_publisher bundle ./schemas
   
   # 5. Update sample configs
   python -m supy.util.update_schema_version --current sample_config.yml
   ```

### CI/CD Integration

**GitHub Actions** (`.github/workflows/publish-schema.yml`):
```yaml
on:
  push:
    branches: [master]
    paths:
      - 'src/supy/data_model/**'
  release:
    types: [published]

jobs:
  publish-schema:
    steps:
      - name: Generate schemas
        run: |
          version=$(python -c "from supy.data_model._schema_version import CURRENT_SCHEMA_VERSION; print(CURRENT_SCHEMA_VERSION)")
          python -m supy.util.schema_publisher bundle ./schemas
          
      - name: Update symlink
        run: |
          cd schemas
          rm -f latest
          ln -s v${version} latest
          
      - name: Commit schemas
        run: |
          git add schemas/
          git commit -m "chore: Update schemas to v${version}"
```

## Access Patterns

### 1. Reading Version in Python
```python
# From source
from supy.data_model._schema_version import CURRENT_SCHEMA_VERSION

# From config
config = SUEWSConfig.from_yaml("config.yml")
print(f"Config uses schema v{config.schema_version}")

# From JSON Schema
import json
with open("schemas/v1.0/schema.json") as f:
    schema = json.load(f)
    print(f"Schema version: {schema['version']}")
```

### 2. Validation Tools
```bash
# CLI validation
suews-validate check config.yml
# Output: Configuration valid (schema v1.0)

# Show version info
suews-validate info
# Output: Current schema version: 1.0
#         Available versions: 0.9, 1.0
```

### 3. Migration Detection
```python
def needs_migration(config_path: Path) -> bool:
    """Check if configuration needs migration."""
    with open(config_path) as f:
        config = yaml.safe_load(f)
    
    config_version = config.get("schema_version", "1.0")
    return config_version != CURRENT_SCHEMA_VERSION
```

## Storage Persistence

### 1. Git Version Tags
```bash
# Tag schema releases
git tag schema-v1.0 -m "Schema version 1.0 release"
git tag schema-v1.1 -m "Schema version 1.1 with irrigation"
```

### 2. Archive Policy
- Keep all schema versions in `schemas/` directory
- Move deprecated versions to `schemas/archive/` after 2 major versions
- Never delete schema files (users may need old versions)

### 3. Database Storage (Future)
```sql
-- For web services or multi-user systems
CREATE TABLE schema_versions (
    version VARCHAR(10) PRIMARY KEY,
    release_date DATE NOT NULL,
    description TEXT,
    is_current BOOLEAN DEFAULT FALSE,
    compatibility_json TEXT,
    schema_json TEXT NOT NULL
);
```

## Error Handling

### Version Mismatch
```python
try:
    validate_schema_version(config_version, strict=True)
except ValueError as e:
    print(f"Schema version error: {e}")
    print(f"Current version: {CURRENT_SCHEMA_VERSION}")
    print(f"Config version: {config_version}")
    print("Run: suews-validate migrate config.yml")
```

### Missing Version
```python
if "schema_version" not in config:
    warnings.warn(
        f"No schema_version field found. "
        f"Assuming v{CURRENT_SCHEMA_VERSION}. "
        f"Add 'schema_version: \"{CURRENT_SCHEMA_VERSION}\"' to your config.",
        UserWarning
    )
```

## Testing Requirements

### Version Storage Tests
```python
def test_version_format():
    """Ensure version follows MAJOR.MINOR format."""
    assert re.match(r'^\d+\.\d+$', CURRENT_SCHEMA_VERSION)

def test_version_history_complete():
    """Ensure all versions have descriptions."""
    for version in COMPATIBLE_VERSIONS.keys():
        assert version in SCHEMA_VERSIONS

def test_schema_files_exist():
    """Ensure schema files exist for current version."""
    schema_dir = Path(f"schemas/v{CURRENT_SCHEMA_VERSION}")
    assert schema_dir.exists()
    assert (schema_dir / "schema.json").exists()
```

## Summary

Schema versions are stored in:
1. **Source**: `_schema_version.py` (authoritative)
2. **Configs**: YAML field `schema_version` (declaration)
3. **Schemas**: `schemas/vX.Y/` directories (distribution)
4. **Runtime**: Pydantic model field (validation)

This multi-location approach ensures:
- Version tracking at every level
- Clear migration paths
- External tool compatibility
- Historical preservation