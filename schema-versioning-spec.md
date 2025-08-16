# SUEWS Schema Versioning Specification v1.0

## Executive Summary

SUEWS YAML configuration files use a **single schema version** field that tracks structural changes to the configuration format, independent of SUEWS model versions. This specification refines the current implementation based on the schema-plan.md requirements.

## Core Principles

### 1. Single Version Field
- **Field name**: `schema_version` (not `schemaVersion` as in plan - already implemented)
- **Format**: Semantic versioning without patch level: `"MAJOR.MINOR"` (e.g., `"1.0"`, `"1.1"`, `"2.0"`)
- **Location**: Top-level field in YAML configuration

### 2. Version Independence
- Schema versions are **completely independent** of SUEWS release versions
- Schema changes occur only when configuration structure changes
- Model physics changes do NOT trigger schema version changes

### 3. Versioning Rules

#### Major Version Changes (1.0 → 2.0)
Breaking changes requiring migration:
- Removing required fields
- Renaming fields (without aliases)
- Changing field types incompatibly
- Restructuring configuration hierarchy
- Changing validation rules that invalidate existing configs

#### Minor Version Changes (1.0 → 1.1)
Backward compatible additions:
- Adding optional fields with defaults
- Adding new validation warnings (not errors)
- Expanding allowed values in enums
- Adding field aliases for gentle migration
- Relaxing constraints

## Implementation Details

### Current State (✅ Implemented)
```yaml
# In configuration file
name: my_config
schema_version: "1.0"  # Required field
description: Urban climate simulation
```

```python
# In _schema_version.py
CURRENT_SCHEMA_VERSION = "1.0"
SCHEMA_VERSIONS = {
    "1.0": "Initial YAML schema with full Pydantic data model (2025.8)"
}
```

### Compatibility Matrix

```python
COMPATIBLE_VERSIONS = {
    "1.0": ["1.0"],           # Current
    "1.1": ["1.0", "1.1"],    # Future: 1.1 accepts 1.0 configs
    "2.0": ["2.0"],           # Future: 2.0 breaks compatibility
}
```

### Validation Behaviour

1. **Missing schema_version**: Assume current version (backward compatibility)
2. **Same version**: No warnings
3. **Compatible older**: Info message, continue
4. **Incompatible older**: Warning by default, error in strict mode
5. **Newer version**: Error (user needs newer SUEWS)

## Migration Framework

### Migration Path Discovery
```python
class SchemaMigrator:
    migration_handlers = {
        ("1.0", "1.1"): migrate_1_0_to_1_1,  # Future
        ("1.1", "2.0"): migrate_1_1_to_2_0,  # Future
    }
    
    def find_migration_path(from_ver, to_ver):
        # Find shortest path through version graph
        return path
```

### Migration Principles
1. **Preserve comments** when possible (ruamel.yaml)
2. **Warn about lossy changes**
3. **Generate migration report**
4. **Support dry-run mode**
5. **Validate post-migration**

## Governance

### Schema Evolution Process (SIP - SUEWS Improvement Proposal)

1. **Proposal Stage**
   ```markdown
   # SIP-XXX: Schema Change Title
   
   ## Problem
   What limitation does this address?
   
   ## Proposal
   Specific schema changes with examples
   
   ## Impact
   - Breaking change: Yes/No
   - Migration required: Yes/No
   - Affected users: Estimate
   
   ## Migration
   Step-by-step migration guide
   
   ## Examples
   Before/after configuration snippets
   ```

2. **Review Requirements**
   - Hydrology expert review (if hydro fields affected)
   - Micromet expert review (if met fields affected)
   - User impact assessment
   - Migration tool implementation

3. **Deprecation Policy**
   - Mark deprecated fields with `x-deprecated: true` in JSON Schema
   - Maintain deprecated fields for ≥1 minor version
   - Clear warnings with migration guidance
   - Document in changelog and migration guide

## JSON Schema Integration

### Schema Export with Version
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://suews.org/schema/1.0/config.json",
  "title": "SUEWS Configuration Schema v1.0",
  "properties": {
    "schema_version": {
      "type": "string",
      "const": "1.0",
      "description": "Configuration schema version"
    }
  }
}
```

### Version-specific Schemas
```
schemas/
├── 1.0/
│   ├── schema.json
│   └── schema.yaml
├── 1.1/  # Future
└── latest -> 1.0/  # Symlink
```

## CLI Tools Integration

### Validation with Version Check
```bash
suews-validate check config.yml
# Output: ✓ Valid configuration (schema v1.0)

suews-validate check old-config.yml
# Output: ⚠ Valid but uses schema v0.9 (current: v1.0)
#         Run: suews-validate migrate old-config.yml
```

### Migration Tool
```bash
suews-validate migrate config.yml --to 1.1
# Output: Migrated from v1.0 to v1.1
#         Changes:
#         - Added default value for new_field
#         - Renamed old_field to new_field
```

## Testing Requirements

### Version Detection Tests
```python
def test_auto_detect_version():
    # Test detection heuristics
    assert detect_version({}) == "1.0"  # No version field
    assert detect_version({"schema_version": "1.0"}) == "1.0"
    assert detect_version({"version": "old"}) == "0.9"  # Legacy
```

### Compatibility Tests
```python
def test_version_compatibility():
    assert is_compatible("1.0", "1.0") == True
    assert is_compatible("1.0", "1.1") == True  # Forward compat
    assert is_compatible("1.0", "2.0") == False
```

### Migration Tests
```python
def test_migration_preserves_data():
    original = load_config("v1.0.yml")
    migrated = migrate(original, "1.0", "1.1")
    # Verify no data loss
    assert_equivalent(original, migrated)
```

## Quick Release Checklist

- [ ] Update CURRENT_SCHEMA_VERSION if structure changed
- [ ] Add migration handler for version transitions
- [ ] Generate new JSON Schema for version
- [ ] Update compatibility matrix
- [ ] Test migration round-trip
- [ ] Document changes in migration guide
- [ ] Update sample configurations
- [ ] Tag schema release in git

## Decision Log

### Resolved Questions (from schema-plan.md)

1. **Q: Canonical artefact - Pydantic or JSON Schema?**
   **A: Pydantic models are source of truth**, JSON Schema is generated output
   - Rationale: Maintains Python-first development
   - JSON Schema consumable by external tools

2. **Q: Field naming - schemaVersion or schema_version?**
   **A: schema_version** (snake_case, already implemented)
   - Rationale: Consistent with Python/YAML conventions
   - Matches other field naming in config

3. **Q: Patch level in version (1.0.0 vs 1.0)?**
   **A: Two-level (1.0)** for simplicity
   - Rationale: Schema changes are rare
   - Patch level adds unnecessary complexity

### Open Questions

1. **Strictness level for physics checks**: Warnings or errors?
   - Current: Warnings by default
   - Consider: Strict mode flag for CI

2. **Migration tool behaviour**: Auto-migrate or require explicit?
   - Current: Explicit migration required
   - Consider: Auto-migrate with --auto flag

## Next Steps

1. **Immediate** (Current PR)
   - ✅ Basic versioning implemented
   - ✅ Migration framework ready
   - ⚠️ Need comprehensive tests

2. **Follow-up** (Separate PRs)
   - Add migration handlers for legacy configs
   - Implement deprecation warnings
   - Add property-based testing
   - Create migration cookbook

## References

- Original plan: `schema-plan.md`
- Implementation: `src/supy/data_model/_schema_version.py`
- Migration framework: `src/supy/util/schema_migration.py`
- PR: #603