# YAML Versioning Strategy - Design Plan

## Executive Summary

**Current State**: Implemented dual version tracking (`version` for model, `config_version` for schema)

**Recommended State**: Single `schema_version` field tracking configuration structure only

**Rationale**: Reduce complexity while maintaining all benefits of version tracking

---

## Problem Analysis

### What We're Actually Solving

1. **Configuration Portability**: User receives YAML from colleague, needs to work across SUEWS versions
2. **Schema Evolution**: Fields get renamed, restructured, or deprecated over time  
3. **Debugging Support**: "Why doesn't this config work?" needs clear answers
4. **Backward Compatibility**: Old configs should work or have clear migration path

### What We're NOT Solving

- Tracking every SUEWS code change (that's what git tags are for)
- Forcing users to update configs with every release
- Creating version management burden for users

---

## Design Options Comparison

### Option 1: Current Implementation (Dual Version)
```yaml
version: 2025.8.15.dev325      # Model version
config_version: v1.0            # Schema version
```

**Pros:**
- Maximum information available
- Can detect both model and schema mismatches
- Already implemented

**Cons:**
- Confusing which version matters when
- Model version changes frequently (dev builds)
- Two concepts to explain to users

### Option 2: Schema Version Only (Recommended)
```yaml
schema_version: "1.0"           # Only track structure changes
```

**Pros:**
- Single, clear concept
- Changes rarely (only on breaking changes)
- Industry standard (Docker Compose, K8s)
- User-friendly

**Cons:**
- Less granular tracking
- Can't detect model-specific issues

### Option 3: No Explicit Version
```yaml
# No version field - rely on validation
```

**Pros:**
- Simplest for users
- No maintenance overhead

**Cons:**
- Hard to debug issues
- No clear migration path
- Poor user experience when things break

---

## Recommended Implementation Plan

### Phase 1: Simplify to Schema Version (Week 1)

#### 1.1 Data Model Changes
```python
class SUEWSConfig(BaseModel):
    name: str
    schema_version: Optional[str] = Field(
        default="1.0",
        description="Configuration schema version (not SUEWS model version)"
    )
    description: str
    # Remove 'version' and 'config_version' fields
```

#### 1.2 Schema Version Definition
```python
# src/supy/data_model/_schema_version.py
CURRENT_SCHEMA_VERSION = "1.0"

# When do we increment?
SCHEMA_VERSIONS = {
    "1.0": "Initial YAML schema with full data model",
    # Future: "1.1": "Added new field X",
    # Future: "2.0": "Breaking change: renamed Y to Z"
}
```

#### 1.3 Validation Logic
```python
@model_validator(mode="after")
def check_schema_compatibility(self):
    if not self.schema_version:
        self.schema_version = CURRENT_SCHEMA_VERSION
        return self
    
    if self.schema_version != CURRENT_SCHEMA_VERSION:
        # Provide migration guidance
        logger.info(f"Config uses schema {self.schema_version}, "
                   f"current is {CURRENT_SCHEMA_VERSION}")
        
        # Future: Auto-migration for known versions
        if self.schema_version in MIGRATION_HANDLERS:
            return migrate_config(self)
```

### Phase 2: Migration Support (Week 2)

#### 2.1 Migration Framework
```python
# src/supy/util/schema_migration.py
class SchemaMigrator:
    """Handle schema version migrations."""
    
    def migrate_0_9_to_1_0(self, config_dict):
        """Migrate pre-1.0 configs to 1.0 schema."""
        # Example: Rename old fields
        if 'old_field' in config_dict:
            config_dict['new_field'] = config_dict.pop('old_field')
        return config_dict
    
    def auto_detect_version(self, config_dict):
        """Detect schema version from structure."""
        # Heuristics to identify version
        if 'version' in config_dict:  # Current implementation
            return "0.9"  # Pre-schema version
        return config_dict.get('schema_version', '1.0')
```

#### 2.2 User-Facing Migration Tool
```bash
# Command-line migration
suews-migrate-config old_config.yml --output new_config.yml

# Python API
from supy.util import migrate_config
new_config = migrate_config('old_config.yml')
```

### Phase 3: Documentation & Transition (Week 3)

#### 3.1 Documentation Structure
```
docs/source/inputs/yaml/
├── schema_versioning.rst    # How versioning works
├── migration_guide.rst      # How to migrate configs
└── schema_changelog.rst     # What changed between versions
```

#### 3.2 Clear Versioning Policy
```markdown
## When Schema Version Changes

### Patch Version (1.0 -> 1.0.1)
- Never used for schema (backward compatible)

### Minor Version (1.0 -> 1.1)  
- New optional fields added
- Backward compatible changes
- Old configs work without modification

### Major Version (1.0 -> 2.0)
- Breaking changes (field renames, restructures)
- Migration required
- Clear migration documentation provided
```

---

## Implementation Strategy

### For Current PR (#603)

**Option A: Merge and Iterate**
1. Merge current implementation as v1
2. Simplify in follow-up PR
3. Maintain backward compatibility

**Option B: Revise Before Merge**
1. Simplify to single `schema_version` now
2. One clean implementation
3. No backward compatibility needed

**Recommendation**: Option B - Revise before merge

### Key Changes Needed

1. **Replace dual version with single `schema_version`**
2. **Remove model version tracking from YAML**
3. **Simplify validation to schema checking only**
4. **Update documentation to reflect simpler approach**
5. **Keep version update utility but simplify**

---

## Benefits of Simplified Approach

### For Users
- **One concept to understand**: "Does my config structure match?"
- **Rare updates**: Schema changes maybe once per year
- **Clear migration path**: When changes happen, tools help
- **No version churn**: Not tracking dev builds

### For Developers  
- **Clear change policy**: When to increment version
- **Decoupled from releases**: Can release SUEWS without schema changes
- **Migration framework**: Systematic way to handle evolution
- **Testable**: Can test migration paths

### For Maintainers
- **Sustainable**: Low maintenance overhead
- **Professional**: Follows industry patterns
- **Documented**: Clear versioning policy
- **Flexible**: Can add complexity later if needed

---

## Decision Points for Review

1. **Single vs Dual Version**: Do we need model version in YAML?
   - Recommend: No, schema version only

2. **Version Format**: Semantic (1.0.0) or simple (1.0)?
   - Recommend: Simple (1.0) for major.minor only

3. **Default Behavior**: What if no version specified?
   - Recommend: Assume current version, warn if structure doesn't match

4. **Migration Strategy**: Auto-migrate or require manual?
   - Recommend: Auto where possible, manual for complex cases

5. **Timeline**: Revise PR now or merge and iterate?
   - Recommend: Revise now for cleaner history

---

## Next Steps

If you approve this simplified approach:

1. I'll revise the current implementation
2. Implement single `schema_version` field
3. Create migration framework skeleton
4. Update all documentation
5. Simplify tests

The result will be a cleaner, more maintainable solution that better serves the scientific user community while providing all necessary version tracking capabilities.