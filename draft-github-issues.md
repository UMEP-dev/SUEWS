# Draft GitHub Issues for Schema Versioning

## Issue 1: Add CLI validation command for YAML configurations

**Title**: Add `supy validate` command for YAML configuration validation

**Description**:
Now that we have JSON schema versioning in place, we should add a CLI command to validate YAML configuration files against their declared schema version.

**Tasks**:
- [ ] Add `validate` subcommand to supy CLI
- [ ] Read schema_version from YAML file
- [ ] Fetch corresponding schema (cached locally)
- [ ] Validate configuration against schema
- [ ] Provide clear error messages for validation failures
- [ ] Add --schema-version override option

**Example usage**:
```bash
supy validate config.yml
# ✓ Valid SUEWS configuration (schema version 0.1)

supy validate config.yml --schema-version 1.0
# ✗ Validation failed: property 'foo' not recognised
```

---

## Issue 2: Create schema migration tooling

**Title**: Implement automated migration between schema versions

**Description**:
As the schema evolves, users need tools to migrate their existing configurations to newer schema versions.

**Tasks**:
- [ ] Create migration framework in data_model.schema
- [ ] Implement 0.1 → 1.0 migration (when 1.0 is released)
- [ ] Add `supy migrate` CLI command
- [ ] Generate migration report showing changes
- [ ] Backup original file before migration
- [ ] Add --dry-run option

**Example**:
```bash
supy migrate config.yml --to-version 1.0
# Migrating from schema 0.1 to 1.0...
# - Renamed field 'old_name' to 'new_name'
# - Added default value for 'new_field'
# ✓ Migration complete. Original backed up to config.yml.bak
```

---

## Issue 3: Generate parameter documentation from schema

**Title**: Auto-generate parameter documentation from JSON schema

**Description**:
The JSON schema contains rich metadata about parameters. We should generate documentation automatically from this.

**Tasks**:
- [ ] Create schema documentation generator
- [ ] Extract parameter descriptions, types, constraints
- [ ] Generate RST files for Sphinx
- [ ] Include in ReadTheDocs build
- [ ] Add examples for each parameter type
- [ ] Cross-reference with scientific documentation

---

## Issue 4: VS Code extension for SUEWS YAML

**Title**: Create VS Code extension for SUEWS YAML editing

**Description**:
While basic schema validation works via $schema, a dedicated extension could provide enhanced features.

**Features**:
- [ ] Schema-aware autocomplete
- [ ] Inline documentation on hover
- [ ] Quick fixes for common issues
- [ ] Snippets for common configurations
- [ ] Validate against multiple schema versions
- [ ] Migration assistance

---

## Issue 5: Schema compatibility matrix documentation

**Title**: Document schema version compatibility with SUEWS releases

**Description**:
Users need to know which schema versions are compatible with which SUEWS releases.

**Tasks**:
- [ ] Create compatibility matrix table
- [ ] Add to schema registry page
- [ ] Document in ReadTheDocs
- [ ] Add compatibility check to supy
- [ ] Warn if schema/suews version mismatch

**Example matrix**:
| Schema Version | SUEWS Versions | Status |
|---------------|---------------|---------|
| 0.1 | 2023.1+ | Current |
| 1.0 | 2024.1+ | Planned |

---

## Issue 6: Schema testing improvements

**Title**: Add comprehensive schema validation tests

**Description**:
Ensure schema changes don't break existing configurations.

**Tasks**:
- [ ] Create test suite with valid/invalid configs
- [ ] Test all schema versions
- [ ] Test migration paths
- [ ] Test backward compatibility
- [ ] Add to CI/CD pipeline
- [ ] Performance benchmarks for validation

---

## Priority Order

1. **High Priority**: CLI validation command (Issue 1)
2. **High Priority**: Compatibility matrix (Issue 5)
3. **Medium Priority**: Migration tooling (Issue 2)
4. **Medium Priority**: Schema testing (Issue 6)
5. **Low Priority**: Documentation generation (Issue 3)
6. **Low Priority**: VS Code extension (Issue 4)