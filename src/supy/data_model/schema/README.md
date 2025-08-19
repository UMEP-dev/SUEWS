# Schema Management System

This directory contains the schema versioning and management system for SUEWS/SuPy YAML configuration files.

## Overview

The schema system provides:
- **Version tracking** for configuration file formats
- **Backward compatibility** checking between schema versions
- **Migration tools** for updating configs between versions
- **JSON Schema export** for validation and IDE support
- **Automated publishing** to GitHub Pages

## Components

### Core Modules

#### `version.py`
- Defines `CURRENT_SCHEMA_VERSION` constant
- Central source of truth for the current schema version
- Used by all other schema modules

#### `__init__.py`
- Main API entry point for schema operations
- Exports key functions:
  - `is_schema_compatible()` - Check version compatibility
  - `validate_schema_version()` - Validate with warnings/errors
  - `get_schema_compatibility_message()` - Get user-friendly messages
  - `SchemaMigrator` - Handle config migrations
  - `migrate_config_file()` - File-level migration helper

#### `migration.py`
- `SchemaMigrator` class for config migrations
- Handles transitions between schema versions
- Auto-detects version from config structure
- Migration strategies:
  - `0.9` → `1.0`: Dual-version to single schema_version
  - Future migrations added here

#### `exporter.py`
- Exports Pydantic models to JSON Schema
- Generates schema files for GitHub Pages
- Creates versioned schema URLs
- Supports IDE validation via `$schema` field

#### `publisher.py`
- GitHub Pages deployment utilities
- Creates index.html for schema browsing
- Manages `latest` symlink for stable versions
- Handles dev vs production deployments

#### `updater.py`
- YAML file schema version updates
- `update_yaml_schema_version()` - Update file in place
- `increment_schema_version()` - Version bumping utility
- Handles cleanup of old version fields

## Schema Versioning Strategy

### Version Format
- Format: `MAJOR.MINOR` (e.g., `1.0`, `1.1`, `2.0`)
- **Major version**: Breaking changes, incompatible
- **Minor version**: Backward compatible additions

### Compatibility Rules
1. Same major version = compatible
2. Different major version = incompatible
3. Minor version increments preserve compatibility
4. Missing version assumed to be current

### Version Lifecycle
```
Config Creation → Version Assignment → Validation → Migration (if needed) → Processing
```

## Usage Examples

### In Configuration Files
```yaml
# Specify schema version for compatibility
schema_version: "1.0"

# Optional: Enable IDE validation
$schema: "https://umep-dev.github.io/SUEWS/schema/suews-config/1.0.json"

# Configuration content
model:
  ...
sites:
  ...
```

### In Python Code
```python
from supy.data_model.schema import (
    validate_schema_version,
    SchemaMigrator,
    CURRENT_SCHEMA_VERSION
)

# Validate version
validate_schema_version("1.0", strict=True)

# Migrate config
migrator = SchemaMigrator()
new_config = migrator.migrate(old_config, to_version="1.0")
```

### Command Line Tools
```bash
# Export schema to JSON
python -m supy.data_model.schema.exporter

# Update YAML file schema version
python -c "from supy.data_model.schema import update_yaml_schema_version; update_yaml_schema_version('config.yml', '1.0')"
```

## CI/CD Integration

### GitHub Actions Workflow
The `.github/workflows/publish-schema.yml` workflow:
1. **Builds** schema on every push/PR
2. **Validates** schema generation
3. **Deploys** to GitHub Pages for:
   - Production releases (tags without 'dev')
   - Development releases (nightly builds, dev tags)
4. **Attaches** schema files to GitHub releases

### Deployment URLs
- **Production**: `https://umep-dev.github.io/SUEWS/schema/suews-config/{version}.json`
- **Latest stable**: `https://umep-dev.github.io/SUEWS/schema/suews-config/latest.json`
- **Development**: Published with dev tags, not linked as latest

## Migration Guide

### From Dual-Version System (pre-1.0)
Old format:
```yaml
version: "2025.8.1"
config_version: "v1.0"
```

New format:
```yaml
schema_version: "1.0"
```

Migration is automatic when loading old configs.

### Adding New Migrations
1. Update `migration.py` with new migration function
2. Add to `MIGRATION_FUNCTIONS` dict
3. Increment `CURRENT_SCHEMA_VERSION` in `version.py`
4. Update tests in `test/data_model/test_schema_versioning.py`

## Testing

Run schema tests:
```bash
pytest test/data_model/test_schema_versioning.py -v
```

Test coverage includes:
- Version compatibility checking
- Migration paths
- YAML file updates
- Schema export functionality
- Sample config validation

## Development Guidelines

### Adding Schema Changes

1. **Backward Compatible Changes** (minor version):
   - Adding optional fields
   - Adding new model options
   - Relaxing validation constraints
   
2. **Breaking Changes** (major version):
   - Removing fields
   - Changing field types
   - Renaming required fields
   - Tightening validation

### Version Update Process
1. Update `CURRENT_SCHEMA_VERSION` in `version.py`
2. Add migration if needed in `migration.py`
3. Update sample configs
4. Run tests to ensure compatibility
5. Document changes in CHANGELOG

## Troubleshooting

### Common Issues

**Schema version mismatch warning**
- Config uses different schema version
- Check compatibility with `is_schema_compatible()`
- Migrate if needed with `SchemaMigrator`

**IDE validation not working**
- Ensure `$schema` field points to correct URL
- Check schema is published to GitHub Pages
- Verify JSON Schema export is valid

**Migration fails**
- Check if migration path exists for versions
- Ensure config structure matches expected format
- Review migration logs for specific errors

## Future Enhancements

- [ ] Automatic schema inference from code
- [ ] Schema diffing tool for version comparison
- [ ] Interactive migration wizard
- [ ] Schema validation in pre-commit hooks
- [ ] Multi-version schema testing framework

## References

- [JSON Schema Specification](https://json-schema.org/)
- [Pydantic Schema Export](https://docs.pydantic.dev/latest/usage/json_schema/)
- [SUEWS Configuration Guide](https://suews.readthedocs.io/configuration)