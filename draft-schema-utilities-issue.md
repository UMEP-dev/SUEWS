# GitHub Issue: Implement Schema Version Management Utilities

**Title**: Add CLI utilities for YAML schema version management and migration

**Labels**: enhancement, yaml-config, schema

**Description**:

Now that we have schema versioning in place (#603), we need user-friendly CLI utilities to help users manage schema versions in their YAML configurations.

## Current State

We have the building blocks in `src/supy/data_model/schema/`:
- `updater.py` - Updates schema versions in configs
- `migration.py` - Migrates between schema versions  
- `publisher.py` - Generates schemas

However, these are not easily accessible to users as CLI commands.

## Proposed Implementation

### 1. Schema Version Update Command

Create `supy schema-version` command:

```bash
# Check current schema version in file
supy schema-version check config.yml

# Update to current schema version (0.1)
supy schema-version update config.yml --current

# Set specific version
supy schema-version update config.yml --version 0.1

# Update all configs in directory
supy schema-version update-all ./configs/ --current

# Show available schema versions
supy schema-version list
```

### 2. Schema Migration Command

Create `supy schema-migrate` command:

```bash
# Check if migration needed
supy schema-migrate check config.yml

# Migrate to latest version
supy schema-migrate config.yml --to-latest

# Migrate to specific version
supy schema-migrate config.yml --to-version 1.0

# Dry run to see what would change
supy schema-migrate config.yml --dry-run

# Migrate with backup
supy schema-migrate config.yml --backup
```

### 3. Schema Validation Command

Create `supy schema-validate` command:

```bash
# Validate against declared schema version
supy schema-validate config.yml

# Validate against specific version
supy schema-validate config.yml --schema-version 0.1

# Validate all configs in directory
supy schema-validate ./configs/*.yml

# Validate with detailed error output
supy schema-validate config.yml --verbose
```

## Implementation Details

### Entry Points

Add to `pyproject.toml`:
```toml
[project.scripts]
supy-schema = "supy.cli.schema:main"
# Or integrate into main supy CLI
```

### Module Structure

```
src/supy/cli/
├── __init__.py
├── schema.py          # Main schema CLI
├── schema_version.py  # Version management commands
├── schema_migrate.py  # Migration commands
└── schema_validate.py # Validation commands
```

### Key Features

1. **User-friendly output**: Clear messages, progress indicators
2. **Safety first**: Always create backups before modifications
3. **Batch operations**: Process multiple files efficiently
4. **Dry run mode**: Preview changes before applying
5. **Integration**: Work with existing supy CLI structure

## Benefits

1. **Ease of use**: Simple commands instead of Python module invocations
2. **Discoverability**: Commands show up in `supy --help`
3. **Safety**: Built-in backup and dry-run features
4. **Consistency**: Unified CLI interface
5. **CI/CD friendly**: Easy to use in automation

## Testing

- Unit tests for each command
- Integration tests with sample configs
- Test migration paths between versions
- Test error handling and edge cases

## Documentation Updates

Update documentation in:
- `docs/source/inputs/yaml/schema_publishing.rst`
- `docs/source/inputs/yaml/schema_versioning.rst`
- CLI help text
- README examples

## Priority

Medium - Users can currently manage versions manually, but CLI tools would greatly improve user experience.

## Related Issues

- #603 - YAML configuration version tracking and schema management