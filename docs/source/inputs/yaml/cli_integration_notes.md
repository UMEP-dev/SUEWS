# Schema CLI Integration Notes

## Summary

Issues #612 and #613 have been successfully integrated into a single, unified CLI command: `suews-schema`. This approach provides better user experience and cleaner integration with the future suews-wizard (#544).

## Implementation Details

### Unified Command Structure

Instead of multiple separate commands as originally proposed:
- ❌ `supy schema-version`
- ❌ `supy schema-migrate`  
- ❌ `supy schema-validate`
- ❌ `supy validate`

We now have a single entry point with subcommands:
- ✅ `suews-schema info`
- ✅ `suews-schema version`
- ✅ `suews-schema validate`
- ✅ `suews-schema migrate`
- ✅ `suews-schema export`

### Benefits of This Approach

1. **Consistency**: Follows the existing `suews-*` CLI convention (suews-run, suews-convert, etc.)
2. **Discoverability**: All schema-related operations in one place
3. **Modularity**: Clean separation allows easy integration with suews-wizard
4. **Extensibility**: Easy to add new schema-related subcommands in future
5. **Maintainability**: Single module (`schema_cli.py`) contains all schema CLI logic

### Integration with suews-wizard (#544)

The current modular design facilitates wizard integration:

```python
# Future wizard can import and use these functions directly
from supy.cmd.schema_cli import (
    validate_file_against_schema,
    read_yaml_file,
    # ... other utilities
)

# Or call via subprocess for isolation
subprocess.run(['suews-schema', 'validate', config_file])
```

### Key Features Implemented

#### From Issue #612 (CLI utilities):
- ✅ Check and update schema versions
- ✅ Migrate configurations between versions
- ✅ Dry-run capability
- ✅ Backup creation
- ✅ Batch operations

#### From Issue #613 (validation):
- ✅ Validate against declared or specific schema version
- ✅ Multiple file validation
- ✅ CI/CD friendly with exit codes
- ✅ Multiple output formats (table, json, yaml)
- ✅ Verbose error reporting

### Technical Architecture

```
src/supy/cmd/
├── schema_cli.py          # New unified CLI module
├── validate_config.py     # Existing validation (kept for compatibility)
├── table_converter.py     # Table to YAML conversion
└── SUEWS.py              # Main SUEWS runner

src/supy/data_model/schema/
├── version.py            # Schema version management
├── migration.py          # Migration framework
└── publisher.py          # Schema generation and export
```

### Usage Examples

```bash
# Check versions across all configs
suews-schema version configs/*.yml

# Update all configs to current schema
suews-schema version configs/*.yml --update

# Validate for CI/CD
suews-schema validate configs/*.yml --strict

# Migrate with preview
suews-schema migrate old_config.yml --dry-run
suews-schema migrate old_config.yml --target-version 2.0

# Export schema for documentation
suews-schema export -o schema.json
```

### Future Enhancements

1. **Auto-detection**: Wizard could auto-detect and suggest migrations
2. **Interactive mode**: Add interactive prompts for migration choices
3. **Schema registry**: Could add online schema registry support
4. **Validation profiles**: Different validation strictness levels
5. **Schema diff**: Show differences between schema versions

## Conclusion

The unified `suews-schema` CLI successfully addresses both #612 and #613 in a single, cohesive implementation that:
- Provides all requested functionality
- Maintains consistency with existing CLI tools
- Facilitates future wizard integration
- Follows Python packaging best practices
- Offers excellent user experience with rich formatting

This implementation is ready for testing and deployment.