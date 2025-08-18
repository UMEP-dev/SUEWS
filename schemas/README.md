# SUEWS Configuration Schemas

This directory contains the permanent archive of all SUEWS YAML configuration schema versions.

## Purpose

- **Permanent Storage**: All schema versions are stored here in the main repository
- **Source of Truth**: These files are the canonical schemas that get deployed to GitHub Pages
- **Version History**: Git history tracks all changes to schemas
- **Backward Compatibility**: Users can reference any historical version

## Directory Structure

```
schemas/
├── README.md                    # This file
└── suews-config/               # SUEWS configuration schemas
    ├── registry.json           # Registry of all versions with metadata
    ├── 0.1.json               # Version 0.1 (initial release)
    ├── 1.0.json               # Version 1.0 (future)
    └── latest.json            # Symlink/copy of current version
```

## Schema Versioning Policy

SUEWS uses **semantic versioning (major.minor)** for configuration schemas:

- **Major version (1.0 → 2.0)**: Breaking changes requiring migration
  - Removing required fields
  - Changing field types or structure
  - Renaming fields without backward compatibility
  
- **Minor version (1.0 → 1.1)**: Backward compatible additions
  - Adding new optional fields
  - Adding new validation rules that don't break existing configs
  - Extending enumerations with new values
  
**Note**: We use only major.minor versioning (e.g., 1.0, 1.1, 2.0), not patch versions.
Schema versions are independent of SUEWS release versions.

## URLs

Once deployed to GitHub Pages, schemas are accessible at:

- Specific version: `https://umep-dev.github.io/SUEWS/schema/suews-config/{version}.json`
- Latest version: `https://umep-dev.github.io/SUEWS/schema/suews-config/latest.json`
- Registry: `https://umep-dev.github.io/SUEWS/schema/suews-config/registry.json`

## Usage in YAML Files

```yaml
# Pin to specific version (recommended for production)
schema_version: "0.1"
$schema: "https://umep-dev.github.io/SUEWS/schema/suews-config/0.1.json"

# Or use latest (for development)
$schema: "https://umep-dev.github.io/SUEWS/schema/suews-config/latest.json"
```

## Workflow

1. **Schema Generation**: When SUEWS model changes require schema updates:
   - Update Pydantic models in `src/supy/data_model/`
   - Bump version in `src/supy/data_model/schema/version.py`
   - Run `python scripts/extract_schema_fixed.py` to generate new schema
   - **No build required**: Schema generation works without compiling SUEWS

2. **Deployment**: GitHub Actions workflow automatically:
   - Generates schemas without building (using standalone script)
   - Copies all schemas from this directory
   - Deploys to GitHub Pages with `keep_files: true`
   - Preserves all historical versions

## Important Notes

- **Never delete schema files** from this directory
- **Never modify published schema files** - create new versions instead
- **Always test** schema changes with example YAML files before release
- **Document breaking changes** in CHANGELOG.md