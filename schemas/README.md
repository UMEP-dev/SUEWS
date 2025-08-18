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

### Automatic Schema Management

Schemas are managed by the **Schema Management and Protection** workflow (`.github/workflows/schema-management.yml`):

1. **Automatic Generation**: Triggered when:
   - Pydantic models in `src/supy/data_model/` are modified
   - Schema version is bumped in `src/supy/data_model/schema/version.py`
   - Push to master branch with relevant changes

2. **Protection**: The workflow:
   - Validates all PR changes to schemas
   - Prevents manual edits (only auto-generated changes allowed)
   - Adds headers marking files as auto-generated
   - Uses CODEOWNERS for additional review protection

3. **Deployment**: Automatically:
   - Commits generated schemas to repository
   - Deploys to GitHub Pages with version preservation
   - Creates PR previews for testing

### Manual Testing

For local testing only (changes won't be accepted in PRs):
```bash
python .github/scripts/generate_schema.py
```

## Important Notes

- **DO NOT MANUALLY EDIT** schema files - they are auto-generated
- **Never delete schema files** from this directory
- **Never modify published schema files** - create new versions instead
- **All changes must come through** the automated workflow
- **Protected by**:
  - GitHub Actions validation on every PR
  - CODEOWNERS requiring bot/admin approval
  - Auto-generated headers in files
  - Workflow validation checks
- **Document breaking changes** in CHANGELOG.md