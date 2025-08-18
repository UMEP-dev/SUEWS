# Nightly Builds and Versioning System

## Overview

SUEWS/SuPy uses an automated nightly build system that creates development releases on TestPyPI. This document explains how the versioning works and how to troubleshoot common issues.

## Versioning Scheme

### Tag Format
- **Nightly builds**: `YYYY.M.D.dev` (e.g., `2025.8.17.dev`)
- **Production releases**: Standard semantic versioning (e.g., `2025.1.0`)

### Published Version Format
The published version on TestPyPI includes a commit count suffix:
- **At tag**: `YYYY.M.D.dev0` (the tag itself)
- **After tag**: `YYYY.M.D.devN` (where N is the number of commits after the tag)

Example:
- Tag created: `2025.8.17.dev`
- Published at tag: `2025.8.17.dev0`
- After 2 commits: `2025.8.17.dev2`

## Workflow Behavior

### Nightly Build Process (2 AM UTC)
1. GitHub Actions workflow triggers via cron schedule
2. **Creates a git tag FIRST** with format `YYYY.M.D.dev` (ensures correct version)
3. Builds wheels for all supported Python versions (3.9-3.13) and platforms
4. Publishes to TestPyPI with version `YYYY.M.D.dev0`

### Resilience Features
As of August 2025, the workflow has been updated to be more resilient:
- **Tag creation BEFORE build**: Tags are created before wheels are built to ensure version consistency
- **Tag creation**: Creates tags even if some platform builds fail (but not if cancelled)
- **TestPyPI deployment**: Proceeds with available wheels even if some platforms fail
- **Consecutive tags**: Ensures daily tags are created to maintain consistency

## Installation

### Latest Development Version
```bash
pip install -i https://test.pypi.org/simple/ supy
```

### Specific Development Version
```bash
pip install -i https://test.pypi.org/simple/ supy==2025.8.17.dev0
```

## Troubleshooting

### Missing Tags
If you notice gaps in nightly build tags (e.g., `2025.8.2.dev` is missing):
- Check the GitHub Actions run history for that date
- Prior to August 2025, tags were only created after fully successful builds
- After the update, tags are created even with partial build failures

### Version Number Inconsistencies (Fixed August 2025)
Prior to August 2025, version numbers on TestPyPI were inconsistent because:
1. **Tag timing issue**: Tags were created AFTER builds, causing version mismatches
2. **Example**: Aug 16 build used Aug 15 tag + 327 commits = `2025.8.15.dev327`

After the fix:
1. **Tags created BEFORE builds**: Ensures correct version numbering
2. **Commit count suffix**: Numbers after `.dev` indicate commits since the tag
3. **Build failures**: Tags are still created to maintain consecutive daily versions

### Checking Available Versions
```bash
# List all development versions
curl -s https://test.pypi.org/pypi/supy/json | jq -r '.releases | keys[]' | grep dev | sort -V

# Check specific date range
curl -s https://test.pypi.org/pypi/supy/json | jq -r '.releases | keys[]' | grep "2025\.8\." | sort -V
```

## Technical Implementation

### Version Generation (`get_ver_git.py`)
The version is generated dynamically using git describe:
1. Finds the most recent tag matching `[0-9]*`
2. Counts commits since that tag
3. For `.dev` tags, always appends the commit count for consistency
4. For production tags, only adds `.devN` if there are commits after the tag

### GitHub Actions Workflow
Key configuration in `.github/workflows/build-publish_to_pypi.yml`:
- **Schedule**: Runs at 2 AM UTC daily
- **Tag creation**: Creates `YYYY.M.D.dev` format tags
- **TestPyPI deployment**: Publishes development builds
- **PyPI deployment**: Only for non-dev tagged releases

## Best Practices

1. **Monitor nightly builds**: Check GitHub Actions for build status
2. **Use specific versions**: Pin to specific dev versions in testing environments
3. **Report issues**: If builds consistently fail, report to maintainers
4. **Clean up old versions**: TestPyPI has storage limits; old dev versions may be removed

## Related Files
- `.github/workflows/build-publish_to_pypi.yml` - CI/CD workflow
- `get_ver_git.py` - Version generation script
- `pyproject.toml` - Package configuration