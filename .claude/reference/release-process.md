# SUEWS Release Process

## Overview

Every SUEWS release automatically creates **two PyPI versions** from a single git tag:

| Version Type | Version | NumPy | Target Users |
|--------------|---------|-------|--------------|
| Standard | `2024.10.7` | ≥2.0 | Standalone Python users |
| UMEP | `2024.10.7.post1` | 1.x | QGIS/UMEP plugin users |

## Creating a Release

### Standard Process

```bash
# 1. Create and push tag
git tag v2024.10.7
git push origin v2024.10.7

# CI automatically:
# - Builds 2024.10.7 with NumPy ≥2.0 (build_wheels job)
# - Builds 2024.10.7.post1 with NumPy 1.x (build_umep job)
# - Deploys both to PyPI (deploy_pypi job)
```

### Timeline

- Both builds run **in parallel** (~20 minutes total)
- Both deployed together to PyPI
- No manual steps required

## What Happens Automatically

### 1. Standard Build (`build_wheels` job)

- Builds wheels with NumPy ≥2.0
- Uses current `pyproject.toml` configuration
- Creates version `2024.10.7`
- Deployed to PyPI

### 2. UMEP Build (`build_umep` job)

**Modifications applied:**
- `pyproject.toml` NumPy requirements:
  - Build: `oldest-supported-numpy` (instead of `numpy>=2.0`)
  - Runtime: `numpy>=1.22,<2.0` (instead of `numpy>=2.0`)
- Version string: `.post1` suffix added via `BUILD_UMEP_VARIANT` env var

**Result:**
- Creates version `2024.10.7.post1`
- Binary compatible with NumPy 1.26.4 (QGIS 3.40 LTR)
- Deployed to PyPI

### 3. Deployment (`deploy_pypi` job)

- Waits for both build jobs
- Collects wheels from both
- Publishes all wheels to PyPI together
- Succeeds if either build succeeds

## UMEP Integration

### UMEP Side

UMEP requirements file specifies:
```python
supy==2024.10.7.post1
```

UMEP users run:
```bash
pip install -r umep-requirements.txt
```

They never see version details - it just works.

### SUEWS Side

- No public documentation of `.post1` versions
- `.post1` automatically exists for every production release
- No coordination needed with UMEP team
- They update their requirements file when ready

## Post-Release Tasks

### 1. Update CHANGELOG.md

```markdown
## 2024.10.7 (2025-10-08)

### Features
- [features]

### Bug Fixes
- [fixes]

---

## 2024.10.7.post1 (2025-10-08)

UMEP/QGIS build (NumPy 1.x). No source changes from 2024.10.7.
```

### 2. Create GitHub Release

Both versions in description:
```markdown
# SUEWS 2024.10.7

[Release notes]

## Installation

**Standalone users:**
```bash
pip install supy==2024.10.7
```

**UMEP/QGIS users:**
See UMEP documentation.
```

### 3. Notify UMEP Team (First Time Only)

After first `.post1` release:
```
From version 2024.10.7, SUEWS provides .post1 builds for QGIS compatibility.

Please update UMEP requirements to:
  supy==2024.10.7.post1

All future releases will automatically include .post1 versions.
```

## Technical Details

### Version String Generation

`get_ver_git.py` checks `BUILD_UMEP_VARIANT` environment variable:
```python
if os.environ.get('BUILD_UMEP_VARIANT') == 'true':
    version = version + '.post1'
```

Set by `build_umep` job in CI workflow.

### Build Configuration

**Standard build:**
- NumPy ≥2.0 at build time
- NumPy ≥2.0 at runtime
- Modern Python 3.10+

**UMEP build:**
- `oldest-supported-numpy` at build time (creates 1.x compatible binary)
- NumPy ≥1.22,<2.0 at runtime
- QGIS 3.40 LTR compatible (NumPy 1.26.4)

### Troubleshooting

**Q: What if one build fails?**
A: Deployment proceeds with available wheels. Independent builds mean one failure doesn't block the other.

**Q: Can I skip UMEP builds?**
A: Not recommended. Always create both for consistency. UMEP users expect `.post1` to exist.

**Q: How to test before production release?**
A: Use test tag (e.g., `v2024.10.test`) to trigger builds and check TestPyPI.

**Q: Version ordering on PyPI?**
A: `2024.10.7.post1` sorts after `2024.10.7` but before `2024.10.8`. Latest non-post version preferred by pip unless exact version specified.

## See Also

- `.github/workflows/build-publish_to_pypi.yml` - CI configuration
- `get_ver_git.py` - Version string logic
- GitHub issue #724 - Original UMEP compatibility discussion
