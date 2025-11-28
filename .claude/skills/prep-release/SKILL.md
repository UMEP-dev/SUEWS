---
name: prep-release
description: Prepare a SUEWS release with pre-flight checks, CHANGELOG analysis, and tag generation. Use when preparing to release a new version of SUEWS, when asked to create a release, or before tagging. Performs pre-flight validation (tests, docs, clean working tree), analyses commits since last release, suggests CHANGELOG entries, generates tag commands, and provides post-release verification checklist.
---

# SUEWS Release Preparation

Guide through release process per `dev-ref/RELEASE_MANUAL.md`.

## Release Workflow

### Step 1: Pre-Flight Checks

```bash
# Must pass before release
git branch --show-current        # Must be master
git status --porcelain           # Must be clean
git log origin/master..HEAD      # No unpushed commits
make test                        # Tests pass
make docs                        # Docs build
```

Report format:
```
[PASS] On master branch
[PASS] No uncommitted changes
[PASS] Tests pass
[PASS] Docs build
Ready: YES/NO
```

### Step 2: CHANGELOG Analysis

```bash
# Find last release
git describe --tags --abbrev=0

# Commits since
git log $(git describe --tags --abbrev=0)..HEAD --format="%h %s" --no-merges
```

Categorise as:
- `[feature]`: New functionality
- `[bugfix]`: Bug fixes (link GitHub issue)
- `[change]`: User-facing changes
- `[maintenance]`: Internal/dev tooling
- `[doc]`: Documentation

### Step 3: Version

Format: `YYYY.MM.DD` (date-based)

```bash
VERSION="$(date +%Y.%m.%d)"
# Check availability
git tag -l "$VERSION"
```

Creates dual versions:
- `2025.11.27` (standard, NumPy ≥2.0)
- `2025.11.27rc1` (UMEP, NumPy 1.x)

### Step 4: Tag Command

```bash
VERSION="2025.11.27"
git tag -a "$VERSION" -m "Release $VERSION

Key changes:
- Feature 1
- Feature 2

Full changelog: https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md"

git push origin $VERSION
```

### Step 5: Post-Release Checklist

```
[ ] GitHub Actions (~20 min)
    https://github.com/UMEP-dev/SUEWS/actions
    - build_wheels (standard)
    - build_umep (QGIS)
    - publish (PyPI)

[ ] PyPI
    https://pypi.org/project/supy/
    - supy 2025.11.27
    - supy 2025.11.27rc1

[ ] Zenodo DOI
    https://zenodo.org/me/uploads

[ ] (Optional) Announce
    https://github.com/UMEP-dev/SUEWS/discussions
```

## Abort Release

If needed before CI completes:
```bash
git tag -d $VERSION
git push origin :refs/tags/$VERSION
```

## Key Points

- CHANGELOG must have entry for today before tagging
- Tests and docs must pass
- Everything else (dual builds, PyPI, Zenodo) is automatic
- ~20 minutes from tag push to PyPI availability
