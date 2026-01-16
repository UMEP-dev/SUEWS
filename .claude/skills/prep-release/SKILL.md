---
name: prep-release
description: Prepare SUEWS release with pre-flight checks and tag generation.
---

# Prep Release

Guide through release process per `dev-ref/RELEASE_MANUAL.md`.

## Workflow

| Step | Action | Details |
|------|--------|---------|
| 0 | **Assess** necessity | Score commits, check timing |
| 1 | **Create** tracking issue | `gh issue create --template release-checklist.md` |
| 2 | **Pre-flight** checks | verify-build, sync-docs, lint-code |
| 3 | **CHANGELOG** analysis | Use log-changes or manual |
| 4 | **Update** docs | CHANGELOG.md, version-history page |
| 5 | **Version** | Format: `YYYY.M.D` |
| 6 | **Commit & tag** | Push docs, create annotated tag |
| 7 | **Verify** | Monitor CI, PyPI, Zenodo |

Details: `references/release-steps.md`

## Release Decision

**RELEASE NOW** (any one):
- Critical bugfix / security patch
- External deadline

**RECOMMENDED** (score ≥5):
- Features (+3), Bugfixes (+2), >30 days (+2), >60 days (+3)

**WAIT** (any one):
- <7 days since last, only maintenance, failing tests

## Pre-Flight Checklist

```
[PASS/FAIL] On master branch
[PASS/FAIL] Clean working tree
[PASS/FAIL] verify-build: No issues
[PASS/FAIL] sync-docs: Docs consistent
[PASS/FAIL] Tests pass
[PASS/FAIL] Docs build
Ready: YES/NO
```

## Key Commands

```bash
# Version format
VERSION="$(date +%Y.%-m.%-d)"

# Create tag
git tag -a "$VERSION" -m "Release title"
git push origin $VERSION

# Abort (if needed)
git tag -d $VERSION && git push origin :refs/tags/$VERSION
```

## Dual-Build System

Each release creates:
- `YYYY.M.D` - Standard (NumPy ≥2.0)
- `YYYY.M.Drc1` - UMEP (NumPy 1.x)

## References

- `references/release-steps.md` - Full step-by-step guide
- `references/assessment-criteria.md` - Decision scoring
- `dev-ref/RELEASE_MANUAL.md` - Complete manual
