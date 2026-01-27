---
name: prep-release
description: Prepare SUEWS release with pre-flight checks and tag generation.
---

# Prep Release

Guide through release process per the repo's `dev-ref/RELEASE_MANUAL.md`.

## Triggers

- "Prepare release", "prep release", "release SUEWS"
- "Is it time to release?", "should we release?"
- "Create a new version", "tag a release"

## Workflow

This workflow is **workspace-independent** - run from any worktree.

0. **Assess necessity** - Use assessment criteria, check timing and user needs
1. **Select dev tag** - Pick a CI-verified dev tag as release base
2. **Create release branch** - `git checkout -b release/YYYY.M.D origin/master`
3. **CHANGELOG analysis** - Use log-changes or manual (current format)
4. **Update docs** - CHANGELOG.md, version history page and toctree
5. **Submit PR** - Create PR, wait for CI, merge to master
6. **Tag release** - Tag the merge commit on master
7. **Verify** - Monitor Actions, PyPI, GitHub Release, Zenodo

Details: `references/release-steps.md`

## Release Decision

Release when ready. Scoring is guidance only.

**RELEASE NOW** (any one):
- Critical bugfix / security patch
- External deadline

**RECOMMENDED** (score ≥5):
- Features (+3), Bugfixes (+2), User-facing change (+2), User/collaborator request (+2), >30 days (+2), >60 days (+3)

**WAIT** (any one):
- <7 days since last, only maintenance, failing tests

## Dev Tag Selection

Dev tags (`YYYY.M.D.dev`) are CI-verified commits. Release from a known-good dev tag to skip redundant local testing.

```bash
# List recent dev tags with CI status (run from any worktree)
git fetch --tags origin
git tag -l "*.dev" --sort=-v:refname | head -5
gh run list --branch master --limit 10
```

Selection criteria:
- CI passed (all workflows green)
- On master lineage
- Recent enough (includes desired changes)
- No critical issues reported since

## Pre-Release Checklist

```
[PASS/FAIL] Dev tag selected: YYYY.M.D.dev
[PASS/FAIL] CI status: All workflows passed
[PASS/FAIL] On master lineage
[PASS/FAIL] Release branch created
[PASS/FAIL] Docs updated (CHANGELOG, version-history)
[PASS/FAIL] PR submitted and CI passed
[PASS/FAIL] PR merged to master
Ready to tag: YES/NO
```

## Key Commands

```bash
# === Run from any worktree ===

# 1. Select dev tag
git fetch --tags origin
DEV_TAG="2026.1.25.dev"
gh run list --commit $(git rev-parse $DEV_TAG) --json conclusion,name

# 2. Create release branch
VERSION="$(date +%Y.%-m.%-d)"
git fetch origin master
git checkout -b release/$VERSION origin/master

# 3. Update docs, commit
git add CHANGELOG.md docs/source/version-history/
git commit -m "docs: update changelog and version history for $VERSION"

# 4. Submit PR
git push -u origin release/$VERSION
gh pr create --title "Release $VERSION" --body "Release documentation for $VERSION"

# 5. After PR merges, tag the merge commit
git fetch origin master
gh pr list --state merged --search "Release $VERSION" --json mergeCommit --jq '.[0].mergeCommit.oid'
# Use the merge commit SHA to tag
git tag -a "$VERSION" <merge-commit-sha> -m "Release title"
git push origin "$VERSION"

# Abort (if needed)
git tag -d "$VERSION" && git push origin ":refs/tags/$VERSION"
```

## Dual-Build System

Each tag triggers two PyPI uploads via GitHub Actions:
- `YYYY.M.D` - Standard build (NumPy ≥2.0, modern environments)
- `YYYY.M.Drc1` - UMEP build (NumPy 1.x compatibility for QGIS plugin)

The `rc1` variant is created automatically by the `build_umep` workflow - no manual RC tagging required for standard releases.

## References

- `references/release-steps.md` - Full step-by-step guide
- `references/assessment-criteria.md` - Decision scoring
- Repo: `dev-ref/RELEASE_MANUAL.md` - Complete manual (in repository root)
