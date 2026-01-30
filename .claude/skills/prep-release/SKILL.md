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
4. **Update docs** - CHANGELOG.md, version history RST (`:pr:` syntax), toctree
5. **GitHub Release notes** - Create `.github/releases/YYYY.M.D.md` (Markdown)
6. **Submit PR** - Create PR, wait for CI, merge to master
7. **Tag release** - Tag the merge commit on master
8. **Verify** - Monitor Actions, PyPI, GitHub Release, Zenodo

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
[PASS/FAIL] No incomplete prior releases (all merged release PRs have tags)
[PASS/FAIL] Dev tag selected: YYYY.M.D.dev
[PASS/FAIL] CI status: All workflows passed
[PASS/FAIL] On master lineage
[PASS/FAIL] Release branch created
[PASS/FAIL] Docs updated (CHANGELOG, version-history RST)
[PASS/FAIL] GitHub Release notes created (.github/releases/)
[PASS/FAIL] PR submitted and CI passed
[PASS/FAIL] PR merged to master
[PASS/FAIL] Git tag created on merge commit
[PASS/FAIL] GitHub Release published (triggers Zenodo DOI)
Ready: YES/NO
```

## Common Mistakes

### Incomplete release: PR merged but no tag created

The most common release failure is merging the release PR (step 6) and then forgetting steps 7-8 (tag and verify). Without the tag:
- No wheels are built or published to PyPI
- No GitHub Release or Zenodo DOI is created
- The CHANGELOG references a version that does not exist on PyPI

**Recovery:** Complete the release by tagging the merge commit on master:

```bash
git fetch origin master
MERGE_SHA=$(gh pr list --state merged --search "Release YYYY.M.D" \
  --json mergeCommit --jq '.[0].mergeCommit.oid')
git tag -a "YYYY.M.D" "$MERGE_SHA" -m "Release YYYY.M.D"
git push origin "YYYY.M.D"
```

### Starting a new release when a previous one is incomplete

Before starting a new release, check for incomplete releases:

```bash
# Compare merged release PRs to existing tags
gh pr list --state merged --search "Release" --limit 5 --json title,mergeCommit
git tag -l "[0-9]*.[0-9]*.[0-9]*" --sort=-v:refname | head -5
```

If a previous release PR was merged but never tagged, complete that release first. Starting a new release on top of an incomplete one creates confusion in the CHANGELOG and version history.

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

# 3. Update docs (use :pr:`XXX` in RST, #XXX in Markdown)
# - CHANGELOG.md
# - docs/source/version-history/v$VERSION.rst
# - .github/releases/$VERSION.md (GitHub Release notes)
git add CHANGELOG.md docs/source/version-history/ .github/releases/
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
