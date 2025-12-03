---
name: prep-release
description: Prepare a SUEWS release with pre-flight checks, CHANGELOG analysis, and tag generation. Use when preparing to release a new version of SUEWS, when asked to create a release, or before tagging. Performs release necessity assessment, pre-flight validation (tests, docs, clean working tree), analyses commits since last release, suggests CHANGELOG entries, generates tag commands, and provides post-release verification checklist.
---

# SUEWS Release Preparation

Guide through release process per `dev-ref/RELEASE_MANUAL.md`.

## Step 0: Release Necessity Assessment

Gather data and apply decision criteria to determine if a release is warranted.

### 0a. Gather Release Metrics

```bash
# Last release info
LAST_TAG=$(git describe --tags --abbrev=0)
LAST_DATE=$(git log -1 --format=%ci "$LAST_TAG")
DAYS_SINCE=$(( ($(date +%s) - $(git log -1 --format=%ct "$LAST_TAG")) / 86400 ))

echo "Last release: $LAST_TAG ($DAYS_SINCE days ago)"

# Commit counts by type (analyse commit messages)
git log ${LAST_TAG}..HEAD --format="%s" --no-merges | \
  awk '
    /^feat/ { features++ }
    /^fix/ { fixes++ }
    /^docs/ { docs++ }
    /^refactor|^perf|^style|^test|^ci|^chore|^build/ { maintenance++ }
    END {
      print "Features:", features+0
      print "Bugfixes:", fixes+0
      print "Documentation:", docs+0
      print "Maintenance:", maintenance+0
      print "Total:", features+fixes+docs+maintenance+0
    }
  '

# Check for critical commits
git log ${LAST_TAG}..HEAD --format="%s" --no-merges | grep -iE "(critical|security|urgent|hotfix)" || echo "No critical commits"

# Open PRs ready to merge
gh pr list --state open --json number,title,mergeable --jq '.[] | select(.mergeable == "MERGEABLE") | "#\(.number): \(.title)"'
```

### 0b. Decision Criteria

**RELEASE NOW** (any one triggers):
- Critical bugfix merged (user-impacting bug)
- Security vulnerability patched
- External deadline imminent (conference, teaching, collaborator request)

**RELEASE RECOMMENDED** (score ≥5):
| Factor | Points |
|--------|--------|
| ≥1 feature commit | +3 |
| ≥2 bugfix commits | +2 |
| ≥1 user-facing change | +2 |
| >30 days since last release | +2 |
| >60 days since last release | +3 |
| User/collaborator request | +2 |

**WAIT** (any one triggers):
- <7 days since last release (unless critical)
- Only maintenance/docs commits
- Major feature branch not yet merged
- Tests failing on master

### 0c. Assessment Report Format

```
=== RELEASE ASSESSMENT ===

Last release: 2025.11.15 (13 days ago)

Commits since last release:
  Features:     2
  Bugfixes:     3
  Docs:         1
  Maintenance:  5
  Total:       11

Critical commits: None
Pending PRs (ready): #925, #927

Score: 7/10
  +3  features (2)
  +2  bugfixes (3)
  +2  days since release (>30)

Recommendation: RELEASE RECOMMENDED
Reason: Accumulated features and bugfixes provide user value.

Proceed? [Explain decision or ask user]
```

---

## Step 1: Create Release Tracking Issue

Create a GitHub issue from the release checklist template to track progress:

```bash
gh issue create --template release-checklist.md \
  --title "Release YYYY.M.D: [Feature Name]" \
  --label release
```

Then edit the issue to fill in Version, Title, and Why fields.

---

## Step 2: Pre-Flight Checks

### 2a. Invoke Related Skills

Before proceeding, run these skills for comprehensive validation:

1. **`verify-build` skill**: Check meson.build, pyproject.toml, CI matrix consistency
2. **`sync-docs` skill**: Verify documentation matches code
3. **`lint-code` skill**: Check code style compliance

### 2b. Core Checks

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
[PASS] verify-build: No issues
[PASS] sync-docs: Documentation consistent
[PASS] Tests pass
[PASS] Docs build
Ready: YES/NO
```

## Step 3: CHANGELOG Analysis

**Invoke the `log-changes` skill** for comprehensive CHANGELOG management, or use these commands for quick analysis:

```bash
# Find last release
git describe --tags --abbrev=0

# Commits since
git log $(git describe --tags --abbrev=0)..HEAD --format="%h %s" --no-merges
```

Categorise as (see `log-changes` skill for details):
- `[feature]`: New functionality
- `[bugfix]`: Bug fixes (link GitHub issue)
- `[change]`: User-facing changes
- `[maintenance]`: Internal/dev tooling
- `[doc]`: Documentation

## Step 4: Update Documentation

### 4a. Update CHANGELOG.md

Add entry under today's date with categorised changes.

### 4b. Create Version History Page

```bash
VERSION="YYYY.M.D"
# Create version history page
cat > docs/source/version-history/v${VERSION}.rst << 'EOF'
.. _new_latest:

Version YYYY.M.D
================

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
   :start-after: ## YYYY.M.D
   :end-before: ##
EOF
```

Update `docs/source/version-history/version-history.rst`:
- Add new page to toctree
- Remove `new_latest` label from previous version file

## Step 5: Version

Format: `YYYY.M.D` (date-based, single digits for month/day)

```bash
VERSION="$(date +%Y.%-m.%-d)"
# Check availability
git tag -l "$VERSION"
```

Creates dual versions:
- `2025.11.27` (standard, NumPy ≥2.0)
- `2025.11.27rc1` (UMEP, NumPy 1.x)

## Step 6: Commit & Tag

```bash
VERSION="2025.11.27"
TITLE="Feature Name"

# Commit documentation updates
git add CHANGELOG.md docs/source/version-history/
git commit -m "docs: update changelog and version history for $VERSION"
git push

# Create annotated tag
git tag -a "$VERSION" -m "$TITLE

Key changes:
- Feature 1
- Feature 2

Full changelog: https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md"

git push origin $VERSION
```

## Step 7: Post-Release Verification

Monitor (~20 min):
```
[ ] GitHub Actions: https://github.com/UMEP-dev/SUEWS/actions
    - build_wheels (standard)
    - build_umep (QGIS)
    - publish (PyPI)

[ ] PyPI: https://pypi.org/project/supy/
    - supy YYYY.M.D
    - supy YYYY.M.Drc1

[ ] Zenodo DOI (~10 min): https://zenodo.org/me/uploads

[ ] Test install: pip install --upgrade supy
```

24h monitoring:
```
[ ] Watch GitHub issues for problems
```

Announce (optional):
```
[ ] https://github.com/UMEP-dev/SUEWS/discussions
```

---

## Abort Release

If needed before CI completes:
```bash
git tag -d $VERSION
git push origin :refs/tags/$VERSION
```

## RC Release (Major Changes)

For release candidates:
```bash
git tag -a "${VERSION}rc1" -m "Release candidate 1 for $VERSION"
git push origin "${VERSION}rc1"
```

## References

- Full guide: `dev-ref/RELEASE_MANUAL.md`
- Hotfix process: `dev-ref/RELEASE_MANUAL.md#appendix-f-hotfix-and-patch-release-process`
- Issue template: `.github/ISSUE_TEMPLATE/release-checklist.md`
