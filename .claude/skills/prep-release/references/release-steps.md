# Release Steps - Detailed

This workflow is **workspace-independent** - run from any git worktree with access to the repository.

## Step 0: Release Necessity Assessment

### Gather Metrics

```bash
# Last release info
LAST_TAG=$(git describe --tags --abbrev=0)
LAST_DATE=$(git log -1 --format=%ci "$LAST_TAG")
DAYS_SINCE=$(( ($(date +%s) - $(git log -1 --format=%ct "$LAST_TAG")) / 86400 ))

echo "Last release: $LAST_TAG ($DAYS_SINCE days ago)"

# Commit counts by type
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

# Note: If commit messages do not use these prefixes, review the log manually.

# Critical commits
git log ${LAST_TAG}..HEAD --format="%s" --no-merges | grep -iE "(critical|security|urgent|hotfix)" || echo "No critical commits"

# Ready PRs
gh pr list --state open --json number,title,mergeable --jq '.[] | select(.mergeable == "MERGEABLE") | "#\(.number): \(.title)"'
```

### Assessment Report

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
```

---

## Step 1: Select Dev Tag

Dev tags (`YYYY.M.D.dev`) are created by CI and represent tested commits. Releasing from a known-good dev tag skips redundant local testing.

**Run from any worktree** - just need `git` and `gh` access to the repo.

### Find Candidate Tags

```bash
# Fetch latest tags from any worktree
git fetch --tags origin
git tag -l "*.dev" --sort=-v:refname | head -10

# Check CI status for a specific tag
DEV_TAG="2026.1.25.dev"
gh run list --commit $(git rev-parse $DEV_TAG) --json conclusion,name,createdAt \
  --jq '.[] | "\(.conclusion)\t\(.name)"'
```

### Verify Tag Lineage

```bash
# Ensure dev tag is in master history
git merge-base --is-ancestor $DEV_TAG origin/master && echo "OK: in master lineage"

# Show commits between dev tag and master HEAD (should be few/none)
git log ${DEV_TAG}..origin/master --oneline
```

### Selection Criteria

- **CI passed**: All workflows green (build_wheels, tests, docs)
- **On master lineage**: Tag must be ancestor of origin/master
- **Recent enough**: Contains the changes you want to release
- **No blockers**: No critical issues reported since the tag

### Selection Report

```
=== DEV TAG SELECTION ===

Candidate: 2026.1.25.dev
Commit: abc1234
Date: 2026-01-25 14:30:00

CI Status:
  build_wheels: passed
  test-full:    passed
  docs:         passed

Lineage: In master history
Commits since tag: 3 (all docs/minor)

Selected: YES
```

---

## Step 2: Create Release Branch

Create a release branch from `origin/master`. This can be done from any worktree.

```bash
VERSION="$(date +%Y.%-m.%-d)"

# Fetch latest master
git fetch origin master

# Create release branch (no need to be on master)
git checkout -b release/$VERSION origin/master

# Verify you're on the release branch
git branch --show-current  # Should show: release/YYYY.M.D
```

**Why a release branch?**
- Keeps docs changes separate from ongoing master work
- PR-based workflow for review and CI
- No direct commits to master
- Can be done from any worktree

---

## Step 3: CHANGELOG Analysis

Use `log-changes` or:

```bash
# Find last proper release (not dev tags)
LAST_RELEASE=$(git tag -l --sort=-v:refname | grep -v -E "(dev|rc)" | head -1)
git log ${LAST_RELEASE}..origin/master --format="%h %s" --no-merges
```

Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`

Current format in `CHANGELOG.md` uses date headings like `### 23 Jan 2026`.

---

## Step 4: Update Documentation

### CHANGELOG.md

Add entry under today's date.

### Version History Page

```bash
VERSION="YYYY.M.D"
PREV="2025.11.20"  # Update to latest existing version file

cp "docs/source/version-history/v${PREV}.rst" "docs/source/version-history/v${VERSION}.rst"
```

Edit the new file to:
- Update the title and release date
- Replace the narrative summary and highlights with the new release details
- Keep `.. _new_latest:` on the new file and remove it from the previous latest
- **Use `:pr:` roles for GitHub references** (configured in `conf.py` via `extlinks`)

Update `docs/source/version-history/version-history.rst` to add the new file at the top of the toctree.

### RST GitHub Link Syntax

Use Sphinx extlinks roles for clickable PR/issue references:

```rst
- Fixed biogenic CO2 calculations (:pr:`1117`)
- Resolved thread safety issue (:issue:`1081`)
- See :gh:`245` for background
```

Renders as:
- `:pr:`1117`` → [#1117](https://github.com/UMEP-dev/SUEWS/pull/1117)
- `:issue:`1081`` → [#1081](https://github.com/UMEP-dev/SUEWS/issues/1081)
- `:gh:`245`` → [GH-245](https://github.com/UMEP-dev/SUEWS/issues/245)

Configuration in `docs/source/conf.py`:
```python
extlinks = {
    "issue": ("https://github.com/UMEP-dev/SUEWS/issues/%s", "#%s"),
    "pr": ("https://github.com/UMEP-dev/SUEWS/pull/%s", "#%s"),
    "gh": ("https://github.com/UMEP-dev/SUEWS/issues/%s", "GH-%s"),
}
```

---

## Step 5: Submit PR

Commit docs changes and submit a PR for review.

```bash
VERSION="$(date +%Y.%-m.%-d)"

# Commit docs changes
git add CHANGELOG.md docs/source/version-history/
git commit -m "docs: update changelog and version history for $VERSION"

# Push release branch
git push -u origin release/$VERSION

# Create PR
gh pr create \
  --title "Release $VERSION" \
  --body "## Release Documentation

Updates for version $VERSION:
- CHANGELOG.md
- Version history page

Ready to merge and tag after CI passes."
```

### Wait for CI and Merge

- Wait for PR CI checks to pass
- Review docs if needed
- Merge PR to master (use merge commit, not squash)

---

## Step 6: Tag Release

After PR merges, tag the merge commit on master.

```bash
VERSION="2026.1.27"
TITLE="Feature Name"

# Get the merge commit SHA
MERGE_SHA=$(gh pr list --state merged --search "Release $VERSION" \
  --json mergeCommit --jq '.[0].mergeCommit.oid')

echo "Tagging commit: $MERGE_SHA"

# Create annotated tag on the merge commit
git tag -a "$VERSION" "$MERGE_SHA" -m "$TITLE

Key changes:
- Feature 1
- Feature 2

Full changelog: https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md"

# Push the tag
git push origin "$VERSION"
```

**Note**: Version format is `YYYY.M.D` with no leading zeros (e.g. `2026.1.7`).

---

## Step 7: Post-Release Verification

**Monitor (~20 min):**
- GitHub Actions: build_wheels, build_umep, publish
- PyPI: supy `YYYY.M.D` and `YYYY.M.Drc1` both appear
- GitHub Release: created automatically after successful publish
- Zenodo DOI appears on the dashboard
- ReadTheDocs build succeeds
- Test: `pip install --upgrade supy` in a fresh environment

**24h monitoring:**
- Watch GitHub issues

**Cleanup:**
```bash
# Delete local and remote release branch (optional)
git branch -d release/$VERSION
git push origin --delete release/$VERSION
```

---

## Abort Release

```bash
git tag -d $VERSION
git push origin :refs/tags/$VERSION
```

## RC Release (Testing Only)

RC tags are for pre-release testing. Standard releases use the base tag, and the UMEP build automatically publishes a `rc1` variant from that tag.

```bash
git tag -a "${VERSION}rc1" -m "Release candidate 1 for $VERSION"
git push origin "${VERSION}rc1"
```
