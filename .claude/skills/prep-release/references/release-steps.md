# Release Steps - Detailed

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

## Step 1: Create Tracking Issue

```bash
gh issue create --template release-checklist.md \
  --title "Release YYYY.M.D: [Feature Name]" \
  --label release
```

---

## Step 2: Pre-Flight Checks

### Invoke Skills

1. `verify-build` - meson.build, pyproject.toml, CI
2. `sync-docs` - Documentation matches code
3. `lint-code` - Code style compliance

### Core Checks

```bash
git branch --show-current        # Must be master
git status --porcelain           # Must be clean
git log origin/master..HEAD      # No unpushed commits
make test                        # Tests pass
make docs                        # Docs build
```

---

## Step 3: CHANGELOG Analysis

Use `log-changes` or:

```bash
git describe --tags --abbrev=0
git log $(git describe --tags --abbrev=0)..HEAD --format="%h %s" --no-merges
```

Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`

---

## Step 4: Update Documentation

### CHANGELOG.md

Add entry under today's date.

### Version History Page

```bash
VERSION="YYYY.M.D"
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

Update toctree, remove `new_latest` from previous.

---

## Step 5: Version

```bash
VERSION="$(date +%Y.%-m.%-d)"
git tag -l "$VERSION"  # Check availability
```

---

## Step 6: Commit & Tag

```bash
VERSION="2025.11.27"
TITLE="Feature Name"

git add CHANGELOG.md docs/source/version-history/
git commit -m "docs: update changelog and version history for $VERSION"
git push

git tag -a "$VERSION" -m "$TITLE

Key changes:
- Feature 1
- Feature 2

Full changelog: https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md"

git push origin $VERSION
```

---

## Step 7: Post-Release Verification

**Monitor (~20 min):**
- GitHub Actions: build_wheels, build_umep, publish
- PyPI: supy YYYY.M.D, supy YYYY.M.Drc1
- Zenodo DOI
- Test: `pip install --upgrade supy`

**24h monitoring:**
- Watch GitHub issues

---

## Abort Release

```bash
git tag -d $VERSION
git push origin :refs/tags/$VERSION
```

## RC Release

```bash
git tag -a "${VERSION}rc1" -m "Release candidate 1 for $VERSION"
git push origin "${VERSION}rc1"
```
