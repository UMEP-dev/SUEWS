# Release Steps - Detailed

This workflow is **workspace-independent** - run from any git worktree with access to the repository. Step numbers match `prep-release/SKILL.md`.

## Step 0: Release Necessity Assessment

### Gather Metrics

```bash
# Last formal release info (exclude .dev tags)
LAST_TAG=$(git tag -l "[0-9]*.[0-9]*.[0-9]*" --sort=-v:refname | grep -v -E "dev" | head -1)
LAST_DATE=$(git log -1 --format=%ci "$LAST_TAG")
DAYS_SINCE=$(( ($(date +%s) - $(git log -1 --format=%ct "$LAST_TAG")) / 86400 ))

echo "Last release: $LAST_TAG ($DAYS_SINCE days ago, $LAST_DATE)"

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

# Note: If commit messages do not use these prefixes, review CHANGELOG.md and git log manually.

# Critical commits
git log ${LAST_TAG}..HEAD --format="%s" --no-merges | grep -iE "(critical|security|urgent|hotfix)" || echo "No critical commits"

# Ready PRs
gh pr list --state open --json number,title,mergeable --jq '.[] | select(.mergeable == "MERGEABLE") | "#\(.number): \(.title)"'
```

---

## Step 1: Select Dev Tag

Dev tags (`YYYY.M.D.dev`) are created by CI and represent tested commits. Releasing from a known-good dev tag skips redundant local testing.

```bash
# Fetch latest tags from any worktree
git fetch --tags origin
git tag -l "*.dev" --sort=-v:refname | head -10

# Check CI status for a specific tag
DEV_TAG="2026.1.25.dev"
gh run list --commit $(git rev-parse "$DEV_TAG") --json conclusion,name,createdAt \
  --jq '.[] | "\(.conclusion)\t\(.name)"'

# Ensure dev tag is in master history
git merge-base --is-ancestor "$DEV_TAG" origin/master && echo "[OK] in master lineage"
git log "${DEV_TAG}..origin/master" --oneline
```

### Selection Criteria

- **CI passed**: All workflows green (`build-publish_to_pypi.yml` / `build-wheels-reusable.yml`, tests, docs)
- **On master lineage**: Tag must be an ancestor of `origin/master`
- **Recent enough**: Contains the changes you want to release
- **No blockers**: No critical issues reported since the tag

---

## Step 2: Create Release Branch

Create a release branch from `origin/master` (no direct commits to `master`):

```bash
VERSION="$(date +%Y.%-m.%-d)"
git fetch origin master
git checkout -b "release/$VERSION" origin/master
git branch --show-current  # Should show: release/YYYY.M.D
```

---

## Step 3: Schema Version Audit and `.devN` Collapse

Inspect data-model changes since the last formal release:

```bash
LAST_RELEASE=$(git tag -l "[0-9]*.[0-9]*.[0-9]*" --sort=-v:refname | grep -v -E "dev" | head -1)
git log --oneline "${LAST_RELEASE}..HEAD" -- src/supy/data_model/
```

If any structural schema bump occurred during the cycle (`.dev1` .. `.devN`), perform the **Release-PR collapse step** per `.claude/rules/python/schema-versioning.md`:

1. **`src/supy/data_model/configuration/version.py`**:
   - Collapse `CURRENT_SCHEMA_VERSION` from `"YYYY.M.devN"` to plain CalVer `"YYYY.M"` (no leading zeros, e.g. `"2026.5"`).
   - Remove every `.devN` entry from `SCHEMA_VERSIONS` and add one consolidated `"YYYY.M"` entry summarising the union of deltas from the dev cycle.
2. **`src/supy/util/converter/yaml_upgrade.py`**:
   - Collapse all `(YYYY.M.devN, YYYY.M.devM)` and `(<prev-release>, YYYY.M.dev1)` entries in `_HANDLERS` into a single `(<prev-release>, "YYYY.M")` handler applying the ordered union of deltas (keeping human-readable `reason` strings on every dropped field).
   - Add the release-tag mapping to `_PACKAGE_TO_SCHEMA` (e.g. `"2026.5.10": "2026.5"`).
3. **Sample and regression fixtures**:
   - Update `schema_version` in `src/supy/sample_data/sample_config.yml` to `"YYYY.M"`.
   - Regenerate `test/fixtures/sample_short/sample_config_short.yml`:
     ```bash
     python test/fixtures/sample_short/make_sample_short.py
     ```
   - Vendor the collapsed release fixture under `test/fixtures/release_configs/${VERSION}.yml` (verified by `test/data_model/test_release_compat.py` and `test/data_model/test_yaml_upgrade.py`).

---

## Step 4: Benchmark Gate (PREREQUISITE)

Run the multi-version regression gate (`benchmark/REPRODUCE.md`) **before** merging the release PR or pushing the tag:

1. **Build the candidate** from the `build-suews` CI wheel artefact or `make dev` in a clean venv (do NOT install from PyPI -- `${VERSION}` is not published yet). Save `freeze.txt`.
2. **Generate a per-release config** valid in the candidate's own schema via a load -> dump round-trip (`SUEWSSimulation(canonical).config.model_dump()` -> `benchmark/inputs/config_${VERSION}.yml`).
3. **Run twice for reproducibility** against the restricted Zenodo forcing/obs record (`ZENODO_TOKEN` via env; never print or commit obs/forcing):
   ```bash
   python benchmark/run_benchmark.py --version "$VERSION"
   ```
   Require a byte-identical fingerprint across both runs.
4. **Evaluate regression**:
   ```bash
   python benchmark/assemble_index.py
   python benchmark/check_regression.py --candidate "$VERSION"
   ```
   - Exit `1`: regression beyond tolerances or lost metric coverage -> **HARD STOP** (do not deposit).
   - Exit `3`: within proposed tolerances, awaiting recorded maintainer review on the release issue.
   - Exit `0`: passes under an approved policy in `benchmark/regression_thresholds.json`.

---

## Step 5: CHANGELOG Analysis

Use `/log-changes` or inspect commits since the last formal release:

```bash
LAST_RELEASE=$(git tag -l "[0-9]*.[0-9]*.[0-9]*" --sort=-v:refname | grep -v -E "dev" | head -1)
git log "${LAST_RELEASE}..origin/master" --format="%ad||%h||%s" --date=format:"%d %b %Y" --no-merges
```

Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`, with governance status tags (`[experimental]`, `[stable]`, `[internal]`) on `[feature]` and `[change]` entries. Do not modify `## Annual Statistics` in `CHANGELOG.md`.

---

## Step 6: Update Documentation and Verify Knowledge Pack

### CHANGELOG.md and Version History RST

```bash
VERSION="YYYY.M.D"
PREV="2026.6.5"  # Latest existing version file under docs/source/version-history/

cp "docs/source/version-history/v${PREV}.rst" "docs/source/version-history/v${VERSION}.rst"
```

Edit `docs/source/version-history/v${VERSION}.rst`:
- Update title and release date.
- Keep `.. _new_latest:` on the new file and remove it from `v${PREV}.rst`.
- Add `v${VERSION}` at the top of the toctree in `docs/source/version-history/version-history.rst`.
- Use Sphinx extlinks roles (`:pr:`, `:issue:`, `:gh:`) for clickable references:
  - ``:pr:`1117` `` -> [#1117](https://github.com/UMEP-dev/SUEWS/pull/1117)
  - ``:issue:`1081` `` -> [#1081](https://github.com/UMEP-dev/SUEWS/issues/1081)
  - ``:gh:`245` `` -> [GH-245](https://github.com/UMEP-dev/SUEWS/issues/245)
- **Filter by governance status**: exclude `[experimental]` and `[internal]` entries; include `[stable]` and untagged features/changes plus bugfixes, maintenance, and docs.
- If a schema bump collapsed in Step 3, document the migration chain under "Breaking Changes" and align `docs/source/contributing/schema/schema_versioning.rst` and `docs/source/inputs/transition_guide.rst`.

### Knowledge Pack Freshness Gate

`src/supy/meson.build` builds the embedded knowledge pack (`manifest.json` and `chunks.jsonl.gz`) into `@OUTDIR@` on every wheel build (`build_always_stale: true`), so `src/supy/knowledge/pack/current/` is intentionally **untracked** in git -- never commit generated `.gz` pack artefacts into `src/supy/`. Verify pack generation cleanly against the release base via:

```bash
python scripts/lint/check_knowledge_pack_freshness.py --base "${LAST_RELEASE}"
```

---

## Step 6a: Docs Sanity Check

Regenerate the tracked JSON Schema artefact so its `version`, `title`, `$id`, and `schema_version` carry the released plain CalVer label (never `.devN`), then scan for stray dev schema labels per `.claude/rules/docs/release-docs-sanity.md`:

```bash
suews schema export -o docs/source/inputs/tables/schema.json

grep -rn "schema 2026\.[0-9]*\.dev\|v2026\.[0-9]*\.dev[0-9]\|\"version\": \"2026\.[0-9]*\.dev" \
  docs/source/ | grep -vi "collapsing\|development cycle\|dev1\.\.\|dev1.*dev14"
```

Any line printed by the scan is a stray `.dev` label to fix before opening the release PR (or dispatch the `docs-reviewer` agent in release mode).

---

## Step 7: GitHub Release Notes

Create `.github/releases/${VERSION}.md` in Markdown for the GitHub Releases page (installation, filtered highlights, citation info, `/stable/` documentation links). Key RST -> Markdown conversions:
- ``(:pr:`1038`)`` -> `(#1038)`
- `` ``code`` `` -> `` `code` ``
- `**bold**` -> `**bold**`

---

## Step 8: Issue Tracking and Sub-Issues

Update the parent release issue (`2-meta:release`) with the governance and benchmark-gate summary, and create linked GitHub sub-issues for manual post-merge steps:
- Submit PR and merge to `master` (closed by the release PR)
- Tag merge commit on `master`
- Verify PyPI, GitHub Release, Zenodo, and run `release-docs-anchor.yml`
- Record benchmark result in `benchmark/results/index.json` and `site/benchmark/`
- Update `UMEP-dev/umep-reqs` to `supy==${VERSION}`

---

## Step 9: Submit PR and Merge

```bash
git add CHANGELOG.md docs/source/version-history/ docs/source/inputs/tables/schema.json .github/releases/
# Include schema collapse, sample_short, and release_configs files if updated (never src/supy/knowledge/pack/current/)
git commit -m "docs: prepare release $VERSION"
git push -u origin "release/$VERSION"

gh pr create \
  --title "Release $VERSION" \
  --body "## Release $VERSION

- Collapsed schema version and updated migration/docs artefacts (if applicable)
- Passed pre-release benchmark gate
- Updated CHANGELOG.md, version history, schema.json, and GitHub Release notes"
```

Wait for all PR CI checks to pass, then merge to `master` using a merge commit.

---

## Step 10: Tag Release

Tag the merge commit on `master` (version format `YYYY.M.D`, no leading zeros):

```bash
git fetch origin master
MERGE_SHA=$(gh pr list --state merged --search "Release $VERSION" \
  --json mergeCommit --jq '.[0].mergeCommit.oid')

echo "Tagging commit: $MERGE_SHA"
git tag -a "$VERSION" "$MERGE_SHA" -m "Release $VERSION

Full changelog: https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md"
git push origin "$VERSION"
```

---

## Step 11: Post-Release Verification

Monitor (~20 min):
- **GitHub Actions**: `build-publish_to_pypi.yml` (each platform's `cp312-abi3` wheel build plus chained `api` test lane, `build_mcp`, and PyPI publish)
- **PyPI**: `supy==${VERSION}` appears (single `cp312-abi3` wheel per platform supporting `numpy>=1.22`; no separate `rc1` build)
- **GitHub Release**: published automatically with `.github/releases/${VERSION}.md`
- **Zenodo**: new version DOI minted
- **Smoke install**: `pip install --upgrade "supy==${VERSION}"` in a fresh venv

---

## Step 11a: Publish Stable Docs (`release-docs-anchor.yml`)

Once `docs-sync.yml` has updated the `rtd` branch with the merged release docs, dispatch `release-docs-anchor.yml` to build a clean, banner-free HTML anchor (`SUEWS_DOCS_VERSION_REF=${VERSION}`) and move the `${VERSION}` tag onto it:

```bash
gh workflow run release-docs-anchor.yml -f release_tag="$VERSION" --repo UMEP-dev/SUEWS
```

After Read the Docs finishes rebuilding `stable`, verify that the development banner is absent:

```bash
curl -fsSL https://docs.suews.io/stable/inputs/forcing-data.html \
  | grep -c "built from a development version"   # must print 0
```

Note: RTD `stable` rebuilds automatically when the tag moves, whereas the numbered `/YYYY.M.D/` RTD version may require clicking **Build version** in the RTD dashboard (see `republish-docs`).

---

## Step 12: Record Benchmark Result

Publish the derived benchmark statistics for `${VERSION}` (never commit raw observations or forcing):

1. Ensure `benchmark/inputs/config_${VERSION}.yml` and the updated `benchmark/results/index.json` (`python benchmark/assemble_index.py`) are committed on a post-release PR, along with `site/benchmark/` updates.
2. Upload the new version to the restricted Zenodo reproducibility stack.

---

## Step 13: Update UMEP Requirements

Update [UMEP-dev/umep-reqs](https://github.com/UMEP-dev/umep-reqs) to point directly at `supy==${VERSION}`:

```bash
cd /tmp && rm -rf umep-reqs
gh repo clone UMEP-dev/umep-reqs && cd umep-reqs
git checkout -b "update-supy-${VERSION}"

# Update pyproject.toml:
# From: "supy==OLD_VERSION",
# To:   "supy==${VERSION}",

git add pyproject.toml
git commit -m "chore(deps): update supy to ${VERSION}"
git push -u origin "update-supy-${VERSION}"
gh pr create --title "chore(deps): update supy to ${VERSION}" \
  --body "Updates supy to ${VERSION}."
```

**Note on retired `rc1` wheels**: Prior to 2026-04, a separate `YYYY.M.Drc1` wheel was built for `numpy<2.0`. Since the Rust bridge removed all NumPy C-ABI dependencies, the single `cp312-abi3` wheel supports `numpy>=1.22` across QGIS 3 LTR (NumPy 1.26.x), QGIS 4 (NumPy 2.x), and standard Python environments. Never tag or reference `rc1` for UMEP.

---

## Abort Release (Emergency Only, Before PyPI Publish)

```bash
git tag -d "$VERSION"
git push origin ":refs/tags/$VERSION"
```
