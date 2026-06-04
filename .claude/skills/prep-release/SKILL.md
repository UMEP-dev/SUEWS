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
3. **Schema version audit** - `git log <last-release-tag>..HEAD -- src/supy/data_model/`. If any commit is a structural change (see `.claude/rules/python/schema-versioning.md` for triggers), confirm that `CURRENT_SCHEMA_VERSION` was bumped, `SCHEMA_VERSIONS` has a matching entry, `sample_config.yml` carries the new version, and `yaml_upgrade.py::_HANDLERS` has a `(previous_schema -> current_schema)` handler (the handler registry is the single source of truth for compatibility; `is_schema_compatible` derives from it). Stop and fix if any are missing — this was the gap closed in gh#1304.
4. **CHANGELOG analysis** - Use log-changes or manual (current format)
5. **Update docs** - CHANGELOG.md, version history RST (`:pr:` syntax), toctree
6. **GitHub Release notes** - Create `.github/releases/YYYY.M.D.md` (Markdown)
7. **Issue tracking** - Update the release issue, create sub-issues for manual steps
8. **Submit PR** - Create PR, wait for CI, merge to master
9. **Tag release** - Tag the merge commit on master
10. **Verify** - Monitor Actions, PyPI, GitHub Release, Zenodo
11. **Benchmark the release** - Once the release is on PyPI, run the multi-version benchmark for the new version: per-release schema-valid config, reproducible fingerprint, regression check vs the previous release, then update the benchmark page + `benchmark/results/index.json` and version the Zenodo reproducibility stack. See `## Benchmark the release`.
12. **Update umep-reqs** - PR to update supy version in UMEP-dev/umep-reqs

Details: `references/release-steps.md`

## Issue Tracking and Sub-Issues

The release issue (labelled `2-meta:release`) tracks the overall release. Steps 0-6 are automated by the prep-release workflow and the PR. Steps 7-10 are **manual** and must not be closed by the PR merge.

When preparing a release:

1. **Update the release issue** with current version, progress checklist, and governance review results
2. **Create sub-issues** for each manual post-merge step:
   - Submit PR and merge to master (closed by the PR itself)
   - Tag merge commit on master (manual)
   - Verify PyPI, GitHub Release, Zenodo (manual)
   - Update umep-reqs with rc1 version (manual, separate repo)
3. **Link sub-issues** to the parent release issue using GitHub sub-issues

The parent release issue stays open until all sub-issues are closed. The PR should only close the "Submit PR" sub-issue, not the parent.

## Status Tags and Release Notes

CHANGELOG entries use status tags (`[experimental]`, `[stable]`, `[internal]`) on `[feature]` and `[change]` items. These tags control what appears in public-facing release documentation:

- **`[stable]`** and **untagged** entries -> included in release notes (RST and GitHub Release MD)
- **`[experimental]`** entries -> excluded from release notes; remain only in the CHANGELOG
- **`[internal]`** entries -> excluded from release notes
- **`[bugfix]`**, **`[maintenance]`**, **`[doc]`** entries -> always included (no status tag needed)

When writing version history RST and GitHub Release notes, filter the CHANGELOG to include only stable/untagged features and changes alongside all bugfixes, maintenance, and documentation entries. Do not expose experimental features in public release documentation.

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

## Release Checklist

```
[PASS/FAIL] No incomplete prior releases (all merged release PRs have tags)
[PASS/FAIL] Dev tag selected: YYYY.M.D.dev
[PASS/FAIL] CI status: All workflows passed
[PASS/FAIL] On master lineage
[PASS/FAIL] Release branch created
[PASS/FAIL] Schema version sync (sample_config.yml matches CURRENT_SCHEMA_VERSION; run /verify-build)
[PASS/FAIL] Schema version bump covers every structural data_model change since last tag (see .claude/rules/python/schema-versioning.md)
[PASS/FAIL] Knowledge pack rebuilt against HEAD (run `suews knowledge build --output src/supy/knowledge/pack/current --repo-root .` if data_model/ or cmd/ touched since last release; see gh#1406)
[PASS/FAIL] Docs updated (CHANGELOG, version-history RST)
[PASS/FAIL] GitHub Release notes created (.github/releases/)
[PASS/FAIL] Release issue updated, sub-issues created for manual steps
[PASS/FAIL] PR submitted and CI passed
[PASS/FAIL] PR merged to master
[PASS/FAIL] Git tag created on merge commit
[PASS/FAIL] GitHub Release published (triggers Zenodo DOI)
[PASS/FAIL] Release benchmarked: reproducible fingerprint, per-release schema-valid config, no energy-balance regression vs previous release (or regression reviewed and accepted)
[PASS/FAIL] Benchmark page + benchmark/results/index.json updated; Zenodo reproducibility stack versioned for the new release
[PASS/FAIL] umep-reqs PR created (UMEP-dev/umep-reqs)
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

## Single Wheel per Platform

Each tag triggers one PyPI upload per platform via GitHub Actions:
- `YYYY.M.D` - cp39-abi3 wheel (installs on cp39..cp3xx)

The runtime pin is `numpy>=1.22`, so the same wheel works in QGIS 3 LTR
(NumPy 1.26.4), QGIS 4 (NumPy 2.x), and any modern Python environment.
The UMEP (`rc1`) variant was retired in 2026-04 once the Rust bridge
removed all NumPy C-ABI dependencies.

## Benchmark the release

Every release is added to the multi-version regression benchmark (KCL/London, Ward 2016), so the suite shows performance across releases and flags regressions. The harness lives in `benchmark/`; full method in `benchmark/REPRODUCE.md`. Run this **after** the release is on PyPI (so `supy==<version>` installs).

Steps (one new release point):

1. **Pinned env** - `uv venv --python 3.12 benchmark/.venv-<version>`; `uv pip install "supy==<version>"`. Pin `pandas<3` for pre-2026 releases (their `read_forcing` breaks under pandas 3); keep `numpy` at the ABI the wheel was built against. Save `freeze.txt`.
2. **Per-release config** - generate a config valid in the release's OWN schema by a load->dump round-trip in that release's venv (`SUEWSSimulation(canonical).config.model_dump()` to `inputs/config_<version>.yml`); record `config_schema_on_load` + `config_hash`. `suews-convert` is NOT a YAML schema migrator. Never reuse a foreign-schema config silently.
3. **Run** - fetch obs + forcing from the restricted Zenodo record (token via the `ZENODO_TOKEN` secret/env; never print or commit it), then `run_benchmark.py` over the fixed period **twice**; require a byte-identical fingerprint (the reproducibility gate). Log an install/run failure with the exact error rather than inventing a number.
4. **Regression check** - compare the new release's MAE/MBE against the previous release; treat a material increase in any flux as a regression to review (this is the release gate, advisory unless agreed otherwise).
5. **Publish derived results** - `assemble_index.py` -> `benchmark/results/index.json`; update `site/benchmark/` (suite row + per-site detail) so the new release appears; add it as a new version in the restricted Zenodo reproducibility stack (and the data record if obs/forcing changed). Commit derived stats + the per-release config only; **never** commit obs/forcing (gitignored).

Governance gate: the fully-automated CI version needs the real observations on a **restricted production Zenodo record** plus a `ZENODO_TOKEN` GitHub secret and data-owner (Grimmond-group) sign-off. Until that lands, run this step manually against the restricted sandbox record; only derived statistics are ever published.

## References

- `benchmark/REPRODUCE.md` - Benchmark method + exact commands
- `references/release-steps.md` - Full step-by-step guide
- `references/assessment-criteria.md` - Decision scoring
- Repo: `dev-ref/RELEASE_MANUAL.md` - Complete manual (in repository root)
