---
name: republish-docs
description: Republish or revise the Read the Docs pages for an ALREADY-RELEASED SUEWS version without cutting a new code release. Use when /stable/ (or a numbered /X.Y.Z/) shows a "Development Version" banner, a stale .dev schema label, or any doc fix that must reach a published release. Fixes docs on master, lets docs-sync refresh the rtd branch, runs the release-docs-anchor workflow to rebuild a banner-free anchor and move the tag, then verifies.
---

# Republish / revise released docs

Drive a documentation fix onto an **already-released** SUEWS version's Read the
Docs pages (`/stable/` and the numbered `/X.Y.Z/`) without cutting a new code
release. The published release tag is moved onto a freshly-built, banner-free
docs **anchor** commit; PyPI and Zenodo are untouched.

A new release uses `prep-release` instead (its step 11a runs the same anchor
workflow). How `latest`, `stable` and the numbered versions are built, and why
`stable` must not show the banner, is in
`.claude/rules/docs/release-docs-sanity.md`; read it first.

## Procedure

### 1. Fix the docs on master (PR)

Make the content fix on a normal branch and merge to `master`, so `latest` and
all future releases inherit it. If the fix touches schema labels, regenerate
`schema.json` and run the stray-dev-label scan, both as given in
`release-docs-sanity.md`.

### 2. Let docs-sync refresh the rtd branch

After the PR merges, `docs-sync.yml` runs automatically. Wait for it, then
confirm the fix reached `rtd` (the anchor builds FROM `rtd`):
```bash
gh run list --repo UMEP-dev/SUEWS --workflow docs-sync.yml --limit 1 \
  --json status,conclusion
git fetch origin rtd
git show origin/rtd:docs/source/inputs/tables/schema.json | grep -c "<stale-label>"   # want 0
```

### 3. Dispatch the anchor workflow

```bash
gh workflow run release-docs-anchor.yml -f release_tag=<X.Y.Z> --repo UMEP-dev/SUEWS
```
It checks out `rtd`, asserts the tag resolves to a clean version, does a FULL
clean rebuild (`rm -rf build` + `-E -a`), asserts the banner is absent, commits
an off-master anchor, and force-moves `<X.Y.Z>` onto it (pushing only the tag).

### 4. Monitor + verify the anchor

```bash
gh run watch <run-id> --repo UMEP-dev/SUEWS --exit-status
git fetch origin "refs/tags/<X.Y.Z>:refs/tags/<X.Y.Z>" --force
git ls-remote --tags origin <X.Y.Z>          # tag now points at the new anchor SHA
# the anchor's committed HTML is the source of truth (independent of RTD caching):
git show <X.Y.Z>:docs/build/html/inputs/forcing-data.html \
  | grep -c "built from a development version"   # want 0
```

### 5. Verify the live site after RTD rebuilds

RTD rebuilds asynchronously (a few minutes after the tag moves). Poll:
```bash
curl -fsSL https://docs.suews.io/stable/inputs/forcing-data.html \
  | grep -c "built from a development version"    # want 0
```

## Gotchas (all hit on the first real run; the workflow now guards them)

- **Banner grep must be banner-specific.** Grep `built from a development
  version` (only `docs/source/conf.py`'s announcement emits it), NOT the bare
  phrase `Development Version` -- that legitimately appears in `installation.rst`
  (TestPyPI dev builds) and `searchindex.js`, causing false positives.
- **The anchor build must be a FULL clean rebuild.** The `rtd` branch ships
  pre-built HTML carrying the `.dev` banner on every page. An incremental
  `sphinx-build` only re-renders changed pages, leaving unchanged pages with the
  stale banner. The `.dev` -> clean version flip is a theme-level change that
  does not invalidate per-page doctrees, so the workflow does `rm -rf build` and
  passes `-E -a`.
- **Never point a release tag at the `rtd` branch HEAD directly** -- its HTML
  was built with the `.dev` version and carries the banner. Always go through
  `release-docs-anchor.yml`.
- **The numbered `/X.Y.Z/` RTD version needs a MANUAL rebuild.** A tag
  *force-move* does not re-trigger the numbered version's build on RTD (RTD keys
  on the version name, which is unchanged). `stable` rebuilds automatically, but
  `/X.Y.Z/` keeps its old cached build until someone clicks **Versions ->
  X.Y.Z -> Build version** in the RTD dashboard. This step is human-only (no
  RTD-platform token in CI); surface it to the maintainer.

## References

- `.github/workflows/release-docs-anchor.yml` and `docs-sync.yml` -- the automation.
- `dev-ref/RELEASE_MANUAL.md` -- "Documentation on Read the Docs (release docs & revisions)".
- `.claude/rules/python/schema-versioning.md` -- schema-label collapse rules.
