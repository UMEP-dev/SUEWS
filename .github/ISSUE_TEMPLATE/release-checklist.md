---
name: Release Checklist
about: SUEWS release checklist. ONLY FOR DEVELOPERS!
title: 'Release YYYY.M.D: [Feature Name]'
labels: release
assignees: ''

---
**Version**: YYYY.M.D
**Title**: [Feature Name]
**Why**: <!-- Significant feature / critical fixes / teaching / conference -->

## Pre-Flight Checks

- [ ] **Clean state**: `git checkout master && git pull && git status` (clean)
- [ ] **Tests pass**: `make test` (all pass)
- [ ] **Docs build**: `make docs` (no errors)

## Create Release

- [ ] **Update CHANGELOG.md**: Add entry under today's date with `[feature]`, `[bugfix]`, `[change]` tags + issue links
- [ ] **Commit & push**: `git add CHANGELOG.md && git commit -m "docs: update changelog for YYYY.M.D" && git push`
- [ ] **Tag & push**:
```bash
VERSION="YYYY.M.D"
TITLE="Feature Name"
git tag -a "$VERSION" -m "$TITLE

Key changes:
- Change 1
- Change 2

Breaking changes: None

See CHANGELOG.md for details."
git push origin $VERSION
```

## Verify Deployment (~20 min)

- [ ] **GitHub Actions**: Check [workflows](https://github.com/UMEP-dev/SUEWS/actions) pass
- [ ] **PyPI**: Verify [supy](https://pypi.org/project/supy/) shows new version
- [ ] **GitHub Release**: Check [releases](https://github.com/UMEP-dev/SUEWS/releases) created automatically
- [ ] **Zenodo** (~10 min): Check [dashboard](https://zenodo.org/me/uploads) for DOI
- [ ] **Test install**: `pip install --upgrade supy` in fresh environment

## Monitor (24h)

- [ ] Watch GitHub issues for problems

## Announce (Optional)

- [ ] [UMEP Discussions](https://github.com/UMEP-dev/UMEP/discussions)

---

**RC Release?** (Major changes only): `git tag -a "YYYY.M.Drc1" -m "..." && git push origin YYYY.M.Drc1`
**Hotfix needed?** See [dev-ref/RELEASE_MANUAL.md Appendix F](../../dev-ref/RELEASE_MANUAL.md#appendix-f-hotfix-and-patch-release-process)
**Full guide**: [dev-ref/RELEASE_MANUAL.md](../../dev-ref/RELEASE_MANUAL.md)
