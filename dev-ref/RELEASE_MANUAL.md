# SUEWS Release Manual

> **Purpose**: This manual provides comprehensive guidelines for SUEWS releases, from development through publication.
> **Audience**: Core developers, release managers, and contributors.
> **Last Updated**: August 2025

## Executive Summary

SUEWS adopts a **rolling release model** that reflects the continuous nature of research software development while providing stable reference points for academic citations.

**Key Principles:**
- **Release when ready**, not by calendar
- **No fixed schedules** or promises about release dates
- **Continuous development** with daily `.dev` builds
- **Feature-triggered releases** when significant work is complete
- **Academic citation support** via Zenodo DOI for each tagged release

## 1. Version Format

```
YYYY.M.D[.dev]
```

- **YEAR**: Full year (e.g., 2025)
- **MONTH**: Month of release (1-12)
- **DAY**: Day of release (1-31)
- **.dev**: Optional suffix for development builds

### Examples
- **Development build**: `2025.8.8.dev` (daily/continuous)
- **Tagged release**: `2025.8.15` (when feature is ready)

## 2. Release Philosophy

### Rolling Release Model

#### No Fixed Schedule
- Release when features are complete and tested
- No artificial deadlines or quarterly promises
- No pressure to meet calendar dates

#### Continuous Development
- Daily development builds continue as normal
- Every commit to master gets a `.dev` tag
- Development is transparent and continuous

#### Feature-Triggered Releases
When something significant is ready:
1. Remove `.dev` suffix from that day's version
2. Create GitHub release with descriptive title
3. Add release notes explaining the significance
4. Zenodo automatically assigns DOI for citation

### Example Timeline
```
2025.8.8.dev   → Daily development
2025.8.9.dev   → More development
2025.8.10.dev  → Feature complete
2025.8.10      → Tagged release: "Improved Snow Physics"
2025.8.11.dev  → Development continues
2025.8.15      → Tagged release: "Bug fixes for extreme events"
2025.8.16.dev  → Development continues
```

## 3. What Triggers a Release?

### Release When:
- ✅ **Significant feature** is complete and tested
- ✅ **Important bug fixes** that users need
- ✅ **Before conferences** for presentations
- ✅ **Collaborators need** a stable reference
- ✅ **Paper submissions** requiring specific version
- ✅ **Teaching needs** stable version for semester

### NOT Because:
- ❌ It's the first Tuesday of the month
- ❌ We promised a Q2 release
- ❌ It's been X weeks since last release
- ❌ Calendar says it's release day

## 4. Release Naming & Communication

The version number is just the date. The **significance** comes from descriptions:

### GitHub Release Title
```
2025.8.15: Improved Snow Module
2025.9.3: Critical Fix for QF Calculations
2025.10.1: New Anthropogenic Heat Methods
```

### Git Tag Message
```bash
git tag -a 2025.8.15 -m "Improved Snow Module

- Enhanced snow accumulation physics
- Fixed snow melt energy balance
- Added new snow density parameterisation"
```

### Changelog Entry
```markdown
## 2025.8.15 - Improved Snow Module
- [feature] Enhanced snow accumulation physics
- [feature] New snow density parameterisation
- [bugfix] Fixed snow melt energy balance
```

## 5. Distribution & Citation

### Distribution Channels

#### PyPI (Python Package Index)
- **Tagged releases**: Published immediately
- **Dev builds**: Daily, marked as pre-release
- **Installation**:
  - Stable: `pip install supy`
  - Dev: `pip install --pre supy`


#### GitHub Releases
- Source tarballs
- Release notes with meaningful titles
- Binary wheels for major platforms

### Academic Citation

#### Zenodo DOI (To Be Implemented)
> **Status**: Not yet configured. See Appendix D for setup instructions.

**When configured**:
- Each tagged release will automatically receive a DOI
- Permanent archival for reproducibility  
- Machine-readable metadata

#### Citation Format
```
SUEWS dev team (2025). SUEWS v2025.8.15: Improved Snow Module.
Zenodo. https://doi.org/10.5281/zenodo.XXXXXXX
```

#### BibTeX Entry
```bibtex
@software{suews_2025_8_15,
  author       = {SUEWS dev team},
  title        = {SUEWS: Surface Urban Energy and Water Balance Scheme},
  month        = 8,
  year         = 2025,
  version      = {2025.8.15},
  doi          = {10.5281/zenodo.XXXXXXX},
  url          = {https://github.com/UMEP-dev/SUEWS}
}
```

## 6. Quality Assurance

### Before Any Release
- ✅ All tests pass (`make test`)
- ✅ Documentation builds (`make docs`)
- ✅ Changelog updated
- ✅ Clean working directory

### Testing Levels
- **Every commit**: CI/CD automated tests
- **Before release**: Full test suite locally
- **Optional beta**: Share dev build for testing if needed

## 7. Practical Workflows

### Daily Development
```bash
# Normal development
git commit -m "Add feature X"
git push
# Automatic: Creates 2025.8.8.dev on PyPI
```

### Creating a Release
```bash
# When feature is ready and tested
git tag -a 2025.8.15 -m "Improved Snow Module

- Feature description
- Bug fixes
- Breaking changes (if any)"

git push origin 2025.8.15

# This triggers:
# - GitHub release creation
# - PyPI upload (without .dev)
# - Zenodo DOI assignment
```

### Post-Release
```bash
# Development continues immediately
git commit -m "Start next feature"
# Automatic: 2025.8.16.dev
```

## 8. Documentation Strategy

### Documentation Principles
- Documentation travels with code
- Each tagged release has corresponding docs
- Development builds use latest documentation
- Version switcher on ReadTheDocs

### Documentation Updates
- Inline during development
- Review before tagging release
- Can update after release if needed

## 9. Breaking Changes

### Handling Breaking Changes
- **Clear communication** in release title: "Breaking: API Changes"
- **Migration guide** in release notes
- **No special timing** - release when ready
- **Deprecation warnings** when possible (but not required)

### Example
```
2025.10.5: Breaking: New Configuration Format
- Old format still works with warning
- Migration guide included
- Will remove old format support in future (no specific date)
```

## 10. Benefits of Rolling Release

### For Developers
- No release deadline stress
- Natural development rhythm
- Release when quality is right
- No artificial feature freezes

### For Users
- Transparent development (daily progress visible)
- Features available when ready
- Clear communication about changes
- Can pin to specific versions for stability

### For Academia
- Every tagged version is citable (DOI)
- No waiting for quarterly releases
- Reproducible research with specific versions
- Stable versions available when needed for teaching


## 11. Step-by-Step Release Guide

### Pre-Release Preparation

#### 1. Decide if Release is Needed
Ask yourself:
- Is there a significant feature complete?
- Are there critical bug fixes users are waiting for?
- Is a stable version needed for teaching/conference/paper?

If yes to any → proceed with release.

#### 2. Prepare Your Environment
```bash
# Ensure on master branch
git checkout master
git pull origin master

# Ensure clean working directory
git status  # Should be clean
```

#### 3. Run Quality Checks
```bash
# Run full test suite
make test

# Build documentation
make docs

# Check for any deprecation warnings
python -W all -m pytest test/
```

### Creating the Release

#### 4. Update CHANGELOG.md
```bash
vim CHANGELOG.md
```

Add entry under current date:
```markdown
### 15 Aug 2025
- [feature] Brief description of main feature ([#XXX](github.com/UMEP-dev/SUEWS/issues/XXX))
  - Detailed point 1
  - Detailed point 2
- [bugfix] Fixed issue with... ([#YYY](github.com/UMEP-dev/SUEWS/issues/YYY))
- [change] Breaking change if any
```

#### 5. Commit Changes
```bash
git add CHANGELOG.md
git commit -m "docs: update changelog for 2025.8.15 release"
git push origin master
```

#### 6. Create Git Tag
```bash
# Format: YYYY.M.D (no leading zeros for month/day)
VERSION="2025.8.15"
TITLE="Improved Snow Module"  # Short, descriptive title

git tag -a "$VERSION" -m "$TITLE

Key changes:
- Enhanced snow accumulation physics
- Fixed snow melt energy balance
- Added new snow density parameterisation

Breaking changes:
- None (or list them)

See CHANGELOG.md for full details."
```

#### 7. Push Tag to GitHub
```bash
git push origin $VERSION
```

This automatically triggers:
- GitHub Actions build & test
- PyPI package upload
- ~~Zenodo DOI assignment~~ (not yet configured)

### Post-Release Tasks

#### 8. Verify Release
- Check [GitHub Actions](https://github.com/UMEP-dev/SUEWS/actions)
- Verify on [PyPI](https://pypi.org/project/supy/)
- ~~Check Zenodo for DOI~~ (not yet configured)

#### 9. Create GitHub Release
1. Go to [GitHub Releases](https://github.com/UMEP-dev/SUEWS/releases)
2. Click "Draft a new release"
3. Select your tag
4. Title: "SUEWS v2025.8.15: Improved Snow Module"
5. Description: Copy from tag message, add:
   - Installation: `pip install --upgrade supy`
   - Citation: Zenodo DOI link
   - Link to documentation

#### 10. Announce Release (Optional)
- SUEWS mailing list
- UMEP forum
- Social media if major feature

### Troubleshooting

#### If Tests Fail After Tagging
```bash
# Delete local tag
git tag -d $VERSION

# Delete remote tag
git push origin --delete $VERSION

# Fix issue, then retag
```

#### If PyPI Upload Fails
- Check GitHub Actions logs
- Verify PyPI credentials in repository secrets
- Manual upload: `python -m build && twine upload dist/*`

## 13. Release Decision Matrix

| Scenario | Action | Example |
|----------|--------|---------|
| Major feature complete | Tag release | New physics module ready |
| Critical bug fix | Tag release immediately | Calculation error affecting results |
| Minor bug fixes accumulated | Consider release | 5+ small fixes over 2 weeks |
| Conference/paper deadline | Tag release if stable | Team needs citable version |
| Teaching semester starting | Tag release | Stable version for students |
| Only minor tweaks | Keep as dev | Documentation typos, code cleanup |
| Experimental feature | Keep as dev | Not ready for production |

## 14. FAQ

**Q: How often will releases happen?**
A: When features are ready. Could be weekly, monthly, or longer gaps.

**Q: What if I need a stable version for teaching?**
A: Use any tagged release (without .dev). We can tag a release before semester if needed.

**Q: How do I know what changed?**
A: Check GitHub releases page - each release has a descriptive title and notes.

**Q: Can I request a release?**
A: Yes, if you need a stable version for a paper/conference, let us know.

**Q: What about long-term support?**
A: No formal LTS, but older versions remain on PyPI/Zenodo indefinitely.

## Appendix A: Quick Reference Card

### Release in 5 Minutes
```bash
# 1. Ensure tests pass
make test

# 2. Update changelog
vim CHANGELOG.md

# 3. Create tag with meaningful message
git tag -a 2025.8.15 -m "Title: Brief description

- Detail 1
- Detail 2"

# 4. Push tag
git push origin 2025.8.15

# 5. Verify on GitHub/PyPI/Zenodo
```

### Communication Templates

#### Email/Forum Announcement
```
SUEWS v2025.8.15 Released: Improved Snow Module

Install: pip install --upgrade supy
Details: https://github.com/UMEP-dev/SUEWS/releases/tag/2025.8.15
Cite: https://doi.org/10.5281/zenodo.XXXXXXX

Key changes:
- Enhanced snow physics
- Fixed energy balance bug
- Improved performance
```

---

## Appendix B: Technical Details

### GitHub Actions Workflow
The release workflow (`build-publish_to_pypi.yml`) triggers on tags:
- Builds wheels for all platforms
- Runs test suite
- Uploads to PyPI if tests pass
- Creates GitHub release draft

### Zenodo Integration (Not Yet Configured)
Once set up:
- Repository linked via GitHub-Zenodo integration
- DOI assigned automatically on release
- Metadata from GitHub repository
- Authors from `.zenodo.json` if present

### Version Resolution
Version determined by `get_ver_git.py`:
- Tags without .dev → formal release
- Commits after tag → adds .dev suffix
- Used by build system automatically

## Appendix C: Roles & Permissions

### Who Can Release?
- **Core team**: Full release permissions
- **Contributors**: Can suggest via PR
- **Release manager**: Designated person for major releases

### Required Permissions
- GitHub: Push tags to repository
- PyPI: Maintainer access (automated via GitHub Actions)
- Zenodo: Automatic via GitHub integration

## Appendix D: Setting Up Zenodo Integration

### One-Time Setup (Repository Admin Required)

1. **Go to Zenodo**
   - Visit https://zenodo.org/account/settings/github/
   - Log in with GitHub (must have admin access to UMEP-dev/SUEWS)

2. **Enable Repository**
   - Find "UMEP-dev/SUEWS" in the list
   - Toggle the switch to ON

3. **Configure Metadata (Optional)**
   Create `.zenodo.json` in repository root:
   ```json
   {
     "creators": [
       {"name": "Sun, Ting", "orcid": "0000-0000-0000-0000"},
       {"name": "Grimmond, Sue"},
       {"name": "Ward, Helen"},
       {"name": "Omidvar, Hamidreza"}
     ],
     "license": "GPL-3.0",
     "title": "SUEWS: Surface Urban Energy and Water Balance Scheme",
     "keywords": ["urban climate", "energy balance", "hydrology", "meteorology"]
   }
   ```

4. **Test with Next Release**
   - Create a GitHub release (not just a tag)
   - Zenodo will automatically archive and assign DOI
   - DOI appears in Zenodo dashboard

### Benefits Once Configured
- Automatic DOI for every release
- Permanent archive (even if GitHub goes down)
- Professional citations for academic papers
- Version-specific DOIs for reproducibility

---

*This manual guides SUEWS releases using a sustainable rolling release model.*
*Last updated: August 2025*
*Owner: SUEWS Development Team*
*Location: `/dev-ref/RELEASE_MANUAL.md`*