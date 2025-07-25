# Tasks: Add Changelog to Documentation Site

## Overview

This document outlines the implementation tasks for adding the CHANGELOG.md to the SUEWS documentation site. Tasks are organised in priority order and include clear completion criteria.

## Implementation Tasks

### Phase 1: Preparation and Verification

#### TASK-001: Verify MyST Parser Configuration
**Priority**: High  
**Status**: Pending  
**Description**: Ensure MyST parser is properly configured in Sphinx setup  
**Acceptance Criteria**:
- [ ] Check `docs/source/conf.py` for MyST parser in extensions
- [ ] Verify markdown file support is configured
- [ ] Test MyST parser with a simple markdown include
- [ ] Document any missing configuration

**Implementation**:
```bash
# Check current configuration
grep -n "myst_parser" docs/source/conf.py
grep -n "source_suffix" docs/source/conf.py
```

#### TASK-002: Verify CHANGELOG.md Location and Format
**Priority**: High  
**Status**: Pending  
**Description**: Confirm CHANGELOG.md exists and review its structure  
**Acceptance Criteria**:
- [ ] Confirm CHANGELOG.md exists in repository root
- [ ] Review markdown syntax used (headers, links, lists)
- [ ] Identify any non-standard markdown that might need attention
- [ ] Check file size and number of versions documented

**Implementation**:
```bash
# Verify file and check size
ls -la CHANGELOG.md
wc -l CHANGELOG.md
head -50 CHANGELOG.md
```

### Phase 2: Core Implementation

#### TASK-003: Create changelog.rst File
**Priority**: High  
**Status**: Pending  
**Description**: Create the changelog.rst file that includes CHANGELOG.md  
**Acceptance Criteria**:
- [ ] Create `docs/source/changelog.rst`
- [ ] Add proper title and description
- [ ] Implement include directive with MyST parser
- [ ] Add :orphan: directive to prevent TOC warnings

**Implementation**:
```rst
:orphan:

=========
Changelog
=========

This page contains the complete history of changes for SUEWS.

For the most up-to-date changes, see the `CHANGELOG.md on GitHub <https://github.com/UMEP-dev/SUEWS/blob/master/CHANGELOG.md>`_.

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
```

#### TASK-004: Update Main Index Page
**Priority**: High  
**Status**: Pending  
**Description**: Add changelog reference to main documentation index  
**Acceptance Criteria**:
- [ ] Add changelog link to appropriate section of index.rst
- [ ] Ensure link text is clear and descriptive
- [ ] Position prominently for visibility
- [ ] Test link functionality

**Implementation Steps**:
1. Open `docs/source/index.rst`
2. Locate appropriate section (likely "Quick Links" or similar)
3. Add changelog reference with clear description

#### TASK-005: Update Developer Section Navigation
**Priority**: High  
**Status**: Pending  
**Description**: Add changelog to developer/contributor documentation section  
**Acceptance Criteria**:
- [ ] Modify `docs/source/recent-development/index.rst`
- [ ] Add changelog to the toctree
- [ ] Position logically within developer resources
- [ ] Ensure proper indentation and formatting

**Implementation Steps**:
1. Open `docs/source/recent-development/index.rst`
2. Locate the toctree directive
3. Add `../changelog` entry in appropriate position

### Phase 3: Testing and Validation

#### TASK-006: Local Documentation Build Test
**Priority**: High  
**Status**: Pending  
**Description**: Build documentation locally to test changelog integration  
**Acceptance Criteria**:
- [ ] Documentation builds without errors
- [ ] Changelog appears in built HTML
- [ ] All markdown elements render correctly
- [ ] Navigation links work as expected

**Implementation**:
```bash
cd docs
make clean
make html
# Open _build/html/changelog.html in browser
```

#### TASK-007: Link Verification
**Priority**: Medium  
**Status**: Pending  
**Description**: Verify all links within the changelog remain functional  
**Acceptance Criteria**:
- [ ] GitHub issue links work correctly
- [ ] Pull request links are valid
- [ ] Any external links are functional
- [ ] Relative links (if any) resolve correctly

**Implementation**:
- Manual testing of key links
- Consider automated link checking if many links exist

#### TASK-008: Cross-browser Testing
**Priority**: Medium  
**Status**: Pending  
**Description**: Test changelog rendering across different browsers  
**Acceptance Criteria**:
- [ ] Test in Chrome/Chromium
- [ ] Test in Firefox
- [ ] Test in Safari (if on macOS)
- [ ] Verify responsive design on mobile viewport

### Phase 4: Finalisation

#### TASK-009: Update Documentation Build Instructions
**Priority**: Low  
**Status**: Pending  
**Description**: Update any developer documentation about docs building  
**Acceptance Criteria**:
- [ ] Check if build instructions need updates
- [ ] Document any new dependencies (if any)
- [ ] Note changelog inclusion in documentation guide

#### TASK-010: Create Pull Request
**Priority**: High  
**Status**: Pending  
**Description**: Create PR with all changes for review  
**Acceptance Criteria**:
- [ ] All changes committed with clear messages
- [ ] PR description references issue #537
- [ ] Include testing steps in PR description
- [ ] Add screenshots of rendered changelog

**PR Description Template**:
```markdown
## Summary
- Adds CHANGELOG.md to documentation site as requested in #537
- Changelog appears in developer section and is referenced from main index
- Uses Sphinx include directive to maintain single source of truth

## Changes
- Created `docs/source/changelog.rst` to include CHANGELOG.md
- Updated `docs/source/index.rst` with changelog reference  
- Updated `docs/source/recent-development/index.rst` navigation

## Testing
1. Build docs locally: `cd docs && make clean html`
2. Navigate to changelog in built documentation
3. Verify all sections render correctly
4. Test navigation links

## Screenshots
[Include screenshots of rendered changelog]
```

## Rollback Plan

If issues arise, rollback involves:
1. Remove `docs/source/changelog.rst`
2. Revert changes to `index.rst`
3. Revert changes to `recent-development/index.rst`
4. Rebuild documentation

## Success Metrics

- [ ] Documentation builds successfully with changelog included
- [ ] No increase in build warnings or errors
- [ ] Changelog is accessible from main navigation
- [ ] All existing documentation functionality preserved
- [ ] Positive feedback from issue reporter and maintainers

### Phase 5: Changelog Automation Implementation

#### TASK-011: Choose Automation Approach
**Priority**: High  
**Status**: Pending  
**Description**: Decide between Claude bot automation, PR-enforced updates, or hybrid approach  
**Acceptance Criteria**:
- [ ] Review team preferences and constraints
- [ ] Evaluate Claude bot availability and API access
- [ ] Consider PR velocity and contributor experience
- [ ] Document decision and rationale

#### TASK-012: Implement Changelog Validation Workflow
**Priority**: High  
**Status**: Pending  
**Description**: Create GitHub Action to check for changelog updates in PRs  
**Acceptance Criteria**:
- [ ] Create `.github/workflows/changelog-check.yml`
- [ ] Implement logic to detect CHANGELOG.md modifications
- [ ] Add exemption logic for docs-only and CI-only changes
- [ ] Configure appropriate error messages
- [ ] Test with sample PRs

**Implementation**:
```yaml
name: Changelog Check
on:
  pull_request:
    types: [opened, synchronize]

jobs:
  check-changelog:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - name: Check Changelog Modified
        run: |
          if git diff --name-only origin/${{ github.base_ref }}..HEAD | grep -q "CHANGELOG.md"; then
            echo "✅ Changelog updated"
          else
            echo "❌ Please update CHANGELOG.md"
            exit 1
          fi
```

#### TASK-013: Configure Branch Protection Rules
**Priority**: High  
**Status**: Pending  
**Description**: Update master branch protection to require changelog check  
**Acceptance Criteria**:
- [ ] Add changelog-check to required status checks
- [ ] Configure bypass rules for admins if needed
- [ ] Document the new requirement in contributing guide
- [ ] Test with a sample PR

#### TASK-014: Implement Claude Bot Integration (if chosen)
**Priority**: Medium  
**Status**: Pending  
**Description**: Set up Claude bot to automatically generate changelog entries  
**Acceptance Criteria**:
- [ ] Create custom GitHub Action for Claude integration
- [ ] Implement PR analysis logic
- [ ] Configure Claude API credentials securely
- [ ] Set up automatic PR creation for changelog updates
- [ ] Add error handling and fallback mechanisms

**Implementation Structure**:
```
.github/
├── workflows/
│   └── changelog-automation.yml
└── actions/
    └── claude-changelog/
        ├── action.yml
        ├── requirements.txt
        └── main.py
```

#### TASK-015: Create Changelog Entry Templates
**Priority**: Medium  
**Status**: Pending  
**Description**: Provide templates to help contributors write good changelog entries  
**Acceptance Criteria**:
- [ ] Create `.github/PULL_REQUEST_TEMPLATE/feature.md` with changelog section
- [ ] Add examples of good changelog entries
- [ ] Document changelog format in CONTRIBUTING.md
- [ ] Include guidance on categorisation (Added, Changed, Fixed, etc.)

**Template Example**:
```markdown
## Changelog Entry

Please add your changes to CHANGELOG.md under the [Unreleased] section:

### Added
- [ ] New features or capabilities

### Changed  
- [ ] Changes to existing functionality

### Fixed
- [ ] Bug fixes

### Removed
- [ ] Deprecated features removed
```

#### TASK-016: Implement Conventional Commits Support (optional)
**Priority**: Low  
**Status**: Pending  
**Description**: Add support for generating changelog from conventional commits  
**Acceptance Criteria**:
- [ ] Document conventional commit format in contributing guide
- [ ] Create script to parse commit messages
- [ ] Integrate with changelog generation workflow
- [ ] Add commit message validation hook

#### TASK-017: Set Up Release Automation
**Priority**: Medium  
**Status**: Pending  
**Description**: Automate changelog updates during release process  
**Acceptance Criteria**:
- [ ] Create release workflow that updates version in changelog
- [ ] Automatically move [Unreleased] entries to version section
- [ ] Add comparison links between versions
- [ ] Tag and create GitHub release with changelog excerpt

#### TASK-018: Create Conflict Resolution Strategy
**Priority**: Medium  
**Status**: Pending  
**Description**: Implement strategy to handle multiple PRs updating changelog  
**Acceptance Criteria**:
- [ ] Document merge conflict resolution process
- [ ] Consider implementing changelog fragments approach
- [ ] Set up merge queue if using automated updates
- [ ] Create helper scripts for conflict resolution

#### TASK-019: Add Monitoring and Metrics
**Priority**: Low  
**Status**: Pending  
**Description**: Set up monitoring for changelog automation  
**Acceptance Criteria**:
- [ ] Track success rate of automated updates
- [ ] Monitor time from PR merge to changelog update
- [ ] Set up alerts for automation failures
- [ ] Create dashboard for changelog metrics

#### TASK-020: Update Documentation
**Priority**: High  
**Status**: Pending  
**Description**: Document the new changelog automation process  
**Acceptance Criteria**:
- [ ] Update CONTRIBUTING.md with changelog requirements
- [ ] Document the automation workflow
- [ ] Add troubleshooting guide
- [ ] Create video tutorial for contributors

### Phase 6: Tag System Implementation

#### TASK-021: Update CHANGELOG.md Header with Tag Guidelines
**Priority**: High  
**Status**: Pending  
**Description**: Update the header comment in CHANGELOG.md with new tag system  
**Acceptance Criteria**:
- [ ] Replace existing tag definitions with new audience-focused system
- [ ] Include examples of proper tag usage
- [ ] Add migration note for deprecated tags
- [ ] Ensure backward compatibility message

**Implementation**:
```markdown
<!-- Changelog Entry Guidelines -->
<!-- 
Primary Tags (Required - Choose One):
- [user]: Changes visible to SUEWS/SuPy users (features, API changes, result-affecting fixes)
- [dev]: Developer/contributor changes only (build system, tests, workflows)

Secondary Tags (Optional):
- [new]: New features or capabilities
- [fix]: Bug fixes
- [change]: Modifications to existing behaviour
- [remove]: Deprecated features removed
- [docs]: Documentation updates

Examples:
- [user][new]: Added new urban morphology calculation method
- [user][fix]: Fixed energy balance convergence issue (#123)
- [dev][new]: Added Docker development environment
- [dev][fix]: Resolved test suite memory leak

Legacy tags (deprecated, kept for compatibility):
- [feature] → Use [user][new] or [dev][new]
- [bugfix] → Use [user][fix] or [dev][fix]
- [maintenance] → Use [dev][change]
- [doc] → Use [user][docs] or [dev][docs]
- [change] → Use [user][change]
-->
```

#### TASK-022: Create Changelog Processing Script
**Priority**: High  
**Status**: Pending  
**Description**: Implement Python script to filter changelog by tags  
**Acceptance Criteria**:
- [ ] Create `docs/scripts/process_changelog.py`
- [ ] Support filtering by primary tags
- [ ] Handle multi-tag entries correctly
- [ ] Preserve date headers and structure
- [ ] Add command-line interface

**Implementation**:
```python
#!/usr/bin/env python3
"""Process changelog to create filtered views."""

import argparse
import re
from pathlib import Path

def filter_changelog(input_path, tags_to_include):
    """Filter changelog entries by tags."""
    # Implementation details in design doc
    pass

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--tags", nargs="+", required=True)
    args = parser.parse_args()
    
    filter_changelog(args.input, args.tags)
```

#### TASK-023: Create User-Focused Changelog View
**Priority**: High  
**Status**: Pending  
**Description**: Create changelog-user.rst showing only user-facing changes  
**Acceptance Criteria**:
- [ ] Create `docs/source/changelog-user.rst`
- [ ] Clear introduction explaining filtered view
- [ ] Link to full changelog for completeness
- [ ] Test rendering with sample entries

#### TASK-024: Update Contributing Guidelines
**Priority**: High  
**Status**: Pending  
**Description**: Document new tag system in CONTRIBUTING.md  
**Acceptance Criteria**:
- [ ] Add changelog tag section to CONTRIBUTING.md
- [ ] Include clear examples for each tag combination
- [ ] Explain rationale for audience-focused tags
- [ ] Add decision tree for tag selection

#### TASK-025: Implement Tag Validation in PR Checks
**Priority**: Medium  
**Status**: Pending  
**Description**: Update changelog validation to check for new tag format  
**Acceptance Criteria**:
- [ ] Modify changelog-check workflow
- [ ] Validate at least one primary tag present
- [ ] Warn about deprecated tag usage
- [ ] Provide helpful error messages

#### TASK-026: Create Tag Migration Script (Optional)
**Priority**: Low  
**Status**: Pending  
**Description**: Script to help migrate existing entries to new tag system  
**Acceptance Criteria**:
- [ ] Analyse existing entries and suggest new tags
- [ ] Create mapping from old to new tags
- [ ] Generate migration report
- [ ] Support dry-run mode

## Time Estimates

- Phase 1 (Preparation): 15 minutes
- Phase 2 (Implementation): 30 minutes
- Phase 3 (Testing): 30 minutes
- Phase 4 (Finalisation): 15 minutes
- Phase 5 (Automation): 4-8 hours
  - Validation workflow: 1 hour
  - Branch protection: 30 minutes
  - Claude bot integration: 2-4 hours
  - Templates and docs: 1 hour
  - Testing and refinement: 1-2 hours
- Phase 6 (Tag System): 2-3 hours
  - Update changelog header: 15 minutes
  - Processing script: 1 hour
  - User changelog view: 30 minutes
  - Contributing docs: 30 minutes
  - Validation updates: 30 minutes
  - Migration script: 30 minutes (optional)
- **Total estimate**: 7.5-12.5 hours

## Dependencies

- Write access to create new files
- MyST parser already configured (assumed)
- CHANGELOG.md exists in expected location
- Documentation build environment available