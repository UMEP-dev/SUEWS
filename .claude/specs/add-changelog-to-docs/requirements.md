# Requirements: Add Changelog to Documentation Site

## Overview
This specification addresses GitHub issue #537, which requests adding the main CHANGELOG.md file to the SUEWS documentation site for better visibility of project changes and updates.

## User Stories (EARS Notation)

### Core Requirements

**REQ-001**: WHEN a user visits the SUEWS documentation site, THE SYSTEM SHALL display a changelog section that includes the contents of the main CHANGELOG.md file.

**REQ-002**: WHEN the documentation is built, THE SYSTEM SHALL automatically include the latest version of CHANGELOG.md from the repository root.

**REQ-003**: WHEN a user navigates the documentation, THE SYSTEM SHALL provide a changelog link in the "For developers/contributors" section of the navigation menu.

**REQ-004**: WHEN a user visits the main documentation index page, THE SYSTEM SHALL display a reference link to the changelog for easy access.

### Display Requirements

**REQ-005**: WHEN the changelog is displayed in the documentation, THE SYSTEM SHALL preserve the original markdown formatting and structure.

**REQ-006**: WHEN the changelog contains version headers, THE SYSTEM SHALL ensure they are properly rendered as navigable sections in the Sphinx documentation.

**REQ-007**: WHEN the changelog contains links to GitHub issues or pull requests, THE SYSTEM SHALL ensure these links remain functional in the rendered documentation.

### Build Requirements

**REQ-008**: WHEN the documentation build process runs, THE SYSTEM SHALL verify that the CHANGELOG.md file exists in the repository root before including it.

**REQ-009**: WHEN the CHANGELOG.md file is missing, THE SYSTEM SHALL provide a clear error message during the documentation build process.

**REQ-010**: WHEN the documentation is built for different versions or branches, THE SYSTEM SHALL include the changelog specific to that version/branch.

## Acceptance Criteria

1. The changelog appears in the documentation site navigation under "For developers/contributors"
2. The changelog content matches the repository's CHANGELOG.md exactly
3. All markdown formatting is properly converted to reStructuredText/HTML
4. Links within the changelog remain functional
5. The main index page includes a visible reference to the changelog
6. The documentation builds successfully with the changelog included
7. Version headers in the changelog are navigable sections in the docs

### Automation Requirements

**REQ-011**: WHEN a pull request is merged to master, THE SYSTEM SHALL automatically update the CHANGELOG.md file with the changes introduced by that PR.

**REQ-012**: WHEN automatic changelog updates are enabled, THE SYSTEM SHALL use one of two approaches: (a) Claude bot automatically summarises changes and updates CHANGELOG.md, OR (b) enforce that every PR must include CHANGELOG.md updates before merging.

**REQ-013**: WHEN using Claude bot automation (approach a), THE SYSTEM SHALL analyse the PR title, description, and changed files to generate an appropriate changelog entry following the existing format.

**REQ-014**: WHEN using PR-enforced updates (approach b), THE SYSTEM SHALL validate that CHANGELOG.md has been modified in the PR and block merging if it hasn't (with exceptions for documentation-only or CI-only changes).

**REQ-015**: WHEN changelog updates are made automatically, THE SYSTEM SHALL follow the existing changelog format including version numbers, dates, and categorisation (Added, Changed, Fixed, etc.).

**REQ-016**: WHEN the documentation site is rebuilt after a changelog update, THE SYSTEM SHALL automatically reflect the new changes without manual intervention.

**REQ-017**: WHEN using conventional commits, THE SYSTEM SHALL parse commit messages to automatically categorise changes (feat: → Added, fix: → Fixed, etc.).

**REQ-018**: WHEN a release is created, THE SYSTEM SHALL ensure the changelog contains all changes since the last release with proper version numbering.

### Tag Management Requirements

**REQ-019**: WHEN a contributor adds a changelog entry, THE SYSTEM SHALL require them to use one of the defined category tags that clearly indicates the audience (user vs developer).

**REQ-020**: WHEN the changelog is displayed in documentation, THE SYSTEM SHALL support filtering entries by audience type (user-facing changes only, or all changes).

**REQ-021**: WHEN a tag is marked as deprecated, THE SYSTEM SHALL still display historical entries with that tag but prevent new entries from using it.

**REQ-022**: WHEN generating automated changelog entries, THE SYSTEM SHALL intelligently assign the correct tag based on the nature of the changes (file paths, commit messages, PR labels).

**REQ-023**: WHEN the documentation is built, THE SYSTEM SHALL generate separate filtered views: a user changelog (showing only user-facing changes) and a full changelog (showing all changes).

**REQ-024**: WHEN contributors are unsure which tag to use, THE SYSTEM SHALL provide clear guidelines and examples in the contributing documentation.

## Out of Scope

- Manual changelog generation from git history (automated only)
- Editing changelog content through the documentation site
- Separate changelogs for different components
- Changelog search functionality
- RSS/Atom feeds for changelog updates

## Dependencies

- Existing CHANGELOG.md file in repository root
- Sphinx documentation system
- reStructuredText processing capabilities
- Documentation build pipeline
- GitHub Actions for automation workflows
- Claude bot integration (for approach a)
- GitHub branch protection rules (for approach b)
- Conventional commits specification (optional)

## Risks and Mitigations

1. **Risk**: Large changelog file may impact documentation build time
   - **Mitigation**: Monitor build performance; consider pagination if needed

2. **Risk**: Markdown to reStructuredText conversion issues
   - **Mitigation**: Test common markdown patterns; provide fallback rendering

3. **Risk**: Broken links in historical changelog entries
   - **Mitigation**: Document link format requirements; consider link validation

4. **Risk**: Automated changelog entries may be incorrect or poorly formatted
   - **Mitigation**: Implement review process; allow manual override; provide clear templates

5. **Risk**: Merge conflicts in CHANGELOG.md when multiple PRs update it
   - **Mitigation**: Use merge queue; implement conflict resolution strategy; consider separate changelog files per PR

6. **Risk**: Claude bot may be unavailable or fail to update changelog
   - **Mitigation**: Implement fallback to manual process; add monitoring and alerts

## Success Metrics

- Documentation build time remains within acceptable limits (< 10% increase)
- Zero build failures due to changelog inclusion
- Positive user feedback on changelog accessibility
- Increased visibility of project changes and updates
- 95%+ of merged PRs have changelog entries (excluding exempt categories)
- Average time from PR merge to changelog update < 5 minutes (for automation)
- Zero manual interventions required for standard changelog updates
- Changelog format consistency maintained across all entries